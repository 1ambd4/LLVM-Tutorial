#include "./KaleidoscopeJIT.h"
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <llvm-c/TargetMachine.h>
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/ExecutionEngine/JITSymbol.h>
#include <llvm/ExecutionEngine/Orc/Shared/ExecutorAddress.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/ConstantFolder.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassInstrumentation.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/StandardInstrumentations.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/Reassociate.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>
#include <llvm/Transforms/Utils/Mem2Reg.h>
#include <map>
#include <memory>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//
// lexer to parse code like:
//
// # Compute the x'th fibonacci number.
// def fib(x)
//     if x < 3 then
//         1
//     else
//         fib(x-1) + fib(x-2)
//  # This expression will compute the 40th number.
//  fib(40)

// Here, we define know character [-256, -1],
// for the purpose return the ASCII value of unknown character.
enum Token {
  tok_eof = -1,

  // commands
  tok_def = -2,
  tok_extern = -3,

  // primary
  tok_identifier = -4,
  tok_number = -5,

  // control
  tok_if = -6,
  tok_then = -7,
  tok_else = -8,

  tok_for = -9,
  tok_in = -10,

  // operators
  tok_binary = -11,
  tok_unary = -12,

  // var definition
  tok_var = -13,
};

static std::string IdentifierStr; // Filled in if tok_identifier.
static double NumVal;             // Filled in if tok_number.

// Return the next token from standard input.
static int gettok() {
  static int LastChar = ' ';

  // Skip any whitespace.
  while (isspace(LastChar)) {
    LastChar = getchar();
  }

  // Recognize identifiers and specific keywords.
  // identifier: [a-zA-z][a-zA-Z0-9]*
  if (isalpha(LastChar)) {
    IdentifierStr = LastChar;
    while (isalnum((LastChar = getchar()))) {
      IdentifierStr += LastChar;
    }

    if (IdentifierStr == "def") {
      return tok_def;
    }
    if (IdentifierStr == "extern") {
      return tok_extern;
    }
    if (IdentifierStr == "if") {
      return tok_if;
    }
    if (IdentifierStr == "then") {
      return tok_then;
    }
    if (IdentifierStr == "else") {
      return tok_else;
    }
    if (IdentifierStr == "for") {
      return tok_for;
    }
    if (IdentifierStr == "in") {
      return tok_in;
    }
    if (IdentifierStr == "binary") {
      return tok_binary;
    }
    if (IdentifierStr == "unary") {
      return tok_unary;
    }
    if (IdentifierStr == "var") {
      return tok_var;
    }
    return tok_identifier;
  }

  // Recognize numeric values.
  // number: [0-9.]+
  // TODO error check like `1.23.456.7890`
  if (isdigit(LastChar) || LastChar == '.') {
    std::string NumStr{""};
    do {
      NumStr += LastChar;
      LastChar = getchar();
    } while (isdigit(LastChar) || LastChar == '.');

    NumVal = strtod(NumStr.c_str(), nullptr);
    return tok_number;
  }

  // Handle comments by skipping to the end of the line,
  // and return the next token.
  if (LastChar == '#') {
    do {
      LastChar = getchar();
    } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

    if (LastChar != EOF) {
      return gettok();
    }
  }

  // Finally, if the input doesn't math one of the above cases,
  // it is either an operator character like '+' or the end of the file.
  // These are handled like behind.
  if (LastChar == EOF) {
    return tok_eof;
  }

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  LastChar = getchar();
  return ThisChar;
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

namespace {

// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
  virtual ~ExprAST() = default;
  // In order to generate LLVM IR, should define virtual code generation
  // (codegen) methods in each AST class. The codegen() method says to emit IR
  // for that AST node along with all the things it depends on, and they all
  // return an LLVM Value object. "Value" is the class used to represent a
  // "Static Single Assignment (SSA) register" or "SSA value" in LLVM.
  //
  // Note that instead of adding virtual methods to the ExprAST class hierarchy,
  // it could also make sense to use a visitor pattern or some other way to
  // model this. Again, this tutorial won’t dwell on good software engineering
  // practices: for our purposes, adding a virtual method is simplest.
  virtual llvm::Value *codegen() = 0;
};

// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST {
public:
  NumberExprAST(double Val) : Val(Val) {}
  llvm::Value *codegen() override;

private:
  double Val;
};

// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
public:
  VariableExprAST(const std::string &Name) : Name(Name) {}
  llvm::Value *codegen() override;
  const std::string &getName() const { return Name; }

private:
  std::string Name;
};

// BinaryExprAST - Expression class for a binary operator.
// Note that there is no discussion about precedence of binary operators,
// lexical structure, etc.
class BinaryExprAST : public ExprAST {
public:
  BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
                std::unique_ptr<ExprAST> RHS)
      : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
  llvm::Value *codegen() override;

private:
  char Op;
  std::unique_ptr<ExprAST> LHS;
  std::unique_ptr<ExprAST> RHS;
};

// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
public:
  CallExprAST(const std::string &Callee,
              std::vector<std::unique_ptr<ExprAST>> Args)
      : Callee(Callee), Args(std::move(Args)) {}
  llvm::Value *codegen() override;

private:
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;
};

// IfExprAST - Expression class for if/then/else.
class IfExprAST : public ExprAST {
public:
  IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then,
            std::unique_ptr<ExprAST> Else)
      : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}
  llvm::Value *codegen() override;

private:
  std::unique_ptr<ExprAST> Cond;
  std::unique_ptr<ExprAST> Then;
  std::unique_ptr<ExprAST> Else;
};

// ForExpAST - Expression class for for/in.
class ForExprAST : public ExprAST {
public:
  ForExprAST(const std::string &VarName, std::unique_ptr<ExprAST> Start,
             std::unique_ptr<ExprAST> End, std::unique_ptr<ExprAST> Step,
             std::unique_ptr<ExprAST> Body)
      : VarName(VarName), Start(std::move(Start)), End(std::move(End)),
        Step(std::move(Step)), Body(std::move(Body)) {}
  llvm::Value *codegen() override;

private:
  std::string VarName;
  std::unique_ptr<ExprAST> Start;
  std::unique_ptr<ExprAST> End;
  std::unique_ptr<ExprAST> Step;
  std::unique_ptr<ExprAST> Body;
};

// UnaryExprAST - Expresson class for a unary operator.
class UnaryExprAST : public ExprAST {
public:
  UnaryExprAST(char Opcode, std::unique_ptr<ExprAST> Operand)
      : Opcode(Opcode), Operand(std::move(Operand)) {}
  llvm::Value *codegen() override;

private:
  char Opcode;
  std::unique_ptr<ExprAST> Operand;
};

// VarExprAST - Expression class for var/in
class VarExprAST : public ExprAST {
public:
  VarExprAST(
      std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames,
      std::unique_ptr<ExprAST> Body)
      : VarNames(std::move(VarNames)), Body(std::move(Body)) {}
  llvm::Value *codegen() override;

private:
  std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;
  std::unique_ptr<ExprAST> Body;
};

// For our basic language, these are all of the expression nodes we'll define.
// Because it doesn't have conditional control flow, it isn't Turing-complete;
// we'll fix that in a later installment.

// The two things we need next are a way to talk about the interface to
// a function, and a way to talk about functions themselves:

// PrototypeAST - This class represents the "prototype" for a function,
// which captures its name, and its argument names (thus implicitly the number
// of arguments the function takes).
// Since all values are double precision floating point, the type of each
// argument doesn't need to be stored anywhere. In a more aggressive and
// realistic language, the "ExprAST" class would probably have a type field.
class PrototypeAST {
public:
  PrototypeAST(const std::string &Name, std::vector<std::string> Args,
               bool IsOperator = false, unsigned Prec = 0)
      : Name(Name), Args(std::move(Args)), IsOperator(IsOperator),
        Precedence(Prec) {}
  llvm::Function *codegen();
  const std::string &getName() const { return Name; }

  bool isUnaryOp() const { return IsOperator && Args.size() == 1; }
  bool isBinaryOp() const { return IsOperator && Args.size() == 2; }

  char getOperatorName() const {
    assert(isUnaryOp() || isBinaryOp());
    return Name[Name.size() - 1];
  }

  unsigned getBinaryPrecedence() const { return Precedence; }

private:
  std::string Name;
  std::vector<std::string> Args;
  bool IsOperator;
  unsigned Precedence; // Precedence if a binary op.
};

// FunctionAST - This class represents a function definition itself.
class FunctionAST {
public:
  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<ExprAST> Body)
      : Proto(std::move(Proto)), Body(std::move(Body)) {}
  llvm::Function *codegen();

private:
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<ExprAST> Body;
};
} // end of anonymous namespace

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

// CurTok/getNextToken - Provide a simple token buffer.
// CurTok is the current token the parser is looking at.
// getNextToken reads another token from the lexer and updates CurTok.
//
// Every function in our parser will assume that CurTok is the current token
// that needs to be parsed.
static int CurTok;
static int getNextToken() { return CurTok = gettok(); }

// LogError* - There are little helper functions for error handling.
// These routines make it easier to handle errors in routines
// that have various return types: they always return null.
std::unique_ptr<ExprAST> LogError(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
  LogError(Str);
  return nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression();

// The routine is very simple: it expects to be called
// when the current token is a tok_number token.
// It takes the current number value, creates a NumberExprAST node,
// advances the lexer to the next token, and finally returns.
//
// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
  auto Result = std::make_unique<NumberExprAST>(NumVal);
  getNextToken();
  return std::move(Result);
}

// There are some interesting aspects to this. The most important one is that
// this routine eats all of the tokens that correspond to the production and
// returns the lexer buffer with the next token (which is not part of the
// grammar production) ready to go. This is a fairly standard way to go for
// recursive descent parsers. For a better example, the parenthesis operator is
// defined like this:
//
// parenexpr :: = '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
  getNextToken(); // eat '('
  auto V = ParseExpression();
  if (!V) {
    return nullptr;
  }

  if (CurTok != ')') {
    return LogError("expected ')");
  }
  getNextToken(); // eat ')'

  return V;
}

// Handling variable references and function calls.
// Here uses look-ahead to determine if the current identifier is a stand alone
// variable reference or if it is a function call expression. It handles this by
// checking to see if the token after the identifier is a '(' token,
// constructing either a VariableExprAST or CallExprAST node as appropriate.
//
// identifierexpr
//   ::= identifier
//   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;

  getNextToken(); // eat identifier
  // Simple variable ref.
  if (CurTok != '(') {
    return std::make_unique<VariableExprAST>(IdName);
  }

  // Function call.
  getNextToken(); // eat '('
                  //
  std::vector<std::unique_ptr<ExprAST>> Args;
  if (CurTok != ')') {
    while (true) {
      if (auto Arg = ParseExpression()) {
        Args.push_back(std::move(Arg));
      } else {
        return nullptr;
      }

      if (CurTok == ')') {
        break;
      }
      if (CurTok != ',') {
        return LogError("expected ')' or ',' in arguments list");
      }

      getNextToken();
    }
  }

  getNextToken(); // eat ')'
  return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

static std::unique_ptr<ExprAST> ParseIfExpr();
static std::unique_ptr<ExprAST> ParseForExpr();
static std::unique_ptr<ExprAST> ParseVarExpr();

// Now that we have all of our simple expression-parsing logic in place,
// we can define a helper function to wrap it together into one entry point.
// In order to parse an arbitrary primary expression, we need to determine
// what sort of expression it is.
//
// Now that you see the definition of this function, it is more obvious why we
// can assume the state of CurTok in the various functions. This uses
// look-ahead to determine which sort of expression is being inspected, and
// then parses it with a function call.
//
// primary
//   ::= identifier
//   ::= numberexpr
//   ::= parenexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
  switch (CurTok) {
  case tok_identifier:
    return ParseIdentifierExpr();
  case tok_number:
    return ParseNumberExpr();
  case tok_if:
    return ParseIfExpr();
  case tok_for:
    return ParseForExpr();
  case tok_var:
    return ParseVarExpr();
  case '(':
    return ParseParenExpr();
  default:
    return LogError("unknown token when expecting an expression");
  }
}

static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS);
static std::unique_ptr<ExprAST> ParseUnary();

// BinopPrecedence - This holds the precedence for each binary operator.
static std::map<char, int> BinopPrecedence;

// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence() {
  if (!isascii(CurTok)) {
    return -1;
  }

  // Make sure it's a declared binop.
  int TokPrec = BinopPrecedence[CurTok];
  if (TokPrec <= 0) {
    return -1;
  }
  return TokPrec;
}

// For the basic form of Kaleidoscope, we will only support 4 binary operators
// This can obviously be extended by you, our brave and intrepid reader.
//
// The smaller the lower.
static void InitTokPrecedence() {
  // With current framework, adding new assignment operator is simple. Just like
  // any other binary operator, but handle it internally (instead of allowing
  // the user to define it).
  // 1 is the lowest precedence.
  BinopPrecedence['='] = 2;
  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40;
}

// With the helper above defined, we can now start parsing binary expressions.
// The basic idea of operator precedence parsing is to break down an
// expression with potentially ambiguous binary operators into pieces.
//
// Consider, for example, the expression "a+b+(c+d)*e*f+g". Operator
// precedence parsing considers this as a stream of primary expressions
// separated by binary operators. As such, it will first parse the leading
// primary expression "a", then it will see the pairs [+, b] [+, (c+d)] [*, e]
// [*, f] and [+, g]. Note that because parentheses are primary expressions,
// the binary expression parser doesn't need to worry about nested
// subexpressions like (c+d) at all.
//
// expression
//   ::= primary binoprhs
//   ::= unary binoprhs
static std::unique_ptr<ExprAST> ParseExpression() {
  auto LHS = ParseUnary();
  // auto LHS = ParsePrimary();
  if (!LHS) {
    return nullptr;
  }

  return ParseBinOpRHS(0, std::move(LHS));
}

// The precedence value passed into ParseBinOpRHS indicates the minimal
// operator precedence that the function is allowed to eat.
//
// For example, if the current pair stream is [+, x] and ParseBinOpRHS is
// passed in a precedence of 40, it will not consume any tokens (because the
// precedence of '+' is only 20).
//
// binoprhs
//   ::= ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS) {
  // If this is a binop, find it precedence.
  while (true) {
    int TokPrec = GetTokPrecedence();

    // If this is a binop that binds at least as tightly as the current binop,
    // consume it, otherwise we are done.
    if (TokPrec < ExprPrec) {
      return LHS;
    }

    // Know this is a binop.
    int BinOp = CurTok;
    getNextToken(); // eat binop

    // auto RHS = ParsePrimary();
    // Parse the unary expression after the binary operator.
    auto RHS = ParseUnary();
    if (!RHS) {
      return nullptr;
    }

    // If BinOp binds less tightly with RHS than the operator after RHS,
    // let the pending operator take RHS as its LHS.
    int NextPrec = GetTokPrecedence();
    if (TokPrec < NextPrec) {
      RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
      if (!RHS) {
        return nullptr;
      }
    }

    LHS =
        std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
  }
}

// If Syntax:
// def fib(x)
//   if x < 3 then
//     1
//   else
//     fib(x-1) + fib(x-2)
//
// ifexpr
//   ::= 'if' expression 'then' expression 'else' expression
static std::unique_ptr<ExprAST> ParseIfExpr() {
  getNextToken(); // eat the 'if'.

  // condition.
  auto Cond = ParseExpression();
  if (!Cond) {
    return nullptr;
  }

  if (CurTok != tok_then) {
    return LogError("expected THEN");
  }
  getNextToken(); // eat the 'then'.

  auto Then = ParseExpression();
  if (!Then) {
    return nullptr;
  }

  if (CurTok != tok_else) {
    return LogError("expected ");
  }
  getNextToken(); // eat the 'else'.

  auto Else = ParseExpression();
  if (!Else) {
    return nullptr;
  }

  return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then),
                                     std::move(Else));
}

// For Syntax:
// def bar(n)
//     for i = 1, i < n, 1.0 in
//         putchar(42);
//
// forexpr
//   ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expr
//
// The parser code is also fairly standard. The only interesting thing here is
// handling of the optional step value. The parser code handles it by checking
// to see if the second comma is present. If not, it sets the step value to
// null in the AST node.
static std::unique_ptr<ExprAST> ParseForExpr() {
  getNextToken(); // eat the 'for'

  if (CurTok != tok_identifier) {
    return LogError("expected identifier after for");
  }

  std::string IdName = IdentifierStr;
  getNextToken(); // eat identifier

  if (CurTok != '=') {
    return LogError("expected '=' after for");
  }
  getNextToken(); // eat '='

  auto Start = ParseExpression();
  if (!Start) {
    return nullptr;
  }
  if (CurTok != ',') {
    return LogError("expected ',' after for start value");
  }
  getNextToken(); // eat ','

  auto End = ParseExpression();
  if (!End) {
    return nullptr;
  }

  // The step value is optional.
  std::unique_ptr<ExprAST> Step;
  if (CurTok == ',') {
    getNextToken(); // eat ','
    Step = ParseExpression();
    if (!Step) {
      return nullptr;
    }
  }

  if (CurTok != tok_in) {
    return LogError("expected 'in' after for");
  }
  getNextToken(); // eat 'in'

  auto Body = ParseExpression();
  if (!Body) {
    return nullptr;
  }

  return std::make_unique<ForExprAST>(IdName, std::move(Start), std::move(End),
                                      std::move(Step), std::move(Body));
}

// unary
//   ::= primary
//   :: '!' unary
static std::unique_ptr<ExprAST> ParseUnary() {
  // If the current token is an operator, it must be a primary expr.
  if (!isascii(CurTok) || CurTok == '(' || CurTok == '.') {
    return ParsePrimary();
  }

  // If this is a unary operator, read it.
  int Opc = CurTok;
  getNextToken();
  if (auto Operand = ParseUnary()) {
    return std::make_unique<UnaryExprAST>(Opc, std::move(Operand));
  }

  return nullptr;
}

static std::unique_ptr<ExprAST> ParseVarExpr() {
  getNextToken(); // eat the var.

  std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;

  // At least one variable name is required.
  if (CurTok != tok_identifier) {
    return LogError("expected identifier after var.");
  }

  while (true) {
    std::string Name = IdentifierStr;
    getNextToken(); // eat identifier

    // Read the optional initializer
    std::unique_ptr<ExprAST> Init;
    if (CurTok == '=') {
      getNextToken(); // eat the '='

      Init = ParseExpression();
      if (!Init)
        return nullptr;
    }

    VarNames.push_back(std::make_pair(Name, std::move(Init)));

    // End of var list, exit loop.
    if (CurTok != ',')
      break;
    getNextToken(); // eat the ','

    if (CurTok != tok_identifier) {
      return LogError("expected identifier list after var");
    }
  }

  // At this point, we have to have 'in'.
  if (CurTok != tok_in) {
    return LogError("expected 'in' keyword after 'var'");
  }
  getNextToken(); // eat 'in'

  auto Body = ParseExpression();
  if (!Body) {
    return nullptr;
  }

  return std::make_unique<VarExprAST>(std::move(VarNames), std::move(Body));
}

// The next thing missing is handling of function prototypes. In Kaleidoscope,
// these are used both for 'extern' function declarations as well as function
// body definitions.
//
// prototype
//   ::= id '(' id* ')'
//   ::= binary LETTER number? (id, id)
//   ::= unary LETTER (id)
static std::unique_ptr<PrototypeAST> ParsePrototype() {
  std::string FnName;

  unsigned Kind = 0; // 0 = identifier, 1 = unary, 2 = binary
  unsigned BinaryPrecedence = 30;

  switch (CurTok) {
  case tok_identifier:
    FnName = IdentifierStr;
    Kind = 0;
    getNextToken();
    break;
  case tok_unary:
    getNextToken();
    if (!isascii(CurTok)) {
      return LogErrorP("Expected unary operator");
    }
    FnName = "unary";
    FnName += (char)CurTok;
    Kind = 1;
    getNextToken();
    break;
  case tok_binary:
    getNextToken();
    if (!isascii(CurTok)) {
      return LogErrorP("Expected binary operator.");
    }
    FnName = "binary";
    FnName += (char)CurTok;
    Kind = 2;
    getNextToken();

    // Read the precedence if present
    if (CurTok == tok_number) {
      if (NumVal < 1 || NumVal > 100) {
        return LogErrorP("Invalid precedence: must be 1..100");
      }
      BinaryPrecedence = (unsigned)NumVal;
      getNextToken();
    }
    break;
  default:
    return LogErrorP("Except function name in prototype.");
  }

  if (CurTok != '(') {
    return LogErrorP("Expected '(' in prototype");
  }

  // Read the list of argument names.
  std::vector<std::string> ArgNames;
  while (getNextToken() == tok_identifier) {
    ArgNames.push_back(IdentifierStr);
  }
  if (CurTok != ')') {
    return LogErrorP("Expected ')' in prototype");
  }
  getNextToken(); // eat ')'

  // Verify right number of names for operator
  if (Kind && ArgNames.size() != Kind) {
    return LogErrorP("Invalid number of operands for operator");
  }

  return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames), Kind != 0,
                                        BinaryPrecedence);
}

// Function definition is very simple, just a prototype plus an expression to
// implement the body
static std::unique_ptr<FunctionAST> ParseDefinition() {
  getNextToken(); // eat def
  auto Proto = ParsePrototype();
  if (!Proto) {
    return nullptr;
  }

  // if (auto E = ParseIdentifierExpr()) {
  if (auto E = ParseExpression()) {
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  }
  return nullptr;
}

// In addition, we support 'extern' to declare functions like 'sin' and 'cos'
// as well as to support forward declaration of user functions. These
// 'extern's are just prototypes with no body.
static std::unique_ptr<PrototypeAST> ParseExtern() {
  getNextToken(); // eat extern
  return ParsePrototype();
}

// Finally, we’ll also let the user type in arbitrary top-level expressions
// and evaluate them on the fly. We will handle this by defining anonymous
// nullary (zero argument) functions for them.
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
  if (auto E = ParseExpression()) {
    // Make anonymous proto.
    auto Proto = std::make_unique<PrototypeAST>("__anon_expr",
                                                std::vector<std::string>());
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  }

  return nullptr;
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

// TheContext is an opaque object that owns a lot of core LLVM data
// structures, such as the type and constant value tables. We don’t need to
// understand it in detail, we just need a single instance to pass into APIs
// that require it.
//
static std::unique_ptr<llvm::LLVMContext> TheContext;
// static llvm::LLVMContext TheContext;
// The Builder object is a helper object that makes it easy to generate LLVM
// instructions. Instances of the IRBuilder class template keep track of the
// current place to insert instructions and has methods to create new
// instructions.
//
static std::unique_ptr<llvm::IRBuilder<>> Builder;
// static llvm::IRBuilder<> Builder(TheContext);
// TheModule is an LLVM construct that contains functions and global
// variables. In many ways, it is the top-level structure that the LLVM IR
// uses to contain code. It will own the memory for all of the IR that we
// generate, which is why the codegen() method returns a raw Value*, rather
// than a unique_ptr<Value>.
static std::unique_ptr<llvm::Module> TheModule;
// The NamedValues map keeps track of which values are defined in the current
// scope and what their LLVM representation is. (In other words, it is a
// symbol table for the code). In this form of Kaleidoscope, the only things
// that can be referenced are function parameters. As such, function
// parameters will be in this map when generating code for their function
// body.
static std::map<std::string, llvm::AllocaInst *> NamedValues;

static std::unique_ptr<llvm::orc::KaleidoscopeJIT> TheJIT;

static std::unique_ptr<llvm::FunctionPassManager> TheFPM;
static std::unique_ptr<llvm::LoopAnalysisManager> TheLAM;
static std::unique_ptr<llvm::FunctionAnalysisManager> TheFAM;
static std::unique_ptr<llvm::CGSCCAnalysisManager> TheCGAM;
static std::unique_ptr<llvm::ModuleAnalysisManager> TheMAM;
static std::unique_ptr<llvm::PassInstrumentationCallbacks> ThePIC;
static std::unique_ptr<llvm::StandardInstrumentations> TheSI;

static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;

static llvm::ExitOnError ExitOnErr;

llvm::Value *LogErrorV(const char *Str) {
  LogError(Str);
  return nullptr;
}

// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
// the function. This is used for mutable variables etc.
static llvm::AllocaInst *
CreateEntryBlockAlloca(llvm::Function *TheFunction,
                       const llvm::StringRef &VarName) {
  llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                         TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(llvm::Type::getDoubleTy(*TheContext), nullptr,
                           VarName);
}

llvm::Function *getFunction(std::string Name) {
  // First, see if the function has already been added to the current module.
  if (auto *F = TheModule->getFunction(Name)) {
    return F;
  }

  // If not, check whether we can codegen the declaration from some existing
  // prototype.
  auto FI = FunctionProtos.find(Name);
  if (FI != FunctionProtos.end()) {
    return FI->second->codegen();
  }

  // If no existing prototype exists, return null.
  return nullptr;
}

// In the LLVM IR, numeric constants are represented with the ConstantFP
// class, which holds the numeric value in an APFloat internally (APFloat has
// the capability of holding floating point constants of Arbitrary Precision).
// This code basically just creates and returns a ConstantFP. Note that in the
// LLVM IR that constants are all uniqued together and shared. For this
// reason, the API uses the "foo::get(…)" idiom instead of "new foo(..)" or
// "foo::Create(..)".
llvm::Value *NumberExprAST::codegen() {
  return llvm::ConstantFP::get(*TheContext, llvm::APFloat(Val));
}

// References to variables are also quite simple using LLVM. In the simple
// version of Kaleidoscope, we assume that the variable has already been
// emitted somewhere and its value is available. In practice, the only values
// that can be in the NamedValues map are function arguments. This code simply
// checks to see that the specified name is in the map (if not, an unknown
// variable is being referenced) and returns the value for it.
llvm::Value *VariableExprAST::codegen() {
  // Look this variable up in the function.
  llvm::AllocaInst *A = NamedValues[Name];
  if (!A) {
    LogErrorV("Unknown variable name.");
  }
  return Builder->CreateLoad(A->getAllocatedType(), A, Name.c_str());
}

llvm::Value *IfExprAST::codegen() {
  llvm::Value *CondV = Cond->codegen();
  if (!CondV) {
    return nullptr;
  }

  // Convert condition to a bool by comparing non-equal to 0.0.
  CondV = Builder->CreateFCmpONE(
      CondV, llvm::ConstantFP::get(*TheContext, llvm::APFloat(0.0)), "ifcond");

  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();

  // Create blocks for the then and else cases.  Insert the 'then' block at
  // the end of the function.
  llvm::BasicBlock *ThenBB =
      llvm::BasicBlock::Create(*TheContext, "then", TheFunction);
  llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(*TheContext, "else");
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*TheContext, "ifcont");

  Builder->CreateCondBr(CondV, ThenBB, ElseBB);

  // Emit then value.
  Builder->SetInsertPoint(ThenBB);

  llvm::Value *ThenV = Then->codegen();
  if (!ThenV) {
    return nullptr;
  }

  Builder->CreateBr(MergeBB);
  // Codegen of 'Then' can change the current block, update ThenBB for the
  // PHI.
  ThenBB = Builder->GetInsertBlock();

  // Emit else block.
  TheFunction->insert(TheFunction->end(), ElseBB);
  Builder->SetInsertPoint(ElseBB);

  llvm::Value *ElseV = Else->codegen();
  if (!ElseV) {
    return nullptr;
  }

  Builder->CreateBr(MergeBB);
  // Codegen of 'Else' can change the current block, update ElseBB for the
  // PHI.
  ElseBB = Builder->GetInsertBlock();

  // Emit merge block.
  TheFunction->insert(TheFunction->end(), MergeBB);
  Builder->SetInsertPoint(MergeBB);
  llvm::PHINode *PN =
      Builder->CreatePHI(llvm::Type::getDoubleTy(*TheContext), 2, "IfTemp");

  PN->addIncoming(ThenV, ThenBB);
  PN->addIncoming(ElseV, ElseBB);
  return PN;
}

llvm::Value *ForExprAST::codegen() {
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  // Create an alloca for the variable in the entry block.
  llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);

  // Emit the start code first, without 'variable' in scope.
  llvm::Value *StartVal = Start->codegen();
  if (!StartVal) {
    return nullptr;
  }

  // Store the value into the alloca.
  Builder->CreateStore(StartVal, Alloca);

  // llvm::BasicBlock *PreheaderBB = Builder->GetInsertBlock();
  // Make the new basic block for the loop header, inserting after current
  // block.
  llvm::BasicBlock *LoopBB =
      llvm::BasicBlock::Create(*TheContext, "Loop", TheFunction);

  // Insert an explicit fall through from the current block to the LoopBB.
  Builder->CreateBr(LoopBB);

  // Start insertion in LoopBB.
  Builder->SetInsertPoint(LoopBB);

  // Start the PHI node with an entry for Start.
  // llvm::PHINode *Variable =
  //     Builder->CreatePHI(llvm::Type::getDoubleTy(*TheContext), 2, VarName);
  // Variable->addIncoming(StartVal, PreheaderBB);

  // Within the loop, the variable is defined equal to the PHI node. If it
  // shadows an existing variable, we have to restore it, so save it now.
  llvm::AllocaInst *OldVal = NamedValues[VarName];
  NamedValues[VarName] = Alloca;

  // Emit the body of the loop. This, like any other expr, can change the
  // current BB. Note that we ignore the value computed by the body, but don't
  // allow an error.
  if (!Body->codegen()) {
    return nullptr;
  }

  // Emit the step value.
  llvm::Value *StepVal = nullptr;
  if (Step) {
    StepVal = Step->codegen();
    if (!StepVal) {
      return nullptr;
    }
  } else {
    // If step not specified, use 1.0 as default.
    StepVal = llvm::ConstantFP::get(*TheContext, llvm::APFloat(1.0));
  }

  // llvm::Value *NextVal = Builder->CreateFAdd(Variable, StepVal, "nextval");

  // Compute the end condition.
  llvm::Value *EndCond = End->codegen();
  if (!EndCond) {
    return nullptr;
  }

  // Reload, increment, and restore the alloca. This handles the case where the
  // body of the loop mutates the variable.
  llvm::Value *CurVar =
      Builder->CreateLoad(Alloca->getAllocatedType(), Alloca, VarName.c_str());

  // Convert condition to a bool by comparing non-equal to 0.0.
  EndCond = Builder->CreateFCmpONE(
      EndCond, llvm::ConstantFP::get(*TheContext, llvm::APFloat(0.0)),
      "loopcond");

  // Create the "after loop" block and insert it.
  // llvm::BasicBlock *LoopEndBB = Builder->GetInsertBlock();
  llvm::BasicBlock *AfterBB =
      llvm::BasicBlock::Create(*TheContext, "afterloop", TheFunction);

  // Insert the conditional branch into the end of LoopEndBB.
  Builder->CreateCondBr(EndCond, LoopBB, AfterBB);

  // Any new code will be inserted in AfterBB.
  Builder->SetInsertPoint(AfterBB);

  // And a new entry to the PHI node for the backedge.
  // Variable->addIncoming(NextVal, LoopEndBB);

  // Restore the unshadowed variable.
  if (OldVal) {
    NamedValues[VarName] = OldVal;
  } else {
    NamedValues.erase(VarName);
  }

  // for expr always return 0.0.
  return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(*TheContext));
}

// Binarkjy operators start to get more interesting. The basic idea here is
// that we recursively emit code for the left-hand side of the expression,
// then the right-hand side, then we compute the result of the binary
// expression. In this code, we do a simple switch on the opcode to create the
// right LLVM instruction. In the example above, the LLVM builder class is
// starting to show its value. IRBuilder knows where to insert the newly
// created instruction, all you have to do is specify what instruction to
// create (e.g. with CreateFAdd), which operands to use (L and R here) and
// optionally provide a name for the generated instruction. One nice thing
// about LLVM is that the name is just a hint. For instance, if the code above
// emits multiple “addtmp” variables, LLVM will automatically provide each one
// with an increasing, unique numeric suffix. Local value names for
// instructions are purely optional, but it makes it much easier to read the
// IR dumps.
llvm::Value *BinaryExprAST::codegen() {
  // Special case '=' because we don't want to emit the LHS as an expression.
  if (Op == '=') {
    // Assignment requires the LHS to be an identifier.
    // This assume we're building without RTTI because LLVM build that way be
    // default. If you build LLVM with RTTI this can be changed to a
    // dynamic_cast for automatic error checking.
    VariableExprAST *LHSE = static_cast<VariableExprAST *>(LHS.get());
    if (!LHSE) {
      return LogErrorV("destination of '=' must be a variable.");
    }

    // Codegen the RHS.
    llvm::Value *Val = RHS->codegen();
    if (!Val) {
      return nullptr;
    }

    // Look up the name.
    llvm::Value *Variable = NamedValues[LHSE->getName()];
    if (!Variable) {
      return LogErrorV("Unknown variable name.");
    }

    Builder->CreateStore(Val, Variable);
    return Val;
  }

  llvm::Value *L = LHS->codegen();
  llvm::Value *R = RHS->codegen();
  if (!L || !R) {
    return nullptr;
  }

  switch (Op) {
  case '+':
    return Builder->CreateFAdd(L, R, "AddTemp");
  case '-':
    return Builder->CreateFSub(L, R, "SubTemp");
  case '*':
    return Builder->CreateFMul(L, R, "MulTemp");
  case '<':
    L = Builder->CreateFCmpULT(L, R, "CMPTemp");
    // Convert bool 0/1 to double 0.0 or 1.0
    return Builder->CreateUIToFP(L, llvm::Type::getDoubleTy(*TheContext),
                                 "BoolTemp");
  default:
    break;
    // return LogErrorV("Invalid Binary Operator.");
  }

  // If it wasn't a builtin binary operator, it must be a user defined one.
  // Emit a call to it.
  llvm::Function *F = getFunction(std::string("binary") + Op);
  assert(F && "binary operator not found!");

  llvm::Value *Ops[2] = {L, R};
  return Builder->CreateCall(F, Ops, "binop");
}

llvm::Value *UnaryExprAST::codegen() {
  llvm::Value *OperandV = Operand->codegen();
  if (!OperandV) {
    return nullptr;
  }

  llvm::Function *F = getFunction(std::string("unary") + Opcode);
  if (!F) {
    return LogErrorV("Unknow unary operator");
  }

  return Builder->CreateCall(F, OperandV, "unop");
}

llvm::Value *VarExprAST::codegen() {
  std::vector<llvm::AllocaInst *> OldBindings;

  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();

  // Register all variables and emit their initializer.
  for (unsigned i = 0, e = VarNames.size(); i != e; ++i) {
    const std::string &VarName = VarNames[i].first;
    ExprAST *Init = VarNames[i].second.get();

    // Emit the initializer before adding the variable to scope.
    // like: var a = 1 in var a = a in ...
    llvm::Value *InitVal;
    if (Init) {
      InitVal = Init->codegen();
      if (!InitVal) {
        return nullptr;
      }
    } else { // If not specified, use 0.0
      InitVal = llvm::ConstantFP::get(*TheContext, llvm::APFloat(0.0));
    }

    llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
    Builder->CreateStore(InitVal, Alloca);

    // Remeber the old variable binding so that we can restore the binding when
    // we unused.
    OldBindings.push_back(NamedValues[VarName]);
    // Remerber this binding.
    NamedValues[VarName] = Alloca;
  }

  // Codegen the body, now that all vars are in scope.
  llvm::Value *BodyVal = Body->codegen();
  if (!BodyVal) {
    return nullptr;
  }

  // Pop all our variables from scope.
  for (unsigned i = 0, e = VarNames.size(); i != e; ++i) {
    NamedValues[VarNames[i].first] = OldBindings[i];
  }

  // Restore teh body computation.
  return BodyVal;
}

// Code generation for function calls is quite straightforward with LLVM. The
// code above initially does a function name lookup in the LLVM Module's
// symbol table. Recall that the LLVM Module is the container that holds the
// functions we are JIT'ing. By giving each function the same name as what the
// user specifies, we can use the LLVM symbol table to resolve function names
// for us. Once we have the function to call, we recursively codegen each
// argument that is to be passed in, and create an LLVM call instruction. Note
// that LLVM uses the native C calling conventions by default, allowing these
// calls to also call into standard library functions like "sin" and "cos",
// with no additional effort.
llvm::Value *CallExprAST::codegen() {
  // Look up the name in the global module table.
  // Notice: dont't call TheModule->getFunction(CalleeF) direct.
  // llvm::Function *CalleeF = TheModule->getFunction(Callee);
  llvm::Function *CalleeF = getFunction(Callee);
  if (!CalleeF) {
    return LogErrorV("unknown function referenced.");
  }

  // If arguments mismatch error.
  if (CalleeF->arg_size() != Args.size()) {
    return LogErrorV("Incorrect # arguments passed.");
  }

  std::vector<llvm::Value *> ArgsV;
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgsV.push_back(Args[i]->codegen());
    if (!ArgsV.back()) {
      return nullptr;
    }
  }
  return Builder->CreateCall(CalleeF, ArgsV, "CallTemp");
}

// The call to FunctionType::get creates the FunctionType that should be used
// for a given Prototype.
// Then, Function::Create creates the IR Function corresponding to the
// Prototype. This indicates the type, linkage and name to use, as well as
// which module to insert into. "external linkage" means that the function may
// be defined outside the current module and/or that it is callable by
// functions outside the module. Finally, we set the name of each of the
// function's arguments according to the names given in the Prototype. This
// step isn't strictly necessary, but keeping the names consistent makes the
// IR more readable, and allows subsequent code to refer directly to the
// arguments for their names, rather than having to look up them up in the
// Prototype AST.
llvm::Function *PrototypeAST::codegen() {
  // Make the function type: double(double, double) etc.
  std::vector<llvm::Type *> Doubles(Args.size(),
                                    llvm::Type::getDoubleTy(*TheContext));
  llvm::FunctionType *FT = llvm::FunctionType::get(
      llvm::Type::getDoubleTy(*TheContext), Doubles, false);
  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::ExternalLinkage, Name, TheModule.get());

  // Set name for all arguments.
  unsigned Idx = 0;
  for (auto &Arg : F->args()) {
    Arg.setName(Args[Idx++]);
  }

  return F;
}

// At this point we have a function prototype with no body. This is how LLVM
// IR represents function declarations. For extern statements in Kaleidoscope,
// this is as far as we need to go. For function definitions however, we need
// to codegen and attach a function body.
//
// For function definitions, we start by searching TheModule's symbol table
// for an existing version of this function, in case one has already been
// created using an 'extern' statement. If Module::getFunction returns null
// then no previous version exists, so we'll codegen one from the Prototype.
// In either case, we want to assert that the function is empty (i.e. has no
// body yet) before we start.
//
// Once the insertion point has been set up and the NamedValues map populated,
// we call the codegen() method for the root expression of the function. If no
// error happens, this emits code to compute the expression into the entry
// block and returns the value that was computed. Assuming no error, we then
// create an LLVM ret instruction, which completes the function.
//
// Once the function is built, we call verifyFunction, which is provided by
// LLVM. This function does a variety of consistency checks on the generated
// code, to determine if our compiler is doing everything right. Using this is
// important: it can catch a lot of bugs. Once the function is finished and
// validated, we return it.
//
// The only piece left here is handling of the error case. For simplicity, we
// handle this by merely deleting the function we produced with the
// eraseFromParent method. This allows the user to redefine a function that
// they incorrectly typed in before: if we didn't delete it, it would live in
// the symbol table, with a body, preventing future redefinition.
llvm::Function *FunctionAST::codegen() {
  // Transfer ownership of the prototype to the FunctionProtos map, but keep a
  // reference to it for use below.
  auto &P = *Proto;
  FunctionProtos[Proto->getName()] = std::move(Proto);
  llvm::Function *TheFunction = getFunction(P.getName());
  if (!TheFunction) {
    return nullptr;
  }

  // If this is an operator, install it.
  if (P.isBinaryOp()) {
    BinopPrecedence[P.getOperatorName()] = P.getBinaryPrecedence();
  }

  // Create a new basic block to start insertion into.
  llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(*TheContext, "entry", TheFunction);
  Builder->SetInsertPoint(BB);

  // Record the function arguments in the NamedValues map.
  NamedValues.clear();

  for (auto &Arg : TheFunction->args()) {
    // Create an alloca for this variable.
    llvm::AllocaInst *Alloca =
        CreateEntryBlockAlloca(TheFunction, Arg.getName());

    // Store the initial value into the alloca.
    Builder->CreateStore(&Arg, Alloca);

    // Add arguments to variable symbol table.
    NamedValues[std::string(Arg.getName())] = Alloca;
  }

  if (llvm::Value *RetVal = Body->codegen()) {
    // Finish off the function.
    Builder->CreateRet(RetVal);

    // Validate the generated code, checking for consistency.
    // This function is defined in llvm/IR/Verifier.h
    verifyFunction(*TheFunction);

    // Optimize the function.
    // The FunctionPassManager optimizes and updates the LLVM Function* in
    // place, improving (hopefully) its body.
    // TheFPM->run(*TheFunction, *TheFAM);

    return TheFunction;
  }

  // Error reading body, remove function.
  TheFunction->eraseFromParent();

  if (P.isBinaryOp()) {
    BinopPrecedence.erase(P.getOperatorName());
  }
  return nullptr;
}
// Now we get to the point where the Builder is set up. The first line creates
// a new basic block (named "entry"), which is inserted into TheFunction. The
// second line then tells the builder that new instructions should be inserted
// into the end of the new basic block. Basic blocks in LLVM are an important
// part of functions that define the Control Flow Graph. Since we don’t have
// any control flow, our functions will only contain one block at this point.

// TODO
// This code does have a bug, though: If the FunctionAST::codegen() method
// finds an existing IR Function, it does not validate its signature against
// the definition's own prototype. This means that an earlier ‘extern’
// declaration will take precedence over the function definition’s signature,
// which can cause codegen to fail, for instance if the function arguments are
// named differently. There are a number of ways to fix this bug, see what you
// can come up with! Here is a testcase:
// extern foo(a); # OK, defines foo. def
// foo(b) b;      # Error: Unknown variable name. (decl using 'a' takes
//                # precedence).

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

static void InitializeModuleAndPassManger() {
  // Open a new module
  TheContext = std::make_unique<llvm::LLVMContext>();
  TheModule = std::make_unique<llvm::Module>("JIT", *TheContext);

  // Create a new builder for the module.
  Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);
}

// This function not used now.
static void InitializeModuleAndMangers() {
  // Open a new context and module.
  TheContext = std::make_unique<llvm::LLVMContext>();
  TheModule = std::make_unique<llvm::Module>("JIT", *TheContext);
  TheModule->setDataLayout(TheJIT->getDataLayout());

  // Create a new builder for the module.
  Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);

  // Create new pass and analysis managers.
  TheFPM = std::make_unique<llvm::FunctionPassManager>();
  TheLAM = std::make_unique<llvm::LoopAnalysisManager>();
  TheFAM = std::make_unique<llvm::FunctionAnalysisManager>();
  TheCGAM = std::make_unique<llvm::CGSCCAnalysisManager>();
  TheMAM = std::make_unique<llvm::ModuleAnalysisManager>();
  ThePIC = std::make_unique<llvm::PassInstrumentationCallbacks>();
  TheSI = std::make_unique<llvm::StandardInstrumentations>(
      *TheContext, true); // true - enable DebugLogging

  TheSI->registerCallbacks(*ThePIC, TheFAM.get());

  // And transform passes.
  // Do simple "peephole" optimizations and bit-twiddling optzns.
  TheFPM->addPass(llvm::InstCombinePass());
  // Reassociate expressions.
  TheFPM->addPass(llvm::ReassociatePass());
  // Eliminate Common SubExpressions.
  TheFPM->addPass(llvm::GVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  TheFPM->addPass(llvm::SimplifyCFGPass());

  // Promote allocas to registers.
  // So, the version of llvm I use doesn't have this pass in mem2reg.h.
  // TheFPM->addPass(llvm::createPromoteMemoryToRegisterPass());
  // Do simple "peephole" optimizaitons and bit-twiddling optzns.
  // But have PromotePass in it.
  TheFPM->addPass(llvm::PromotePass());
  // TheFPM->addPass(llvm::createInstructionCombiningPass());
  // TheFPM->addPass(llvm::InstructionCombiningPass());
  // Reassociate expressions.
  TheFPM->addPass(llvm::ReassociatePass());

  // Register analysis passes used in these transform passes.
  llvm::PassBuilder PB;
  PB.registerModuleAnalyses(*TheMAM);
  PB.registerFunctionAnalyses(*TheFAM);
  PB.crossRegisterProxies(*TheLAM, *TheFAM, *TheCGAM, *TheMAM);
}

static void HandleDefinition() {
  if (auto FnAST = ParseDefinition()) {
    if (auto *FnIR = FnAST->codegen()) {
      fprintf(stderr, "Read function definition:\n");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleExtern() {
  if (auto ProtoAST = ParseExtern()) {
    if (auto *FnIR = ProtoAST->codegen()) {
      fprintf(stderr, "Read extern:\n");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");
      FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (auto FnAST = ParseTopLevelExpr()) {
    if (auto *FnIR = FnAST->codegen(); FnIR) {
      fprintf(stderr, "Read top level expression:\n");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");

      // Create a ResourceTracker to track JIT'd memory allocated to our
      // anonymous expression -- that way we can free it after executing.
      auto RT = TheJIT->getMainJITDylib().createResourceTracker();

      auto TSM = llvm::orc::ThreadSafeModule(std::move(TheModule),
                                             std::move(TheContext));
      ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
      InitializeModuleAndMangers();

      // Search the JIT for the __anon_expr symbol.
      auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));

      // Get the symbol's address and cast it to the right type (takes no
      // arguments, returns a double) so we can call it as a native function.
      double (*FP)() = (double (*)())(intptr_t)ExprSymbol.getAddress();
      fprintf(stderr, "Evaluated to %f\n", FP());

      // Delete the anonymous expression module from the JIT.
      ExitOnErr(RT->remove());
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

// Now that we have all the pieces, let’s build a little driver that will let
// us actually execute this code we’ve built!
//
// The driver for this simply invokes all of the parsing pieces with a
// top-level dispatch loop.
//
// top ::=
//   definition
//   | external
//   | expression
//   | ';'
static void MainLoop() {
  while (true) {
    fprintf(stderr, "ready> ");
    switch (CurTok) {
    case tok_eof:
      return;
    case ';':
      getNextToken();
      break;
    case tok_def:
      HandleDefinition();
      break;
    case tok_extern:
      HandleExtern();
      break;
    default:
      HandleTopLevelExpression();
      break;
    }
  }
}

//===----------------------------------------------------------------------===//
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//

// How does the JIT know about sin and cos? The answer is surprisingly simple:
// The KaleidoscopeJIT has a straightforward symbol resolution rule that it uses
// to find symbols that aren't available in any given module: First it searches
// all the modules that have already been added to the JIT, from the most recent
// to the oldest, to find the newest definition. If no definition is found
// inside the JIT, it falls back to calling "dlsym("sin")" on the Kaleidoscope
// process itself. Since "sin" is defined within the JIT's address space, it
// simply patches up calls in the module to call the libm version of sin
// directly. But in some cases this even goes further: as sin and cos are names
// of standard math functions, the constant folder will directly evaluate the
// function calls to the correct result when called with constants like in the
// "sin(1.0)" above.

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

// putchard - putchar that takes a double and return 0.
extern "C" DLLEXPORT double putchard(double x) {
  fputc((char)x, stderr);
  return 0;
}

// printd - printf that takes a double prints it as "%f\n".
extern "C" DLLEXPORT double printd(double x) {
  fprintf(stderr, "%f\n", x);
  return 0;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main() {
  // Prepare the environment to create code for the current native target and
  // declare and initialize the JIT. This is done by calling some
  // InitializeNativeTarget\* functions and adding a global variable TheJIT.
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  InitTokPrecedence();

  // Prime the first token.
  fprintf(stderr, "ready> ");
  getNextToken();

  // Disable JIT.
  // TheJIT = ExitOnErr(llvm::orc::KaleidoscopeJIT::Create());

  // Make the module, which holds all the code.
  // InitializeModuleAndMangers();
  InitializeModuleAndPassManger();

  // Run the main "interpreter loop" now.
  MainLoop();

  // Print out all of the generated code.
  // TheModule->print(llvm::errs(), nullptr);

  // Initialize the target registry etc.
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  // target triple: <arch><sub>-<vendor>-<sys>-<abi>
  // like: x86_64-pc-linux-gnu
  auto TargetTriple = LLVMGetDefaultTargetTriple();
  TheModule->setTargetTriple(TargetTriple);

  std::string Error;
  auto Target = llvm::TargetRegistry::lookupTarget(TargetTriple, Error);

  // Print an error and exit if we couldn't find the request target.
  // This generally occurs if we have forgotten to initialise the TargetRegistry
  // or we have a bogus target triple.
  if (!Target) {
    llvm::errs() << Error;
    return 1;
  }

  auto CPU = "generic";
  auto Features = "";

  llvm::TargetOptions opt;
  auto TheTargetMachine = Target->createTargetMachine(
      TargetTriple, CPU, Features, opt, llvm::Reloc::PIC_);

  TheModule->setDataLayout(TheTargetMachine->createDataLayout());
  TheModule->setTargetTriple(TargetTriple);

  // We're ready to emit object code, now define where to write file to.
  auto FileName = "output.o";
  std::error_code EC;
  llvm::raw_fd_ostream dest(FileName, EC, llvm::sys::fs::OF_None);

  if (EC) {
    llvm::errs() << "Could not open file: " << EC.message();
    return -1;
  }

  // #include <llvm/IR/LegacyPassManager.h>
  // Finally, define a pass that emits object code, then run the pass.
  llvm::legacy::PassManager pass;
  auto FileType = llvm::CGFT_ObjectFile;

  if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
    llvm::errs() << "The TargetMachine can't emit a file of this type.";
    return 1;
  }

  pass.run(*TheModule);
  dest.flush();

  llvm::outs() << "Wrote: " << FileName << "\n";

  return 0;
}
