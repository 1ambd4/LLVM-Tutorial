#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
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
// for the purpose return the ASCII value of unknow character.
enum Token {
  tok_eof = -1,

  // commands
  tok_def = -2,
  tok_extern = -3,

  // primary
  tok_identifier = -4,
  tok_number = -5,
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
};

// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST {
public:
  NumberExprAST(double Val) : Val(Val) {}

private:
  double Val;
};

// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
public:
  VariableExprAST(const std::string &Name) : Name(Name) {}

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

private:
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;
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
  PrototypeAST(const std::string &Name, std::vector<std::string> Args)
      : Name(Name), Args(std::move(Args)) {}

private:
  std::string Name;
  std::vector<std::string> Args;
};

// FunctionAST - This class represents a function definition itself.
class FunctionAST {
public:
  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<ExprAST> Body)
      : Proto(std::move(Proto)), Body(std::move(Body)) {}

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

// Now that we have all of our simple expression-parsing logic in place,
// we can define a helper function to wrap it together into one entry point.
// In order to parse an arbitrary primary expression, we need to determine what
// sort of expression it is.
//
// Now that you see the definition of this function, it is more obvious why we
// can assume the state of CurTok in the various functions. This uses look-ahead
// to determine which sort of expression is being inspected, and then parses it
// with a function call.
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
  case '(':
    return ParseParenExpr();
  default:
    return LogError("unknown token when expecting an expression");
  }
}

static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS);

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
static std::unique_ptr<ExprAST> ParseExpression() {
  auto LHS = ParsePrimary();
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

    auto RHS = ParsePrimary();
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

// The next thing missing is handling of function prototypes. In Kaleidoscope,
// these are used both for 'extern' function declarations as well as function
// body definitions.
//
// prototype
//   ::= id '(' id* ')'
static std::unique_ptr<PrototypeAST> ParsePrototype() {
  if (CurTok != tok_identifier) {
    return LogErrorP("Expected function name in prototype");
  }

  std::string FnName = IdentifierStr;
  getNextToken(); // eat '('
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

  return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
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
// Top-Level parsing
//===----------------------------------------------------------------------===//

static void HandleDefinition() {
  if (ParseDefinition()) {
    fprintf(stderr, "Parsed a function definition.\n");
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleExtern() {
  if (ParseExtern()) {
    fprintf(stderr, "Parsed an extern.\n");
  } else {
    getNextToken();
  }
}

static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (ParseTopLevelExpr()) {
    fprintf(stderr, "Parsed a top-level expr.\n");
  } else {
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

int main() {
  InitTokPrecedence();

  // Prime the first token.
  fprintf(stderr, "ready> ");
  getNextToken();

  // Run the main "interpreter loop" now.
  MainLoop();

  return 0;
}
