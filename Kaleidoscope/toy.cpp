#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <string>

using namespace std;

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
    while (isalnum(LastChar = getchar())) {
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

  // Otherwise, just return the character as its ASCII value.
  int ThisChar = LastChar;
  LastChar = getchar();
  return ThisChar;
}

int main() {}
