var ctorSymbol, funcType;

// class Location
var Location = function(file, line) {
  this.file = file;
  this.line = line;
};
Location.prototype.toString = function() {
  var file;
  if ((file = this.file) !== null) {
    return "in " + file + " on line " + this.line;
  }
  return "on line " + this.line;
};

// class Log
var Log = function() {
  this.messages = [];
  this.hasErrors = false;
};
Log.prototype.warning = function(location, text) {
  this.messages.push("warning " + location + ": " + text);
};
Log.prototype.error = function(location, text) {
  this.messages.push("error " + location + ": " + text);
  this.hasErrors = true;
};

// class TokenKind
var TokenKind = function(name, text) {
  this.name = name;
  this.text = text;
};

// Symbols
TokenKind.LPAREN = new TokenKind("LPAREN", "(");
TokenKind.RPAREN = new TokenKind("RPAREN", ")");
TokenKind.LBRACKET = new TokenKind("LBRACKET", "[");
TokenKind.RBRACKET = new TokenKind("RBRACKET", "]");
TokenKind.LBRACE = new TokenKind("LBRACE", "{");
TokenKind.RBRACE = new TokenKind("RBRACE", "}");
TokenKind.COMMA = new TokenKind("COMMA", ",");
TokenKind.SEMICOLON = new TokenKind("SEMICOLON", ";");
TokenKind.SPLAT = new TokenKind("SPLAT", "...");
TokenKind.DOT = new TokenKind("DOT", ".");
TokenKind.QUESTION = new TokenKind("QUESTION", "?");
TokenKind.ARROW = new TokenKind("ARROW", "->");

// Symbolic operators
TokenKind.INC = new TokenKind("INC", "++");
TokenKind.DEC = new TokenKind("DEC", "--");
TokenKind.ADD_ASSIGN = new TokenKind("ADD_ASSIGN", "+=");
TokenKind.SUB_ASSIGN = new TokenKind("SUB_ASSIGN", "-=");
TokenKind.MUL_ASSIGN = new TokenKind("MUL_ASSIGN", "*=");
TokenKind.DIV_ASSIGN = new TokenKind("DIV_ASSIGN", "/=");
TokenKind.MOD_ASSIGN = new TokenKind("MOD_ASSIGN", "%=");
TokenKind.SHL_ASSIGN = new TokenKind("SHL_ASSIGN", "<<=");
TokenKind.SHR_ASSIGN = new TokenKind("SHR_ASSIGN", ">>=");
TokenKind.BITOR_ASSIGN = new TokenKind("BITOR_ASSIGN", "|=");
TokenKind.BITAND_ASSIGN = new TokenKind("BITAND_ASSIGN", "&=");
TokenKind.BITXOR_ASSIGN = new TokenKind("BITXOR_ASSIGN", "^=");
TokenKind.ADD = new TokenKind("ADD", "+");
TokenKind.SUB = new TokenKind("SUB", "-");
TokenKind.MUL = new TokenKind("MUL", "*");
TokenKind.DIV = new TokenKind("DIV", "/");
TokenKind.MOD = new TokenKind("MOD", "%");
TokenKind.SHL = new TokenKind("SHL", "<<");
TokenKind.SHR = new TokenKind("SHR", ">>");
TokenKind.BITOR = new TokenKind("BITOR", "|");
TokenKind.BITAND = new TokenKind("BITAND", "&");
TokenKind.BITXOR = new TokenKind("BITXOR", "^");
TokenKind.EQ = new TokenKind("EQ", "==");
TokenKind.NEQ = new TokenKind("NEQ", "!=");
TokenKind.LTE = new TokenKind("LTE", "<=");
TokenKind.GTE = new TokenKind("GTE", ">=");
TokenKind.LT = new TokenKind("LT", "<");
TokenKind.GT = new TokenKind("GT", ">");
TokenKind.ASSIGN = new TokenKind("ASSIGN", "=");
TokenKind.BITNOT = new TokenKind("BITNOT", "~");

// Keyword operators
TokenKind.AND = new TokenKind("AND", "and");
TokenKind.OR = new TokenKind("OR", "or");
TokenKind.IS = new TokenKind("IS", "is");
TokenKind.IN = new TokenKind("IN", "in");
TokenKind.NOT = new TokenKind("NOT", "not");

// Keywords
TokenKind.STATIC = new TokenKind("STATIC", "static");
TokenKind.OVER = new TokenKind("OVER", "over");
TokenKind.ABSTRACT = new TokenKind("ABSTRACT", "abstract");
TokenKind.EXTERN = new TokenKind("EXTERN", "extern");
TokenKind.PROP = new TokenKind("PROP", "prop");
TokenKind.RETURN = new TokenKind("RETURN", "return");
TokenKind.IF = new TokenKind("IF", "if");
TokenKind.ELSE = new TokenKind("ELSE", "else");
TokenKind.WHILE = new TokenKind("WHILE", "while");
TokenKind.FOR = new TokenKind("FOR", "for");
TokenKind.CONTINUE = new TokenKind("CONTINUE", "continue");
TokenKind.BREAK = new TokenKind("BREAK", "break");
TokenKind.FAIL = new TokenKind("FAIL", "fail");
TokenKind.CLASS = new TokenKind("CLASS", "class");
TokenKind.FN = new TokenKind("FN", "fn");

// Keyword literals
TokenKind.NULL = new TokenKind("NULL", "null");
TokenKind.VOID = new TokenKind("VOID", "void");
TokenKind.THIS = new TokenKind("THIS", "this");
TokenKind.BASE = new TokenKind("BASE", "base");
TokenKind.TRUE = new TokenKind("TRUE", "true");
TokenKind.FALSE = new TokenKind("FALSE", "false");
TokenKind.VAR = new TokenKind("VAR", "var");
TokenKind.INT = new TokenKind("INT", "int");
TokenKind.BOOL = new TokenKind("BOOL", "bool");
TokenKind.FLOAT = new TokenKind("FLOAT", "float");
TokenKind.STRING = new TokenKind("STRING", "string");

// Literals
TokenKind.IDENT = new TokenKind("IDENT", null);
TokenKind.INT_LIT = new TokenKind("INT_LIT", null);
TokenKind.FLOAT_LIT = new TokenKind("FLOAT_LIT", null);
TokenKind.STRING_LIT = new TokenKind("STRING_LIT", null);

// Other
TokenKind.LARGS = new TokenKind("LARGS", null);
TokenKind.RARGS = new TokenKind("RARGS", null);
TokenKind.IS_NOT = new TokenKind("IS_NOT", null);
TokenKind.NOT_IN = new TokenKind("NOT_IN", null);
TokenKind.NEWLINE = new TokenKind("NEWLINE", null);
TokenKind.COMMENT = new TokenKind("COMMENT", null);
TokenKind.END_OF_FILE = new TokenKind("END_OF_FILE", null);
TokenKind.ERROR = new TokenKind("ERROR", null);
TokenKind.SYMBOLS = [
  TokenKind.LPAREN,
  TokenKind.RPAREN,
  TokenKind.LBRACKET,
  TokenKind.RBRACKET,
  TokenKind.LBRACE,
  TokenKind.RBRACE,
  TokenKind.COMMA,
  TokenKind.SEMICOLON,
  TokenKind.SPLAT,
  TokenKind.DOT,
  TokenKind.QUESTION,
  TokenKind.ARROW,
  TokenKind.INC,
  TokenKind.DEC,
  TokenKind.ADD_ASSIGN,
  TokenKind.SUB_ASSIGN,
  TokenKind.MUL_ASSIGN,
  TokenKind.DIV_ASSIGN,
  TokenKind.MOD_ASSIGN,
  TokenKind.SHL_ASSIGN,
  TokenKind.SHR_ASSIGN,
  TokenKind.BITOR_ASSIGN,
  TokenKind.BITAND_ASSIGN,
  TokenKind.BITXOR_ASSIGN,
  TokenKind.ADD,
  TokenKind.SUB,
  TokenKind.MUL,
  TokenKind.DIV,
  TokenKind.MOD,
  TokenKind.SHL,
  TokenKind.SHR,
  TokenKind.BITOR,
  TokenKind.BITAND,
  TokenKind.BITXOR,
  TokenKind.EQ,
  TokenKind.NEQ,
  TokenKind.LTE,
  TokenKind.GTE,
  TokenKind.LT,
  TokenKind.GT,
  TokenKind.ASSIGN,
  TokenKind.BITNOT
];
TokenKind.IDENTIFIERS = [
  TokenKind.AND,
  TokenKind.OR,
  TokenKind.IS,
  TokenKind.IN,
  TokenKind.NOT,
  TokenKind.STATIC,
  TokenKind.OVER,
  TokenKind.ABSTRACT,
  TokenKind.EXTERN,
  TokenKind.PROP,
  TokenKind.RETURN,
  TokenKind.IF,
  TokenKind.ELSE,
  TokenKind.WHILE,
  TokenKind.FOR,
  TokenKind.CONTINUE,
  TokenKind.BREAK,
  TokenKind.FAIL,
  TokenKind.CLASS,
  TokenKind.FN,
  TokenKind.NULL,
  TokenKind.VOID,
  TokenKind.THIS,
  TokenKind.BASE,
  TokenKind.TRUE,
  TokenKind.FALSE,
  TokenKind.VAR,
  TokenKind.INT,
  TokenKind.BOOL,
  TokenKind.FLOAT,
  TokenKind.STRING
];
TokenKind.LEFT_BRACKETS = [
  TokenKind.LPAREN,
  TokenKind.LBRACKET,
  TokenKind.LBRACE
];
TokenKind.RIGHT_BRACKETS = [
  TokenKind.RPAREN,
  TokenKind.RBRACKET,
  TokenKind.RBRACE
];
TokenKind.REMOVE_NEWLINE_BEFORE = [
  TokenKind.RPAREN,
  TokenKind.RBRACKET,
  TokenKind.RBRACE,
  TokenKind.DOT,
  TokenKind.ELSE
];
TokenKind.REMOVE_NEWLINE_AFTER = [
  TokenKind.LPAREN,
  TokenKind.LBRACKET,
  TokenKind.LBRACE,
  TokenKind.COMMA,
  TokenKind.SEMICOLON,
  TokenKind.DOT,
  TokenKind.ARROW,
  TokenKind.ADD_ASSIGN,
  TokenKind.SUB_ASSIGN,
  TokenKind.MUL_ASSIGN,
  TokenKind.DIV_ASSIGN,
  TokenKind.MOD_ASSIGN,
  TokenKind.SHL_ASSIGN,
  TokenKind.SHR_ASSIGN,
  TokenKind.BITOR_ASSIGN,
  TokenKind.BITAND_ASSIGN,
  TokenKind.BITXOR_ASSIGN,
  TokenKind.ADD,
  TokenKind.SUB,
  TokenKind.MUL,
  TokenKind.DIV,
  TokenKind.MOD,
  TokenKind.SHL,
  TokenKind.SHR,
  TokenKind.BITOR,
  TokenKind.BITAND,
  TokenKind.BITXOR,
  TokenKind.EQ,
  TokenKind.NEQ,
  TokenKind.LTE,
  TokenKind.GTE,
  TokenKind.LT,
  TokenKind.GT,
  TokenKind.ASSIGN,
  TokenKind.BITNOT,
  TokenKind.AND,
  TokenKind.OR,
  TokenKind.IS,
  TokenKind.IN,
  TokenKind.NOT
];

// class Token
var Token = function(location, kind, text) {
  this.location = location;
  this.kind = kind;
  this.text = text;
};
Token.prototype.toString = function() {
  return this.kind.name + " " + JSON.stringify(this.text);
};

// class Tokenizer
var Tokenizer = function() {};
Tokenizer.isNumber = function(c) {
  return c >= "0" && c <= "9";
};
Tokenizer.isLetter = function(c) {
  return c >= "A" && c <= "Z" || c >= "a" && c <= "z" || c === "_";
};
Tokenizer.isSpace = function(c) {
  return c === " " || c === "\t" || c === "\r" || c === "\n";
};
Tokenizer.tokenize = function(log, file, text) {
  var t;
  var tokens = [];
  var line = 1;
  var i = 0;
  loop:
  while (i < text.length) {
    // Create an error token by default
    var token = new Token(new Location(file, line), TokenKind.ERROR, "");
    var start = i;

    // Parse int literal
    if (Tokenizer.isNumber(text[i])) {
      token.kind = TokenKind.INT_LIT;
      i++;
      while (i < text.length && Tokenizer.isNumber(text[i])) {
        i++;
      }

      // Parse float literal
      if (i + 1 < text.length && text[i] === "." && Tokenizer.isNumber(text[i + 1])) {
        token.kind = TokenKind.FLOAT_LIT;
        i += 2;
        while (i < text.length && Tokenizer.isNumber(text[i])) {
          i++;
        }
      }
    } else if (Tokenizer.isLetter(text[i])) {
      // Parse identifier
      token.kind = TokenKind.IDENT;
      i++;
      while (i < text.length && (Tokenizer.isLetter(text[i]) || Tokenizer.isNumber(text[i]))) {
        i++;
      }
      token.text = text.slice(start, i);
      for (var i3 = 0; i3 < TokenKind.IDENTIFIERS.length; i3++) {
        var kind = TokenKind.IDENTIFIERS[i3];
        if (kind.text === token.text) {
          token.kind = kind;
          break;
        }
      }
    } else if (Tokenizer.isSpace(text[i])) {
      // Parse whitespace
      while (i < text.length && Tokenizer.isSpace(text[i])) {
        if (text[i] === "\n") {
          token.kind = TokenKind.NEWLINE;
          line++;
        }
        i++;
      }
      if (token.kind === TokenKind.ERROR) {
        continue;
      }
      if (tokens.length > 0 && tokens[tokens.length - 1].kind === TokenKind.NEWLINE) {
        tokens[tokens.length - 1].text += text.slice(start, i);
        continue;
      }
    } else if (text[i] === "\"" || text[i] === "'") {
      // Parse string literal
      i++;
      while (i < text.length) {
        if (text[i] === text[start]) {
          token.kind = TokenKind.STRING_LIT;
          i++;
          break;
        } else if (text[i] === "\\" && i + 1 < text.length) {
          var c = text[++i];
          if (c === "\"" || c === "'" || c === "\\") {
            token.text += c;
          } else if (c === "t") {
            token.text += "\t";
          } else if (c === "r") {
            token.text += "\r";
          } else if (c === "n") {
            token.text += "\n";
          } else if (c === "x") {
            var hi = -1;
            var lo = -1;
            if (i + 2 < text.length) {
              hi = "0123456789abcdef".indexOf(text[++i].toLowerCase());
              lo = "0123456789abcdef".indexOf(text[++i].toLowerCase());
            }
            if (hi === -1 || lo === -1) {
              log.error(new Location(file, line), "byte escape sequence needs two hexadecimal characters");
              break loop;
            } else {
              token.text += String.fromCharCode(hi * 16 + lo);
            }
          } else {
            log.warning(new Location(file, line), "string contains invalid escape sequence \"\\" + text[i] + "\"");
            token.text += "\\" + text[i];
          }
          i++;
        } else {
          if (text[i] === "\n") {
            line++;
          }
          token.text += text[i++];
        }
      }
      if (token.kind === TokenKind.ERROR) {
        log.error(token.location, "unterminated string literal");
        break;
      }
      tokens.push(token);
      continue;
    } else if (text[i] === "/" && i + 1 < text.length && text[i + 1] === "/") {
      // Check for single line comments
      token.kind = TokenKind.COMMENT;
      i += 2;
      start = i;
      while (i < text.length) {
        if (text[i++] === "\n") {
          line++;
          break;
        }
      }
    } else if (text[i] === "/" && i + 1 < text.length && text[i + 1] === "+") {
      // Check for multi-line comments (they nest, unlike C-style comments)
      i += 2;
      var depth = 1;
      while (i + 1 < text.length) {
        if (text[i] === "/" && text[i + 1] === "+") {
          i += 2;
          depth++;
        } else if (text[i] === "+" && text[i + 1] === "/") {
          i += 2;
          if (--depth === 0) {
            break;
          }
        } else {
          if (text[i] === "\n") {
            line++;
          }
          i++;
        }
      }
      if (depth > 0) {
        log.error(token.location, "unterminated multi-line comment");
        break;
      }
      continue;
    } else if (text[i] === "\\") {
      // Backslashes escape newlines
      i++;
      while (text[i] !== "\n" && Tokenizer.isSpace(text[i])) {
        i++;
      }
      if (text[i] !== "\n") {
        log.error(token.location, "expected newline after backslash");
        break;
      }
      i++;
      continue;
    } else {
      // Check for symbols
      for (var i2 = 0; i2 < TokenKind.SYMBOLS.length; i2++) {
        var kind2 = TokenKind.SYMBOLS[i2];
        if ((t = kind2.text) !== null && text.slice(i, i + t.length) === t) {
          token.kind = kind2;
          i += t.length;
          break;
        }
      }

      // If we get here it's an error
      if (token.kind === TokenKind.ERROR) {
        log.error(token.location, "unexpected \"" + text[i] + "\"");
        break;
      }
    }

    // Set the text based on how far we've parsed
    token.text = text.slice(start, i);
    tokens.push(token);
  }

  // Every token string ends with an END_OF_FILE token
  tokens.push(new Token(new Location(file, line), TokenKind.END_OF_FILE, ""));

  // Post-process the parsed tokens
  var stack = [];
  i = 0;
  while (i < tokens.length) {
    var token2 = tokens[i++];

    // Try to merge this token with the previous token
    if (i >= 2) {
      var prev = tokens[i - 2];
      if (prev.kind === TokenKind.NOT && token2.kind === TokenKind.IN) {
        // Need to handle "not in" specially
        token2.kind = TokenKind.NOT_IN;
        tokens.splice(--i - 1, 1);
      } else if (prev.kind === TokenKind.IS && token2.kind === TokenKind.NOT) {
        // Need to handle "is not" specially
        token2.kind = TokenKind.IS_NOT;
        tokens.splice(--i - 1, 1);
      }
    }

    // Remove newlines based on previous token or next token to enable line
    // continuations. Note: Make sure to be conservative. We want to be
    // like CoffeeScript, not like JavaScript ASI! Anything that is at all
    // ambiguous should be disallowed.
    if (TokenKind.REMOVE_NEWLINE_BEFORE.indexOf(token2.kind) >= 0 && i >= 2 && tokens[i - 2].kind === TokenKind.NEWLINE) {
      tokens.splice(--i - 1, 1);
      continue;
    }
    if (token2.kind === TokenKind.NEWLINE && i >= 2 && TokenKind.REMOVE_NEWLINE_AFTER.indexOf(tokens[i - 2].kind) >= 0) {
      tokens.splice(--i, 1);
      continue;
    }

    // Keep track of the current bracket nesting using a stack
    if (TokenKind.LEFT_BRACKETS.indexOf(token2.kind) >= 0) {
      stack.push(token2);
    } else if (stack.length > 0 && TokenKind.RIGHT_BRACKETS.indexOf(token2.kind) >= 0) {
      var top = stack.pop();

      // Convert parentheses to lambda argument tokens
      if (top.kind === TokenKind.LPAREN && token2.kind === TokenKind.RPAREN && i < tokens.length && tokens[i].kind === TokenKind.ARROW) {
        top.kind = TokenKind.LARGS;
        token2.kind = TokenKind.RARGS;
      }
    }
  }
  return tokens;
};

// class UnaryOp
var UnaryOp = function() {};
UnaryOp.NEG = new UnaryOp();
UnaryOp.NOT = new UnaryOp();
UnaryOp.BITNOT = new UnaryOp();
UnaryOp.PRE_INC = new UnaryOp();
UnaryOp.PRE_DEC = new UnaryOp();
UnaryOp.POST_INC = new UnaryOp();
UnaryOp.POST_DEC = new UnaryOp();

// class BinaryOp
var BinaryOp = function() {};
BinaryOp.ADD_ASSIGN = new BinaryOp();
BinaryOp.SUB_ASSIGN = new BinaryOp();
BinaryOp.MUL_ASSIGN = new BinaryOp();
BinaryOp.DIV_ASSIGN = new BinaryOp();
BinaryOp.MOD_ASSIGN = new BinaryOp();
BinaryOp.SHL_ASSIGN = new BinaryOp();
BinaryOp.SHR_ASSIGN = new BinaryOp();
BinaryOp.BITOR_ASSIGN = new BinaryOp();
BinaryOp.BITAND_ASSIGN = new BinaryOp();
BinaryOp.BITXOR_ASSIGN = new BinaryOp();
BinaryOp.AND = new BinaryOp();
BinaryOp.OR = new BinaryOp();
BinaryOp.ADD = new BinaryOp();
BinaryOp.SUB = new BinaryOp();
BinaryOp.MUL = new BinaryOp();
BinaryOp.DIV = new BinaryOp();
BinaryOp.MOD = new BinaryOp();
BinaryOp.SHL = new BinaryOp();
BinaryOp.SHR = new BinaryOp();
BinaryOp.BITOR = new BinaryOp();
BinaryOp.BITAND = new BinaryOp();
BinaryOp.BITXOR = new BinaryOp();
BinaryOp.EQ = new BinaryOp();
BinaryOp.NEQ = new BinaryOp();
BinaryOp.LTE = new BinaryOp();
BinaryOp.GTE = new BinaryOp();
BinaryOp.LT = new BinaryOp();
BinaryOp.GT = new BinaryOp();
BinaryOp.ASSIGN = new BinaryOp();
BinaryOp.IS = new BinaryOp();
BinaryOp.IN = new BinaryOp();

////////////////////////////////////////////////////////////////////////////////
// Nodes
////////////////////////////////////////////////////////////////////////////////

// class Node
var Node = function(location) {
  this.location = location;
  this.scope = Scope.ERROR;
};
Node.prototype.accept = null;

// class Module
var Module = function(location, body) {
  Node.call(this, location);
  this.body = body;
};
Module.prototype = Object.create(Node.prototype);
Module.prototype.accept = function(v) {
  v.visitModule(this);
};

// class Block
var Block = function(location) {
  Node.call(this, location);
  this.stmts = [];
};
Block.prototype = Object.create(Node.prototype);
Block.prototype.accept = function(v) {
  v.visitBlock(this);
};

////////////////////////////////////////////////////////////////////////////////
// Statements
////////////////////////////////////////////////////////////////////////////////

// class Stmt
var Stmt = function(location, comment) {
  Node.call(this, location);
  this.comment = comment;
};
Stmt.prototype = Object.create(Node.prototype);

// class ExprStmt
var ExprStmt = function(location, comment, value) {
  Stmt.call(this, location, comment);
  this.value = value;
};
ExprStmt.prototype = Object.create(Stmt.prototype);
ExprStmt.prototype.accept = function(v) {
  v.visitExprStmt(this);
};

// class IfStmt
var IfStmt = function(location, comment, value, trueBody, falseBody) {
  Stmt.call(this, location, comment);
  this.value = value;
  this.trueBody = trueBody;
  this.falseBody = falseBody;
};
IfStmt.prototype = Object.create(Stmt.prototype);
IfStmt.prototype.accept = function(v) {
  v.visitIfStmt(this);
};

// class WhileStmt
var WhileStmt = function(location, comment, value, body) {
  Stmt.call(this, location, comment);
  this.value = value;
  this.body = body;
};
WhileStmt.prototype = Object.create(Stmt.prototype);
WhileStmt.prototype.accept = function(v) {
  v.visitWhileStmt(this);
};

// class ForStmt
var ForStmt = function(location, comment, id, value, body) {
  Stmt.call(this, location, comment);
  this.id = id;
  this.value = value;
  this.body = body;
};
ForStmt.prototype = Object.create(Stmt.prototype);
ForStmt.prototype.accept = function(v) {
  v.visitForStmt(this);
};

// class ContinueStmt
var ContinueStmt = function(location, comment, value) {
  Stmt.call(this, location, comment);
  this.value = value;
};
ContinueStmt.prototype = Object.create(Stmt.prototype);
ContinueStmt.prototype.accept = function(v) {
  v.visitContinueStmt(this);
};

// class BreakStmt
var BreakStmt = function(location, comment, value) {
  Stmt.call(this, location, comment);
  this.value = value;
};
BreakStmt.prototype = Object.create(Stmt.prototype);
BreakStmt.prototype.accept = function(v) {
  v.visitBreakStmt(this);
};

// class FailStmt
var FailStmt = function(location, comment, value) {
  Stmt.call(this, location, comment);
  this.value = value;
};
FailStmt.prototype = Object.create(Stmt.prototype);
FailStmt.prototype.accept = function(v) {
  v.visitFailStmt(this);
};

// class ReturnStmt
var ReturnStmt = function(location, comment, value) {
  Stmt.call(this, location, comment);
  this.value = value;
};
ReturnStmt.prototype = Object.create(Stmt.prototype);
ReturnStmt.prototype.accept = function(v) {
  v.visitReturnStmt(this);
};

// class CommentStmt
var CommentStmt = function(location, comment) {
  Stmt.call(this, location, comment);
};
CommentStmt.prototype = Object.create(Stmt.prototype);
CommentStmt.prototype.accept = function(v) {
  v.visitCommentStmt(this);
};

////////////////////////////////////////////////////////////////////////////////
// Definitions
////////////////////////////////////////////////////////////////////////////////

// class Def
var Def = function(location, comment, id, modifiers) {
  Stmt.call(this, location, comment);
  this.id = id;
  this.modifiers = modifiers;
};
Def.prototype = Object.create(Stmt.prototype);

// class VarDef
var VarDef = function(location, comment, id, modifiers, type, value) {
  Def.call(this, location, comment, id, modifiers);
  this.type = type;
  this.value = value;
  this.isSplat = false;
};
VarDef.prototype = Object.create(Def.prototype);
VarDef.prototype.accept = function(v) {
  v.visitVarDef(this);
};

// class FuncDef
var FuncDef = function(location, comment, id, modifiers, returnType, args, body) {
  Def.call(this, location, comment, id, modifiers);
  this.returnType = returnType;
  this.args = args;
  this.body = body;
};
FuncDef.prototype = Object.create(Def.prototype);
FuncDef.prototype.accept = function(v) {
  v.visitFuncDef(this);
};

// class CtorDef
var CtorDef = function(location, comment, id, modifiers, args, body) {
  Def.call(this, location, comment, id, modifiers);
  this.args = args;
  this.body = body;
};
CtorDef.prototype = Object.create(Def.prototype);
CtorDef.prototype.accept = function(v) {
  v.visitCtorDef(this);
};

// class ClassDef
var ClassDef = function(location, comment, id, modifiers, baseType, body) {
  Def.call(this, location, comment, id, modifiers);
  this.baseType = baseType;
  this.body = body;
};
ClassDef.prototype = Object.create(Def.prototype);
ClassDef.prototype.accept = function(v) {
  v.visitClassDef(this);
};

////////////////////////////////////////////////////////////////////////////////
// Expressions
////////////////////////////////////////////////////////////////////////////////

// class Expr
var Expr = function(location) {
  Node.call(this, location);
  this.resolvedType = SpecialType.ERROR;
};
Expr.prototype = Object.create(Node.prototype);

// class UnaryExpr
var UnaryExpr = function(location, op, value) {
  Expr.call(this, location);
  this.op = op;
  this.value = value;
};
UnaryExpr.prototype = Object.create(Expr.prototype);
UnaryExpr.prototype.accept = function(v) {
  v.visitUnaryExpr(this);
};

// class BinaryExpr
var BinaryExpr = function(location, left, op, right) {
  Expr.call(this, location);
  this.left = left;
  this.op = op;
  this.right = right;
};
BinaryExpr.prototype = Object.create(Expr.prototype);
BinaryExpr.prototype.accept = function(v) {
  v.visitBinaryExpr(this);
};

// class TertiaryExpr
var TertiaryExpr = function(location, trueValue, value, falseValue) {
  Expr.call(this, location);
  this.trueValue = trueValue;
  this.value = value;
  this.falseValue = falseValue;
};
TertiaryExpr.prototype = Object.create(Expr.prototype);
TertiaryExpr.prototype.accept = function(v) {
  v.visitTertiaryExpr(this);
};

// class CallExpr
var CallExpr = function(location, value, args) {
  Expr.call(this, location);
  this.value = value;
  this.args = args;
  this.ctorSymbol = null;
};
CallExpr.prototype = Object.create(Expr.prototype);
CallExpr.prototype.accept = function(v) {
  v.visitCallExpr(this);
};

// class IndexExpr
var IndexExpr = function(location, value, index) {
  Expr.call(this, location);
  this.value = value;
  this.index = index;
};
IndexExpr.prototype = Object.create(Expr.prototype);
IndexExpr.prototype.accept = function(v) {
  v.visitIndexExpr(this);
};

// class MemberExpr
var MemberExpr = function(location, value, id) {
  Expr.call(this, location);
  this.value = value;
  this.id = id;
};
MemberExpr.prototype = Object.create(Expr.prototype);
MemberExpr.prototype.accept = function(v) {
  v.visitMemberExpr(this);
};

// class InitExpr
var InitExpr = function(location, values) {
  Expr.call(this, location);
  this.values = values;
  this.ctorSymbol = null;
};
InitExpr.prototype = Object.create(Expr.prototype);
InitExpr.prototype.accept = function(v) {
  v.visitInitExpr(this);
};

// class MatchExpr
var MatchExpr = function(location, id, type, value) {
  Expr.call(this, location);
  this.id = id;
  this.type = type;
  this.value = value;
};
MatchExpr.prototype = Object.create(Expr.prototype);
MatchExpr.prototype.accept = function(v) {
  v.visitMatchExpr(this);
};

// class LambdaExpr
var LambdaExpr = function(location, args, body) {
  Expr.call(this, location);
  this.args = args;
  this.body = body;
};
LambdaExpr.prototype = Object.create(Expr.prototype);
LambdaExpr.prototype.accept = function(v) {
  v.visitLambdaExpr(this);
};

// class ArrayTypeExpr
var ArrayTypeExpr = function(location, elementType) {
  Expr.call(this, location);
  this.elementType = elementType;
};
ArrayTypeExpr.prototype = Object.create(Expr.prototype);
ArrayTypeExpr.prototype.accept = function(v) {
  v.visitArrayTypeExpr(this);
};

// class FuncTypeExpr
var FuncTypeExpr = function(location, returnType, args) {
  Expr.call(this, location);
  this.returnType = returnType;
  this.args = args;
};
FuncTypeExpr.prototype = Object.create(Expr.prototype);
FuncTypeExpr.prototype.accept = function(v) {
  v.visitFuncTypeExpr(this);
};

// class NullableTypeExpr
var NullableTypeExpr = function(location, innerType) {
  Expr.call(this, location);
  this.innerType = innerType;
};
NullableTypeExpr.prototype = Object.create(Expr.prototype);
NullableTypeExpr.prototype.accept = function(v) {
  v.visitNullableTypeExpr(this);
};

////////////////////////////////////////////////////////////////////////////////
// Literals
////////////////////////////////////////////////////////////////////////////////

// class TypeExpr
var TypeExpr = function(location, type) {
  Expr.call(this, location);
  this.type = type;
};
TypeExpr.prototype = Object.create(Expr.prototype);
TypeExpr.prototype.accept = function(v) {
  v.visitTypeExpr(this);
};

// class ThisExpr
var ThisExpr = function(location) {
  Expr.call(this, location);
};
ThisExpr.prototype = Object.create(Expr.prototype);
ThisExpr.prototype.accept = function(v) {
  v.visitThisExpr(this);
};

// class BaseExpr
var BaseExpr = function(location) {
  Expr.call(this, location);
};
BaseExpr.prototype = Object.create(Expr.prototype);
BaseExpr.prototype.accept = function(v) {
  v.visitBaseExpr(this);
};

// class IdentExpr
var IdentExpr = function(location, name) {
  Expr.call(this, location);
  this.name = name;
  this.symbol = null;
};
IdentExpr.prototype = Object.create(Expr.prototype);
IdentExpr.prototype.accept = function(v) {
  v.visitIdentExpr(this);
};

// class IntExpr
var IntExpr = function(location, value) {
  Expr.call(this, location);
  this.value = value;
};
IntExpr.prototype = Object.create(Expr.prototype);
IntExpr.prototype.accept = function(v) {
  v.visitIntExpr(this);
};

// class BoolExpr
var BoolExpr = function(location, value) {
  Expr.call(this, location);
  this.value = value;
};
BoolExpr.prototype = Object.create(Expr.prototype);
BoolExpr.prototype.accept = function(v) {
  v.visitBoolExpr(this);
};

// class FloatExpr
var FloatExpr = function(location, value) {
  Expr.call(this, location);
  this.value = value;
};
FloatExpr.prototype = Object.create(Expr.prototype);
FloatExpr.prototype.accept = function(v) {
  v.visitFloatExpr(this);
};

// class StringExpr
var StringExpr = function(location, value) {
  Expr.call(this, location);
  this.value = value;
};
StringExpr.prototype = Object.create(Expr.prototype);
StringExpr.prototype.accept = function(v) {
  v.visitStringExpr(this);
};

////////////////////////////////////////////////////////////////////////////////
// Visitor
////////////////////////////////////////////////////////////////////////////////

// class Visitor
var Visitor = function() {};

// Nodes
Visitor.prototype.visit = function(node) {
  var node2;
  if ((node2 = node) !== null) {
    node2.accept(this);
  }
};
Visitor.prototype.visitModule = function(node) {
  this.visit(node.body);
};
Visitor.prototype.visitBlock = function(node) {
  for (var i = 0; i < node.stmts.length; i++) {
    var stmt = node.stmts[i];
    this.visit(stmt);
  }
};

// Statements
Visitor.prototype.visitExprStmt = function(node) {
  this.visit(node.value);
};
Visitor.prototype.visitIfStmt = function(node) {
  this.visit(node.value);
  this.visit(node.trueBody);
  this.visit(node.falseBody);
};
Visitor.prototype.visitWhileStmt = function(node) {
  this.visit(node.value);
  this.visit(node.body);
};
Visitor.prototype.visitForStmt = function(node) {
  this.visit(node.id);
  this.visit(node.value);
  this.visit(node.body);
};
Visitor.prototype.visitContinueStmt = function(node) {};
Visitor.prototype.visitBreakStmt = function(node) {};
Visitor.prototype.visitFailStmt = function(node) {
  this.visit(node.value);
};
Visitor.prototype.visitReturnStmt = function(node) {
  this.visit(node.value);
};
Visitor.prototype.visitCommentStmt = function(node) {};

// Definitions
Visitor.prototype.visitVarDef = function(node) {
  this.visit(node.id);
  this.visit(node.type);
  this.visit(node.value);
};
Visitor.prototype.visitFuncDef = function(node) {
  this.visit(node.id);
  this.visit(node.returnType);
  for (var i = 0; i < node.args.length; i++) {
    var arg = node.args[i];
    this.visit(arg);
  }
  this.visit(node.body);
};
Visitor.prototype.visitCtorDef = function(node) {
  this.visit(node.id);
  for (var i = 0; i < node.args.length; i++) {
    var arg = node.args[i];
    this.visit(arg);
  }
  this.visit(node.body);
};
Visitor.prototype.visitClassDef = function(node) {
  this.visit(node.id);
  this.visit(node.baseType);
  this.visit(node.body);
};

// Expressions
Visitor.prototype.visitUnaryExpr = function(node) {
  this.visit(node.value);
};
Visitor.prototype.visitBinaryExpr = function(node) {
  this.visit(node.left);
  this.visit(node.right);
};
Visitor.prototype.visitTertiaryExpr = function(node) {
  this.visit(node.trueValue);
  this.visit(node.value);
  this.visit(node.falseValue);
};
Visitor.prototype.visitCallExpr = function(node) {
  this.visit(node.value);
  for (var i = 0; i < node.args.length; i++) {
    var arg = node.args[i];
    this.visit(arg);
  }
};
Visitor.prototype.visitIndexExpr = function(node) {
  this.visit(node.value);
  this.visit(node.index);
};
Visitor.prototype.visitMemberExpr = function(node) {
  this.visit(node.id);
  this.visit(node.value);
};
Visitor.prototype.visitInitExpr = function(node) {
  for (var i = 0; i < node.values.length; i++) {
    var value = node.values[i];
    this.visit(value);
  }
};
Visitor.prototype.visitMatchExpr = function(node) {
  this.visit(node.id);
  this.visit(node.type);
  this.visit(node.value);
};
Visitor.prototype.visitLambdaExpr = function(node) {
  for (var i = 0; i < node.args.length; i++) {
    var arg = node.args[i];
    this.visit(arg);
  }
  this.visit(node.body);
};
Visitor.prototype.visitArrayTypeExpr = function(node) {
  this.visit(node.elementType);
};
Visitor.prototype.visitFuncTypeExpr = function(node) {
  this.visit(node.returnType);
  for (var i = 0; i < node.args.length; i++) {
    var arg = node.args[i];
    this.visit(arg);
  }
};
Visitor.prototype.visitNullableTypeExpr = function(node) {
  this.visit(node.innerType);
};

// Literals
Visitor.prototype.visitTypeExpr = function(node) {};
Visitor.prototype.visitThisExpr = function(node) {};
Visitor.prototype.visitBaseExpr = function(node) {};
Visitor.prototype.visitIdentExpr = function(node) {};
Visitor.prototype.visitIntExpr = function(node) {};
Visitor.prototype.visitBoolExpr = function(node) {};
Visitor.prototype.visitFloatExpr = function(node) {};
Visitor.prototype.visitStringExpr = function(node) {};

// class Modifier
var Modifier = function() {};
Modifier.STATIC = 1 << 0;
Modifier.OVER = 1 << 1;
Modifier.ABSTRACT = 1 << 2;
Modifier.EXTERN = 1 << 3;
Modifier.PROP = 1 << 4;

// class SymbolKind
var SymbolKind = function() {};
SymbolKind.DUMMY = new SymbolKind();
SymbolKind.CLASS = new SymbolKind();
SymbolKind.VARIABLE = new SymbolKind();
SymbolKind.FUNCTION = new SymbolKind();

// class Symbol
var Symbol = function(kind, name, type, scope, modifiers) {
  this.kind = kind;
  this.name = name;
  this.type = type;
  this.scope = scope;
  this.modifiers = modifiers;

  // The symbol that this overrides in the base class (requires "over")
  this.baseSymbol = null;

  // The original name for mangling purposes (reserved words)
  this.originalName = name;

  // The original scope for codegen purposes (match expressions)
  this.originalScope = scope;
};
Symbol.prototype.toString = function() {
  return "\"" + this.name + "\" of type " + this.type;
};

// class ScopeKind
var ScopeKind = function() {};
ScopeKind.ERROR = new ScopeKind();
ScopeKind.MODULE = new ScopeKind();
ScopeKind.STATIC = new ScopeKind();
ScopeKind.INSTANCE = new ScopeKind();
ScopeKind.FUNCTION = new ScopeKind();
ScopeKind.BLOCK = new ScopeKind();
ScopeKind.MATCH = new ScopeKind();
ScopeKind.LOOP = new ScopeKind();

// class Scope
var Scope = function(kind, parentScope, baseScope) {
  this.kind = kind;
  this.parentScope = parentScope;
  this.baseScope = baseScope;

  // The associated symbol (the class or the loop variable, for example)
  this.symbol = null;
  this.symbols = [];
};
Scope.ERROR = new Scope(ScopeKind.ERROR, null, null);
Scope.prototype.define = function(log, location, kind, name, type, modifiers) {
  if (this.find(name) !== null) {
    log.error(location, "\"" + name + "\" is already defined in this scope");
  }
  var symbol = new Symbol(kind, name, type, this, modifiers);
  this.symbols.push(symbol);
  return symbol;
};
Scope.prototype.find = function(name) {
  for (var i = 0; i < this.symbols.length; i++) {
    var symbol = this.symbols[i];
    if (symbol.name === name) {
      return symbol;
    }
  }
  return null;
};
Scope.prototype.lookup = function(name, isMember) {
  var scope, parentScope;

  // Search this scope
  var symbol = this.find(name);
  if (symbol !== null) {
    return symbol;
  }

  // Search the base scope
  var nextScope = this.baseScope;
  while ((scope = nextScope) !== null) {
    symbol = scope.find(name);
    if (symbol !== null) {
      return symbol;
    }
    nextScope = scope.baseScope;
  }

  // Search the parent scope
  if ((this.kind !== ScopeKind.STATIC || !isMember) && (parentScope = this.parentScope) !== null) {
    return parentScope.lookup(name, isMember);
  }
  return null;
};

// class Type
var Type = function() {};
Type.prototype.toString = null;
Type.paramsFor = function(type) {
  var type2;
  if ((type2 = type) instanceof ArrayType) {
    return [type2.elementType];
  }
  return [];
};
Type.isPrimitive = function(type) {
  return [
    ClassType.INT,
    ClassType.BOOL,
    ClassType.FLOAT,
    ClassType.STRING
  ].indexOf(type) >= 0;
};
Type.removeParams = function(type, params) {
  var type2, type3, type4, type5;
  if ((type2 = type) instanceof TypeParam) {
    if (type2.index >= params.length) {
      throw new Error("fail in src/type.bend on line 20");
    }
    return params[type2.index];
  }
  if ((type3 = type) instanceof MetaType) {
    var instanceType = Type.removeParams(type3.instanceType, params);
    if (instanceType === type3.instanceType) {
      return type3;
    }
    return new MetaType(instanceType);
  }
  if ((type4 = type) instanceof ArrayType) {
    var elementType = Type.removeParams(type4.elementType, params);
    if (elementType === type4.elementType) {
      return type4;
    }
    return new ArrayType(elementType);
  }
  if ((type5 = type) instanceof FuncType) {
    var returnType = Type.removeParams(type5.returnType, params);
    var changed = returnType !== type5.returnType;
    var argTypes = [];
    for (var i = 0; i < type5.argTypes.length; i++) {
      var oldArgType = type5.argTypes[i];
      var newArgType = Type.removeParams(oldArgType, params);
      argTypes.push(newArgType);
      if (newArgType !== oldArgType) {
        changed = true;
      }
    }
    if (!changed) {
      return type5;
    }
    var funcType = new FuncType(returnType, argTypes, type5.minArgCount);
    funcType.argNames = type5.argNames;
    return funcType;
  }
  return type;
};
Type.equals = function(a, b) {
  var a2, b2, a3, b3, a4, b4, a5, b5;
  if (a === b || a === SpecialType.VAR || b === SpecialType.VAR) {
    return true;
  }
  if ((a2 = a) instanceof NullableType && (b2 = b) instanceof NullableType) {
    return Type.equals(a2.innerType, b2.innerType);
  }
  if ((a3 = a) instanceof MetaType && (b3 = b) instanceof MetaType) {
    return Type.equals(a3.instanceType, b3.instanceType);
  }
  if ((a4 = a) instanceof ArrayType && (b4 = b) instanceof ArrayType) {
    return Type.equals(a4.elementType, b4.elementType);
  }
  if ((a5 = a) instanceof FuncType && (b5 = b) instanceof FuncType) {
    if (!Type.equals(a5.returnType, b5.returnType) || a5.argTypes.length !== b5.argTypes.length || a5.minArgCount !== b5.minArgCount) {
      return false;
    }
    var i = 0;
    while (i < a5.argTypes.length) {
      if (!Type.equals(a5.argTypes[i], b5.argTypes[i])) {
        return false;
      }
      i++;
    }
    return true;
  }
  return false;
};
Type.canImplicitlyConvert = function(from, to) {
  var to2, from2, from3, to3, baseType;
  if (Type.equals(from, to)) {
    return true;
  }
  if ((to2 = to) instanceof NullableType) {
    if (from === SpecialType.NULL) {
      return true;
    }
    if ((from2 = from) instanceof NullableType) {
      return Type.canImplicitlyConvert(from2.innerType, to2.innerType);
    }
    return Type.canImplicitlyConvert(from, to2.innerType);
  }
  if ((from3 = from) instanceof ClassType && (to3 = to) instanceof ClassType) {
    while (true) {
      if (Type.equals(from3, to3)) {
        return true;
      } else if ((baseType = from3.baseType) !== null) {
        from3 = baseType;
      } else {
        break;
      }
    }
  }
  return false;
};

// class TypeParam
var TypeParam = function(index) {
  Type.call(this);
  this.index = index;
};
TypeParam.prototype = Object.create(Type.prototype);
TypeParam.T0 = new TypeParam(0);
TypeParam.prototype.toString = function() {
  return "<" + this.index + ">";
};

// class SpecialType
var SpecialType = function(name) {
  Type.call(this);
  this.name = name;
};
SpecialType.prototype = Object.create(Type.prototype);
SpecialType.ERROR = new SpecialType("<error>");
SpecialType.NULL = new SpecialType("null");
SpecialType.VOID = new SpecialType("void");
SpecialType.VAR = new SpecialType("var");

// Don't use MetaType for void because we want to restrict it to return types
SpecialType.TYPE_VOID = new SpecialType("type void");
SpecialType.prototype.toString = function() {
  return this.name;
};

// class NullableType
var NullableType = function(innerType) {
  Type.call(this);
  this.innerType = innerType;
};
NullableType.prototype = Object.create(Type.prototype);
NullableType.prototype.toString = function() {
  return this.innerType + "?";
};

// class MetaType
var MetaType = function(instanceType) {
  Type.call(this);
  this.instanceType = instanceType;
};
MetaType.prototype = Object.create(Type.prototype);
MetaType.VAR = new MetaType(SpecialType.VAR);
MetaType.prototype.toString = function() {
  return "type " + this.instanceType;
};

// class ArrayType
var ArrayType = function(elementType) {
  Type.call(this);
  this.elementType = elementType;
};
ArrayType.prototype = Object.create(Type.prototype);
ArrayType.prototype.toString = function() {
  return this.elementType + "[]";
};

// class FuncType
var FuncType = function(returnType, argTypes, minArgCount) {
  Type.call(this);
  this.returnType = returnType;
  this.argTypes = argTypes;

  // If all arguments are required, minArgCount == argTypes.length
  this.minArgCount = minArgCount;

  // A splat can take zero or more arguments, -1 means no splat
  this.splatIndex = -1;

  // Argument names are currently used for automatically generated constructors,
  // but may also be used for keyword arguments in the future. Must be the same
  // length as argTypes if non-null.
  this.argNames = null;
};
FuncType.prototype = Object.create(Type.prototype);
FuncType.prototype.toString = function() {
  var args = [];
  var i = 0;
  while (i < this.argTypes.length) {
    var arg = this.argTypes[i].toString();
    if (i > this.minArgCount) {
      arg = "?" + arg;
    }
    if (i === this.splatIndex) {
      arg += "...";
    }
    args.push(arg);
    i++;
  }
  return this.returnType + " fn(" + args.join(", ") + ")";
};

// class ClassType
var ClassType = function(name, staticScope, instanceScope, isAbstract) {
  Type.call(this);
  this.name = name;
  this.staticScope = staticScope;
  this.instanceScope = instanceScope;
  this.isAbstract = isAbstract;
  this.baseType = null;
  this.metaType = new MetaType(this);
};
ClassType.prototype = Object.create(Type.prototype);
ClassType.prototype.toString = function() {
  return this.name;
};
ClassType.makeNative = function(name) {
  var staticScope = new Scope(ScopeKind.STATIC, null, null);
  var instanceScope = new Scope(ScopeKind.INSTANCE, staticScope, null);
  var classType = new ClassType(name, staticScope, instanceScope, false);
  var funcType = new FuncType(SpecialType.VOID, [classType], 1);
  var symbol = new Symbol(SymbolKind.FUNCTION, "this", funcType, instanceScope, 0);
  staticScope.symbol = symbol;
  instanceScope.symbol = symbol;
  instanceScope.symbols.push(symbol);
  return classType;
};
ClassType.INT = ClassType.makeNative("int");
ClassType.BOOL = ClassType.makeNative("bool");
ClassType.FLOAT = ClassType.makeNative("float");
ClassType.STRING = ClassType.makeNative("string");
ClassType.INT.baseType = ClassType.FLOAT;
if ((ctorSymbol = ClassType.INT.instanceScope.find("this")) !== null && (funcType = ctorSymbol.type) instanceof FuncType) {
  funcType.argTypes[0] = ClassType.FLOAT;
}

// class Parselet
var Parselet = function(kind, leftBindingPower) {
  this.kind = kind;
  this.leftBindingPower = leftBindingPower;
  this.prefixParselet = null;
  this.infixParselet = null;
};

// class Pratt
var Pratt = function() {
  this.table = [];
};
Pratt.prototype.find = function(kind) {
  for (var i = 0; i < this.table.length; i++) {
    var parselet = this.table[i];
    if (parselet.kind === kind) {
      return parselet;
    }
  }
  return null;
};
Pratt.prototype.parse = function(context, rightBindingPower) {
  var parselet, prefixParselet, left, parselet2, infixParselet;
  if ((parselet = this.find(context.current().kind)) !== null && (prefixParselet = parselet.prefixParselet) !== null) {
    var maybeLeft = prefixParselet(context);
    while ((left = maybeLeft) !== null && (parselet2 = this.find(context.current().kind)) !== null && (infixParselet = parselet2.infixParselet) !== null && rightBindingPower < parselet2.leftBindingPower) {
      maybeLeft = infixParselet(context, left);
    }
    return maybeLeft;
  } else {
    context.log.error(context.current().location, "unexpected " + context.current().kind.name);
  }
  return null;
};
Pratt.prototype.get = function(kind, bindingPower) {
  var parselet;
  if ((parselet = this.find(kind)) !== null) {
    if (bindingPower > parselet.leftBindingPower) {
      parselet.leftBindingPower = bindingPower;
    }
    return parselet;
  }
  var parselet2 = new Parselet(kind, bindingPower);
  this.table.push(parselet2);
  return parselet2;
};
Pratt.prototype.literal = function(kind, callback) {
  this.get(kind, 0).prefixParselet = function(context) {
    var token = context.current();
    context.next();
    return callback(token);
  };
};
Pratt.prototype.prefix = function(kind, bindingPower, callback) {
  var that = this;
  this.get(kind, 0).prefixParselet = function(context) {
    var value;
    var token = context.current();
    context.next();
    if ((value = that.parse(context, bindingPower)) !== null) {
      return callback(context, token, value);
    }
    return null;
  };
};
Pratt.prototype.postfix = function(kind, bindingPower, callback) {
  this.get(kind, bindingPower).infixParselet = function(context, left) {
    var token = context.current();
    context.next();
    return callback(context, left, token);
  };
};
Pratt.prototype.infix = function(kind, bindingPower, rightAssociative, callback) {
  var that = this;
  var rightPower = bindingPower;
  if (rightAssociative) {
    rightPower--;
  }
  this.get(kind, bindingPower).infixParselet = function(context, left) {
    var right;
    var token = context.current();
    context.next();
    if ((right = that.parse(context, rightPower)) !== null) {
      return callback(context, left, token, right);
    }
    return null;
  };
};

// class Context
var Context = function(log, tokens) {
  this.log = log;
  this.tokens = tokens;
  this.index = 0;
};
Context.prototype.current = function() {
  return this.tokens[this.index];
};
Context.prototype.next = function() {
  if (this.index + 1 < this.tokens.length) {
    this.index++;
  }
};
Context.prototype.peek = function(kind) {
  return this.current().kind === kind;
};
Context.prototype.peekMore = function(kind, inc) {
  if (this.index + inc >= this.tokens.length) {
    return this.tokens[this.tokens.length - 1].kind === kind;
  }
  return this.tokens[this.index + inc].kind === kind;
};
Context.prototype.eat = function(kind) {
  if (this.peek(kind)) {
    this.next();
    return true;
  }
  return false;
};
Context.prototype.expect = function(kind) {
  var text = this.current().text;
  if (this.eat(kind)) {
    return true;
  }
  this.log.error(this.current().location, "expected " + kind.name + " but got " + this.current().kind.name);
  return false;
};

// class BlockKind
var BlockKind = function() {};
BlockKind.NORMAL = new BlockKind();
BlockKind.MODULE = new BlockKind();
BlockKind.CLASS = new BlockKind();

// class UnaryInfo
var UnaryInfo = function(kind, op) {
  this.kind = kind;
  this.op = op;
};
UnaryInfo.PREFIX_INFO = [
  new UnaryInfo(TokenKind.SUB, UnaryOp.NEG),
  new UnaryInfo(TokenKind.NOT, UnaryOp.NOT),
  new UnaryInfo(TokenKind.BITNOT, UnaryOp.BITNOT),
  new UnaryInfo(TokenKind.INC, UnaryOp.PRE_INC),
  new UnaryInfo(TokenKind.DEC, UnaryOp.PRE_DEC)
];
UnaryInfo.POSTFIX_INFO = [
  new UnaryInfo(TokenKind.INC, UnaryOp.POST_INC),
  new UnaryInfo(TokenKind.DEC, UnaryOp.POST_DEC)
];

// class BinaryInfo
var BinaryInfo = function(kind, op, precedence) {
  this.kind = kind;
  this.op = op;
  this.precedence = precedence;
};
BinaryInfo.INFO = [
  new BinaryInfo(TokenKind.ASSIGN, BinaryOp.ASSIGN, 10),
  new BinaryInfo(TokenKind.ADD_ASSIGN, BinaryOp.ADD_ASSIGN, 10),
  new BinaryInfo(TokenKind.SUB_ASSIGN, BinaryOp.SUB_ASSIGN, 10),
  new BinaryInfo(TokenKind.MUL_ASSIGN, BinaryOp.MUL_ASSIGN, 10),
  new BinaryInfo(TokenKind.DIV_ASSIGN, BinaryOp.DIV_ASSIGN, 10),
  new BinaryInfo(TokenKind.MOD_ASSIGN, BinaryOp.MOD_ASSIGN, 10),
  new BinaryInfo(TokenKind.SHL_ASSIGN, BinaryOp.SHL_ASSIGN, 10),
  new BinaryInfo(TokenKind.SHR_ASSIGN, BinaryOp.SHR_ASSIGN, 10),
  new BinaryInfo(TokenKind.BITOR_ASSIGN, BinaryOp.BITOR_ASSIGN, 10),
  new BinaryInfo(TokenKind.BITAND_ASSIGN, BinaryOp.BITAND_ASSIGN, 10),
  new BinaryInfo(TokenKind.BITXOR_ASSIGN, BinaryOp.BITXOR_ASSIGN, 10),
  new BinaryInfo(TokenKind.OR, BinaryOp.OR, 30),
  new BinaryInfo(TokenKind.AND, BinaryOp.AND, 40),
  new BinaryInfo(TokenKind.BITOR, BinaryOp.BITOR, 50),
  new BinaryInfo(TokenKind.BITXOR, BinaryOp.BITXOR, 60),
  new BinaryInfo(TokenKind.BITAND, BinaryOp.BITAND, 70),
  new BinaryInfo(TokenKind.EQ, BinaryOp.EQ, 80),
  new BinaryInfo(TokenKind.NEQ, BinaryOp.NEQ, 80),
  new BinaryInfo(TokenKind.LT, BinaryOp.LT, 90),
  new BinaryInfo(TokenKind.GT, BinaryOp.GT, 90),
  new BinaryInfo(TokenKind.LTE, BinaryOp.LTE, 90),
  new BinaryInfo(TokenKind.GTE, BinaryOp.GTE, 90),
  new BinaryInfo(TokenKind.IS, BinaryOp.IS, 90),
  new BinaryInfo(TokenKind.IN, BinaryOp.IN, 90),
  new BinaryInfo(TokenKind.SHL, BinaryOp.SHL, 100),
  new BinaryInfo(TokenKind.SHR, BinaryOp.SHR, 100),
  new BinaryInfo(TokenKind.ADD, BinaryOp.ADD, 110),
  new BinaryInfo(TokenKind.SUB, BinaryOp.SUB, 110),
  new BinaryInfo(TokenKind.MUL, BinaryOp.MUL, 120),
  new BinaryInfo(TokenKind.DIV, BinaryOp.DIV, 120),
  new BinaryInfo(TokenKind.MOD, BinaryOp.MOD, 120)
];

// class Parser
var Parser = function() {
  var that = this;
  this.pratt = new Pratt();
  this.prattWithoutMatch = new Pratt();
  var ASSIGNMENT_PRECEDENCE = 10;
  var TERTIARY_PRECEDENCE = 20;
  var MATCH_VALUE_PRECEDENCE = 40;
  var IS_AND_IN_PRECEDENCE = 90;
  var UNARY_PRECEDENCE = 130;
  var MATCH_PRECEDENCE = 140;
  var TIGHT_PRECEDENCE = 150;

  // Literals
  this.pratt.literal(TokenKind.NULL, function(token) {
    return new TypeExpr(token.location, SpecialType.NULL);
  });
  this.pratt.literal(TokenKind.VOID, function(token) {
    return new TypeExpr(token.location, SpecialType.TYPE_VOID);
  });
  this.pratt.literal(TokenKind.VAR, function(token) {
    return new TypeExpr(token.location, MetaType.VAR);
  });
  this.pratt.literal(TokenKind.INT, function(token) {
    return new TypeExpr(token.location, ClassType.INT.metaType);
  });
  this.pratt.literal(TokenKind.BOOL, function(token) {
    return new TypeExpr(token.location, ClassType.BOOL.metaType);
  });
  this.pratt.literal(TokenKind.FLOAT, function(token) {
    return new TypeExpr(token.location, ClassType.FLOAT.metaType);
  });
  this.pratt.literal(TokenKind.STRING, function(token) {
    return new TypeExpr(token.location, ClassType.STRING.metaType);
  });
  this.pratt.literal(TokenKind.THIS, function(token) {
    return new ThisExpr(token.location);
  });
  this.pratt.literal(TokenKind.BASE, function(token) {
    return new BaseExpr(token.location);
  });
  this.pratt.literal(TokenKind.TRUE, function(token) {
    return new BoolExpr(token.location, true);
  });
  this.pratt.literal(TokenKind.FALSE, function(token) {
    return new BoolExpr(token.location, false);
  });
  this.pratt.literal(TokenKind.INT_LIT, function(token) {
    return new IntExpr(token.location, token.text | 0);
  });
  this.pratt.literal(TokenKind.FLOAT_LIT, function(token) {
    return new FloatExpr(token.location, +token.text);
  });
  this.pratt.literal(TokenKind.STRING_LIT, function(token) {
    return new StringExpr(token.location, token.text);
  });

  // Parenthesized group
  this.pratt.get(TokenKind.LPAREN, 0).prefixParselet = function(context) {
    var node;
    context.next();
    if ((node = that.pratt.parse(context, 0)) !== null && context.expect(TokenKind.RPAREN)) {
      return node;
    }
    return null;
  };

  // Identifier or lambda shortcut
  this.pratt.get(TokenKind.IDENT, 0).prefixParselet = function(context) {
    var token = context.current();
    context.next();
    if (context.eat(TokenKind.ARROW)) {
      return that.parseLambdaBody(context, token.location, [new IdentExpr(token.location, token.text)]);
    }
    return new IdentExpr(token.location, token.text);
  };

  // Unary prefix operators
  for (var i2 = 0; i2 < UnaryInfo.PREFIX_INFO.length; i2++) {
    with ({ env$0: {} }) {
      env$0.info = UnaryInfo.PREFIX_INFO[i2];
      this.pratt.prefix(env$0.info.kind, UNARY_PRECEDENCE, function(context, token, value) {
        if ((env$0.info.op === UnaryOp.PRE_INC || env$0.info.op === UnaryOp.PRE_DEC) && !that.isLeftHandSide(context, value)) {
          return null;
        }
        return new UnaryExpr(token.location, env$0.info.op, value);
      });
    }
  }

  // Unary postfix operators
  for (var i3 = 0; i3 < UnaryInfo.POSTFIX_INFO.length; i3++) {
    with ({ env$1: {} }) {
      env$1.info2 = UnaryInfo.POSTFIX_INFO[i3];
      this.pratt.postfix(env$1.info2.kind, UNARY_PRECEDENCE, function(context, value, token) {
        if ((env$1.info2.op === UnaryOp.POST_INC || env$1.info2.op === UnaryOp.POST_DEC) && !that.isLeftHandSide(context, value)) {
          return null;
        }
        return new UnaryExpr(token.location, env$1.info2.op, value);
      });
    }
  }

  // Binary operators
  for (var i4 = 0; i4 < BinaryInfo.INFO.length; i4++) {
    with ({ env$2: {} }) {
      env$2.info3 = BinaryInfo.INFO[i4];
      var precedence = env$2.info3.precedence;
      env$2.isAssignment = precedence === ASSIGNMENT_PRECEDENCE;
      this.pratt.infix(env$2.info3.kind, precedence, env$2.isAssignment, function(context, left, token, right) {
        if (env$2.isAssignment && !that.isLeftHandSide(context, left)) {
          return null;
        }
        return new BinaryExpr(token.location, left, env$2.info3.op, right);
      });
    }
  }

  // Compound binary operators
  this.pratt.infix(TokenKind.IS_NOT, IS_AND_IN_PRECEDENCE, false, function(context, left, token, right) {
    return new UnaryExpr(token.location, UnaryOp.NOT, new BinaryExpr(token.location, left, BinaryOp.IS, right));
  });
  this.pratt.infix(TokenKind.NOT_IN, IS_AND_IN_PRECEDENCE, false, function(context, left, token, right) {
    return new UnaryExpr(token.location, UnaryOp.NOT, new BinaryExpr(token.location, left, BinaryOp.IN, right));
  });

  // Tertiary operator
  this.pratt.get(TokenKind.IF, TERTIARY_PRECEDENCE).infixParselet = function(context, left) {
    var middle, right;
    context.next();
    if ((middle = that.pratt.parse(context, TERTIARY_PRECEDENCE)) !== null && context.expect(TokenKind.ELSE) && (right = that.pratt.parse(context, TERTIARY_PRECEDENCE)) !== null) {
      return new TertiaryExpr(middle.location, left, middle, right);
    }
  };

  // Initializer list
  this.pratt.get(TokenKind.LBRACE, 0).prefixParselet = function(context) {
    var value;
    var location = context.current().location;
    context.next();
    var values = [];
    while (!context.eat(TokenKind.RBRACE)) {
      // Ignore comments before initializer list items
      while (context.eat(TokenKind.COMMENT)) {
        context.eat(TokenKind.NEWLINE);
      }

      // Parse the list item
      if ((value = that.pratt.parse(context, 0)) !== null) {
        values.push(value);
      } else {
        return null;
      }

      // Ignore comments after initializer list items
      while (context.eat(TokenKind.COMMENT)) {
        context.eat(TokenKind.NEWLINE);
      }

      // Check if there may be more items
      if (!context.eat(TokenKind.NEWLINE) && !context.eat(TokenKind.COMMA)) {
        if (!context.expect(TokenKind.RBRACE)) {
          return null;
        }
        break;
      }
    }
    return new InitExpr(location, values);
  };

  // Function call
  this.pratt.get(TokenKind.LPAREN, TIGHT_PRECEDENCE).infixParselet = function(context, left) {
    var arg;
    var location = context.current().location;
    context.next();
    var args = [];
    while (!context.eat(TokenKind.RPAREN)) {
      if (args.length > 0 && !context.expect(TokenKind.COMMA)) {
        return null;
      }
      if ((arg = that.pratt.parse(context, 0)) !== null) {
        args.push(arg);
      } else {
        return null;
      }
    }
    return new CallExpr(location, left, args);
  };

  // Nullable type
  this.pratt.postfix(TokenKind.QUESTION, TIGHT_PRECEDENCE, function(context, value, token) {
    return new NullableTypeExpr(token.location, value);
  });

  // Function type
  this.pratt.get(TokenKind.FN, TIGHT_PRECEDENCE).infixParselet = function(context, left) {
    var arg;
    var location = context.current().location;
    context.next();
    if (!context.expect(TokenKind.LPAREN)) {
      return null;
    }
    var args = [];
    while (!context.eat(TokenKind.RPAREN)) {
      if (args.length > 0 && !context.expect(TokenKind.COMMA)) {
        return null;
      }
      if ((arg = that.pratt.parse(context, 0)) !== null) {
        args.push(arg);
      } else {
        return null;
      }
    }
    return new FuncTypeExpr(location, left, args);
  };

  // Index access
  this.pratt.get(TokenKind.LBRACKET, TIGHT_PRECEDENCE).infixParselet = function(context, left) {
    var node;
    var location = context.current().location;
    context.next();
    if (context.eat(TokenKind.RBRACKET)) {
      return new ArrayTypeExpr(location, left);
    }
    if ((node = that.pratt.parse(context, 0)) !== null && context.expect(TokenKind.RBRACKET)) {
      return new IndexExpr(location, left, node);
    }
    return null;
  };

  // Member access
  this.pratt.get(TokenKind.DOT, TIGHT_PRECEDENCE).infixParselet = function(context, left) {
    var location = context.current().location;
    context.next();
    var id = new IdentExpr(context.current().location, context.current().text);
    if (context.expect(TokenKind.IDENT)) {
      return new MemberExpr(location, left, id);
    } else {
      return null;
    }
  };

  // Lambda expression shortcut
  this.pratt.get(TokenKind.ARROW, 0).prefixParselet = function(context) {
    var location = context.current().location;
    context.next();
    return that.parseLambdaBody(context, location, []);
  };

  // Lambda expression
  this.pratt.get(TokenKind.LARGS, 0).prefixParselet = function(context) {
    var location = context.current().location;
    var args = [];
    context.next();
    while (!context.eat(TokenKind.RARGS)) {
      if (args.length > 0 && !context.expect(TokenKind.COMMA)) {
        return null;
      }
      var token = context.current();
      args.push(new IdentExpr(token.location, context.current().text));
      if (!context.expect(TokenKind.IDENT)) {
        return null;
      }
    }
    if (!context.expect(TokenKind.ARROW)) {
      return null;
    }
    return that.parseLambdaBody(context, location, args);
  };

  // Match expression
  this.pratt.get(TokenKind.IDENT, MATCH_PRECEDENCE).infixParselet = function(context, left) {
    var value;
    var id = new IdentExpr(context.current().location, context.current().text);
    context.next();
    if (!context.expect(TokenKind.ASSIGN)) {
      return null;
    }
    if ((value = that.pratt.parse(context, MATCH_VALUE_PRECEDENCE)) !== null) {
      return new MatchExpr(id.location, id, left, value);
    } else {
      return null;
    }
  };

  // Create another pratt parser that doesn't parse top-level match expressions
  this.prattWithoutMatch.table = this.pratt.table.slice();
  var i = 0;
  while (i < this.prattWithoutMatch.table.length) {
    var oldParselet = this.prattWithoutMatch.table[i];
    if (oldParselet.kind === TokenKind.IDENT) {
      var newParselet = new Parselet(oldParselet.kind, oldParselet.leftBindingPower);
      newParselet.prefixParselet = oldParselet.prefixParselet;
      this.prattWithoutMatch.table[i] = newParselet;
      break;
    }
    i++;
  }
};
Parser.prototype.parse = function(log, tokens) {
  var block;
  var context = new Context(log, tokens);
  var location = context.current().location;
  if ((block = this.parseBlock(context, BlockKind.MODULE, "")) !== null) {
    return new Module(location, block);
  }
  return null;
};
Parser.prototype.parseLambdaBody = function(context, location, args) {
  var body, value;
  if (context.peek(TokenKind.LBRACE)) {
    if ((body = this.parseBlock(context, BlockKind.NORMAL, "")) !== null) {
      return new LambdaExpr(location, args, body);
    } else {
      return null;
    }
  }
  if ((value = this.pratt.parse(context, 0)) !== null) {
    var body2 = new Block(value.location);
    body2.stmts.push(new ReturnStmt(value.location, null, value));
    return new LambdaExpr(location, args, body2);
  }
  return null;
};
Parser.prototype.eatEndOfLine = function(context) {
  return context.eat(TokenKind.SEMICOLON) || context.eat(TokenKind.NEWLINE) || context.peek(TokenKind.RBRACE) || context.peek(TokenKind.RBRACKET) || context.peek(TokenKind.END_OF_FILE);
};
Parser.prototype.eatCommentedEndOfLine = function(context, stmt) {
  var existing;
  var comment = context.current().text;
  if (context.eat(TokenKind.COMMENT)) {
    if ((existing = stmt.comment) !== null) {
      stmt.comment = existing + comment;
    } else {
      stmt.comment = comment;
    }
    context.eat(TokenKind.NEWLINE);
    return true;
  }
  return this.eatEndOfLine(context);
};
Parser.prototype.expectEndOfLine = function(context) {
  if (this.eatEndOfLine(context)) {
    return true;
  }
  return context.expect(TokenKind.NEWLINE);
};
Parser.prototype.expectCommentedEndOfLine = function(context, stmt) {
  if (this.eatCommentedEndOfLine(context, stmt)) {
    return true;
  }
  return context.expect(TokenKind.NEWLINE);
};
Parser.prototype.isLeftHandSide = function(context, node) {
  if (node instanceof IdentExpr || node instanceof MemberExpr || node instanceof IndexExpr) {
    return true;
  }
  context.log.error(node.location, "invalid left-hand side in assignment");
  return false;
};
Parser.prototype.appendComment = function(start, end) {
  var start2;
  if ((start2 = start) !== null) {
    return start2 + end;
  }
  return end;
};
Parser.prototype.insertCommentInBlock = function(comment, block) {
  if (block.stmts.length > 0 && block.stmts[0].comment === null) {
    block.stmts[0].comment = comment;
  } else {
    block.stmts.unshift(new CommentStmt(block.location, comment));
  }
};
Parser.prototype.parseFunctionArgs = function(context) {
  var type;
  var args = [];
  var splats = 0;
  while (!context.eat(TokenKind.RPAREN)) {
    if (args.length > 0 && !context.expect(TokenKind.COMMA)) {
      return null;
    }
    if ((type = this.prattWithoutMatch.parse(context, 0)) !== null) {
      var arg = new VarDef(type.location, null, new IdentExpr(context.current().location, context.current().text), 0, type, null);
      if (!context.expect(TokenKind.IDENT)) {
        return null;
      }
      if (context.eat(TokenKind.SPLAT)) {
        arg.isSplat = true;
        if (++splats > 1) {
          context.log.error(arg.location, "function has multiple splats");
          return null;
        }
      }
      args.push(arg);
    } else {
      return null;
    }
  }
  return args;
};
Parser.prototype.parseFunctionBody = function(context) {
  if (context.peek(TokenKind.LBRACE) || context.peek(TokenKind.NEWLINE) && context.peekMore(TokenKind.LBRACE, 1)) {
    return this.parseBlock(context, BlockKind.NORMAL, "");
  }
  return null;
};

// Blocks are parsed as a unit instead of as individual statements because
// each statement may compile into zero or multiple statements. For example,
// declaring multiple comma-separated variables on the same line generates
// one VarDef statement for each variable.
Parser.prototype.parseBlock = function(context, kind, className) {
  var value, trueBody, value2, trueBody2, body, value3, body2, value4, body3, binaryExpr, id, value5, body4, args, type, id4, binaryExpr2, id5, args2;
  var block = new Block(context.current().location);
  var stop = TokenKind.RBRACE;

  // Parse the start of the block
  if (kind === BlockKind.MODULE) {
    stop = TokenKind.END_OF_FILE;
  } else {
    // Allow an optional newline before the left brace to support different styles
    context.eat(TokenKind.NEWLINE);
    if (!context.expect(TokenKind.LBRACE)) {
      return null;
    }
  }
  context.eat(TokenKind.NEWLINE);

  // Keep parsing statements until the stop token
  loop:
  while (!context.eat(stop)) {
    var location = context.current().location;

    // See if this statement has one or more comment lines
    var comment = null;
    while (context.peek(TokenKind.COMMENT)) {
      var appended = this.appendComment(comment, context.current().text);
      comment = appended;
      context.next();

      // Generate a separate statement if this is is a comment block
      if (context.eat(TokenKind.NEWLINE)) {
        block.stmts.push(new CommentStmt(location, appended));
        comment = null;
        if (context.eat(stop)) {
          break loop;
        }
      } else if (context.eat(stop)) {
        block.stmts.push(new CommentStmt(location, appended));
        break loop;
      }
    }

    // If statement, contains lots of tricky comment logic:
    // 
    //   // body 1
    //   if true {}
    //   // body 2
    //   else if false {}
    //   // body 3
    //   else {}
    // 
    // Trailing comments are also supported:
    // 
    //   if true {} // body 1
    //   else if false {} // body 2
    //   else {} // body 3
    // 
    if (context.eat(TokenKind.IF)) {
      if ((value = this.pratt.parse(context, 0)) !== null && (trueBody = this.parseBlock(context, BlockKind.NORMAL, "")) !== null) {
        var ifStmt = new IfStmt(location, comment, value, trueBody, null);
        var blockForTrailingComment = ifStmt.trueBody;
        block.stmts.push(ifStmt);
        while (true) {
          // Is there a TokenKind.ELSE, optionally preceded by a newline and
          // comments?
          // 
          // This is the only part of the parse that needs arbitrary lookahead.
          // It could be reduced to constant lookahead with a token prepass
          // that merges TokenKind.COMMENT tokens, but that's tricky because
          // trailing comments should not be merged. Abitrary lookahead here
          // seems simpler:
          // 
          //   int a // Trailing comment for a
          //   // Regular comment for b
          //   int b
          // 
          var index = 0;
          if (context.peek(TokenKind.COMMENT)) {
            index++;
          } else if (context.peek(TokenKind.NEWLINE)) {
            index++;
            while (context.peekMore(TokenKind.COMMENT, index)) {
              index++;
            }
          }
          if (!context.peekMore(TokenKind.ELSE, index)) {
            break;
          }

          // Actually parse the comment now that we know there is an "else"
          comment = null;
          if (context.peek(TokenKind.COMMENT)) {
            this.insertCommentInBlock(context.current().text, blockForTrailingComment);
            context.next();
          } else if (context.eat(TokenKind.NEWLINE)) {
            while (context.peek(TokenKind.COMMENT)) {
              comment = this.appendComment(comment, context.current().text);
              context.next();
            }
          }
          context.eat(TokenKind.ELSE);

          // Parse the else block
          location = context.current().location;
          if (context.eat(TokenKind.IF)) {
            if ((value2 = this.pratt.parse(context, 0)) !== null && (trueBody2 = this.parseBlock(context, BlockKind.NORMAL, "")) !== null) {
              var nestedIfStmt = new IfStmt(location, null, value2, trueBody2, null);
              var falseBody = new Block(location);
              this.insertCommentInBlock(comment, trueBody2);
              falseBody.stmts.push(nestedIfStmt);
              ifStmt.falseBody = falseBody;
              ifStmt = nestedIfStmt;
              blockForTrailingComment = trueBody2;
            } else {
              return null;
            }
          } else if ((body = this.parseBlock(context, BlockKind.NORMAL, "")) !== null) {
            this.insertCommentInBlock(comment, body);
            ifStmt.falseBody = body;
            blockForTrailingComment = body;
            break;
          } else {
            return null;
          }
        }
        if (context.peek(TokenKind.COMMENT)) {
          this.insertCommentInBlock(context.current().text, blockForTrailingComment);
          context.next();
        }
        if (!this.expectEndOfLine(context)) {
          return null;
        }
        continue;
      } else {
        return null;
      }
    }

    // While statement
    if (context.eat(TokenKind.WHILE)) {
      if ((value3 = this.pratt.parse(context, 0)) !== null && (body2 = this.parseBlock(context, BlockKind.NORMAL, "")) !== null) {
        var whileStmt = new WhileStmt(location, comment, value3, body2);
        if (!this.expectCommentedEndOfLine(context, whileStmt)) {
          return null;
        }
        block.stmts.push(whileStmt);
        continue;
      } else {
        return null;
      }
    }

    // For-in statement
    if (context.eat(TokenKind.FOR)) {
      if ((value4 = this.pratt.parse(context, 0)) !== null && (body3 = this.parseBlock(context, BlockKind.NORMAL, "")) !== null) {
        if ((binaryExpr = value4) instanceof BinaryExpr && binaryExpr.op === BinaryOp.IN && (id = binaryExpr.left) instanceof IdentExpr) {
          var forStmt = new ForStmt(location, comment, id, binaryExpr.right, body3);
          if (!this.expectCommentedEndOfLine(context, forStmt)) {
            return null;
          }
          block.stmts.push(forStmt);
          continue;
        } else {
          context.log.error(location, "invalid for-in statement");
          return null;
        }
      } else {
        return null;
      }
    }

    // Fail statement
    if (context.eat(TokenKind.FAIL)) {
      // Generate a default failure message
      var text = "fail " + location;
      var failStmt = new FailStmt(location, comment, new StringExpr(location, text));

      // Check for a custom failure message
      if (!this.eatCommentedEndOfLine(context, failStmt)) {
        if ((value5 = this.pratt.parse(context, 0)) !== null) {
          failStmt.value = value5;
          if (!this.expectCommentedEndOfLine(context, failStmt)) {
            return null;
          }
        } else {
          return null;
        }
      }
      block.stmts.push(failStmt);
      continue;
    }

    // Return statement
    if (context.eat(TokenKind.RETURN)) {
      var returnStmt = new ReturnStmt(location, comment, null);
      if (!this.eatCommentedEndOfLine(context, returnStmt)) {
        returnStmt.value = this.pratt.parse(context, 0);
        if (returnStmt.value === null || !this.expectCommentedEndOfLine(context, returnStmt)) {
          return null;
        }
      }
      block.stmts.push(returnStmt);
      continue;
    }

    // Continue statement
    if (context.eat(TokenKind.CONTINUE)) {
      var text2 = context.current().text;
      var value6 = 1;
      if (context.eat(TokenKind.INT_LIT)) {
        value6 = text2 | 0;
      }
      var continueStmt = new ContinueStmt(location, comment, value6);
      if (!this.expectCommentedEndOfLine(context, continueStmt)) {
        return null;
      }
      block.stmts.push(continueStmt);
      continue;
    }

    // Break statement
    if (context.eat(TokenKind.BREAK)) {
      var text3 = context.current().text;
      var value7 = 1;
      if (context.eat(TokenKind.INT_LIT)) {
        value7 = text3 | 0;
      }
      var breakStmt = new BreakStmt(location, comment, value7);
      if (!this.expectCommentedEndOfLine(context, breakStmt)) {
        return null;
      }
      block.stmts.push(breakStmt);
      continue;
    }

    // Parse modifiers for definitions in a specific order
    var modifiers = 0;
    if (context.eat(TokenKind.EXTERN)) {
      modifiers |= Modifier.EXTERN;
    }
    if (context.eat(TokenKind.ABSTRACT)) {
      modifiers |= Modifier.ABSTRACT;
    }
    if (context.eat(TokenKind.STATIC)) {
      modifiers |= Modifier.STATIC;
    }
    if (context.eat(TokenKind.OVER)) {
      modifiers |= Modifier.OVER;
    }
    if (context.eat(TokenKind.PROP)) {
      var propName = context.current().text;
      if (!context.expect(TokenKind.IDENT)) {
        return null;
      }
      modifiers |= Modifier.PROP;
    }

    // Class definition
    if (context.eat(TokenKind.CLASS)) {
      var id2 = new IdentExpr(context.current().location, context.current().text);
      if (!context.expect(TokenKind.IDENT)) {
        return null;
      }
      var baseType = null;
      if (context.eat(TokenKind.IS)) {
        baseType = this.pratt.parse(context, 0);
        if (baseType === null) {
          return null;
        }
      }
      if ((body4 = this.parseBlock(context, BlockKind.CLASS, id2.name)) !== null) {
        var classDef = new ClassDef(location, comment, id2, modifiers, baseType, body4);
        if (!this.expectCommentedEndOfLine(context, classDef)) {
          return null;
        }
        block.stmts.push(classDef);
        continue;
      } else {
        return null;
      }
    }

    // Constructor definition
    if (kind === BlockKind.CLASS && context.eat(TokenKind.THIS)) {
      var id3 = new IdentExpr(context.current().location, "this");
      if (!context.expect(TokenKind.LPAREN)) {
        return null;
      }
      if ((args = this.parseFunctionArgs(context)) !== null) {
        var body5 = this.parseFunctionBody(context);
        var ctorDef = new CtorDef(location, comment, id3, modifiers, args, body5);
        if (!this.expectCommentedEndOfLine(context, ctorDef)) {
          return null;
        }
        block.stmts.push(ctorDef);
        continue;
      } else {
        return null;
      }
    }

    // Typed definition
    if ((type = this.prattWithoutMatch.parse(context, 0)) !== null) {
      // Parse an expression statement
      if (modifiers === 0 && !context.peek(TokenKind.IDENT)) {
        var stmt;
        if (kind === BlockKind.CLASS) {
          // Free identifiers and simple assignments inside a class definition
          // are converted to static instance variables of the same type as the
          // class they are contained in to support (more powerful) enum-like
          // behavior.
          if ((id4 = type) instanceof IdentExpr) {
            stmt = new VarDef(location, comment, id4, Modifier.STATIC, new IdentExpr(location, className), new InitExpr(location, []));
          } else if ((binaryExpr2 = type) instanceof BinaryExpr && binaryExpr2.op === BinaryOp.ASSIGN && (id5 = binaryExpr2.left) instanceof IdentExpr) {
            stmt = new VarDef(location, comment, id5, Modifier.STATIC, new IdentExpr(location, className), binaryExpr2.right);
          } else {
            context.log.error(location, "free expression not allowed in class definition");
            return null;
          }
        } else {
          stmt = new ExprStmt(location, comment, type);
        }
        if (!this.expectCommentedEndOfLine(context, stmt)) {
          return null;
        }
        block.stmts.push(stmt);
        continue;
      }

      // Symbol definition
      var id6 = new IdentExpr(context.current().location, context.current().text);
      if (!context.expect(TokenKind.IDENT)) {
        return null;
      }

      // Function definition
      if (context.eat(TokenKind.LPAREN)) {
        if ((args2 = this.parseFunctionArgs(context)) !== null) {
          var body6 = this.parseFunctionBody(context);
          var funcDef = new FuncDef(location, comment, id6, modifiers, type, args2, body6);
          if (!this.expectCommentedEndOfLine(context, funcDef)) {
            return null;
          }
          block.stmts.push(funcDef);
          continue;
        } else {
          return null;
        }
      }

      // Variable definition
      while (true) {
        var value8 = null;
        if (context.eat(TokenKind.ASSIGN)) {
          value8 = this.pratt.parse(context, 0);
          if (value8 === null) {
            return null;
          }
        }
        var varDef = new VarDef(location, comment, id6, modifiers, type, value8);
        block.stmts.push(varDef);

        // The beginning comment only applies to the first variable
        comment = null;
        if (!context.eat(TokenKind.COMMA)) {
          if (!this.expectCommentedEndOfLine(context, varDef)) {
            return null;
          }
          break;
        }
        id6 = new IdentExpr(context.current().location, context.current().text);
        if (!context.expect(TokenKind.IDENT)) {
          return null;
        }
      }
    } else {
      return null;
    }
  }
  return block;
};

// class Pass
var Pass = function(log) {
  Visitor.call(this);
  this.log = log;
};
Pass.prototype = Object.create(Visitor.prototype);
Pass.prototype.instanceType = function(node, allowVoid) {
  var metaType;
  if (node.resolvedType === SpecialType.TYPE_VOID) {
    if (allowVoid) {
      return SpecialType.VOID;
    }
    this.log.error(node.location, "type void is not allowed here");
    return SpecialType.ERROR;
  }
  if ((metaType = node.resolvedType) instanceof MetaType) {
    return metaType.instanceType;
  }
  this.log.error(node.location, node.resolvedType + " is not a type description");
  return SpecialType.ERROR;
};

// class CreateScopesPass
var CreateScopesPass = function(log) {
  Pass.call(this, log);
  this.scope = new Scope(ScopeKind.MODULE, null, null);
};
CreateScopesPass.prototype = Object.create(Pass.prototype);
CreateScopesPass.prototype.visitModule = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitModule.call(this, node);
};
CreateScopesPass.prototype.visitBlock = function(node) {
  node.scope = this.scope;
  this.scope = new Scope(ScopeKind.BLOCK, this.scope, null);
  Pass.prototype.visitBlock.call(this, node);
  this.scope = node.scope;
};
CreateScopesPass.prototype.visitExprStmt = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitExprStmt.call(this, node);
};
CreateScopesPass.prototype.visitIfStmt = function(node) {
  node.scope = this.scope;

  // Use the scope of the expression for the true branch in case it contains
  // one or more match expressions
  this.visit(node.value);
  this.scope = node.value.scope;
  this.visit(node.trueBody);
  this.scope = node.scope;
  this.visit(node.falseBody);
};
CreateScopesPass.prototype.visitWhileStmt = function(node) {
  node.scope = this.scope;

  // Use the scope of the expression for the loop body in case it contains
  // one or more match expressions
  this.visit(node.value);
  this.scope = node.value.scope;
  this.scope = new Scope(ScopeKind.LOOP, this.scope, null);
  this.visit(node.body);
  this.scope = node.scope;
};
CreateScopesPass.prototype.visitForStmt = function(node) {
  node.scope = this.scope;
  this.visit(node.value);
  this.scope = new Scope(ScopeKind.LOOP, this.scope, null);
  this.scope.define(this.log, node.location, SymbolKind.VARIABLE, node.id.name, SpecialType.ERROR, 0);
  this.visit(node.id);
  this.visit(node.body);
  this.scope = node.scope;
};
CreateScopesPass.prototype.visitContinueStmt = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitContinueStmt.call(this, node);
};
CreateScopesPass.prototype.visitBreakStmt = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitBreakStmt.call(this, node);
};
CreateScopesPass.prototype.visitFailStmt = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitFailStmt.call(this, node);
};
CreateScopesPass.prototype.visitReturnStmt = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitReturnStmt.call(this, node);
};
CreateScopesPass.prototype.visitCommentStmt = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitCommentStmt.call(this, node);
};
CreateScopesPass.prototype.visitVarDef = function(node) {
  node.scope = this.scope;
  node.id.symbol = this.scope.define(this.log, node.location, SymbolKind.VARIABLE, node.id.name, SpecialType.ERROR, node.modifiers);
  Pass.prototype.visitVarDef.call(this, node);
};
CreateScopesPass.prototype.visitFuncDef = function(node) {
  node.scope = this.scope;
  node.id.symbol = this.scope.define(this.log, node.location, SymbolKind.FUNCTION, node.id.name, SpecialType.ERROR, node.modifiers);
  this.scope = new Scope(ScopeKind.FUNCTION, this.scope, null);
  this.scope.symbol = node.id.symbol;
  Pass.prototype.visitFuncDef.call(this, node);
  this.scope = node.scope;
};
CreateScopesPass.prototype.visitCtorDef = function(node) {
  node.scope = this.scope;
  node.id.symbol = this.scope.define(this.log, node.location, SymbolKind.FUNCTION, node.id.name, SpecialType.ERROR, node.modifiers);
  this.scope = new Scope(ScopeKind.FUNCTION, this.scope, null);
  this.scope.symbol = node.id.symbol;
  Pass.prototype.visitCtorDef.call(this, node);
  this.scope = node.scope;
};
CreateScopesPass.prototype.visitClassDef = function(node) {
  var def, def2;
  node.scope = this.scope;
  var staticScope = new Scope(ScopeKind.STATIC, this.scope, null);
  var instanceScope = new Scope(ScopeKind.INSTANCE, staticScope, null);
  var classType = new ClassType(node.id.name, staticScope, instanceScope, (node.modifiers & Modifier.ABSTRACT) !== 0);
  node.id.symbol = this.scope.define(this.log, node.location, SymbolKind.CLASS, node.id.name, classType.metaType, node.modifiers);
  staticScope.symbol = node.id.symbol;
  instanceScope.symbol = node.id.symbol;
  this.visit(node.id);
  this.visit(node.baseType);
  for (var i = 0; i < node.body.stmts.length; i++) {
    var stmt = node.body.stmts[i];
    if ((def = stmt) instanceof VarDef) {
      if ((def.modifiers & Modifier.STATIC) !== 0) {
        this.scope = staticScope;
      } else {
        this.scope = instanceScope;
      }
    } else if ((def2 = stmt) instanceof FuncDef) {
      if ((def2.modifiers & Modifier.STATIC) !== 0) {
        this.scope = staticScope;
      } else {
        this.scope = instanceScope;
      }
    } else if (stmt instanceof CtorDef) {
      this.scope = instanceScope;
    } else if (stmt instanceof ClassDef) {
      this.scope = staticScope;
    } else {
      this.scope = node.scope;
    }
    this.visit(stmt);
  }
  this.scope = node.scope;
};
CreateScopesPass.prototype.visitUnaryExpr = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitUnaryExpr.call(this, node);
};
CreateScopesPass.prototype.visitBinaryExpr = function(node) {
  node.scope = this.scope;
  if (node.op === BinaryOp.AND) {
    // For short-circuit and expressions, wrap the right expression in the
    // left expression's scope since new variables from match expressions
    // in the left expression should propagate into the right expression.
    // Also set this node's scope to the scope of the right expression
    // because we will need to propagate it into the true branch of if
    // statements.
    var parentScope = this.scope;
    this.visit(node.left);
    this.scope = node.left.scope;
    this.visit(node.right);
    node.scope = node.right.scope;
    this.scope = parentScope;
  } else {
    Pass.prototype.visitBinaryExpr.call(this, node);
  }
};
CreateScopesPass.prototype.visitTertiaryExpr = function(node) {
  node.scope = this.scope;

  // Use the scope of the expression for the true branch in case it contains
  // one or more match expressions
  this.visit(node.value);
  this.scope = node.value.scope;
  this.visit(node.trueValue);
  this.scope = node.scope;
  this.visit(node.falseValue);
};
CreateScopesPass.prototype.visitCallExpr = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitCallExpr.call(this, node);
};
CreateScopesPass.prototype.visitIndexExpr = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitIndexExpr.call(this, node);
};
CreateScopesPass.prototype.visitMemberExpr = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitMemberExpr.call(this, node);
};
CreateScopesPass.prototype.visitInitExpr = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitInitExpr.call(this, node);
};
CreateScopesPass.prototype.visitMatchExpr = function(node) {
  node.scope = new Scope(ScopeKind.MATCH, this.scope, null);
  node.scope.define(this.log, node.location, SymbolKind.VARIABLE, node.id.name, SpecialType.ERROR, 0);
  Pass.prototype.visitMatchExpr.call(this, node);
  node.id.scope = node.scope;
};
CreateScopesPass.prototype.visitLambdaExpr = function(node) {
  node.scope = this.scope;
  this.scope = new Scope(ScopeKind.FUNCTION, this.scope, null);
  for (var i = 0; i < node.args.length; i++) {
    var arg = node.args[i];
    this.scope.define(this.log, node.location, SymbolKind.VARIABLE, arg.name, SpecialType.ERROR, 0);
  }
  Pass.prototype.visitLambdaExpr.call(this, node);
  this.scope = node.scope;
};
CreateScopesPass.prototype.visitArrayTypeExpr = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitArrayTypeExpr.call(this, node);
};
CreateScopesPass.prototype.visitFuncTypeExpr = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitFuncTypeExpr.call(this, node);
};
CreateScopesPass.prototype.visitNullableTypeExpr = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitNullableTypeExpr.call(this, node);
};
CreateScopesPass.prototype.visitTypeExpr = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitTypeExpr.call(this, node);
};
CreateScopesPass.prototype.visitThisExpr = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitThisExpr.call(this, node);
};
CreateScopesPass.prototype.visitBaseExpr = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitBaseExpr.call(this, node);
};
CreateScopesPass.prototype.visitIdentExpr = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitIdentExpr.call(this, node);
};
CreateScopesPass.prototype.visitIntExpr = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitIntExpr.call(this, node);
};
CreateScopesPass.prototype.visitBoolExpr = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitBoolExpr.call(this, node);
};
CreateScopesPass.prototype.visitFloatExpr = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitFloatExpr.call(this, node);
};
CreateScopesPass.prototype.visitStringExpr = function(node) {
  node.scope = this.scope;
  Pass.prototype.visitStringExpr.call(this, node);
};

// class ResolveBaseTypesPass
var ResolveBaseTypesPass = function(log) {
  Pass.call(this, log);
  this.resolveTypesPass = new ResolveTypesPass(log);
};
ResolveBaseTypesPass.prototype = Object.create(Pass.prototype);
ResolveBaseTypesPass.prototype.visitClassDef = function(node) {
  var baseType, metaType, baseClassType, symbol, metaType2, derivedType, classType;
  if ((baseType = node.baseType) !== null) {
    // Resolve and validate the base type
    this.resolveTypesPass.visit(baseType);
    if ((metaType = baseType.resolvedType) instanceof MetaType && (baseClassType = metaType.instanceType) instanceof ClassType && (symbol = node.id.symbol) !== null && (metaType2 = symbol.type) instanceof MetaType && (derivedType = metaType2.instanceType) instanceof ClassType) {
      // Don't create a cycle
      var maybeClass = baseClassType;
      while ((classType = maybeClass) !== null) {
        if (classType === derivedType) {
          this.log.error(baseType.location, "base type creates cycle");
          return;
        }
        maybeClass = classType.baseType;
      }

      // If we get here, we know setting the base type won't cause a cycle
      derivedType.baseType = baseClassType;
      derivedType.staticScope.baseScope = baseClassType.staticScope;
      derivedType.instanceScope.baseScope = baseClassType.instanceScope;
    } else {
      this.log.error(baseType.location, "could not resolve base type of " + node.id.name);
    }
  }
  Pass.prototype.visitClassDef.call(this, node);
};

// class ResolveSymbolTypesPass
var ResolveSymbolTypesPass = function(log) {
  Pass.call(this, log);
  this.resolveTypesPass = new ResolveTypesPass(log);
};
ResolveSymbolTypesPass.prototype = Object.create(Pass.prototype);
ResolveSymbolTypesPass.prototype.visitVarDef = function(node) {
  var symbol;
  Pass.prototype.visitVarDef.call(this, node);
  this.resolveTypesPass.visit(node.type);
  if ((symbol = node.id.symbol) !== null) {
    symbol.type = this.instanceType(node.type, false);
  }
};
ResolveSymbolTypesPass.prototype.visitFuncDef = function(node) {
  var symbol;
  Pass.prototype.visitFuncDef.call(this, node);
  this.resolveTypesPass.visit(node.returnType);
  var argTypes = [];
  var argNames = [];
  var splatIndex = -1;
  var i = 0;
  for (var i2 = 0; i2 < node.args.length; i2++) {
    var arg = node.args[i2];
    var argType = this.instanceType(arg.type, false);
    argTypes.push(argType);
    argNames.push(arg.id.name);
    if (arg.isSplat) {
      if (!(argType instanceof ArrayType)) {
        this.log.error(arg.location, "splats must be arrays");
      }
      splatIndex = i;
    }
    i++;
  }
  var funcType = new FuncType(this.instanceType(node.returnType, true), argTypes, argTypes.length);
  if (splatIndex !== -1) {
    funcType.minArgCount--;
  }
  funcType.splatIndex = splatIndex;
  funcType.argNames = argNames;
  if ((symbol = node.id.symbol) !== null) {
    symbol.type = funcType;
  }
};
ResolveSymbolTypesPass.prototype.visitCtorDef = function(node) {
  var symbol;
  Pass.prototype.visitCtorDef.call(this, node);
  var argTypes = [];
  var argNames = [];
  for (var i = 0; i < node.args.length; i++) {
    var arg = node.args[i];
    argTypes.push(this.instanceType(arg.type, false));
    argNames.push(arg.id.name);
  }
  var funcType = new FuncType(SpecialType.VOID, argTypes, argTypes.length);
  funcType.argNames = argNames;
  if ((symbol = node.id.symbol) !== null) {
    symbol.type = funcType;
  }
};

// class CheckOverridePass
var CheckOverridePass = function(log) {
  Pass.call(this, log);
};
CheckOverridePass.prototype = Object.create(Pass.prototype);
CheckOverridePass.prototype.visitFuncDef = function(node) {
  var symbol, scope, baseSymbol;

  // Check if this function overrides another function in the base class
  var overridesSymbol = false;
  if ((symbol = node.id.symbol) !== null) {
    var baseScope = node.scope.baseScope;
    while ((scope = baseScope) !== null) {
      if ((baseSymbol = scope.find(node.id.name)) !== null) {
        if (!Type.equals(baseSymbol.type, symbol.type)) {
          this.log.error(node.location, "function \"" + node.id.name + "\" of type " + symbol.type + " overrides symbol \"" + baseSymbol.name + "\" of different type " + baseSymbol.type);
          return;
        }
        overridesSymbol = true;
        break;
      }
      baseScope = scope.baseScope;
    }
  }

  // Emit an error if the "over" annotation is incorrect
  var shouldOverrideSymbol = (node.modifiers & Modifier.OVER) !== 0;
  if (overridesSymbol && !shouldOverrideSymbol) {
    this.log.error(node.location, "function \"" + node.id.name + "\" overrides a function in a base class and must be marked \"over\"");
  } else if (!overridesSymbol && shouldOverrideSymbol) {
    this.log.error(node.location, "function \"" + node.id.name + "\" is marked \"over\" but doesn't override anything");
  } else if (overridesSymbol && node.body === null) {
    this.log.error(node.location, "function \"" + node.id.name + "\" cannot be an override without a function body");
  }
};

// class GenerateCtorsPass
var GenerateCtorsPass = function(log) {
  Pass.call(this, log);
  this.resolveSymbolTypesPass = new ResolveSymbolTypesPass(log);
  this.createScopesPass = new CreateScopesPass(log);
};
GenerateCtorsPass.prototype = Object.create(Pass.prototype);
GenerateCtorsPass.prototype.visitClassDef = function(node) {
  var symbol, metaType, classType, stmt2, body, varDef, value, baseType, ctorSymbol, funcType, argNames, varDef2, value2;
  Pass.prototype.visitClassDef.call(this, node);

  // If the body of the class doesn't have any constructors, then generate
  // some automatically. One constructor is generated with one argument per
  // field (including inherited members) and, if there are any fields with
  // default values, another constructor is generated without arguments for
  // those fields.
  if ((symbol = node.id.symbol) !== null && (metaType = symbol.type) instanceof MetaType && (classType = metaType.instanceType) instanceof ClassType) {
    var location = node.location;
    for (var i4 = 0; i4 < node.body.stmts.length; i4++) {
      var stmt = node.body.stmts[i4];
      if ((stmt2 = stmt) instanceof CtorDef) {
        // Insert initializers for member variables with default initializations
        if ((body = stmt2.body) !== null) {
          var initializers = [];
          for (var i2 = 0; i2 < node.body.stmts.length; i2++) {
            var stmt3 = node.body.stmts[i2];
            if ((varDef = stmt3) instanceof VarDef && (varDef.modifiers & Modifier.STATIC) === 0 && (value = varDef.value) !== null) {
              initializers.push(new ExprStmt(location, varDef.comment, new BinaryExpr(location, new MemberExpr(location, new ThisExpr(location), new IdentExpr(location, varDef.id.name)), BinaryOp.ASSIGN, value)));
            }
          }
          for (var i3 = 0, array = initializers.reverse(); i3 < array.length; i3++) {
            var stmt4 = array[i3];
            body.stmts.unshift(stmt4);
            this.createScopesPass.scope = body.scope;
            this.createScopesPass.visit(stmt4);
            this.resolveSymbolTypesPass.visit(stmt4);
          }
        }
        return;
      }
    }
    var body2 = new Block(location);
    var args = [];

    // Add the arguments necessary to call base constructor
    // TODO: what if the base class comes after this, and hasn't had a constructor generated yet? :(
    if ((baseType = classType.baseType) !== null && (ctorSymbol = baseType.instanceScope.find("this")) !== null && (funcType = ctorSymbol.type) instanceof FuncType && (argNames = funcType.argNames) !== null) {
      var baseArgs = [];
      var i = 0;
      while (i < argNames.length) {
        var name = argNames[i];
        args.push(new VarDef(location, null, new IdentExpr(location, name), 0, new TypeExpr(location, new MetaType(funcType.argTypes[i])), null));
        baseArgs.push(new IdentExpr(location, name));
        i++;
      }
      body2.stmts.push(new ExprStmt(location, null, new CallExpr(location, new BaseExpr(location), baseArgs)));
    }

    // Add additional arguments for each instance variable
    for (var i5 = 0; i5 < node.body.stmts.length; i5++) {
      var stmt5 = node.body.stmts[i5];
      if ((varDef2 = stmt5) instanceof VarDef && (varDef2.modifiers & Modifier.STATIC) === 0) {
        var id = new IdentExpr(location, varDef2.id.name);
        var type = new TypeExpr(location, varDef2.type.resolvedType);
        if ((value2 = varDef2.value) !== null) {
          body2.stmts.push(new ExprStmt(location, varDef2.comment, new BinaryExpr(location, new MemberExpr(location, new ThisExpr(location), new IdentExpr(location, varDef2.id.name)), BinaryOp.ASSIGN, value2)));
        } else {
          args.push(new VarDef(location, null, id, 0, type, null));
          body2.stmts.push(new ExprStmt(location, varDef2.comment, new BinaryExpr(location, new MemberExpr(location, new ThisExpr(location), new IdentExpr(location, varDef2.id.name)), BinaryOp.ASSIGN, new IdentExpr(location, varDef2.id.name))));
        }
      }
    }

    // Create the constructor node
    var id2 = new IdentExpr(location, "this");
    var ctorDef = new CtorDef(location, null, id2, 0, args, body2);
    node.body.stmts.unshift(ctorDef);
    this.createScopesPass.scope = classType.instanceScope;
    this.createScopesPass.visit(ctorDef);
    this.resolveSymbolTypesPass.visit(ctorDef);
  }
};

// class TypeContext
var TypeContext = function(targetType, node) {
  this.targetType = targetType;
  this.node = node;
};

// class ResolveTypesPass
var ResolveTypesPass = function(log) {
  Pass.call(this, log);
  this.classType = null;
  this.funcType = null;
  this.context = null;
};
ResolveTypesPass.prototype = Object.create(Pass.prototype);
ResolveTypesPass.prototype.checkForValue = function(node) {
  if (node.resolvedType instanceof MetaType || node.resolvedType === SpecialType.TYPE_VOID) {
    this.log.error(node.location, node.resolvedType + " is not a value");
    return false;
  }
  return true;
};
ResolveTypesPass.prototype.visitWithContext = function(typeWithContext, nodeToVisit) {
  var type;
  if ((type = typeWithContext) instanceof NullableType) {
    typeWithContext = type.innerType;
  }
  var old = this.context;
  this.context = new TypeContext(typeWithContext, nodeToVisit);
  this.visit(nodeToVisit);
  this.context = old;
};
ResolveTypesPass.prototype.checkConversion = function(location, from, to) {
  if (from !== SpecialType.ERROR && !Type.canImplicitlyConvert(from, to)) {
    this.log.error(location, "cannot implicitly convert from " + from + " to " + to);
  }
};
ResolveTypesPass.prototype.visitCallArgs = function(location, funcType, args) {
  var arrayType;
  if (args.length < funcType.minArgCount) {
    this.log.error(location, "expected at least " + funcType.minArgCount + " arguments but got " + args.length);
  } else if (args.length > funcType.argTypes.length && funcType.splatIndex === -1) {
    this.log.error(location, "expected at most " + funcType.argTypes.length + " arguments but got " + args.length);
  } else {
    var i = 0;
    while (i < args.length) {
      var argType = SpecialType.ERROR;
      if (funcType.splatIndex !== -1 && i >= funcType.splatIndex) {
        if (i + funcType.argTypes.length - args.length > funcType.splatIndex) {
          argType = funcType.argTypes[i + funcType.argTypes.length - args.length];
        } else if ((arrayType = funcType.argTypes[funcType.splatIndex]) instanceof ArrayType) {
          argType = arrayType.elementType;
        }
      } else {
        argType = funcType.argTypes[i];
      }
      var arg = args[i];
      this.visitWithContext(argType, arg);
      this.checkConversion(arg.location, arg.resolvedType, argType);
      i++;
    }
  }
};
ResolveTypesPass.prototype.getInstanceType = function(node) {
  var parentScope, symbol, metaType, classType;
  var scope = node.scope;
  while (scope.kind !== ScopeKind.STATIC && scope.kind !== ScopeKind.INSTANCE) {
    if ((parentScope = scope.parentScope) !== null) {
      scope = parentScope;
    } else {
      return null;
    }
  }
  if (scope.kind === ScopeKind.INSTANCE && (symbol = scope.symbol) !== null && (metaType = symbol.type) instanceof MetaType && (classType = metaType.instanceType) instanceof ClassType) {
    return classType;
  }
  return null;
};
ResolveTypesPass.prototype.visitExprStmt = function(node) {
  Pass.prototype.visitExprStmt.call(this, node);
  this.checkForValue(node.value);
};
ResolveTypesPass.prototype.visitIfStmt = function(node) {
  this.visit(node.value);
  this.checkConversion(node.value.location, node.value.resolvedType, ClassType.BOOL);
  this.visit(node.trueBody);
  this.visit(node.falseBody);
};
ResolveTypesPass.prototype.visitWhileStmt = function(node) {
  Pass.prototype.visitWhileStmt.call(this, node);
  this.checkConversion(node.value.location, node.value.resolvedType, ClassType.BOOL);
};
ResolveTypesPass.prototype.visitForStmt = function(node) {
  var symbol, arrayType;
  this.visit(node.value);
  this.visit(node.id);
  if ((symbol = node.id.symbol) !== null) {
    if ((arrayType = node.value.resolvedType) instanceof ArrayType) {
      symbol.type = arrayType.elementType;
    } else if (node.value.resolvedType === ClassType.STRING || node.value.resolvedType === SpecialType.VAR) {
      symbol.type = node.value.resolvedType;
    } else {
      this.log.error(node.location, node.value.resolvedType + " is not iterable");
    }
  }
  this.visit(node.body);
};
ResolveTypesPass.prototype.visitFailStmt = function(node) {
  Pass.prototype.visitFailStmt.call(this, node);
  this.checkConversion(node.location, node.value.resolvedType, ClassType.STRING);
};
ResolveTypesPass.prototype.visitReturnStmt = function(node) {
  var funcType, value;
  if ((funcType = this.funcType) !== null) {
    if ((value = node.value) !== null) {
      if (funcType.returnType === SpecialType.VOID) {
        this.log.error(node.location, "returning value in void function");
      } else {
        this.visitWithContext(funcType.returnType, value);
        this.checkConversion(node.location, value.resolvedType, funcType.returnType);
        this.checkForValue(value);
      }
    } else if (funcType.returnType !== SpecialType.VOID) {
      this.log.error(node.location, "missing return value in non-void function");
    }
  } else {
    this.log.error(node.location, "\"return\" used outside function");
  }
};
ResolveTypesPass.prototype.visitVarDef = function(node) {
  var value, metaType;
  this.visit(node.type);
  this.visit(node.id);
  if ((value = node.value) !== null && (metaType = node.type.resolvedType) instanceof MetaType) {
    this.visitWithContext(metaType.instanceType, value);
    this.checkConversion(value.location, value.resolvedType, metaType.instanceType);
  }
};
ResolveTypesPass.prototype.visitFuncDef = function(node) {
  var symbol, type;
  var old = this.funcType;
  if ((symbol = node.id.symbol) !== null && (type = symbol.type) instanceof FuncType) {
    this.funcType = type;
  }
  Pass.prototype.visitFuncDef.call(this, node);
  this.funcType = old;
};
ResolveTypesPass.prototype.visitCtorDef = function(node) {
  var symbol, type;
  var old = this.funcType;
  if ((symbol = node.id.symbol) !== null && (type = symbol.type) instanceof FuncType) {
    this.funcType = type;
  }
  Pass.prototype.visitCtorDef.call(this, node);
  this.funcType = old;
};
ResolveTypesPass.prototype.visitClassDef = function(node) {
  var symbol, metaType, type;
  var old = this.classType;
  if ((symbol = node.id.symbol) !== null && (metaType = symbol.type) instanceof MetaType && (type = metaType.instanceType) instanceof ClassType) {
    this.classType = type;
  }
  Pass.prototype.visitClassDef.call(this, node);
  this.classType = old;
};
ResolveTypesPass.prototype.visitBinaryExpr = function(node) {
  this.visit(node.left);
  if (node.op === BinaryOp.ASSIGN) {
    // Allow the type of the new value to be deduced from the storage location:
    // 
    //   float fn(int) x
    //   x = a -> a
    // 
    this.visitWithContext(node.left.resolvedType, node.right);
    this.checkConversion(node.location, node.right.resolvedType, node.left.resolvedType);
  } else if (node.op === BinaryOp.IN) {
    // Allow the type of the container to be deduced from the type of the element:
    // 
    //   if x in { 1, 2, 3 } { ... }
    // 
    this.visitWithContext(new ArrayType(node.left.resolvedType), node.right);
  } else {
    this.visit(node.right);
  }
};
ResolveTypesPass.prototype.visitTertiaryExpr = function(node) {
  var context;
  this.visit(node.value);
  this.checkConversion(node.value.location, node.value.resolvedType, ClassType.BOOL);
  if ((context = this.context) !== null && context.node === node) {
    this.visitWithContext(context.targetType, node.trueValue);
    this.visitWithContext(context.targetType, node.falseValue);
    this.checkConversion(node.trueValue.location, node.trueValue.resolvedType, context.targetType);
    this.checkConversion(node.falseValue.location, node.falseValue.resolvedType, context.targetType);
  } else {
    this.visit(node.trueValue);
    this.visit(node.falseValue);
  }
};
ResolveTypesPass.prototype.visitCallExpr = function(node) {
  var funcType, metaType, classType, ctorSymbol, funcType2, arrayType, classType2, ctorSymbol2, funcType3;
  this.visit(node.value);
  if ((funcType = node.value.resolvedType) instanceof FuncType) {
    node.resolvedType = funcType.returnType;
    this.visitCallArgs(node.location, funcType, node.args);
  } else if ((metaType = node.value.resolvedType) instanceof MetaType) {
    if ((classType = metaType.instanceType) instanceof ClassType) {
      if (classType.isAbstract) {
        this.log.error(node.location, "cannot instantiate abstract class " + classType);
      } else {
        node.ctorSymbol = classType.instanceScope.lookup("this", true);
        node.resolvedType = classType;
        if ((ctorSymbol = node.ctorSymbol) !== null && (funcType2 = ctorSymbol.type) instanceof FuncType) {
          this.visitCallArgs(node.location, funcType2, node.args);
        } else {
          throw new Error("fail in src/pass.bend on line 702");
        }
      }
    } else if ((arrayType = metaType.instanceType) instanceof ArrayType) {
      // Call the array constructor
      for (var i2 = 0; i2 < node.args.length; i2++) {
        var arg = node.args[i2];
        this.visitWithContext(arrayType.elementType, arg);
      }
      node.resolvedType = arrayType;
    } else {
      // Call the type's constructor, which just has one argument
      var type = metaType.instanceType;
      var argType = type;
      if (type === ClassType.INT) {
        argType = ClassType.FLOAT;
      }
      this.visitCallArgs(node.location, new FuncType(type, [argType], 1), node.args);
      node.resolvedType = type;
    }
  } else if ((node.value instanceof ThisExpr || node.value instanceof BaseExpr) && (classType2 = node.value.resolvedType) instanceof ClassType) {
    node.ctorSymbol = classType2.instanceScope.lookup("this", true);
    node.resolvedType = classType2;
    if ((ctorSymbol2 = node.ctorSymbol) !== null && (funcType3 = ctorSymbol2.type) instanceof FuncType) {
      this.visitCallArgs(node.location, funcType3, node.args);
    } else {
      throw new Error("fail in src/pass.bend on line 726");
    }
  } else if (node.value.resolvedType === SpecialType.VAR) {
    for (var i = 0; i < node.args.length; i++) {
      var arg2 = node.args[i];
      this.visitWithContext(SpecialType.VAR, arg2);
    }
    node.resolvedType = SpecialType.VAR;
  } else {
    this.log.error(node.location, "cannot call " + node.value.resolvedType);
  }
};
ResolveTypesPass.prototype.visitIndexExpr = function(node) {
  var arrayType;
  Pass.prototype.visitIndexExpr.call(this, node);
  if ((arrayType = node.value.resolvedType) instanceof ArrayType) {
    node.resolvedType = arrayType.elementType;
    this.checkConversion(node.location, node.index.resolvedType, ClassType.INT);
  } else if (node.value.resolvedType === ClassType.STRING) {
    node.resolvedType = ClassType.STRING;
    this.checkConversion(node.location, node.index.resolvedType, ClassType.INT);
  } else if (node.value.resolvedType === SpecialType.VAR) {
    node.resolvedType = SpecialType.VAR;
  } else {
    this.log.error(node.location, "cannot index into " + node.value.resolvedType);
  }
};
ResolveTypesPass.prototype.visitMemberExpr = function(node) {
  var metaType, classType, classType2, scope, symbol, arrayType, metaType2, arrayType2;
  this.visit(node.value);

  // Find the scope to look in for the member
  var maybeScope = null;
  if ((metaType = node.value.resolvedType) instanceof MetaType) {
    if ((classType = metaType.instanceType) instanceof ClassType) {
      maybeScope = classType.staticScope;
    } else if (node.value.resolvedType instanceof ArrayType) {
      maybeScope = SpecialScope.ARRAY_STATIC;
    }
  } else if ((classType2 = node.value.resolvedType) instanceof ClassType) {
    maybeScope = classType2.instanceScope;
  } else if (node.value.resolvedType instanceof ArrayType) {
    maybeScope = SpecialScope.ARRAY_INSTANCE;
  } else if (node.value.resolvedType === SpecialType.VAR) {
    node.resolvedType = SpecialType.VAR;
    return;
  }

  // Perform the lookup if we found one
  if ((scope = maybeScope) !== null) {
    var argTypes = null;
    if ((symbol = scope.lookup(node.id.name, true)) !== null) {
      node.id.symbol = symbol;
      node.resolvedType = symbol.type;
      if ((arrayType = node.value.resolvedType) instanceof ArrayType) {
        node.resolvedType = Type.removeParams(node.resolvedType, [arrayType.elementType]);
      } else if ((metaType2 = node.value.resolvedType) instanceof MetaType && (arrayType2 = metaType2.instanceType) instanceof ArrayType) {
        node.resolvedType = Type.removeParams(node.resolvedType, [arrayType2.elementType]);
      }
    } else {
      this.log.error(node.location, "no member \"" + node.id.name + "\" on " + node.value.resolvedType);
    }
  } else {
    this.log.error(node.location, node.value.resolvedType + " has no members");
  }
};
ResolveTypesPass.prototype.visitInitExpr = function(node) {
  var context, arrayType, classType, ctorSymbol, funcType;
  if ((context = this.context) !== null && context.node === node) {
    if ((arrayType = context.targetType) instanceof ArrayType) {
      for (var i = 0; i < node.values.length; i++) {
        var value = node.values[i];
        this.visitWithContext(arrayType.elementType, value);
        this.checkConversion(value.location, value.resolvedType, arrayType.elementType);
      }
      node.resolvedType = arrayType;
    } else if ((classType = context.targetType) instanceof ClassType) {
      if (classType.isAbstract) {
        this.log.error(node.location, "cannot instantiate abstract class " + classType);
      } else if ((ctorSymbol = classType.instanceScope.lookup("this", true)) !== null && (funcType = ctorSymbol.type) instanceof FuncType) {
        node.ctorSymbol = ctorSymbol;
        node.resolvedType = classType;
        this.visitCallArgs(node.location, funcType, node.values);
      } else {
        this.log.error(node.location, classType + " has no constructor");
      }
    } else {
      this.log.error(node.location, context.targetType + " has no constructor");
    }
  } else {
    this.log.error(node.location, "need context to resolve initializer list type");
  }
};
ResolveTypesPass.prototype.visitMatchExpr = function(node) {
  var symbol;
  Pass.prototype.visitMatchExpr.call(this, node);
  if ((symbol = node.id.symbol) !== null && symbol.type === SpecialType.ERROR) {
    symbol.type = this.instanceType(node.type, false);
    if (symbol.type instanceof NullableType) {
      this.log.error(node.location, "cannot match on nullable type " + symbol.type);
    } else if ((Type.isPrimitive(symbol.type) || !(symbol.type instanceof ClassType)) && !Type.equals(node.value.resolvedType, new NullableType(symbol.type))) {
      this.log.error(node.location, "type " + symbol.type + " cannot be matched with " + node.value.resolvedType + ", only with " + new NullableType(symbol.type));
    } else {
      this.checkConversion(node.location, symbol.type, node.value.resolvedType);
    }
  }
};
ResolveTypesPass.prototype.visitLambdaExpr = function(node) {
  var context, funcType, targetType2, symbol;
  var targetType = null;
  if ((context = this.context) !== null && context.node === node) {
    if (context.targetType === SpecialType.VAR) {
      var argTypes = [];
      for (var i2 = 0; i2 < node.args.length; i2++) {
        var arg = node.args[i2];
        argTypes.push(SpecialType.VAR);
      }
      targetType = new FuncType(SpecialType.VAR, argTypes, argTypes.length);
    } else if ((funcType = context.targetType) instanceof FuncType) {
      targetType = funcType;
    }
  }
  if ((targetType2 = targetType) !== null) {
    if (targetType2.argTypes.length === node.args.length) {
      var i = 0;
      while (i < node.args.length) {
        var arg2 = node.args[i];
        this.visit(arg2);
        if ((symbol = arg2.symbol) !== null && symbol.type === SpecialType.ERROR) {
          symbol.type = targetType2.argTypes[i];
        }
        i++;
      }
      node.resolvedType = targetType2;

      // Wrap the body with the function type to resolve return statements
      var old = this.funcType;
      this.funcType = targetType2;
      this.visit(node.body);
      this.funcType = old;
    } else {
      this.log.error(node.location, "lambda expected " + node.args.length + " arguments but context gave " + targetType2.argTypes.length);
    }
  } else {
    this.log.error(node.location, "need context to resolve lambda type");
  }
};
ResolveTypesPass.prototype.visitArrayTypeExpr = function(node) {
  Pass.prototype.visitArrayTypeExpr.call(this, node);
  var elementType = this.instanceType(node.elementType, false);
  if (elementType !== SpecialType.ERROR) {
    node.resolvedType = new MetaType(new ArrayType(elementType));
  }
};
ResolveTypesPass.prototype.visitFuncTypeExpr = function(node) {
  Pass.prototype.visitFuncTypeExpr.call(this, node);
  var returnType = this.instanceType(node.returnType, true);
  if (returnType === SpecialType.ERROR) {
    return;
  }
  var argTypes = [];
  for (var i = 0; i < node.args.length; i++) {
    var arg = node.args[i];
    var argType = this.instanceType(arg, false);
    if (argType === SpecialType.ERROR) {
      return;
    }
    argTypes.push(argType);
  }
  node.resolvedType = new MetaType(new FuncType(returnType, argTypes, argTypes.length));
};
ResolveTypesPass.prototype.visitNullableTypeExpr = function(node) {
  Pass.prototype.visitNullableTypeExpr.call(this, node);
  var innerType = this.instanceType(node.innerType, false);
  if (innerType instanceof NullableType) {
    this.log.error(node.location, innerType + " is already nullable");
  } else if (innerType === SpecialType.VAR) {
    node.resolvedType = MetaType.VAR;
  } else if (innerType !== SpecialType.ERROR) {
    node.resolvedType = new MetaType(new NullableType(innerType));
  }
};
ResolveTypesPass.prototype.visitTypeExpr = function(node) {
  node.resolvedType = node.type;
};
ResolveTypesPass.prototype.visitThisExpr = function(node) {
  var classType;
  if ((classType = this.getInstanceType(node)) !== null) {
    node.resolvedType = classType;
  } else {
    this.log.error(node.location, "\"this\" used outside instance scope");
  }
};
ResolveTypesPass.prototype.visitBaseExpr = function(node) {
  var classType, baseType;
  if ((classType = this.getInstanceType(node)) !== null) {
    if ((baseType = classType.baseType) !== null) {
      node.resolvedType = baseType;
    } else {
      this.log.error(node.location, classType + " has no base class");
    }
  } else {
    this.log.error(node.location, "\"base\" used outside instance scope");
  }
};
ResolveTypesPass.prototype.visitIdentExpr = function(node) {
  var symbol, symbol2;
  var argTypes = null;
  if ((symbol = node.scope.lookup(node.name, false)) !== null) {
    node.symbol = symbol;
  } else {
    this.log.error(node.location, "no symbol \"" + node.name + "\"");
  }
  if ((symbol2 = node.symbol) !== null) {
    node.resolvedType = symbol2.type;
  }
};
ResolveTypesPass.prototype.visitIntExpr = function(node) {
  node.resolvedType = ClassType.INT;
};
ResolveTypesPass.prototype.visitBoolExpr = function(node) {
  node.resolvedType = ClassType.BOOL;
};
ResolveTypesPass.prototype.visitFloatExpr = function(node) {
  node.resolvedType = ClassType.FLOAT;
};
ResolveTypesPass.prototype.visitStringExpr = function(node) {
  node.resolvedType = ClassType.STRING;
};

// class SpecialScope
var SpecialScope = function() {};
SpecialScope.ARRAY_STATIC = new Scope(ScopeKind.STATIC, null, null);
SpecialScope.ARRAY_INSTANCE = new Scope(ScopeKind.INSTANCE, SpecialScope.ARRAY_STATIC, null);

// Shortcut for defining a field
SpecialScope.field = function(scope, name, type) {
  var symbol = new Symbol(SymbolKind.VARIABLE, name, type, scope, 0);
  scope.symbols.push(symbol);
  return symbol;
};

// Shortcut for defining a function
SpecialScope.func = function(scope, name, funcType) {
  var symbol = new Symbol(SymbolKind.FUNCTION, name, funcType, scope, 0);
  scope.symbols.push(symbol);
  return symbol;
};
SpecialScope.init = function() {
  var T0 = TypeParam.T0;
  var VAR = SpecialType.VAR;
  var VOID = SpecialType.VOID;
  var ARRAY = new ArrayType(T0);
  var INT = ClassType.INT;
  var BOOL = ClassType.BOOL;
  var FLOAT = ClassType.FLOAT;
  var STRING = ClassType.STRING;
  SpecialScope.func(INT.instanceScope, "toString", new FuncType(STRING, [], 0));
  SpecialScope.func(BOOL.instanceScope, "toString", new FuncType(STRING, [], 0));
  SpecialScope.func(FLOAT.instanceScope, "toString", new FuncType(STRING, [], 0));
  SpecialScope.func(STRING.instanceScope, "toString", new FuncType(STRING, [], 0));
  SpecialScope.func(INT.instanceScope, "toHex", new FuncType(STRING, [], 0));
  SpecialScope.field(STRING.instanceScope, "length", INT);
  SpecialScope.func(STRING.staticScope, "fromCharCode", new FuncType(STRING, [INT], 1));
  SpecialScope.func(STRING.instanceScope, "quote", new FuncType(STRING, [STRING], 1));
  SpecialScope.func(STRING.instanceScope, "slice", new FuncType(STRING, [INT, INT], 1));
  SpecialScope.func(STRING.instanceScope, "toInt", new FuncType(INT, [], 0));
  SpecialScope.func(STRING.instanceScope, "toFloat", new FuncType(FLOAT, [], 0));
  SpecialScope.func(STRING.instanceScope, "substr", new FuncType(STRING, [INT, INT], 1));
  SpecialScope.func(STRING.instanceScope, "split", new FuncType(new ArrayType(STRING), [STRING], 1));
  SpecialScope.func(STRING.instanceScope, "replace", new FuncType(STRING, [STRING, STRING], 2));
  SpecialScope.func(STRING.instanceScope, "replacePattern", new FuncType(STRING, [STRING, STRING], 2));
  SpecialScope.func(STRING.instanceScope, "toLowerCase", new FuncType(STRING, [], 0));
  SpecialScope.func(STRING.instanceScope, "toUpperCase", new FuncType(STRING, [], 0));
  SpecialScope.func(STRING.instanceScope, "indexOf", new FuncType(INT, [STRING], 1));
  SpecialScope.func(STRING.instanceScope, "charCodeAt", new FuncType(INT, [INT], 1));
  SpecialScope.field(SpecialScope.ARRAY_INSTANCE, "length", INT);
  SpecialScope.func(SpecialScope.ARRAY_INSTANCE, "push", new FuncType(VOID, [T0], 1));
  SpecialScope.func(SpecialScope.ARRAY_INSTANCE, "pop", new FuncType(T0, [], 0));
  SpecialScope.func(SpecialScope.ARRAY_INSTANCE, "unshift", new FuncType(VOID, [T0], 1));
  SpecialScope.func(SpecialScope.ARRAY_INSTANCE, "shift", new FuncType(T0, [], 0));
  SpecialScope.func(SpecialScope.ARRAY_INSTANCE, "splice", new FuncType(ARRAY, [INT, INT], 2));
  SpecialScope.func(SpecialScope.ARRAY_INSTANCE, "join", new FuncType(STRING, [STRING], 1));
  SpecialScope.func(SpecialScope.ARRAY_INSTANCE, "filter", new FuncType(ARRAY, [new FuncType(BOOL, [T0], 1)], 1));
  SpecialScope.func(SpecialScope.ARRAY_INSTANCE, "slice", new FuncType(ARRAY, [INT, INT], 0));
  SpecialScope.func(SpecialScope.ARRAY_INSTANCE, "map", new FuncType(new ArrayType(VAR), [new FuncType(VAR, [T0], 1)], 1));
  SpecialScope.func(SpecialScope.ARRAY_INSTANCE, "concat", new FuncType(ARRAY, [ARRAY], 1));
  SpecialScope.func(SpecialScope.ARRAY_INSTANCE, "indexOf", new FuncType(INT, [T0], 1));
  SpecialScope.func(SpecialScope.ARRAY_INSTANCE, "reverse", new FuncType(ARRAY, [], 0));
};

// class JsNative
var JsNative = function() {};

// class Handler0
JsNative.Handler0 = function(symbol, callback) {
  this.symbol = symbol;
  this.callback = callback;
};

// class Handler1
JsNative.Handler1 = function(symbol, callback) {
  this.symbol = symbol;
  this.callback = callback;
};

// class Handler2
JsNative.Handler2 = function(symbol, callback) {
  this.symbol = symbol;
  this.callback = callback;
};
JsNative.handlers0 = [];
JsNative.handlers1 = [];
JsNative.handlers2 = [];
JsNative.register0 = function(symbol, callback) {
  JsNative.handlers0.push(new JsNative.Handler0(symbol, callback));
};
JsNative.register1 = function(symbol, callback) {
  JsNative.handlers1.push(new JsNative.Handler1(symbol, callback));
};
JsNative.register2 = function(symbol, callback) {
  JsNative.handlers2.push(new JsNative.Handler2(symbol, callback));
};
JsNative.handleCall = function(symbol, target, args, visit) {
  if (args.length === 0) {
    for (var i3 = 0; i3 < JsNative.handlers0.length; i3++) {
      var handler = JsNative.handlers0[i3];
      if (handler.symbol === symbol) {
        return handler.callback(visit, target);
      }
    }
  } else if (args.length === 1) {
    for (var i2 = 0; i2 < JsNative.handlers1.length; i2++) {
      var handler2 = JsNative.handlers1[i2];
      if (handler2.symbol === symbol) {
        return handler2.callback(visit, target, args[0]);
      }
    }
  } else if (args.length === 2) {
    for (var i = 0; i < JsNative.handlers2.length; i++) {
      var handler3 = JsNative.handlers2[i];
      if (handler3.symbol === symbol) {
        return handler3.callback(visit, target, args[0], args[1]);
      }
    }
  }
  return null;
};
JsNative.get = function(scope, name) {
  var symbol;
  if ((symbol = scope.find(name)) !== null) {
    return symbol;
  }
  throw new Error("fail in src/native.bend on line 112");
};
JsNative.escapeNonASCII = function(text) {
  var i = 0;
  while (i < text.length) {
    var code = text.charCodeAt(i++);
    if (code === 9) {
      text = text.slice(0, i - 1) + "\\t" + text.slice(i);
      i++;
    } else if (code === 10) {
      text = text.slice(0, i - 1) + "\\n" + text.slice(i);
      i++;
    } else if (code === 13) {
      text = text.slice(0, i - 1) + "\\r" + text.slice(i);
      i++;
    } else if (code < 32 || code > 126) {
      var word = code.toString(16);
      if (word.length === 1) {
        word = "\\x0" + word;
      } else if (word.length === 2) {
        word = "\\x" + word;
      } else if (word.length === 3) {
        word = "\\u0" + word;
      } else if (word.length === 4) {
        word = "\\u" + word;
      } else {
        throw new Error("fail in src/native.bend on line 137");
      }
      text = text.slice(0, i - 1) + word + text.slice(i);
      i += word.length - 1;
    }
  }
  return text;
};
JsNative.init = function() {
  JsNative.register1(JsNative.get(ClassType.INT.instanceScope, "this"), function(visit, target, arg0) {
    return new JsBinary(visit(arg0), JsBinaryOp.BITOR, new JsText("0"));
  });
  JsNative.register1(JsNative.get(ClassType.BOOL.instanceScope, "this"), function(visit, target, arg0) {
    return visit(arg0);
  });
  JsNative.register1(JsNative.get(ClassType.FLOAT.instanceScope, "this"), function(visit, target, arg0) {
    return visit(arg0);
  });
  JsNative.register1(JsNative.get(ClassType.STRING.instanceScope, "this"), function(visit, target, arg0) {
    return visit(arg0);
  });
  JsNative.register0(JsNative.get(ClassType.INT.instanceScope, "toHex"), function(visit, target) {
    return new JsCall(new JsBinary(visit(target), JsBinaryOp.MEMBER, new JsText("toString")), [new JsText("16")]);
  });
  JsNative.register1(JsNative.get(ClassType.STRING.instanceScope, "quote"), function(visit, target, arg0) {
    return new JsCall(new JsBinary(new JsText("JSON"), JsBinaryOp.MEMBER, new JsText("stringify")), [visit(target)]);
  });
  JsNative.register0(JsNative.get(ClassType.STRING.instanceScope, "toInt"), function(visit, target) {
    return new JsBinary(visit(target), JsBinaryOp.BITOR, new JsText("0"));
  });
  JsNative.register0(JsNative.get(ClassType.STRING.instanceScope, "toFloat"), function(visit, target) {
    return new JsUnary(visit(target), JsUnaryOp.POS);
  });
  JsNative.register2(JsNative.get(ClassType.STRING.instanceScope, "replace"), function(visit, target, arg0, arg1) {
    var arg02;
    var ESCAPE_REGEX = "([\\.\\?\\*\\+\\^\\$\\[\\]\\/\\\\\\(\\)\\{\\}\\|\\-])";
    if ((arg02 = arg0) instanceof StringExpr) {
      return new JsCall(new JsBinary(visit(target), JsBinaryOp.MEMBER, new JsText("replace")), [
        new JsText("/" + JsNative.escapeNonASCII(arg02.value.replace(new RegExp(ESCAPE_REGEX, "g"), "\\$1")) + "/g"),
        visit(arg1)
      ]);
    }
    return new JsCall(new JsBinary(visit(target), JsBinaryOp.MEMBER, new JsText("replace")), [
      new JsCall(new JsUnary(new JsText("RegExp"), JsUnaryOp.NEW), [
        new JsCall(new JsBinary(visit(arg0), JsBinaryOp.MEMBER, new JsText("replace")), [
          new JsText("/" + ESCAPE_REGEX + "/g"),
          new JsText("\"\\\\$1\"")
        ]),
        new JsText("\"g\"")
      ]),
      visit(arg1)
    ]);
  });
  JsNative.register2(JsNative.get(ClassType.STRING.instanceScope, "replacePattern"), function(visit, target, arg0, arg1) {
    var arg02;
    if ((arg02 = arg0) instanceof StringExpr) {
      return new JsCall(new JsBinary(visit(target), JsBinaryOp.MEMBER, new JsText("replace")), [
        new JsText("/" + JsNative.escapeNonASCII(arg02.value.replace(/\//g, "\\/")) + "/g"),
        visit(arg1)
      ]);
    }
    return new JsCall(new JsBinary(visit(target), JsBinaryOp.MEMBER, new JsText("replace")), [
      new JsCall(new JsUnary(new JsText("RegExp"), JsUnaryOp.NEW), [visit(arg0), new JsText("\"g\"")]),
      visit(arg1)
    ]);
  });
  JsNative.register1(JsNative.get(ClassType.STRING.staticScope, "fromCharCode"), function(visit, target, arg0) {
    return new JsCall(new JsBinary(new JsText("String"), JsBinaryOp.MEMBER, new JsText("fromCharCode")), [visit(arg0)]);
  });
};
SpecialScope.init();
JsNative.init();

// class JsNode
var JsNode = function() {};

// class JsFile
var JsFile = function(nodes) {
  JsNode.call(this);
  this.nodes = nodes;
};
JsFile.prototype = Object.create(JsNode.prototype);

// class JsBlock
var JsBlock = function(nodes) {
  JsNode.call(this);
  this.nodes = nodes;
};
JsBlock.prototype = Object.create(JsNode.prototype);

// class JsText
var JsText = function(text) {
  JsNode.call(this);
  this.text = text;
};
JsText.prototype = Object.create(JsNode.prototype);

// class JsComment
var JsComment = function(text, isStmt) {
  JsNode.call(this);
  this.text = text;
  this.isStmt = isStmt;
};
JsComment.prototype = Object.create(JsNode.prototype);

// class JsCall
var JsCall = function(value, args) {
  JsNode.call(this);
  this.value = value;
  this.args = args;
};
JsCall.prototype = Object.create(JsNode.prototype);

// class JsFunc
var JsFunc = function(name, args, body) {
  JsNode.call(this);
  this.name = name;
  this.args = args;
  this.body = body;
};
JsFunc.prototype = Object.create(JsNode.prototype);

// class JsUnary
var JsUnary = function(value, op) {
  JsNode.call(this);
  this.value = value;
  this.op = op;
};
JsUnary.prototype = Object.create(JsNode.prototype);

// class JsBinary
var JsBinary = function(left, op, right) {
  JsNode.call(this);
  this.left = left;
  this.op = op;
  this.right = right;
};
JsBinary.prototype = Object.create(JsNode.prototype);

// class JsTertiary
var JsTertiary = function(test, yes, no) {
  JsNode.call(this);
  this.test = test;
  this.yes = yes;
  this.no = no;
};
JsTertiary.prototype = Object.create(JsNode.prototype);

// class JsVars
var JsVars = function(nodes) {
  JsNode.call(this);
  this.nodes = nodes;
};
JsVars.prototype = Object.create(JsNode.prototype);

// class JsVar
var JsVar = function(name, value) {
  JsNode.call(this);
  this.name = name;
  this.value = value;
};
JsVar.prototype = Object.create(JsNode.prototype);

// class JsJump
var JsJump = function(kind, value) {
  JsNode.call(this);
  this.kind = kind;
  this.value = value;
};
JsJump.prototype = Object.create(JsNode.prototype);

// class JsArray
var JsArray = function(nodes) {
  JsNode.call(this);
  this.nodes = nodes;
};
JsArray.prototype = Object.create(JsNode.prototype);

// class JsObj
var JsObj = function(nodes) {
  JsNode.call(this);
  this.nodes = nodes;
};
JsObj.prototype = Object.create(JsNode.prototype);

// class JsProp
var JsProp = function(name, value) {
  JsNode.call(this);
  this.name = name;
  this.value = value;
};
JsProp.prototype = Object.create(JsNode.prototype);

// class JsIf
var JsIf = function(test, yes, no) {
  JsNode.call(this);
  this.test = test;
  this.yes = yes;
  this.no = no;
};
JsIf.prototype = Object.create(JsNode.prototype);

// class JsWhile
var JsWhile = function(test, body, label) {
  JsNode.call(this);
  this.test = test;
  this.body = body;
  this.label = label;
};
JsWhile.prototype = Object.create(JsNode.prototype);

// class JsFor
var JsFor = function(init, test, update, body, label) {
  JsNode.call(this);
  this.init = init;
  this.test = test;
  this.update = update;
  this.body = body;
  this.label = label;
};
JsFor.prototype = Object.create(JsNode.prototype);

// class JsSwitch
var JsSwitch = function(value, body) {
  JsNode.call(this);
  this.value = value;
  this.body = body;
};
JsSwitch.prototype = Object.create(JsNode.prototype);

// class JsWith
var JsWith = function(value, body) {
  JsNode.call(this);
  this.value = value;
  this.body = body;
};
JsWith.prototype = Object.create(JsNode.prototype);

// class JsUnaryOp
var JsUnaryOp = function() {};
JsUnaryOp.NEW = new JsUnaryOp();
JsUnaryOp.PRE_INC = new JsUnaryOp();
JsUnaryOp.PRE_DEC = new JsUnaryOp();
JsUnaryOp.POST_INC = new JsUnaryOp();
JsUnaryOp.POST_DEC = new JsUnaryOp();
JsUnaryOp.NOT = new JsUnaryOp();
JsUnaryOp.BITNOT = new JsUnaryOp();
JsUnaryOp.POS = new JsUnaryOp();
JsUnaryOp.NEG = new JsUnaryOp();
JsUnaryOp.TYPEOF = new JsUnaryOp();
JsUnaryOp.VOID = new JsUnaryOp();
JsUnaryOp.DELETE = new JsUnaryOp();

// class JsBinaryOp
var JsBinaryOp = function() {};
JsBinaryOp.MEMBER = new JsBinaryOp();
JsBinaryOp.INDEX = new JsBinaryOp();
JsBinaryOp.MUL = new JsBinaryOp();
JsBinaryOp.DIV = new JsBinaryOp();
JsBinaryOp.MOD = new JsBinaryOp();
JsBinaryOp.ADD = new JsBinaryOp();
JsBinaryOp.SUB = new JsBinaryOp();
JsBinaryOp.SHL = new JsBinaryOp();
JsBinaryOp.SHR = new JsBinaryOp();
JsBinaryOp.USHR = new JsBinaryOp();
JsBinaryOp.LT = new JsBinaryOp();
JsBinaryOp.LTE = new JsBinaryOp();
JsBinaryOp.GT = new JsBinaryOp();
JsBinaryOp.GTE = new JsBinaryOp();
JsBinaryOp.IN = new JsBinaryOp();
JsBinaryOp.INSTANCEOF = new JsBinaryOp();
JsBinaryOp.EQ = new JsBinaryOp();
JsBinaryOp.NEQ = new JsBinaryOp();
JsBinaryOp.SEQ = new JsBinaryOp();
JsBinaryOp.SNEQ = new JsBinaryOp();
JsBinaryOp.BITAND = new JsBinaryOp();
JsBinaryOp.BITXOR = new JsBinaryOp();
JsBinaryOp.BITOR = new JsBinaryOp();
JsBinaryOp.AND = new JsBinaryOp();
JsBinaryOp.OR = new JsBinaryOp();
JsBinaryOp.ASSIGN = new JsBinaryOp();
JsBinaryOp.ADD_ASSIGN = new JsBinaryOp();
JsBinaryOp.SUB_ASSIGN = new JsBinaryOp();
JsBinaryOp.MUL_ASSIGN = new JsBinaryOp();
JsBinaryOp.DIV_ASSIGN = new JsBinaryOp();
JsBinaryOp.MOD_ASSIGN = new JsBinaryOp();
JsBinaryOp.SHL_ASSIGN = new JsBinaryOp();
JsBinaryOp.SHR_ASSIGN = new JsBinaryOp();
JsBinaryOp.USHR_ASSIGN = new JsBinaryOp();
JsBinaryOp.BITAND_ASSIGN = new JsBinaryOp();
JsBinaryOp.BITXOR_ASSIGN = new JsBinaryOp();
JsBinaryOp.BITOR_ASSIGN = new JsBinaryOp();
JsBinaryOp.COMMA = new JsBinaryOp();

// class JsJumpKind
var JsJumpKind = function() {};
JsJumpKind.RETURN = new JsJumpKind();
JsJumpKind.THROW = new JsJumpKind();
JsJumpKind.CONTINUE = new JsJumpKind();
JsJumpKind.BREAK = new JsJumpKind();
JsJumpKind.CASE = new JsJumpKind();
JsJumpKind.DEFAULT = new JsJumpKind();

// class JsUnaryOpInfo
var JsUnaryOpInfo = function(op, precedence, template) {
  this.op = op;
  this.precedence = precedence;
  this.template = template;
};
JsUnaryOpInfo.info = [
  new JsUnaryOpInfo(JsUnaryOp.NEW, 1, "new {0}"),
  new JsUnaryOpInfo(JsUnaryOp.PRE_INC, 3, "++{0}"),
  new JsUnaryOpInfo(JsUnaryOp.PRE_DEC, 3, "--{0}"),
  new JsUnaryOpInfo(JsUnaryOp.POST_INC, 3, "{0}++"),
  new JsUnaryOpInfo(JsUnaryOp.POST_DEC, 3, "{0}--"),
  new JsUnaryOpInfo(JsUnaryOp.NOT, 4, "!{0}"),
  new JsUnaryOpInfo(JsUnaryOp.BITNOT, 4, "~{0}"),
  new JsUnaryOpInfo(JsUnaryOp.POS, 4, "+{0}"),
  new JsUnaryOpInfo(JsUnaryOp.NEG, 4, "-{0}"),
  new JsUnaryOpInfo(JsUnaryOp.TYPEOF, 4, "typeof {0}"),
  new JsUnaryOpInfo(JsUnaryOp.VOID, 4, "void {0}"),
  new JsUnaryOpInfo(JsUnaryOp.DELETE, 4, "delete {0}")
];

// class JsBinaryOpInfo
var JsBinaryOpInfo = function(op, precedence, template) {
  this.op = op;
  this.precedence = precedence;
  this.template = template;
};
JsBinaryOpInfo.info = [
  new JsBinaryOpInfo(JsBinaryOp.MEMBER, 1, "{0}.{1}"),
  new JsBinaryOpInfo(JsBinaryOp.INDEX, 1, "{0}[{1}]"),
  new JsBinaryOpInfo(JsBinaryOp.MUL, 5, "{0} * {1}"),
  new JsBinaryOpInfo(JsBinaryOp.DIV, 5, "{0} / {1}"),
  new JsBinaryOpInfo(JsBinaryOp.MOD, 5, "{0} % {1}"),
  new JsBinaryOpInfo(JsBinaryOp.ADD, 6, "{0} + {1}"),
  new JsBinaryOpInfo(JsBinaryOp.SUB, 6, "{0} - {1}"),
  new JsBinaryOpInfo(JsBinaryOp.SHL, 7, "{0} << {1}"),
  new JsBinaryOpInfo(JsBinaryOp.SHR, 7, "{0} >> {1}"),
  new JsBinaryOpInfo(JsBinaryOp.USHR, 7, "{0} >>> {1}"),
  new JsBinaryOpInfo(JsBinaryOp.LT, 8, "{0} < {1}"),
  new JsBinaryOpInfo(JsBinaryOp.LTE, 8, "{0} <= {1}"),
  new JsBinaryOpInfo(JsBinaryOp.GT, 8, "{0} > {1}"),
  new JsBinaryOpInfo(JsBinaryOp.GTE, 8, "{0} >= {1}"),
  new JsBinaryOpInfo(JsBinaryOp.IN, 8, "{0} in {1}"),
  new JsBinaryOpInfo(JsBinaryOp.INSTANCEOF, 8, "{0} instanceof {1}"),
  new JsBinaryOpInfo(JsBinaryOp.EQ, 9, "{0} == {1}"),
  new JsBinaryOpInfo(JsBinaryOp.NEQ, 9, "{0} != {1}"),
  new JsBinaryOpInfo(JsBinaryOp.SEQ, 9, "{0} === {1}"),
  new JsBinaryOpInfo(JsBinaryOp.SNEQ, 9, "{0} !== {1}"),
  new JsBinaryOpInfo(JsBinaryOp.BITAND, 10, "{0} & {1}"),
  new JsBinaryOpInfo(JsBinaryOp.BITXOR, 11, "{0} ^ {1}"),
  new JsBinaryOpInfo(JsBinaryOp.BITOR, 12, "{0} | {1}"),
  new JsBinaryOpInfo(JsBinaryOp.AND, 13, "{0} && {1}"),
  new JsBinaryOpInfo(JsBinaryOp.OR, 14, "{0} || {1}"),
  new JsBinaryOpInfo(JsBinaryOp.ASSIGN, 16, "{0} = {1}"),
  new JsBinaryOpInfo(JsBinaryOp.ADD_ASSIGN, 16, "{0} += {1}"),
  new JsBinaryOpInfo(JsBinaryOp.SUB_ASSIGN, 16, "{0} -= {1}"),
  new JsBinaryOpInfo(JsBinaryOp.MUL_ASSIGN, 16, "{0} *= {1}"),
  new JsBinaryOpInfo(JsBinaryOp.DIV_ASSIGN, 16, "{0} /= {1}"),
  new JsBinaryOpInfo(JsBinaryOp.MOD_ASSIGN, 16, "{0} %= {1}"),
  new JsBinaryOpInfo(JsBinaryOp.SHL_ASSIGN, 16, "{0} <<= {1}"),
  new JsBinaryOpInfo(JsBinaryOp.SHR_ASSIGN, 16, "{0} >>= {1}"),
  new JsBinaryOpInfo(JsBinaryOp.USHR_ASSIGN, 16, "{0} >>>= {1}"),
  new JsBinaryOpInfo(JsBinaryOp.BITAND_ASSIGN, 16, "{0} &= {1}"),
  new JsBinaryOpInfo(JsBinaryOp.BITXOR_ASSIGN, 16, "{0} ^= {1}"),
  new JsBinaryOpInfo(JsBinaryOp.BITOR_ASSIGN, 16, "{0} |= {1}"),
  new JsBinaryOpInfo(JsBinaryOp.COMMA, 17, "{0}, {1}")
];

// class JsPrinter
var JsPrinter = function() {
  this.spaces = "";
};
JsPrinter.rightAssociative = [
  JsBinaryOp.ASSIGN,
  JsBinaryOp.ADD_ASSIGN,
  JsBinaryOp.SUB_ASSIGN,
  JsBinaryOp.MUL_ASSIGN,
  JsBinaryOp.DIV_ASSIGN,
  JsBinaryOp.MOD_ASSIGN,
  JsBinaryOp.SHL_ASSIGN,
  JsBinaryOp.SHR_ASSIGN,
  JsBinaryOp.USHR_ASSIGN,
  JsBinaryOp.BITAND_ASSIGN,
  JsBinaryOp.BITXOR_ASSIGN,
  JsBinaryOp.BITOR_ASSIGN
];
JsPrinter.precedence = function(node) {
  var unary, binary;
  if ((unary = node) instanceof JsUnary) {
    for (var i2 = 0; i2 < JsUnaryOpInfo.info.length; i2++) {
      var info = JsUnaryOpInfo.info[i2];
      if (info.op === unary.op) {
        return info.precedence;
      }
    }
    throw new Error("fail in src/js-node.bend on line 179");
  } else if ((binary = node) instanceof JsBinary) {
    for (var i = 0; i < JsBinaryOpInfo.info.length; i++) {
      var info2 = JsBinaryOpInfo.info[i];
      if (info2.op === binary.op) {
        return info2.precedence;
      }
    }
    throw new Error("fail in src/js-node.bend on line 185");
  } else if (node instanceof JsCall) {
    // This is technically 2, but causes unnecessary parentheses in that case
    return 1;
  } else if (node instanceof JsTertiary) {
    return 15;
  } else {
    return 0;
  }
};
JsPrinter.prototype.visitStmts = function(stmts) {
  var func, jump, stmt2;
  var text = "";
  var wasComment = false;
  var wasCommentStmt = false;
  for (var i = 0; i < stmts.length; i++) {
    var stmt = stmts[i];
    var isComment = stmt instanceof JsComment;
    var isNamedFunc = (func = stmt) instanceof JsFunc && func.name !== null;
    var isBlockStmt = stmt instanceof JsIf || stmt instanceof JsWhile || stmt instanceof JsFor || stmt instanceof JsSwitch || stmt instanceof JsWith;
    var needsColon = (jump = stmt) instanceof JsJump && (jump.kind === JsJumpKind.CASE || jump.kind === JsJumpKind.DEFAULT);
    var printed = this.visit(stmt);
    if (printed.indexOf("{") === 0 || (printed.indexOf("function ") === 0 || printed.indexOf("function(") === 0) && !isNamedFunc) {
      printed = "(" + printed + ")";
    }
    if (isComment && !wasComment && text.length > 0 || wasCommentStmt) {
      text += "\n";
    }
    wasComment = isComment;
    wasCommentStmt = (stmt2 = stmt) instanceof JsComment && stmt2.isStmt;
    if (needsColon) {
      text += this.spaces.slice(2) + printed + ":\n";
      continue;
    }
    text += this.spaces + printed;
    if (isComment || isNamedFunc || isBlockStmt) {
      text += "\n";
    } else {
      text += ";\n";
    }
  }
  return text;
};
JsPrinter.prototype.visit = function(node) {
  var that = this, node2, node3, node4, node5, node6, node7, name, body, node8, node9, node10, node11, node13, value, node14, value2, node15, node17, node19, node20, no, node21, label, node22, label2, node23, node24;
  if ((node2 = node) instanceof JsFile) {
    return this.visitStmts(node2.nodes);
  } else if ((node3 = node) instanceof JsBlock) {
    this.spaces += "  ";
    var text = this.visitStmts(node3.nodes);
    this.spaces = this.spaces.slice(2);
    return "{\n" + text + this.spaces + "}";
  } else if ((node4 = node) instanceof JsText) {
    return node4.text;
  } else if ((node5 = node) instanceof JsComment) {
    // Remove any trailing newline
    var text2 = node5.text;
    if (text2.length > 0 && text2[text2.length - 1] === "\n") {
      text2 = text2.slice(0, text2.length - 1);
    }
    return text2.split("\n").map(function(line) {
      return "//" + line;
    }).join("\n" + this.spaces);
  } else if ((node6 = node) instanceof JsCall) {
    var args = [];
    for (var i7 = 0; i7 < node6.args.length; i7++) {
      var arg = node6.args[i7];
      args.push(this.visit(arg));
    }
    return this.visit(node6.value) + "(" + args.join(", ") + ")";
  } else if ((node7 = node) instanceof JsFunc) {
    var text3 = "function";
    if ((name = node7.name) !== null) {
      text3 += " " + name;
    }
    var args2 = [];
    for (var i6 = 0; i6 < node7.args.length; i6++) {
      var arg2 = node7.args[i6];
      args2.push(this.visit(arg2));
    }
    text3 += "(" + args2.join(", ") + ") ";
    if ((body = node7.body) instanceof JsBlock && body.nodes.length === 0) {
      text3 += "{}";
    } else {
      text3 += this.visit(node7.body);
    }
    return text3;
  } else if ((node8 = node) instanceof JsUnary) {
    for (var i5 = 0; i5 < JsUnaryOpInfo.info.length; i5++) {
      var info = JsUnaryOpInfo.info[i5];
      if (info.op === node8.op) {
        var text4 = this.visit(node8.value);
        if (JsPrinter.precedence(node8.value) > JsPrinter.precedence(node8)) {
          text4 = "(" + text4 + ")";
        }
        return info.template.replace(/\{0\}/g, text4);
      }
    }
    throw new Error("fail in src/js-node.bend on line 261");
  } else if ((node9 = node) instanceof JsBinary) {
    var left = this.visit(node9.left);
    var right = this.visit(node9.right);
    var needLeft;
    var needRight;
    if (JsPrinter.rightAssociative.indexOf(node9.op) >= 0) {
      needLeft = JsPrinter.precedence(node9.left) >= JsPrinter.precedence(node9);
      needRight = JsPrinter.precedence(node9.right) > JsPrinter.precedence(node9);
    } else {
      needLeft = JsPrinter.precedence(node9.left) > JsPrinter.precedence(node9);
      needRight = JsPrinter.precedence(node9.right) >= JsPrinter.precedence(node9);
    }
    if (needLeft) {
      left = "(" + left + ")";
    }
    if (needRight && node9.op !== JsBinaryOp.INDEX) {
      right = "(" + right + ")";
    }

    // If conversion to number doesn't result in NaN, make sure it has an extra dot.
    // "100.toHex()" in Bend must be called as "100..toString(16)" in JavaScript.
    if (node9.op === JsBinaryOp.MEMBER && +left === +left && left.indexOf(".") < 0) {
      left += ".";
    }
    for (var i4 = 0; i4 < JsBinaryOpInfo.info.length; i4++) {
      var info2 = JsBinaryOpInfo.info[i4];
      if (info2.op === node9.op) {
        // Hack to avoid replacing "{1}" in the text already replaced for "{0}"
        var text5 = info2.template.replace(/\{1\}/g, "\u0001");
        return text5.replace(/\{0\}/g, left).replace(/\x01/g, right);
      }
    }
    throw new Error("fail in src/js-node.bend on line 290");
  } else if ((node10 = node) instanceof JsTertiary) {
    return this.visit(node10.test) + " ? " + this.visit(node10.yes) + " : " + this.visit(node10.no);
  } else if ((node11 = node) instanceof JsVars) {
    var nodes = [];
    for (var i3 = 0; i3 < node11.nodes.length; i3++) {
      var node12 = node11.nodes[i3];
      nodes.push(this.visit(node12));
    }
    return "var " + nodes.join(", ");
  } else if ((node13 = node) instanceof JsVar) {
    var text6 = node13.name;
    if ((value = node13.value) !== null) {
      text6 += " = " + this.visit(value);
    }
    return text6;
  } else if ((node14 = node) instanceof JsJump) {
    var postfix = "";
    if ((value2 = node14.value) !== null) {
      postfix = " " + this.visit(value2);
    }
    if (node14.kind === JsJumpKind.CONTINUE) {
      return "continue" + postfix;
    } else if (node14.kind === JsJumpKind.BREAK) {
      return "break" + postfix;
    } else if (node14.kind === JsJumpKind.RETURN) {
      return "return" + postfix;
    } else if (node14.kind === JsJumpKind.THROW) {
      return "throw" + postfix;
    } else if (node14.kind === JsJumpKind.CASE) {
      return "case" + postfix;
    } else if (node14.kind === JsJumpKind.DEFAULT) {
      return "default" + postfix;
    } else {
      throw new Error("fail in src/js-node.bend on line 314");
    }
  } else if ((node15 = node) instanceof JsArray) {
    if (node15.nodes.length === 0) {
      return "[]";
    }
    if (node15.nodes.length === 1) {
      return "[" + this.visit(node15.nodes[0]) + "]";
    }
    this.spaces += "  ";
    var totalSize = 0;
    var newlines = false;
    var texts = [];
    for (var i2 = 0; i2 < node15.nodes.length; i2++) {
      var node16 = node15.nodes[i2];
      var text7 = this.visit(node16);
      if (text7.indexOf("\n") >= 0) {
        newlines = true;
      }
      totalSize += text7.length;
      texts.push(text7);
    }
    if (!newlines && totalSize < 32) {
      var text8 = texts.join(", ");
      this.spaces = this.spaces.slice(2);
      return "[" + text8 + "]";
    }
    var text9 = texts.map(function(text) {
      return that.spaces + text;
    }).join(",\n");
    this.spaces = this.spaces.slice(2);
    return "[\n" + text9 + "\n" + this.spaces + "]";
  } else if ((node17 = node) instanceof JsObj) {
    if (node17.nodes.length === 0) {
      return "{}";
    }
    if (node17.nodes.length === 1) {
      return "{ " + this.visit(node17.nodes[0]) + " }";
    }
    this.spaces += "  ";
    var texts2 = [];
    for (var i = 0; i < node17.nodes.length; i++) {
      var node18 = node17.nodes[i];
      texts2.push(this.spaces + this.visit(node18));
    }
    var text10 = texts2.join(",\n");
    this.spaces = this.spaces.slice(2);
    return "{\n" + text10 + "\n" + this.spaces + "}";
  } else if ((node19 = node) instanceof JsProp) {
    return node19.name + ": " + this.visit(node19.value);
  } else if ((node20 = node) instanceof JsIf) {
    var postfix2 = "";
    if ((no = node20.no) !== null) {
      postfix2 = " else " + this.visit(no);
    }
    return "if (" + this.visit(node20.test) + ") " + this.visit(node20.yes) + postfix2;
  } else if ((node21 = node) instanceof JsWhile) {
    var prefix = "";
    if ((label = node21.label) !== null) {
      prefix = label + ":\n" + this.spaces;
    }
    return prefix + "while (" + this.visit(node21.test) + ") " + this.visit(node21.body);
  } else if ((node22 = node) instanceof JsFor) {
    var prefix2 = "";
    if ((label2 = node22.label) !== null) {
      prefix2 = label2 + ":\n" + this.spaces;
    }
    return prefix2 + "for (" + this.visit(node22.init) + "; " + this.visit(node22.test) + "; " + this.visit(node22.update) + ") " + this.visit(node22.body);
  } else if ((node23 = node) instanceof JsSwitch) {
    return "switch (" + this.visit(node23.value) + ") " + this.visit(node23.body);
  } else if ((node24 = node) instanceof JsWith) {
    return "with (" + this.visit(node24.value) + ") " + this.visit(node24.body);
  } else {
    throw new Error("fail in src/js-node.bend on line 372");
  }
};

// class JsUnaryInfo
var JsUnaryInfo = function(op, jsOp) {
  this.op = op;
  this.jsOp = jsOp;
};
JsUnaryInfo.INFO = [
  new JsUnaryInfo(UnaryOp.NEG, JsUnaryOp.NEG),
  new JsUnaryInfo(UnaryOp.NOT, JsUnaryOp.NOT),
  new JsUnaryInfo(UnaryOp.BITNOT, JsUnaryOp.BITNOT),
  new JsUnaryInfo(UnaryOp.PRE_INC, JsUnaryOp.PRE_INC),
  new JsUnaryInfo(UnaryOp.PRE_DEC, JsUnaryOp.PRE_DEC),
  new JsUnaryInfo(UnaryOp.POST_INC, JsUnaryOp.POST_INC),
  new JsUnaryInfo(UnaryOp.POST_DEC, JsUnaryOp.POST_DEC)
];

// class JsBinaryInfo
var JsBinaryInfo = function(op, jsOp) {
  this.op = op;
  this.jsOp = jsOp;
};
JsBinaryInfo.INFO = [
  new JsBinaryInfo(BinaryOp.ADD_ASSIGN, JsBinaryOp.ADD_ASSIGN),
  new JsBinaryInfo(BinaryOp.SUB_ASSIGN, JsBinaryOp.SUB_ASSIGN),
  new JsBinaryInfo(BinaryOp.MUL_ASSIGN, JsBinaryOp.MUL_ASSIGN),
  new JsBinaryInfo(BinaryOp.DIV_ASSIGN, JsBinaryOp.DIV_ASSIGN),
  new JsBinaryInfo(BinaryOp.MOD_ASSIGN, JsBinaryOp.MOD_ASSIGN),
  new JsBinaryInfo(BinaryOp.SHL_ASSIGN, JsBinaryOp.SHL_ASSIGN),
  new JsBinaryInfo(BinaryOp.SHR_ASSIGN, JsBinaryOp.USHR_ASSIGN),
  new JsBinaryInfo(BinaryOp.BITOR_ASSIGN, JsBinaryOp.BITOR_ASSIGN),
  new JsBinaryInfo(BinaryOp.BITAND_ASSIGN, JsBinaryOp.BITAND_ASSIGN),
  new JsBinaryInfo(BinaryOp.BITXOR_ASSIGN, JsBinaryOp.BITXOR_ASSIGN),
  new JsBinaryInfo(BinaryOp.AND, JsBinaryOp.AND),
  new JsBinaryInfo(BinaryOp.OR, JsBinaryOp.OR),
  new JsBinaryInfo(BinaryOp.ADD, JsBinaryOp.ADD),
  new JsBinaryInfo(BinaryOp.SUB, JsBinaryOp.SUB),
  new JsBinaryInfo(BinaryOp.MUL, JsBinaryOp.MUL),
  new JsBinaryInfo(BinaryOp.DIV, JsBinaryOp.DIV),
  new JsBinaryInfo(BinaryOp.MOD, JsBinaryOp.MOD),
  new JsBinaryInfo(BinaryOp.SHL, JsBinaryOp.SHL),
  new JsBinaryInfo(BinaryOp.SHR, JsBinaryOp.USHR),
  new JsBinaryInfo(BinaryOp.BITOR, JsBinaryOp.BITOR),
  new JsBinaryInfo(BinaryOp.BITAND, JsBinaryOp.BITAND),
  new JsBinaryInfo(BinaryOp.BITXOR, JsBinaryOp.BITXOR),
  new JsBinaryInfo(BinaryOp.EQ, JsBinaryOp.SEQ),
  new JsBinaryInfo(BinaryOp.NEQ, JsBinaryOp.SNEQ),
  new JsBinaryInfo(BinaryOp.LTE, JsBinaryOp.LTE),
  new JsBinaryInfo(BinaryOp.GTE, JsBinaryOp.GTE),
  new JsBinaryInfo(BinaryOp.LT, JsBinaryOp.LT),
  new JsBinaryInfo(BinaryOp.GT, JsBinaryOp.GT),
  new JsBinaryInfo(BinaryOp.ASSIGN, JsBinaryOp.ASSIGN),
  new JsBinaryInfo(BinaryOp.IS, JsBinaryOp.INSTANCEOF)
];

// class JsConverter
var JsConverter = function() {
  this.loopCaptures = new JsLoopCaptures();
  this.thisAlias = new JsConverter.ThisAlias("this");
  this.loopEnvs = [];
  this.loops = [];

  // TODO: Remove hack
  this.hack = 0;
};

// class ThisAlias
JsConverter.ThisAlias = function(name) {
  this.name = name;
  this.nestedFuncCount = 0;
  this.isUsed = false;
};
JsConverter.ThisAlias.prototype.resolveName = function() {
  if (this.nestedFuncCount === 0) {
    return "this";
  }
  this.isUsed = true;
  return this.name;
};

// TODO: consolidate loop stuff into a map when maps are a thing?
JsConverter.LoopEnv = function(loopScope, envName) {
  this.loopScope = loopScope;
  this.envName = envName;
};
JsConverter.prototype.envForSymbol = function(symbol) {
  for (var i2 = 0; i2 < this.loopCaptures.symbolInfo.length; i2++) {
    var info = this.loopCaptures.symbolInfo[i2];
    if (info.symbol === symbol) {
      for (var i = 0; i < this.loopEnvs.length; i++) {
        var loopEnv = this.loopEnvs[i];
        if (loopEnv.loopScope === info.loopScope) {
          return loopEnv.envName;
        }
      }

      // Can't use findUnusedName() here because it won't catch duplicate names in nested functions:
      // 
      //   while true {
      //     int i
      //     void func() { int env = i }
      //   }
      // 
      var envName = "env$" + this.hack++;
      this.loopEnvs.push(new JsConverter.LoopEnv(info.loopScope, envName));
      return envName;
    }
  }
  return null;
};
JsConverter.prototype.visitStmts = function(stmts) {
  var comment;
  var nodes = [];
  for (var i = 0; i < stmts.length; i++) {
    var stmt = stmts[i];
    var visited = this.visitStmt(stmt);
    if (visited.length === 0 && !(stmt instanceof CommentStmt)) {
      continue;
    }
    if ((comment = stmt.comment) !== null) {
      nodes.push(new JsComment(comment, stmt instanceof CommentStmt));
    }
    nodes = nodes.concat(visited);
  }
  return nodes;
};
JsConverter.prototype.constructPrefix = function(list) {
  if (list.length === 0) {
    throw new Error("fail in src/js.bend on line 116");
  }
  if (list.length === 1) {
    return new JsText(list[0]);
  }
  return new JsBinary(this.constructPrefix(list.slice(0, list.length - 1)), JsBinaryOp.MEMBER, new JsText(list[list.length - 1]));
};
JsConverter.prototype.prefixFromScope = function(nextScope) {
  var scope, scope2, symbol;
  var prefix = [];
  if ((scope = nextScope) !== null && scope.kind === ScopeKind.INSTANCE) {
    prefix.unshift("prototype");
    nextScope = scope.parentScope;
  }
  while ((scope2 = nextScope) !== null && scope2.kind === ScopeKind.STATIC && (symbol = scope2.symbol) !== null) {
    prefix.unshift(symbol.name);
    nextScope = scope2.parentScope;
  }
  return prefix;
};
JsConverter.prototype.makeVar = function(symbol, value, scope) {
  var env, value2, value3;

  // Don't emit extern symbols
  if ((symbol.modifiers & Modifier.EXTERN) !== 0) {
    return [];
  }
  if ((env = this.envForSymbol(symbol)) !== null) {
    var node = new JsBinary(new JsText(env), JsBinaryOp.MEMBER, new JsText(symbol.name));
    if ((value2 = value) !== null) {
      node = new JsBinary(node, JsBinaryOp.ASSIGN, value2);
    }
    return [node];
  }
  var prefix = this.prefixFromScope(scope);
  if (prefix.length === 0) {
    return [new JsVars([new JsVar(symbol.name, value)])];
  }
  var prefixChain = this.constructPrefix(prefix.concat([symbol.name]));
  if ((value3 = value) !== null) {
    return [new JsBinary(prefixChain, JsBinaryOp.ASSIGN, value3)];
  }
  return [prefixChain];
};
JsConverter.prototype.findUnusedName = function(scope, prefix) {
  var parentScope;

  // Get to the top-level scope
  while (JsRenamer.IGNORED_SCOPE_KINDS.indexOf(scope.kind) >= 0 && (parentScope = scope.parentScope) !== null) {
    scope = parentScope;
  }

  // Find an unused name
  var name = prefix;
  var count = 2;
  while (scope.find(name) !== null) {
    name = prefix + count++;
  }

  // Reserve that name
  scope.symbols.push(new Symbol(SymbolKind.DUMMY, name, SpecialType.ERROR, scope, 0));
  return name;
};
JsConverter.prototype.generateFunction = function(scope, funcType, args, body) {
  var symbol;
  this.thisAlias.nestedFuncCount++;
  var jsArgs = [];
  var argStmts = [];
  var i = 0;
  for (var i2 = 0; i2 < args.length; i2++) {
    var arg = args[i2];
    var argName = (symbol = arg.id.symbol) !== null ? symbol.name : arg.id.name;
    if (funcType.splatIndex === -1 || i < funcType.splatIndex) {
      jsArgs.push(new JsText(argName));
    } else if (i === funcType.splatIndex) {
      var end = funcType.splatIndex - funcType.argTypes.length + 1;
      var callArgs = [new JsText("arguments")];
      if (i > 0 || end < 0) {
        callArgs.push(new JsText(i.toString()));
      }
      if (end < 0) {
        callArgs.push(new JsText(end.toString()));
      }
      argStmts.push(new JsVars([new JsVar(argName, new JsCall(new JsBinary(new JsBinary(new JsBinary(new JsText("Array"), JsBinaryOp.MEMBER, new JsText("prototype")), JsBinaryOp.MEMBER, new JsText("slice")), JsBinaryOp.MEMBER, new JsText("call")), callArgs))]));
    } else {
      var index = args.length - i;
      argStmts.push(new JsVars([new JsVar(argName, new JsBinary(new JsText("arguments"), JsBinaryOp.INDEX, new JsBinary(new JsBinary(new JsText("arguments"), JsBinaryOp.MEMBER, new JsText("length")), JsBinaryOp.SUB, new JsText(index.toString()))))]));
    }
    i++;
  }
  var jsBody = new JsBlock(this.declareVarsBeforeStmts(body.scope, body.stmts, scope.kind === ScopeKind.INSTANCE));
  for (var i3 = 0, array = argStmts.reverse(); i3 < array.length; i3++) {
    var stmt = array[i3];
    jsBody.nodes.unshift(stmt);
  }
  this.thisAlias.nestedFuncCount--;
  return new JsFunc(null, jsArgs, jsBody);
};
JsConverter.prototype.visitStmt = function(node) {
  var node2, node3, falseBody, block, elseIf, node4, body, node5, body2, memberExpr, symbol, env, node6, node7, label4, parentScope, node8, node9, value3, node10, symbol2, value4, node11, symbol3, funcType, body3, node12, symbol4, metaType, classType, baseType, ctor, symbol5, comment, symbol6;

  // Statements
  if ((node2 = node) instanceof ExprStmt) {
    return [this.visit(node2.value)];
  } else if ((node3 = node) instanceof IfStmt) {
    var no = null;
    if ((falseBody = node3.falseBody) !== null) {
      var elseBlock = this.visit(falseBody);
      if ((block = elseBlock) instanceof JsBlock && block.nodes.length === 1 && (elseIf = block.nodes[0]) instanceof JsIf) {
        no = elseIf;
      } else {
        no = elseBlock;
      }
    }
    return [new JsIf(this.visit(node3.value), this.visit(node3.trueBody), no)];
  } else if ((node4 = node) instanceof WhileStmt) {
    this.loops.push(null);
    if ((body = this.visit(node4.body)) instanceof JsBlock) {
      var label = this.loops.pop();

      // Generate the environment if used
      for (var i4 = 0; i4 < this.loopEnvs.length; i4++) {
        var loopEnv = this.loopEnvs[i4];
        if (loopEnv.loopScope === node4.body.scope) {
          body = new JsBlock([new JsWith(new JsObj([new JsProp(loopEnv.envName, new JsObj([]))]), body)]);
          break;
        }
      }
      return [new JsWhile(this.visit(node4.value), body, label)];
    } else {
      throw new Error("fail in src/js.bend on line 251");
    }
  } else if ((node5 = node) instanceof ForStmt) {
    this.loops.push(null);
    if ((body2 = this.visit(node5.body)) instanceof JsBlock) {
      var label2 = this.loops.pop();
      var index = new JsText(this.findUnusedName(node5.scope, "i"));
      var vars = new JsVars([new JsVar(index.text, new JsText("0"))]);
      var array = this.visit(node5.value);

      // Only generate a separate if node.value isn't simple (a MemberExpr chain ending in an IdentExpr)
      var value = node5.value;
      while ((memberExpr = value) instanceof MemberExpr) {
        value = memberExpr.value;
      }
      if (!(value instanceof IdentExpr)) {
        var name = this.findUnusedName(node5.scope, "array");
        vars.nodes.push(new JsVar(name, array));
        array = new JsText(name);
      }

      // Generate the loop variable
      var arrayIndex = new JsBinary(array, JsBinaryOp.INDEX, index);
      if ((symbol = node5.id.symbol) !== null) {
        if ((env = this.envForSymbol(symbol)) !== null) {
          body2.nodes.unshift(new JsBinary(new JsBinary(new JsText(env), JsBinaryOp.MEMBER, new JsText(symbol.name)), JsBinaryOp.ASSIGN, arrayIndex));
        } else {
          body2.nodes.unshift(new JsVars([new JsVar(symbol.name, arrayIndex)]));
        }
      }

      // Generate the environment if used
      for (var i3 = 0; i3 < this.loopEnvs.length; i3++) {
        var loopEnv2 = this.loopEnvs[i3];
        if (loopEnv2.loopScope === node5.body.scope) {
          body2 = new JsBlock([new JsWith(new JsObj([new JsProp(loopEnv2.envName, new JsObj([]))]), body2)]);
          break;
        }
      }

      // Combine everything into the full loop
      return [new JsFor(vars, new JsBinary(index, JsBinaryOp.LT, new JsBinary(array, JsBinaryOp.MEMBER, new JsText("length"))), new JsUnary(index, JsUnaryOp.POST_INC), body2, label2)];
    }
    throw new Error("fail in src/js.bend on line 298");
  } else if (node instanceof ContinueStmt || node instanceof BreakStmt) {
    // Extract the statement type and loop count
    var kind;
    var value2;
    var label3 = null;
    if ((node6 = node) instanceof ContinueStmt) {
      kind = JsJumpKind.CONTINUE;
      value2 = node6.value;
    } else if ((node7 = node) instanceof BreakStmt) {
      kind = JsJumpKind.BREAK;
      value2 = node7.value;
    }

    // If the loop count isn't 1, generate a label for the target loop
    if (value2 > 1 && value2 <= this.loops.length) {
      var i = this.loops.length - value2;
      var text;
      if ((label4 = this.loops[i]) !== null) {
        text = label4;
      } else {
        // Find an unused symbol name in the top-level scope and reserve it.
        // Even though JavaScript has a different label namespace, making
        // sure labels are named differently than variables avoids confusion.
        var scope = node.scope;
        while (JsRenamer.IGNORED_SCOPE_KINDS.indexOf(scope.kind) >= 0) {
          if ((parentScope = scope.parentScope) !== null) {
            scope = parentScope;
          } else {
            break;
          }
        }
        text = this.findUnusedName(scope, "loop");
        this.loops[i] = text;
      }
      label3 = new JsText(text);
    }
    return [new JsJump(kind, label3)];
  } else if ((node8 = node) instanceof FailStmt) {
    return [new JsJump(JsJumpKind.THROW, new JsCall(new JsUnary(new JsText("Error"), JsUnaryOp.NEW), [this.visit(node8.value)]))];
  } else if ((node9 = node) instanceof ReturnStmt) {
    if ((value3 = node9.value) !== null) {
      return [new JsJump(JsJumpKind.RETURN, this.visit(value3))];
    }
    return [new JsJump(JsJumpKind.RETURN, null)];
  } else if (node instanceof CommentStmt) {
    return [];
  } else if ((node10 = node) instanceof VarDef) {
    // Definitions
    if (node10.scope.kind === ScopeKind.INSTANCE) {
      return [];
    }
    if ((symbol2 = node10.id.symbol) !== null) {
      if ((value4 = node10.value) !== null) {
        return this.makeVar(symbol2, this.visit(value4), node10.scope);
      }
      return this.makeVar(symbol2, null, node10.scope);
    } else {
      throw new Error("fail in src/js.bend on line 357");
    }
  } else if ((node11 = node) instanceof FuncDef) {
    var value5 = new JsText("null");
    if ((symbol3 = node11.id.symbol) !== null && (funcType = symbol3.type) instanceof FuncType) {
      if ((body3 = node11.body) !== null) {
        value5 = this.generateFunction(node11.scope, funcType, node11.args, body3);
      }
      return this.makeVar(symbol3, value5, node11.scope);
    } else {
      throw new Error("fail in src/js.bend on line 365");
    }
  } else if ((node12 = node) instanceof ClassDef) {
    if ((node12.modifiers & Modifier.EXTERN) !== 0) {
      // Don't emit code for external classes, including all class members
      return [];
    } else if ((symbol4 = node12.id.symbol) !== null && (metaType = symbol4.type) instanceof MetaType && (classType = metaType.instanceType) instanceof ClassType) {
      // If there's a base class, initialize the prototype using Object.create()
      var nodes = [];
      if ((baseType = classType.baseType) !== null) {
        nodes.push(new JsBinary(this.constructPrefix(this.prefixFromScope(classType.staticScope).concat(["prototype"])), JsBinaryOp.ASSIGN, new JsCall(new JsBinary(new JsText("Object"), JsBinaryOp.MEMBER, new JsText("create")), [this.constructPrefix(this.prefixFromScope(baseType.instanceScope))])));
      }

      // Look for the constructor and use that for the function body
      for (var i2 = 0; i2 < node12.body.stmts.length; i2++) {
        with ({ env$3: {} }) {
          var stmt = node12.body.stmts[i2];
          if ((env$3.ctor = stmt) instanceof CtorDef) {
            // Add comments to classes without them
            if (node12.comment === null && env$3.ctor.comment === null) {
              node12.comment = " class " + node12.id.name;
            }

            // Pick out the constructor from the other statements
            if ((symbol5 = node12.id.symbol) !== null) {
              nodes = this.makeVar(symbol5, this.visit(env$3.ctor), node12.scope).concat(nodes);
            }
            if ((comment = env$3.ctor.comment) !== null) {
              nodes.unshift(new JsComment(comment, false));
            }
            return nodes.concat(this.visitStmts(node12.body.stmts.filter(function(stmt) {
              return stmt !== env$3.ctor;
            })));
          }
        }
      }

      // Add comments to classes without them
      if (node12.comment === null) {
        node12.comment = " class " + node12.id.name;
      }

      // Class doesn't have a constructor so use an empty object instead
      if ((symbol6 = node12.id.symbol) !== null) {
        nodes = this.makeVar(symbol6, new JsObj([]), node12.scope).concat(nodes);
      }
      return nodes.concat(this.visitStmts(node12.body.stmts));
    } else {
      throw new Error("fail in src/js.bend on line 406");
    }
  } else {
    throw new Error("fail in src/js.bend on line 409");
  }
};
JsConverter.applyDeMorgansTransform = function(node) {
  var node2, node3, node4;
  if ((node2 = node) instanceof JsText) {
    if (node2.text === "true") {
      return new JsText("false");
    } else if (node2.text === "false") {
      return new JsText("true");
    }
  } else if ((node3 = node) instanceof JsUnary) {
    if (node3.op === JsUnaryOp.NOT) {
      return node3.value;
    }
  } else if ((node4 = node) instanceof JsBinary) {
    if (node4.op === JsBinaryOp.AND) {
      return new JsBinary(JsConverter.applyDeMorgansTransform(node4.left), JsBinaryOp.OR, JsConverter.applyDeMorgansTransform(node4.right));
    } else if (node4.op === JsBinaryOp.OR) {
      return new JsBinary(JsConverter.applyDeMorgansTransform(node4.left), JsBinaryOp.AND, JsConverter.applyDeMorgansTransform(node4.right));
    } else if (node4.op === JsBinaryOp.EQ) {
      return new JsBinary(node4.left, JsBinaryOp.NEQ, node4.right);
    } else if (node4.op === JsBinaryOp.NEQ) {
      return new JsBinary(node4.left, JsBinaryOp.EQ, node4.right);
    } else if (node4.op === JsBinaryOp.LT) {
      return new JsBinary(node4.left, JsBinaryOp.GTE, node4.right);
    } else if (node4.op === JsBinaryOp.GT) {
      return new JsBinary(node4.left, JsBinaryOp.LTE, node4.right);
    } else if (node4.op === JsBinaryOp.LTE) {
      return new JsBinary(node4.left, JsBinaryOp.GT, node4.right);
    } else if (node4.op === JsBinaryOp.GTE) {
      return new JsBinary(node4.left, JsBinaryOp.LT, node4.right);
    }
  }
  return new JsUnary(node, JsUnaryOp.NOT);
};
JsConverter.prototype.declareVarsBeforeStmts = function(scope, stmts, createThisAlias) {
  var vars = [];
  for (var i = 0; i < scope.symbols.length; i++) {
    var symbol = scope.symbols[i];
    if (symbol.originalScope.kind === ScopeKind.MATCH) {
      vars.push(new JsVar(symbol.name, null));
    }
  }
  var nodes;
  if (createThisAlias) {
    // Generate an alias for "this" and add a variable for it if needed
    var old = this.thisAlias;
    this.thisAlias = new JsConverter.ThisAlias(this.findUnusedName(scope, "that"));
    nodes = this.visitStmts(stmts);
    if (this.thisAlias.isUsed) {
      vars.unshift(new JsVar(this.thisAlias.name, new JsText("this")));
    }
    this.thisAlias = old;
  } else {
    nodes = this.visitStmts(stmts);
  }
  if (vars.length > 0) {
    nodes.unshift(new JsVars(vars));
  }
  return nodes;
};
JsConverter.prototype.visit = function(node) {
  var that = this, node2, node3, node4, symbol, funcType, body, node5, node6, node7, node8, value2, symbol2, baseValue, baseType, ctorSymbol, native, metaType, classType, classType2, memberExpr, symbol3, native2, metaType2, node9, node10, symbol4, node11, nullableType, metaType3, node12, ctorSymbol2, native3, classType3, node13, symbol5, node14, metaType4, classType4, node15, node16, symbol6, env, node17, node18, node19, node20;
  if ((node2 = node) instanceof Module) {
    this.loopCaptures.visit(node2);
    return new JsFile(this.declareVarsBeforeStmts(node2.scope, node2.body.stmts, false));
  } else if ((node3 = node) instanceof Block) {
    return new JsBlock(this.visitStmts(node3.stmts));
  } else if ((node4 = node) instanceof CtorDef) {
    if ((symbol = node4.id.symbol) !== null && (funcType = symbol.type) instanceof FuncType && (body = node4.body) !== null) {
      return this.generateFunction(node4.scope, funcType, node4.args, body);
    }
    return new JsText("null");
  } else if ((node5 = node) instanceof UnaryExpr) {
    // Expressions
    for (var i9 = 0; i9 < JsUnaryInfo.INFO.length; i9++) {
      var info = JsUnaryInfo.INFO[i9];
      if (info.op === node5.op) {
        var value = this.visit(node5.value);
        if (info.jsOp === JsUnaryOp.NOT) {
          return JsConverter.applyDeMorgansTransform(value);
        }
        return new JsUnary(value, info.jsOp);
      }
    }
    throw new Error("fail in src/js.bend on line 504");
  } else if ((node6 = node) instanceof BinaryExpr) {
    if (node6.op === BinaryOp.IN) {
      return new JsBinary(new JsCall(new JsBinary(this.visit(node6.right), JsBinaryOp.MEMBER, new JsText("indexOf")), [this.visit(node6.left)]), JsBinaryOp.GTE, new JsText("0"));
    }

    // TODO: handle /= with temporary variable
    if (node6.op === BinaryOp.DIV && node6.left.resolvedType === ClassType.INT && node6.right.resolvedType === ClassType.INT) {
      return new JsBinary(new JsBinary(this.visit(node6.left), JsBinaryOp.DIV, this.visit(node6.right)), JsBinaryOp.BITOR, new JsText("0"));
    }
    for (var i8 = 0; i8 < JsBinaryInfo.INFO.length; i8++) {
      var info2 = JsBinaryInfo.INFO[i8];
      if (info2.op === node6.op) {
        return new JsBinary(this.visit(node6.left), info2.jsOp, this.visit(node6.right));
      }
    }
    throw new Error("fail in src/js.bend on line 520");
  } else if ((node7 = node) instanceof TertiaryExpr) {
    return new JsTertiary(this.visit(node7.value), this.visit(node7.trueValue), this.visit(node7.falseValue));
  } else if ((node8 = node) instanceof CallExpr) {
    if ((value2 = node8.value) instanceof MemberExpr && (symbol2 = value2.id.symbol) !== null && (baseValue = value2.value) instanceof BaseExpr && (baseType = baseValue.resolvedType) instanceof ClassType) {
      var args = [new JsText(this.thisAlias.resolveName())];
      for (var i7 = 0; i7 < node8.args.length; i7++) {
        var arg = node8.args[i7];
        args.push(this.visit(arg));
      }
      return new JsCall(this.constructPrefix(this.prefixFromScope(baseType.instanceScope).concat([symbol2.name, "call"])), args);
    } else if ((ctorSymbol = node8.ctorSymbol) !== null) {
      if ((native = JsNative.handleCall(ctorSymbol, node8, node8.args, function(x) {
        return that.visit(x);
      })) !== null) {
        return native;
      } else if ((metaType = node8.value.resolvedType) instanceof MetaType && (classType = metaType.instanceType) instanceof ClassType) {
        var args2 = [];
        for (var i6 = 0; i6 < node8.args.length; i6++) {
          var arg2 = node8.args[i6];
          args2.push(this.visit(arg2));
        }
        return new JsCall(new JsUnary(this.constructPrefix(this.prefixFromScope(classType.staticScope)), JsUnaryOp.NEW), args2);
      } else if ((classType2 = node8.value.resolvedType) instanceof ClassType) {
        var args3 = [new JsText(this.thisAlias.resolveName())];
        for (var i5 = 0; i5 < node8.args.length; i5++) {
          var arg3 = node8.args[i5];
          args3.push(this.visit(arg3));
        }
        return new JsCall(new JsBinary(this.constructPrefix(this.prefixFromScope(classType2.staticScope)), JsBinaryOp.MEMBER, new JsText("call")), args3);
      } else {
        throw new Error("fail in src/js.bend on line 548");
      }
    } else if ((memberExpr = node8.value) instanceof MemberExpr && (symbol3 = memberExpr.id.symbol) !== null && (native2 = JsNative.handleCall(symbol3, memberExpr.value, node8.args, function(x) {
      return that.visit(x);
    })) !== null) {
      return native2;
    } else {
      var args4 = [];
      for (var i4 = 0; i4 < node8.args.length; i4++) {
        var arg4 = node8.args[i4];
        args4.push(this.visit(arg4));
      }
      if ((metaType2 = node8.value.resolvedType) instanceof MetaType) {
        if (metaType2.instanceType instanceof ArrayType) {
          // TODO: Replace this special case with a splat constructor in native.bend
          return new JsArray(args4);
        } else if ((metaType2.instanceType instanceof FuncType || metaType2.instanceType === SpecialType.VAR) && args4.length === 1) {
          return args4[0];
        } else {
          throw new Error("fail in src/js.bend on line 566");
        }
      }
      return new JsCall(this.visit(node8.value), args4);
    }
  } else if ((node9 = node) instanceof IndexExpr) {
    return new JsBinary(this.visit(node9.value), JsBinaryOp.INDEX, this.visit(node9.index));
  } else if ((node10 = node) instanceof MemberExpr) {
    if (node10.value.resolvedType === SpecialType.VAR) {
      return new JsBinary(this.visit(node10.value), JsBinaryOp.MEMBER, new JsText(node10.id.name));
    } else if ((symbol4 = node10.id.symbol) !== null) {
      return new JsBinary(this.visit(node10.value), JsBinaryOp.MEMBER, new JsText(symbol4.name));
    } else {
      throw new Error("fail in src/js.bend on line 581");
    }
  } else if ((node11 = node) instanceof MatchExpr) {
    if ((nullableType = node11.value.resolvedType) instanceof NullableType && (metaType3 = node11.type.resolvedType) instanceof MetaType && Type.equals(nullableType.innerType, metaType3.instanceType)) {
      return new JsBinary(new JsBinary(this.visit(node11.id), JsBinaryOp.ASSIGN, this.visit(node11.value)), JsBinaryOp.SNEQ, new JsText("null"));
    }
    return new JsBinary(new JsBinary(this.visit(node11.id), JsBinaryOp.ASSIGN, this.visit(node11.value)), JsBinaryOp.INSTANCEOF, this.visit(node11.type));
  } else if ((node12 = node) instanceof InitExpr) {
    if ((ctorSymbol2 = node12.ctorSymbol) !== null) {
      if ((native3 = JsNative.handleCall(ctorSymbol2, node12, node12.values, function(x) {
        return that.visit(x);
      })) !== null) {
        return native3;
      } else if ((classType3 = node12.resolvedType) instanceof ClassType) {
        var values = [];
        for (var i3 = 0; i3 < node12.values.length; i3++) {
          var value3 = node12.values[i3];
          values.push(this.visit(value3));
        }
        return new JsCall(new JsUnary(this.constructPrefix(this.prefixFromScope(classType3.staticScope)), JsUnaryOp.NEW), values);
      } else {
        throw new Error("fail in src/js.bend on line 607");
      }
    } else {
      var nodes = [];
      for (var i2 = 0; i2 < node12.values.length; i2++) {
        var value4 = node12.values[i2];
        nodes.push(this.visit(value4));
      }
      return new JsArray(nodes);
    }
  } else if ((node13 = node) instanceof LambdaExpr) {
    this.thisAlias.nestedFuncCount++;
    var args5 = [];
    for (var i = 0; i < node13.args.length; i++) {
      var arg5 = node13.args[i];
      if ((symbol5 = arg5.symbol) !== null) {
        args5.push(new JsText(symbol5.name));
      }
    }
    var jsNode = new JsFunc(null, args5, new JsBlock(this.declareVarsBeforeStmts(node13.body.scope, node13.body.stmts, false)));
    this.thisAlias.nestedFuncCount--;
    return jsNode;
  } else if ((node14 = node) instanceof TypeExpr) {
    // Literals
    if (node14.type === SpecialType.NULL) {
      return new JsText("null");
    } else if ((metaType4 = node14.type) instanceof MetaType && (classType4 = metaType4.instanceType) instanceof ClassType) {
      return this.constructPrefix(this.prefixFromScope(classType4.staticScope));
    } else {
      return new JsText("<" + node14.type + ">");
    }
  } else if ((node15 = node) instanceof ThisExpr) {
    return new JsText(this.thisAlias.resolveName());
  } else if ((node16 = node) instanceof IdentExpr) {
    if ((symbol6 = node16.symbol) !== null) {
      if ((env = this.envForSymbol(symbol6)) !== null) {
        return new JsBinary(new JsText(env), JsBinaryOp.MEMBER, new JsText(symbol6.name));
      }
      if (symbol6.scope.kind === ScopeKind.INSTANCE) {
        return new JsBinary(new JsText(this.thisAlias.resolveName()), JsBinaryOp.MEMBER, new JsText(symbol6.name));
      }
      return this.constructPrefix(this.prefixFromScope(symbol6.scope).concat([symbol6.name]));
    } else {
      throw new Error("fail in src/js.bend on line 647");
    }
  } else if ((node17 = node) instanceof IntExpr) {
    return new JsText(node17.value + "");
  } else if ((node18 = node) instanceof BoolExpr) {
    return new JsText(node18.value + "");
  } else if ((node19 = node) instanceof FloatExpr) {
    return new JsText(node19.value + "");
  } else if ((node20 = node) instanceof StringExpr) {
    return new JsText(JSON.stringify(node20.value));
  } else {
    throw new Error("fail in src/js.bend on line 662");
  }
};

// class JsRenamer
var JsRenamer = function() {
  Visitor.call(this);
};
JsRenamer.prototype = Object.create(Visitor.prototype);

// From https://developer.mozilla.org/en/JavaScript/Reference/Reserved_Words
// and https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects
JsRenamer.RESERVED_WORDS = [
  "break",
  "case",
  "catch",
  "continue",
  "debugger",
  "default",
  "delete",
  "do",
  "else",
  "finally",
  "for",
  "function",
  "if",
  "in",
  "instanceof",
  "new",
  "return",
  "switch",
  "throw",
  "try",
  "typeof",
  "var",
  "void",
  "while",
  "with",
  "class",
  "enum",
  "export",
  "extends",
  "import",
  "super",
  "implements",
  "interface",
  "let",
  "package",
  "private",
  "protected",
  "public",
  "static",
  "yield",
  "const",
  "constructor",
  "prototype",
  "arguments",
  "null",
  "true",
  "false",
  "__proto__"
];
JsRenamer.IGNORED_SCOPE_KINDS = [
  ScopeKind.BLOCK,
  ScopeKind.LOOP,
  ScopeKind.MATCH
];
JsRenamer.prototype.visitIdentExpr = function(node) {
  var symbol, parentScope;

  // Check if we've already modified this symbol
  if ((symbol = node.symbol) !== null) {
    if (symbol.scope !== symbol.originalScope || symbol.name !== symbol.originalName) {
      return;
    }

    // Remove the symbol from its scope
    var scope = symbol.scope;
    scope.symbols.splice(scope.symbols.indexOf(symbol), 1);

    // Move the symbol definition up to the top level
    while (JsRenamer.IGNORED_SCOPE_KINDS.indexOf(scope.kind) >= 0) {
      if ((parentScope = scope.parentScope) !== null) {
        scope = parentScope;
      } else {
        break;
      }
    }

    // Rename the symbol so it fits in that scope
    var originalName = symbol.originalName;
    var count = 2;
    symbol.name = originalName;
    while (JsRenamer.RESERVED_WORDS.indexOf(symbol.name) >= 0 || scope.find(symbol.name) !== null) {
      symbol.name = originalName + count++;
    }

    // Insert the symbol in that scope
    scope.symbols.push(symbol);
    symbol.scope = scope;
  }
};

// Variables in loops should be unique per iteration, which doesn't happen in
// JavaScript because loops do not create a new environment. This is only a
// problem when those variables are captured by a closure:
// 
//   // This will create 10 functions that return 10 instead of 0 through 9
//   var funcs = [];
//   for (var i = 0; i < 10; i++) {
//     funcs.push(function() { return i; });
//   }
// 
// There are many ways to fix this and some are subtly wrong. The simplest
// approach would be to wrap the loop body in an immediately invoked function
// expression, but that breaks loop constructs like break and continue:
// 
//   var funcs = [];
//   for (var i = 0; i < 10; i++) {
//     (function(i) {
//       funcs.push(function() { return i; });
//       break; // Break and continue statements no longer work
//     })(i);
//   }
// 
// A correct approach is taken by Google's Traceur compiler, which compiles
// experimental JavaScript features to existing JavaScript. One of those
// experimental features is the let statement, which can define a variable that
// is confined to the scope of the loop:
// 
//   // This will create 10 functions that return 0 through 9 (note the "let")
//   var funcs = [];
//   for (let i = 0; i < 10; i++) {
//     funcs.push(function() { return i; });
//   }
// 
// Google's Traceur compiler abuses try-catch to introduce a new scope:
// 
//   // This is the above code compiled with Traceur
//   var funcs =[]; 
//   { 
//     try { 
//       throw undefined; 
//     } catch($i) { 
//       $i = 0; 
//       for(; $i < 10; $i ++) { 
//         try { 
//           throw undefined; 
//         } catch(i) { 
//           i = $i; 
//           try { 
//             funcs.push(function() { 
//               return i; 
//             }); 
//           } finally { 
//             $i = i; 
//           } 
//         } 
//       } 
//     } 
//   } 
// 
// Try-catch works and allows break and continue but unfortunately is not very
// elegant or fast. Another way to introduce a new scope is the with statement,
// but that is also not that fast and has other subtle problems. Immediately
// invoked function expression are the fastest (see http://jsperf.com/new-scope).
// Another attempt might wrap each function expression in an immediately invoked
// function expression with all loop variables as arguments, which still allows
// break and continue statements to work:
// 
//   // This will create 10 functions that return 0 through 9
//   var funcs = [];
//   for (var i = 0; i < 10; i++) {
//     funcs.push(function(i) { return function() { return i; }; }(i));
//   }
// 
// This is almost a correct solution, except when we modify the captured
// variable. Modification inside the function expression  will modify the copy
// stored in the argument and not the variable itself, and modification outside
// the function expression will modify the variable itself and not the copy:
// 
//   // The "i++" has no effect because it modifies the argument
//   var funcs = [];
//   for (var i = 0; i < 10; i++) {
//     funcs.push(function(i) { return function() { return i++; }; }(i));
//   }
// 
// This can be fixed by adding another layer of indirection and storing the
// value as the only element in a single-element array. This is what the HaXe
// compiler does:
// 
//   // The "i2[0]++" now has the intended effect
//   var funcs = [];
//   for (var i = 0; i < 10; i++) {
//     var i2 = [i];
//     funcs.push(function(i2) { return function() { return i2[0]++; }; }(i2));
//   }
// 
// It seems more elegant and readable to use an object instead of an array,
// which can store all captured variables in the new environment at once:
// 
//   // The "env.i++" now has the intended effect
//   var funcs = [];
//   for (var i = 0; i < 10; i++) {
//     var env = {};
//     env.i = i;
//     funcs.push(function(env) { return function() { return env.i++; }; }(env));
//   }
// 
var JsLoopCaptures = function() {
  Visitor.call(this);
  this.symbolInfo = [];
};
JsLoopCaptures.prototype = Object.create(Visitor.prototype);

// class SymbolInfo
JsLoopCaptures.SymbolInfo = function(symbol, loopScope) {
  this.symbol = symbol;
  this.loopScope = loopScope;
};
JsLoopCaptures.ALLOWED_SCOPE_KINDS = [
  ScopeKind.BLOCK,
  ScopeKind.LOOP,
  ScopeKind.MATCH
];
JsLoopCaptures.prototype.visitFuncDef = function(node) {
  // Don't visit the identifier for this function or it will "capture" itself
  for (var i = 0; i < node.args.length; i++) {
    var arg = node.args[i];
    this.visit(arg);
  }
  this.visit(node.body);
};
JsLoopCaptures.prototype.visitIdentExpr = function(node) {
  var symbol, parentScope, parentScope2;
  if ((symbol = node.symbol) !== null && (symbol.modifiers & Modifier.EXTERN) === 0 && JsLoopCaptures.ALLOWED_SCOPE_KINDS.indexOf(symbol.originalScope.kind) >= 0) {
    // Don't process this symbol again (TODO: make more efficient)
    for (var i = 0; i < this.symbolInfo.length; i++) {
      var info = this.symbolInfo[i];
      if (info.symbol === symbol) {
        return;
      }
    }

    // Check if this identifier is captured by a function
    var scope = node.scope;
    var inFunc = false;
    while (scope !== symbol.originalScope) {
      if (scope.kind === ScopeKind.FUNCTION) {
        inFunc = true;
      }

      // Advance to the next scope
      if ((parentScope = scope.parentScope) !== null) {
        scope = parentScope;
      } else {
        break;
      }
    }

    // Check if this symbol is defined inside a loop
    if (inFunc) {
      var scope2 = symbol.originalScope;
      while (JsLoopCaptures.ALLOWED_SCOPE_KINDS.indexOf(scope2.kind) >= 0) {
        if (scope2.kind === ScopeKind.LOOP) {
          this.symbolInfo.push(new JsLoopCaptures.SymbolInfo(symbol, scope2));
          break;
        }

        // Advance to the next scope
        if ((parentScope2 = scope2.parentScope) !== null) {
          scope2 = parentScope2;
        } else {
          break;
        }
      }
    }
  }
};

// class JavaScript
var JavaScript = function() {};
JavaScript.convert = function(node) {
  new JsRenamer().visit(node);
  return new JsPrinter().visit(new JsConverter().visit(node));
};

// class Compiler
var Compiler = function() {
  this.module = new Module(new Location("<all>", 0), new Block(new Location("<all>", 0)));
  this.tokens = [];
  this.log = new Log();
};
Compiler.prototype.addFile = function(file, text) {
  var fileModule;

  // Try to tokenize the file (using a temporary log so we know if there were errors)
  var fileLog = new Log();
  var fileTokens = Tokenizer.tokenize(fileLog, file, text);
  this.log.messages = this.log.messages.concat(fileLog.messages);
  this.tokens = this.tokens.concat(fileTokens);

  // If tokenizing worked, try parsing the file
  if (!fileLog.hasErrors) {
    if ((fileModule = new Parser().parse(this.log, fileTokens)) !== null) {
      this.module.body.stmts = this.module.body.stmts.concat(fileModule.body.stmts);
    }
  } else {
    this.log.hasErrors = true;
  }
};
Compiler.prototype.compile = function() {
  var passes = [
    new CreateScopesPass(this.log),
    new ResolveBaseTypesPass(this.log),
    new ResolveSymbolTypesPass(this.log),
    new CheckOverridePass(this.log),
    new GenerateCtorsPass(this.log),
    new ResolveTypesPass(this.log)
  ];

  // Run the passes
  for (var i = 0; i < passes.length; i++) {
    var pass = passes[i];
    pass.visit(this.module);
    if (this.log.hasErrors) {
      return null;
    }
  }
  return this.module;
};

// A command-line compiler driver for node.js
var main = function() {
  var node2, outputPath2;
  var USAGE = "\nUsage:\n  bend [inputs] [-o output]\n\nOptions:\n  -h, --help         Print this help text\n  -p, --print-code   Print the compiled output to stdout without evaluating it\n  -o, --output-file  Store the output in the file provided by the next argument\n\nNote:\n  Without -p or -o, bend will run your code using node\n";
  var files = [];
  var flagRun = true;
  var flagPrintCode = false;
  var flagOutputFile = false;
  var outputPath = null;

  // Parse command-line arguments
  for (var i = 0, array = process.argv.slice(2); i < array.length; i++) {
    var arg = array[i];
    if (arg.length === 0) {
      continue;
    } else if (flagOutputFile) {
      outputPath = arg;
      flagOutputFile = false;
    } else if (arg[0] === "-") {
      if (["-h", "--help"].indexOf(arg) >= 0) {
        process.stdout.write(USAGE + "\n");
        process.exit(0);
      } else if (["-p", "--print-code"].indexOf(arg) >= 0) {
        flagPrintCode = true;
        flagRun = false;
      } else if (["-o", "--output-file"].indexOf(arg) >= 0) {
        flagOutputFile = true;
        flagRun = false;
      } else {
        process.stdout.write("\nUnknown flag:\n  " + arg + "\n");
        process.stdout.write(USAGE + "\n");
        process.exit(1);
      }
    } else {
      files.push(arg);
    }
  }
  if (files.length === 0 || flagOutputFile) {
    process.stdout.write(USAGE + "\n");
    process.exit(1);
  }

  // Run the compiler
  var fs = require("fs");
  var compiler = new Compiler();
  for (var i2 = 0; i2 < files.length; i2++) {
    var file = files[i2];
    compiler.addFile(file, fs.readFileSync(file, "utf8"));
  }
  var node = compiler.compile();
  for (var i3 = 0; i3 < compiler.log.messages.length; i3++) {
    var message = compiler.log.messages[i3];
    process.stdout.write(message + "\n");
  }

  // Use the compiled results
  if ((node2 = node) !== null) {
    var code = JavaScript.convert(node2);
    if ((outputPath2 = outputPath) !== null) {
      fs.writeFile(outputPath2, code, function(err) {
        if (err !== null) {
          process.stdout.write("error: could not write to \"" + outputPath2 + "\"\n");
          process.exit(1);
        }
      });
    }
    if (flagPrintCode) {
      process.stdout.write(code);
    }
    if (flagRun) {
      eval(code);
    }
  } else {
    process.exit(1);
  }
};

// Export the compiler so this module can be used with require()
exports.Compiler = Compiler;
exports.JavaScript = JavaScript;
exports.main = main;

// Run main() if we weren't included via require()
if (module.parent === null) {
  main();
}
