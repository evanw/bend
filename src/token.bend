class TokenKind {
  string name
  string? text

  // Symbols
  LPAREN = { 'LPAREN', '(' }
  RPAREN = { 'RPAREN', ')' }
  LBRACKET = { 'LBRACKET', '[' }
  RBRACKET = { 'RBRACKET', ']' }
  LBRACE = { 'LBRACE', '{' }
  RBRACE = { 'RBRACE', '}' }
  COMMA = { 'COMMA', ',' }
  SEMICOLON = { 'SEMICOLON', ';' }
  SPLAT = { 'SPLAT', '...' }
  DOT = { 'DOT', '.' }
  QUESTION = { 'QUESTION', '?' }
  ARROW = { 'ARROW', '->' }

  // Symbolic operators
  INC = { 'INC', '++' }
  DEC = { 'DEC', '--' }
  ADD_ASSIGN = { 'ADD_ASSIGN', '+=' }
  SUB_ASSIGN = { 'SUB_ASSIGN', '-=' }
  MUL_ASSIGN = { 'MUL_ASSIGN', '*=' }
  DIV_ASSIGN = { 'DIV_ASSIGN', '/=' }
  MOD_ASSIGN = { 'MOD_ASSIGN', '%=' }
  SHL_ASSIGN = { 'SHL_ASSIGN', '<<=' }
  SHR_ASSIGN = { 'SHR_ASSIGN', '>>=' }
  BITOR_ASSIGN = { 'BITOR_ASSIGN', '|=' }
  BITAND_ASSIGN = { 'BITAND_ASSIGN', '&=' }
  BITXOR_ASSIGN = { 'BITXOR_ASSIGN', '^=' }
  ADD = { 'ADD', '+' }
  SUB = { 'SUB', '-' }
  MUL = { 'MUL', '*' }
  DIV = { 'DIV', '/' }
  MOD = { 'MOD', '%' }
  SHL = { 'SHL', '<<' }
  SHR = { 'SHR', '>>' }
  BITOR = { 'BITOR', '|' }
  BITAND = { 'BITAND', '&' }
  BITXOR = { 'BITXOR', '^' }
  EQ = { 'EQ', '==' }
  NEQ = { 'NEQ', '!=' }
  LTE = { 'LTE', '<=' }
  GTE = { 'GTE', '>=' }
  LT = { 'LT', '<' }
  GT = { 'GT', '>' }
  ASSIGN = { 'ASSIGN', '=' }
  BITNOT = { 'BITNOT', '~' }

  // Keyword operators
  AND = { 'AND', 'and' }
  OR = { 'OR', 'or' }
  IS = { 'IS', 'is' }
  IN = { 'IN', 'in' }
  NOT = { 'NOT', 'not' }

  // Keywords
  STATIC = { 'STATIC', 'static' }
  OVER = { 'OVER', 'over' }
  ABSTRACT = { 'ABSTRACT', 'abstract' }
  EXTERN = { 'EXTERN', 'extern' }
  PROP = { 'PROP', 'prop' }
  RETURN = { 'RETURN', 'return' }
  IF = { 'IF', 'if' }
  ELSE = { 'ELSE', 'else' }
  WHILE = { 'WHILE', 'while' }
  FOR = { 'FOR', 'for' }
  CONTINUE = { 'CONTINUE', 'continue' }
  BREAK = { 'BREAK', 'break' }
  FAIL = { 'FAIL', 'fail' }
  CLASS = { 'CLASS', 'class' }
  FN = { 'FN', 'fn' }

  // Keyword literals
  NULL = { 'NULL', 'null' }
  VOID = { 'VOID', 'void' }
  THIS = { 'THIS', 'this' }
  BASE = { 'BASE', 'base' }
  TRUE = { 'TRUE', 'true' }
  FALSE = { 'FALSE', 'false' }
  VAR = { 'VAR', 'var' }
  INT = { 'INT', 'int' }
  BOOL = { 'BOOL', 'bool' }
  FLOAT = { 'FLOAT', 'float' }
  STRING = { 'STRING', 'string' }

  // Literals
  IDENT = { 'IDENT', null }
  INT_LIT = { 'INT_LIT', null }
  FLOAT_LIT = { 'FLOAT_LIT', null }
  STRING_LIT = { 'STRING_LIT', null }

  // Other
  LARGS = { 'LARGS', null }
  RARGS = { 'RARGS', null }
  IS_NOT = { 'IS_NOT', null }
  NOT_IN = { 'NOT_IN', null }
  NEWLINE = { 'NEWLINE', null }
  COMMENT = { 'COMMENT', null }
  END_OF_FILE = { 'END_OF_FILE', null }
  ERROR = { 'ERROR', null }

  static TokenKind[] SYMBOLS = {
    // Symbols
    LPAREN
    RPAREN
    LBRACKET
    RBRACKET
    LBRACE
    RBRACE
    COMMA
    SEMICOLON
    SPLAT
    DOT
    QUESTION
    ARROW

    // Symbolic operators
    INC
    DEC
    ADD_ASSIGN
    SUB_ASSIGN
    MUL_ASSIGN
    DIV_ASSIGN
    MOD_ASSIGN
    SHL_ASSIGN
    SHR_ASSIGN
    BITOR_ASSIGN
    BITAND_ASSIGN
    BITXOR_ASSIGN
    ADD
    SUB
    MUL
    DIV
    MOD
    SHL
    SHR
    BITOR
    BITAND
    BITXOR
    EQ
    NEQ
    LTE
    GTE
    LT
    GT
    ASSIGN
    BITNOT
  }

  static TokenKind[] IDENTIFIERS = {
    // Keyword operators
    AND
    OR
    IS
    IN
    NOT

    // Keywords
    STATIC
    OVER
    ABSTRACT
    EXTERN
    PROP
    RETURN
    IF
    ELSE
    WHILE
    FOR
    CONTINUE
    BREAK
    FAIL
    CLASS
    FN

    // Keyword literals
    NULL
    VOID
    THIS
    BASE
    TRUE
    FALSE
    VAR
    INT
    BOOL
    FLOAT
    STRING
  }

  static TokenKind[] LEFT_BRACKETS = {
    LPAREN
    LBRACKET
    LBRACE
  }

  static TokenKind[] RIGHT_BRACKETS = {
    RPAREN
    RBRACKET
    RBRACE
  }

  static TokenKind[] REMOVE_NEWLINE_BEFORE = {
    // Symbols
    RPAREN
    RBRACKET
    RBRACE
    DOT

    // Keywords
    ELSE
  }

  static TokenKind[] REMOVE_NEWLINE_AFTER = {
    // Symbols
    LPAREN
    LBRACKET
    LBRACE
    COMMA
    SEMICOLON
    DOT
    ARROW

    // Symbolic operators
    ADD_ASSIGN
    SUB_ASSIGN
    MUL_ASSIGN
    DIV_ASSIGN
    MOD_ASSIGN
    SHL_ASSIGN
    SHR_ASSIGN
    BITOR_ASSIGN
    BITAND_ASSIGN
    BITXOR_ASSIGN
    ADD
    SUB
    MUL
    DIV
    MOD
    SHL
    SHR
    BITOR
    BITAND
    BITXOR
    EQ
    NEQ
    LTE
    GTE
    LT
    GT
    ASSIGN
    BITNOT

    // Keyword operators
    AND
    OR
    IS
    IN
    NOT
  }
}

class Token {
  Location location
  TokenKind kind
  string text

  string toString() { return kind.name + ' ' + text.quote('"') }
}

class Tokenizer {
  static bool isNumber(string c) {
    return c >= '0' and c <= '9'
  }

  static bool isLetter(string c) {
    return (c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z') or c == '_'
  }

  static bool isSpace(string c) {
    return c == ' ' or c == '\t' or c == '\r' or c == '\n'
  }

  static Token[] tokenize(Log log, string file, string text) {
    Token[] tokens = {}
    int line = 1
    int i = 0

    while i < text.length {
      // Create an error token by default
      Token token = { { file, line }, TokenKind.ERROR, '' }
      int start = i

      // Parse int literal
      if isNumber(text[i]) {
        token.kind = TokenKind.INT_LIT
        i++
        while i < text.length and isNumber(text[i]) { i++ }

        // Parse float literal
        if i + 1 < text.length and text[i] == '.' and isNumber(text[i + 1]) {
          token.kind = TokenKind.FLOAT_LIT
          i += 2
          while i < text.length and isNumber(text[i]) { i++ }
        }
      }

      // Parse identifier
      else if isLetter(text[i]) {
        token.kind = TokenKind.IDENT
        i++
        while i < text.length and (isLetter(text[i]) or isNumber(text[i])) { i++ }
        token.text = text.slice(start, i)
        for kind in TokenKind.IDENTIFIERS {
          if kind.text == token.text {
            token.kind = kind
            break
          }
        }
      }

      // Parse whitespace
      else if isSpace(text[i]) {
        while i < text.length and isSpace(text[i]) {
          if text[i] == '\n' {
            token.kind = TokenKind.NEWLINE
            line++
          }
          i++
        }
        if token.kind == TokenKind.ERROR { continue }
        if tokens.length > 0 and tokens[tokens.length - 1].kind == TokenKind.NEWLINE {
          tokens[tokens.length - 1].text += text.slice(start, i)
          continue
        }
      }

      // Parse string literal
      else if text[i] == '"' or text[i] == '\'' {
        i++
        while i < text.length {
          if text[i] == text[start] {
            token.kind = TokenKind.STRING_LIT
            i++
            break
          }
          else if text[i] == '\\' and i + 1 < text.length {
            string c = text[++i]
            if c == '"' or c == '\'' or c == '\\' { token.text += c }
            else if c == 't' { token.text += '\t' }
            else if c == 'r' { token.text += '\r' }
            else if c == 'n' { token.text += '\n' }
            else if c == 'x' {
              int hi = -1, lo = -1
              if i + 2 < text.length {
                hi = '0123456789abcdef'.indexOf(text[++i].toLowerCase())
                lo = '0123456789abcdef'.indexOf(text[++i].toLowerCase())
              }
              if hi == -1 or lo == -1 {
                log.error({ file, line }, 'byte escape sequence needs two hexadecimal characters')
                break 2
              }
              else { token.text += string.fromCharCode(hi * 16 + lo) }
            }
            else {
              log.warning({ file, line }, 'string contains invalid escape sequence "\\' + text[i] + '"')
              token.text += '\\' + text[i]
            }
            i++
          }
          else {
            if text[i] == '\n' { line++ }
            token.text += text[i++]
          }
        }
        if token.kind == TokenKind.ERROR {
          log.error(token.location, 'unterminated string literal')
          break
        }
        tokens.push(token)
        continue
      }

      // Check for single line comments
      else if text[i] == '/' and i + 1 < text.length and text[i + 1] == '/' {
        token.kind = TokenKind.COMMENT
        i += 2
        start = i
        while i < text.length {
          if text[i++] == '\n' {
            line++
            break
          }
        }
      }

      // Check for multi-line comments (they nest, unlike C-style comments)
      else if text[i] == '/' and i + 1 < text.length and text[i + 1] == '+' {
        i += 2
        int depth = 1
        while i + 1 < text.length {
          if text[i] == '/' and text[i + 1] == '+' {
            i += 2
            depth++
          }
          else if text[i] == '+' and text[i + 1] == '/' {
            i += 2
            if --depth == 0 { break }
          }
          else {
            if text[i] == '\n' { line++ }
            i++
          }
        }
        if depth > 0 {
          log.error(token.location, 'unterminated multi-line comment')
          break
        }
        continue
      }

      // Backslashes escape newlines
      else if text[i] == '\\' {
        i++
        while text[i] != '\n' and isSpace(text[i]) { i++ }
        if text[i] != '\n' {
          log.error(token.location, 'expected newline after backslash')
          break
        }
        i++
        continue
      }

      // Check for symbols
      else {
        for kind in TokenKind.SYMBOLS {
          if string t = kind.text and text.slice(i, i + t.length) == t {
            token.kind = kind
            i += t.length
            break
          }
        }

        // If we get here it's an error
        if token.kind == TokenKind.ERROR {
          log.error(token.location, 'unexpected "' + text[i] + '"')
          break
        }
      }

      // Set the text based on how far we've parsed
      token.text = text.slice(start, i)
      tokens.push(token)
    }

    // Every token string ends with an END_OF_FILE token
    tokens.push({ { file, line }, TokenKind.END_OF_FILE, '' })

    // Post-process the parsed tokens
    Token[] stack = {}
    i = 0
    while i < tokens.length {
      Token token = tokens[i++]

      // Try to merge this token with the previous token
      if i >= 2 {
        Token prev = tokens[i - 2]
        if prev.kind == TokenKind.NOT and token.kind == TokenKind.IN {
          // Need to handle "not in" specially
          token.kind = TokenKind.NOT_IN
          tokens.splice(--i - 1, 1)
        }
        else if prev.kind == TokenKind.IS and token.kind == TokenKind.NOT {
          // Need to handle "is not" specially
          token.kind = TokenKind.IS_NOT
          tokens.splice(--i - 1, 1)
        }
      }

      // Remove newlines based on previous token or next token to enable line
      // continuations. Note: Make sure to be conservative. We want to be
      // like CoffeeScript, not like JavaScript ASI! Anything that is at all
      // ambiguous should be disallowed.
      if token.kind in TokenKind.REMOVE_NEWLINE_BEFORE and i >= 2 and tokens[i - 2].kind == TokenKind.NEWLINE {
        tokens.splice(--i - 1, 1)
        continue
      }
      if token.kind == TokenKind.NEWLINE and i >= 2 and tokens[i - 2].kind in TokenKind.REMOVE_NEWLINE_AFTER {
        tokens.splice(--i, 1)
        continue
      }

      // Keep track of the current bracket nesting using a stack
      if token.kind in TokenKind.LEFT_BRACKETS { stack.push(token) }
      else if stack.length > 0 and token.kind in TokenKind.RIGHT_BRACKETS {
        Token top = stack.pop()

        // Convert parentheses to lambda argument tokens
        if top.kind == TokenKind.LPAREN and token.kind == TokenKind.RPAREN and
            i < tokens.length and tokens[i].kind == TokenKind.ARROW {
          top.kind = TokenKind.LARGS
          token.kind = TokenKind.RARGS
        }
      }
    }

    return tokens
  }
}
