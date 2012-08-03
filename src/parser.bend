class Parselet {
  TokenKind kind
  int leftBindingPower
  Expr? fn(Context)? prefixParselet = null
  Expr? fn(Context, Expr)? infixParselet = null
}

class Pratt {
  Parselet[] table = {}

  Parselet? find(TokenKind kind) {
    for parselet in table {
      if parselet.kind == kind { return parselet }
    }
    return null
  }

  Expr? parse(Context context, int rightBindingPower) {
    if Parselet parselet = find(context.current().kind) and
        Expr? fn(Context) prefixParselet = parselet.prefixParselet {
      Expr? maybeLeft = prefixParselet(context)
      while Expr left = maybeLeft and
          Parselet parselet = find(context.current().kind) and
          Expr? fn(Context, Expr) infixParselet = parselet.infixParselet and
          rightBindingPower < parselet.leftBindingPower {
        maybeLeft = infixParselet(context, left)
      }
      return maybeLeft
    }
    else { context.log.error(context.current().location, 'unexpected ' + context.current().kind.name) }
    return null
  }

  Parselet get(TokenKind kind, int bindingPower) {
    if Parselet parselet = find(kind) {
      if bindingPower > parselet.leftBindingPower {
        parselet.leftBindingPower = bindingPower
      }
      return parselet
    }
    Parselet parselet = { kind, bindingPower }
    table.push(parselet)
    return parselet
  }

  void literal(TokenKind kind, Expr? fn(Token) callback) {
    get(kind, 0).prefixParselet = context -> {
      Token token = context.current()
      context.next()
      return callback(token)
    }
  }

  void prefix(TokenKind kind, int bindingPower, Expr? fn(Context, Token, Expr) callback) {
    get(kind, 0).prefixParselet = context -> {
      Token token = context.current()
      context.next()
      if Expr value = parse(context, bindingPower) {
        return callback(context, token, value)
      }
      return null
    }
  }

  void postfix(TokenKind kind, int bindingPower, Expr? fn(Context, Expr, Token) callback) {
    get(kind, bindingPower).infixParselet = (context, left) -> {
      Token token = context.current()
      context.next()
      return callback(context, left, token)
    }
  }

  void infix(TokenKind kind, int bindingPower, bool rightAssociative, Expr? fn(Context, Expr, Token, Expr) callback) {
    int rightPower = bindingPower
    if rightAssociative { rightPower-- }
    get(kind, bindingPower).infixParselet = (context, left) -> {
      Token token = context.current()
      context.next()
      if Expr right = parse(context, rightPower) {
        return callback(context, left, token, right)
      }
      return null
    }
  }
}

class Context {
  Log log
  Token[] tokens
  int index = 0

  Token current() {
    return tokens[index]
  }

  void next() {
    if index + 1 < tokens.size { index++ }
  }

  bool peek(TokenKind kind) {
    return current().kind == kind
  }

  bool peekMore(TokenKind kind, int inc) {
    if index + inc >= tokens.size { return tokens[tokens.size - 1].kind == kind }
    return tokens[index + inc].kind == kind
  }

  bool eat(TokenKind kind) {
    if peek(kind) { next(); return true }
    return false
  }

  bool expect(TokenKind kind) {
    string text = current().text
    if eat(kind) { return true }
    log.error(current().location, 'expected ' + kind.name + ' but got ' + current().kind.name)
    return false
  }
}

class BlockKind {
  NORMAL
  MODULE
  CLASS
}

class UnaryInfo {
  TokenKind kind
  UnaryOp op

  static UnaryInfo[] PREFIX_INFO = {
    { TokenKind.SUB, UnaryOp.NEG }
    { TokenKind.NOT, UnaryOp.NOT }
    { TokenKind.BITNOT, UnaryOp.BITNOT }
    { TokenKind.INC, UnaryOp.PRE_INC }
    { TokenKind.DEC, UnaryOp.PRE_DEC }
  }

  static UnaryInfo[] POSTFIX_INFO = {
    { TokenKind.INC, UnaryOp.POST_INC }
    { TokenKind.DEC, UnaryOp.POST_DEC }
  }
}

class BinaryInfo {
  TokenKind kind
  BinaryOp op
  int precedence

  static BinaryInfo[] INFO = {
    { TokenKind.ASSIGN, BinaryOp.ASSIGN, 10 }
    { TokenKind.ADD_ASSIGN, BinaryOp.ADD_ASSIGN, 10 }
    { TokenKind.SUB_ASSIGN, BinaryOp.SUB_ASSIGN, 10 }
    { TokenKind.MUL_ASSIGN, BinaryOp.MUL_ASSIGN, 10 }
    { TokenKind.DIV_ASSIGN, BinaryOp.DIV_ASSIGN, 10 }
    { TokenKind.MOD_ASSIGN, BinaryOp.MOD_ASSIGN, 10 }
    { TokenKind.SHL_ASSIGN, BinaryOp.SHL_ASSIGN, 10 }
    { TokenKind.SHR_ASSIGN, BinaryOp.SHR_ASSIGN, 10 }
    { TokenKind.BITOR_ASSIGN, BinaryOp.BITOR_ASSIGN, 10 }
    { TokenKind.BITAND_ASSIGN, BinaryOp.BITAND_ASSIGN, 10 }
    { TokenKind.BITXOR_ASSIGN, BinaryOp.BITXOR_ASSIGN, 10 }
    { TokenKind.OR, BinaryOp.OR, 30 }
    { TokenKind.AND, BinaryOp.AND, 40 }
    { TokenKind.BITOR, BinaryOp.BITOR, 50 }
    { TokenKind.BITXOR, BinaryOp.BITXOR, 60 }
    { TokenKind.BITAND, BinaryOp.BITAND, 70 }
    { TokenKind.EQ, BinaryOp.EQ, 80 }
    { TokenKind.NEQ, BinaryOp.NEQ, 80 }
    { TokenKind.LT, BinaryOp.LT, 90 }
    { TokenKind.GT, BinaryOp.GT, 90 }
    { TokenKind.LTE, BinaryOp.LTE, 90 }
    { TokenKind.GTE, BinaryOp.GTE, 90 }
    { TokenKind.IS, BinaryOp.IS, 90 }
    { TokenKind.IN, BinaryOp.IN, 90 }
    { TokenKind.SHL, BinaryOp.SHL, 100 }
    { TokenKind.SHR, BinaryOp.SHR, 100 }
    { TokenKind.ADD, BinaryOp.ADD, 110 }
    { TokenKind.SUB, BinaryOp.SUB, 110 }
    { TokenKind.MUL, BinaryOp.MUL, 120 }
    { TokenKind.DIV, BinaryOp.DIV, 120 }
    { TokenKind.MOD, BinaryOp.MOD, 120 }
  }
}

class Parser {
  Pratt pratt = {}
  Pratt prattWithoutMatch = {}

  this() {
    int ASSIGNMENT_PRECEDENCE = 10
    int TERTIARY_PRECEDENCE = 20
    int MATCH_VALUE_PRECEDENCE = 40
    int IS_AND_IN_PRECEDENCE = 90
    int UNARY_PRECEDENCE = 130
    int MATCH_PRECEDENCE = 140
    int TIGHT_PRECEDENCE = 150

    // Literals
    pratt.literal(TokenKind.NULL, token -> TypeExpr(token.location, SpecialType.NULL))
    pratt.literal(TokenKind.VOID, token -> TypeExpr(token.location, SpecialType.TYPE_VOID))
    pratt.literal(TokenKind.VAR, token -> TypeExpr(token.location, MetaType.VAR))
    pratt.literal(TokenKind.INT, token -> TypeExpr(token.location, ClassType.INT.metaType))
    pratt.literal(TokenKind.BOOL, token -> TypeExpr(token.location, ClassType.BOOL.metaType))
    pratt.literal(TokenKind.FLOAT, token -> TypeExpr(token.location, ClassType.FLOAT.metaType))
    pratt.literal(TokenKind.STRING, token -> TypeExpr(token.location, ClassType.STRING.metaType))
    pratt.literal(TokenKind.THIS, token -> ThisExpr(token.location))
    pratt.literal(TokenKind.BASE, token -> BaseExpr(token.location))
    pratt.literal(TokenKind.TRUE, token -> BoolExpr(token.location, true))
    pratt.literal(TokenKind.FALSE, token -> BoolExpr(token.location, false))
    pratt.literal(TokenKind.INT_LIT, token -> IntExpr(token.location, token.text.toInt()))
    pratt.literal(TokenKind.FLOAT_LIT, token -> FloatExpr(token.location, token.text.toFloat()))
    pratt.literal(TokenKind.STRING_LIT, token -> StringExpr(token.location, token.text))

    // Parenthesized group
    pratt.get(TokenKind.LPAREN, 0).prefixParselet = context -> {
      context.next()
      if Expr node = pratt.parse(context, 0) and context.expect(TokenKind.RPAREN) { return node }
      return null
    }

    // Identifier or lambda shortcut
    pratt.get(TokenKind.IDENT, 0).prefixParselet = context -> {
      Token token = context.current()
      context.next()
      if context.eat(TokenKind.ARROW) {
        return parseLambdaBody(context, token.location, { IdentExpr(token.location, token.text) })
      }
      return IdentExpr(token.location, token.text)
    }

    // Unary prefix operators
    for info in UnaryInfo.PREFIX_INFO {
      pratt.prefix(info.kind, UNARY_PRECEDENCE, (context, token, value) -> {
        if (info.op == UnaryOp.PRE_INC or info.op == UnaryOp.PRE_DEC) and
            not isLeftHandSide(context, value) {
          return null
        }
        return UnaryExpr(token.location, info.op, value)
      })
    }

    // Unary postfix operators
    for info in UnaryInfo.POSTFIX_INFO {
      pratt.postfix(info.kind, UNARY_PRECEDENCE, (context, value, token) -> {
        if (info.op == UnaryOp.POST_INC or info.op == UnaryOp.POST_DEC) and
            not isLeftHandSide(context, value) {
          return null
        }
        return UnaryExpr(token.location, info.op, value)
      })
    }

    // Binary operators
    for info in BinaryInfo.INFO {
      int precedence = info.precedence
      bool isAssignment = precedence == ASSIGNMENT_PRECEDENCE
      pratt.infix(info.kind, precedence, isAssignment, (context, left, token, right) -> {
        if isAssignment and not isLeftHandSide(context, left) { return null }
        return BinaryExpr(token.location, left, info.op, right)
      })
    }

    // Compound binary operators
    pratt.infix(TokenKind.IS_NOT, IS_AND_IN_PRECEDENCE, false, (context, left, token, right) -> {
      return UnaryExpr(token.location, UnaryOp.NOT, BinaryExpr(token.location, left, BinaryOp.IS, right))
    })
    pratt.infix(TokenKind.NOT_IN, IS_AND_IN_PRECEDENCE, false, (context, left, token, right) -> {
      return UnaryExpr(token.location, UnaryOp.NOT, BinaryExpr(token.location, left, BinaryOp.IN, right))
    })

    // Tertiary operator
    pratt.get(TokenKind.IF, TERTIARY_PRECEDENCE).infixParselet = (context, left) -> {
      context.next()
      if Expr middle = pratt.parse(context, TERTIARY_PRECEDENCE) and
          context.expect(TokenKind.ELSE) and
          Expr right = pratt.parse(context, TERTIARY_PRECEDENCE) {
        return TertiaryExpr(middle.location, left, middle, right)
      }
    }

    // Initializer list
    pratt.get(TokenKind.LBRACE, 0).prefixParselet = context -> {
      Location location = context.current().location
      context.next()
      Expr[] values = {}
      while not context.eat(TokenKind.RBRACE) {
        // Ignore comments before initializer list items
        while context.eat(TokenKind.COMMENT) { context.eat(TokenKind.NEWLINE) }

        // Parse the list item
        if Expr value = pratt.parse(context, 0) {
          values.push(value)
        }
        else { return null }

        // Ignore comments after initializer list items
        while context.eat(TokenKind.COMMENT) { context.eat(TokenKind.NEWLINE) }

        // Check if there may be more items
        if not context.eat(TokenKind.NEWLINE) and not context.eat(TokenKind.COMMA) {
          if not context.expect(TokenKind.RBRACE) { return null }
          break
        }
      }
      return InitExpr(location, values)
    }

    // Function call
    pratt.get(TokenKind.LPAREN, TIGHT_PRECEDENCE).infixParselet = (context, left) -> {
      Location location = context.current().location
      context.next()
      Expr[] args = {}
      while not context.eat(TokenKind.RPAREN) {
        if args.size > 0 and not context.expect(TokenKind.COMMA) { return null }
        if Expr arg = pratt.parse(context, 0) {
          args.push(arg)
        }
        else { return null }
      }
      return CallExpr(location, left, args)
    }

    // Nullable type
    pratt.postfix(TokenKind.QUESTION, TIGHT_PRECEDENCE, (context, value, token) -> {
      return NullableTypeExpr(token.location, value)
    })

    // Function type
    pratt.get(TokenKind.FN, TIGHT_PRECEDENCE).infixParselet = (context, left) -> {
      Location location = context.current().location
      context.next()
      if not context.expect(TokenKind.LARGS) { return null }
      Expr[] args = {}
      while not context.eat(TokenKind.RARGS) {
        if args.size > 0 and not context.expect(TokenKind.COMMA) { return null }
        if Expr arg = pratt.parse(context, 0) { args.push(arg) }
        else { return null }
      }
      return FuncTypeExpr(location, left, args)
    }

    // Resolved class type
    pratt.get(TokenKind.LPARAM, TIGHT_PRECEDENCE).infixParselet = (context, left) -> {
      Location location = context.current().location
      context.next()
      Expr[] params = {}
      while not context.eat(TokenKind.RPARAM) {
        if params.size > 0 and not context.expect(TokenKind.COMMA) { return null }
        if Expr param = pratt.parse(context, 0) { params.push(param) }
        else { return null }
      }
      return SpecificTypeExpr(location, left, params)
    }

    // Index access
    pratt.get(TokenKind.LBRACKET, TIGHT_PRECEDENCE).infixParselet = (context, left) -> {
      Location location = context.current().location
      context.next()
      if context.eat(TokenKind.RBRACKET) {
        return ArrayTypeExpr(location, left)
      }
      if Expr node = pratt.parse(context, 0) and context.expect(TokenKind.RBRACKET) {
        return IndexExpr(location, left, node)
      }
      return null
    }

    // Member access
    pratt.get(TokenKind.DOT, TIGHT_PRECEDENCE).infixParselet = (context, left) -> {
      Location location = context.current().location
      context.next()
      IdentExpr id = { context.current().location, context.current().text }
      if context.expect(TokenKind.IDENT) { return MemberExpr(location, left, id) }
      else { return null }
    }

    // Lambda expression shortcut
    pratt.get(TokenKind.ARROW, 0).prefixParselet = context -> {
      Location location = context.current().location
      context.next()
      return parseLambdaBody(context, location, {})
    }

    // Lambda expression
    pratt.get(TokenKind.LARGS, 0).prefixParselet = context -> {
      Location location = context.current().location
      IdentExpr[] args = {}
      context.next()
      while not context.eat(TokenKind.RARGS) {
        if args.size > 0 and not context.expect(TokenKind.COMMA) { return null }
        Token token = context.current()
        args.push(IdentExpr(token.location, context.current().text))
        if not context.expect(TokenKind.IDENT) { return null }
      }
      if not context.expect(TokenKind.ARROW) { return null }
      return parseLambdaBody(context, location, args)
    }

    // Match expression
    pratt.get(TokenKind.IDENT, MATCH_PRECEDENCE).infixParselet = (context, left) -> {
      IdentExpr id = { context.current().location, context.current().text }
      context.next()
      if not context.expect(TokenKind.ASSIGN) { return null }
      if Expr value = pratt.parse(context, MATCH_VALUE_PRECEDENCE) {
        return MatchExpr(id.location, id, left, value)
      }
      else { return null }
    }

    // Create another pratt parser that doesn't parse top-level match expressions
    prattWithoutMatch.table = pratt.table.slice()
    int i = 0
    while i < prattWithoutMatch.table.size {
      Parselet oldParselet = prattWithoutMatch.table[i]
      if oldParselet.kind == TokenKind.IDENT {
        Parselet newParselet = { oldParselet.kind, oldParselet.leftBindingPower }
        newParselet.prefixParselet = oldParselet.prefixParselet
        prattWithoutMatch.table[i] = newParselet
        break
      }
      i++
    }
  }

  Module? parse(Log log, Token[] tokens) {
    Context context = { log, tokens }
    Location location = context.current().location
    if Block block = parseBlock(context, BlockKind.MODULE, '') {
      return { location, block }
    }
    return null
  }

  LambdaExpr? parseLambdaBody(Context context, Location location, IdentExpr[] args) {
    if context.peek(TokenKind.LBRACE) {
      if Block body = parseBlock(context, BlockKind.NORMAL, '') { return LambdaExpr(location, args, body) }
      else { return null }
    }
    if Expr value = pratt.parse(context, 0) {
      Block body = { value.location }
      body.stmts.push(ReturnStmt(value.location, null, value))
      return LambdaExpr(location, args, body)
    }
    return null
  }

  bool eatEndOfLine(Context context) {
    return context.eat(TokenKind.SEMICOLON) or
      context.eat(TokenKind.NEWLINE) or
      context.peek(TokenKind.RBRACE) or
      context.peek(TokenKind.RBRACKET) or
      context.peek(TokenKind.END_OF_FILE)
  }

  bool eatCommentedEndOfLine(Context context, Stmt stmt) {
    string comment = context.current().text
    if context.eat(TokenKind.COMMENT) {
      if string existing = stmt.comment { stmt.comment = existing + comment }
      else { stmt.comment = comment }
      context.eat(TokenKind.NEWLINE)
      return true
    }
    return eatEndOfLine(context)
  }

  bool expectEndOfLine(Context context) {
    if eatEndOfLine(context) { return true }
    return context.expect(TokenKind.NEWLINE)
  }

  bool expectCommentedEndOfLine(Context context, Stmt stmt) {
    if eatCommentedEndOfLine(context, stmt) { return true }
    return context.expect(TokenKind.NEWLINE)
  }

  bool isLeftHandSide(Context context, Expr node) {
    if node is IdentExpr or node is MemberExpr or node is IndexExpr { return true }
    context.log.error(node.location, 'invalid left-hand side in assignment')
    return false
  }

  string appendComment(string? start, string end) {
    if string start = start { return start + end }
    return end
  }

  void insertCommentInBlock(string? comment, Block block) {
    if block.stmts.size > 0 and block.stmts[0].comment == null {
      block.stmts[0].comment = comment
    }
    else { block.stmts.unshift(CommentStmt(block.location, comment)) }
  }

  VarDef[]? parseFunctionArgs(Context context) {
    VarDef[] args = {}
    int splats = 0
    while not context.eat(TokenKind.RPAREN) {
      if args.size > 0 and not context.expect(TokenKind.COMMA) { return null }
      if Expr type = prattWithoutMatch.parse(context, 0) {
        VarDef arg = { type.location, null, IdentExpr(context.current().location, context.current().text), 0, type, null }
        if not context.expect(TokenKind.IDENT) { return null }
        if context.eat(TokenKind.SPLAT) {
          arg.isSplat = true
          if ++splats > 1 {
            context.log.error(arg.location, 'function has multiple splats')
            return null
          }
        }
        args.push(arg)
      }
      else { return null }
    }
    return args
  }

  Block? parseFunctionBody(Context context) {
    if context.peek(TokenKind.LBRACE) or (
        context.peek(TokenKind.NEWLINE) and
        context.peekMore(TokenKind.LBRACE, 1)) {
      return parseBlock(context, BlockKind.NORMAL, '')
    }
    return null
  }

  // Blocks are parsed as a unit instead of as individual statements because
  // each statement may compile into zero or multiple statements. For example,
  // declaring multiple comma-separated variables on the same line generates
  // one VarDef statement for each variable.
  Block? parseBlock(Context context, BlockKind kind, string className) {
    Block block = { context.current().location }
    TokenKind stop = TokenKind.RBRACE

    // Parse the start of the block
    if kind == BlockKind.MODULE { stop = TokenKind.END_OF_FILE }
    else {
      // Allow an optional newline before the left brace to support different styles
      context.eat(TokenKind.NEWLINE)
      if not context.expect(TokenKind.LBRACE) { return null }
    }
    context.eat(TokenKind.NEWLINE)

    // Keep parsing statements until the stop token
    while not context.eat(stop) {
      Location location = context.current().location

      // See if this statement has one or more comment lines
      string? comment = null
      while context.peek(TokenKind.COMMENT) {
        string appended = appendComment(comment, context.current().text)
        comment = appended
        context.next()

        // Generate a separate statement if this is is a comment block
        if context.eat(TokenKind.NEWLINE) {
          block.stmts.push(CommentStmt(location, appended))
          comment = null
          if context.eat(stop) { break 2 }
        }
        else if context.eat(stop) {
          block.stmts.push(CommentStmt(location, appended))
          break 2
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
      if context.eat(TokenKind.IF) {
        if Expr value = pratt.parse(context, 0) and
            Block trueBody = parseBlock(context, BlockKind.NORMAL, '') {
          IfStmt ifStmt = { location, comment, value, trueBody, null }
          Block blockForTrailingComment = ifStmt.trueBody
          block.stmts.push(ifStmt)
          while true {
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
            int index = 0
            if context.peek(TokenKind.COMMENT) { index++ }
            else if context.peek(TokenKind.NEWLINE) {
              index++
              while context.peekMore(TokenKind.COMMENT, index) { index++ }
            }
            if not context.peekMore(TokenKind.ELSE, index) { break }

            // Actually parse the comment now that we know there is an "else"
            comment = null
            if context.peek(TokenKind.COMMENT) {
              insertCommentInBlock(context.current().text, blockForTrailingComment)
              context.next()
            }
            else if context.eat(TokenKind.NEWLINE) {
              while context.peek(TokenKind.COMMENT) {
                comment = appendComment(comment, context.current().text)
                context.next()
              }
            }
            context.eat(TokenKind.ELSE)

            // Parse the else block
            location = context.current().location
            if context.eat(TokenKind.IF) {
              if Expr value = pratt.parse(context, 0) and
                  Block trueBody = parseBlock(context, BlockKind.NORMAL, '') {
                IfStmt nestedIfStmt = { location, null, value, trueBody, null }
                Block falseBody = { location }
                insertCommentInBlock(comment, trueBody)
                falseBody.stmts.push(nestedIfStmt)
                ifStmt.falseBody = falseBody
                ifStmt = nestedIfStmt
                blockForTrailingComment = trueBody
              }
              else { return null }
            }
            else if Block body = parseBlock(context, BlockKind.NORMAL, '') {
              insertCommentInBlock(comment, body)
              ifStmt.falseBody = body
              blockForTrailingComment = body
              break
            }
            else { return null }
          }
          if context.peek(TokenKind.COMMENT) {
            insertCommentInBlock(context.current().text, blockForTrailingComment)
            context.next()
          }
          if not expectEndOfLine(context) { return null }
          continue
        }
        else { return null }
      }

      // While statement
      if context.eat(TokenKind.WHILE) {
        if Expr value = pratt.parse(context, 0) and
            Block body = parseBlock(context, BlockKind.NORMAL, '') {
          WhileStmt whileStmt = { location, comment, value, body }
          if not expectCommentedEndOfLine(context, whileStmt) { return null }
          block.stmts.push(whileStmt)
          continue
        }
        else { return null }
      }

      // For-in statement
      if context.eat(TokenKind.FOR) {
        if Expr value = pratt.parse(context, 0) and
            Block body = parseBlock(context, BlockKind.NORMAL, '') {
          if BinaryExpr binaryExpr = value and binaryExpr.op == BinaryOp.IN and IdentExpr id = binaryExpr.left {
            ForStmt forStmt = { location, comment, id, binaryExpr.right, body }
            if not expectCommentedEndOfLine(context, forStmt) { return null }
            block.stmts.push(forStmt)
            continue
          }
          else {
            context.log.error(location, 'invalid for-in statement')
            return null
          }
        }
        else { return null }
      }

      // Fail statement
      if context.eat(TokenKind.FAIL) {
        // Generate a default failure message
        string text = 'fail ' + location
        FailStmt failStmt = { location, comment, StringExpr(location, text) }

        // Check for a custom failure message
        if not eatCommentedEndOfLine(context, failStmt) {
          if Expr value = pratt.parse(context, 0) {
            failStmt.value = value
            if not expectCommentedEndOfLine(context, failStmt) { return null }
          }
          else { return null }
        }
        block.stmts.push(failStmt)
        continue
      }

      // Return statement
      if context.eat(TokenKind.RETURN) {
        ReturnStmt returnStmt = { location, comment, null }
        if not eatCommentedEndOfLine(context, returnStmt) {
          returnStmt.value = pratt.parse(context, 0)
          if returnStmt.value == null or not expectCommentedEndOfLine(context, returnStmt) { return null }
        }
        block.stmts.push(returnStmt)
        continue
      }

      // Continue statement
      if context.eat(TokenKind.CONTINUE) {
        string text = context.current().text
        int value = 1
        if context.eat(TokenKind.INT_LIT) { value = text.toInt() }
        ContinueStmt continueStmt = { location, comment, value }
        if not expectCommentedEndOfLine(context, continueStmt) { return null }
        block.stmts.push(continueStmt)
        continue
      }

      // Break statement
      if context.eat(TokenKind.BREAK) {
        string text = context.current().text
        int value = 1
        if context.eat(TokenKind.INT_LIT) { value = text.toInt() }
        BreakStmt breakStmt = { location, comment, value }
        if not expectCommentedEndOfLine(context, breakStmt) { return null }
        block.stmts.push(breakStmt)
        continue
      }

      // Parse modifiers for definitions in a specific order
      int modifiers = 0
      if context.eat(TokenKind.EXTERN) { modifiers |= Modifier.EXTERN }
      if context.eat(TokenKind.ABSTRACT) { modifiers |= Modifier.ABSTRACT }
      if context.eat(TokenKind.STATIC) { modifiers |= Modifier.STATIC }
      if context.eat(TokenKind.OVER) { modifiers |= Modifier.OVER }
      if context.eat(TokenKind.PROP) {
        string propName = context.current().text
        if not context.expect(TokenKind.IDENT) { return null }
        modifiers |= Modifier.PROP
      }

      // Class definition
      if context.eat(TokenKind.CLASS) {
        IdentExpr id = { context.current().location, context.current().text }
        if not context.expect(TokenKind.IDENT) { return null }
        TypeParam[] params = {}
        if context.eat(TokenKind.LPARAM) {
          while not context.eat(TokenKind.RPARAM) {
            if params.size > 0 and not context.expect(TokenKind.COMMA) { return null }
            params.push({ context.current().text, params.size })
            if not context.expect(TokenKind.IDENT) { return null }
          }
        }
        Expr? baseType = null
        if context.eat(TokenKind.IS) {
          baseType = pratt.parse(context, 0)
          if baseType == null { return null }
        }
        if Block body = parseBlock(context, BlockKind.CLASS, id.name) {
          ClassDef classDef = { location, comment, id, modifiers, baseType, body, params }
          if not expectCommentedEndOfLine(context, classDef) { return null }
          block.stmts.push(classDef)
          continue
        }
        else { return null }
      }

      // Constructor definition
      if kind == BlockKind.CLASS and context.eat(TokenKind.THIS) {
        IdentExpr id = { context.current().location, 'this' }
        if not context.expect(TokenKind.LPAREN) { return null }
        if VarDef[] args = parseFunctionArgs(context) {
          Block? body = parseFunctionBody(context)
          CtorDef ctorDef = { location, comment, id, modifiers, args, body }
          if not expectCommentedEndOfLine(context, ctorDef) { return null }
          block.stmts.push(ctorDef)
          continue
        }
        else { return null }
      }

      // Typed definition
      if Expr type = prattWithoutMatch.parse(context, 0) {
        // Parse an expression statement
        if modifiers == 0 and not context.peek(TokenKind.IDENT) {
          Stmt stmt
          if kind == BlockKind.CLASS {
            // Free identifiers and simple assignments inside a class definition
            // are converted to static instance variables of the same type as the
            // class they are contained in to support (more powerful) enum-like
            // behavior.
            if IdentExpr id = type {
              stmt = VarDef(location, comment, id, Modifier.STATIC, IdentExpr(location, className), InitExpr(location, {}))
            }
            else if BinaryExpr binaryExpr = type and binaryExpr.op == BinaryOp.ASSIGN and IdentExpr id = binaryExpr.left {
              stmt = VarDef(location, comment, id, Modifier.STATIC, IdentExpr(location, className), binaryExpr.right)
            }
            else {
              context.log.error(location, 'free expression not allowed in class definition')
              return null
            }
          }
          else { stmt = ExprStmt(location, comment, type) }
          if not expectCommentedEndOfLine(context, stmt) { return null }
          block.stmts.push(stmt)
          continue
        }

        // Symbol definition
        IdentExpr id = { context.current().location, context.current().text }
        if not context.expect(TokenKind.IDENT) { return null }

        // Function definition
        if context.eat(TokenKind.LPAREN) {
          if VarDef[] args = parseFunctionArgs(context) {
            Block? body = parseFunctionBody(context)
            FuncDef funcDef = { location, comment, id, modifiers, type, args, body }
            if not expectCommentedEndOfLine(context, funcDef) { return null }
            block.stmts.push(funcDef)
            continue
          }
          else { return null }
        }

        // Variable definition
        while true {
          Expr? value = null
          if context.eat(TokenKind.ASSIGN) {
            value = pratt.parse(context, 0)
            if value == null { return null }
          }
          VarDef varDef = { location, comment, id, modifiers, type, value }
          block.stmts.push(varDef)
          comment = null // The beginning comment only applies to the first variable
          if not context.eat(TokenKind.COMMA) {
            if not expectCommentedEndOfLine(context, varDef) { return null }
            break
          }
          id = { context.current().location, context.current().text }
          if not context.expect(TokenKind.IDENT) { return null }
        }
      }
      else { return null }
    }

    return block
  }
}
