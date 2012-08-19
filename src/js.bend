class JsUnaryInfo {
  UnaryOp op
  JsUnaryOp jsOp

  static JsUnaryInfo[] INFO = {
    { UnaryOp.NEG, JsUnaryOp.NEG }
    { UnaryOp.NOT, JsUnaryOp.NOT }
    { UnaryOp.BITNOT, JsUnaryOp.BITNOT }
    { UnaryOp.PRE_INC, JsUnaryOp.PRE_INC }
    { UnaryOp.PRE_DEC, JsUnaryOp.PRE_DEC }
    { UnaryOp.POST_INC, JsUnaryOp.POST_INC }
    { UnaryOp.POST_DEC, JsUnaryOp.POST_DEC }
  }
}

class JsBinaryInfo {
  BinaryOp op
  JsBinaryOp jsOp

  static JsBinaryInfo[] INFO = {
    { BinaryOp.ADD_ASSIGN, JsBinaryOp.ADD_ASSIGN }
    { BinaryOp.SUB_ASSIGN, JsBinaryOp.SUB_ASSIGN }
    { BinaryOp.MUL_ASSIGN, JsBinaryOp.MUL_ASSIGN }
    { BinaryOp.DIV_ASSIGN, JsBinaryOp.DIV_ASSIGN }
    { BinaryOp.MOD_ASSIGN, JsBinaryOp.MOD_ASSIGN }
    { BinaryOp.SHL_ASSIGN, JsBinaryOp.SHL_ASSIGN }
    { BinaryOp.SHR_ASSIGN, JsBinaryOp.USHR_ASSIGN }
    { BinaryOp.BITOR_ASSIGN, JsBinaryOp.BITOR_ASSIGN }
    { BinaryOp.BITAND_ASSIGN, JsBinaryOp.BITAND_ASSIGN }
    { BinaryOp.BITXOR_ASSIGN, JsBinaryOp.BITXOR_ASSIGN }
    { BinaryOp.AND, JsBinaryOp.AND }
    { BinaryOp.OR, JsBinaryOp.OR }
    { BinaryOp.ADD, JsBinaryOp.ADD }
    { BinaryOp.SUB, JsBinaryOp.SUB }
    { BinaryOp.MUL, JsBinaryOp.MUL }
    { BinaryOp.DIV, JsBinaryOp.DIV }
    { BinaryOp.MOD, JsBinaryOp.MOD }
    { BinaryOp.SHL, JsBinaryOp.SHL }
    { BinaryOp.SHR, JsBinaryOp.USHR }
    { BinaryOp.BITOR, JsBinaryOp.BITOR }
    { BinaryOp.BITAND, JsBinaryOp.BITAND }
    { BinaryOp.BITXOR, JsBinaryOp.BITXOR }
    { BinaryOp.EQ, JsBinaryOp.SEQ }
    { BinaryOp.NEQ, JsBinaryOp.SNEQ }
    { BinaryOp.LTE, JsBinaryOp.LTE }
    { BinaryOp.GTE, JsBinaryOp.GTE }
    { BinaryOp.LT, JsBinaryOp.LT }
    { BinaryOp.GT, JsBinaryOp.GT }
    { BinaryOp.ASSIGN, JsBinaryOp.ASSIGN }
    { BinaryOp.IS, JsBinaryOp.INSTANCEOF }
  }
}

class JsConverter {
  class ThisAlias {
    string name
    int nestedFuncCount = 0
    bool isUsed = false

    string resolveName() {
      if nestedFuncCount == 0 { return 'this' }
      isUsed = true
      return name
    }
  }

  // TODO: consolidate loop stuff into a map when maps are a thing?
  class LoopEnv {
    Scope loopScope
    string envName
  }

  JsLoopCaptures loopCaptures = {}
  ThisAlias thisAlias = { 'this' }
  LoopEnv[] loopEnvs = {}
  string?[] loops = {}

  // TODO: Remove hack
  int hack = 0

  string? envForSymbol(Symbol symbol) {
    for info in loopCaptures.symbolInfo {
      if info.symbol == symbol {
        for loopEnv in loopEnvs {
          if loopEnv.loopScope == info.loopScope {
            return loopEnv.envName
          }
        }
        // Can't use findUnusedName() here because it won't catch duplicate names in nested functions:
        // 
        //   while true {
        //     int i
        //     void func() { int env = i }
        //   }
        // 
        string envName = 'env$' + hack++
        loopEnvs.push(LoopEnv(info.loopScope, envName))
        return envName
      }
    }
    return null
  }

  JsNode[] visitStmts(Stmt[] stmts) {
    JsNode[] nodes = {}
    for stmt in stmts {
      JsNode[] visited = visitStmt(stmt)
      if visited.size == 0 and stmt is not CommentStmt { continue }
      if string comment = stmt.comment { nodes.push(JsComment(comment, stmt is CommentStmt)) }
      nodes = nodes.concat(visited)
    }
    return nodes
  }

  JsNode constructPrefix(string[] list) {
    if list.size == 0 { fail }
    if list.size == 1 { return JsText(list[0]) }
    return JsBinary(constructPrefix(list.slice(0, list.size - 1)),
      JsBinaryOp.MEMBER, JsText(list[list.size - 1]))
  }

  string[] prefixFromScope(Scope? nextScope) {
    string[] prefix = {}
    if Scope scope = nextScope and scope.kind == ScopeKind.INSTANCE {
      prefix.unshift('prototype')
      nextScope = scope.parentScope
    }
    while Scope scope = nextScope and scope.kind == ScopeKind.STATIC and Symbol symbol = scope.symbol {
      prefix.unshift(symbol.name)
      nextScope = scope.parentScope
    }
    return prefix
  }

  JsNode[] makeVar(Symbol symbol, JsNode? value, Scope scope) {
    // Don't emit extern symbols
    if (symbol.modifiers & Modifier.EXTERN) != 0 {
      return {}
    }
    if string env = envForSymbol(symbol) {
      JsNode node = JsBinary(JsText(env), JsBinaryOp.MEMBER, JsText(symbol.name))
      if JsNode value = value { node = JsBinary(node, JsBinaryOp.ASSIGN, value) }
      return { node }
    }
    string[] prefix = prefixFromScope(scope)
    if prefix.size == 0 {
      return { JsVars({ JsVar(symbol.name, value) }) }
    }
    JsNode prefixChain = constructPrefix(prefix.concat({ symbol.name }))
    if JsNode value = value { return { JsBinary(prefixChain, JsBinaryOp.ASSIGN, value) } }
    return { prefixChain }
  }

  string findUnusedName(Scope scope, string prefix) {
    // Get to the top-level scope
    while scope.kind in JsRenamer.IGNORED_SCOPE_KINDS and Scope parentScope = scope.parentScope {
      scope = parentScope
    }

    // Find an unused name
    string name = prefix
    int count = 2
    while scope.find(name) != null { name = prefix + count++ }

    // Reserve that name
    scope.symbols.push({ SymbolKind.DUMMY, name, SpecialType.ERROR, scope, 0 })
    return name
  }

  JsFunc generateFunction(Scope scope, FuncType funcType, VarDef[] args, Block body) {
    thisAlias.nestedFuncCount++
    JsNode[] jsArgs = {}
    JsNode[] argStmts = {}
    int i = 0
    for arg in args {
      string argName = symbol.name if Symbol symbol = arg.id.symbol else arg.id.name
      if funcType.splatIndex == -1 or i < funcType.splatIndex {
        jsArgs.push(JsText(argName))
      }
      else if i == funcType.splatIndex {
        int end = funcType.splatIndex - funcType.argTypes.size + 1
        JsNode[] callArgs = { JsText('arguments') }
        if i > 0 or end < 0 { callArgs.push(JsText(i.toString())) }
        if end < 0 { callArgs.push(JsText(end.toString())) }
        argStmts.push(JsVars({ JsVar(
          argName,
          JsCall(
            JsBinary(
              JsBinary(
                JsBinary(
                  JsText('Array'),
                  JsBinaryOp.MEMBER,
                  JsText('prototype')),
                JsBinaryOp.MEMBER,
                JsText('slice')),
              JsBinaryOp.MEMBER,
              JsText('call')),
            callArgs)) }))
      }
      else {
        int index = args.size - i
        argStmts.push(JsVars({ JsVar(
          argName,
          JsBinary(
            JsText('arguments'),
            JsBinaryOp.INDEX,
            JsBinary(
              JsBinary(JsText('arguments'), JsBinaryOp.MEMBER, JsText('length')),
              JsBinaryOp.SUB,
              JsText(index.toString())))) }))
      }
      i++
    }
    JsBlock jsBody = { declareVarsBeforeStmts(body.scope, body.stmts, scope.kind == ScopeKind.INSTANCE) }
    for stmt in argStmts.reverse() { jsBody.nodes.unshift(stmt) }
    thisAlias.nestedFuncCount--
    return JsFunc(null, jsArgs, jsBody)
  }

  JsNode[] visitStmt(Stmt node) {
    // Statements
    if ExprStmt node = node {
      return { visit(node.value) }
    }
    else if IfStmt node = node {
      JsNode? no = null
      if Block falseBody = node.falseBody {
        JsNode elseBlock = visit(falseBody)
        if JsBlock block = elseBlock and block.nodes.size == 1 and JsIf elseIf = block.nodes[0] {
          no = elseIf
        }
        else { no = elseBlock }
      }
      return { JsIf(visit(node.value), visit(node.trueBody), no) }
    }
    else if WhileStmt node = node {
      loops.push(null)
      if JsBlock body = visit(node.body) {
        string? label = loops.pop()

        // Generate the environment if used
        for loopEnv in loopEnvs {
          if loopEnv.loopScope == node.body.scope {
            body = JsBlock({ JsWith(JsObj({ JsProp(loopEnv.envName, JsObj({})) }), body) })
            break
          }
        }

        return { JsWhile(visit(node.value), body, label) }
      }
      else { fail }
    }
    else if ForStmt node = node {
      loops.push(null)
      if JsBlock body = visit(node.body) {
        string? label = loops.pop()
        JsText index = { findUnusedName(node.scope, 'i') }
        JsVars vars = { { JsVar(index.text, JsText('0')) } }
        JsNode array = visit(node.value)

        // Only generate a separate if node.value isn't simple (a MemberExpr chain ending in an IdentExpr)
        Expr value = node.value
        while MemberExpr memberExpr = value { value = memberExpr.value }
        if value is not IdentExpr {
          string name = findUnusedName(node.scope, 'array')
          vars.nodes.push(JsVar(name, array))
          array = JsText(name)
        }

        // Generate the loop variable
        JsBinary arrayIndex = { array, JsBinaryOp.INDEX, index }
        if Symbol symbol = node.id.symbol {
          if string env = envForSymbol(symbol) {
            body.nodes.unshift(JsBinary(
              JsBinary(JsText(env), JsBinaryOp.MEMBER, JsText(symbol.name)),
              JsBinaryOp.ASSIGN, arrayIndex))
          }
          else {
            body.nodes.unshift(JsVars({ JsVar(symbol.name, arrayIndex) }))
          }
        }

        // Generate the environment if used
        for loopEnv in loopEnvs {
          if loopEnv.loopScope == node.body.scope {
            body = JsBlock({ JsWith(JsObj({ JsProp(loopEnv.envName, JsObj({})) }), body) })
            break
          }
        }

        // Combine everything into the full loop
        return { JsFor(
          vars,
          JsBinary(index, JsBinaryOp.LT, JsBinary(array, JsBinaryOp.MEMBER, JsText('length'))),
          JsUnary(index, JsUnaryOp.POST_INC),
          body, label) }
      }
      fail
    }
    else if node is ContinueStmt or node is BreakStmt {
      // Extract the statement type and loop count
      JsJumpKind kind
      int value
      JsNode? label = null
      if ContinueStmt node = node {
        kind = JsJumpKind.CONTINUE
        value = node.value
      } else if BreakStmt node = node {
        kind = JsJumpKind.BREAK
        value = node.value
      }

      // If the loop count isn't 1, generate a label for the target loop
      if value > 1 and value <= loops.size {
        int i = loops.size - value
        string text
        if string label = loops[i] { text = label }
        else {
          // Find an unused symbol name in the top-level scope and reserve it.
          // Even though JavaScript has a different label namespace, making
          // sure labels are named differently than variables avoids confusion.
          Scope scope = node.scope
          while scope.kind in JsRenamer.IGNORED_SCOPE_KINDS {
            if Scope parentScope = scope.parentScope { scope = parentScope }
            else { break }
          }
          text = findUnusedName(scope, 'loop')
          loops[i] = text
        }
        label = JsText(text)
      }
      return { JsJump(kind, label) }
    }
    else if FailStmt node = node {
      return { JsJump(JsJumpKind.THROW,
        JsCall(
          JsUnary(JsText('Error'), JsUnaryOp.NEW),
          { visit(node.value) })) }
    }
    else if ReturnStmt node = node {
      if Expr value = node.value { return { JsJump(JsJumpKind.RETURN, visit(value)) } }
      return { JsJump(JsJumpKind.RETURN, null) }
    }
    else if node is CommentStmt {
      return {}
    }

    // Definitions
    else if VarDef node = node {
      if node.scope.kind == ScopeKind.INSTANCE { return {} }
      if Symbol symbol = node.id.symbol {
        if Expr value = node.value {
          return makeVar(symbol, visit(value), node.scope)
        }
        return makeVar(symbol, null, node.scope)
      }
      else { fail }
    }
    else if FuncDef node = node {
      JsNode value = JsText('null')
      if Symbol symbol = node.id.symbol and FuncType funcType = symbol.type {
        if Block body = node.body { value = generateFunction(node.scope, funcType, node.args, body) }
        return makeVar(symbol, value, node.scope)
      }
      else { fail }
    }
    else if ClassDef node = node {
      if (node.modifiers & Modifier.EXTERN) != 0 {
        // Don't emit code for external classes, including all class members
        return {}
      }
      else if Symbol symbol = node.id.symbol and MetaType metaType = symbol.type and ClassType classType = metaType.instanceType {
        // If there's a base class, initialize the prototype using Object.create()
        JsNode[] nodes = {}
        if ClassType baseType = classType.baseType {
          nodes.push(JsBinary(
            constructPrefix(prefixFromScope(classType.staticScope).concat({ 'prototype' })),
            JsBinaryOp.ASSIGN,
            JsCall(
              JsBinary(JsText('Object'), JsBinaryOp.MEMBER, JsText('create')),
              { constructPrefix(prefixFromScope(baseType.instanceScope)) })))
        }

        // Look for the constructor and use that for the function body
        for stmt in node.body.stmts {
          if CtorDef ctor = stmt {
            // Add comments to classes without them
            if node.comment == null and ctor.comment == null {
              node.comment = ' class ' + node.id.name
            }

            // Pick out the constructor from the other statements
            if Symbol symbol = node.id.symbol { nodes = makeVar(symbol, visit(ctor), node.scope).concat(nodes) }
            if string comment = ctor.comment { nodes.unshift(JsComment(comment, false)) }
            return nodes.concat(visitStmts(node.body.stmts.filter(stmt -> stmt != ctor)))
          }
        }

        // Add comments to classes without them
        if node.comment == null { node.comment = ' class ' + node.id.name }

        // Class doesn't have a constructor so use an empty object instead
        if Symbol symbol = node.id.symbol { nodes = makeVar(symbol, JsObj({}), node.scope).concat(nodes) }
        return nodes.concat(visitStmts(node.body.stmts))
      }
      else { fail }
    }

    else { fail }
  }

  static JsNode applyDeMorgansTransform(JsNode node) {
    if JsText node = node {
      if node.text == 'true' {
        return JsText('false')
      }
      else if node.text == 'false' {
        return JsText('true')
      }
    }
    else if JsUnary node = node {
      if node.op == JsUnaryOp.NOT {
        return node.value
      }
    }
    else if JsBinary node = node {
      if node.op == JsBinaryOp.AND {
        return JsBinary(applyDeMorgansTransform(node.left), JsBinaryOp.OR, applyDeMorgansTransform(node.right))
      }
      else if node.op == JsBinaryOp.OR {
        return JsBinary(applyDeMorgansTransform(node.left), JsBinaryOp.AND, applyDeMorgansTransform(node.right))
      }
      else if node.op == JsBinaryOp.EQ {
        return JsBinary(node.left, JsBinaryOp.NEQ, node.right)
      }
      else if node.op == JsBinaryOp.NEQ {
        return JsBinary(node.left, JsBinaryOp.EQ, node.right)
      }
      else if node.op == JsBinaryOp.LT {
        return JsBinary(node.left, JsBinaryOp.GTE, node.right)
      }
      else if node.op == JsBinaryOp.GT {
        return JsBinary(node.left, JsBinaryOp.LTE, node.right)
      }
      else if node.op == JsBinaryOp.LTE {
        return JsBinary(node.left, JsBinaryOp.GT, node.right)
      }
      else if node.op == JsBinaryOp.GTE {
        return JsBinary(node.left, JsBinaryOp.LT, node.right)
      }
    }
    return JsUnary(node, JsUnaryOp.NOT)
  }

  JsNode[] declareVarsBeforeStmts(Scope scope, Stmt[] stmts, bool createThisAlias) {
    JsNode[] vars = {}
    for symbol in scope.symbols {
      if symbol.originalScope.kind == ScopeKind.MATCH {
        vars.push(JsVar(symbol.name, null))
      }
    }

    JsNode[] nodes
    if createThisAlias {
      // Generate an alias for "this" and add a variable for it if needed
      ThisAlias old = thisAlias
      thisAlias = ThisAlias(findUnusedName(scope, 'that'))
      nodes = visitStmts(stmts)
      if thisAlias.isUsed { vars.unshift(JsVar(thisAlias.name, JsText('this'))) }
      thisAlias = old
    }
    else { nodes = visitStmts(stmts) }

    if vars.size > 0 { nodes.unshift(JsVars(vars)) }
    return nodes
  }

  JsNode visit(Node node) {
    if Module node = node {
      loopCaptures.visit(node)
      return JsFile(declareVarsBeforeStmts(node.scope, node.body.stmts, false))
    }
    else if Block node = node {
      return JsBlock(visitStmts(node.stmts))
    }
    else if CtorDef node = node {
      if Symbol symbol = node.id.symbol and FuncType funcType = symbol.type and Block body = node.body {
        return generateFunction(node.scope, funcType, node.args, body)
      }
      return JsText('null')
    }

    // Expressions
    else if UnaryExpr node = node {
      for info in JsUnaryInfo.INFO {
        if info.op == node.op {
          JsNode value = visit(node.value)
          if info.jsOp == JsUnaryOp.NOT {
            return applyDeMorgansTransform(value)
          }
          return JsUnary(value, info.jsOp)
        }
      }
      fail
    }
    else if BinaryExpr node = node {
      if node.op == BinaryOp.IN {
        return JsBinary(JsCall(JsBinary(visit(node.right), JsBinaryOp.MEMBER,
          JsText('indexOf')), { visit(node.left) }), JsBinaryOp.GTE, JsText('0'))
      }
      // TODO: handle /= with temporary variable
      if node.op == BinaryOp.DIV and node.left.resolvedType == ClassType.INT and node.right.resolvedType == ClassType.INT {
        return JsBinary(JsBinary(visit(node.left), JsBinaryOp.DIV, visit(node.right)), JsBinaryOp.BITOR, JsText('0'))
      }
      for info in JsBinaryInfo.INFO {
        if info.op == node.op {
          return JsBinary(visit(node.left), info.jsOp, visit(node.right))
        }
      }
      fail
    }
    else if TertiaryExpr node = node {
      return JsTertiary(visit(node.value), visit(node.trueValue), visit(node.falseValue))
    }
    else if CallExpr node = node {
      if MemberExpr value = node.value and
          Symbol symbol = value.id.symbol and
          BaseExpr baseValue = value.value and
          ClassType baseType = baseValue.resolvedType {
        JsNode[] args = { JsText(thisAlias.resolveName()) }
        for arg in node.args { args.push(visit(arg)) }
        return JsCall(constructPrefix(prefixFromScope(baseType.instanceScope).concat({ symbol.name, 'call' })), args)
      }
      else if Symbol ctorSymbol = node.ctorSymbol {
        if JsNode native = JsNative.handleCall(ctorSymbol, node, node.args, x -> visit(x)) {
          return native
        }
        else if MetaType metaType = node.value.resolvedType and ClassType classType = metaType.instanceType {
          JsNode[] args = {}
          for arg in node.args { args.push(visit(arg)) }
          return JsCall(JsUnary(constructPrefix(prefixFromScope(classType.staticScope)), JsUnaryOp.NEW), args)
        }
        else if ClassType classType = node.value.resolvedType {
          JsNode[] args = { JsText(thisAlias.resolveName()) }
          for arg in node.args { args.push(visit(arg)) }
          return JsCall(JsBinary(constructPrefix(prefixFromScope(classType.staticScope)), JsBinaryOp.MEMBER, JsText('call')), args)
        }
        else { fail }
      }
      else if MemberExpr memberExpr = node.value and
          Symbol symbol = memberExpr.id.symbol and
          JsNode native = JsNative.handleCall(symbol, memberExpr.value, node.args, x -> visit(x)) {
        return native
      }
      else {
        JsNode[] args = {}
        for arg in node.args { args.push(visit(arg)) }
        if MetaType metaType = node.value.resolvedType {
          if metaType.instanceType is ArrayType {
            // TODO: Replace this special case with a splat constructor in native.bend
            return JsArray(args)
          }
          else if (metaType.instanceType is FuncType or metaType.instanceType == SpecialType.VAR) and args.size == 1 {
            return args[0]
          }
          else { fail }
        }
        return JsCall(visit(node.value), args)
      }
    }
    else if IndexExpr node = node {
      return JsBinary(visit(node.value), JsBinaryOp.INDEX, visit(node.index))
    }
    else if MemberExpr node = node {
      if (node.value.resolvedType == ClassType.STRING or node.value.resolvedType is ArrayType) and Symbol symbol = node.id.symbol and symbol.name == 'size' {
        return JsBinary(visit(node.value), JsBinaryOp.MEMBER, JsText('length'))
      }
      else if node.value.resolvedType == SpecialType.VAR {
        return JsBinary(visit(node.value), JsBinaryOp.MEMBER, JsText(node.id.name))
      }
      else if Symbol symbol = node.id.symbol {
        return JsBinary(visit(node.value), JsBinaryOp.MEMBER, JsText(symbol.name))
      }
      else { fail }
    }
    else if MatchExpr node = node {
      if NullableType nullableType = node.value.resolvedType and
          MetaType metaType = node.type.resolvedType and
          Type.equals(nullableType.innerType, metaType.instanceType) {
        return JsBinary(
          JsBinary(visit(node.id), JsBinaryOp.ASSIGN, visit(node.value)),
          JsBinaryOp.SNEQ,
          JsText('null'))
      }
      return JsBinary(
        JsBinary(visit(node.id), JsBinaryOp.ASSIGN, visit(node.value)),
        JsBinaryOp.INSTANCEOF,
        visit(node.type))
    }
    else if InitExpr node = node {
      if Symbol ctorSymbol = node.ctorSymbol {
        if JsNode native = JsNative.handleCall(ctorSymbol, node, node.values, x -> visit(x)) {
          return native
        }
        else if ClassType classType = node.resolvedType {
          JsNode[] values = {}
          for value in node.values { values.push(visit(value)) }
          return JsCall(JsUnary(constructPrefix(prefixFromScope(classType.staticScope)), JsUnaryOp.NEW), values)
        }
        else { fail }
      }
      else {
        JsNode[] nodes = {}
        for value in node.values { nodes.push(visit(value)) }
        return JsArray(nodes)
      }
    }
    else if LambdaExpr node = node {
      thisAlias.nestedFuncCount++
      JsNode[] args = {}
      for arg in node.args { if Symbol symbol = arg.symbol { args.push(JsText(symbol.name)) } }
      JsNode jsNode = JsFunc(null, args, JsBlock(declareVarsBeforeStmts(node.body.scope, node.body.stmts, false)))
      thisAlias.nestedFuncCount--
      return jsNode
    }

    // Literals
    else if TypeExpr node = node {
      if node.type == SpecialType.NULL {
        return JsText('null')
      }
      else if MetaType metaType = node.type and ClassType classType = metaType.instanceType {
        return constructPrefix(prefixFromScope(classType.staticScope))
      }
      else { return JsText('<' + node.type + '>') }
    }
    else if ThisExpr node = node {
      return JsText(thisAlias.resolveName())
    }
    else if IdentExpr node = node {
      if Symbol symbol = node.symbol {
        if string env = envForSymbol(symbol) {
          return JsBinary(JsText(env), JsBinaryOp.MEMBER, JsText(symbol.name))
        }
        if symbol.scope.kind == ScopeKind.INSTANCE {
          return JsBinary(JsText(thisAlias.resolveName()), JsBinaryOp.MEMBER, JsText(symbol.name))
        }
        return constructPrefix(prefixFromScope(symbol.scope).concat({ symbol.name }))
      }
      else { fail }
    }
    else if IntExpr node = node {
      return JsText(node.value + '')
    }
    else if BoolExpr node = node {
      return JsText(node.value + '')
    }
    else if FloatExpr node = node {
      return JsText(node.value + '')
    }
    else if StringExpr node = node {
      return JsText(node.value.quote("'"))
    }

    else { fail }
  }
}

class JsRenamer is Visitor {
  // From https://developer.mozilla.org/en/JavaScript/Reference/Reserved_Words
  // and https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects
  static string[] RESERVED_WORDS = {
    // Reserved words
    'break',
    'case',
    'catch',
    'continue',
    'debugger',
    'default',
    'delete',
    'do',
    'else',
    'finally',
    'for',
    'function',
    'if',
    'in',
    'instanceof',
    'new',
    'return',
    'switch',
    // 'this', // Constructors use this name
    'throw',
    'try',
    'typeof',
    'var',
    'void',
    'while',
    'with',

    // Words reserved for possible future use
    'class',
    'enum',
    'export',
    'extends',
    'import',
    'super',
    'implements',
    'interface',
    'let',
    'package',
    'private',
    'protected',
    'public',
    'static',
    'yield',
    'const',

    // Other
    'constructor',
    'prototype',
    'arguments',
    'null',
    'true',
    'false',
    '__proto__'
  }

  static ScopeKind[] IGNORED_SCOPE_KINDS = {
    ScopeKind.BLOCK
    ScopeKind.LOOP
    ScopeKind.MATCH
  }

  over void visitIdentExpr(IdentExpr node) {
    // Check if we've already modified this symbol
    if Symbol symbol = node.symbol {
      if symbol.scope != symbol.originalScope or symbol.name != symbol.originalName { return }

      // Remove the symbol from its scope
      Scope scope = symbol.scope
      scope.symbols.splice(scope.symbols.indexOf(symbol), 1)

      // Move the symbol definition up to the top level
      while scope.kind in IGNORED_SCOPE_KINDS {
        if Scope parentScope = scope.parentScope { scope = parentScope }
        else { break }
      }

      // Rename the symbol so it fits in that scope
      string originalName = symbol.originalName
      int count = 2
      symbol.name = originalName
      while symbol.name in RESERVED_WORDS or scope.find(symbol.name) != null {
        symbol.name = originalName + count++
      }

      // Insert the symbol in that scope
      scope.symbols.push(symbol)
      symbol.scope = scope
    }
  }
}

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
class JsLoopCaptures is Visitor {
  class SymbolInfo {
    Symbol symbol
    Scope loopScope
  }

  SymbolInfo[] symbolInfo = {}

  static ScopeKind[] ALLOWED_SCOPE_KINDS = {
    ScopeKind.BLOCK
    ScopeKind.LOOP
    ScopeKind.MATCH
  }

  over void visitFuncDef(FuncDef node) {
    // Don't visit the identifier for this function or it will "capture" itself
    for arg in node.args { visit(arg) }
    visit(node.body)
  }

  over void visitIdentExpr(IdentExpr node) {
    if Symbol symbol = node.symbol and
        (symbol.modifiers & Modifier.EXTERN) == 0 and
        symbol.originalScope.kind in ALLOWED_SCOPE_KINDS {
      // Don't process this symbol again (TODO: make more efficient)
      for info in symbolInfo {
        if info.symbol == symbol { return }
      }

      // Check if this identifier is captured by a function
      Scope scope = node.scope
      bool inFunc = false
      while scope != symbol.originalScope {
        if scope.kind == ScopeKind.FUNCTION {
          inFunc = true
        }

        // Advance to the next scope
        if Scope parentScope = scope.parentScope { scope = parentScope }
        else { break }
      }

      // Check if this symbol is defined inside a loop
      if inFunc {
        Scope scope = symbol.originalScope
        while scope.kind in ALLOWED_SCOPE_KINDS {
          if scope.kind == ScopeKind.LOOP {
            symbolInfo.push({ symbol, scope })
            break
          }

          // Advance to the next scope
          if Scope parentScope = scope.parentScope { scope = parentScope }
          else { break }
        }
      }
    }
  }
}

class JavaScript {
  static string convert(Node node) {
    JsRenamer().visit(node)
    return JsPrinter().visit(JsConverter().visit(node))
  }
}
