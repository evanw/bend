abstract class SpecialScope {
  static Scope ARRAY_STATIC = { ScopeKind.STATIC, null, null }
  static Scope ARRAY_INSTANCE = { ScopeKind.INSTANCE, ARRAY_STATIC, null }

  // Shortcut for defining a field
  static Symbol field(Scope scope, string name, Type type) {
    Symbol symbol = { SymbolKind.VARIABLE, name, type, scope, 0 }
    scope.symbols.push(symbol)
    return symbol
  }

  // Shortcut for defining a function
  static Symbol func(Scope scope, string name, FuncType funcType) {
    Symbol symbol = { SymbolKind.FUNCTION, name, funcType, scope, 0 }
    scope.symbols.push(symbol)
    return symbol
  }

  static void init() {
    Type T0 = TypeParam.T0
    Type VAR = SpecialType.VAR
    Type VOID = SpecialType.VOID
    Type ARRAY = ArrayType(T0)
    ClassType INT = ClassType.INT
    ClassType BOOL = ClassType.BOOL
    ClassType FLOAT = ClassType.FLOAT
    ClassType STRING = ClassType.STRING

    func(INT.instanceScope, 'toString', { STRING, {}, 0 })
    func(BOOL.instanceScope, 'toString', { STRING, {}, 0 })
    func(FLOAT.instanceScope, 'toString', { STRING, {}, 0 })
    func(STRING.instanceScope, 'toString', { STRING, {}, 0 })

    func(INT.instanceScope, 'toHex', { STRING, {}, 0 })

    field(STRING.instanceScope, 'length', INT)
    func(STRING.staticScope, 'fromCharCode', { STRING, { INT }, 1 })
    func(STRING.instanceScope, 'quote', { STRING, { STRING }, 1 })
    func(STRING.instanceScope, 'slice', { STRING, { INT, INT }, 1 })
    func(STRING.instanceScope, 'toInt', { INT, {}, 0 })
    func(STRING.instanceScope, 'toFloat', { FLOAT, {}, 0 })
    func(STRING.instanceScope, 'substr', { STRING, { INT, INT }, 1 })
    func(STRING.instanceScope, 'split', { ArrayType(STRING), { STRING }, 1 })
    func(STRING.instanceScope, 'replace', { STRING, { STRING, STRING }, 2 })
    func(STRING.instanceScope, 'replacePattern', { STRING, { STRING, STRING }, 2 })
    func(STRING.instanceScope, 'toLowerCase', { STRING, {}, 0 })
    func(STRING.instanceScope, 'toUpperCase', { STRING, {}, 0 })
    func(STRING.instanceScope, 'indexOf', { INT, { STRING }, 1 })
    func(STRING.instanceScope, 'charCodeAt', { INT, { INT }, 1 })

    field(ARRAY_INSTANCE, 'length', INT)
    func(ARRAY_INSTANCE, 'push', { VOID, { T0 }, 1 })
    func(ARRAY_INSTANCE, 'pop', { T0, {}, 0 })
    func(ARRAY_INSTANCE, 'unshift', { VOID, { T0 }, 1 })
    func(ARRAY_INSTANCE, 'shift', { T0, {}, 0 })
    func(ARRAY_INSTANCE, 'splice', { ARRAY, { INT, INT }, 2 })
    func(ARRAY_INSTANCE, 'join', { STRING, { STRING }, 1 })
    func(ARRAY_INSTANCE, 'filter', { ARRAY, { FuncType(BOOL, { T0 }, 1) }, 1 })
    func(ARRAY_INSTANCE, 'slice', { ARRAY, { INT, INT }, 0 })
    func(ARRAY_INSTANCE, 'map', { ArrayType(VAR), { FuncType(VAR, { T0 }, 1) }, 1 })
    func(ARRAY_INSTANCE, 'concat', { ARRAY, { ARRAY }, 1 })
    func(ARRAY_INSTANCE, 'indexOf', { INT, { T0 }, 1 })
    func(ARRAY_INSTANCE, 'reverse', { ARRAY, {}, 0 })
  }
}

class JsNative {
  class Handler0 {
    Symbol symbol
    JsNode fn(JsNode fn(Expr), Expr) callback
  }

  class Handler1 {
    Symbol symbol
    JsNode fn(JsNode fn(Expr), Expr, Expr) callback
  }

  class Handler2 {
    Symbol symbol
    JsNode fn(JsNode fn(Expr), Expr, Expr, Expr) callback
  }

  static Handler0[] handlers0 = {}
  static Handler1[] handlers1 = {}
  static Handler2[] handlers2 = {}

  static void register0(Symbol symbol, JsNode fn(JsNode fn(Expr), Expr) callback) { handlers0.push({ symbol, callback }) }
  static void register1(Symbol symbol, JsNode fn(JsNode fn(Expr), Expr, Expr) callback) { handlers1.push({ symbol, callback }) }
  static void register2(Symbol symbol, JsNode fn(JsNode fn(Expr), Expr, Expr, Expr) callback) { handlers2.push({ symbol, callback }) }

  static JsNode? handleCall(Symbol symbol, Expr target, Expr[] args, JsNode fn(Expr) visit) {
    if args.length == 0 {
      for handler in handlers0 {
        if handler.symbol == symbol { return handler.callback(visit, target) }
      }
    }
    else if args.length == 1 {
      for handler in handlers1 {
        if handler.symbol == symbol { return handler.callback(visit, target, args[0]) }
      }
    }
    else if args.length == 2 {
      for handler in handlers2 {
        if handler.symbol == symbol { return handler.callback(visit, target, args[0], args[1]) }
      }
    }
    return null
  }

  static Symbol get(Scope scope, string name) {
    if Symbol symbol = scope.find(name) { return symbol }
    fail
  }

  static string escapeNonASCII(string text) {
    int i = 0
    while i < text.length {
      int code = text.charCodeAt(i++)
      if code == 9 {
        text = text.slice(0, i - 1) + '\\t' + text.slice(i)
        i++
      }
      else if code == 10 {
        text = text.slice(0, i - 1) + '\\n' + text.slice(i)
        i++
      }
      else if code == 13 {
        text = text.slice(0, i - 1) + '\\r' + text.slice(i)
        i++
      }
      else if code < 32 or code > 126 {
        string word = code.toHex()
        if word.length == 1 { word = '\\x0' + word }
        else if word.length == 2 { word = '\\x' + word }
        else if word.length == 3 { word = '\\u0' + word }
        else if word.length == 4 { word = '\\u' + word }
        else { fail }
        text = text.slice(0, i - 1) + word + text.slice(i)
        i += word.length - 1
      }
    }
    return text
  }

  static void init() {
    register1(get(ClassType.INT.instanceScope, 'this'), (visit, target, arg0) ->
      JsBinary(visit(arg0), JsBinaryOp.BITOR, JsText('0')))

    register1(get(ClassType.BOOL.instanceScope, 'this'), (visit, target, arg0) ->
      visit(arg0))

    register1(get(ClassType.FLOAT.instanceScope, 'this'), (visit, target, arg0) ->
      visit(arg0))

    register1(get(ClassType.STRING.instanceScope, 'this'), (visit, target, arg0) ->
      visit(arg0))

    register0(get(ClassType.INT.instanceScope, 'toHex'), (visit, target) ->
      JsCall(JsBinary(visit(target), JsBinaryOp.MEMBER, JsText('toString')), { JsText('16') }))

    register1(get(ClassType.STRING.instanceScope, 'quote'), (visit, target, arg0) ->
      JsCall(JsBinary(JsText('JSON'), JsBinaryOp.MEMBER, JsText('stringify')), { visit(target) }))

    register0(get(ClassType.STRING.instanceScope, 'toInt'), (visit, target) ->
      JsBinary(visit(target), JsBinaryOp.BITOR, JsText('0')))

    register0(get(ClassType.STRING.instanceScope, 'toFloat'), (visit, target) ->
      JsUnary(visit(target), JsUnaryOp.POS))

    register2(get(ClassType.STRING.instanceScope, 'replace'), (visit, target, arg0, arg1) -> {
      string ESCAPE_REGEX = '([\\.\\?\\*\\+\\^\\$\\[\\]\\/\\\\\\(\\)\\{\\}\\|\\-])'
      if StringExpr arg0 = arg0 {
        return JsCall(JsBinary(visit(target), JsBinaryOp.MEMBER, JsText('replace')), {
          JsText('/' + escapeNonASCII(arg0.value.replacePattern(ESCAPE_REGEX, '\\$1')) + '/g'), visit(arg1) })
      }
      return JsCall(JsBinary(visit(target), JsBinaryOp.MEMBER, JsText('replace')), {
        JsCall(JsUnary(JsText('RegExp'), JsUnaryOp.NEW), { JsCall(JsBinary(visit(arg0), JsBinaryOp.MEMBER, JsText('replace')),
          { JsText('/' + ESCAPE_REGEX + '/g'), JsText('"\\\\$1"') }), JsText('"g"') }), visit(arg1) })
    })

    register2(get(ClassType.STRING.instanceScope, 'replacePattern'), (visit, target, arg0, arg1) -> {
      if StringExpr arg0 = arg0 {
        return JsCall(JsBinary(visit(target), JsBinaryOp.MEMBER, JsText('replace')), {
          JsText('/' + escapeNonASCII(arg0.value.replace('/', '\\/')) + '/g'), visit(arg1) })
      }
      return JsCall(JsBinary(visit(target), JsBinaryOp.MEMBER, JsText('replace')), {
        JsCall(JsUnary(JsText('RegExp'), JsUnaryOp.NEW), { visit(arg0), JsText('"g"') }), visit(arg1) })
    })

    register1(get(ClassType.STRING.staticScope, 'fromCharCode'), (visit, target, arg0) ->
      JsCall(JsBinary(JsText('String'), JsBinaryOp.MEMBER, JsText('fromCharCode')), { visit(arg0) }))
  }
}

SpecialScope.init()
JsNative.init()
