abstract class SpecialScope {
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
    ClassType INT = ClassType.INT
    ClassType BOOL = ClassType.BOOL
    ClassType FLOAT = ClassType.FLOAT
    ClassType STRING = ClassType.STRING
    ClassType ARRAY = ClassType.ARRAY
    Type T = ARRAY.params[0]
    Type VAR = SpecialType.VAR
    Type VOID = SpecialType.VOID
    Type ARRAY_T = SpecificType(ARRAY, { T })
    Type ARRAY_VAR = SpecificType(ARRAY, { VAR })
    Type ARRAY_STRING = SpecificType(ARRAY, { STRING })

    func(INT.instanceScope, 'toString', { STRING, {}, 0 })
    func(BOOL.instanceScope, 'toString', { STRING, {}, 0 })
    func(FLOAT.instanceScope, 'toString', { STRING, {}, 0 })
    func(STRING.instanceScope, 'toString', { STRING, {}, 0 })

    func(INT.instanceScope, 'toHex', { STRING, {}, 0 })

    field(STRING.instanceScope, 'size', INT)
    func(STRING.staticScope, 'fromCharCode', { STRING, { INT }, 1 })
    func(STRING.instanceScope, 'quote', { STRING, { STRING }, 1 })
    func(STRING.instanceScope, 'slice', { STRING, { INT, INT }, 1 })
    func(STRING.instanceScope, 'toInt', { INT, {}, 0 })
    func(STRING.instanceScope, 'toFloat', { FLOAT, {}, 0 })
    func(STRING.instanceScope, 'substr', { STRING, { INT, INT }, 1 })
    func(STRING.instanceScope, 'split', { ARRAY_STRING, { STRING }, 1 })
    func(STRING.instanceScope, 'replace', { STRING, { STRING, STRING }, 2 })
    func(STRING.instanceScope, 'replacePattern', { STRING, { STRING, STRING }, 2 })
    func(STRING.instanceScope, 'toLowerCase', { STRING, {}, 0 })
    func(STRING.instanceScope, 'toUpperCase', { STRING, {}, 0 })
    func(STRING.instanceScope, 'indexOf', { INT, { STRING }, 1 })
    func(STRING.instanceScope, 'charCodeAt', { INT, { INT }, 1 })

    field(ARRAY.instanceScope, 'size', INT)
    func(ARRAY.instanceScope, 'push', { VOID, { T }, 1 })
    func(ARRAY.instanceScope, 'pop', { T, {}, 0 })
    func(ARRAY.instanceScope, 'unshift', { VOID, { T }, 1 })
    func(ARRAY.instanceScope, 'shift', { T, {}, 0 })
    func(ARRAY.instanceScope, 'splice', { ARRAY_T, { INT, INT, ARRAY_T }, 2 })
    func(ARRAY.instanceScope, 'join', { STRING, { STRING }, 1 })
    func(ARRAY.instanceScope, 'filter', { ARRAY_T, { FuncType(BOOL, { T }, 1) }, 1 })
    func(ARRAY.instanceScope, 'slice', { ARRAY_T, { INT, INT }, 0 })
    func(ARRAY.instanceScope, 'map', { ARRAY_VAR, { FuncType(VAR, { T }, 1) }, 1 })
    func(ARRAY.instanceScope, 'concat', { ARRAY_T, { ARRAY_T }, 1 })
    func(ARRAY.instanceScope, 'indexOf', { INT, { T }, 1 })
    func(ARRAY.instanceScope, 'reverse', { ARRAY_T, {}, 0 })

    if Symbol symbol = INT.instanceScope.find('this') and FuncType funcType = symbol.type {
      funcType.argTypes[0] = FLOAT
    }

    if Symbol symbol = ARRAY.instanceScope.find('splice') and FuncType funcType = symbol.type {
      funcType.splatIndex = 2
    }
  }
}

class JsNative {
  class Handler {
    Symbol symbol
    JsNode fn(JsNode fn(Expr), Expr, Expr[]) callback
  }

  static Handler[] handlers = {}

  static void register(Symbol symbol, JsNode fn(JsNode fn(Expr), Expr, Expr[]) callback) {
    handlers.push({ symbol, callback })
  }

  static JsNode? handleCall(Symbol symbol, Expr target, Expr[] args, JsNode fn(Expr) visit) {
    for handler in handlers {
      if handler.symbol == symbol {
        return handler.callback(visit, target, args)
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
    while i < text.size {
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
        if word.size == 1 { word = '\\x0' + word }
        else if word.size == 2 { word = '\\x' + word }
        else if word.size == 3 { word = '\\u0' + word }
        else if word.size == 4 { word = '\\u' + word }
        else { fail }
        text = text.slice(0, i - 1) + word + text.slice(i)
        i += word.size - 1
      }
    }
    return text
  }

  static void init() {
    register(get(ClassType.INT.instanceScope, 'this'), (visit, target, args) ->
      JsBinary(visit(args[0]), JsBinaryOp.BITOR, JsText('0')))

    register(get(ClassType.BOOL.instanceScope, 'this'), (visit, target, args) ->
      visit(args[0]))

    register(get(ClassType.FLOAT.instanceScope, 'this'), (visit, target, args) ->
      visit(args[0]))

    register(get(ClassType.STRING.instanceScope, 'this'), (visit, target, args) ->
      visit(args[0]))

    register(get(ClassType.ARRAY.instanceScope, 'this'), (visit, target, args) ->
      JsArray(args.map(visit)))

    register(get(ClassType.INT.instanceScope, 'toHex'), (visit, target, args) ->
      JsCall(JsBinary(visit(target), JsBinaryOp.MEMBER, JsText('toString')), { JsText('16') }))

    register(get(ClassType.STRING.instanceScope, 'quote'), (visit, target, args) ->
      JsCall(JsBinary(JsText('JSON'), JsBinaryOp.MEMBER, JsText('stringify')), { visit(target) }))

    register(get(ClassType.STRING.instanceScope, 'toInt'), (visit, target, args) ->
      JsBinary(visit(target), JsBinaryOp.BITOR, JsText('0')))

    register(get(ClassType.STRING.instanceScope, 'toFloat'), (visit, target, args) ->
      JsUnary(visit(target), JsUnaryOp.POS))

    register(get(ClassType.STRING.instanceScope, 'replace'), (visit, target, args) -> {
      string ESCAPE_REGEX = '([\\.\\?\\*\\+\\^\\$\\[\\]\\/\\\\\\(\\)\\{\\}\\|\\-])'
      if StringExpr arg0 = args[0] {
        return JsCall(JsBinary(visit(target), JsBinaryOp.MEMBER, JsText('replace')), {
          JsText('/' + escapeNonASCII(arg0.value.replacePattern(ESCAPE_REGEX, '\\$1')) + '/g'), visit(args[1]) })
      }
      return JsCall(JsBinary(visit(target), JsBinaryOp.MEMBER, JsText('replace')), {
        JsCall(JsUnary(JsText('RegExp'), JsUnaryOp.NEW), { JsCall(JsBinary(visit(args[0]), JsBinaryOp.MEMBER, JsText('replace')),
          { JsText('/' + ESCAPE_REGEX + '/g'), JsText('"\\\\$1"') }), JsText('"g"') }), visit(args[1]) })
    })

    register(get(ClassType.STRING.instanceScope, 'replacePattern'), (visit, target, args) -> {
      if StringExpr arg0 = args[0] {
        return JsCall(JsBinary(visit(target), JsBinaryOp.MEMBER, JsText('replace')), {
          JsText('/' + escapeNonASCII(arg0.value.replace('/', '\\/')) + '/g'), visit(args[1]) })
      }
      return JsCall(JsBinary(visit(target), JsBinaryOp.MEMBER, JsText('replace')), {
        JsCall(JsUnary(JsText('RegExp'), JsUnaryOp.NEW), { visit(args[0]), JsText('"g"') }), visit(args[1]) })
    })

    register(get(ClassType.STRING.staticScope, 'fromCharCode'), (visit, target, args) ->
      JsCall(JsBinary(JsText('String'), JsBinaryOp.MEMBER, JsText('fromCharCode')), { visit(args[0]) }))
  }
}

SpecialScope.init()
JsNative.init()
