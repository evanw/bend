class Modifier {
  static int STATIC   = 1 << 0
  static int OVER     = 1 << 1
  static int ABSTRACT = 1 << 2
  static int EXTERN   = 1 << 3
  static int PROP     = 1 << 4
}

class SymbolKind {
  DUMMY
  CLASS
  VARIABLE
  FUNCTION
}

class Symbol {
  SymbolKind kind
  string name
  Type type
  Scope scope
  int modifiers
  Symbol? baseSymbol = null // The symbol that this overrides in the base class (requires "over")
  string originalName = name // The original name for mangling purposes (reserved words)
  Scope originalScope = scope // The original scope for codegen purposes (match expressions)

  string toString() { return '"' + name + '" of type ' + type }
}
