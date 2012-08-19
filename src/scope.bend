class ScopeKind {
  ERROR
  MODULE
  STATIC
  INSTANCE
  FUNCTION
  BLOCK
  MATCH
  LOOP
}

class Scope {
  ScopeKind kind
  Scope? parentScope
  Scope? baseScope
  Symbol? symbol = null // The associated symbol (the class or the loop variable, for example)
  Symbol[] symbols = {}

  ERROR = { ScopeKind.ERROR, null, null }

  Symbol define(Log log, Location location, SymbolKind kind, string name, Type type, int modifiers) {
    // Classes actually have two scopes (static and instance) but we want unique identifiers between them
    if find(name) != null or (Symbol symbol = this.symbol and MetaType metaType = symbol.type and ClassType type = metaType.instanceType and (
        (this.kind == ScopeKind.INSTANCE and type.staticScope.find(name) != null) or
        (this.kind == ScopeKind.STATIC and type.instanceScope.find(name) != null))) {
      log.error(location, '"' + name + '" is already defined in this scope')
    }
    Symbol symbol = { kind, name, type, this, modifiers }
    symbols.push(symbol)
    return symbol
  }

  Symbol? find(string name) {
    for symbol in symbols {
      if symbol.name == name { return symbol }
    }
    return null
  }

  Symbol? lookup(string name, bool isMember) {
    // Search this scope
    Symbol? symbol = find(name)
    if symbol != null { return symbol }

    // Search the base scope
    Scope? nextScope = baseScope
    while Scope scope = nextScope {
      symbol = scope.find(name)
      if symbol != null { return symbol }
      nextScope = scope.baseScope
    }

    // Search the parent scope
    if (kind != ScopeKind.STATIC or not isMember) and Scope parentScope = parentScope {
      return parentScope.lookup(name, isMember)
    }
    return null
  }
}
