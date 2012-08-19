abstract class Type {
  string toString()

  static Type[] paramsFor(Type type) {
    if ArrayType type = type { return { type.elementType } }
    return {}
  }

  static bool isPrimitive(Type type) {
    return type in {
      ClassType.INT
      ClassType.BOOL
      ClassType.FLOAT
      ClassType.STRING
    }
  }

  static Type removeParams(Type type, Type[] params) {
    if TypeParam type = type {
      if type.index >= params.size { fail }
      return params[type.index]
    }
    if MetaType type = type {
      Type instanceType = removeParams(type.instanceType, params)
      if instanceType == type.instanceType { return type }
      return MetaType(instanceType)
    }
    if ArrayType type = type {
      Type elementType = removeParams(type.elementType, params)
      if elementType == type.elementType { return type }
      return ArrayType(elementType)
    }
    if FuncType type = type {
      Type returnType = removeParams(type.returnType, params)
      bool changed = returnType != type.returnType
      Type[] argTypes = {}
      for oldArgType in type.argTypes {
        Type newArgType = removeParams(oldArgType, params)
        argTypes.push(newArgType)
        if newArgType != oldArgType { changed = true }
      }
      if not changed { return type }
      FuncType funcType = { returnType, argTypes, type.minArgCount }
      funcType.argNames = type.argNames
      return funcType
    }
    return type
  }

  static bool equals(Type a, Type b) {
    if a == b or a == SpecialType.VAR or b == SpecialType.VAR { return true }
    if NullableType a = a and NullableType b = b {
      return equals(a.innerType, b.innerType)
    }
    if MetaType a = a and MetaType b = b {
      return equals(a.instanceType, b.instanceType)
    }
    if ArrayType a = a and ArrayType b = b {
      return equals(a.elementType, b.elementType)
    }
    if FuncType a = a and FuncType b = b {
      if not equals(a.returnType, b.returnType) or
          a.argTypes.size != b.argTypes.size or
          a.minArgCount != b.minArgCount {
        return false
      }
      int i = 0
      while i < a.argTypes.size {
        if not equals(a.argTypes[i], b.argTypes[i]) { return false }
        i++
      }
      return true
    }
    return false
  }

  static bool canImplicitlyConvert(Type from, Type to) {
    if equals(from, to) { return true }
    if NullableType to = to {
      if from == SpecialType.NULL { return true }
      if NullableType from = from {
        return canImplicitlyConvert(from.innerType, to.innerType)
      }
      return canImplicitlyConvert(from, to.innerType)
    }
    if ClassType from = from and ClassType to = to {
      while true {
        if equals(from, to) { return true }
        else if ClassType baseType = from.baseType { from = baseType }
        else { break }
      }
    }
    return false
  }
}

class TypeParam is Type {
  int index

  T0 = { 0 }

  over string toString() { return '<' + index + '>' }
}

class SpecialType is Type {
  string name

  ERROR = { '<error>' }
  NULL = { 'null' }
  VOID = { 'void' }
  VAR = { 'var' }

  // Don't use MetaType for void because we want to restrict it to return types
  TYPE_VOID = { 'type void' }

  over string toString() { return name }
}

class NullableType is Type {
  Type innerType

  over string toString() { return innerType + '?' }
}

class MetaType is Type {
  Type instanceType

  VAR = { SpecialType.VAR }

  over string toString() { return 'type ' + instanceType }
}

class ArrayType is Type {
  Type elementType

  over string toString() { return elementType + '[]' }
}

class FuncType is Type {
  Type returnType
  Type[] argTypes

  // If all arguments are required, minArgCount == argTypes.size
  int minArgCount

  // A splat can take zero or more arguments, -1 means no splat
  int splatIndex = -1

  // Argument names are currently used for automatically generated constructors,
  // but may also be used for keyword arguments in the future. Must be the same
  // length as argTypes if non-null.
  string[]? argNames = null

  over string toString() {
    string[] args = {}
    int i = 0
    while i < argTypes.size {
      string arg = argTypes[i].toString()
      if i > minArgCount { arg = '?' + arg }
      if i == splatIndex { arg += '...' }
      args.push(arg)
      i++
    }
    return returnType + ' fn(' + args.join(', ') + ')'
  }
}

class ClassType is Type {
  string name
  Scope staticScope
  Scope instanceScope
  bool isAbstract
  ClassType? baseType = null
  MetaType metaType = { this }

  over string toString() { return name }

  static ClassType makeNative(string name) {
    Scope staticScope = { ScopeKind.STATIC, null, null }
    Scope instanceScope = { ScopeKind.INSTANCE, staticScope, null }
    ClassType classType = { name, staticScope, instanceScope, false }
    FuncType funcType = { SpecialType.VOID, { classType }, 1 }
    Symbol symbol = { SymbolKind.FUNCTION, 'this', funcType, instanceScope, 0 }
    staticScope.symbol = symbol
    instanceScope.symbol = symbol
    instanceScope.symbols.push(symbol)
    return classType
  }

  INT = makeNative('int')
  BOOL = makeNative('bool')
  FLOAT = makeNative('float')
  STRING = makeNative('string')
}

ClassType.INT.baseType = ClassType.FLOAT
if Symbol ctorSymbol = ClassType.INT.instanceScope.find('this') and
    FuncType funcType = ctorSymbol.type {
  funcType.argTypes[0] = ClassType.FLOAT
}
