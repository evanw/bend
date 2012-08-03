abstract class Type {
  string toString()

  static bool isArray(Type type) {
    return SpecificType type = type and type.classType == ClassType.ARRAY
  }

  // A type is generic if any contained class type isn't specialized:
  // 
  //   class A<T> {}
  //   class B<T> {}
  // 
  //   Generic types: A, A<B>
  //   Specific types: A<int>, A<B<int>>
  // 
  static bool isGeneric(Type type) {
    if MetaType type = type { return isGeneric(type.instanceType) }
    if NullableType type = type { return isGeneric(type.innerType) }
    if FuncType type = type {
      if isGeneric(type.returnType) { return true }
      for argType in type.argTypes { if isGeneric(argType) { return true } }
      return false
    }
    if ClassType type = type { return type.params.size > 0 }
    if SpecificType type = type {
      for paramType in type.paramTypes { if isGeneric(paramType) { return true } }
      return false
    }
    return false
  }

  // Substitute each TypeParam for the corresponding type in params. Will fail
  // if a TypeParam has an index outside the bounds of params.
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
      funcType.splatIndex = type.splatIndex
      funcType.argNames = type.argNames
      return funcType
    }
    if ClassType type = type and type.params.size > 0 {
      Type[] paramTypes = {}
      for oldParamType in type.params {
        Type newParamType = removeParams(oldParamType, params)
        if newParamType is TypeParam { return type }
        paramTypes.push(newParamType)
      }
      return SpecificType(type, paramTypes)
    }
    if SpecificType type = type {
      bool changed = false
      Type[] paramTypes = {}
      for oldParamType in type.paramTypes {
        Type newParamType = removeParams(oldParamType, params)
        paramTypes.push(newParamType)
        if newParamType != oldParamType { changed = true }
      }
      if not changed { return type }
      return SpecificType(type.classType, paramTypes)
    }
    return type
  }

  // Compare types for equality. This is not exact equality since
  // SpecialType.VAR is a wildcard and will match any type. For
  // example, Foo<int[]> and Foo<var[]> are equal.
  static bool equals(Type a, Type b) {
    if a == b or a == SpecialType.VAR or b == SpecialType.VAR { return true }
    if NullableType a = a and NullableType b = b {
      return equals(a.innerType, b.innerType)
    }
    if MetaType a = a and MetaType b = b {
      return equals(a.instanceType, b.instanceType)
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
    if SpecificType a = a and SpecificType b = b {
      if not equals(a.classType, b.classType) or
          a.paramTypes.size != b.paramTypes.size {
        return false
      }
      int i = 0
      while i < a.paramTypes.size {
        if not equals(a.paramTypes[i], b.paramTypes[i]) { return false }
        i++
      }
      return true
    }
    return false
  }

  static bool canImplicitlyConvert(Type from, Type to) {
    if equals(from, to) { return true }
    if from == ClassType.INT and to == ClassType.FLOAT { return true }
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
  string name
  int index
  MetaType metaType = { this }

  over string toString() { return name }
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
  TypeParam[] params
  ClassType? baseType = null
  MetaType metaType = { this }

  over string toString() { return name }

  static ClassType makeNative(string name, bool isArray) {
    Scope staticScope = { ScopeKind.STATIC, null, null }
    Scope instanceScope = { ScopeKind.INSTANCE, staticScope, null }
    ClassType classType = { name, staticScope, instanceScope, false, {} }
    FuncType funcType = { SpecialType.VOID, { classType }, 1 }
    Symbol symbol = { SymbolKind.FUNCTION, 'this', funcType, instanceScope, 0 }
    staticScope.symbol = symbol
    instanceScope.symbol = symbol
    instanceScope.symbols.push(symbol)
    if isArray {
      funcType.splatIndex = 0
      funcType.minArgCount--
      classType.params.push({ 'T', 0 })
    }
    return classType
  }

  INT = makeNative('int', false)
  BOOL = makeNative('bool', false)
  FLOAT = makeNative('float', false)
  STRING = makeNative('string', false)
  ARRAY = makeNative('array', true)
}

class SpecificType is Type {
  ClassType classType
  Type[] paramTypes

  over string toString() {
    if classType == ClassType.ARRAY and paramTypes.size == 1 {
      return paramTypes[0] + '[]'
    }
    return classType.name + '<' + paramTypes.join(', ') + '>'
  }
}
