class Pass is Visitor {
  Log log

  Type instanceType(Expr node, bool allowVoid) {
    if node.resolvedType == SpecialType.TYPE_VOID {
      if allowVoid { return SpecialType.VOID }
      log.error(node.location, 'type void is not allowed here')
      return SpecialType.ERROR
    }
    if MetaType metaType = node.resolvedType { return metaType.instanceType }
    log.error(node.location, node.resolvedType + ' is not a type description')
    return SpecialType.ERROR
  }
}

class CreateScopesPass is Pass {
  Scope scope = { ScopeKind.MODULE, null, null }

  over void visitModule(Module node) {
    node.scope = scope
    base.visitModule(node)
  }

  over void visitBlock(Block node) {
    node.scope = scope
    scope = { ScopeKind.BLOCK, scope, null }
    base.visitBlock(node)
    scope = node.scope
  }

  over void visitExprStmt(ExprStmt node) {
    node.scope = scope
    base.visitExprStmt(node)
  }

  over void visitIfStmt(IfStmt node) {
    node.scope = scope
    // Use the scope of the expression for the true branch in case it contains
    // one or more match expressions
    visit(node.value)
    scope = node.value.scope
    visit(node.trueBody)
    scope = node.scope
    visit(node.falseBody)
  }

  over void visitWhileStmt(WhileStmt node) {
    node.scope = scope
    // Use the scope of the expression for the loop body in case it contains
    // one or more match expressions
    visit(node.value)
    scope = node.value.scope
    scope = { ScopeKind.LOOP, scope, null }
    visit(node.body)
    scope = node.scope
  }

  over void visitForStmt(ForStmt node) {
    node.scope = scope
    visit(node.value)
    scope = { ScopeKind.LOOP, scope, null }
    scope.define(log, node.location, SymbolKind.VARIABLE, node.id.name, SpecialType.ERROR, 0)
    visit(node.id)
    visit(node.body)
    scope = node.scope
  }

  over void visitContinueStmt(ContinueStmt node) {
    node.scope = scope
    base.visitContinueStmt(node)
  }

  over void visitBreakStmt(BreakStmt node) {
    node.scope = scope
    base.visitBreakStmt(node)
  }

  over void visitFailStmt(FailStmt node) {
    node.scope = scope
    base.visitFailStmt(node)
  }

  over void visitReturnStmt(ReturnStmt node) {
    node.scope = scope
    base.visitReturnStmt(node)
  }

  over void visitCommentStmt(CommentStmt node) {
    node.scope = scope
    base.visitCommentStmt(node)
  }

  over void visitVarDef(VarDef node) {
    node.scope = scope
    node.id.symbol = scope.define(log, node.location, SymbolKind.VARIABLE, node.id.name, SpecialType.ERROR, node.modifiers)
    base.visitVarDef(node)
  }

  over void visitFuncDef(FuncDef node) {
    node.scope = scope
    node.id.symbol = scope.define(log, node.location, SymbolKind.FUNCTION, node.id.name, SpecialType.ERROR, node.modifiers)
    scope = { ScopeKind.FUNCTION, scope, null }
    scope.symbol = node.id.symbol
    base.visitFuncDef(node)
    scope = node.scope
  }

  over void visitCtorDef(CtorDef node) {
    node.scope = scope
    node.id.symbol = scope.define(log, node.location, SymbolKind.FUNCTION, node.id.name, SpecialType.ERROR, node.modifiers)
    scope = { ScopeKind.FUNCTION, scope, null }
    scope.symbol = node.id.symbol
    base.visitCtorDef(node)
    scope = node.scope
  }

  over void visitClassDef(ClassDef node) {
    node.scope = scope
    Scope staticScope = { ScopeKind.STATIC, scope, null }
    Scope instanceScope = { ScopeKind.INSTANCE, staticScope, null }
    ClassType classType = { node.id.name, staticScope, instanceScope, (node.modifiers & Modifier.ABSTRACT) != 0, node.params }
    node.id.symbol = scope.define(log, node.location, SymbolKind.CLASS, node.id.name, classType.metaType, node.modifiers)
    staticScope.symbol = node.id.symbol
    instanceScope.symbol = node.id.symbol
    visit(node.id)
    visit(node.baseType)
    for param in node.params {
      staticScope.define(log, node.location, SymbolKind.PARAM, param.name, param.metaType, Modifier.STATIC)
    }
    for stmt in node.body.stmts {
      if VarDef def = stmt {
        if (def.modifiers & Modifier.STATIC) != 0 { scope = staticScope }
        else { scope = instanceScope }
      }
      else if FuncDef def = stmt {
        if (def.modifiers & Modifier.STATIC) != 0 { scope = staticScope }
        else { scope = instanceScope }
      }
      else if stmt is CtorDef { scope = instanceScope }
      else if stmt is ClassDef { scope = staticScope }
      else { scope = node.scope }
      visit(stmt)
    }
    scope = node.scope
  }

  over void visitUnaryExpr(UnaryExpr node) {
    node.scope = scope
    base.visitUnaryExpr(node)
  }

  over void visitBinaryExpr(BinaryExpr node) {
    node.scope = scope
    if node.op == BinaryOp.AND {
      // For short-circuit and expressions, wrap the right expression in the
      // left expression's scope since new variables from match expressions
      // in the left expression should propagate into the right expression.
      // Also set this node's scope to the scope of the right expression
      // because we will need to propagate it into the true branch of if
      // statements.
      Scope parentScope = scope
      visit(node.left)
      scope = node.left.scope
      visit(node.right)
      node.scope = node.right.scope
      scope = parentScope
    }
    else { base.visitBinaryExpr(node) }
  }

  over void visitTertiaryExpr(TertiaryExpr node) {
    node.scope = scope
    // Use the scope of the expression for the true branch in case it contains
    // one or more match expressions
    visit(node.value)
    scope = node.value.scope
    visit(node.trueValue)
    scope = node.scope
    visit(node.falseValue)
  }

  over void visitCallExpr(CallExpr node) {
    node.scope = scope
    base.visitCallExpr(node)
  }

  over void visitIndexExpr(IndexExpr node) {
    node.scope = scope
    base.visitIndexExpr(node)
  }

  over void visitMemberExpr(MemberExpr node) {
    node.scope = scope
    base.visitMemberExpr(node)
  }

  over void visitInitExpr(InitExpr node) {
    node.scope = scope
    base.visitInitExpr(node)
  }

  over void visitMatchExpr(MatchExpr node) {
    node.scope = { ScopeKind.MATCH, scope, null }
    node.scope.define(log, node.location, SymbolKind.VARIABLE, node.id.name, SpecialType.ERROR, 0)
    base.visitMatchExpr(node)
    node.id.scope = node.scope
  }

  over void visitLambdaExpr(LambdaExpr node) {
    node.scope = scope
    scope = { ScopeKind.FUNCTION, scope, null }
    for arg in node.args { scope.define(log, node.location, SymbolKind.VARIABLE, arg.name, SpecialType.ERROR, 0) }
    base.visitLambdaExpr(node)
    scope = node.scope
  }

  over void visitArrayTypeExpr(ArrayTypeExpr node) {
    node.scope = scope
    base.visitArrayTypeExpr(node)
  }

  over void visitFuncTypeExpr(FuncTypeExpr node) {
    node.scope = scope
    base.visitFuncTypeExpr(node)
  }

  over void visitNullableTypeExpr(NullableTypeExpr node) {
    node.scope = scope
    base.visitNullableTypeExpr(node)
  }

  over void visitSpecificTypeExpr(SpecificTypeExpr node) {
    node.scope = scope
    base.visitSpecificTypeExpr(node)
  }

  over void visitTypeExpr(TypeExpr node) {
    node.scope = scope
    base.visitTypeExpr(node)
  }

  over void visitThisExpr(ThisExpr node) {
    node.scope = scope
    base.visitThisExpr(node)
  }

  over void visitBaseExpr(BaseExpr node) {
    node.scope = scope
    base.visitBaseExpr(node)
  }

  over void visitIdentExpr(IdentExpr node) {
    node.scope = scope
    base.visitIdentExpr(node)
  }

  over void visitIntExpr(IntExpr node) {
    node.scope = scope
    base.visitIntExpr(node)
  }

  over void visitBoolExpr(BoolExpr node) {
    node.scope = scope
    base.visitBoolExpr(node)
  }

  over void visitFloatExpr(FloatExpr node) {
    node.scope = scope
    base.visitFloatExpr(node)
  }

  over void visitStringExpr(StringExpr node) {
    node.scope = scope
    base.visitStringExpr(node)
  }
}

class ResolveBaseTypesPass is Pass {
  ResolveTypesPass resolveTypesPass = { log }

  over void visitClassDef(ClassDef node) {
    if Expr baseType = node.baseType {
      // Resolve and validate the base type
      resolveTypesPass.visit(baseType)
      if MetaType metaType = baseType.resolvedType and
          ClassType baseClassType = metaType.instanceType and
          Symbol symbol = node.id.symbol and
          MetaType metaType = symbol.type and
          ClassType derivedType = metaType.instanceType {
        // Don't create a cycle
        ClassType? maybeClass = baseClassType
        while ClassType classType = maybeClass {
          if classType == derivedType {
            log.error(baseType.location, 'base type creates cycle')
            return
          }
          maybeClass = classType.baseType
        }

        // If we get here, we know setting the base type won't cause a cycle
        derivedType.baseType = baseClassType
        derivedType.staticScope.baseScope = baseClassType.staticScope
        derivedType.instanceScope.baseScope = baseClassType.instanceScope
      }
      else { log.error(baseType.location, 'could not resolve base type of ' + node.id.name) }
    }
    base.visitClassDef(node)
  }
}

class ResolveSymbolTypesPass is Pass {
  ResolveTypesPass resolveTypesPass = { log }

  void checkInstantiable(Location location, Type type) {
    if Type.isGeneric(type) { log.error(location, 'cannot instantiate generic type ' + type) }
  }

  over void visitVarDef(VarDef node) {
    base.visitVarDef(node)
    resolveTypesPass.visit(node.type)
    if Symbol symbol = node.id.symbol {
      symbol.type = instanceType(node.type, false)
      checkInstantiable(node.location, symbol.type)
    }
  }

  over void visitFuncDef(FuncDef node) {
    base.visitFuncDef(node)
    resolveTypesPass.visit(node.returnType)
    Type[] argTypes = {}
    string[] argNames = {}
    int splatIndex = -1
    int i = 0
    for arg in node.args {
      Type argType = instanceType(arg.type, false)
      argTypes.push(argType)
      argNames.push(arg.id.name)
      if arg.isSplat {
        if not Type.isArray(argType) {
          log.error(arg.location, 'splats must be arrays')
        }
        splatIndex = i
      }
      i++
    }
    FuncType funcType = { instanceType(node.returnType, true), argTypes, argTypes.size }
    if splatIndex != -1 { funcType.minArgCount-- }
    funcType.splatIndex = splatIndex
    funcType.argNames = argNames
    if Symbol symbol = node.id.symbol {
      symbol.type = funcType
      checkInstantiable(node.location, symbol.type)
    }
  }

  over void visitCtorDef(CtorDef node) {
    base.visitCtorDef(node)
    Type[] argTypes = {}
    string[] argNames = {}
    for arg in node.args {
      argTypes.push(instanceType(arg.type, false))
      argNames.push(arg.id.name)
    }
    FuncType funcType = { SpecialType.VOID, argTypes, argTypes.size }
    funcType.argNames = argNames
    if Symbol symbol = node.id.symbol {
      symbol.type = funcType
      checkInstantiable(node.location, symbol.type)
    }
  }
}

class CheckOverridePass is Pass {
  over void visitFuncDef(FuncDef node) {
    // Check if this function overrides another function in the base class
    bool overridesSymbol = false
    if Symbol symbol = node.id.symbol {
      Scope? baseScope = node.scope.baseScope
      while Scope scope = baseScope {
        if Symbol baseSymbol = scope.find(node.id.name) {
          if not Type.equals(baseSymbol.type, symbol.type) {
            log.error(node.location, 'function "' + node.id.name + '" of type ' + symbol.type + ' overrides symbol "' + baseSymbol.name + '" of different type ' + baseSymbol.type)
            return
          }
          overridesSymbol = true
          break
        }
        baseScope = scope.baseScope
      }
    }

    // Emit an error if the "over" annotation is incorrect
    bool shouldOverrideSymbol = (node.modifiers & Modifier.OVER) != 0
    if overridesSymbol and not shouldOverrideSymbol {
      log.error(node.location, 'function "' + node.id.name + '" overrides a function in a base class and must be marked "over"')
    }
    else if not overridesSymbol and shouldOverrideSymbol {
      log.error(node.location, 'function "' + node.id.name + '" is marked "over" but doesn\'t override anything')
    }
  }
}

class GenerateCtorsPass is Pass {
  ResolveSymbolTypesPass resolveSymbolTypesPass = { log }
  CreateScopesPass createScopesPass = { log }

  over void visitClassDef(ClassDef node) {
    base.visitClassDef(node)

    // If the body of the class doesn't have any constructors, then generate
    // some automatically. One constructor is generated with one argument per
    // field (including inherited members) and, if there are any fields with
    // default values, another constructor is generated without arguments for
    // those fields.
    if Symbol symbol = node.id.symbol and
        MetaType metaType = symbol.type and
        ClassType classType = metaType.instanceType {
      Location location = node.location
      for stmt in node.body.stmts {
        if CtorDef stmt = stmt {
          // Insert initializers for member variables with default initializations
          if Block body = stmt.body {
            ExprStmt[] initializers = {}
            for stmt in node.body.stmts {
              if VarDef varDef = stmt and (varDef.modifiers & Modifier.STATIC) == 0 and Expr value = varDef.value {
                initializers.push(ExprStmt(location, varDef.comment,
                  BinaryExpr(location,
                    MemberExpr(location,
                      ThisExpr(location),
                      IdentExpr(location, varDef.id.name)),
                    BinaryOp.ASSIGN,
                    value)))
              }
            }
            for stmt in initializers.reverse() {
              body.stmts.unshift(stmt)
              createScopesPass.scope = body.scope
              createScopesPass.visit(stmt)
              resolveSymbolTypesPass.visit(stmt)
            }
          }
          return
        }
      }
      Block body = { location }
      VarDef[] args = {}

      // Add the arguments necessary to call base constructor
      // TODO: what if the base class comes after this, and hasn't had a constructor generated yet? :(
      if ClassType baseType = classType.baseType and
          Symbol ctorSymbol = baseType.instanceScope.find('this') and
          FuncType funcType = ctorSymbol.type and
          string[] argNames = funcType.argNames {
        Expr[] baseArgs = {}
        int i = 0
        while i < argNames.size {
          string name = argNames[i]
          args.push({ location, null, IdentExpr(location, name), 0, TypeExpr(location, MetaType(funcType.argTypes[i])), null })
          baseArgs.push(IdentExpr(location, name))
          i++
        }
        body.stmts.push(ExprStmt(location, null, CallExpr(location, BaseExpr(location), baseArgs)))
      }

      // Add additional arguments for each instance variable
      for stmt in node.body.stmts {
        if VarDef varDef = stmt and (varDef.modifiers & Modifier.STATIC) == 0 {
          IdentExpr id = { location, varDef.id.name }
          TypeExpr type = { location, varDef.type.resolvedType }
          if Expr value = varDef.value {
            body.stmts.push(ExprStmt(location, varDef.comment,
              BinaryExpr(location,
                MemberExpr(location,
                  ThisExpr(location),
                  IdentExpr(location, varDef.id.name)),
                BinaryOp.ASSIGN,
                value)))
          }
          else {
            args.push({ location, null, id, 0, type, null })
            body.stmts.push(ExprStmt(location, varDef.comment,
              BinaryExpr(location,
                MemberExpr(location,
                  ThisExpr(location),
                  IdentExpr(location, varDef.id.name)),
                BinaryOp.ASSIGN,
                IdentExpr(location, varDef.id.name))))
          }
        }
      }

      // Create the constructor node
      IdentExpr id = { location, 'this' }
      CtorDef ctorDef = { location, null, id, 0, args, body }
      node.body.stmts.unshift(ctorDef)
      createScopesPass.scope = classType.instanceScope
      createScopesPass.visit(ctorDef)
      resolveSymbolTypesPass.visit(ctorDef)
    }
  }
}

class TypeContext {
  Type targetType
  Node node
}

class CtorCallInfo {
  Symbol? ctorSymbol
  Type resolvedType
}

class ResolveTypesPass is Pass {
  ClassType? classType = null
  FuncType? funcType = null
  TypeContext? context = null

  bool checkForValue(Expr node) {
    if node.resolvedType is MetaType or node.resolvedType == SpecialType.TYPE_VOID {
      log.error(node.location, node.resolvedType + ' is not a value')
      return false
    }
    return true
  }

  void visitWithContext(Type typeWithContext, Node nodeToVisit) {
    if NullableType type = typeWithContext { typeWithContext = type.innerType }
    TypeContext? old = context
    context = { typeWithContext, nodeToVisit }
    visit(nodeToVisit)
    context = old
  }

  void checkConversion(Location location, Type from, Type to) {
    if from != SpecialType.ERROR and not Type.canImplicitlyConvert(from, to) {
      log.error(location, 'cannot implicitly convert from ' + from + ' to ' + to)
    }
  }

  void visitCallArgs(Location location, FuncType funcType, Expr[] args) {
    if args.size < funcType.minArgCount {
      log.error(location, 'expected at least ' + funcType.minArgCount + ' arguments but got ' + args.size)
    }
    else if args.size > funcType.argTypes.size and funcType.splatIndex == -1 {
      log.error(location, 'expected at most ' + funcType.argTypes.size + ' arguments but got ' + args.size)
    }
    else {
      int i = 0
      while i < args.size {
        Type argType = SpecialType.ERROR
        if funcType.splatIndex != -1 and i >= funcType.splatIndex {
          if i + funcType.argTypes.size - args.size > funcType.splatIndex {
            argType = funcType.argTypes[i + funcType.argTypes.size - args.size]
          }
          else if SpecificType type = funcType.argTypes[funcType.splatIndex] and type.classType == ClassType.ARRAY {
            argType = type.paramTypes[0]
          }
        }
        else { argType = funcType.argTypes[i] }
        Expr arg = args[i]
        visitWithContext(argType, arg)
        checkConversion(arg.location, arg.resolvedType, argType)
        i++
      }
    }
  }

  ClassType? getInstanceType(Node node) {
    Scope scope = node.scope
    while scope.kind != ScopeKind.STATIC and scope.kind != ScopeKind.INSTANCE {
      if Scope parentScope = scope.parentScope { scope = parentScope }
      else { return null }
    }
    if scope.kind == ScopeKind.INSTANCE and
        Symbol symbol = scope.symbol and
        MetaType metaType = symbol.type and
        ClassType classType = metaType.instanceType {
      return classType
    }
    return null
  }

  over void visitExprStmt(ExprStmt node) {
    base.visitExprStmt(node)
    checkForValue(node.value)
  }

  over void visitIfStmt(IfStmt node) {
    base.visitIfStmt(node)
    checkConversion(node.value.location, node.value.resolvedType, ClassType.BOOL)
  }

  over void visitWhileStmt(WhileStmt node) {
    base.visitWhileStmt(node)
    checkConversion(node.value.location, node.value.resolvedType, ClassType.BOOL)
  }

  over void visitForStmt(ForStmt node) {
    visit(node.value)
    visit(node.id)
    if Symbol symbol = node.id.symbol {
      if SpecificType type = node.value.resolvedType and type.classType == ClassType.ARRAY {
        symbol.type = type.paramTypes[0]
      }
      else if node.value.resolvedType == ClassType.STRING or node.value.resolvedType == SpecialType.VAR {
        symbol.type = node.value.resolvedType
      }
      else { log.error(node.location, node.value.resolvedType + ' is not iterable') }
    }
    visit(node.body)
  }

  over void visitFailStmt(FailStmt node) {
    base.visitFailStmt(node)
    checkConversion(node.location, node.value.resolvedType, ClassType.STRING)
  }

  over void visitReturnStmt(ReturnStmt node) {
    if FuncType funcType = funcType {
      if Expr value = node.value {
        if funcType.returnType == SpecialType.VOID {
          log.error(node.location, 'returning value in void function')
        }
        else {
          visitWithContext(funcType.returnType, value)
          checkConversion(node.location, value.resolvedType, funcType.returnType)
          checkForValue(value)
        }
      }
      else if funcType.returnType != SpecialType.VOID {
        log.error(node.location, 'missing return value in non-void function')
      }
    }
    else { log.error(node.location, '"return" used outside function') }
  }

  over void visitVarDef(VarDef node) {
    visit(node.type)
    visit(node.id)
    if Expr value = node.value and MetaType metaType = node.type.resolvedType {
      visitWithContext(metaType.instanceType, value)
      checkConversion(value.location, value.resolvedType, metaType.instanceType)
    }
  }

  over void visitFuncDef(FuncDef node) {
    FuncType? old = funcType
    if Symbol symbol = node.id.symbol and FuncType type = symbol.type { funcType = type }
    base.visitFuncDef(node)
    funcType = old
  }

  over void visitCtorDef(CtorDef node) {
    FuncType? old = funcType
    if Symbol symbol = node.id.symbol and FuncType type = symbol.type { funcType = type }
    base.visitCtorDef(node)
    funcType = old
  }

  over void visitClassDef(ClassDef node) {
    ClassType? old = classType
    if Symbol symbol = node.id.symbol and MetaType metaType = symbol.type and
        ClassType type = metaType.instanceType {
      classType = type
    }
    base.visitClassDef(node)
    classType = old
  }

  over void visitBinaryExpr(BinaryExpr node) {
    visit(node.left)
    if node.op == BinaryOp.ASSIGN {
      // Allow the type of the new value to be deduced from the storage location:
      // 
      //   float fn(int) x
      //   x = a -> a
      // 
      visitWithContext(node.left.resolvedType, node.right)
      checkConversion(node.location, node.right.resolvedType, node.left.resolvedType)
    }
    else if node.op == BinaryOp.IN {
      // Allow the type of the container to be deduced from the type of the element:
      // 
      //   if x in { 1, 2, 3 } { ... }
      // 
      visitWithContext(SpecificType(ClassType.ARRAY, { node.left.resolvedType }), node.right)
    }
    else { visit(node.right) }
  }

  over void visitTertiaryExpr(TertiaryExpr node) {
    if TypeContext context = context and context.node == node {
      visit(node.value)
      visitWithContext(context.targetType, node.trueValue)
      visitWithContext(context.targetType, node.falseValue)
      checkConversion(node.trueValue.location, node.trueValue.resolvedType, context.targetType)
      checkConversion(node.falseValue.location, node.falseValue.resolvedType, context.targetType)
    }
    else { base.visitTertiaryExpr(node) }
  }

  CtorCallInfo? callCtor(Location location, Expr[] args, Type type) {
    // Common logic for class constructors
    CtorCallInfo? handleClassType(ClassType classType, Type fn(Type) patch) {
      if classType.isAbstract {
        log.error(location, 'cannot instantiate abstract class ' + classType)
        return null
      }
      else if Symbol ctorSymbol = classType.instanceScope.lookup('this', true) and
          FuncType ctorType = patch(ctorSymbol.type) {
        visitCallArgs(location, ctorType, args)
        return { ctorSymbol, type }
      }
      log.error(location, type + ' has no constructor')
      return null
    }

    if Type.isGeneric(type) {
      log.error(location, 'cannot instantiate generic type ' + type)
      return null
    }
    else if SpecificType type = type {
      // Resolve generic constructors before calling them
      return handleClassType(type.classType, ctorType ->
        Type.removeParams(ctorType, type.paramTypes))
    }
    else if ClassType classType = type {
      // Normal class constructor
      return handleClassType(classType, ctorType ->
        ctorType)
    }
    else if type is FuncType or type == SpecialType.VAR {
      // Special-case constructors for functions and var
      visitCallArgs(location, { type, { type }, 1 }, args)
      return { null, type }
    }
    log.error(location, type + ' has no constructor')
    return null
  }

  over void visitCallExpr(CallExpr node) {
    visit(node.value)
    if FuncType funcType = node.value.resolvedType {
      node.resolvedType = funcType.returnType
      visitCallArgs(node.location, funcType, node.args)
    }
    else if MetaType metaType = node.value.resolvedType {
      if CtorCallInfo info = callCtor(node.location, node.args, metaType.instanceType) {
        node.ctorSymbol = info.ctorSymbol
        node.resolvedType = info.resolvedType
      }
    }
    else if (node.value is ThisExpr or node.value is BaseExpr) and
        ClassType classType = node.value.resolvedType {
      node.ctorSymbol = classType.instanceScope.lookup('this', true)
      node.resolvedType = classType
      if Symbol ctorSymbol = node.ctorSymbol and FuncType funcType = ctorSymbol.type {
        visitCallArgs(node.location, funcType, node.args)
      }
      else { fail }
    }
    else if node.value.resolvedType == SpecialType.VAR {
      for arg in node.args { visitWithContext(SpecialType.VAR, arg) }
      node.resolvedType = SpecialType.VAR
    }
    else { log.error(node.location, 'cannot call ' + node.value.resolvedType) }
  }

  over void visitIndexExpr(IndexExpr node) {
    base.visitIndexExpr(node)
    if SpecificType type = node.value.resolvedType and type.classType == ClassType.ARRAY {
      node.resolvedType = type.paramTypes[0]
      checkConversion(node.location, node.index.resolvedType, ClassType.INT)
    }
    else if node.value.resolvedType == ClassType.STRING {
      node.resolvedType = ClassType.STRING
      checkConversion(node.location, node.index.resolvedType, ClassType.INT)
    }
    else if node.value.resolvedType == SpecialType.VAR {
      node.resolvedType = SpecialType.VAR
    }
    else { log.error(node.location, 'cannot index into ' + node.value.resolvedType) }
  }

  over void visitMemberExpr(MemberExpr node) {
    visit(node.value)

    // Find the scope to look in for the member
    Scope? maybeScope = null
    Type[] paramTypes = {}
    if MetaType metaType = node.value.resolvedType {
      if ClassType classType = metaType.instanceType {
        maybeScope = classType.staticScope
      }
      else if SpecificType instantiatedType = metaType.instanceType {
        maybeScope = instantiatedType.classType.staticScope
        paramTypes = instantiatedType.paramTypes
      }
    }
    else if ClassType classType = node.value.resolvedType {
      maybeScope = classType.instanceScope
    }
    else if SpecificType instantiatedType = node.value.resolvedType {
      maybeScope = instantiatedType.classType.instanceScope
      paramTypes = instantiatedType.paramTypes
    }
    else if node.value.resolvedType == SpecialType.VAR {
      node.resolvedType = SpecialType.VAR
      return
    }

    // Perform the lookup if we found one
    if Scope scope = maybeScope {
      Type[]? argTypes = null
      if Symbol symbol = scope.lookup(node.id.name, true) {
        node.id.symbol = symbol
        node.resolvedType = symbol.type
        if paramTypes.size > 0 { node.resolvedType = Type.removeParams(node.resolvedType, paramTypes) }
      }
      else { log.error(node.location, 'no member "' + node.id.name + '" on ' + node.value.resolvedType) }
    }
    else { log.error(node.location, node.value.resolvedType + ' has no members') }
  }

  over void visitInitExpr(InitExpr node) {
    if TypeContext context = context and context.node == node {
      if CtorCallInfo info = callCtor(node.location, node.values, context.targetType) {
        node.ctorSymbol = info.ctorSymbol
        node.resolvedType = info.resolvedType
      }
    }
    else { log.error(node.location, 'need context to resolve initializer list type') }
  }

  over void visitMatchExpr(MatchExpr node) {
    base.visitMatchExpr(node)
    if Symbol symbol = node.id.symbol {
      symbol.type = instanceType(node.type, false)
      if node.value.resolvedType is not ClassType and node.value.resolvedType is not NullableType {
        log.error(node.location, 'expected value of class or nullable type for match expression but got ' + node.value.resolvedType)
      }
      else if symbol.type is NullableType {
        log.error(node.location, 'cannot match on nullable type ' + symbol.type)
      }
      else { checkConversion(node.location, symbol.type, node.value.resolvedType) }
    }
  }

  over void visitLambdaExpr(LambdaExpr node) {
    FuncType? targetType = null
    if TypeContext context = context and context.node == node {
      if context.targetType == SpecialType.VAR {
        Type[] argTypes = {}
        for arg in node.args { argTypes.push(SpecialType.VAR) }
        targetType = { SpecialType.VAR, argTypes, argTypes.size }
      }
      else if FuncType funcType = context.targetType {
        targetType = funcType
      }
    }
    if FuncType targetType = targetType {
      if targetType.argTypes.size == node.args.size {
        int i = 0
        while i < node.args.size {
          IdentExpr arg = node.args[i]
          visit(arg)
          if Symbol symbol = arg.symbol and symbol.type == SpecialType.ERROR {
            symbol.type = targetType.argTypes[i]
          }
          i++
        }
        node.resolvedType = targetType

        // Wrap the body with the function type to resolve return statements
        FuncType? old = funcType
        funcType = targetType
        visit(node.body)
        funcType = old
      }
      else { log.error(node.location, 'lambda expected ' + node.args.size + ' arguments but context gave ' + targetType.argTypes.size) }
    }
    else { log.error(node.location, 'need context to resolve lambda type') }
  }

  over void visitArrayTypeExpr(ArrayTypeExpr node) {
    base.visitArrayTypeExpr(node)
    Type elementType = instanceType(node.elementType, false)
    if elementType != SpecialType.ERROR {
      node.resolvedType = MetaType(SpecificType(ClassType.ARRAY, { elementType }))
    }
  }

  over void visitFuncTypeExpr(FuncTypeExpr node) {
    base.visitFuncTypeExpr(node)
    Type returnType = instanceType(node.returnType, true)
    if returnType == SpecialType.ERROR { return }
    Type[] argTypes = {}
    for argType in node.argTypes {
      Type type = instanceType(argType, false)
      if type == SpecialType.ERROR { return }
      argTypes.push(type)
    }
    node.resolvedType = MetaType(FuncType(returnType, argTypes, argTypes.size))
  }

  over void visitNullableTypeExpr(NullableTypeExpr node) {
    base.visitNullableTypeExpr(node)
    Type innerType = instanceType(node.innerType, false)
    if innerType is NullableType {
      log.error(node.location, innerType + ' is already nullable')
    }
    else if innerType == SpecialType.VAR {
      node.resolvedType = MetaType.VAR
    }
    else if innerType != SpecialType.ERROR {
      node.resolvedType = MetaType(NullableType(innerType))
    }
  }

  over void visitSpecificTypeExpr(SpecificTypeExpr node) {
    base.visitSpecificTypeExpr(node)
    Type classType = instanceType(node.classType, false)
    if ClassType classType = classType and classType.params.size > 0 {
      if classType.isAbstract {
        log.error(node.location, 'cannot instantiate abstract class ' + classType)
      }
      else if classType.params.size == node.paramTypes.size {
        Type[] paramTypes = {}
        for param in node.paramTypes {
          Type type = instanceType(param, false)
          if type == SpecialType.ERROR { return }
          paramTypes.push(type)
        }
        node.resolvedType = MetaType(SpecificType(classType, paramTypes))
      }
      else { log.error(node.location, 'expected ' + classType.params.size + ' type parameters but got ' + node.paramTypes.size) }
    }
    else { log.error(node.location, classType + ' has no free type parameters') }
  }

  over void visitTypeExpr(TypeExpr node) {
    node.resolvedType = node.type
  }

  over void visitThisExpr(ThisExpr node) {
    if ClassType classType = getInstanceType(node) {
      node.resolvedType = classType
    }
    else { log.error(node.location, '"this" used outside instance scope') }
  }

  over void visitBaseExpr(BaseExpr node) {
    if ClassType classType = getInstanceType(node) {
      if ClassType baseType = classType.baseType {
        node.resolvedType = baseType
      }
      else { log.error(node.location, classType + ' has no base class') }
    }
    else { log.error(node.location, '"base" used outside instance scope') }
  }

  over void visitIdentExpr(IdentExpr node) {
    Type[]? argTypes = null
    if Symbol symbol = node.scope.lookup(node.name, false) { node.symbol = symbol }
    else { log.error(node.location, 'no symbol "' + node.name + '"') }
    if Symbol symbol = node.symbol { node.resolvedType = symbol.type }
  }

  over void visitIntExpr(IntExpr node) {
    node.resolvedType = ClassType.INT
  }

  over void visitBoolExpr(BoolExpr node) {
    node.resolvedType = ClassType.BOOL
  }

  over void visitFloatExpr(FloatExpr node) {
    node.resolvedType = ClassType.FLOAT
  }

  over void visitStringExpr(StringExpr node) {
    node.resolvedType = ClassType.STRING
  }
}
