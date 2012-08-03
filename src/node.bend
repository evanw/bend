class UnaryOp {
  NEG
  NOT
  BITNOT
  PRE_INC
  PRE_DEC
  POST_INC
  POST_DEC
}

class BinaryOp {
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
  AND
  OR
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
  IS
  IN
}

////////////////////////////////////////////////////////////////////////////////
// Nodes
////////////////////////////////////////////////////////////////////////////////

abstract class Node { Location location; Scope scope = Scope.ERROR;          void accept(Visitor v) }
class Module is Node { Block body;                                           over void accept(Visitor v) { v.visitModule(this) } }
class Block is Node { Stmt[] stmts = {};                                     over void accept(Visitor v) { v.visitBlock(this) } }

////////////////////////////////////////////////////////////////////////////////
// Statements
////////////////////////////////////////////////////////////////////////////////

abstract class Stmt is Node { string? comment }
class ExprStmt is Stmt { Expr value;                                         over void accept(Visitor v) { v.visitExprStmt(this) } }
class IfStmt is Stmt { Expr value; Block trueBody; Block? falseBody;         over void accept(Visitor v) { v.visitIfStmt(this) } }
class WhileStmt is Stmt { Expr value; Block body;                            over void accept(Visitor v) { v.visitWhileStmt(this) } }
class ForStmt is Stmt { IdentExpr id; Expr value; Block body;                over void accept(Visitor v) { v.visitForStmt(this) } }
class ContinueStmt is Stmt { int value;                                      over void accept(Visitor v) { v.visitContinueStmt(this) } }
class BreakStmt is Stmt { int value;                                         over void accept(Visitor v) { v.visitBreakStmt(this) } }
class FailStmt is Stmt { Expr value;                                         over void accept(Visitor v) { v.visitFailStmt(this) } }
class ReturnStmt is Stmt { Expr? value;                                      over void accept(Visitor v) { v.visitReturnStmt(this) } }
class CommentStmt is Stmt {                                                  over void accept(Visitor v) { v.visitCommentStmt(this) } }

////////////////////////////////////////////////////////////////////////////////
// Definitions
////////////////////////////////////////////////////////////////////////////////

abstract class Def is Stmt { IdentExpr id; int modifiers }
class VarDef is Def { Expr type; Expr? value; bool isSplat = false;          over void accept(Visitor v) { v.visitVarDef(this) } }
class FuncDef is Def { Expr returnType; VarDef[] args; Block? body;          over void accept(Visitor v) { v.visitFuncDef(this) } }
class CtorDef is Def { VarDef[] args; Block? body;                           over void accept(Visitor v) { v.visitCtorDef(this) } }
class ClassDef is Def { Expr? baseType; Block body;                          over void accept(Visitor v) { v.visitClassDef(this) } }

////////////////////////////////////////////////////////////////////////////////
// Expressions
////////////////////////////////////////////////////////////////////////////////

abstract class Expr is Node { Type resolvedType = SpecialType.ERROR }
class UnaryExpr is Expr { UnaryOp op; Expr value;                            over void accept(Visitor v) { v.visitUnaryExpr(this) } }
class BinaryExpr is Expr { Expr left; BinaryOp op; Expr right;               over void accept(Visitor v) { v.visitBinaryExpr(this) } }
class TertiaryExpr is Expr { Expr trueValue; Expr value; Expr falseValue;    over void accept(Visitor v) { v.visitTertiaryExpr(this) } }
class CallExpr is Expr { Expr value; Expr[] args; Symbol? ctorSymbol = null; over void accept(Visitor v) { v.visitCallExpr(this) } }
class IndexExpr is Expr { Expr value; Expr index;                            over void accept(Visitor v) { v.visitIndexExpr(this) } }
class MemberExpr is Expr { Expr value; IdentExpr id;                         over void accept(Visitor v) { v.visitMemberExpr(this) } }
class InitExpr is Expr { Expr[] values; Symbol? ctorSymbol = null;           over void accept(Visitor v) { v.visitInitExpr(this) } }
class MatchExpr is Expr { IdentExpr id; Expr type; Expr value;               over void accept(Visitor v) { v.visitMatchExpr(this) } }
class LambdaExpr is Expr { IdentExpr[] args; Block body;                     over void accept(Visitor v) { v.visitLambdaExpr(this) } }
class ArrayTypeExpr is Expr { Expr elementType;                              over void accept(Visitor v) { v.visitArrayTypeExpr(this) } }
class FuncTypeExpr is Expr { Expr returnType; Expr[] args;                   over void accept(Visitor v) { v.visitFuncTypeExpr(this) } }
class NullableTypeExpr is Expr { Expr innerType;                             over void accept(Visitor v) { v.visitNullableTypeExpr(this) } }

////////////////////////////////////////////////////////////////////////////////
// Literals
////////////////////////////////////////////////////////////////////////////////

class TypeExpr is Expr { Type type;                                          over void accept(Visitor v) { v.visitTypeExpr(this) } }
class ThisExpr is Expr {                                                     over void accept(Visitor v) { v.visitThisExpr(this) } }
class BaseExpr is Expr {                                                     over void accept(Visitor v) { v.visitBaseExpr(this) } }
class IdentExpr is Expr { string name; Symbol? symbol = null;                over void accept(Visitor v) { v.visitIdentExpr(this) } }
class IntExpr is Expr { int value;                                           over void accept(Visitor v) { v.visitIntExpr(this) } }
class BoolExpr is Expr { bool value;                                         over void accept(Visitor v) { v.visitBoolExpr(this) } }
class FloatExpr is Expr { float value;                                       over void accept(Visitor v) { v.visitFloatExpr(this) } }
class StringExpr is Expr { string value;                                     over void accept(Visitor v) { v.visitStringExpr(this) } }

////////////////////////////////////////////////////////////////////////////////
// Visitor
////////////////////////////////////////////////////////////////////////////////

class Visitor {
  // Nodes
  void visit(Node? node) {
    if Node node = node { node.accept(this) }
  }
  void visitModule(Module node) {
    visit(node.body)
  }
  void visitBlock(Block node) {
    for stmt in node.stmts { visit(stmt) }
  }

  // Statements
  void visitExprStmt(ExprStmt node) {
    visit(node.value)
  }
  void visitIfStmt(IfStmt node) {
    visit(node.value)
    visit(node.trueBody)
    visit(node.falseBody)
  }
  void visitWhileStmt(WhileStmt node) {
    visit(node.value)
    visit(node.body)
  }
  void visitForStmt(ForStmt node) {
    visit(node.id)
    visit(node.value)
    visit(node.body)
  }
  void visitContinueStmt(ContinueStmt node) {}
  void visitBreakStmt(BreakStmt node) {}
  void visitFailStmt(FailStmt node) {
    visit(node.value)
  }
  void visitReturnStmt(ReturnStmt node) {
    visit(node.value)
  }
  void visitCommentStmt(CommentStmt node) {}

  // Definitions
  void visitVarDef(VarDef node) {
    visit(node.id)
    visit(node.type)
    visit(node.value)
  }
  void visitFuncDef(FuncDef node) {
    visit(node.id)
    visit(node.returnType)
    for arg in node.args { visit(arg) }
    visit(node.body)
  }
  void visitCtorDef(CtorDef node) {
    visit(node.id)
    for arg in node.args { visit(arg) }
    visit(node.body)
  }
  void visitClassDef(ClassDef node) {
    visit(node.id)
    visit(node.baseType)
    visit(node.body)
  }

  // Expressions
  void visitUnaryExpr(UnaryExpr node) {
    visit(node.value)
  }
  void visitBinaryExpr(BinaryExpr node) {
    visit(node.left)
    visit(node.right)
  }
  void visitTertiaryExpr(TertiaryExpr node) {
    visit(node.trueValue)
    visit(node.value)
    visit(node.falseValue)
  }
  void visitCallExpr(CallExpr node) {
    visit(node.value)
    for arg in node.args { visit(arg) }
  }
  void visitIndexExpr(IndexExpr node) {
    visit(node.value)
    visit(node.index)
  }
  void visitMemberExpr(MemberExpr node) {
    visit(node.id)
    visit(node.value)
  }
  void visitInitExpr(InitExpr node) {
    for value in node.values { visit(value) }
  }
  void visitMatchExpr(MatchExpr node) {
    visit(node.id)
    visit(node.type)
    visit(node.value)
  }
  void visitLambdaExpr(LambdaExpr node) {
    for arg in node.args { visit(arg) }
    visit(node.body)
  }
  void visitArrayTypeExpr(ArrayTypeExpr node) {
    visit(node.elementType)
  }
  void visitFuncTypeExpr(FuncTypeExpr node) {
    visit(node.returnType)
    for arg in node.args { visit(arg) }
  }
  void visitNullableTypeExpr(NullableTypeExpr node) {
    visit(node.innerType)
  }

  // Literals
  void visitTypeExpr(TypeExpr node) {}
  void visitThisExpr(ThisExpr node) {}
  void visitBaseExpr(BaseExpr node) {}
  void visitIdentExpr(IdentExpr node) {}
  void visitIntExpr(IntExpr node) {}
  void visitBoolExpr(BoolExpr node) {}
  void visitFloatExpr(FloatExpr node) {}
  void visitStringExpr(StringExpr node) {}
}
