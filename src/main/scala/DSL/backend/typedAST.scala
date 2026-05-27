package DSL.backend

import DSL.frontend.AST._

object typedAST {

  sealed trait TyAstNode

  sealed abstract class TyExpr extends TyAstNode {
    def ty: Ty
  }

  enum UnaryOp { case Sum, Prod, Max, Min }
  enum BinaryOp { case Dice, Add, Sub, Mul, Div, Eq, Lt, Le, Gt, Ge }

  case class TyIntLiteral(value: Int, ty: Ty) extends TyExpr
  case class TyIdent(name: String, ty: Ty) extends TyExpr
  case class TyCustomDist(dist: Map[Int, Double], ty: Ty) extends TyExpr
  case class TyCall(name: String, args: List[TyExpr], ty: Ty) extends TyExpr
  case class TyUnary(op: UnaryOp, inner: TyExpr, ty: Ty) extends TyExpr
  case class TyBinary(op: BinaryOp, left: TyExpr, right: TyExpr, ty: Ty) extends TyExpr

  case class TyBlock(
    statements: List[Stmt],
    finalExpr: TyExpr,
    ty: Ty
  ) extends TyExpr

  case class TyIfBranch(
    bindings: List[RollBinding],
    condition: TyExpr,
    body: TyBlock
  )

  case class TyIfExpr(
    branches: List[TyIfBranch],
    elseBranch: TyBlock,
    ty: Ty
  ) extends TyExpr

  case class TyMapExpr(funcName: String, inner: TyExpr, ty: Ty) extends TyExpr

  // Pool Types
  case class TyPool(items: List[TyExpr], ty: Ty) extends TyExpr
  case class TyPoolConcat(left: TyExpr, right: TyExpr, ty: Ty) extends TyExpr
}