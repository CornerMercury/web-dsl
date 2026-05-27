package DSL.frontend

object AST {
  sealed trait AstNode
  sealed trait Expr extends AstNode

  case class Ident(name: String) extends Expr
  case class Call(name: String, args: List[Expr]) extends Expr
  case class IntLiteral(value: Int) extends Expr
  case class Dice(count: Expr, sides: Expr) extends Expr
  case class Sum(expr: Expr) extends Expr
  case class Prod(expr: Expr) extends Expr
  case class Max(expr: Expr) extends Expr
  case class Min(expr: Expr) extends Expr
  case class MapExpr(funcName: String, expr: Expr) extends Expr
  case class CustomDist(dist: Map[Int, Double]) extends Expr

  // Pool Nodes
  case class Pool(items: List[Expr]) extends Expr
  case class PoolConcat(left: Expr, right: Expr) extends Expr

  case class Add(left: Expr, right: Expr) extends Expr
  case class Sub(left: Expr, right: Expr) extends Expr
  case class Mul(left: Expr, right: Expr) extends Expr
  case class Div(left: Expr, right: Expr) extends Expr
  case class Eq(left: Expr, right: Expr) extends Expr
  case class Lt(left: Expr, right: Expr) extends Expr
  case class Le(left: Expr, right: Expr) extends Expr
  case class Gt(left: Expr, right: Expr) extends Expr
  case class Ge(left: Expr, right: Expr) extends Expr

  // Roll binding used inside if expression
  case class RollBinding(name: String, expr: Expr)

  case class Block(statements: List[Stmt], finalExpr: Expr) extends Expr

  // Branch helper for if/elif
  case class IfBranch(
    bindings: List[RollBinding],
    condition: Expr,
    body: Block
  ) extends AstNode

  // Expression-level if
  case class IfExpr(
    branches: List[IfBranch],
    elseBranch: Block
  ) extends Expr

  sealed trait Stmt extends AstNode
  case class Assign(name: String, expr: Expr) extends Stmt

  sealed trait Type
  case object DistType extends Type
  case object PoolType extends Type

  case class Param(name: String, typ: Option[Type])

  case class Func(name: String, params: List[Param], body: Block) extends Stmt

  // Program holds Either[Stmt, Expr] to distinguish top-level prints from definitions
  case class Program(topLevel: List[Either[Stmt, Expr]]) extends AstNode
}