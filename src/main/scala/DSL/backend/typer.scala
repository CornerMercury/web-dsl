package DSL.backend

import DSL.frontend.AST._
import DSL.backend.typedAST._
import semanticTypes._

object typer {

  def annotate(expr: Expr): TyExpr = infer(expr)

  private def infer(expr: Expr): TyExpr = expr match {

    case Ident(name)      => TyIdent(name, UnknownTy)
    case IntLiteral(n)    => TyIntLiteral(n, DistTy(ScalarTy))
    case CustomDist(dist) => TyCustomDist(dist, DistTy(classify(dist)))
    
    case Call(name, args) => TyCall(name, args.map(infer), DistTy(GenericTy))

    case MapExpr(funcName, inner) =>
      val tInner = infer(inner)
      TyMapExpr(funcName, tInner, DistTy(GenericTy))

    case Block(stmts, finalExpr) =>
      val tFinal = infer(finalExpr)
      TyBlock(stmts, tFinal, tFinal.ty)

    case IfExpr(branches, elseB) =>
      val tBranches = branches.map { b =>
        TyIfBranch(b.bindings, infer(b.condition), infer(b.body).asInstanceOf[TyBlock])
      }
      val tElse = infer(elseB).asInstanceOf[TyBlock]
      TyIfExpr(tBranches, tElse, tElse.ty)

    case Sum(inner) =>
      val tInner = infer(inner)
      val resTy = if (tInner.ty == DistTy(ScalarTy)) DistTy(ScalarTy) else DistTy(GenericTy)
      TyUnary(UnaryOp.Sum, tInner, resTy)

    case Prod(inner) =>
      val tInner = infer(inner)
      val resTy = if (tInner.ty == DistTy(ScalarTy)) DistTy(ScalarTy) else DistTy(GenericTy)
      TyUnary(UnaryOp.Prod, tInner, resTy)

    case Max(inner) =>
      val tInner = infer(inner)
      TyUnary(UnaryOp.Max, tInner, DistTy(ScalarTy))

    case Min(inner) =>
      val tInner = infer(inner)
      TyUnary(UnaryOp.Min, tInner, DistTy(ScalarTy))

    case Dice(c, s) =>
      val tC = infer(c)
      val tS = infer(s)
      val resTy = c match {
        case IntLiteral(1) => DistTy(UniformTy)
        case _             => PoolTy
      }
      TyBinary(BinaryOp.Dice, tC, tS, resTy)

    case Add(l, r) => binary(l, r, BinaryOp.Add)
    case Sub(l, r) => binary(l, r, BinaryOp.Sub)
    case Mul(l, r) => binary(l, r, BinaryOp.Mul)
    case Div(l, r) => binary(l, r, BinaryOp.Div)
    
    case Eq(l, r)  => binaryComp(l, r, BinaryOp.Eq)
    case Lt(l, r)  => binaryComp(l, r, BinaryOp.Lt)
    case Le(l, r)  => binaryComp(l, r, BinaryOp.Le)
    case Gt(l, r)  => binaryComp(l, r, BinaryOp.Gt)
    case Ge(l, r)  => binaryComp(l, r, BinaryOp.Ge)

    case Pool(items) =>
      val tItems = items.map(infer)
      TyPool(tItems, PoolTy)

    case PoolConcat(left, right) =>
      val tLeft = infer(left)
      val tRight = infer(right)
      TyPoolConcat(tLeft, tRight, PoolTy)
  }

  private def binary(l: Expr, r: Expr, op: BinaryOp): TyBinary = {
    val tL = infer(l)
    val tR = infer(r)
    val resTy =
      if (tL.ty == DistTy(ScalarTy) && tR.ty == DistTy(ScalarTy)) DistTy(ScalarTy)
      else DistTy(GenericTy)
    TyBinary(op, tL, tR, resTy)
  }

  private def binaryComp(l: Expr, r: Expr, op: BinaryOp): TyBinary = {
    val tL = infer(l)
    val tR = infer(r)
    
    val resTy = 
      if (tL.ty == DistTy(ScalarTy) && tR.ty == DistTy(ScalarTy)) DistTy(ScalarTy)
      else DistTy(BernoulliTy(0.0)) 
      
    TyBinary(op, tL, tR, resTy)
  }
}