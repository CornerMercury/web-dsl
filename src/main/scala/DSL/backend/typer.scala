package DSL.backend

import DSL.frontend.AST._
import DSL.backend.typedAST._
import semanticTypes._

object typer {

  type Env = Map[String, Ty]

  def typeProgram(topLevel: List[Either[Stmt, Expr]], env: Env): List[Either[TyStmt, TyExpr]] = {
    topLevel.map {
      case Left(stmt)  => Left(typeStmt(stmt, env))
      case Right(expr) => Right(infer(expr, env))
    }
  }

  def typeStmt(stmt: Stmt, env: Env): TyStmt = stmt match {
    case Assign(name, expr) =>
      val tExpr = infer(expr, env)
      TyAssign(name, tExpr)

    case Func(name, params, body) =>
      val paramEnv: Env = params.map { p =>
        val pTy = p.typ match {
          case Some(PoolType) => PoolTy
          case Some(DistType) => DistTy(GenericTy)
          case None => DistTy(GenericTy) 
        }
        p.name -> pTy
      }.toMap

      val tBody = infer(body, env ++ paramEnv).asInstanceOf[TyBlock]
      TyFunc(name, params, tBody)
  }

  def infer(expr: Expr, env: Env): TyExpr = expr match {
    case Ident(name) =>
      TyIdent(name, env(name))

    case IntLiteral(n) => TyIntLiteral(n, DistTy(ScalarTy))
    
    case CustomDist(dist) => TyCustomDist(dist, DistTy(classify(dist)))
    
    case Call(name, args) => 
      TyCall(name, args.map(infer(_, env)), DistTy(GenericTy))

    case MapExpr(funcName, inner) =>
      val tInner = infer(inner, env)
      TyMapExpr(funcName, tInner, DistTy(GenericTy))

    case Block(stmts, finalExpr) =>
      var currentEnv = env
      val typedStmts = stmts.map { stmt =>
        val tStmt = typeStmt(stmt, currentEnv)
        tStmt match {
          case TyAssign(name, tExpr) => currentEnv = currentEnv.updated(name, tExpr.ty)
          case _ => ()
        }
        tStmt
      }
      val tFinal = infer(finalExpr, currentEnv)
      TyBlock(typedStmts, tFinal, tFinal.ty)

    case IfExpr(branches, elseB) =>
      var bindingEnv = env
      val tBranches = branches.map { b =>
        val typedBinds = b.bindings.map { rb =>
          TyRollBinding(rb.name, infer(rb.expr, bindingEnv))
        }
        
        bindingEnv = bindingEnv ++ typedBinds.map(b => b.name -> b.expr.ty)
        
        TyIfBranch(
          typedBinds,
          infer(b.condition, bindingEnv),
          infer(b.body, bindingEnv).asInstanceOf[TyBlock]
        )
      }
      
      val tElse = infer(elseB, bindingEnv).asInstanceOf[TyBlock]
      TyIfExpr(tBranches, tElse, tElse.ty)

    case Sum(inner) =>
      val tInner = infer(inner, env)
      val resTy = tInner.ty match {
        case DistTy(x) => DistTy(x)
        case _ => DistTy(GenericTy)
      }
      TyUnary(UnaryOp.Sum, tInner, resTy)

    case Prod(inner) =>
      val tInner = infer(inner, env)
      val resTy = tInner.ty match {
        case DistTy(x) => DistTy(x)
        case PoolTy    => DistTy(GenericTy)
      }
      TyUnary(UnaryOp.Prod, tInner, resTy)

    case Max(inner) =>
      val tInner = infer(inner, env)
      TyUnary(UnaryOp.Max, tInner, DistTy(ScalarTy))

    case Min(inner) =>
      val tInner = infer(inner, env)
      TyUnary(UnaryOp.Min, tInner, DistTy(ScalarTy))

    case Dice(c, s) =>
      val tC = infer(c, env)
      val tS = infer(s, env)
      
      val resTy = c match {
        case IntLiteral(1) =>
          s match {
            case IntLiteral(_) => DistTy(UniformTy)
            case _             => tS.ty match {
              case DistTy(sub) => DistTy(sub)
              case PoolTy      => DistTy(GenericTy)
            }
          }
        case IntLiteral(0) =>
          DistTy(ScalarTy)
        case IntLiteral(n) if n > 1 =>
          PoolTy
        case _ =>
          // Non-literal count (variable, expression, etc.) → treat as pool
          PoolTy
      }
  
      TyBinary(BinaryOp.Dice, tC, tS, resTy)

    case Add(l, r) => binary(l, r, BinaryOp.Add, env)
    case Sub(l, r) => binary(l, r, BinaryOp.Sub, env)
    case Mul(l, r) => binary(l, r, BinaryOp.Mul, env)
    case Div(l, r) => binary(l, r, BinaryOp.Div, env)
    
    case Eq(l, r)  => binaryComp(l, r, BinaryOp.Eq, env)
    case Lt(l, r)  => binaryComp(l, r, BinaryOp.Lt, env)
    case Le(l, r)  => binaryComp(l, r, BinaryOp.Le, env)
    case Gt(l, r)  => binaryComp(l, r, BinaryOp.Gt, env)
    case Ge(l, r)  => binaryComp(l, r, BinaryOp.Ge, env)

    case Pool(items) =>
      val tItems = items.map(infer(_, env))
      TyPool(tItems, PoolTy)

    case PoolConcat(left, right) =>
      val tLeft = infer(left, env)
      val tRight = infer(right, env)
      TyPoolConcat(tLeft, tRight, PoolTy)
  }

  private def binary(l: Expr, r: Expr, op: BinaryOp, env: Env): TyBinary = {
    val tL = infer(l, env)
    val tR = infer(r, env)
    val resTy =
      if (tL.ty == DistTy(ScalarTy) && tR.ty == DistTy(ScalarTy)) DistTy(ScalarTy)
      else DistTy(GenericTy)
    TyBinary(op, tL, tR, resTy)
  }

  private def binaryComp(l: Expr, r: Expr, op: BinaryOp, env: Env): TyBinary = {
    val tL = infer(l, env)
    val tR = infer(r, env)
    
    val resTy = 
      if (tL.ty == DistTy(ScalarTy) && tR.ty == DistTy(ScalarTy)) DistTy(ScalarTy)
      else DistTy(BernoulliTy(0.0)) 
      
    TyBinary(op, tL, tR, resTy)
  }
}