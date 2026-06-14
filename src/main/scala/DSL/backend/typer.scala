package DSL.backend

import DSL.frontend.AST._
import DSL.backend.typedAST._
import semanticTypes._

object typer {

  type Env = Map[String, Ty]
  type FuncEnv = Map[String, TyFunc]

  def typeProgram(topLevel: List[Either[Stmt, Expr]], env: Env): List[Either[TyStmt, TyExpr]] = {
    var funcEnv: FuncEnv = Map.empty
    var results = List.empty[Either[TyStmt, TyExpr]]
    
    topLevel.foreach {
      case Left(stmt) =>
        val tStmt = typeStmt(stmt, env, funcEnv)
        // Collect functions into the environment for forward reference
        tStmt match {
          case tf: TyFunc => funcEnv = funcEnv.updated(tf.name, tf)
          case _ =>
        }
        results = results :+ Left(tStmt)
        
      case Right(expr) =>
        results = results :+ Right(infer(expr, env, funcEnv))
    }
    
    results
  }

  def typeStmt(stmt: Stmt, env: Env, funcEnv: FuncEnv): TyStmt = stmt match {
    case Assign(name, expr) =>
      val tExpr = infer(expr, env, funcEnv)
      TyAssign(name, tExpr)

    case Func(name, params, body) =>
      // 1. Determine the set of Ty options for each parameter based on annotation
      val paramOptions: List[List[Ty]] = params.map { p =>
        p.typ match {
          case Some(PoolType) => List(PoolTy)
          case Some(DistType) | None => 
            // Cartesian product options for Distributions
            List(
              DistTy(ScalarTy), 
              DistTy(BernoulliTy), 
              DistTy(BinomialTy), 
              DistTy(UniformTy), 
              DistTy(GenericTy)
            )
        }
      }

      // 2. Compute Cartesian Product of all options
      val combinations = cartesianProduct(paramOptions)

      // 3. Generate a specialised typed body for each combination
      val specialisations = combinations.map { types =>
        val specialisedEnv: Env = params.zip(types).map { case (p, t) =>
          p.name -> t
        }.toMap ++ env // Include outer environment

        val tBody = infer(body, specialisedEnv, funcEnv).asInstanceOf[TyBlock]
        types -> tBody
      }.toMap

      TyFunc(name, params, specialisations)
  }

  private def cartesianProduct[T](lists: List[List[T]]): List[List[T]] = lists match {
    case Nil => List(Nil)
    case head :: tail => 
      for { h <- head; t <- cartesianProduct(tail) } yield h :: t
  }

  def infer(expr: Expr, env: Env, funcEnv: FuncEnv): TyExpr = expr match {
    case Ident(name) =>
      TyIdent(name, env(name))

    case IntLiteral(n) => TyIntLiteral(n, DistTy(ScalarTy))
    
    case CustomDist(dist) => TyCustomDist(dist, DistTy(classify(dist)))
    
    case Call(name, args) => 
      val tArgs = args.map(infer(_, env, funcEnv))
      
      // Attempt to infer a specific return type if arguments are known
      val argTypes = tArgs.map(_.ty)
      
      val resTy = funcEnv.get(name) match {
        case Some(func) =>
          func.specialisations.get(argTypes) match {
            case Some(body) => body.ty
            case None => DistTy(GenericTy)
          }
        case None => DistTy(GenericTy)
      }
      
      TyCall(name, tArgs, resTy)

    case MapExpr(funcName, inner) =>
      val tInner = infer(inner, env, funcEnv)
      TyMapExpr(funcName, tInner, DistTy(GenericTy))

    case Block(stmts, finalExpr) =>
      var currentEnv = env
      val typedStmts = stmts.map { stmt =>
        val tStmt = typeStmt(stmt, currentEnv, funcEnv)
        tStmt match {
          case TyAssign(name, tExpr) => currentEnv = currentEnv.updated(name, tExpr.ty)
          case _ => ()
        }
        tStmt
      }
      val tFinal = infer(finalExpr, currentEnv, funcEnv)
      TyBlock(typedStmts, tFinal, tFinal.ty)

    case IfExpr(branches, elseB) =>
      var bindingEnv = env
      val tBranches = branches.map { b =>
        val typedBinds = b.bindings.map { rb =>
          TyRollBinding(rb.name, infer(rb.expr, bindingEnv, funcEnv))
        }
        
        bindingEnv = bindingEnv ++ typedBinds.map(b => b.name -> b.expr.ty)
        
        TyIfBranch(
          typedBinds,
          infer(b.condition, bindingEnv, funcEnv),
          infer(b.body, bindingEnv, funcEnv).asInstanceOf[TyBlock]
        )
      }
      
      val tElse = infer(elseB, bindingEnv, funcEnv).asInstanceOf[TyBlock]
      TyIfExpr(tBranches, tElse, tElse.ty)

    case Sum(inner) =>
      val tInner = infer(inner, env, funcEnv)
      val resTy = tInner.ty match {
        case DistTy(x) => DistTy(x)
        case _ => DistTy(GenericTy)
      }
      TyUnary(UnaryOp.Sum, tInner, resTy)

    case Prod(inner) =>
      val tInner = infer(inner, env, funcEnv)
      val resTy = tInner.ty match {
        case DistTy(x) => DistTy(x)
        case PoolTy    => DistTy(GenericTy)
      }
      TyUnary(UnaryOp.Prod, tInner, resTy)

    case Max(inner) =>
      val tInner = infer(inner, env, funcEnv)
      TyUnary(UnaryOp.Max, tInner, DistTy(ScalarTy))

    case Min(inner) =>
      val tInner = infer(inner, env, funcEnv)
      TyUnary(UnaryOp.Min, tInner, DistTy(ScalarTy))

    case Dice(c, s) =>
      val tC = infer(c, env, funcEnv)
      val tS = infer(s, env, funcEnv)
      
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
          PoolTy
      }
  
      TyBinary(BinaryOp.Dice, tC, tS, resTy)

    case Add(l, r) => binary(l, r, BinaryOp.Add, env, funcEnv)
    case Sub(l, r) => binary(l, r, BinaryOp.Sub, env, funcEnv)
    case Mul(l, r) => binary(l, r, BinaryOp.Mul, env, funcEnv)
    case Div(l, r) => binary(l, r, BinaryOp.Div, env, funcEnv)
    
    case Eq(l, r)  => binaryComp(l, r, BinaryOp.Eq, env, funcEnv)
    case Lt(l, r)  => binaryComp(l, r, BinaryOp.Lt, env, funcEnv)
    case Le(l, r)  => binaryComp(l, r, BinaryOp.Le, env, funcEnv)
    case Gt(l, r)  => binaryComp(l, r, BinaryOp.Gt, env, funcEnv)
    case Ge(l, r)  => binaryComp(l, r, BinaryOp.Ge, env, funcEnv)

    case Pool(items) =>
      val tItems = items.map(infer(_, env, funcEnv))
      TyPool(tItems, PoolTy)

    case PoolConcat(left, right) =>
      val tLeft = infer(left, env, funcEnv)
      val tRight = infer(right, env, funcEnv)
      TyPoolConcat(tLeft, tRight, PoolTy)
  }

  private def binary(l: Expr, r: Expr, op: BinaryOp, env: Env, funcEnv: FuncEnv): TyBinary = {
    val tL = infer(l, env, funcEnv)
    val tR = infer(r, env, funcEnv)
    val resTy =
      if (tL.ty == DistTy(ScalarTy) && tR.ty == DistTy(ScalarTy)) DistTy(ScalarTy)
      else DistTy(GenericTy)
    TyBinary(op, tL, tR, resTy)
  }

  private def binaryComp(l: Expr, r: Expr, op: BinaryOp, env: Env, funcEnv: FuncEnv): TyBinary = {
    val tL = infer(l, env, funcEnv)
    val tR = infer(r, env, funcEnv)
    
    val resTy = 
      if (tL.ty == DistTy(ScalarTy) && tR.ty == DistTy(ScalarTy)) DistTy(ScalarTy)
      else DistTy(BernoulliTy)
      
    TyBinary(op, tL, tR, resTy)
  }
}