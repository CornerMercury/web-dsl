package DSL.backend

import DSL.frontend.AST._
import typedAST._
import Builtins._

object interpreter {
  type Env = Map[String, (Ty, Value)]
  type FuncEnv = Map[String, TyFunc]

  def interpretProgram(
    typedProgram: List[Either[TyStmt, TyExpr]],
    sem: DistributionSemantics = DefaultDistributionSemantics
  ): List[Distribution] = {

    val funcEnv: FuncEnv = typedProgram.collect {
      case Left(tf: TyFunc) => tf.name -> tf
    }.toMap

    val (_, results) = typedProgram.foldLeft((Map.empty[String, (Ty, Value)], List.empty[Distribution])) {
      case ((env, acc), Left(TyAssign(name, expr))) =>
        val value = eval(expr, env, funcEnv, sem)
        (env.updated(name, (expr.ty, value)), acc)
        
      case ((env, acc), Left(_: TyFunc)) =>
        (env, acc) 

      case ((env, acc), Right(expr)) =>
        val value = eval(expr, env, funcEnv, sem)
        val dist = forceDist(value, sem)
        (env, acc :+ dist)
    }

    results
  }

  private def forceDist(v: Value, sem: DistributionSemantics): Distribution = v match {
    case DistValue(d) => d
    case PoolValue(items) => items.foldLeft(sem.scalar(0))((acc, d) => sem.add(acc, d))
  }

  private def resolveType(expr: TyExpr, env: Env): Ty = expr match {
    case TyIdent(name, _) => 
      env.get(name) match {
        case Some((t, _)) => t
        case None => expr.ty
      }
    case _ => expr.ty
  }

  private def expectDist(v: Value): Distribution = v match {
    case DistValue(d) => d
    case _ => throw new IllegalStateException(s"Type mismatch: Expected DistValue, got $v")
  }

  private def expectPool(v: Value): List[Distribution] = v match {
    case PoolValue(items) => items
    case _ => throw new IllegalStateException(s"Type mismatch: Expected PoolValue, got $v")
  }

  private def isBernoulli(d: Distribution): Boolean = {
    d.size == 2 && d.contains(0) && d.contains(1)
  }

  private def isUniformDie(d: Distribution): Boolean = {
    if (d.size <= 1) return false
    val keys = d.keys.toSeq.sorted
    if (keys != (1 to d.size)) return false
    val probs = d.values.toSeq
    val p0 = probs.head
    probs.forall(p => math.abs(p - p0) < 1e-9)
  }

  private def eval(
    expr: TyExpr,
    env: Env,
    funcEnv: FuncEnv,
    sem: DistributionSemantics
  ): Value = expr match {

    case TyIntLiteral(n, _) => DistValue(sem.scalar(n))

    case TyIdent(name, _) =>
      env.getOrElse(name,
        throw new IllegalArgumentException(s"Unbound identifier: $name"))._2

    case TyCustomDist(d, _) => DistValue(sem.custom(d))

    case TyPool(items, _) =>
      val evaluatedItems = items.flatMap { item =>
        eval(item, env, funcEnv, sem) match {
          case PoolValue(innerItems) => innerItems
          case DistValue(d) => List(d)
        }
      }
      PoolValue(evaluatedItems)

    case TyPoolConcat(left, right, _) =>
      val lVal = eval(left, env, funcEnv, sem)
      val rVal = eval(right, env, funcEnv, sem)
      
      val leftItems = lVal match {
        case PoolValue(items) => items
        case DistValue(d) => List(d)
      }
      val rightItems = rVal match {
        case PoolValue(items) => items
        case DistValue(d) => List(d)
      }
      PoolValue(leftItems ++ rightItems)

    case TyBinary(BinaryOp.Dice, countExpr, sidesExpr, _) =>
      val countVal = eval(countExpr, env, funcEnv, sem)
      val sidesVal = eval(sidesExpr, env, funcEnv, sem)
      
      val sidesDist = forceDist(sidesVal, sem)
      
      val n: Int = countVal match {
        case DistValue(d) =>
          if (d.size != 1)
            throw new IllegalStateException(s"Dice count must be a single value, got ${d.size} outcomes")
          d.keys.head.toInt
        case PoolValue(items) =>
          if (items.size != 1)
            throw new IllegalStateException(s"Dice count must be a single value, got pool with ${items.size} items")
          items.head.keys.head.toInt
        case _ =>
          throw new IllegalStateException(s"Expected DistValue or PoolValue for dice count, got $countVal")
      }
      
      if (n < 0) throw new IllegalStateException(s"Dice count cannot be negative: $n")
      
      if (n == 0) {
        PoolValue(Nil)   // empty pool
      } else {
        val oneDieDist = sidesExpr match {
          case TyIntLiteral(sides, _) =>
            // generate a uniform distribution
            sem.dice(sem.scalar(1), sem.scalar(sides))
          case _ =>
            // If it's a variable or custom distribution, use it directly
            forceDist(eval(sidesExpr, env, funcEnv, sem), sem)
        }
        
        PoolValue(List.fill(n)(oneDieDist))
      }

    case TyCall(name, args, _) =>
      Builtins.all.get(name) match {
        case Some(builtin) =>
          val evaluatedArgs = args.map(a => eval(a, env, funcEnv, sem))
          builtin.implementation(evaluatedArgs, sem)

        case None =>
          val func = funcEnv.getOrElse(name,
            throw new IllegalArgumentException(s"Undefined function: $name"))
          
          if (func.params.size != args.size) {
            throw new IllegalArgumentException(s"Function ${func.name} expects ${func.params.size} arguments, got ${args.size}")
          }

          val evaluatedArgs = args.map(a => forceDist(eval(a, env, funcEnv, sem), sem))
          
          val newEnv = env ++ func.params.zip(args).zip(evaluatedArgs).map { 
            case ((param, argExpr), value) => param.name -> (argExpr.ty, DistValue(value))
          }
          
          eval(func.body, newEnv, funcEnv, sem)
      }

    case TyMapExpr(funcName, inner, _) =>
      val innerVal = eval(inner, env, funcEnv, sem)
      val func = funcEnv.getOrElse(funcName,
        throw new IllegalArgumentException(s"Undefined function: $funcName"))
      
      if (func.params.size != 1) {
        throw new IllegalArgumentException(s"Function ${func.name} expects 1 argument for map, got ${func.params.size}")
      }

      val dist = forceDist(innerVal, sem)
      
      var result: Distribution = Map.empty
      for ((v, p) <- dist) {
        val newEnv = env + (func.params.head.name -> (DistTy(ScalarTy), DistValue(Map(v -> 1.0))))
        val resVal = eval(func.body, newEnv, funcEnv, sem)
        val resDist = forceDist(resVal, sem)
        for ((rv, rp) <- resDist) {
          result = result.updated(rv, result.getOrElse(rv, 0.0) + p * rp)
        }
      }
      DistValue(result)

    case TyBlock(stmts, finalExpr, _) =>
      var currentEnv = env
      stmts.foreach {
        case TyAssign(name, e) =>
          val value = eval(e, currentEnv, funcEnv, sem)
          currentEnv = currentEnv.updated(name, (e.ty, value))
        case TyFunc(_, _, _) => () 
      }
      eval(finalExpr, currentEnv, funcEnv, sem)

    case TyIfExpr(branches, elseB, _) =>
      val allBindings = branches.flatMap(_.bindings)
      val enumerated = enumerateBindings(allBindings, env, funcEnv, sem)

      var result: Distribution = Map.empty

      for ((bindEnv, weight) <- enumerated) {
        var branchTaken = false

        for (branch <- branches if !branchTaken) {
          val condDist = forceDist(eval(branch.condition, bindEnv, funcEnv, sem), sem)
          val pTrue = condDist.getOrElse(1, 0.0)
          val pFalse = condDist.getOrElse(0, 0.0)

          if (pTrue > 0.0) {
            val resVal = eval(branch.body, bindEnv, funcEnv, sem)
            val resDist = forceDist(resVal, sem)
            result = MathOps.merge(result, MathOps.scale(resDist, pTrue * weight))
          }

          if (pFalse > 0.0 && pTrue < 1.0) {
          } else if (pTrue > 0.0) {
            branchTaken = true
          }
        }

        if (!branchTaken) {
          val elseVal = eval(elseB, bindEnv, funcEnv, sem)
          val elseDist = forceDist(elseVal, sem)
          result = MathOps.merge(result, MathOps.scale(elseDist, weight))
        }
      }

      DistValue(result)

    case TyUnary(UnaryOp.Sum, inner, _) =>
      val actualTy = resolveType(inner, env)
      actualTy match {
        case PoolTy =>
          val items = expectPool(eval(inner, env, funcEnv, sem))
          if (items.nonEmpty) {
            val groups = items.groupBy(identity).view.mapValues(_.size).toMap

            val optimizedGroups = groups.map { case (dist, count) =>
              if (count == 1) {
                dist
              } else if (isBernoulli(dist)) {
                val p = dist(1)
                MathOps.fastBinomial(count, p)
              } else if (isUniformDie(dist)) {
                val sides = dist.keys.max
                sem.dice(sem.scalar(count), sem.scalar(sides))
              } else {
                (1 until count).foldLeft(dist)((acc, _) => sem.add(acc, dist))
              }
            }.toList

            DistValue(optimizedGroups.foldLeft(sem.scalar(0))((acc, d) => sem.add(acc, d)))
          } else {
            DistValue(sem.scalar(0))
          }

        case DistTy(_) =>
          val v = eval(inner, env, funcEnv, sem)
          DistValue(forceDist(v, sem))
        
        case UnknownTy =>
          throw new IllegalStateException("Cannot Sum UnknownTy")
      }

    case TyUnary(UnaryOp.Prod, inner, _) =>
      val actualTy = resolveType(inner, env)
      actualTy match {
        case PoolTy =>
          val items = expectPool(eval(inner, env, funcEnv, sem))
          if (items.nonEmpty) {
            val groups = items.groupBy(identity).view.mapValues(_.size).toMap

            val optimizedGroups = groups.map { case (dist, count) =>
              if (count == 1) {
                dist
              } else if (isBernoulli(dist)) {
                val p = dist(1)
                val pProd = math.pow(p, count)
                Map(0 -> (1.0 - pProd), 1 -> pProd)
              } else {
                (1 until count).foldLeft(dist)((acc, _) => sem.mul(acc, dist))
              }
            }.toList

            DistValue(optimizedGroups.foldLeft(sem.scalar(1))((acc, d) => sem.mul(acc, d)))
          } else {
             DistValue(sem.scalar(1))
          }

        case DistTy(_) =>
          val v = eval(inner, env, funcEnv, sem)
          DistValue(forceDist(v, sem))

        case UnknownTy =>
           throw new IllegalStateException("Cannot Prod UnknownTy")
      }

    case TyUnary(UnaryOp.Max, i, _) =>
      val v = eval(i, env, funcEnv, sem)
      val d = forceDist(v, sem)
      DistValue(sem.max(d))

    case TyUnary(UnaryOp.Min, i, _) =>
      val v = eval(i, env, funcEnv, sem)
      val d = forceDist(v, sem)
      DistValue(sem.min(d))

    case TyBinary(op, l, r, _) =>
      val lVal = eval(l, env, funcEnv, sem)
      val rVal = eval(r, env, funcEnv, sem)
      
      val actualTyL = resolveType(l, env)
      val actualTyR = resolveType(r, env)

      val dL = forceDist(lVal, sem)
      val dR = forceDist(rVal, sem)
      
      val res = op match {
        case BinaryOp.Add  => sem.add(dL, dR)
        case BinaryOp.Sub  => sem.sub(dL, dR)
        case BinaryOp.Mul  => sem.mul(dL, dR)
        case BinaryOp.Div  => sem.div(dL, dR)
        case BinaryOp.Eq   => sem.eq(dL, actualTyL, dR, actualTyR)
        case BinaryOp.Lt   => sem.lt(dL, actualTyL, dR, actualTyR)
        case BinaryOp.Le   => sem.le(dL, actualTyL, dR, actualTyR)
        case BinaryOp.Gt   => sem.gt(dL, actualTyL, dR, actualTyR)
        case BinaryOp.Ge   => sem.ge(dL, actualTyL, dR, actualTyR)
        case BinaryOp.Dice => throw new IllegalStateException("Dice should have been handled earlier")
      }
      DistValue(res)
  }

  private def enumerateBindings(
    bindings: List[TyRollBinding],
    baseEnv: Env,
    funcEnv: FuncEnv,
    sem: DistributionSemantics
  ): List[(Env, Double)] = {

    bindings.foldLeft(List((baseEnv, 1.0))) {
      case (acc, TyRollBinding(name, expr)) =>
        acc.flatMap { case (env, weight) =>
          val valExpr = eval(expr, env, funcEnv, sem)
          val dist = forceDist(valExpr, sem)
          dist.map { case (value, p) =>
            (env.updated(name, (DistTy(ScalarTy), DistValue(Map(value -> 1.0)))), weight * p)
          }
        }
    }
  }
}