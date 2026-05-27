package DSL.backend

import DSL.frontend.AST._
import typedAST._
import Builtins._

object interpreter {

  type Env = Map[String, Value]
  type FuncEnv = Map[String, Func]

  def interpretProgram(
    program: Program,
    sem: DistributionSemantics = DefaultDistributionSemantics
  ): List[Distribution] = {

    val funcEnv =
      program.topLevel.collect { case Left(f: Func) => f.name -> f }.toMap

    val (_, results) = program.topLevel.foldLeft((Map.empty[String, Value], List.empty[Distribution])) {
      case ((env, acc), Left(Assign(name, expr))) =>
        val value = eval(typer.annotate(expr), env, funcEnv, sem)
        (env.updated(name, value), acc)
        
      case ((env, acc), Left(Func(_, _, _))) =>
        (env, acc) 

      case ((env, acc), Right(expr)) =>
        val value = eval(typer.annotate(expr), env, funcEnv, sem)
        val dist = forceDist(value, sem)
        (env, acc :+ dist)
    }

    results
  }

  private def forceDist(v: Value, sem: DistributionSemantics): Distribution = v match {
    case DistValue(d) => d
    case PoolValue(items) => items.foldLeft(sem.scalar(0))((acc, d) => sem.add(acc, d))
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
        throw new IllegalArgumentException(s"Unbound identifier: $name"))

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
      val cVal = eval(countExpr, env, funcEnv, sem)
      val sVal = eval(sidesExpr, env, funcEnv, sem)

      (cVal, sVal) match {
        case (DistValue(cDist), _) =>
          if (cDist.size != 1) {
             throw new IllegalStateException("Dice count must be a scalar distribution")
          }
          
          val n = cDist.keys.head
          
          if (n <= 0) {
            PoolValue(List())
          } else if (n == 1) {
            // 1dX is DistTy(UniformTy)
            val sidesDist = forceDist(sVal, sem)
            val countOne = sem.scalar(1)
            DistValue(sem.dice(countOne, sidesDist))
          } else {
            // NdX is PoolTy
            val sidesDist = forceDist(sVal, sem)
            val countOne = sem.scalar(1)
            val oneDieDist = sem.dice(countOne, sidesDist)
            PoolValue(List.fill(n)(oneDieDist))
          }
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
          val newEnv = env ++ func.params.zip(evaluatedArgs).map { case (param, v) => param.name -> DistValue(v) }
          val res = eval(typer.annotate(func.body), newEnv, funcEnv, sem)
          res
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
        val newEnv = env + (func.params.head.name -> DistValue(Map(v -> 1.0)))
        val resVal = eval(typer.annotate(func.body), newEnv, funcEnv, sem)
        val resDist = forceDist(resVal, sem)
        for ((rv, rp) <- resDist) {
          result = result.updated(rv, result.getOrElse(rv, 0.0) + p * rp)
        }
      }
      DistValue(result)

    case TyBlock(stmts, finalExpr, _) =>
      var currentEnv = env
      stmts.foreach {
        case Assign(name, e) =>
          val value = eval(typer.annotate(e), currentEnv, funcEnv, sem)
          currentEnv = currentEnv.updated(name, value)
        case Func(_, _, _) => () 
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
      val innerVal = eval(inner, env, funcEnv, sem)
      innerVal match {
        case PoolValue(items) if items.nonEmpty =>
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

        case _ => 
          DistValue(forceDist(innerVal, sem))
      }

    case TyUnary(UnaryOp.Prod, inner, _) =>
      val innerVal = eval(inner, env, funcEnv, sem)
      innerVal match {
        case PoolValue(items) if items.nonEmpty =>
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

        case DistValue(d) => DistValue(d)
      }

    case TyUnary(UnaryOp.Max, i, _) =>
      val d = forceDist(eval(i, env, funcEnv, sem), sem)
      DistValue(sem.max(d))

    case TyUnary(UnaryOp.Min, i, _) =>
      val d = forceDist(eval(i, env, funcEnv, sem), sem)
      DistValue(sem.min(d))

    case TyBinary(op, l, r, _) =>
      val dL = forceDist(eval(l, env, funcEnv, sem), sem)
      val dR = forceDist(eval(r, env, funcEnv, sem), sem)
      
      val tyL = l.ty
      val tyR = r.ty

      val res = op match {
        case BinaryOp.Add  => sem.add(dL, dR)
        case BinaryOp.Sub  => sem.sub(dL, dR)
        case BinaryOp.Mul  => sem.mul(dL, dR)
        case BinaryOp.Div  => sem.div(dL, dR)
        case BinaryOp.Eq   => sem.eq(dL, tyL, dR, tyR)
        case BinaryOp.Lt   => sem.lt(dL, tyL, dR, tyR)
        case BinaryOp.Le   => sem.le(dL, tyL, dR, tyR)
        case BinaryOp.Gt   => sem.gt(dL, tyL, dR, tyR)
        case BinaryOp.Ge   => sem.ge(dL, tyL, dR, tyR)
        case BinaryOp.Dice => throw new IllegalStateException("Dice should have been handled earlier")
      }
      DistValue(res)
  }

  private def enumerateBindings(
    bindings: List[RollBinding],
    baseEnv: Env,
    funcEnv: FuncEnv,
    sem: DistributionSemantics
  ): List[(Env, Double)] = {

    bindings.foldLeft(List((baseEnv, 1.0))) {
      case (acc, RollBinding(name, expr)) =>
        acc.flatMap { case (env, weight) =>
          val valExpr = eval(typer.annotate(expr), env, funcEnv, sem)
          val dist = forceDist(valExpr, sem)
          dist.map { case (value, p) =>
            (env.updated(name, DistValue(Map(value -> 1.0))), weight * p)
          }
        }
    }
  }
}