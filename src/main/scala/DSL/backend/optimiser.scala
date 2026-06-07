package DSL.backend

import DSL.frontend.AST._

object optimiser {

  def optimise(program: Program): Program =
    Program(optimiseTopLevel(program.topLevel))
  
  private def propagateConstantsTopLevel(
      items: List[Either[Stmt, Expr]]
  ): List[Either[Stmt, Expr]] = {
    val (result, _) = items.foldLeft((List.empty[Either[Stmt, Expr]], Map.empty[String, Expr])) {
      case ((acc, env), item) =>
        item match {
          case Left(Assign(name, expr)) =>
            val optExpr = optimiseExpr(expr, env)
            val newEnv = optExpr match {
              case lit: IntLiteral => env + (name -> lit)
              case _               => env - name
            }
            (acc :+ Left(Assign(name, optExpr)), newEnv)

          case Left(func: Func) =>
            val optFunc = optimiseStmt(func).asInstanceOf[Func]
            (acc :+ Left(optFunc), env)

          case Right(expr) =>
            val optExpr = optimiseExpr(expr, env)
            val assigned = assignedVarsExpr(optExpr)
            val newEnv = env -- assigned
            (acc :+ Right(optExpr), newEnv)
        }
    }
    result
  }

  private def optimiseTopLevel(topLevel: List[Either[Stmt, Expr]]): List[Either[Stmt, Expr]] = {
    val propagated = propagateConstantsTopLevel(topLevel)
    eliminateDeadTopLevelStores(propagated)
  }

  private def optimiseBlock(stmts: List[Stmt], finalExpr: Expr): (List[Stmt], Expr) = {
    val propagated = propagateConstants(stmts)
    val finalUsed  = getUsed(finalExpr)
    val optStmts   = eliminateDeadStores(propagated, finalUsed)
    val optFinal   = optimiseExpr(finalExpr, Map.empty)
    (optStmts, optFinal)
  }

  private def propagateConstants(stmts: List[Stmt]): List[Stmt] = {
    val (finalStmts, _) =
      stmts.foldLeft((List.empty[Stmt], Map.empty[String, Expr])) {
        case ((acc, env), stmt) => stmt match {

          case Assign(name, expr) =>
            val opt = optimiseExpr(expr, env)
            val newEnv = opt match {
              case l: IntLiteral => env + (name -> l)
              case _             => env - name
            }
            (acc :+ Assign(name, opt), newEnv)

          case Func(n, p, b) =>
            val (optStmts, optFinal) = optimiseBlock(b.statements, b.finalExpr)
            (acc :+ Func(n, p, Block(optStmts, optFinal)), env)
        }
      }
    finalStmts
  }

  private def assignedVars(block: Block): Set[String] = {
    block.statements.collect {
      case Assign(name, _) => name
    }.toSet ++ assignedVarsExpr(block.finalExpr)
  }

  private def assignedVarsBranch(branch: IfBranch): Set[String] = {
    branch.bindings.map(_.name).toSet ++ assignedVarsExpr(branch.condition) ++ assignedVars(branch.body)
  }

  private def assignedVarsExpr(expr: Expr): Set[String] = expr match {
    case Block(stmts, finalExpr) =>
      stmts.collect { case Assign(name, _) => name }.toSet ++ assignedVarsExpr(finalExpr)
    case IfExpr(branches, elseB) =>
      branches.flatMap(assignedVarsBranch).toSet ++ assignedVars(elseB)
    case Dice(c, s)    => assignedVarsExpr(c) ++ assignedVarsExpr(s)
    case Sum(i)        => assignedVarsExpr(i)
    case Prod(i)       => assignedVarsExpr(i)
    case Max(i)        => assignedVarsExpr(i)
    case Min(i)        => assignedVarsExpr(i)
    case MapExpr(_, i) => assignedVarsExpr(i)
    case Add(l, r)     => assignedVarsExpr(l) ++ assignedVarsExpr(r)
    case Sub(l, r)     => assignedVarsExpr(l) ++ assignedVarsExpr(r)
    case Mul(l, r)     => assignedVarsExpr(l) ++ assignedVarsExpr(r)
    case Div(l, r)     => assignedVarsExpr(l) ++ assignedVarsExpr(r)
    case Eq(l, r)      => assignedVarsExpr(l) ++ assignedVarsExpr(r)
    case Lt(l, r)      => assignedVarsExpr(l) ++ assignedVarsExpr(r)
    case Le(l, r)      => assignedVarsExpr(l) ++ assignedVarsExpr(r)
    case Gt(l, r)      => assignedVarsExpr(l) ++ assignedVarsExpr(r)
    case Ge(l, r)      => assignedVarsExpr(l) ++ assignedVarsExpr(r)
    case Call(_, args) => args.flatMap(assignedVarsExpr).toSet
    case Pool(items)   => items.flatMap(assignedVarsExpr).toSet
    case PoolConcat(l, r) => assignedVarsExpr(l) ++ assignedVarsExpr(r)
    case _ => Set.empty
  }

  private def removeUnusedBindings(branch: IfBranch, entireIfExpr: IfExpr): IfBranch = {
    if (branch.bindings.isEmpty) return branch

    val usedInIfExpr = getUsed(entireIfExpr)

    val filteredBinds = branch.bindings.filter(b => usedInIfExpr.contains(b.name))

    branch.copy(bindings = filteredBinds)
  }

  private def optimiseIfBranch(branch: IfBranch, entireIfExpr: IfExpr): IfBranch = {
    val cleanedBranch = removeUnusedBindings(branch, entireIfExpr)
    
    val optBinds = cleanedBranch.bindings.map(b => b.copy(expr = optimiseExpr(b.expr, Map.empty)))
    val optCond  = optimiseExpr(cleanedBranch.condition, Map.empty)
    val (optStmts, optFinal) = optimiseBlock(cleanedBranch.body.statements, cleanedBranch.body.finalExpr)
    IfBranch(optBinds, optCond, Block(optStmts, optFinal))
  }

  private def optimiseStmt(stmt: Stmt): Stmt = stmt match {
    case Assign(name, expr) => Assign(name, optimiseExpr(expr, Map.empty))
    case Func(n, p, b) =>
      val (optStmts, optFinal) = optimiseBlock(b.statements, b.finalExpr)
      Func(n, p, Block(optStmts, optFinal))
  }

  private def optimiseExpr(node: Expr, env: Map[String, Expr]): Expr =
    node match {

      case Ident(n)      => env.getOrElse(n, node)

      case Add(l, r)     => foldAdd(optimiseExpr(l, env), optimiseExpr(r, env))
      case Sub(l, r)     => foldSub(optimiseExpr(l, env), optimiseExpr(r, env))
      case Mul(l, r)     => foldMul(optimiseExpr(l, env), optimiseExpr(r, env))
      case Div(l, r)     => foldDiv(optimiseExpr(l, env), optimiseExpr(r, env))
      case Eq(l, r)      => foldEq(optimiseExpr(l, env), optimiseExpr(r, env))
      case Lt(l, r)      => foldLt(optimiseExpr(l, env), optimiseExpr(r, env))
      case Le(l, r)      => foldLe(optimiseExpr(l, env), optimiseExpr(r, env))
      case Gt(l, r)      => foldGt(optimiseExpr(l, env), optimiseExpr(r, env))
      case Ge(l, r)      => foldGe(optimiseExpr(l, env), optimiseExpr(r, env))

      case Call(n, args) =>
        Call(n, args.map(optimiseExpr(_, env)))

      case Dice(c, s) =>
        Dice(optimiseExpr(c, env), optimiseExpr(s, env))

      case Sum(i)        => Sum(optimiseExpr(i, env))
      case Prod(i)       => Prod(optimiseExpr(i, env))
      case Max(i)        => Max(optimiseExpr(i, env))
      case Min(i)        => Min(optimiseExpr(i, env))
      case MapExpr(f, i) => MapExpr(f, optimiseExpr(i, env))

      case Block(stmts, finalExpr) =>
        val (optStmts, optFinal) = optimiseBlock(stmts, finalExpr)
        Block(optStmts, optFinal)

      case IfExpr(branches, elseB) =>
        val optBranches = branches.map(b => optimiseIfBranch(b, IfExpr(branches, elseB)))
        val (optStmts, optFinal) = optimiseBlock(elseB.statements, elseB.finalExpr)
        IfExpr(optBranches, Block(optStmts, optFinal))

      case Pool(items) => Pool(items.map(optimiseExpr(_, env)))
      case PoolConcat(l, r) => PoolConcat(optimiseExpr(l, env), optimiseExpr(r, env))

      case other => other
    }

  private def foldAdd(l: Expr, r: Expr) =
    (l, r) match {
      case (IntLiteral(a), IntLiteral(b)) => IntLiteral(a + b)
      case _ => Add(l, r)
    }

  private def foldSub(l: Expr, r: Expr) =
    (l, r) match {
      case (IntLiteral(a), IntLiteral(b)) => IntLiteral(a - b)
      case _ => Sub(l, r)
    }

  private def foldMul(l: Expr, r: Expr) =
    (l, r) match {
      case (IntLiteral(a), IntLiteral(b)) => IntLiteral(a * b)
      case _ => Mul(l, r)
    }

  private def foldDiv(l: Expr, r: Expr) =
    (l, r) match {
      case (IntLiteral(a), IntLiteral(b)) if b != 0 =>
        IntLiteral(a / b)
      case _ => Div(l, r)
    }

  private def foldEq(l: Expr, r: Expr) =
    (l, r) match {
      case (IntLiteral(a), IntLiteral(b)) =>
        IntLiteral(if (a == b) 1 else 0)
      case _ => Eq(l, r)
    }

  private def foldLt(l: Expr, r: Expr) =
    (l, r) match {
      case (IntLiteral(a), IntLiteral(b)) => IntLiteral(if (a < b) 1 else 0)
      case _ => Lt(l, r)
    }

  private def foldLe(l: Expr, r: Expr) =
    (l, r) match {
      case (IntLiteral(a), IntLiteral(b)) => IntLiteral(if (a <= b) 1 else 0)
      case _ => Le(l, r)
    }

  private def foldGt(l: Expr, r: Expr) =
    (l, r) match {
      case (IntLiteral(a), IntLiteral(b)) => IntLiteral(if (a > b) 1 else 0)
      case _ => Gt(l, r)
    }

  private def foldGe(l: Expr, r: Expr) =
    (l, r) match {
      case (IntLiteral(a), IntLiteral(b)) => IntLiteral(if (a >= b) 1 else 0)
      case _ => Ge(l, r)
    }

  private def eliminateDeadTopLevelStores(topLevel: List[Either[Stmt, Expr]]): List[Either[Stmt, Expr]] = {
    val (rev, _) =
      topLevel.reverse.foldLeft((List.empty[Either[Stmt, Expr]], Set.empty[String])) {
        case ((acc, live), item) => item match {
          case Left(Assign(n, _)) if live.contains(n) =>
            (item :: acc, (live - n) ++ getUsedTopLevelItem(item))
          case Left(Assign(_, _)) =>
            (acc, live)
          case _ =>
            (item :: acc, live ++ getUsedTopLevelItem(item))
        }
      }
    rev
  }

  private def eliminateDeadStores(stmts: List[Stmt], externalLive: Set[String]): List[Stmt] = {
    val (rev, _) =
      stmts.reverse.foldLeft((List.empty[Stmt], externalLive)) {
        case ((acc, live), stmt) => stmt match {

          case Assign(n, e) if live.contains(n) =>
            (stmt :: acc, (live - n) ++ getUsed(e))

          case Assign(_, _) =>
            (acc, live)

          case Func(_, _, _) =>
            (stmt :: acc, live)
        }
      }
    rev
  }

  private def getUsedTopLevelItem(item: Either[Stmt, Expr]): Set[String] = item match {
    case Left(stmt)  => getUsedStmt(stmt)
    case Right(expr) => getUsed(expr)
  }

  private def getUsedStmt(s: Stmt): Set[String] = s match {
    case Assign(_, e) => getUsed(e)
    case Func(_, p, b) =>
      (b.statements.flatMap(getUsedStmt).toSet ++ getUsed(b.finalExpr)) -- p.map(_.name).toSet
  }

  private def getUsed(e: Expr): Set[String] = e match {
    case Ident(n)      => Set(n)
    case Call(_, args) => args.flatMap(getUsed).toSet
    case Add(l, r)     => getUsed(l) ++ getUsed(r)
    case Sub(l, r)     => getUsed(l) ++ getUsed(r)
    case Mul(l, r)     => getUsed(l) ++ getUsed(r)
    case Div(l, r)     => getUsed(l) ++ getUsed(r)
    case Eq(l, r)      => getUsed(l) ++ getUsed(r)
    case Lt(l, r)      => getUsed(l) ++ getUsed(r)
    case Le(l, r)      => getUsed(l) ++ getUsed(r)
    case Gt(l, r)      => getUsed(l) ++ getUsed(r)
    case Ge(l, r)      => getUsed(l) ++ getUsed(r)
    case Dice(c, s)    => getUsed(c) ++ getUsed(s)
    case Sum(i)        => getUsed(i)
    case Prod(i)       => getUsed(i)
    case Max(i)        => getUsed(i)
    case Min(i)        => getUsed(i)
    case MapExpr(_, i) => getUsed(i)
    case Pool(items)   => items.flatMap(getUsed).toSet
    case PoolConcat(l, r) => getUsed(l) ++ getUsed(r)
    case Block(stmts, f) =>
      stmts.flatMap {
        case Assign(_, e) => getUsed(e)
        case Func(_, _, _) => Set.empty[String]
      }.toSet ++ getUsed(f)
    case IfExpr(branches, elseB) =>
      branches.flatMap(b => b.bindings.flatMap(x => getUsed(x.expr)) ++ getUsed(b.condition) ++ getUsed(b.body)).toSet ++
        getUsed(elseB)
    case IfBranch(bindings, cond, body) =>
      bindings.flatMap(x => getUsed(x.expr)).toSet ++ getUsed(cond) ++ getUsed(body)
    case _ => Set.empty
  }
}