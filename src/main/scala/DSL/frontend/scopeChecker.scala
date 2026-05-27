package DSL.frontend

import DSL.frontend.AST._
import DSL.backend.Builtins 
import scala.collection.mutable

sealed trait ScopeError
case class UndeclaredVariable(name: String) extends ScopeError
case class UndeclaredFunction(name: String) extends ScopeError
case class DuplicateFunction(name: String) extends ScopeError
case class DuplicateParameter(name: String) extends ScopeError

object scopeChecker {

  val builtInFunctions: Set[String] = Builtins.all.keys.toSet

  def check(program: Program): List[ScopeError] = {
    val errors = mutable.ListBuffer.empty[ScopeError]
    val scopes = mutable.Stack.empty[mutable.Set[String]]
    val declaredFuncs = mutable.Set.empty[String]

    def downLayer(): Unit = scopes.push(mutable.Set.empty)
    def upLayer(): Unit = scopes.pop()
    def currentScope: mutable.Set[String] = scopes.head
    def isInScope(name: String): Boolean = scopes.exists(_.contains(name))
    def declareVar(name: String): Unit = currentScope.add(name)

    def declareFunc(name: String): Unit = {
      if (declaredFuncs.contains(name)) errors += DuplicateFunction(name)
      else declaredFuncs.add(name)
    }

    def checkExpr(expr: Expr): Unit = expr match {
      case Ident(name) =>
        if (!isInScope(name))
          errors += UndeclaredVariable(name)

      case Call(name, args) =>
        if (!declaredFuncs.contains(name) && !builtInFunctions.contains(name))
          errors += UndeclaredFunction(name)
        args.foreach(checkExpr)
        
      case MapExpr(funcName, inner) =>
        if (!declaredFuncs.contains(funcName) && !builtInFunctions.contains(funcName))
          errors += UndeclaredFunction(funcName)
        checkExpr(inner)

      case Dice(c, s) => checkExpr(c); checkExpr(s)
      case Sum(i)     => checkExpr(i)
      case Prod(i)    => checkExpr(i)
      case Max(i)     => checkExpr(i)
      case Min(i)     => checkExpr(i)
      
      case Pool(items) => items.foreach(checkExpr)
      case PoolConcat(l, r) => checkExpr(l); checkExpr(r)

      case Add(l, r)  => checkExpr(l); checkExpr(r)
      case Sub(l, r)  => checkExpr(l); checkExpr(r)
      case Mul(l, r)  => checkExpr(l); checkExpr(r)
      case Div(l, r)  => checkExpr(l); checkExpr(r)
      case Eq(l, r)   => checkExpr(l); checkExpr(r)
      case Lt(l, r)   => checkExpr(l); checkExpr(r)
      case Le(l, r)   => checkExpr(l); checkExpr(r)
      case Gt(l, r)   => checkExpr(l); checkExpr(r)
      case Ge(l, r)   => checkExpr(l); checkExpr(r)

      case Block(stmts, finalExpr) =>
        downLayer()
        stmts.foreach(checkStmt)
        checkExpr(finalExpr)
        upLayer()

      case IfExpr(branches, elseB) =>
        downLayer()
        branches.foreach { branch =>
          branch.bindings.foreach { b =>
            checkExpr(b.expr)
            declareVar(b.name)
          }
        }
        branches.foreach { branch =>
          checkExpr(branch.condition)
          checkExpr(branch.body)
        }
        checkExpr(elseB)
        upLayer()

      case IfBranch(bindings, cond, body) =>
        downLayer()
        bindings.foreach { b =>
          checkExpr(b.expr)
          declareVar(b.name)
        }
        checkExpr(cond)
        checkExpr(body)
        upLayer()

      case IntLiteral(_) | CustomDist(_) => ()
    }

    def checkStmt(stmt: Stmt): Unit = stmt match {
      case Assign(name, expr) =>
        checkExpr(expr)
        declareVar(name)

      case Func(name, params, body) =>
        downLayer()
        val paramSet = mutable.Set.empty[String]
        params.foreach { p =>
          if (paramSet.contains(p.name))
            errors += DuplicateParameter(p.name)
          else {
            paramSet.add(p.name)
            declareVar(p.name)
          }
        }
        checkExpr(body)
        upLayer()
    }

    program match {
      case Program(topLevel) =>
        downLayer()
        topLevel.foreach { 
          case Left(f: Func) => declareFunc(f.name)
          case _ => 
        }
        topLevel.foreach {
          case Left(stmt) => checkStmt(stmt)
          case Right(expr) => checkExpr(expr)
        }
        upLayer()
    }

    errors.toList
  }
}