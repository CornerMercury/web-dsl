package DSL.backend

import DSL.frontend.AST._
import DSL.backend.typedAST._
import DSL.backend.Builtins
import DSL.backend.semanticTypes._
import scala.collection.mutable

// Aliases
import DSL.frontend.AST.{Type => FType, DistType => FDistType, PoolType => FPoolType}

sealed trait TypeError
case class ArgTypeMismatch(funcName: String, paramName: String, expected: Ty, actual: Ty) extends TypeError
case class InvalidFunctionSignature(name: String) extends TypeError

object typeChecker {

  def check(program: Program): Either[List[TypeError], List[Either[TyStmt, TyExpr]]] = {
    val errors = mutable.ListBuffer.empty[TypeError]
    
    val funcEnvRaw = program.topLevel.collect { case Left(f: Func) => f.name -> f }.toMap
    
    val funcSignatures: Map[String, Map[String, Ty]] = 
      funcEnvRaw.map { case (name, func) =>
        name -> func.params.map { p =>
          val ty = p.typ match {
            case Some(FPoolType) => PoolTy
            case Some(FDistType) => DistTy(GenericTy)
            case None => DistTy(GenericTy)
          }
          p.name -> ty
        }.toMap
      }

    var typedFuncEnv: typer.FuncEnv = Map.empty

    var typeEnv = Map.empty[String, Ty]
    var typedTopLevel = List.empty[Either[TyStmt, TyExpr]]

    program.topLevel.foreach {
      case Left(Assign(name, expr)) =>
        val tExpr = typer.infer(expr, typeEnv, typedFuncEnv)
      
        validateExpr(tExpr, typeEnv, errors, funcSignatures)
        
        typedTopLevel = typedTopLevel :+ Left(typedAST.TyAssign(name, tExpr))
        typeEnv = typeEnv.updated(name, tExpr.ty)

      case Left(f @ Func(name, params, body)) =>
        val sig = funcSignatures.getOrElse(name, Map.empty)
        val funcEnvLocal = typeEnv ++ sig
        
        val tStmt = typer.typeStmt(f, funcEnvLocal, Map.empty)

        tStmt match {
          case tFunc: typedAST.TyFunc =>
            // Add to typedFuncEnv so subsequent calls can resolve it
            typedFuncEnv = typedFuncEnv.updated(tFunc.name, tFunc)
            
            var hasValidSpecialisation = false
            
            // Validate each specialisation individually
            tFunc.specialisations.foreach { case (_, specialisedBody) =>
              val specErrors = mutable.ListBuffer.empty[TypeError]
              validateExpr(specialisedBody, Map.empty, specErrors, Map.empty)
              if (specErrors.isEmpty) {
                hasValidSpecialisation = true
              }
            }
            
            // Only report an error if NO specialisations were valid
            // Temporary until scalar becomes normal type in the future
            if (!hasValidSpecialisation) {
              errors += InvalidFunctionSignature(name)
            }
            
            typedTopLevel = typedTopLevel :+ Left(tFunc)
            
          case _ => 
            // Should not happen for Func inputs
            typedTopLevel = typedTopLevel :+ Left(tStmt)
        }

      case Right(expr) =>
        val tExpr = typer.infer(expr, typeEnv, typedFuncEnv)
        validateExpr(tExpr, typeEnv, errors, funcSignatures)
        typedTopLevel = typedTopLevel :+ Right(tExpr)
    }

    if (errors.nonEmpty) Left(errors.toList)
    else Right(typedTopLevel)
  }

  private def validateExpr(
    tyExpr: TyExpr, 
    env: Map[String, Ty], 
    errors: mutable.ListBuffer[TypeError],
    funcSignatures: Map[String, Map[String, Ty]]
  ): Unit = tyExpr match {
    case TyIntLiteral(_, _) => ()
    case TyIdent(_, _) => () // Assumed valid by scope checker
    
    case TyCustomDist(_, _) => ()
    case TyPool(items, _) => items.foreach(validateExpr(_, env, errors, funcSignatures))
    case TyPoolConcat(l, r, _) => validateExpr(l, env, errors, funcSignatures); validateExpr(r, env, errors, funcSignatures)
    
    case TyBinary(BinaryOp.Dice, countExpr, _, _) =>
      // We allow any type here. If it's not a scalar, it will fail at runtime.
      validateExpr(countExpr, env, errors, funcSignatures)

    case TyUnary(_, inner, _) => validateExpr(inner, env, errors, funcSignatures)
      
    case TyCall(name, args, _) =>
      args.foreach(validateExpr(_, env, errors, funcSignatures))
      
      Builtins.all.get(name) match {
        case Some(builtin) =>
          if (args.size != builtin.paramTypes.size) {
             errors += ArgTypeMismatch(name, "arity", UnknownTy, UnknownTy)
          } else {
            args.zip(builtin.paramTypes).foreach { case (argTyExpr, expectedTy) =>
              if (!satisfies(argTyExpr.ty, expectedTy)) {
                errors += ArgTypeMismatch(name, s"arg", expectedTy, argTyExpr.ty)
              }
            }
          }
        case None =>
          funcSignatures.get(name).foreach { signature =>
            // Check argument count
            if (args.size != signature.size) {
               errors += ArgTypeMismatch(name, "arity", UnknownTy, UnknownTy)
            } else {
              args.zip(signature).foreach { case (argTyExpr, (paramName, expectedTy)) =>
                if (!satisfies(argTyExpr.ty, expectedTy)) {
                  errors += ArgTypeMismatch(name, paramName, expectedTy, argTyExpr.ty)
                }
              }
            }
          }
      }
        
    case TyMapExpr(funcName, inner, _) =>
      validateExpr(inner, env, errors, funcSignatures)
        
    case TyBlock(stmts, finalExpr, _) =>
      stmts.foreach {
        case TyAssign(_, e) => validateExpr(e, env, errors, funcSignatures)
        case tf: typedAST.TyFunc =>
           tf.specialisations.values.foreach { specialisedBody =>
             validateExpr(specialisedBody, env, errors, funcSignatures)
           }
      }
      validateExpr(finalExpr, env, errors, funcSignatures)
        
    case TyIfExpr(branches, elseB, _) =>
      branches.foreach { b =>
        b.bindings.foreach(bind => validateExpr(bind.expr, env, errors, funcSignatures))
        validateExpr(b.condition, env ++ b.bindings.map(x => x.name -> x.expr.ty), errors, funcSignatures)
        validateExpr(b.body, env ++ b.bindings.map(x => x.name -> x.expr.ty), errors, funcSignatures)
      }
      validateExpr(elseB, env, errors, funcSignatures)
        
    case TyBinary(_, l, r, _) =>
      validateExpr(l, env, errors, funcSignatures)
      validateExpr(r, env, errors, funcSignatures)
  }

  private def isScalar(expr: TyExpr): Boolean = expr.ty == DistTy(ScalarTy)

  private def satisfies(actual: Ty, required: Ty): Boolean = {
    if (required == PoolTy) actual == PoolTy 
    else if (required == DistTy(GenericTy)) actual.isInstanceOf[DistTy] || actual == PoolTy
    else if (required == DistTy(ScalarTy)) 
      actual.isInstanceOf[DistTy]
    else true
  }

  private def isScalarOrUnknown(ty: Ty): Boolean = 
    ty == DistTy(ScalarTy) || ty == UnknownTy
}