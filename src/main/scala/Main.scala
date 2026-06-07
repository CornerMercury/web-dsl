package DSL

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html
import parsley.{Success, Failure}
import scala.scalajs.js
import scala.scalajs.js.special

import DSL.frontend.parser
import DSL.frontend.stdlib
import DSL.frontend.AST._
import DSL.frontend.scopeChecker
import DSL.backend.optimiser
import DSL.backend.typeChecker
import DSL.backend.interpreter
import DSL.backend._
import DSL.backend.typedAST._

object Main {
  def main(args: Array[String]): Unit = {
    val func: js.Function1[String, String] = (input: String) => runCompiler(input)

    if (js.typeOf(dom.window) != "undefined") {
      dom.window.asInstanceOf[js.Dynamic].runCompiler = func
    } else {
      // Worker: Use defineProperty to avoid strict mode errors
      val globalScope = js.special.fileLevelThis.asInstanceOf[js.Object]
      val descriptor = new js.PropertyDescriptor {
        value = func
        writable = true
        configurable = true
        enumerable = true
      }
      js.Object.defineProperty(globalScope, "runCompiler", descriptor)
    }
  }

  def runCompiler(input: String): String = {
    val fullInput = stdlib.source + "\n" + input

    try {
      parser.parse(fullInput) match {
        case Success(p: Program) =>
          val scopeErrors = scopeChecker.check(p)
          if (scopeErrors.nonEmpty) {
            return errorJson("Scope Errors", scopeErrors.mkString("\n"))
          }

          val optimised = optimiser.optimise(p)

          typeChecker.check(optimised) match {
            case Left(typeErrors) =>
              errorJson("Type Errors", typeErrors.mkString("\n"))

            case Right(typedProgram) =>
              try {
                val dists = interpreter.interpretProgram(typedProgram)
                val distsJson = dists.zipWithIndex.map { case (dist, idx) =>
                  val sortedData = dist.toSeq.sortBy(_._1)
                  val entries = sortedData.map { case (v, p) =>
                    s"""{"v": $v, "p": $p}"""
                  }.mkString(",")
                  s"""{"id": $idx, "name": "Result ${idx + 1}", "data": [$entries]}"""
                }.mkString(",")
                s"""{"status": "success", "distributions": [$distsJson]}"""
              } catch {
                case e: Exception =>
                  errorJson("Runtime Error", e.getMessage)
              }
          }

        case Failure(err) =>
          errorJson("Syntax Error", err.toString)
      }
    } catch {
      case e: Exception =>
        e.printStackTrace()
        errorJson("Critical Error", e.getMessage)
    }
  }

  def errorJson(title: String, msg: String): String = {
    val cleanMsg = msg.replace("\"", "\\\"").replace("\n", "\\n")
    s"""{"status": "error", "title": "$title", "message": "$cleanMsg"}"""
  }
}