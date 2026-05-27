import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html
import parsley.{Success, Failure}
import scala.scalajs.js

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
    val codeInput = document.getElementById("code-input").asInstanceOf[html.TextArea]
    val runBtn = document.getElementById("run-btn").asInstanceOf[html.Button]

    runBtn.onclick = { (e: dom.MouseEvent) =>
      val sourceCode = codeInput.value
      val jsonResult = runCompiler(sourceCode)
      
      val renderFn = js.Dynamic.global.renderOutput
      if (js.typeOf(renderFn) != "undefined") {
        renderFn(jsonResult)
      } else {
        dom.console.error("renderOutput function not found in HTML.")
      }
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

          val typeErrors = typeChecker.check(optimised)
          if (typeErrors.nonEmpty) {
            return errorJson("Type Errors", typeErrors.mkString("\n"))
          }

          try {
            val dists = interpreter.interpretProgram(optimised)
            
            val distsJson = dists.zipWithIndex.map { case (dist, idx) =>
              val sortedData = dist.toSeq.sortBy(_._1)
              val entries = sortedData.map { case (v, p) => 
                s"""{"v": $v, "p": $p}""" 
              }.mkString(",")
              
              s"""{"id": $idx, "name": "Result ${idx + 1}", "data": [$entries]}"""
            }.mkString(",")
            
            s"""{"status": "success", "distributions": [$distsJson]}"""
            
          } catch {
            case e: Exception => errorJson("Runtime Error", e.getMessage)
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
    // escaping for quotes and newlines
    val cleanMsg = msg.replace("\"", "\\\"").replace("\n", "\\n")
    s"""{"status": "error", "title": "$title", "message": "$cleanMsg"}"""
  }
}