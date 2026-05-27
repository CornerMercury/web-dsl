import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html
import parsley.{Success, Failure}

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
    val outputDiv = document.getElementById("output").asInstanceOf[html.Div]

    runBtn.onclick = { (e: dom.MouseEvent) =>
      outputDiv.innerHTML = "" // Clear previous output
      
      val sourceCode = codeInput.value
      
      try {
        val result = runCompiler(sourceCode)
        
        val pre = document.createElement("pre")
        pre.textContent = result
        outputDiv.appendChild(pre)
        
      } catch {
        case e: Exception =>
          outputDiv.textContent = s"Critical Error: ${e.getMessage}"
          e.printStackTrace()
      }
    }
  }

  def runCompiler(input: String): String = {
    val fullInput = stdlib.source + "\n" + input

    parser.parse(fullInput) match {
      case Success(p: Program) =>
        
        val scopeErrors = scopeChecker.check(p)
        if (scopeErrors.nonEmpty) {
          val errorMsg = scopeErrors.map(e => s"  - $e").mkString("\n")
          return s"Scope Errors Found:\n$errorMsg"
        }

        val optimised = optimiser.optimise(p)

        val typeErrors = typeChecker.check(optimised)
        if (typeErrors.nonEmpty) {
          val errorMsg = typeErrors.map(e => s"  - $e").mkString("\n")
          return s"Type Errors Found:\n$errorMsg"
        }

        try {
          val dists = interpreter.interpretProgram(optimised)
          
          val blocks = dists.zipWithIndex.map { case (dist, idx) =>
            val distLines = dist.toSeq.sortBy(_._1).map { case (v, p) => f"  $v%6d  ${p * 100}%6.2f%%" }
            s"Result ${idx + 1}:\n" + distLines.mkString("\n")
          }
          val distBlock = "Distributions (value → probability):\n" + blocks.mkString("\n\n")
          
          distBlock
          
        } catch {
          case e: Exception => s"Runtime Error: ${e.getMessage}"
        }

      case Failure(err) =>
        s"Syntax Error:\n$err"
    }
  }
}