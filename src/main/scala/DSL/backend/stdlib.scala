package DSL.backend

import typedAST._
import semanticTypes._

object Builtins {
  extension (v: Value) {
    private def asScalar: Int = v match {
      case DistValue(d) if d.size == 1 => d.keys.head
      case _ => throw new IllegalArgumentException(s"Expected scalar, got $v")
    }
    
    private def asPool: List[Distribution] = v match {
      case PoolValue(items) => items
      case DistValue(d) => List(d)
      case _ => throw new IllegalArgumentException(s"Expected pool or dist, got $v")
    }
  }

  case class BuiltinFunction(
    name: String,
    paramTypes: List[Ty], 
    implementation: (List[Value], DistributionSemantics) => Value
  )

  private val functions: List[BuiltinFunction] = List(
    BuiltinFunction("keepLargest", List(DistTy(ScalarTy), PoolTy),
      (args, sem) => {
        val k = args(0).asScalar
        val pool = args(1).asPool
        DistValue(MathOps.keepLargest(k, pool))
      }
    ),
    
    BuiltinFunction("keepSmallest", List(DistTy(ScalarTy), PoolTy),
      (args, sem) => {
        val k = args(0).asScalar
        val pool = args(1).asPool
        DistValue(MathOps.keepSmallest(k, pool))
      }
    ),

    BuiltinFunction("dropLargest", List(DistTy(ScalarTy), PoolTy),
      (args, sem) => {
        val k = args(0).asScalar
        val pool = args(1).asPool
        val n = pool.size
        DistValue(MathOps.keepSmallest(math.max(0, n - k), pool))
      }
    ),

    BuiltinFunction("dropSmallest", List(DistTy(ScalarTy), PoolTy),
      (args, sem) => {
        val k = args(0).asScalar
        val pool = args(1).asPool
        val n = pool.size
        DistValue(MathOps.keepLargest(math.max(0, n - k), pool))
      }
    )
  )

  val all: Map[String, BuiltinFunction] = functions.map(f => f.name -> f).toMap
}