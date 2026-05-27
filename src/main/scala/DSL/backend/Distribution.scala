package DSL.backend

import scala.collection.mutable

type Distribution = Map[Int, Double]

sealed trait Value
case class DistValue(d: Distribution) extends Value
case class PoolValue(items: List[Distribution]) extends Value