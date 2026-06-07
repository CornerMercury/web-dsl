package DSL.backend

import DSL.backend.typedAST._
import semanticTypes._

trait DistributionSemantics {
  def scalar(n: Int): Distribution
  def custom(raw: Map[Int, Double]): Distribution

  def add(d1: Distribution, d2: Distribution): Distribution
  def sub(d1: Distribution, d2: Distribution): Distribution
  def mul(d1: Distribution, d2: Distribution): Distribution
  def div(d1: Distribution, d2: Distribution): Distribution

  // Updated to use the new 'Ty' root type. 
  // In practice, these should be DistTy, but Ty allows for UnknownTy or PoolTy 
  // if passed directly (though the interpreter usually handles PoolTy -> DistTy).
  def eq(d1: Distribution, ty1: Ty, d2: Distribution, ty2: Ty): Distribution
  def lt(d1: Distribution, ty1: Ty, d2: Distribution, ty2: Ty): Distribution
  def le(d1: Distribution, ty1: Ty, d2: Distribution, ty2: Ty): Distribution
  def gt(d1: Distribution, ty1: Ty, d2: Distribution, ty2: Ty): Distribution
  def ge(d1: Distribution, ty1: Ty, d2: Distribution, ty2: Ty): Distribution

  def dice(count: Distribution, sides: Distribution): Distribution

  def max(d: Distribution): Distribution
  def min(d: Distribution): Distribution
}

object DefaultDistributionSemantics extends DistributionSemantics {

  override def scalar(n: Int): Distribution =
    MathOps.scalar(n)

  override def custom(raw: Map[Int, Double]): Distribution = {
    val total = raw.values.sum
    if (total == 0) Map(0 -> 1.0)
    else raw.view.mapValues(_ / total).toMap
  }

  override def add(d1: Distribution, d2: Distribution): Distribution =
    SmartConstructors.add(d1, d2)

  override def sub(d1: Distribution, d2: Distribution): Distribution =
    MathOps.convolve(d1, d2, _ - _)

  override def mul(d1: Distribution, d2: Distribution): Distribution =
    MathOps.convolve(d1, d2, _ * _)

  override def div(d1: Distribution, d2: Distribution): Distribution =
    MathOps.convolveDiv(d1, d2)

  override def eq(d1: Distribution, ty1: Ty, d2: Distribution, ty2: Ty): Distribution = {
    (ty1, ty2) match {
      case (DistTy(ScalarTy), DistTy(ScalarTy)) =>
        val res = if (d1.keys.head == d2.keys.head) 1 else 0
        MathOps.scalar(res)

      case (DistTy(BernoulliTy(p1)), DistTy(BernoulliTy(p2))) =>
        val pTrue = (1.0 - p1) * (1.0 - p2) + (p1 * p2)
        booleanDist(pTrue)

      case _ =>
        val (small, large) = if (d1.size < d2.size) (d1, d2) else (d2, d1)
        var pTrue = 0.0
        small.foreach { case (k, v) => if (large.contains(k)) pTrue += v * large(k) }
        booleanDist(pTrue)
    }
  }

  override def lt(d1: Distribution, ty1: Ty, d2: Distribution, ty2: Ty): Distribution = {
    (ty1, ty2) match {
      case (DistTy(ScalarTy), DistTy(ScalarTy)) =>
        val res = if (d1.keys.head < d2.keys.head) 1 else 0
        MathOps.scalar(res)

      case (DistTy(BernoulliTy(p1)), DistTy(BernoulliTy(p2))) =>
        booleanDist((1.0 - p1) * p2)

      case (DistTy(UniformTy), DistTy(UniformTy)) if d1.size == d2.size =>
        val n = d1.size
        booleanDist((1.0 - 1.0 / n) / 2.0)

      case _ => genericConvolution(d1, d2, _ < _)
    }
  }

  override def le(d1: Distribution, ty1: Ty, d2: Distribution, ty2: Ty): Distribution = {
    (ty1, ty2) match {
      case (DistTy(ScalarTy), DistTy(ScalarTy)) =>
        val res = if (d1.keys.head <= d2.keys.head) 1 else 0
        MathOps.scalar(res)

      case (DistTy(BernoulliTy(p1)), DistTy(BernoulliTy(p2))) =>
        booleanDist(1.0 - (p1 * (1.0 - p2)))

      case (DistTy(UniformTy), DistTy(UniformTy)) if d1.size == d2.size =>
        val n = d1.size
        val pWin = (1.0 - 1.0 / n) / 2.0
        booleanDist(1.0 - pWin)

      case _ => genericConvolution(d1, d2, _ <= _)
    }
  }

  override def gt(d1: Distribution, ty1: Ty, d2: Distribution, ty2: Ty): Distribution = {
    (ty1, ty2) match {
      case (DistTy(ScalarTy), DistTy(ScalarTy)) =>
        val res = if (d1.keys.head > d2.keys.head) 1 else 0
        MathOps.scalar(res)

      case (DistTy(BernoulliTy(p1)), DistTy(BernoulliTy(p2))) =>
        booleanDist(p1 * (1.0 - p2))

      case (DistTy(UniformTy), DistTy(UniformTy)) if d1.size == d2.size =>
        val n = d1.size
        booleanDist((1.0 - 1.0 / n) / 2.0)

      case _ => genericConvolution(d1, d2, _ > _)
    }
  }

  override def ge(d1: Distribution, ty1: Ty, d2: Distribution, ty2: Ty): Distribution = {
    (ty1, ty2) match {
      case (DistTy(ScalarTy), DistTy(ScalarTy)) =>
        val res = if (d1.keys.head >= d2.keys.head) 1 else 0
        MathOps.scalar(res)

      case (DistTy(BernoulliTy(p1)), DistTy(BernoulliTy(p2))) =>
        booleanDist(1.0 - ((1.0 - p1) * p2))

      case (DistTy(UniformTy), DistTy(UniformTy)) if d1.size == d2.size =>
        val n = d1.size
        val pWin = (1.0 - 1.0 / n) / 2.0
        booleanDist(1.0 - pWin)

      case _ => genericConvolution(d1, d2, _ >= _)
    }
  }

  private def genericConvolution(d1: Distribution, d2: Distribution, op: (Int, Int) => Boolean): Distribution = {
    var pTrue = 0.0
    for ((x, px) <- d1; (y, py) <- d2) {
      if (op(x, y)) pTrue += px * py
    }
    booleanDist(pTrue)
  }

  private def booleanDist(pTrue: Double): Distribution = {
    val p = math.max(0.0, math.min(1.0, pTrue))
    
    if (p >= 1.0 - 1e-9) {
      MathOps.scalar(1)
    } else if (p <= 1e-9) {
      MathOps.scalar(0)
    } else {
      Map(0 -> (1.0 - p), 1 -> p)
    }
  }

  override def dice(
    count: Distribution,
    sides: Distribution
  ): Distribution =
    SmartConstructors.dice(count, sides)

  override def max(d: Distribution): Distribution =
    if (d.isEmpty) Map(0 -> 1.0)
    else Map(d.keys.max -> 1.0)

  override def min(d: Distribution): Distribution =
    if (d.isEmpty) Map(0 -> 1.0)
    else Map(d.keys.min -> 1.0)
}