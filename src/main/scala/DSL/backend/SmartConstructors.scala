package DSL.backend

import DSL.backend.semanticTypes._

object SmartConstructors {

  def add(d1: Distribution, d2: Distribution): Distribution = {
    val t1 = classify(d1)
    val t2 = classify(d2)

    (t1, t2) match {
      // Optimization: Summing two compatible Bernoulli trials
      case (BernoulliTy(p1), BernoulliTy(p2)) if Math.abs(p1 - p2) < 1e-9 =>
        MathOps.fastBinomial(2, p1)
      case _ =>
        MathOps.convolve(d1, d2, _ + _)
    }
  }

  def dice(countDist: Distribution, sidesDist: Distribution): Distribution = {
    // Optimization: N (Scalar) d Bernoulli(p) -> Binomial(N, p)
    // This logic assumes we are summing the dice, which is the standard behavior for NdS.
    val tCount = classify(countDist)
    val tSides = classify(sidesDist)

    (tCount, tSides) match {
      case (ScalarTy, BernoulliTy(p)) =>
        val n = countDist.keys.head
        return MathOps.fastBinomial(n, p)
      case _ =>
        // Fall through to standard computation
    }

    // Standard Path: Convolve the dice using addition (Summing the results)
    MathOps.combineDice(countDist, sidesDist, MathOps.diceSumDistribution)
  }
}