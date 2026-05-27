package DSL.backend

sealed trait Ty

/** Type is not (yet) known – reserved for future inputs/variables. */
case object UnknownTy extends Ty

/** A Distribution type wrapping a specific DistSubTy */
case class DistTy(subTy: DistSubTy) extends Ty

/** A collection of distributions, kept separate for lazy evaluation (e.g. Pools). */
case object PoolTy extends Ty

/** Subtypes for DistTy, used for optimisation purposes */
sealed trait DistSubTy

/** Single outcome with probability 1.0. */
case object ScalarTy extends DistSubTy

/** 
 * A specific "Two Outcome" distribution where the outcomes are EXACTLY {0, 1}.
 * This represents a Bernoulli Trial with probability of success 'p'.
 */
case class BernoulliTy(p: Double) extends DistSubTy

/** Any other distribution with exactly two outcomes (e.g. {1, 5}). */
case object BinomialTy extends DistSubTy

/** Any number of outcomes, all with equal probability. */
case object UniformTy extends DistSubTy

/** Arbitrary discrete distribution. */
case object GenericTy extends DistSubTy

object semanticTypes {

  def classify(dist: Map[Int, Double]): DistSubTy = {
    if (dist.size == 1) {
      ScalarTy
    } 
    else if (dist.size == 2) {
      // Check for Bernoulli: Keys must be exactly 0 and 1
      if (dist.contains(0) && dist.contains(1)) {
        BernoulliTy(dist(1)) // Store 'p' (probability of 1)
      } else {
        BinomialTy // Generic 2-outcome distribution (e.g. Coin flip 1 or 2)
      }
    } 
    else if (dist.isEmpty) {
      GenericTy
    } 
    else {
      val probs = dist.values.toSeq
      val p0 = probs.head
      if (probs.forall(p => math.abs(p - p0) < 1e-12)) UniformTy
      else GenericTy
    }
  }
}