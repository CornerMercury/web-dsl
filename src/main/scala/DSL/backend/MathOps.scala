package DSL.backend

import scala.collection.mutable

object MathOps {

  type Distribution = Map[Int, Double]

  def scalar(n: Int): Distribution = Map(n -> 1.0)

  private def isBernoulli(d: Distribution): Boolean = {
    d.keys == Set(0, 1)
  }

  def keepSmallest(k: Int, pool: List[Distribution]): Distribution = {
    if (pool.isEmpty) return Map(0 -> 1.0)
    
    // Optimization: If homogeneous, use the efficient O(F * N * K) algorithm
    val firstDie = pool.head
    if (pool.forall(_ == firstDie)) {
      keepSmallest(k, pool.size, firstDie)
    } else {
      // Fallback for heterogeneous pools
      keepSmallestHeterogeneousDP(k, pool)
    }
  }

  def keepLargest(k: Int, pool: List[Distribution]): Distribution = {
    if (pool.isEmpty) return Map(0 -> 1.0)
    
    val firstDie = pool.head
    if (pool.forall(_ == firstDie)) {
      keepLargest(k, pool.size, firstDie)
    } else {
      keepLargestHeterogeneousDP(k, pool)
    }
  }

  private def keepLargestHeterogeneousDP(k: Int, pool: List[Distribution]): Distribution = {
    var dp = Map(Vector[Int]() -> 1.0)

    for (die <- pool) {
      val nextDp = mutable.Map[Vector[Int], Double]()
      for ((currentTop, probState) <- dp; (face, pFace) <- die) {
        val newTop = (currentTop :+ face).sorted.takeRight(k)
        val newProb = probState * pFace
        nextDp(newTop) = nextDp.getOrElse(newTop, 0.0) + newProb
      }
      dp = nextDp.toMap
    }

    val result = mutable.Map[Int, Double]()
    for ((faces, prob) <- dp) {
      result(faces.sum) = result.getOrElse(faces.sum, 0.0) + prob
    }
    result.toMap
  }

  private def keepSmallestHeterogeneousDP(k: Int, pool: List[Distribution]): Distribution = {
    var dp = Map(Vector[Int]() -> 1.0)

    for (die <- pool) {
      val nextDp = mutable.Map[Vector[Int], Double]()
      for ((currentBot, probState) <- dp; (face, pFace) <- die) {
        val newBot = (currentBot :+ face).sorted.take(k)
        val newProb = probState * pFace
        nextDp(newBot) = nextDp.getOrElse(newBot, 0.0) + newProb
      }
      dp = nextDp.toMap
    }

    val result = mutable.Map[Int, Double]()
    for ((faces, prob) <- dp) {
      result(faces.sum) = result.getOrElse(faces.sum, 0.0) + prob
    }
    result.toMap
  }

  def keepLargest(k: Int, n: Int, dieDist: Distribution): Distribution = {
    if (k <= 0) return Map(0 -> 1.0)
    if (n <= 0) return Map(0 -> 1.0)
    
    if (k == 1) return keepHighestOne(n, dieDist)
    if (k >= n) return sumAll(n, dieDist)

    keepLargestDP(k, n, dieDist)
  }

  def keepSmallest(k: Int, n: Int, dieDist: Distribution): Distribution = {
    if (k <= 0) return Map(0 -> 1.0)
    if (n <= 0) return Map(0 -> 1.0)

    if (k == 1) return keepLowestOne(n, dieDist)
    if (k >= n) return sumAll(n, dieDist)

    keepSmallestDP(k, n, dieDist)
  }

  def dropLargest(k: Int, n: Int, dieDist: Distribution): Distribution = {
    keepSmallest(n - k, n, dieDist)
  }

  def dropSmallest(k: Int, n: Int, dieDist: Distribution): Distribution = {
    keepLargest(n - k, n, dieDist)
  }

  private def keepHighestOne(n: Int, dieDist: Distribution): Distribution = {
    if (n == 0) return Map(0 -> 1.0)
    val faces = dieDist.keys.toSeq.sorted
    val result = mutable.Map[Int, Double]()
    var cumProb = 0.0
    for (face <- faces) {
      val pFace = dieDist(face)
      cumProb += pFace
      val prevCumProb = cumProb - pFace
      val pMax = math.pow(cumProb, n) - math.pow(prevCumProb, n)
      if (pMax > 0.0) result(face) = pMax
    }
    result.toMap
  }

  private def keepLowestOne(n: Int, dieDist: Distribution): Distribution = {
    if (n == 0) return Map(0 -> 1.0)
    val faces = dieDist.keys.toSeq.sorted(Ordering[Int].reverse)
    val result = mutable.Map[Int, Double]()
    var cumProb = 0.0
    for (face <- faces) {
      val pFace = dieDist(face)
      cumProb += pFace
      val prevCumProb = cumProb - pFace
      val pMin = math.pow(cumProb, n) - math.pow(prevCumProb, n)
      if (pMin > 0.0) result(face) = pMin
    }
    result.toMap
  }

  private def sumAll(n: Int, dieDist: Distribution): Distribution = {
    if (n <= 0) return Map(0 -> 1.0)
    (1 until n).foldLeft(dieDist)((acc, _) => convolve(acc, dieDist, _ + _))
  }

  private def keepLargestDP(k: Int, n: Int, dieDist: Distribution): Distribution = {
    val faces = dieDist.keys.toSeq.sorted(Ordering[Int].reverse)
    val cumProbs = new Array[Double](faces.length + 1)
    cumProbs(faces.length) = 0.0
    for (i <- (0 until faces.length).reverse) {
      cumProbs(i) = cumProbs(i + 1) + dieDist(faces(i))
    }

    var dp = Array.fill(n + 1)(Array.fill(k + 1)(mutable.LongMap[Double]()))
    dp(n)(0)(0L) = 1.0

    for (i <- faces.indices) {
      val face = faces(i)
      val pFace = dieDist(face)
      val totalRemainingProb = cumProbs(i)

      if (totalRemainingProb > 0.0) {
        val pCond = pFace / totalRemainingProb
        val binomials = (0 to n).map(rem => computeBinomialPMF(rem, pCond, rem)).toArray
        val nextDp = Array.fill(n + 1)(Array.fill(k + 1)(mutable.LongMap[Double]()))

        for (rem <- 0 to n; r <- 0 to k) {
          val currentMap = dp(rem)(r)
          if (currentMap.nonEmpty) {
            val binomDist = binomials(rem)
            binomDist.foreach { case (c, pC) =>
              if (pC > 0) {
                val newRem = rem - c
                val take = math.min(c, k - r)
                val newR = r + take
                val deltaSum = take.toLong * face
                val targetMap = nextDp(newRem)(newR)
                currentMap.foreach { case (sumKey, probState) =>
                  targetMap(sumKey + deltaSum) = targetMap.getOrElse(sumKey + deltaSum, 0.0) + probState * pC
                }
              }
            }
          }
        }
        dp = nextDp
      }
    }
    dp(0)(k).map { case (l, d) => l.toInt -> d }.toMap
  }

  private def keepSmallestDP(k: Int, n: Int, dieDist: Distribution): Distribution = {
    val negatedDie = dieDist.map { case (v, p) => -v -> p }
    val negatedRes = keepLargest(k, n, negatedDie)
    negatedRes.map { case (v, p) => -v -> p }
  }

  // Time Complexity: O(iS)
  def explodeN(die: Distribution, maxRolls: Int): Distribution = {
    if (maxRolls <= 0) return Map(0 -> 1.0)
    
    if (maxRolls == 1) return die

    val maxVal = die.keys.max
    val pMax = die(maxVal)
    
    if (pMax == 1.0) return Map(maxVal * maxRolls -> 1.0)
    
    if (pMax == 0.0) return die

    val nonMax = die.filterKeys(_ != maxVal).toMap
    val result = mutable.Map[Int, Double]()

    var probPow = 1.0   // pMax^k
    for (k <- 0 to maxRolls - 1) {
      val shiftAmount = k * maxVal
      for ((v, p) <- nonMax) {
        result(v + shiftAmount) =  p * probPow
      }
      probPow *= pMax
    }
    
    result(maxVal * maxRolls) = result.getOrElse(maxVal * maxRolls, 0.0) + probPow

    result.toMap
  }


  def getNthLowest(k: Int, pool: List[Distribution]): Distribution = {
    val n = pool.size
    if (n == 0 || k <= 0 || k > n) return Map.empty[Int, Double]

    val firstDie = pool.head
    if (pool.forall(_ == firstDie)) {
      nthLowestHomogeneous(k, n, firstDie)
    } else {
      if (pool.forall(isBernoulli)) {
        nthLowestBernoulli(k, n, pool)
      } else {
        nthLowestHeterogeneous(k, pool)
      }
    }
  }

  def getNthHighest(k: Int, pool: List[Distribution]): Distribution = {
    val n = pool.size
    if (n == 0 || k <= 0 || k > n) return Map.empty[Int, Double]
    getNthLowest(n - k + 1, pool)
  }

  // --- Homogeneous Case (UniformTy / GenericTy but identical) ---
  // Time Complexity: O(F * N) where F is number of unique faces
  private def nthLowestHomogeneous(k: Int, n: Int, die: Distribution): Distribution = {
    val faces = die.keys.toSeq.sorted
    val result = mutable.Map[Int, Double]()
    var prevCDF = 0.0

    for (face <- faces) {
      val pLeq = die.filterKeys(_ <= face).values.sum
      val currCDF = binomialCDF(n, pLeq, k)
      val pVal = math.max(0.0, currCDF - prevCDF)
      result(face) = pVal
      prevCDF = currCDF
    }
    result.toMap
  }

  // --- Heterogeneous Bernoulli Case (BernoulliTy) ---
  // Time Complexity: O(N^2) - Compute Poisson Binomial once
  private def nthLowestBernoulli(k: Int, n: Int, pool: List[Distribution]): Distribution = {
    
    val distSum = poissonBinomial(pool.map(d => d.getOrElse(1, 0.0))) // Prob of success (rolling 1)
    
    val pZero = distSum.filterKeys(_ <= n - k).values.sum
    val pOne = 1.0 - pZero
    
    Map(0 -> pZero, 1 -> pOne).filter(_._2 > 0.0)
  }

  // --- Generic Heterogeneous Case (GenericTy) ---
  // Time Complexity: O(F * N^2) where F is number of unique faces in the pool
  private def nthLowestHeterogeneous(k: Int, pool: List[Distribution]): Distribution = {
    val allFaces = pool.flatMap(_.keys).toSeq.sorted.distinct
    val result = mutable.Map[Int, Double]()
    
    var prevCDF = 0.0

    for (face <- allFaces) {
      val cdf = poissonBinomialCDF(k, pool, face)
      val pVal = math.max(0.0, cdf - prevCDF)
      result(face) = pVal  
      prevCDF = cdf
    }
    result.toMap
  }


  /**
   * Computes sum_{j=k}^{n} BinomialPMF(n, p, j)
   */
  private def binomialCDF(n: Int, p: Double, k: Int): Double = {
    if (p >= 1.0) return 1.0
    if (p <= 0.0) return 0.0
    if (k <= 0) return 1.0
    if (k > n) return 0.0

    val pmf = computeBinomialPMF(n, p, n) 
    (k to n).map(pmf.getOrElse(_, 0.0)).sum
  }

  /**
   * Computes the probability that the sum of independent Bernoulli variables
   * with probabilities 'probs' is >= k.
   * (Poisson Binomial CDF).
   * Time Complexity: O(N^2)
   */
  private def poissonBinomialCDF(k: Int, pool: List[Distribution], threshold: Int): Double = {
    val n = pool.size
    val probs = pool.map(d => d.filterKeys(_ <= threshold).values.sum)

    var dp = Array.fill(n + 1)(0.0)
    dp(0) = 1.0

    for (p <- probs) {
      val nextDp = Array.fill(n + 1)(0.0)
      for (j <- 0 to n) {
        if (dp(j) > 0) {
          nextDp(j) += dp(j) * (1.0 - p)
          if (j + 1 <= n) nextDp(j + 1) += dp(j) * p
        }
      }
      dp = nextDp
    }

    (k to n).map(dp).sum
  }

  /**
   * Returns the full PMF of the Poisson Binomial distribution (sum of heterogeneous Bernoullis).
   */
  private def poissonBinomial(probs: List[Double]): Map[Int, Double] = {
    val n = probs.size
    var dp = Array.fill(n + 1)(0.0)
    dp(0) = 1.0

    for (p <- probs) {
      val nextDp = Array.fill(n + 1)(0.0)
      for (j <- 0 to n) {
        if (dp(j) > 0) {
          nextDp(j) += dp(j) * (1.0 - p)
          if (j + 1 <= n) nextDp(j + 1) += dp(j) * p
        }
      }
      dp = nextDp
    }
    dp.zipWithIndex.filter(_._1 > 0).map(_.swap).toMap
  }

  private def computeBinomialPMF(n: Int, p: Double, limit: Int): Map[Int, Double] = {
    if (n == 0) return Map(0 -> 1.0)
    if (p == 0.0) return Map(0 -> 1.0)
    if (p == 1.0) return Map(n -> 1.0)

    val m = mutable.Map[Int, Double]()
    var currentProb = math.pow(1.0 - p, n) 
    m(0) = currentProb
    val q = 1.0 - p
    val kMax = math.min(limit, n)
    
    for (k <- 1 to kMax) {
      val num = p * (n - k + 1).toDouble
      val den = q * k.toDouble
      currentProb = currentProb * (num / den)
      m(k) = currentProb
    }
    m.toMap
  }

  def fastBinomial(N: Int, p: Double): Distribution = {
    if (N <= 0) return Map(0 -> 1.0)
    if (p <= 0.0) return Map(0 -> 1.0)
    if (p >= 1.0) return Map(N -> 1.0)
    val q = 1.0 - p
    val dist = mutable.Map[Int, Double]()
    var currentProb = math.pow(q, N)
    dist(0) = currentProb
    for (k <- 1 to N) {
      val numerator = p * (N - k + 1)
      val denominator = q * k
      currentProb = currentProb * (numerator / denominator)
      dist(k) = currentProb
    }
    dist.toMap
  }

  def convolve(d1: Distribution, d2: Distribution, f: (Int, Int) => Int): Distribution = {
    var result: Distribution = Map.empty
    for ((v1, p1) <- d1; (v2, p2) <- d2) {
      val v = f(v1, v2)
      result = result.updated(v, result.getOrElse(v, 0.0) + p1 * p2)
    }
    result
  }
  
  def scale(d: Distribution, factor: Double): Distribution = d.view.mapValues(_ * factor).toMap
  def merge(d1: Distribution, d2: Distribution): Distribution = d2.foldLeft(d1) { case (acc, (v, p)) => acc.updated(v, acc.getOrElse(v, 0.0) + p) }
  
  // Time Complexity: O(N * S^2) where N is count, S is sides (due to convolution)
  def diceSumDistribution(count: Int, sides: Int): Distribution = {
    if (count <= 0) return Map(0 -> 1.0)
    if (sides <= 0) return Map(0 -> 1.0)
    iterativeDice(count, sides, 0, _ + _)
  }

  // Time Complexity: O(|cDist| * |sDist| * cost(diceFn))
  def combineDice(cDist: Distribution, sDist: Distribution, diceFn: (Int, Int) => Distribution): Distribution = {
    var result: Distribution = Map.empty
    for ((c, pC) <- cDist; (s, pS) <- sDist) {
      val d = diceFn(c, s)
      for ((valD, pD) <- d) {
        result = result.updated(valD, result.getOrElse(valD, 0.0) + pD * pC * pS)
      }
    }
    result
  }

  // Time Complexity: O(|d1| * |d2|)
  def convolveDiv(d1: Distribution, d2: Distribution): Distribution = {
    var result: Distribution = Map.empty
    for ((v1, p1) <- d1; (v2, p2) <- d2 if v2 != 0) {
      val v = v1 / v2
      result = result.updated(v, result.getOrElse(v, 0.0) + p1 * p2)
    }
    result
  }

  // Helper for iterative dice calculations (Sum or Product)
  // Time Complexity: O(N * |oneDie| * |result|)
  private def iterativeDice(count: Int, sides: Int, zero: Int, op: (Int, Int) => Int): Distribution = {
    if (count <= 0 || sides <= 0) return Map(zero -> 1.0)
    val oneDie = (1 to sides).map(i => i -> 1.0 / sides).toMap
    (1 until count).foldLeft(oneDie)((acc, _) => convolve(acc, oneDie, op))
  }
}