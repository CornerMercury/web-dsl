package DSL.backend

import scala.collection.mutable

object MathOps {

  type Distribution = Map[Int, Double]

  def scalar(n: Int): Distribution = Map(n -> 1.0)

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