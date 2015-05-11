package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal{
      // Δ = b² - 4ac
      val x = b()
      (x * x - 4 * a() * c())
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val Δ = delta()
      val A = a()
      if (Δ < 0) Set()
      else if (Δ == 0) Set(-b() / (2 * A))
      else if (A == 0) Set(-c() / b())
      else {
        // (-b ± √Δ) / (2a)
        val inv_2a = 1 / (2 * A)
        val minus_b = -b()
        val √ = Math.sqrt(Δ)
        val s1 = inv_2a * (minus_b + √)
        val s2 = inv_2a * (minus_b - √)
        Set(s1, s2)
      }
    }
  }
}
