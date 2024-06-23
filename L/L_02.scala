package L

import scala.annotation.tailrec
import scala.math.*

object L_02 extends App{

  // 2.1. tail rec func that computes factorial of a natural number

  def fact(n: Int): Int = {
    @tailrec
    def aux_fact(i: Int, acc: Int): Int = {
      if (i > n) acc
      else aux_fact(i + 1, acc * i)
    }
    aux_fact(1,1)
  }

  //  println(fact(5))

  // 2.2. tail rec func that computes the GCD of a natural number

  def gcd(a: Int, b: Int): Int = {
    @tailrec
    def gcdAux(a: Int, b: Int): Int = {
      if (b == 0) a
      else gcdAux(b, a % b)
    }
    gcdAux(a, b)
  }

  //  println(gcd(12, 6))

  // 2.3. tail rec funct computing
  // 1 + 2^2 + 3^3 + ... + (n-1)^2 + n^2 (use inner func)

  def sumSquares(n: Int): Int = {
    def auxSumSquares(crt: Int, acc: Int): Int = {
      if (crt > n) acc
      else auxSumSquares(crt + 1, acc + crt * crt)
    }
    auxSumSquares(1, 0)
  }

  //  println(sumSquares(5))

  // 2.4. computes the sum of all natural numbers within a range
  // use two styles to write this function: direct recursion, and tail recursion

  def sumNats(start: Int, stop: Int): Int = {
    if (start > stop) 0
    else start + sumNats(start + 1, stop)

  }

  //  println(sumNats(1, 8))

  def tailSumNats(start: Int, stop: Int): Int = {
    @tailrec
    def auxTailSumNats(crt: Int, acc: Int): Int = {
      if(crt > stop) acc
      else (auxTailSumNats(crt + 1, acc + crt))
    }
    auxTailSumNats(start, 0)
  }

  //  println(tailSumNats(1, 10))

  // 2.5. x, range of values [x_0, x_n]
  // ((x - x_0) - x_1) - x_2) - ... - x_n

  def subtractRange(x: Int, start: Int, stop: Int): Int = {
    @tailrec
    def subtractRangeAux(crt: Int, acc: Int): Int = {
      if (crt > stop) acc
      else subtractRangeAux(crt + 1, acc - crt)
    }
    subtractRangeAux(start, x)
  }

  //  println(subtractRange(100, 1, 10))

  // 2.6 analog to 2.5 BUT
  // x_0 - (x_1 - (x_2 - (... - (x_n - x)...)

  def subtractRange2(x: Int, start: Int, stop: Int): Int = {
    if (start > stop) x
    else start - subtractRange2(x, start + 1, stop)
  }

  def improve(xn: Double, a: Double): Double = (xn + a / xn) * 0.5

  def nth_guess(n: Int, a: Double): Double = {
    @tailrec
    def aux_nth_guess(crt: Int, acc: Double): Double = {
      if (crt > n) acc
      else aux_nth_guess(crt + 1, improve(acc, a))
    }
    aux_nth_guess(0, 1.0)
  }

  //  println(nth_guess(5, 25))

  def acceptable(xn: Double, a: Double): Boolean = {
    val op = xn * xn - a

    if(op.abs <= 0.001) true
    else false
  }

  def mySqrt(a: Double): Double = {

    def improve(xn: Double): Double = 0.5 * (xn + a / xn)

    def acceptable(xn: Double): Boolean = abs(xn * xn - a) <= 0.001

    @tailrec
    def tailSqrt(estimate: Double) : Double = {
      if (acceptable(estimate)) estimate
      else tailSqrt(improve(estimate))
    }

    tailSqrt(1.0)
  }

  println(mySqrt(16))
}
