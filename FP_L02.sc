// Ex.1: tail recursive function for factorial of n
def fact (n : Int) :Int ={
  def aux_fact (i: Int, acc: Int) : Int = {
    if (i == 0) acc
    else aux_fact(i-1, i*acc)
  }
  aux_fact(n, 1)
}

fact(5)

// Ex.2: tail recursive function for GCD
def gcd (a: Int, b: Int) : Int = {
  if (b == 0) a
  else gcd(b, a%b)
}

gcd(14, 21)

// Ex.3: tail recursive function that takes integer n and computes the sum of first n^2 elements
def sumSquares (n: Int) : Int = {
  def prev_sumSquares(i: Int, acc: Int): Int = {
    if (i == 0) acc
    else prev_sumSquares(i - 1, i * i + acc)
  }

  prev_sumSquares(n, 0)
}

sumSquares(5)

// Ex.4: function which computes the sum of all natural numbers within a range. Use two styles to write this function: direct recursion, and tail recursion.
def sumNats (a: Int, b: Int) : Int = {
  if (a > b) 0
  else a + sumNats(a + 1, b)
}

def tailsumNats (start: Int, stop: Int) : Int = {
  def aux_tailsumNats (i: Int, acc: Int) : Int = {
    if (i > stop) acc
    else aux_tailsumNats(i + 1, i + acc)
  }
  aux_tailsumNats(start, 0)
}

sumNats(2, 9)

tailsumNats(2,9)

// Ex.5

def subtractRange(x: Int, start: Int, stop: Int) :Int = {
  if (start > stop) x
  else subtractRange(x - start, start + 1, stop)
}

subtractRange(100, 2, 5)

// Ex.6:
def subtractRange1(x: Int, start: Int, stop: Int): Int = {
  if (stop < start) x
  else subtractRange1(x, start, stop - 1) - stop
}

subtractRange1(2, 5, 12)

// Newton's Square Root Number

def improve(xn: Double, a: Double): Double = {
  (xn + a / xn) / 2
}

def acceptable(xn: Double, a: Double): Boolean = {
  scala.math.abs(xn * xn - a) / a < 0.001
}

def nth_guess(n: Int, a: Double): Double = {
  if (n == 0) 1.0
  else {
    val prevGuess = nth_guess(n - 1, a)
    if (acceptable(prevGuess, a)) prevGuess
    else improve(prevGuess, a)
  }
}

def mySqrt(a: Double): Double = {
  def tailSqrt(estimate: Double): Double = {
    if (acceptable(estimate, a)) estimate
    else tailSqrt(improve(estimate, a))
  }

  tailSqrt(1.0)
}

def acceptableLargeNumbers(xn: Double, a: Double): Boolean = {
  scala.math.abs((xn - a / xn) / xn) < 0.001
}


mySqrt(2e50)

