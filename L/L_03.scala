package L

object L_03 extends App {

  /*
  OBJECTIVES:
  - implement and use HIGHER-ORDER functions. A higher-order function takes other functions as parameter or return them
  - implement CURRY and UNCURRY functions.
   */

  // 3.1. foldWith - uses an operation "op", initial value "b" to reduce a range of integers to a value
  // ( ( 0 + 1 ) + 2 ) + 3 = 6
  // foldWith should be curried (it will take the operation and return another function which expects the bounds

  def foldWith(b: Int)(op: (Int, Int) => Int)(start: Int, stop: Int): Int = {
    def tail_fold(crt: Int, acc: Int): Int = {
      if (crt > stop) acc
      else tail_fold(crt + 1, op(acc, crt))
    }

    tail_fold(start, b)
  }

  //  println(foldWith(0)(_ + _)(1, 3))

  // 3.2. foldConditional -> extends foldWith by also adding a predicate p: Int -> Int
  // foldConditional will reduce only those elements of a range which satisfy the predicate

  def foldConditional(b: Int)(op: (Int, Int) => Int, p: Int => Boolean)(start: Int, stop: Int): Int = {
    def auxFoldConditional(crt: Int, acc: Int): Int = {
      if (crt > stop) acc
      else if (p(crt)) auxFoldConditional(crt + 1, op(acc, crt))
      else auxFoldConditional(crt + 1, acc)
    }

    auxFoldConditional(start, b)
  }

  //  println(foldConditional(0)((_ + _), _ % 2 == 0)(0, 6))

  // 3.3. foldRight -> same behaviour as foldWith
  // BUT the order in which the operation is performed is now:
  // 1 + (2 + (3 + 0) ) = 6

  def foldRight(b: Int)(op: (Int, Int) => Int)(start: Int, stop: Int): Int = {
    if (start > stop) b
    else op(start, foldRight(b)(op)(start + 1, stop))
  }

  //  println(foldRight(0)(_ + _)(1, 3))

  // 3.4. foldMap --> takes values a1, a2, ..., ak from a range and computes f(a1) op f(a2) op ... f(ak)

  def foldMap(op: (Int, Int) => Int, f: Int => Int)(start: Int, stop: Int): Int = {
    def auxFoldMap(crt: Int, acc: Int): Int = {
      if (crt > stop) acc
      else auxFoldMap(crt + 1, op(f(crt), acc))
    }
    auxFoldMap(start, 0)
  }

  // OR

  //  def foldMap(op: (Int, Int) => Int, f: Int => Int)(start: Int, stop: Int): Int = {
  //    if (start > stop) 0
  //    else op(f(start), foldMap(op, f)(start + 1, stop))
  //  }

  //  println(foldMap((_ + _), _ * 2)(1, 3))

  // 3.5 write a function which computes
  // 1 + 2^2 + 3^2 + ... + (n-1)^2 + n^2 ---> using foldMap

  def sumSquares(n: Int): Int = {
    foldMap(_ + _, (x: Int) => x * x)(1, n)
  }

  //  println(sumSquares(5))

  // 3.6. hasDivisor --> check if a range contains a multiple of k.
  // use foldMap and choose f carefully

  def hasDivisor(k: Int, start: Int, stop: Int): Boolean = {
    def foldMap(op: (Boolean, Boolean) => Boolean, f: Int => Boolean)(start: Int, stop: Int): Boolean = {
      if (start > stop) false
      else op(f(start), foldMap(op, f)(start + 1, stop))
    }

    foldMap(_ || _, (x: Int) => x % k == 0)(start, stop)
  }

  //  println(hasDivisor(3, 1, 6))

  /*
  we can compute the sum of an area defined by a function within a range [a, b] (the integral of that function given the range)
  using the following recursive scheme:
  - if the range is small enough, we treat f as a line (and the area as a trapeze).
  it's area is ( f(a) + f(b) ) ( b - a ) / 2
  - otherwise, we compute the mid of the range,
               we recursively compute the integral from a to mid and from mid to b, and add-up the result.
   */

  // 3.7. integrate --> computes the integral of a function f given a range

  def integrate(f: Double => Double)(start: Double, stop: Double): Double = {
    if (stop - start < 0.001){
      (f(start) + f(stop)) * (stop - start) / 2
    } else {
      val mid = start + (stop - start) / 2
      integrate(f)(start, mid) + integrate(f)(mid, stop)
    }
  }

  //  println(integrate(x => x * x)(0.0, 1.0))

  // 3.8. Line2D --> lines in a 2D space, represented as functions.
  // write a function which takes a Line2D and translates it up on the Ox axis by a given offset
  // for instance: translateOx of 2 on y = x + 1 will return y = x + 3

  type Line2D = Int => Int
  def translateOx(offset: Int)(l: Line2D): Line2D = {
    x => l(x) + offset
  }

  //  val line: Line2D = x => x + 1
  //  val translatedLine = translateOx(2)(line)
  //
  //  println(translatedLine(0))
  //  println(translatedLine(1))
  //  println(translatedLine(2))

  // 3.9. translateOy --> takes a Line2D and translates it up on the Oy axis by a given offset.
  def translateOy(offset: Int)(l: Line2D): Line2D = {
    x => l(x + offset)
  }

  // 3.10. intersect -> takes two lines,
  // checks if there exist integer coordinates within a range for x, where the lines intersect

  def intersect(l1: Line2D, l2: Line2D)(start: Int, stop: Int): Boolean = {
    def auxIntersect(crt: Int): Boolean = {
      if (crt > stop) false
      else if (l1(crt) == l2(crt)) true
      else auxIntersect(crt + 1)
    }

    auxIntersect(start)
  }

  // OR
  def intersect1(l1: Line2D, l2: Line2D)(start: Int, stop: Int): Boolean = {
    (start to stop).exists(x => l1(x) == l2(x))
  }

  //  val line1: Line2D = x => x + 2
  //  val line2: Line2D = x => x + 4
  //
  //  println(intersect(line1, line2)(0, 10))

  // 3.11. larger --> takes two lines and a range of integers
  // checks if l1 has larger y values than l2 over the entire range

  def larger(l1: Line2D, l2: Line2D)(start: Int, stop: Int): Boolean = {
    def auxLarger(crt: Int): Boolean = {
      if (crt > stop) true
      else if (l1(crt) < l2(crt)) false
      else auxLarger(crt + 1)
    }
    auxLarger(start)
  }

  // OR
  def larger2(l1: Line2D, l2: Line2D)(start: Int, stop: Int): Boolean = {
    (start to stop).forall(x => l1(x) >= l2(x))
  }


}
