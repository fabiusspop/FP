
// 3.1

def foldWith(b: Int)(op: (Int,Int) => Int)(start: Int, stop: Int): Int = {
  def tail_fold(crt: Int, acc: Int): Int  = {
    if (crt > stop) acc
    else tail_fold(crt + 1, op(acc, crt))
  }
  tail_fold(start, b)
}

foldWith(3)((a: Int, b: Int) => a + b)(1, 3)

// 3.2

def foldConditional(b: Int)(op: (Int,Int) => Int, p: Int => Boolean)(start: Int, stop: Int): Int = {
  def tail_fold(crt: Int, acc: Int): Int  = {
    if (crt > stop) acc
    else tail_fold(crt + 1, if (p(crt)) op(acc, crt) else acc)
  }
  tail_fold(start, b)
}

foldConditional(0)((a: Int, b: Int) => a + b, (x: Int) => x % 2 == 0)(1, 6)

// 3.3

def foldRight(b: Int)(op: (Int,Int) => Int)(start: Int, stop: Int): Int = {
  if (start > stop) b
  else op(start, foldRight(b)(op)(start + 1, stop))
}

foldRight(0)((a: Int, b: Int) => a + b)(1, 3)

// 3.4

def foldMap(op: (Int,Int) => Int, f: Int => Int)(start: Int, stop: Int): Int = {
  if (start > stop) 0
  else op(f(start), foldMap(op, f)(start + 1, stop))
}

foldMap((a: Int, b: Int) => a + b, (x: Int) => x)(1, 3)


// 3.5

def sumSquares(n: Int): Int = {
  foldMap((a: Int, b: Int) => a + b, (x: Int) => x * x)(1, n)
}

sumSquares(3)

// 3.6

def hasDivisor(k: Int, start: Int, stop: Int): Boolean = foldMap((a: Int, b: Int) => a | b, (x: Int) => if (x % k == 0) 1 else 0)(start, stop) != 0

hasDivisor(2, 5, 9)

// 3.7: condition --> smaller than 1

def integrate(f: Double => Double)(start: Double, stop: Double): Double = {
  if (Math.abs(stop - start) < 1) (f(start) + f(stop)) * (stop - start) / 2
  else {
    val mid = (start + stop) / 2
    integrate(f)(start, mid) + integrate(f)(mid, stop)
  }
}

integrate((x: Double) => x * x)(0, 1)

// 3.8

//Defining a 2d line for 3.8, 3.9, 3.10, 3.11

type Line2D = Int => Int

def translateOx(offset: Int)(l: Line2D): Line2D = {
  x => l(x) + offset
}

translateOx(3)((x: Int) => x)(1)

// 3.9

def translateOy(offset: Int)(l: Line2D): Line2D = {
  x => l(x + offset)
}

translateOy(3)((x: Int) => x)(1)

// 3.10

def intersect(l1: Line2D, l2: Line2D)(start: Int, stop: Int): Boolean = {
  def loop(x: Int): Boolean = {
    if (x > stop) false
    else if (l1(x) == l2(x)) true
    else loop(x + 1)
  }
  loop(start)
}

val line1: Line2D = x => x
val line2: Line2D = x => x + 3
val start = 1
val stop = 5

val result = intersect(line1, line2)(start, stop)

// 3.11

def larger(l1: Line2D, l2: Line2D)(start: Int, stop: Int): Boolean = {
  def loop(x: Int): Boolean = {
    if (x > stop) true
    else if (l1(x) < l2(x)) false
    else loop(x + 1)
  }
  loop(start)
}


val result2 = larger(line1, line2)(start, stop)





