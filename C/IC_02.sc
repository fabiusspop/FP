import scala.annotation.tailrec

// compute sum of all integers from a range
def sumAll(start: Int, stop: Int): Int = {
  def loop(i: Int, crtSum: Int): Int = {
    if (i > stop) crtSum
    else loop (i + 1, crtSum + i)
  }
  loop(start, 0)
}

def sumSquares(start: Int, stop: Int): Int = {
  def loop (i: Int, crtSum: Int): Int = {
    if (i > stop) crtSum
    else loop(i + 1, crtSum + i*i)
  }
  loop(start, 0)
}

// more general implementation
def sumWithf(f: Int => Int, start: Int, stop: Int): Int = {
  def loop(i: Int, crtSum: Int): Int = {
    if (i > stop) crtSum
    else loop(i + 1, crtSum + f(i))
  }
  loop(start, 0)
}

// how to use summWithf?
def id(x: Int): Int = x
sumWithf(id, 0, 10)

// in scala we can define functions without naming them (like anonymous classes)
// they are called anonymous functions, or shorter "lambdas".

sumWithf((x: Int) => x, 0, 10)
sumWithf(x => x, 0, 10)
sumWithf(x => x, 0, 10)

sumWithf(x => x*x, 0, 10)

def currySumWithf(f: Int => Int): (Int, Int) => Int = {
  def sumWithf(start: Int, stop: Int): Int = {
    def loop(i: Int, acc: Int): Int = {
      if (i > stop) acc
      else loop(i + 1, f(i) + acc)
    }
    loop(start, 0)
  }
  sumWithf
}

val applyAlg1 = currySumWithf(x => x)
val applyAlg2 = currySumWithf(x => x * x)
applyAlg1(0, 10)
applyAlg2(0, 10)

// currySumWithf has long signature and hard to read
def cleanSumWithf(f: Int => Int)(start: Int, stop: Int): Int = {
  @tailrec
  def loop(i: Int, acc: Int): Int = {
    if (i > stop) acc
    else loop(i + 1, f(i) + acc)
  }
  loop(start, 0)
}

//cleanSumWithf(x => x)(0, 10)

// we say that cleanSumWithf takes its parameters IN TURN.
// first takes a function f and returns another function taking two integers

//def compose(f: Int => Int, g: Int => Int): Int => Int
// tracking types can be eased introducing TYPE ALIASES.
type Algorithm = Int => Int

//def compose(f: Algorithm, g: Algorithm): Algorithm ={
//  def comp(x: Int): Int = f(g(x))
//  comp
//}
//
//// IMPROVED
//
//def compose(f: Algorithm, g: Algorithm): Algorithm = {
//  x => f(g(x))
//}

type Reducer = (Int, Int) => Int

//val Alg1and2: Reducer = cleanSumWithf(compose(x => x, x => x * x))
//Alg1and2(0, 10)

// defining lines as functions
// f(x) = y
type Fun2D = Int => Int
// example of a line
val h = (x: Int) => 2 * x + 1

def make2DFun(a: Int)(b: Int): Fun2D =
{
  x => a * x + b
}

def uncurrymake2DFun(x: Int, a: Int, b: Int): Int = a * x + b

// curry function with known number of parameters and uncurries it

def uncurry(cf: Int => Int => Fun2D): (Int, Int, Int) => Int = {
  def aux(x: Int, y: Int, z: Int): Int = cf(x)(y)(z)
  aux
}

//// improve
//def uncurry(cf: Int => Int => Fun2D): (Int, Int, Int) => Int =
//  (x, y, z) => cf(x)(y)(z)

uncurry(make2DFun)(2, 1, 3)

def uncurry2(cf: Int => Int => Int)(x: Int, y: Int): Int =
  cf(x)(y)