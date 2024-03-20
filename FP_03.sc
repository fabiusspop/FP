def sumInt(start:Int, stop:Int): Int = {
  def loop(i: Int, acc: Int): Int = {
    if (i > stop) acc
    else loop(i + 1, acc + i)
  }
  loop(start, 0)
}

sumInt(4,9)

def sumSquares(start:Int, stop:Int): Int = {
  def loop(i: Int, acc: Int): Int = {
    if (i > stop) acc
    else loop(i + 1, acc + i*i)
  }
  loop(start, 0)
}

sumSquares(1,4)

def sumWithf(f: Int => Int, start:Int, stop:Int): Int = {
  def loop(i: Int, acc: Int): Int = {
    if (i > stop) acc
    else loop(i + 1, acc + f(i))
  }
  loop(start, 0)
}

//def id(x: Int): Int = x
//def square(x: Int): Int = x * x
def sumInt(start: Int, stop:Int): Int = sumWithf(x=> x, start, stop)
def sumSquares(start: Int, stop:Int): Int = sumWithf(x => x * x, start, stop)

// higher order function --> can return (other) functions

def currySumWithf(f: Int => Int): (Int, Int) => Int = {
  def sumWithf(start: Int, stop: Int): Int = {
    def loop(i: Int, acc: Int): Int = {
      if (i > stop) acc
      else loop(i + 1, acc + f(i))
    }
    loop(start, 0)
  }
  sumWithf
}

def alg1(x: Int): Int = x
def alg2(x:Int): Int = x * x
def alg3(x:Int): Int = x * x * x

def cleanSumWithf(f: Int => Int)(start: Int, stop: Int): Int = {
  def loop(i: Int, acc: Int): Int = {
    if (i > stop) acc
    else loop(i + 1, acc + f(i))
  }
  loop(start, 0)

}

//cleanSumWithf takes his parameters one by one

def f(x: Int)(y: Boolean)(z: Char): String = x.toString + y.toString + z.toString

// f and g are known as functional closures (inchideri functionale)

val g: Boolean => Char => String = f(1) // is a function that takes a Boolean
val h: Char => String = f(1)(true) // is a function that takes a Char and returns a String

def compose(f: Int => Int, g: Int => Int): Int => Int = {
  def h(x: Int): Int = f(g(x))
  h
}

def compose2(f: Int => Int, g: Int => Int): Int => Int = {
  x => f(g(x))
    // or f(g(x))
}

val l1: Int => Int = x => 2*x + 1

def curryMake2Dline(a: Int)(b: Int): Int => Int = {
  x => a*x + b
}

val l2 = curryMake2Dline(2)(1)

def uncurry(cf: Int => Int => Int): (Int, Int) => Int = {
  (x, y) => cf(x)(y)
}

def uncurry2(cf: Int => Int => Int)(x: Int, y: Int): Int = {
  cf(x)(y)
}


