class InteractiveCourse01 extends App{

//  val n = 10
//
//  var factorial = 1
//
//  while (i < n){
//    factorial = factorial * i
//    i = i + 1
//  }

  /* var (can change in time) vs val (constant) */

  /* Style 1. In functional programming, values do not change in time.
     Side-effects = programming with changes (like var)
     In functional programming, we avoid side-effects.
     Why?
     a) with side effects, code is more prone to bugs
     b) code is less legible

     How do we program without side-effects?
     Recursive functions
   */

//  def fact(n: Int): Int = {
//    if (n == 0) 1
//    else n * fact(n - 1)
//  }

  /* What does this function return?
    in Scala, each working piece of code is an expression

   */

  // We are given with an interval [a,b] and we want to check if there is a prime number between a and b.
//  def isPrime(n: Int): Boolean = {
  ////    if (n <= 1) false
  ////    else {
  ////      var i = 2
  ////      var prime = true
  ////      while (i < n){
  ////        if (n % i == 0) prime = false
  ////        i = i + 1
  ////      }
  ////      prime
  ////    }
  ////  }
  ////
  ////  val a = 10
  ////  val b = 20
  ////  var i = a
  ////  var found: Boolean = false
  ////
  ////  while (i <= b)
  ////  {
  ////    if (isPrime(i))
  ////    {
  ////      found = true
  ////    }
  ////    i = i + 1
  ////  }
  ////
  ////  println(found)

  // function that computes fibonnaci number

def fib(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else fib(n - 1) + fib(n - 2)
  }
  fib(7)





}
