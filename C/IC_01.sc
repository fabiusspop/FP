val n = 10
var i = 1

var factorial = 1
while (i < n){
  factorial = factorial * i
  i += 1
}

factorial

// in fp: whenever you want to modify a value from x0 to x1
// imagine writing a function that takes x0 and creates the new value
// f(x0) = x1

def fact(n: Int): Int =
{
  if (n == 0) 1
  else n * fact(n - 1)
}

// given interval [a,b], is there a prime number between a and b?

def primeIn(start: Int, stop: Int): Boolean = {
  def isPrime(n: Int): Boolean = {
    def loop(crt: Int): Boolean = {
      if (crt > n / 2) true
      else if (n % crt == 0) false
      else loop(crt + 1)
    }
    loop(2)
  }

  if (start > stop) false
  else if (isPrime(start)) true
  else primeIn(start + 1, stop)
}

primeIn(4, 8)

// exploring issues

def fibo(n: Int): Int = {
  if (n == 0) 0
  else if (n == 1) 1
  else fibo(n-1) + fibo(n - 2)
}
// --> exponential amount of memory

/*
Write fibo using a loop.
In this signature:
fx is the (n-1)th fibo number WHILE
fy is the (n-2)th fibo number
crt is the current number
 */

def fibo2(n: Int): Int = {
  def loop(fx: Int, fy: Int, crt: Int): Int ={
    if (crt > n) fx
    else loop(fx + fy, fx, crt + 1)
  }
  loop(1, 0, 1)
}

fibo2(10)

// sum of all natural numbers from a given interval
def sumNats(start: Int, stop: Int): Int = {
  def auxSumNats(acc: Int, crt: Int): Int = {
    if (crt > stop) acc
    else auxSumNats(acc + crt, crt + 1)
  }
  auxSumNats(0, start)
}

sumNats(1, 10)

