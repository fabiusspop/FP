import scala.annotation.targetName

/*
  1) Infix functions
  2) Polymorphism - examples

  the Option type
  implicits

 */

// Consider the Nat class:

trait Nat
case object Zero extends Nat
case class Succ(n: Nat) extends Nat

def add(x: Nat, y: Nat): Nat = {
  x match {
    case Zero => y
    case Succ(xp) => Succ(add(xp, y))
  }
}

extension (n: Nat) {
  def +(other: Nat): Nat = add(n, other)
}

/*
  Suppose we would like to add Nats more elegantly: by using infix operations
  This is easily possible in Scala, because a subset of characters can be used as function names
  a) We can add the operation + to the trait Nat, to have it as member function.
  b) In Scala, we can add method implementations in traits
  c) In scala, it is possible to call methods such as:
      obj.func(param)
        using the following syntax:
          obj func param
 */

//Succ(Zero) + Succ(Zero)

// Suppose we want to create Nats from different sources:

def fromInt(i: Int): Nat = {
  if (i == 0) Zero
  else Succ(fromInt(i - 1))
}
//fromInt(3)

/*
  Let us introduce the function "andThen", which perform similarly to composition,
  but reversing the order:
  f.andThen(g) will behave on x in the same way as f(g(x))
  which makes code writing more elegant sometimes:
 */

def fromString: String => Nat = {
  ((s: String) => s.toInt) andThen fromInt
}

def fromBoolean(b: Boolean): Nat = {
  if b then Succ(Zero)
  else Zero
}

/*
  Keeping track of all possible construction rules and their logic is
        tedious in the long run.
  There is a simple way to solve this,
  by using COMPANION objects.

  A companion object is just like a normal object.
  However, "inside it" we can define the method "apply".
  Since the Nat object is not a class,
  it does NOT have constructors. However,
  apply implementations allow us to "call" an object:
 */

object Nat {
  def apply(i: Int): Nat = {
    if (i == 0) Zero
    else Succ(Nat(i - 1))
  }

  def apply(s: String): Nat = {
    Nat(s.toInt)
  }

  def apply(b: Boolean): Nat = {
    if (b) Succ(Zero)
    else Zero
  }
}

Nat(1) + Nat("2") + Nat(true)

// Polymorphism
// What is polymorphism?
// We are often using polymorphic lists:
val l: List[Int] = List(1, 2, 3)

/*
And more than often we want to
define operations that behave in the same way,
irrespective of the type of the list.
 */

def size[A](l: List[A]): Int = {
  l match {
    case Nil => 0
    case _ :: xs => 1 + size(xs)
  }
}

def contains[A](x: A, l: List[A]): Boolean = {
  l match {
    case Nil => false
    case y :: ys =>
      x == y || contains(x, ys)
  }
}

/*
  Let us define a few POLYMORPHIC DATATYPES.
  We shall first start with the type "Maybe",
  which will be designed in order to keep "default" values,
  as well as a special "Error" value.
  In order to be able to keep any such possible value,
  "Maybe" will be polymorphic.
 */

trait Maybe[+A]

case object Error extends Maybe[Nothing]
case class Just[A](value: A) extends Maybe[A]

/*
Let us first get used to using values Maybe,
by extending our Nat example
 */

def fromInteger(i: Int): Maybe[Nat] = {
  def aux(i: Int): Nat = {
    if (i == 0) Zero
    else Succ(aux(i - 1))
  }
  if (i < 0) Error
  else Just(aux(i))
}

extension(n: Nat) {
  def +(other: Maybe[Nat]): Maybe[Nat] = {
    (n, other) match {
      case (Zero, Just(_)) => other
      case (Succ(np), Just(_)) =>
        {
          np + other match {
            case Just(result) => Just(Succ(result))
            case _ => Error
          }
        }
      case _ => Error
    }
  }
}

//def add(n: Maybe[Nat], m: Maybe[Nat]): Maybe[Nat] = {
//  (n, m) match {
//    case (Just(Zero), Just(_)) => m
//    case (Just(Succ(np)), Just(_)) =>
//      add(Just(np), m) match {
//        case Just(result) => Just(Succ(result))
//        case _ => Error
//      }
//    case _ => Error
//  }
//}

Zero + fromInteger(2)

/*
  As one may notice, adding may become slightly difficult to implement using Maybe,
  and there is one design pattern that we will explore later in the lecture,
  which deals with this case more elegantly.
  Discussion regarding the definition of Maybe:

  a) What is the type of:
    val l = List(Just(1), Error)

  We would like this type to be List[Maybe[Int]]
  We see that Just(1) has type Maybe[Int]

  Hence, we would like that type of Error be in some sense,
  compatible with Maybe[Int]

  More precisely, Error: Maybe[t], where t is compatible with Int.

  Now, note that t has to be compatible with any other type we might put in maybe boxes
  In Scala, we have the type Nothing which implicitly
    is a subtype of any other built-in or user-defined type.

  Now we have that Nothing "subtype" Int. However, do we also have:
  Maybe[Nothing] "subtype" Maybe[Int]?
  This is not generally the case in other programming languages such as Java,
  however, in Scala, we have TYPE COVARIANCE. Plus "+" makes Maybe covariant.

  In Scala there is already the type "Option" which behaves in precisely the same way as Maybe.
 */

  class X(x: Int)
object X {
  def apply(i: Int): X = new X(i)
  def apply(s: String): X = X(s.toInt)
  def apply(c: Char): X = new X('a')
 }

X('a')