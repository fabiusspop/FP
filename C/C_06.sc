/*1. Infix functions */

trait Nat {
  // in Scala we can define a certain subset of "special characters"
  // to be function names
  def +(other: Option[Nat]): Option[Nat]
}
case object Zero extends Nat {
  def +(other: Option[Nat]): Option[Nat] = other
}
case class Succ(n: Nat) extends Nat {
  // In Scala, there is a shorthand for calling methods:
  // object.method(param)
  // the previous call can rewritten as:
  // object method param
  def +(other: Option[Nat]): Option[Nat] =
    // n: Nat
    // other: Option[Nat]
    // (n + other): Option[Nat]
    n + other match {
      case None => None
      case Some(result) => Some(Succ(result))
    }
}

def add(n: Nat, m: Nat): Nat =
  n match {
    case Zero => m
    case Succ(np) => Succ(add(np,m))
  }

/* Objective 1: can we write infix functions in Scala ?*/
//Succ(Zero) + Succ(Zero)

List(1,2,4) map (_*2)

/* Objective 2: Suppose we want to create Nat values from different "sources" */

def fromInt(i: Int): Nat =
  if (i == 0) Zero
  else Succ(fromInt(i-1))

// We can also define this more functional:
// Suppose f and g are two functions
// In Scala f.andThen(g) behaves on value x the same way as:
// f(g(x))
def fromString: String => Nat =
  ((s: String) => s.toInt) andThen fromInt

def fromBoolean(b: Boolean): Nat =
  if (b) Succ(Zero) else Zero

// Problem: very quickly we have new function names that difficult to keep track of /
// different "algorithms" for constructing Nat values

//fromInt(1) + fromString("1") + fromBoolean(true)

/* a) _Companion_ objects: a companion object to a class or trait
*  C/T, is an object with the same name as C or T.
*  b) the method apply: once defined, this method allows "calling" the object, with various parameters:
*  */
object Nat {
  def apply(): Option[Nat] = Some(Zero)
  def apply(i: Int): Option[Nat] = {
    def aux(i: Int): Nat =
      if (i == 0) Zero
      else Succ(aux(i - 1))

    if (i < 0) None // not a Nat
    else Some(aux(i))
  }

  def apply(s: String): Option[Nat] = Nat(s.toInt)
  def apply(b: Boolean): Option[Nat] =
    Some(if (b) Succ(Zero) else Zero)
}

Nat() // this is a call of the apply method in the object Nat.
// Can this call be mistaken with the "Nat trait" call ?
// no because traits cannot be called.

class C()

object C {
  def apply(): C = ???
}

C() // can this be mistaken with a constructor call for class C?
// the answer is "no", because constructors are called with "new"

new C // this is a constructor call.

/* Remark: Case class constructors are implemented using apply */
// also - similar to overloading constructors
//Nat(1) + Nat("1") + Nat(true)

/* Objective 3: Add error-handling to the Nat operations */

// a) the type Option, with a few examples:

val o1: Option[Int] = Some(1)
val o2 = None

// unboxing of o1
o1 match {
  case Some(x) => x
  case None => 0
}

// + : Nat -> Option[Nat] -> Option[Nat]
// +' : Option[Nat] -> Option[Nat] -> Option[Nat]
Zero + Nat("-1")

/* Polimorfism parametric / Genericitate in Scala */

val l: List[Int] = List(1,2,3)

// size este parametrizata in raport cu tipul "A"
def size[A](l: List[A]): Int =
  l match {
    case Nil => 0
    case _ :: xs => 1 + size(xs)
  }

def contains[A](e: A, l: List[A]): Boolean =
  l match {
    case Nil => false
    case y :: ys => (y == e) || contains(e,ys)
  }

/* Let us define a datatype that is similar to Option */

trait Maybe[+A] // the type constructor Maybe is covariant in the type parameter A. Automatically, if C is a subtype of B, then Maybe[C] is a subtype of Maybe[B].
case object Error extends Maybe[Nothing]
// in Scala, we cannot have polymorphic values (objects)
// no type variables allowed ^^^

case class Just[A](value: A) extends Maybe[A]

val l2: List[Option[Int]] = List(Some(1), None)
// what you prefer as the type of l2 ?
// Option[Int]
// then, Some(1) : Option[Int]
// and also None : Option[Nothing]
/*
    the type t1 has to be "compatible" with Int
    t1 has to be a subtype of Int
    t1 ....      a subtype of Nat
                              other types as well.

    In Scala there is one such type (builtin) and it is called
    Nothing. By default, Nothing is a subtype of any other type.

    We know that:
    Nothing is a subtype of Int.

    Option[Nothing] is a subtype of Option[Int] ?

    Cat is a subtype of Animal
    List[Cat] is a subtype of List[Animal] in Java?
    the answer is NO.

    In particular, if we were to implement Maybe/Option in Java,
    Option[Nothing] is not a subtype of Option[Int] (in Java)

    But would it be nice? to have this type relation?
    Very nice.
    We want to "define" this relationship ^^^

    Now! Since Option is covariant, then:
    Option[Nothing] is a subtype of Option[Int],
*/