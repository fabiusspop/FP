trait Nat {
  def isZero: Boolean
  def add(other: Nat): Nat
  def subtract(other: Nat): Nat
  def greater(other: Nat): Boolean
  def toInt: Int
}

case object Zero extends Nat {
  def isZero: Boolean = true
  def add(other: Nat): Nat = other
  def subtract(other: Nat): Nat = if (other.isZero) this else throw new IllegalArgumentException("Negative number")
  def greater(other: Nat): Boolean = false
  def toInt: Int = 0
}

case class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false
  def add(other: Nat): Nat = new Succ(n.add(other))
  def subtract(other: Nat): Nat = if (other.isZero) this else n.subtract(other.asInstanceOf[Succ].n)
  def greater(other: Nat): Boolean = other match {
    case Zero => true
    case Succ(m) => n.greater(m)
  }
  def toInt: Int = 1 + n.toInt
}

val zero = Zero
val one = Succ(zero)
val two = Succ(one)
val three = Succ(two)

println(zero.isZero)
println(one.isZero)

println(zero.add(one))
println(one.add(one))

println(two.subtract(one))
println(three.subtract(two))


println(two.greater(one))
println(one.greater(two))

println(zero.toInt == 0)
println(one.toInt == 1)
println(two.toInt == 2)

trait OList {

  def head: Int

  def tail: OList

  def foldRight[B](acc: B)(op: (Int, B) => B): B

  def foldLeft[B](acc: B)(op: (B, Int) => B): B

  def indexOf(i: Int): Int

  def filter(p: Int => Boolean): OList

  def map(f: Int => Int): OList

  def partition(p: Int => Boolean): (OList, OList)

  def slice(start: Int, stop: Int): OList

  def forall(p: Int => Boolean): Boolean
}

case object emptyList extends OList {

  def head: Int = ???

  def tail: OList = ???

  def foldRight[B](acc: B)(op: (Int,B) => B): B = acc

  def foldLeft[B](acc: B)(op: (B,Int) => B): B = acc

  def indexOf(i: Int): Int = -1

  def filter(p: Int => Boolean): OList = this

  def map(f: Int => Int): OList = this

  def partition(p: Int => Boolean): (OList, OList) = (this, this)

  def slice(start: Int, stop: Int): OList = this

  def forall(p: Int => Boolean): Boolean = true
}

case class Cons(head: Int, tail: OList) extends OList {

  def foldRight[B](acc: B)(op: (Int,B) => B): B = op(head, tail.foldRight(acc)(op))

  def foldLeft[B](acc: B)(op: (B,Int) => B): B = tail.foldLeft(op(acc, head))(op)

  def indexOf(i: Int): Int = {
    if (head == i) 0
    else 1 + tail.indexOf(i)
  }

  def filter(p: Int => Boolean): OList = {
    if (p(head)) Cons(head, tail.filter(p))
    else tail.filter(p)
  }

  def map(f: Int => Int): OList = Cons(f(head), tail.map(f))

  def partition(p: Int => Boolean): (OList, OList) = {

    val (l1, l2) = tail.partition(p)
    if (p(head)) (Cons(head, l1), l2) else (l1, Cons(head, l2))

  }

  def slice(start: Int, stop: Int): OList = {
    if (start > 0) tail.slice(start - 1, stop - 1)
    else if (stop > 0) Cons(head, tail.slice(0, stop - 1))
    else emptyList
  }

  def forall(p: Int => Boolean): Boolean = p(head) && tail.forall(p)

}

val empty = emptyList
val single = Cons(1, emptyList)
val multiple = Cons(1, Cons(2, Cons(3, emptyList)))

//println(empty.head)
//
//println(empty.tail)
//
//println(single.head)
//println(single.tail == emptyList)
//
//println(multiple.head)
//println(multiple.tail)
//
//println(empty.foldRight(0)(_ + _))
//println(empty.foldLeft(0)(_ + _))
//
//println(empty.indexOf(1))
//println(single.indexOf(1))
//
//println(empty.filter(_ % 2 == 0))
//println(single.filter(_ % 2 == 0))
//println(multiple.filter(_ % 2 == 0))
//
//println(empty.map(_ * 2))
//println(single.map(_ * 2))
//println(multiple.map(_ * 2))
//
//println(empty.partition(_ % 2 == 0))
//println(single.partition(_ % 2 == 0))
//
//println(empty.slice(0, 1))
//println(single.slice(0, 1))
//
//println(empty.forall(_ % 2 == 0))
//println(single.forall(_ % 2 != 0))
//println(!multiple.forall(_ % 2 == 0))