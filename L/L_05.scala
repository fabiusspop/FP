package L

import L.L_03.foldRight
import L.L_04.{Cons, IList, Void}

object L_05 extends App{
  // Cd the following type defined to represent natural numbers:

  trait Nat {
    def isZero: Boolean
    def add(other: Nat): Nat
    def subtract(other: Nat): Nat
    def greater(other: Nat): Boolean
    def toInt: Int
  }

  // When implementing the following methods, think about whether or not they are LOCAL
  // Are they best implemented using functional or OO decomposition?

  case object Zero extends Nat {
    override def isZero: Boolean = true
    override def add(other: Nat): Nat = other
    override def subtract(other: Nat): Nat = Zero
    override def greater(other: Nat): Boolean = false
    override def toInt: Int = 0
  }
  case class Succ(n: Nat) extends Nat {
    override def isZero: Boolean = false
    override def add(other: Nat) = {
      Succ(n.add(other))
    }
    override def subtract(other: Nat) = {
      other match {
        case Zero => this
        case Succ(m) => n.subtract(m)
      }
    }
    override def greater(other: Nat): Boolean = {
      other match {
        case Zero => true
        case Succ(m) => n.greater(m)
      }
    }
    override def toInt: Int = 1 + n.toInt
  }

  /*
  The type O-List (Object-Oriented implementation of lists)
  Start with the following trait, which encodes lists over integers
   */

  trait OList {
    def head: Int
    def tail: OList
    def foldRight[B](acc: B)(op: (Int, B) => B): B
    def foldLeft[B](acc: B)(op: (B,Int) => B): B
    def indexOf(i: Int): Int
    def filter(p: Int => Boolean): OList
    def map(f: Int => Int): OList
    def partition(p: Int => Boolean): (OList, OList)
    def slice(start: Int, stop: Int): OList
    def size: Int = 0
    def forall(p: Int => Boolean): Boolean
  }

  case object OEmpty extends OList {
    def head: Int = throw new NoSuchElementException("empty head")
    def tail: OList = throw new UnsupportedOperationException("empty tail")
    def foldRight[B](acc: B)(op: (Int, B) => B): B = acc
    def foldLeft[B](acc: B)(op: (B, Int) => B): B = acc
    def indexOf(i: Int): Int = -1
    def filter(p: Int => Boolean): OList = this
    def map(f: Int => Int): OList = this
    def partition(p: Int => Boolean): (OList, OList) = (this, this)
    def slice(start: Int, stop: Int): OList = this
    def forall(p: Int => Boolean): Boolean = true


  }

  case class OCons(head: Int, tail: OList) extends OList {
    def foldRight[B](acc: B)(op: (Int, B) => B): B = {
      op(head, tail.foldRight(acc)(op))
    }
    def foldLeft[B](acc: B)(op: (B, Int) => B): B = {
      tail.foldLeft(op(acc, head))(op)
    }
    def filter(p: Int => Boolean): OList = {
      foldRight(OEmpty: OList)((elem, acc) =>
        if (p(elem)) OCons(elem, acc)
        else acc
      )
    }
    def indexOf(i: Int): Int = {
      def tailIndexOf(crt: OList, index: Int): Int = {
        crt match {
          case OEmpty => -1
          case OCons(h, t) => {
            if (h == i) index
            else tailIndexOf(t, index + 1)
          }
        }
      }
      tailIndexOf(this, 0)
    }

    def map(f: Int => Int): OList = {
      def auxMap(crtList: OList): OList = {
        crtList match {
          case OEmpty => crtList
          case OCons(h, t) =>
            OCons(f(h), auxMap(t))
        }
      }
      auxMap(this)
    }

    // OR
    //    def map(f: Int => Int): OList = {
    //      foldRight(OEmpty: OList)((elem, acc) => OCons(f(elem), acc))
    //    }

    def partition(p: Int => Boolean): (OList, OList) = {
      (this.filter(p), this.filter(e => !p(e)))
    }

    //    def slice(start: Int, stop: Int): OList = {
    //      this.drop(start).take(stop - start)
    //    }

    def slice(start: Int, stop: Int): OList = {
      if (start > 0) tail.slice(start - 1, stop - 1)
      else if (stop <= 0) OEmpty
      else OCons(head, tail.slice(start, stop - 1))
    }

    override def size: Int = 1 + tail.size

    def forall(p: Int => Boolean): Boolean = {
      if (this.size == this.filter(p).size) true
      else false
    }

  }


}
