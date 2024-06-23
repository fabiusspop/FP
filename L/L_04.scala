package L

import scala.annotation.tailrec

object L_04 extends App{

  /*
  -------- Algebraic Datatype Definition --------
  Below you will find the algebraic of the datatype IList:
  Void: IList
  Cons: Int x IList -> IList
   */

  trait IList
  case object Void extends IList
  case class Cons(x: Int, xs: IList) extends IList

  /*
  Consider the following axioms for the operator isEmpty

  isEmpty: IList -> Boolean
  isEmpty(Void) = true
  isEmpty(Cons(h, t)) = false
   */

  // 4.1. implement isEmpty in Scala

  def isEmpty(l: IList): Boolean ={
    l match {
      case Void => true
      case Cons(_, _) => false
    }
  }

  // 4.2. write down axioms for size: IList -> Int and implement the operator in Scala
  def size(l: IList): Int = {
    l match {
      case Void => 0
      case Cons(_, t) => 1 + size(t)
    }
  }

  // 4.3. contains --> check if an element is a member of a list
  def contains(e: Int, l: IList): Boolean = {
    l match {
      case Void => false
      case Cons(h, t) =>
        e == h || contains(e, t)
    }
  }

  // 4.4. max --> returns the largest integer from a list
  def max(l: IList): Int = {
    l match {
      case Cons(h, Void) => h
      case Cons(h, t) => {
        val auxMax = max(t)
        if (h > auxMax) h else auxMax
      }
    }
  }

  // 4.5. take --> returns a new list containing the first n elements of the original list

  def take(n: Int)(l: IList): IList = {
    (n, l) match {
      case (0, _) => Void
      case (_, Void) => Void
      case (n, Cons(h, t)) => Cons(h, take(n - 1)(t))
    }
  }

  // 4.6. drop --> returns a new list containing the original list without the first n elements.
  def drop(n: Int)(l: IList): IList = {
    (n, l) match {
      case (0, _) => Void
      case (_, Void) => Void
      case (n, Cons(_, t)) => drop(n - 1)(t)
    }
  }

  // 4.7. append --> concatenates two lists
  def append(l1: IList, l2: IList): IList = {
    l1 match {
      case Void => l2
      case Cons(h, t) => Cons(h, append(t, l2))
    }
  }

  // 4.8. last --> returns the last element of the list
  def last(l: IList): Int = {
    l match {
      case Void => throw new NoSuchElementException("Empty list")
      case Cons(h, Void) => h
      case Cons(_, t) => last(t)
    }
  }

  // 4.9. reverse --> two ways (direct recursion, tail-end recursion)
  // try both

  def tailReverse(l: IList): IList = {
    @tailrec
    def auxReverse(acc: IList, remaining: IList): IList = {
      remaining match {
        case Void => acc
        case Cons(h, t) => auxReverse(Cons(h, acc), t)
      }
    }
    auxReverse(Void, l)
  }

  def directReverse(l: IList): IList = {
    l match {
      case Void => Void
      case Cons(h, t) => append(directReverse(t), Cons(h, Void))
    }
  }

  // 4.10. isSorted --> checks if a list is sorted
  def isSorted(l: IList): Boolean = {
    l match {
      case Void => true
      case Cons(_, Void) => true
      case Cons(h1, Cons(h2, t)) =>
        if (h1 <= h2) isSorted(Cons(h2, t))
        else false
    }
  }

  // 4.11. merge --> merges two sorted lists
  def merge(l1: IList, l2: IList): IList = {
    (l1, l2) match {
      case (Void, _) => l2
      case (_, Void) => l1
      case (Cons(h1, t1), Cons(h2, t2)) =>
        if (h1 <= h2) Cons(h1, merge(t1, l2))
        else Cons(h2, merge(l1, t2))
    }
  }

  // 4.12. mergeSort --> sorts a list
  def mergeSort(l: IList): IList = {
    l match {
      case Void => l
      case Cons(_, Void) => l
      case _ => {
        val mid = size(l) / 2
        merge(mergeSort(take(mid)(l)), mergeSort(drop(mid)(l)))
      }
    }
  }


}
