import scala.annotation.tailrec

object FSets{

  type Set = Int => Boolean

  def profileID: Int = 677945

  def member(e: Int)(s: Set): Boolean = s(e)

  def singleton(x: Int): Set = (e: Int) => e == x

  def ins(x: Int)(s: Set): Set = (e: Int) => s(e) || e == x

  def fromBounds(start: Int, stop: Int): Set = (e: Int) => e >= start && e <= stop

  def union (s1: Set, s2: Set): Set = (e: Int) => s1(e) || s2(e)

  def complement(s1: Set): Set = (e: Int) => !s1(e)

  def sumSet(b: Int)(start: Int, stop: Int)(s: Set): Int = {
    def auxSum(crt: Int, acc: Int): Int = {
      if (crt > stop) acc
      else if (s(crt)) auxSum(crt + 1, acc + crt)
      else auxSum(crt + 1, acc)
    }

    auxSum(start, b)
  }

  def foldLeftSet(b:Int)(op: (Int,Int) => Int)(start: Int, stop: Int)(s: Set): Int = {
    def auxFoldLeftSet(crt: Int, acc: Int): Int = {
      if (crt > stop) acc
      else auxFoldLeftSet(crt + 1, op(acc,  if (s(crt)) crt else 0))
    }

    auxFoldLeftSet(start,b)
  }

  def foldRightSet(b:Int)(op: (Int,Int) => Int)(start: Int, stop: Int)(s: Set): Int = {
    if (start > stop) b
    else op(if (s(start)) start else 0, foldRightSet(b)(op)(start + 1, stop)(s))
  }

  def filter(p: Int => Boolean)(s: Set): Set = (e: Int) => s(e) && p(e)

  // for partition, I supposed I am allowed to use the filter function from above, I hope it s ok
  def partition(p: Int => Boolean)(s: Set): (Set,Set) = (filter(p)(s), filter(e => !p(e))(s))

  def forall(cond: Int => Boolean)(start: Int, stop: Int)(s: Set): Boolean = {
    def anotherForAll(e: Int): Boolean = {
      if (e > stop) true
      else if (s(e) && !cond(e)) false
      else anotherForAll(e + 1)
    }

    anotherForAll(start)
  }

  def exists(cond: Int => Boolean)(start: Int, stop: Int)(s: Set): Boolean = !forall(e => !cond(e))(start, stop)(s)

  //setOfDivByK using another functions???
  def setOfDivByK(k: Int): Set = (e: Int) => e % k == 0

  def moreDivs(k: Int)(start: Int, stop:Int)(s1: Set, s2: Set): Boolean = {
    val countDivs = (acc: Int, e: Int) => if (e != 0 && e % k == 0) acc + 1 else acc
    foldLeftSet(0)(countDivs)(start, stop)(s1) > foldLeftSet(0)(countDivs)(start, stop)(s2)
  }


}
