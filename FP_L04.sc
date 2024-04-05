trait IList
case object Void extends IList
case class Cons(x: Int, xs: IList) extends IList

//Void : IList
//Cons : Int x IList -> IList
//
//isEmpty : IList -> Boolean
//isEmpty(Void) = true
//isEmpty(Cons(h,t)) = false.

// 4.1. isEmpty()
def isEmpty(l:IList): Boolean = l match {
  case Void => true
  case Cons(_, _) => false
}

// 4.2. size()
def size(l: IList): Int = l match {
  case Void => 0
  case Cons(_, xs) => 1 + size(xs)
}

// 4.3. contains()
def contains(e:Int, l: IList): Boolean = l match{
  case Void => false
  case Cons(x, xs) => if (x == e) true else contains(e, xs)
}

// 4.4. max elem
def max(l: IList): Int = l match {
  case Void => 0
  case Cons(x, Void) => x
  case Cons(x, xs) => {
    val m = max(xs)
    if (x > m) x else m
  }
}

// 4.5. list of first n elements
def take (n: Int)(l:IList): IList = (n, l) match {
  case (0, _) => Void
  case (_, Void) => Void
  case (_, Cons(x, xs)) => Cons(x, take(n-1)(xs))
}

// 4.6. dropping the first n elements of a list
def drop(n: Int)(l:IList): IList = (n, l) match {
  case (0, _) => l
  case (_, Void) => Void
  case (_, Cons(_, xs)) => drop(n-1)(xs)
}

// 4.7. concatenation of two lists
def append (l1: IList, l2: IList): IList = l1 match {
  case Void => l2
  case Cons(x, xs) => Cons(x, append(xs, l2))
}

// 4.8. returning the last element from a list
def last(l: IList): Int = l match {
  case Void => 0
  case Cons(x, Void) => x
  case Cons(_, xs) => last(xs)
}

// 4.9. reversing a list - two ways: direct & tail-end

// 4.9.1. direct
def reverseDirect(l: IList): IList = l match {
  case Void => Void
  case Cons(x, xs) => append(reverseDirect(xs), Cons(x, Void))
}

// 4.9.2. tail-end
def reverseTailEnd(l: IList): IList = {
  def reverseTailEndAcc(l: IList, acc: IList): IList = l match {
    case Void => acc
    case Cons(x, xs) => reverseTailEndAcc(xs, Cons(x, acc))
  }
  reverseTailEndAcc(Void, l)
}

// 4.10. checking if a list is sorted
def isSorted(l:IList): Boolean = l match {
  vase Void => true
  case Cons(_, Void) => true
  case Cons(x1, Cons(x2, _)) if x1 > x2 => false
  case Cons(_, xs) => isSorted(xs)
}

// 4.11. merging two sorted lists
def merge(l1: IList, l2: IList): IList = (l1, l2) match{
  case (Void, _) => l2
  case (_, Void) => l1
  case (Cons(x1, xs1), Cons(x2, xs2)) => {
    if (x1 < x2) Cons(x1, merge(xs1, l2))
    else Cons(x2, merge(l1, xs2))
  }
}

// 4.12. sorting a list using mergeSort() - not working
//def mergeSort(l: IList): IList = l match {
//  case Void => Void
//  case Cons(_, Void) => l
//  case _ => {
//    val (l1, l2) = split(l)
//    merge(mergeSort(l1), mergeSort(l2))
//  }
//}
