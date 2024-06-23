// Let us review some of the new ideas from the previous lectures
trait Nat

case object Zero extends Nat
case class Succ(x: Nat) extends Nat

// FUNCTIONAL DECOMPOSITION

def isZero(x: Nat): Boolean = {
  x match {
    case Zero => true
    case Succ(_) => false
  }
}

def add(x: Nat, y: Nat): Nat = {
  x match {
    case Zero => y
    case Succ(xp) => Succ(add(xp, y))
  }
}

def toInteger(x: Nat): Int = {
  x match {
    case Zero => 0
    case Succ(xp) => 1 + toInteger(xp)
  }
}

//add(Succ(Zero), Succ(Zero))

/*
  This style of using objects:
  i) uses constructors to create new objects
  ii) relies on functions that take objects as parameter and "decompose"
          such objects using "match"
  This style is called * FUNCTIONAL DECOMPOSITION *.
  It is a strategy to handle data in your program.

  There is an alternative style, called "OBJECT-ORIENTED DECOMPOSITION",
      which we illustrate below
      (using "SIZE" and "APPEND")

 */

trait FNat {
  def isZero: Boolean
  def add(other: FNat): FNat
  def toInteger: Int
}

case object FZero extends FNat {
  override def isZero = true
  override def add(other: FNat): FNat = other
  override def toInteger: Int = 0
}

case class FSucc(n: FNat) extends FNat {
  override def isZero = false
  override def add(other: FNat): FNat = FSucc(n.add(other))
  override def toInteger: Int = 1 + n.toInteger
}

/*
  Which is better, F or OO decomposition?

  - OO decomposition works really well if the trait for our objects are well defined,
    BUT the object structure may change.
    In the case of lists, the set of list operations is well-defined:
      we want to append, reverse, count, sort, etc lists.
    The object structure may change if we decide to specialise lists
          (e.g.: by creating circular lists or buffers, etc)


  - Functional decomposition would not work as well,
      since each update on the object structure would require
        major rewriting of existing functions (each "match" expression)

  - F-decomposition works really well when the object-structure is fixed and rigid,
                  as in the case for Nat
    Zero and Succ are the only ways to create Nats,
      and we do not anticipate other subtypes or specialisations for Nat.
    This means we can freely add functionalities over nats (add, toInteger) knowing that
        a Nat can only be a Zero or a Succ.

  - Object decomposition would not work as well for Nat.
    Adding a new functionality would require rewriting the trait,
      as well as each case class where the functionality is added.
 */

/*
  Let us now take the case of lists.
  In Scala, list structure is examined via Functional Decomposition (match).
  However, most methods implemented over lists are defined as METHODS,
    since they are rigid, they do not change:
 */

trait IList {
  def size: Int
  def append(l: IList): IList
  def take(n: Int): IList
  def drop(n: Int): IList
  def merge(l: IList): IList
  def mergeSort: IList
}

case object Void extends IList {
  override def size: Int = 0
  override def append(l: IList): IList = l
  override def take(n: Int): IList = Void
  override def drop(n: Int): IList = Void
  override def merge(l: IList): IList = l
  override def mergeSort: IList = Void
}

case class Cons(h: Int, t: IList) extends IList {
  override def size: Int = 1 + t.size

  override def append(l: IList): IList = Cons (h, l.append(t))

  override def take(n: Int): IList = {
    if (n == 0) Void
    else Cons(h, t.take(n - 1))
  }

  override def drop(n: Int): IList = {
    if (n == 0) this
    else t.drop(n - 1)
  }

  override def merge(l: IList): IList = {
    l match {
      case Void => this
      case Cons(x, xs) =>
        if (x > h) Cons(h, t.merge(l))
        else Cons(x, this.merge(xs))
    }
  }

  override def mergeSort: IList = {
    t match {
      case Void => this
      case _ => {
        val mid = this.size / 2
        val l = this.take(mid).mergeSort
        val r = this.drop(mid).mergeSort
        l.merge(r)
      }
    }
  }
}

/*
  Note that, although we have implemented OO decomposition for lists,
    we have not broken in any way the functional programming principles:
      there are NO SIDE-EFFECTS.
      each function creates new values instead of modifying existing ones.

  Which type of decomposition would you prefer for mergesort?
  Note that it is also possible to blend between the two types of decompositions.
  OO also works well when operations are _local_.
  For instance, merge is not local.
  It requires DECOMPOSING the parameter list as well (which we ve done functionally)

  Mergesort using functional decomposition is much cleaner and legible.
 */

def mergeSort(l: IList): IList = {
  def merge(l1: IList, l2: IList): IList = {
    (l1, l2) match {
      case (Void, _) => l2
      case (_, Void) => l1
      case (Cons(x, xs), Cons(y, ys)) =>
        if (x > y) Cons(y, merge(l1, ys))
        else Cons(x, merge(xs, l2))
    }
  }

  l match {
    case Void => l
    case Cons(_, Void) => l
    case _ => {
      val mid = l.size / 2
      merge(mergeSort(l.take(mid)), mergeSort(l.drop(mid)))
    }
  }
}

//mergeSort(Cons(5,Cons(4,Cons(1,Cons(3,Cons(2,Void))))))

// Another example for opting between functional and OO decomposition

trait Expr

case class Atom(a: Int) extends Expr
case class Add(e1: Expr, e2: Expr) extends Expr
case class Mult(e1: Expr, e2: Expr) extends Expr

// 3 * ( 2 + 1 )
Mult(Atom(3), Add(Atom(2), Atom(1)))
// simplify should produce: 3 * 2 + 3 * 1

// simplify is also non-local: hence, the implementation in Mult need to decompose the
// expression e2. Functional decomposition is preferred here:

def simplify(e: Expr): Expr = {
  e match {
    case Atom(_) => e
    case Add(e1, e2) => Add(simplify(e1), simplify(e2))
    case Mult(e1, Add(e2, e3)) => {
      val simpe1 = simplify(e1)
      val simpe2 = simplify(e2)
      val simpe3 = simplify(e3)
      Add(Mult(simpe1, simpe2), Mult(simpe1, simpe3))
    }

    // what other cases are necessary?
  }
}