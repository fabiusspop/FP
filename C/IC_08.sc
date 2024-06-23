trait Nat
case object Zero extends Nat
case class Succ(n: Nat) extends Nat

def fromInt(i: Int): Option[Nat] = {
  def toNat(i: Int): Nat = {
    if (i == 0) Zero
    else Succ(toNat(i - 1))
  }
  if (i < 0) None
  else Some(toNat(i))
}

def larger3(n: Nat): Boolean = {
  n match {
    case Succ(Succ(Succ(_))) => true
    case _ => false
  }
}

def decrement(n: Nat): Option[Nat] = {
  n match {
    case Zero => None
    case Succ(smth) => Some(smth)
  }
}

/*
  Computation Sequence:
  - extract a number
  - decrement it
  - check if it is larger than 3

  We should perform boxing and unboxing, each step of the way.
 */

fromInt(3) match {
  case None => None
  case Some(n) => decrement(n) match {
    case None => None
    case Some(np) => larger3(np)
  }
}

def mapp[A, B](f: A => B)(box: Option[A]): Option[B] = {
  box match {
    case None => None
    case Some(inner) => Some(f(inner))
  }
}

/*
  This form of writing code is better, because there is no
  explicit boxing/unboxing anymore, but still cumbersome
 */

mapp(larger3)(mapp(Succ(_))(fromInt(2)))

/*
  Ideally, we would like to write something like:
  fromInt(2)
    .map(Succ(_))
    .map(larger3)

  Not only this. But we would like to have a transparent mechanism,
  to make map work for other container types such as Option.

  Question: "Who" should have the "map" property?
  - Option[Int]
  - Option[Nat]
  - Option[Char]

  Map should be supported by Option[A], where A can be anything.
  But what if we would also like to support this map for other types, such as List[A]?

  If map should be supported by Option[A] as well as List[A]
                    F[_] <- generalisation for the above.

  F is called a type constructor. It is "like a function" that takes a type
  and creates another type.
  For instance, Option, takes Int, and creates type Option[Int]

  Type constructors such as F are also called "container types".
  Eg of containers: List[A] (List:: type -> type), Tree[A] (Tree: type -> type)

  If a container type such as the above supports the map (fmap) operation, then
  that container type is called a Functor. (Monads are more particular functors)

  How to define in Scala, what functor is?
  A higher-kind constructor
 */

//def mapp[A, B](f: A => B)(box: Option[A]): Option[B]

trait Functor[F[_]] {
  def fmap[A, B](value: F[A])(f: A => B): F[B]
}

// an instance of Functor is an object, not a type.

implicit val optionFunctor: Functor[Option] = new Functor[Option] {
  override def fmap[A, B](box: Option[A])(f: A => B): Option[B] = {
    box match {
      case None => None
      case Some(x) => Some(f(x))
    }
  }
}

optionFunctor.
  fmap(optionFunctor.fmap(fromInt(3))(Succ(_)))(larger3)

// let's make this writing better. To do this, we will use implicits:

implicit class FunctorOps[F[_], A](fa: F[A])(implicit functorInstance: Functor[F]) {
  def map[B](f: A => B): F[B]  = functorInstance.fmap(fa)(f)
}

// trick 1: implicit parameters:
fromInt(3)
  .map(Succ(_))
  .map(larger3)

/*
  fromInt(3) has type Option[Nat]
  .map    Scala will search for a compatible implementation, locally.
  In our program, there is an implicit class that support map, called FunctorOps
  This class will act as a wrapper.

  fromInt(3).map(...) becomes new FunctorOps(fromInt(3)).map(...)

  Now, in:
    new FunctorOps(fromInt(3)) the second parameter is missing,
    thus an implicit value is searched in the program,
    and optionFunctor is type-compatible.

  new FunctorOps(fromInt(3))(optionFunctor)

  I can using functor, transform sequences such as:
  fromInt(3) then increment then larger3

  But can I do sth like this:
  x <- fromInt(2)
  y <- fromInt(3)
  x - y, where minus could also return None?

  In FP, this type of sequencing is done with MONADS.

  In Scala, the cats library support monads, but you can achieve the same effect without them.
 */