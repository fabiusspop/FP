package L

object L_07 extends App {

  trait Nat {
    def +(other: Nat): Nat
  }

  case object Zero extends Nat {
    override def +(other: Nat): Nat = other
  }

  case class Succ(n: Nat) extends Nat {
    override def +(other: Nat): Nat = Succ(n + other)
  }

  object Nat {
    def apply(i: Int): Nat = {
      if (i < 0) Zero
      else Succ(apply(i - 1))
    }

    def apply(s: String): Nat = {
      apply(s.toInt)
    }
  }

  // println(Nat(1) + Zero + Nat("2"))

  // 7.1.2. fromList -> takes a list of integers and converts them to a list of type List[Option[Nat]]
  // if an integer x is positive, it should become a value Some(x)
  // otherwise it should become None

  def fromList(l: List[Integer]): List[Option[Nat]] = {
    l.map(i => if (i < 0) None else Some(Nat(i)))
  }

  // 7.1.3. fromOptions -> takes a list of Option[Nat] and converts it to Option[List[Nat]].
  // Logic: if the list contains at least one None value (hence some conversion from integer failed),
  //            then the result should be None.
  // Otherwise, it should be a list containing all valid naturals.

  def fromOptions(l: List[Option[Nat]]): Option[List[Nat]] = {
    def sequence[A](list: List[Option[A]]): Option[List[A]] = {
      list match {
        case Nil => Some(Nil)

        case x :: xs => x.flatMap(x_new => sequence(xs).map(x_new :: _))

      }
    }

    sequence(l)
  }

  object Dictionary {
    def apply[K, V](inner: List[(K, V)]): Dictionary[K, V] = new Dictionary(inner, None)

    def apply[K, V](inner: List[(K, V)], default: V): Dictionary[K, V] = new Dictionary(inner, Some(default))
  }
  
  // sum of two integers
  

  class Dictionary[K, V](inner: List[(K, V)], default: Option[V] = None) {
    def +(pair: (K, V)): Dictionary[K, V] = new Dictionary(pair :: inner, default)

    def contains(pair: (K, V)): Boolean = inner.exists(_ == pair)

    def contains(key: K): Boolean = inner.exists(_._1 == key)

    def get(key: K): Option[V] = {
      inner.find(_._1 == key).map(_._2).orElse(default)
    }

    def getOrElse(default: V)(key: K): V = {
      this.get(key).getOrElse(default)
    }

    def map(f: V => V): Dictionary[K, V] = {
      new Dictionary(inner.map { case (k, v) => (k, f(v)) }, default.map(f))
    }

    def withDefaultValue(default: V): Dictionary[K, V] = {
      new Dictionary(inner, Some(default))
    }
  }

  // 7.3. Polynomials
  // We can resort to the datatype Map instead of your previously defined Dictionary
  // Cd. a polynomial encoded as a map, where each present key denotes a power of X, and a value denotes its coefficient
  Map(2 -> 2, 0 -> 3)

  case class Polynomial(nonZeroTerms: Map[Int, Int]) {
    def *(n: Int): Polynomial = {
      Polynomial(nonZeroTerms.map { case (power, coefficient) => (power, coefficient * n) })
    }

    override def toString: String = {
      nonZeroTerms
        .filter { case (_, coefficient) => coefficient != 0 }
        .toList
        .sortBy { case (power, _) => -power }
        .map { case (power, coefficient) => s"$coefficient*X^$power" }
        .mkString(" + ")
    }

    def hasRoot(r: Int): Boolean = {
      nonZeroTerms.foldLeft(0){
        case (sum, (power, coefficient)) =>
          sum + coefficient * Math.pow(r, power).toInt
      } == 0
    }
    
    def degree: Int = {
      if (nonZeroTerms.isEmpty) 0
      else nonZeroTerms.keys.max
    }
    
    def +(p2: Polynomial): Polynomial = {
      val resultTerms = p2.nonZeroTerms.foldLeft(nonZeroTerms) {
        case (result, (power, coefficient)) =>
          result + (power -> (coefficient + result.getOrElse(power, 0)))
      }
      Polynomial(resultTerms)
    }

    def *(p2: Polynomial): Polynomial = {
      val resultTerms = for {
        (power1, coefficient1) <- nonZeroTerms
        (power2, coefficient2) <- p2.nonZeroTerms
      } yield (power1 + power2) -> (coefficient1 * coefficient2)

      Polynomial(resultTerms.groupBy(_._1).map { case (power, pairs) => power -> pairs.map(_._2).sum })
    }
  }



}
