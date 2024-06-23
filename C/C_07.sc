// Intro for EXPRESSIONS

List(1, 2, 3).map(_ * 2)

val l: List[Int] = {
  for (x <- List(1, 2, 3))
    yield x * 2
}

List(1, 2, 3).filter(_ % 2 == 1).map(_ + 1)

for (x <- List(1, 2, 3) if x % 2 == 1)
  yield x + 1

// matrix where each element is a pair
val what: List[List[(Int, Int)]] = {
  for (x <- List(1, 2, 3))
    yield
    for (y <- List(4, 5, 6)) // List[(Int, Int)]
      yield (x, y) // (Int, Int
}

// just a flat list, not a matrix
val flat: List[(Int, Int)] = {
  for (x <- List(1, 2, 3);
       y <- List(4, 5, 6))
    yield (x, y)
}

List(1, 2, 3).zip(List(4, 5, 6))

val cartesian = {
  (for (x <- List (1, 2, 3)) yield
    for (y <- List (4, 5, 6)) yield
      (x, y)).foldRight(Nil: List[(Int, Int)])(_ ++ _)
}

val cartesianp = {
  for (x <- List(1, 2, 3);
       y <- List(4, 5, 6)) yield (x, y)
}

type Matrix = List[List[Int]]
val m = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))

m.map(_.map(_ * 2))

for (line <- m) yield
  for (x <- line) yield 2 * x

def add1(m1: Matrix, m2: Matrix): Matrix = {
  m1.zip(m2)
    .map(pairList => pairList._1.zip(pairList._2)
    .map(p => p._1 + p._2))
}

add1(m, m)

/*
def add2(m1: Matrix, m2: Matrix): Matrix =
  for (l1 <- m1; l2 <- m2) yield
    for (plists <- m1.zip(m2)) yield
      for (p <- plists._1.zip(plists._2)) yield
        p._1 + p._2
*/

def transpose(m: Matrix): Matrix ={
  m match {
    case Nil :: _ => Nil
    case _ => m.map(_.head) :: transpose(m.map(_.tail))
  }
}

transpose(m)

def prod(m1: Matrix, m2: Matrix): Matrix = {
  m1.map(line => transpose(m2).map(
    col => line.zip(col).map(p => p._1 * p._2).foldRight(0)(_ + _)
  ))
}

val id = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))

prod(m, id)

def prodp(m1: Matrix, m2: Matrix): Matrix = {
  for (line <- m1) yield
    for (col <- transpose(m2)) yield
      (for (p <- line.zip(col)) yield
        p._1 * p._2).foldRight(0)(_ + _)
}

prodp(m, id)

/*
Let us represent images as matrices of pixels. For simplicity, the value
i of a pixel will designate its brightness.

Side-note: if a pixel is a triple: (x, y, z) -> BPM encoding
 */

class Image (val inner: Option[List[List[Int]]]) {
  // concatenate another image to the right

  def >>(other: Image): Image = {
    // deal with the dimensions of other and inner
    // to be done
    (inner, other.inner) match {
      case (Some(t1), Some(t2)) =>
        Image(for (p <- t1.zip(t2)) yield p._1 ++ p._2)
      case _ => new Image(None)
    }
  }

    //  def mask(other: Image): Image = {
    //    (inner, other.inner) match {
    //      case (Some(t1), Some(t2)) =>
    //        val width = t1(0).size
    //        val height = t1.size
    //
    //        Image(
    //          for (i <- 0 until width) yield
    //            for (j <- 0 until height) yield
    //              if (t2(i)(j) == 1) t1(i)(j) else 0
    //        )
    //    }
    //  }

    override def toString: String = {
      inner match {
        case None => "Error"
        case Some(t) => t.map(_.map(_.toString).mkString(" ")).mkString("\n")
      }
    }

}

val i1 = new Image(Some(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))))


object Image {
  def apply(): Image = Image(None)
  def apply(opt: Option[List[List[Int]]]): Image = new Image(opt)
  def apply(table: List[List[Int]]): Image = Image(Some(table))
  def apply(table: IndexedSeq[IndexedSeq[Int]]): Image =
    Image(table.map(_.toList).toList)
  def apply(value: Int, width: Int, height: Int): Image = {
    def repeat[A](n:Int, v: A): List[A] =
      (for (x <- 0 until n) yield v).toList

    Image(repeat(height, repeat(width, value)))
  }
}

val mask = Image(1,3,3)

println(i1 >> mask)
