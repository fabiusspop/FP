/*
  Higher-order functions
  Datatypes - functional vs OO
  Case classes
  More about lists, and list applications
 */

// Scala lists
val l1: List[Int] = List(1, 2, 3)
val l2: List[Int] = 1 :: (2 :: List(3))
// l1.tail

def size(l: List[Int]): Int = {
  l match {
    case Nil => 0
    case _ :: xs => 1 + size(xs)
  }
}

// Short recap on the most important higher-order functions over lists
l1.map(x => 2 * x)
// Whenever we have simple lambda-functions like x => 2 * x + x + 1
l1.map(_ * 2)
l2.filter(_ % 2 == 0)
// x1 op (x2 op (x3 op acc)))
l1.foldRight(0)(_ + _)
// (((acc op x1) op x2) op x3)
l2.foldLeft(Nil: List[Int])((acc, x) => x :: acc)

// Task 1 - create a function that reads matrices from a String

val m =
  """1,2,3
    |4,5,6
    |7,8,9
    |""".stripMargin

// How do we represent matrices?

type Str = List[Char]
type Mat = List[List[Int]]

val m1 = List(
  List(1, 2, 3),
  List(4, 5 ,6),
  List(7, 8, 9)
)

// main ingredient: a SPLIT!
val test1 = "12 3 45"
//split(' ')(test1.toList)

def readMatrix(s: String): Mat = {
  def toInt(s: Str) = s.foldRight("")(_ + _).toInt

  def split(sep: Char)(content: Str): List[Str] = {
    def op(x: Char, acc: List[Str]): List[Str] =
      acc match {
        case Nil =>
          if (x == sep) Nil
          else List(List(x))

        case st :: rest =>
          if (x == sep) Nil :: acc
          else (x :: st) :: rest
      }

    content.foldRight(Nil: List[Str])(op)
  }

  split('\n')(s.toList)
    .map(split(','))
    .map(_.map(toInt(_)))

}

readMatrix(m)

// dot product
def dot(c: Int, m: Mat): Mat =
  m.map(_.map(_ * c))
/*
  the outer map applies its argument function to each row of the mx
  the inner map applies its argument function to each elem of a row

 */


def transpose(m: Mat): Mat =
  m match {
    case Nil :: _ => Nil
    case _ =>
      m.map(_.head) :: transpose(m.map(_.tail))
    /*
    creates a new row from the first elements of each row in the mx m
    and prepends it to the transposed version of the matrix without its first elements
     */
  }