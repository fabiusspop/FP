// Introduction to Scala lists:

val l1 = List(1, 2, 3)
val l2 = 1 :: 2 :: 3 :: Nil

// Remember that LISTS ARE IMMUTABLE

// Important higher-order functions

l1.map(_ * 2)
l2.filter(_ > 2)

/*
Examples with:
- foldRight
- foldLeft
- zip
 */

// Some applications surrounding matrices:

/*
  Step 1: Reading a matrix from a string
  Let us call a list of characters Str, to make type signatures easier to follow
  Note that Str is different from String
 */

type Str = List[Char]

/*
  We declare the type Tabular to be any matrix-like structure over elements of type A
 */

type Tabular[A] = List[List[A]]

/*
  We define readTabular to be a function that
      reads space-separated lines as a matrix-like structure
 */

//def readTabular[A](read:Str => A)(content: Str): Tabular[A] = {
//  def split(delim: Char)(s: Str): List[Str] = {
//    def op(c: Char, acc: List[Str]): List[Str] = {
//      acc match {
//        case Nil =>
//          if (c == delim) Nil
//          else List(List(c))
//
//        case x :: xs =>
//          if (c == delim) Nil :: acc
//          else (c :: x) :: xs
//      }
//      s.foldRight(Nil: List[Str])(op)
//    }
//    split('\n')(content)
//      .map(split(',') andThen (_.map(read(_))))
//  }
//}