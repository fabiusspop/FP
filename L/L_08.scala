package L

object L_08 extends App{

  type Matrix = List[List[Int]]
  // higher-order functions or for expressions at ur leisure.

  // 8.1.1. function which computes the sum of all elements from a matrix

  def sum(m: Matrix): Int  = {
    var sum = 0
    for (i <- m.indices)
      for (j <- m(i).indices)
        sum = sum + m(i)(j)
    sum
  }

  val matrix1 = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
  val matrix2 = List(List(2, 3, 4), List(5, 6, 7), List(8, 9, 10))
  println(sum(matrix1))

  // 8.1.2. scalarMult -> computes the scalar multiplication of a matrix
  def scalarMult(const: Int, m: Matrix): Matrix = {
    for (row <- m) yield {
      for (element <- row) yield {
        element * const
      }
    }
  }

  println(scalarMult(2, matrix1))

  // 8.1.3. add -> adds two matrices
  // use zip functions
  def add(m1: Matrix, m2: Matrix): Matrix = {
    m1.zip(m2)
      .map(pairList => pairList._1.zip(pairList._2)
      .map(p => p._1 + p._2))
  }

  println(add(matrix1, matrix2))

  // 8.1.4. singleLine --> returns the first column of a matrix, as a single line
  def singleLine(m: Matrix): List[Int] = {
    for (row <- m) yield
      row.head
  }

  println(singleLine(matrix1))

  // 8.1.5. remCol --> return the rest of a matrix, without its first column
  def remCol(m: Matrix): Matrix = {
    m.map(row => row.tail)
  }

  println(remCol(matrix1))

  // 8.1.6. transpose --> performs matrix transposition.
  def transpose(m: Matrix): Matrix = {
    m match {
      case Nil :: _ => Nil
      case _ => m.map(_.head) :: transpose(m.map(_.tail))
    }
  }

  println(transpose(matrix1))

  // 8.1.7. mult -> multiplication of two matrices
  def mult(m1: Matrix, m2: Matrix): Matrix = {
    m1.map(row => transpose(m2).map(
      col => row.zip(col).map(p => p._1 * p._2).foldRight(0)(_ + _)
    ))
  }

  println(mult(matrix1, matrix2))

  type Img = List[List[Int]]

  // 8.2.1. show --> converts an image to a string
  def show(m: Img): String = {
    m.map(row => row.mkString(" "))
      .mkString("\n")
  }

  val img1: Img = List(
    List(0, 0, 1, 0, 0),
    List(0, 1, 0, 1, 0),
    List(0, 1, 1, 1, 0),
    List(1, 0, 0, 0, 1),
    List(1, 0, 0, 0, 1)
  )

  val img2: Img = List(
    List(5, 4, 3, 2, 1),
    List(5, 4, 3, 2, 1),
    List(5, 4, 3, 2, 1),
    List(5, 4, 3, 2, 1),
    List(5, 4, 3, 2, 1)
  )

  println()
  println(show(img1))

  // 8.2.2. hFlip --> performs a horizontal flip on an image
  def hFlip(img: Img): Img = {
    img.map(row => row.reverse)
  }

  println()
  println(show(hFlip(img1)))

  // 8.2.3. vFlip --> performs a vertical flip on an image
  def vFlip(img: Img): Img = img.reverse

  println()
  println(show(vFlip(img1)))

  // 8.2.4. rot90Right --> performs a 90 degrees rotation to the right
  def rot90Right(img: Img): Img = {
    hFlip(transpose(img))
  }

  println()
  println(show(rot90Right(img1)))

  // 8.2.5. rot90Left --> performs a 90 degrees rotation to the left
  def rot90Left(img: Img): Img = {
    vFlip(transpose(img))
  }

  println()
  println(show(rot90Left(img1)))

  // 8.2.6. function which inverts an image (values 0 become 255, 1 - 254, ... )
  def invert(img: Img): Img = {
    img.map(row => row.map(pixel => 255 - pixel))
  }

  println(show(invert(img1)))

  // 8.2.7. cropAt --> crops a given image, using two, two-dimensional coordinates
  // - the higher-left point x and y
  // - the lower-right point x and y
  // horror.......
  def cropAt(img: Img, xSt: Int, ySt: Int, xEnd: Int, yEnd: Int): Img = {
    img
      .drop(ySt)
      .take(yEnd - ySt + 1)
      .map(row =>
        row
          .drop(xSt)
          .take(xEnd - xSt + 1))
  }

  println()
  println(show(cropAt(img1, 1, 1, 2, 3)))

  // 8.2.8 largerPos --> returns a list of all positions which have pixels of larger intensity than x
  def largerPos(img: Img, int: Int): List[(Int, Int)] = {
    img.
      indices
      .flatMap(y =>
        img(y)
          .indices
          .filter(x => img(y)(x) > int)
          .map(x => (y, x)))
          .toList
  }

  println()
  println(largerPos(img1, 0))

  // 8.2.9. contrast --> adds x to the intensity of each pixel
  def contrast(x: Int)(img: Img): Img = {
    img.map(row => row.map(pixel => pixel + x))
  }

  println()
  println(show(contrast(100)(img1)))

  // 8.2.10 hglue --> takes to images and glues them on the horizontal axis (image will be XY_)
  // 8.2.11 vlue --> analog but on vertical axis
  // 8.2.12 diag --> takes a square image, and draws to diagonal lines of intensity 1 across it
  // use _.until(_) and _.toList


}
