package HW.HW2

case class Token(word: String, freq: Int) {
  override def toString: String = s"($word,$freq)"
}
