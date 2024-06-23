package HW.HW2

trait WTreeInterface {

  def height: Int
  def balance: Int
  def rotateLeft: WTree
  def rotateRight: WTree
  def rotateRightLeft: WTree
  def rotateLeftRight: WTree
  def rebalance: WTree

  def isEmpty: Boolean
  def filter(pred: Token => Boolean): WTree
  def ins(w: Token): WTree
  def contains(s:String): Boolean
  def size: Int
}

