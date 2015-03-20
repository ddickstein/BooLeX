package itreegen

trait IRTreeBuilder[T] {
  def newNode(nameOpt: Option[String]): T
  def newConstNode(value: Boolean): T

  def buffer(t1: T): T
  def not(t1: T): T
  def and(t1: T, t2: T): T
  def or(t1: T, t2: T): T
  def xor(t1: T, t2: T): T
  def nand(t1: T, t2: T): T
  def nor(t1: T, t2: T): T
  def xnor(t1: T, t2: T): T

  def group(ts: Seq[T]): T
  def chain(t1: T, t2: T): T
  def blackBox(inT: T, outT: T, inTopLevel: Boolean): T
}
