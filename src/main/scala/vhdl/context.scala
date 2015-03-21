package vhdl

import library._
import itreegen.IRTreeBuilder

sealed trait VhdlContext
case class EntityContext(name: String, ports: PortContext) extends VhdlContext
case class ArchitectureContext(
  entity: EntityContext,
  components: Seq[EntityContext],
  signals: Seq[String],
  portMaps: Seq[Map[Entity, Seq[Signal]]],
  assignments: Map[String, ExpressionContext])

class VhdlContextBuilder extends IRTreeBuilder[VhdlContext] {
  private val iter = Iterator.from(0)
  private def nextName = "signal" + iter.next

  def newNode(nameOpt: Option[String]): VhdlContext
  def newConstNode(value: Boolean): VhdlContext

  def clock(ms: Int): VhdlContext
  def buffer(t1: VhdlContext): VhdlContext
  def not(t1: VhdlContext): VhdlContext
  def and(t1: VhdlContext, t2: VhdlContext): VhdlContext
  def or(t1: VhdlContext, t2: VhdlContext): VhdlContext
  def xor(t1: VhdlContext, t2: VhdlContext): VhdlContext
  def nand(t1: VhdlContext, t2: VhdlContext): VhdlContext
  def nor(t1: VhdlContext, t2: VhdlContext): VhdlContext
  def xnor(t1: VhdlContext, t2: VhdlContext): VhdlContext

  def group(ts: Seq[VhdlContext]): VhdlContext
  def chain(t1: VhdlContext, t2: VhdlContext): VhdlContext
  def blackBox(inVhdlContext: VhdlContext, outVhdlContext: VhdlContext, inTopLevel: Boolean): VhdlContext
}
