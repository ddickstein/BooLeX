package vhdl

import library._
import itreegen.IRTreeBuilder

sealed trait VhdlContext
case class ProjectContext(entities: Seq[EntityContext]) extends VhdlContext
case class EntityContext(name: String, ports: PortContext) extends VhdlContext
case class ArchitectureContext(
  entity: EntityContext,
  components: Seq[EntityContext],
  signals: Seq[String],
  portMaps: Seq[Map[EntityContext, Seq[String]]],
  assignments: Map[String, ExpressionContext]
)
case class PortContext(inputs: Seq[String], outputs: Seq[String])
sealed trait ExpressionContext
