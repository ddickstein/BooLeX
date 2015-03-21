package parser

import scala.util.parsing.input.Positional

package contexts {
  sealed abstract class Context extends Positional
  sealed abstract class ExpressionContext extends Context
  final case class Symbol(name: String) extends Context
  final case class Number(number: String) extends Context
  final case class AssignmentContext(variables: Seq[Symbol], values: Seq[ExpressionContext]) extends Context
  final case class CircuitDeclarationContext(
    name: Symbol,
    paramsOpt: Option[Seq[Symbol]],
    assignments: Seq[AssignmentContext],
    output: OutStatementContext
  ) extends Context
  final case class ModuleContext(circuits: Seq[CircuitDeclarationContext]) extends Context
  final case class OutStatementContext(outputs: Seq[ExpressionContext]) extends Context
  final case class NotExpression(exp: ExpressionContext) extends ExpressionContext
  final case class BufferExpression(exp: ExpressionContext) extends ExpressionContext
  final case class AndExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
  final case class NandExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
  final case class XorExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
  final case class XnorExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
  final case class OrExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
  final case class NorExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
  final case class BooleanValue(value: Boolean) extends ExpressionContext
  final case class Variable(name: String) extends ExpressionContext
  final case class Clock(milliseconds: Number) extends ExpressionContext
  final case class CircuitCallContext(name: Symbol, arguments: Seq[ExpressionContext]) extends ExpressionContext
}
