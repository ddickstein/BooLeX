package parser

import scala.util.parsing.input.Positional

package contexts {
  sealed abstract class Context extends Positional
  sealed abstract class ExpressionContext extends Context
  final case class AssignmentContext(variables: List[String], values: List[ExpressionContext]) extends Context
  final case class CircuitDeclarationContext(
    name: String,
    paramsOpt: Option[List[String]],
    assignments: List[AssignmentContext],
    output: OutStatementContext
  ) extends Context
  final case class ModuleContext(decs: List[CircuitDeclarationContext]) extends Context
  final case class OutStatementContext(outputs: List[ExpressionContext]) extends Context
  final case class NotExpression(exp: ExpressionContext) extends ExpressionContext
  final case class AndExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
  final case class NandExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
  final case class XorExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
  final case class XnorExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
  final case class OrExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
  final case class NorExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
  final case class BooleanValue(value: Boolean) extends ExpressionContext
  final case class Variable(name: String) extends ExpressionContext
  final case class CircuitCallContext(name: String, arguments: List[ExpressionContext]) extends ExpressionContext
}
