package parser

import scala.util.parsing.input.Positional

package contexts {
  abstract class Context extends Positional
  abstract class ExpressionContext extends Context
  case class AssignmentContext(variables: List[String], values: List[ExpressionContext]) extends Context
  case class CircuitDeclarationContext(
    name: String,
    paramsOpt: Option[List[String]],
    assignments: List[AssignmentContext],
    output: OutStatementContext
  ) extends Context
  case class ModuleContext(decs: List[CircuitDeclarationContext]) extends Context
  case class OutStatementContext(outputs: List[ExpressionContext]) extends Context
  case class NotExpression(exp: ExpressionContext) extends ExpressionContext
  case class AndExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
  case class NandExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
  case class XorExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
  case class XnorExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
  case class OrExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
  case class NorExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
  case class BooleanValue(value: Boolean) extends ExpressionContext
  case class Variable(name: String) extends ExpressionContext
  case class CircuitCallContext(name: String, arguments: List[ExpressionContext]) extends ExpressionContext
}
