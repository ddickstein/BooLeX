import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional

abstract class Context extends Positional
case class AssignmentContext(identifiers: List[String], values: List[ExpressionContext]) extends Context
case class CircuitCallContext(name: String, arguments: List[ExpressionContext]) extends Context
case class CircuitDeclarationContext(
  name: String,
  paramsOpt: Option[List[String]],
  assignments: List[AssignmentContext],
  output: OutStatementContext
) extends Context
case class FactorContext(
  booleanValueOpt: Option[Boolean],
  identifierOpt: Option[String],
  circuitCallOpt: Option[CircuitCallContext],
  expressionOpt: Option[ExpressionContext]
) extends Context
case class ModuleContext(decs: List[CircuitDeclarationContext]) extends Context
case class OutStatementContext(outputs: List[ExpressionContext]) extends Context

abstract class ExpressionContext extends Context
case class FactorExpression(factor: FactorContext) extends ExpressionContext
case class NotExpression(exp: ExpressionContext) extends ExpressionContext
case class AndExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
case class NandExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
case class XorExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
case class XnorExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
case class OrExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext
case class NorExpression(exp1: ExpressionContext, exp2: ExpressionContext) extends ExpressionContext

class BoolexParser extends JavaTokenParsers {
  override protected val whiteSpace = """(\s|#.*)+""".r

  def module: Parser[ModuleContext] = positioned(
    rep(circuitDeclaration) ^^ (decs => ModuleContext(decs))
  )
  
  def circuitDeclaration: Parser[CircuitDeclarationContext] = positioned((
      "circuit" ~! ident ~! opt("(" ~! rep1sep(ident, ",") ~! ")") ~!
        rep(assignment) ~!
        outStatement ~!
      "end"
    ) ^^ {
      case "circuit"~name~Some("("~(params)~")")~body~out~"end" =>
        CircuitDeclarationContext(name, Some(params), body, out)
      case "circuit"~name~None~body~out~"end" => CircuitDeclarationContext(name, None, body, out)
    }
  )
  
  def assignment: Parser[AssignmentContext] = positioned(
    rep1sep(ident, ",") ~! "=" ~! rep1sep(expression, ",") ^^ {
      case identifiers~"="~values => AssignmentContext(identifiers, values)
    }
  )
  
  def outStatement: Parser[OutStatementContext] = positioned(
    "out" ~! rep1sep(expression, ",") ^^ {
      case "out"~outputs => OutStatementContext(outputs)
    }
  )

  def circuitCall: Parser[CircuitCallContext] = positioned(
    ident ~! "(" ~! repsep(expression, ",") ~! ")" ^^ {
      case name~"("~args~")" => CircuitCallContext(name, args)
    }
  )

  def factor: Parser[FactorContext] = positioned((
      "(" ~! expression ~! ")"
      | circuitCall
      | ident
    ) ^^ {
      case "true" => FactorContext(Some(true), None, None, None)
      case "false" => FactorContext(Some(false), None, None, None)
      case ident: String => FactorContext(None, Some(ident), None, None)
      case circuitCall: CircuitCallContext => FactorContext(None, None, Some(circuitCall), None)
      case expression: ExpressionContext => FactorContext(None, None, None, Some(expression))
    }
  )

  def expression: Parser[ExpressionContext] = { type EC = ExpressionContext; positioned((
      factor
      | expression ~! "\'"
      | ("not" | "-") ~! expression
      | expression ~! ("and" | "nand") ~! expression
      | expression ~! ("xor" | "xnor") ~! expression
      | expression ~! ("or" | "nor") ~! expression
    ) ^^ {
      case factor: FactorContext => FactorExpression(factor)
      case (exp: EC)~"\'" => NotExpression(exp)
      case "not"~(exp: EC) => NotExpression(exp)
      case "-"~(exp: EC) => NotExpression(exp)
      case (exp1: EC)~"and"~(exp2: EC) => AndExpression(exp1, exp2)
      case (exp1: EC)~"nand"~(exp2: EC) => NandExpression(exp1, exp2)
      case (exp1: EC)~"xor"~(exp2: EC) => XorExpression(exp1, exp2)
      case (exp1: EC)~"xnor"~(exp2: EC) => XnorExpression(exp1, exp2)
      case (exp1: EC)~"or"~(exp2: EC) => OrExpression(exp1, exp2)
      case (exp1: EC)~"nor"~(exp2: EC) => NorExpression(exp1, exp2)
    }
  )}
}

val myCircuit = """
x = 5
"""

val parser = new BoolexParser()
println(parser.parseAll(parser.assignment, myCircuit))

// # This RS latch is properly handled.
// circuit RSLatch(r,s)
//     P = r nor Q
//     Q = s nor P
//     out P, Q
// end

// circuit DLatch(d,clk)
//     out RSLatch(d' and clk, d and clk)
// end

// circuit main(d,clk)
//     out DLatch(d,clk)
// end
