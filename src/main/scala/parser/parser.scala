package parser

import scala.util.parsing.combinator._
import contexts._

object BoolexParser {
  def parse(input: String): Either[String, ModuleContext] = {
    val result = BoolexParserImpl.parseAll(BoolexParserImpl.module, input)
    result match {
      case BoolexParserImpl.Success(result, _) => Right(result)
      case BoolexParserImpl.NoSuccess(msg, next) => {
        Left("[" + next.pos.line + ":" + next.pos.column + "] Syntax error: " + msg)
      }
    }
  }

  private object BoolexParserImpl extends JavaTokenParsers with PackratParsers {
    type EC = ExpressionContext

    override protected val whiteSpace = """(\s|#.*)+""".r

    lazy val module: PackratParser[ModuleContext] = positioned(
      rep1(circuitDeclaration) ^^ (decs => ModuleContext(decs))
    )
    
    lazy val circuitDeclaration: PackratParser[CircuitDeclarationContext] = positioned((
        "circuit" ~ symbol ~ opt("(" ~ rep1sep(symbol, ",") ~ ")") ~
          rep(assignment) ~
          outStatement ~
        "end"
      ) ^^ {
        case "circuit"~name~Some("("~(params)~")")~body~out~"end" =>
          CircuitDeclarationContext(name, Some(params), body, out)
        case "circuit"~name~None~body~out~"end" => CircuitDeclarationContext(name, None, body, out)
      }
    )
    
    lazy val assignment: PackratParser[AssignmentContext] = positioned(
      rep1sep(symbol, ",") ~ "=" ~ rep1sep(l1expression, ",") ^^ {
        case variables~"="~values => AssignmentContext(variables, values)
      }
    )
    
    lazy val outStatement: PackratParser[OutStatementContext] = positioned(
      "out" ~ rep1sep(l1expression, ",") ^^ {
        case "out"~outputs => OutStatementContext(outputs)
      }
    )

    lazy val circuitCall: PackratParser[CircuitCallContext] = positioned(
      symbol ~ "(" ~ repsep(l1expression, ",") ~ ")" ^^ {
        case name~"("~args~")" => CircuitCallContext(name, args)
      }
    )

    lazy val l1expression: PackratParser[ExpressionContext] = positioned(
      ( (l1expression ~ ("xor"|"^") ~ l1expression)
      | (l1expression ~ "xnor" ~ l1expression)
      | l2expression
    ) ^^ {
        case (exp1: EC)~"xor"~(exp2: EC) => XorExpression(exp1, exp2)
        case (exp1: EC)~"^"~(exp2: EC) => XorExpression(exp1, exp2)
        case (exp1: EC)~"xnor"~(exp2: EC) => XnorExpression(exp1, exp2)
        case (exp: EC) => exp
      }
    )

    lazy val l2expression: PackratParser[ExpressionContext] = positioned(
      ( (l2expression ~ ("or"|"+") ~ l2expression)
      | (l2expression ~ "nor" ~ l2expression)
      | l3expression
    ) ^^ {
        case (exp1: EC)~"or"~(exp2: EC) => OrExpression(exp1, exp2)
        case (exp1: EC)~"+"~(exp2: EC) => OrExpression(exp1, exp2)
        case (exp1: EC)~"nor"~(exp2: EC) => NorExpression(exp1, exp2)
        case (exp: EC) => exp
      }
    )

    lazy val l3expression: PackratParser[ExpressionContext] = positioned(
      ( (l3expression ~ ("and"|"*") ~ l3expression)
      | (l3expression ~ "nand" ~ l3expression)
      | l4expression
    ) ^^ {
        case (exp1: EC)~"and"~(exp2: EC) => AndExpression(exp1, exp2)
        case (exp1: EC)~"*"~(exp2: EC) => AndExpression(exp1, exp2)
        case (exp1: EC)~"nand"~(exp2: EC) => NandExpression(exp1, exp2)
        case (exp: EC) => exp
      }
    )

    lazy val l4expression: PackratParser[ExpressionContext] = positioned(
      ( (l4expression ~ "\'")
      | (("not" | "-") ~ l4expression)
      | l5expression
    ) ^^ {
        case (exp: EC)~"\'" => NotExpression(exp)
        case "not"~(exp: EC) => NotExpression(exp)
        case "-"~(exp: EC) => NotExpression(exp)
        case (exp: EC) => exp
      }
    )

    lazy val l5expression: PackratParser[ExpressionContext] = positioned(
      ( "(" ~> l1expression <~ ")"
      | "true"
      | "false"
      | symbol <~ not("(")
      | circuitCall
    ) ^^ {
        case "true" => BooleanValue(true)
        case "false" => BooleanValue(false)
        case symbol: SymbolContext => Variable(symbol)
        case exp: EC => exp
      }
    )

    lazy val symbol: PackratParser[SymbolContext] = positioned(
      ( not("true") ~> not("false") ~> ident
      | failure("`true' and `false' are not valid variable names.")
    ) ^^ {
        case name => SymbolContext(name)
      }
    )
  }
}
