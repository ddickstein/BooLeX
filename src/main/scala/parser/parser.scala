package parser

import errors.{CompileTimeError, SyntaxError}
import lexer.BoolexLexer
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StdTokenParsers

object BoolexParser {
  import contexts._
  def parse(input: String): Either[Seq[CompileTimeError], ModuleContext] = {
    val blxParser = new BoolexParserImpl()
    val result = blxParser.parseAll(input)
    result match {
      case blxParser.Success(result, _) => Right(result)
      case blxParser.NoSuccess(msg, next) => Left(
        List(SyntaxError(msg, Some(next.pos)))
      )
    }
  }

  final private class BoolexParserImpl
    extends StdTokenParsers
    with PackratParsers
  {
    type EC = ExpressionContext

    override type Tokens = BoolexLexer
    override val lexical = new BoolexLexer()

    def parseAll(input: String): ParseResult[ModuleContext] = {
      return phrase(module)(new lexical.Scanner(input))
    }

    lazy val module: PackratParser[ModuleContext] = positioned(
      rep1(circuitDeclaration) ^^ (circuits => ModuleContext(circuits))
    )

    lazy val circuitDeclaration: PackratParser[CircuitDeclarationContext] = {
      positioned(
        (
          "circuit" ~ symbol ~ opt("(" ~ rep1sep(symbol, ",") ~ ")") ~
            rep(assignment) ~
            outStatement ~
          "end"
        ) ^^ {
          case "circuit" ~ name ~ None ~ body ~ out ~ "end" =>
            CircuitDeclarationContext(name, None, body, out)
          case "circuit" ~ name ~ Some("(" ~ params ~ ")")
            ~ body ~ out ~ "end" => {
              CircuitDeclarationContext(name, Some(params), body, out)
            }
        }
      )
    }

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
      ( ("`" ~l5expression)
      | l6expression
    ) ^^ {
        case "`"~(exp: EC) => BufferExpression(exp)
        case (exp: EC) => exp
      }
    )

    lazy val l6expression: PackratParser[ExpressionContext] = positioned(
      ( "(" ~> l1expression <~ ")"
      | "true"
      | "false"
      | symbol <~ not("(")
      | clock
      | circuitCall
    ) ^^ {
        case "true" => BooleanValue(true)
        case "false" => BooleanValue(false)
        case Symbol(name) => Variable(name)
        case exp: EC => exp
      }
    )

    lazy val clock: PackratParser[Clock] = positioned(
      "clock" ~> "(" ~> number <~ ")" ^^ Clock
    )

    lazy val number: PackratParser[Number] = positioned(numericLit ^^ Number)

    lazy val symbol: PackratParser[Symbol] = positioned(ident ^^ Symbol)
  }
}
