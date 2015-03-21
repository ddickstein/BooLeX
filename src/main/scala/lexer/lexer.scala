package lexer

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.input.Reader

final class BoolexLexer extends StdLexical {
  reserved ++= Set("circuit", "out", "end", "true", "false", "and", "or", "not", "nand", "nor", "xor", "xnor", "clock")
  delimiters ++= Set("(", ")", "=", ",", "+", "-", "*", "^", "\'", "`")

  def debug(input: String): List[Token] = {
    val lst = ListBuffer.empty[Token]
    var scanner : Reader[Token] = new Scanner(input)
    while (!scanner.atEnd) {
      lst += scanner.first
      scanner = scanner.drop(1)
    }
    return lst.toList
  }

  override def whitespace: Parser[Any] = rep[Any](whitespaceChar | '#' ~ rep( chrExcept(EofCh, '\n')))
}
