import parser.BoolexParser
import scala.io.Source

object Main {
  def main(args: Array[String]) {
    println(Source.fromFile("examples/adder.blex").mkString)
    // val parser = new BoolexParser()
    // println(parser.parseAll(parser.module, myCircuit))

  }
}
