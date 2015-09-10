package vhdl

import library._

object Boolex2VhdlTranslator {
  import parser.contexts._
  import typechecker.CircuitMetaData

  // returns a map of VHDL filenames to file contents
  def translate(
    module: ModuleContext,
    metaData: CircuitMetaData
  ): Map[String, String] = {
    println("\n                                        * * *                                        ")
    println("\nThe translation of this preprocessed AST into proper VHDL will be done in the summer.")
    println("\n                                        * * *                                        \n")
    println(module + "\n")
    return null
  }
}
