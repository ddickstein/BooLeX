package typechecker

case class CircuitMetaData(
  dependencyGraph: Map[String, Set[String]],
  circuitSpecs: Map[String, (Int, Int)],
  circuitClocksMapOpt: Option[Map[String, Set[Int]]] = None
)
