package typechecker

case class CircuitMetaData(
  totalDependencyGraph: Map[String, Set[String]],
  circuitSpecs: Map[String, (Int, Int)],
  circuitClocksMapOpt: Option[Map[String, Set[Int]]] = None
)
