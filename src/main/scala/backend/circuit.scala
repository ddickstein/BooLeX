package backend

import library._
import itreegen.IRTreeBuilder

case class Circuit(val inputs: Seq[Socket], val outputs: Seq[Socket]) {
  def this(socket: Socket) = this(List(socket), List(socket))
  def this(socketIdOpt: Option[String]) = this(new Socket(socketIdOpt))
}

class CircuitBuilder extends IRTreeBuilder[Circuit] {
  val constantSockets = Map(true -> new ConstantSocket(true), false -> new ConstantSocket(false))

  def newNode(socketIdOpt: Option[String]): Circuit = new Circuit(socketIdOpt)
  def newConstNode(value: Boolean): Circuit = new Circuit(constantSockets(value))
  
  def not(circuit: Circuit): Circuit = connectToGate(NotGate, circuit)
  def and(circuit1: Circuit, circuit2: Circuit): Circuit = connectToGate(AndGate, circuit1, circuit2)
  def or(circuit1: Circuit, circuit2: Circuit): Circuit = connectToGate(OrGate, circuit1, circuit2)
  def xor(circuit1: Circuit, circuit2: Circuit): Circuit = connectToGate(XorGate, circuit1, circuit2)
  def nand(circuit1: Circuit, circuit2: Circuit): Circuit = connectToGate(NandGate, circuit1, circuit2)
  def nor(circuit1: Circuit, circuit2: Circuit): Circuit = connectToGate(NorGate, circuit1, circuit2)
  def xnor(circuit1: Circuit, circuit2: Circuit): Circuit = connectToGate(XnorGate, circuit1, circuit2)
  
  def group(circuits: Seq[Circuit]): Circuit = {
    val inputs = circuits.flatMap(_.inputs)
    val outputs = circuits.flatMap(_.outputs)
    return Circuit(inputs, outputs)
  }

  def chain(circuit1: Circuit, circuit2: Circuit): Circuit = {
    for ((output1, input2) <- circuit1.outputs.zip(circuit2.inputs)) {
      output1.addTarget(input2)
    }
    return Circuit(circuit1.inputs, circuit2.outputs)
  }
  
  def blackBox(input: Circuit, output: Circuit, inTopLevel: Boolean): Circuit = {
    if (inTopLevel) {
      val namesIterator = Iterator.from(1)
      output.outputs.filter(_.idOpt.isEmpty).foreach(_.idOpt = Some("%o" + namesIterator.next))
    }
    return Circuit(input.inputs, output.outputs)
  }

  private def connectToGate(gate: Gate, circuits: Circuit*): Circuit = {
    val destination = new Socket()
    gate match {
      case NotGate => NotGate(circuits(0).outputs(0), destination)
      case AndGate => AndGate(circuits(0).outputs(0), circuits(1).outputs(0), destination)
      case OrGate => OrGate(circuits(0).outputs(0), circuits(1).outputs(0), destination)
      case XorGate => XorGate(circuits(0).outputs(0), circuits(1).outputs(0), destination)
      case NandGate => NandGate(circuits(0).outputs(0), circuits(1).outputs(0), destination)
      case NorGate => NorGate(circuits(0).outputs(0), circuits(1).outputs(0), destination)
      case XnorGate => XnorGate(circuits(0).outputs(0), circuits(1).outputs(0), destination)
    }
    return Circuit(circuits.flatMap(_.inputs), List(destination))
  }
}
