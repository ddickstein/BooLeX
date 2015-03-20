package backend

sealed trait Gate

case object NotGate extends Gate {
  def apply(input: Socket, output: Socket) {
    new NotGateImpl(input: Socket, output: Socket)
  }

  private class NotGateImpl(input: Socket, output: Socket) extends SignalReceiver {
    input.addTarget(this)
    def receive(signal: Signal, sender: SignalSender) {
      sender.fire(new Signal(output, !input.value, 1))
    }
    override def toString: String = "NOT[" + input + "] => " + output
  }
}

case object AndGate extends Gate {
  def apply(input1: Socket, input2: Socket, output: Socket) {
    new AndGateImpl(input1: Socket, input2: Socket, output: Socket)
  }

  private class AndGateImpl(input1: Socket, input2: Socket, output: Socket) extends SignalReceiver {
    input1.addTarget(this)
    input2.addTarget(this)
    def receive(signal: Signal, sender: SignalSender) {
      sender.fire(new Signal(output, input1.value && input2.value, 1))
    }
    override def toString: String = "AND[" + input1 + ", " + input2 + "] => " + output
  }
}

case object OrGate extends Gate {
  def apply(input1: Socket, input2: Socket, output: Socket) {
    new OrGateImpl(input1: Socket, input2: Socket, output: Socket)
  }

  private class OrGateImpl(input1: Socket, input2: Socket, output: Socket) extends SignalReceiver {
    input1.addTarget(this)
    input2.addTarget(this)
    def receive(signal: Signal, sender: SignalSender) {
      sender.fire(new Signal(output, input1.value || input2.value, 1))
    }
    override def toString: String = "OR[" + input1 + ", " + input2 + "] => " + output
  }
}

case object XorGate extends Gate {
  def apply(input1: Socket, input2: Socket, output: Socket) {
    new XorGateImpl(input1: Socket, input2: Socket, output: Socket)
  }

  private class XorGateImpl(input1: Socket, input2: Socket, output: Socket) extends SignalReceiver {
    input1.addTarget(this)
    input2.addTarget(this)
    def receive(signal: Signal, sender: SignalSender) {
      sender.fire(new Signal(output, input1.value ^ input2.value, 1))
    }
    override def toString: String = "XOR[" + input1 + ", " + input2 + "] => " + output
  }
}

case object NandGate extends Gate {
  def apply(input1: Socket, input2: Socket, output: Socket) {
    new NandGateImpl(input1: Socket, input2: Socket, output: Socket)
  }

  private class NandGateImpl(input1: Socket, input2: Socket, output: Socket) extends SignalReceiver {
    input1.addTarget(this)
    input2.addTarget(this)
    def receive(signal: Signal, sender: SignalSender) {
      sender.fire(new Signal(output, !(input1.value && input2.value), 1))
    }
    override def toString: String = "NAND[" + input1 + ", " + input2 + "] => " + output
  }
}

case object NorGate extends Gate {
  def apply(input1: Socket, input2: Socket, output: Socket) {
    new NorGateImpl(input1: Socket, input2: Socket, output: Socket)
  }

  private class NorGateImpl(input1: Socket, input2: Socket, output: Socket) extends SignalReceiver {
    input1.addTarget(this)
    input2.addTarget(this)
    def receive(signal: Signal, sender: SignalSender) {
      sender.fire(new Signal(output, !(input1.value || input2.value), 1))
    }
    override def toString: String = "NOR[" + input1 + ", " + input2 + "] => " + output
  }
}

case object XnorGate extends Gate {
  def apply(input1: Socket, input2: Socket, output: Socket) {
    new XnorGateImpl(input1: Socket, input2: Socket, output: Socket)
  }

  private class XnorGateImpl(input1: Socket, input2: Socket, output: Socket) extends SignalReceiver {
    input1.addTarget(this)
    input2.addTarget(this)
    def receive(signal: Signal, sender: SignalSender) {
      sender.fire(new Signal(output, !(input1.value ^ input2.value), 1))
    }
    override def toString: String = "XNOR[" + input1 + ", " + input2 + "] => " + output
  }
}
