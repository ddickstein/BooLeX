package backend

class Socket(var idOpt: Option[String] = None, private var _value: Boolean = false) extends SignalReceiver {
  private var _targets = scala.collection.mutable.Set.empty[SignalReceiver]
  def value: Boolean = _value
  def targets: Set[SignalReceiver] = _targets.toSet
  def addTarget(target: SignalReceiver): Unit = _targets += target
  def receive(signal: Signal, queue: SignalQueue) {
    _value = signal.value
    targets.foreach(target => queue.fire(new Signal(target, value, 0)))
  }
}

class ConstantSocket(override val value: Boolean) extends Socket(None, value)
