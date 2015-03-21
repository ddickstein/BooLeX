package backend

import scala.collection.mutable.PriorityQueue

import library._

class Signal(val target: SignalReceiver, val value: Boolean, private var _delay: Int) extends Ordered[Signal] {
  def delay: Int = _delay
  def decrement: Unit = _delay -= 1
  def compare(that: Signal): Int = this.delay - that.delay
  def fire(propagate: Signal => Unit) {
    if (delay <= 0) {
      target.receive(this, propagate)
    }
  }
  override def toString: String = value + " -> " + target
}

trait SignalReceiver {
  def receive(signal: Signal, propagate: Signal => Unit): Unit
}
