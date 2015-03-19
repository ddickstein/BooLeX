package backend

import scala.collection.mutable.PriorityQueue

trait SignalReceiver {
  def receive(signal: Signal, queue: SignalQueue): Unit
}

class Signal(
  val target: SignalReceiver,
  val value: Boolean,
  private var _delay: Int
) extends Ordered[Signal] {
  def delay: Int = _delay
  def decrement: Unit = _delay -= 1
  def compare(that: Signal): Int = that.delay - this.delay
  def fire(queue: SignalQueue) {
    if (delay <= 0) {
      target.receive(this, queue)
    }
  }
}

class SignalQueue(delayTime: Int, callback: Set[Socket] => Unit) {

  private val queue = PriorityQueue.empty[Signal]
  private var animation = new Thread(new Animatable())

  class Animatable extends Runnable {
    def run: Unit = {
      while (!Thread.interrupted && queue.nonEmpty) {
        val receivers = scala.collection.mutable.Set.empty[Socket]
        while (queue.nonEmpty && queue.head.delay == 0) {
          val signal = queue.dequeue
          signal.fire(SignalQueue.this)
          if (signal.target.isInstanceOf[Socket]) {
            receivers += signal.target.asInstanceOf[Socket]
          }
        }
        queue.foreach(_.decrement)
        try {
          Thread.sleep(delayTime)
        } catch {
          case e: InterruptedException => ()  
        } finally {
          callback(receivers.toSet)
        }
      }
    }
  }

  def fire(signal: Signal) {
    queue.enqueue(signal)
    if (!animation.isAlive) {
      animation = new Thread(new Animatable())
      animation.start
    }
  }

  def stop: Unit = {
    queue.clear
    animation.interrupt
  }
}
