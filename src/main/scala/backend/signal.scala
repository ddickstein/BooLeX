package backend

import scala.collection.mutable.PriorityQueue

import library._

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
  override def toString: String = value + " -> " + target
}

class SignalQueue(delayTime: Int, callback: Set[(String, Boolean)] => Unit) {

  private val queue = PriorityQueue.empty[Signal]
  private val sideQueue = PriorityQueue.empty[Signal]
  private var initializing = true
  private var animation = new Thread(new Animatable())

  class Animatable extends Runnable {
    def run: Unit = {
      while (!Thread.interrupted && queue.nonEmpty) {
        val receivers = scala.collection.mutable.Set.empty[(String, Boolean)]
        while (queue.nonEmpty && queue.head.delay == 0) {
          val signal = queue.dequeue
          debug("SignalQueue: Firing " + signal)
          signal.fire(SignalQueue.this)
          if (signal.target.isInstanceOf[Socket]) {
            signal.target.asInstanceOf[Socket].idOpt.foreach(name => receivers += (name -> signal.value))
          }
        }
        queue.foreach(_.decrement)
        try {
          Thread.sleep(delayTime)
        } catch {
          case e: InterruptedException => Thread.currentThread.interrupt // restore interrupted signal
        } finally {
          println(receivers.toSet.size)
          callback(receivers.toSet)
        }
      }
      SignalQueue.this.synchronized {
        if (queue.isEmpty && sideQueue.nonEmpty) {
          initializing = false
          sideQueue.foreach(signal => debug("SignalQueue: Preparing " + signal))
          queue ++= sideQueue
          sideQueue.clear
        }
      }
    }
  }

  def fire(signal: Signal, initializing: Boolean = false) {
    this.synchronized {
      if (this.initializing ==> initializing) {
        debug("SignalQueue: Preparing " + signal)
        queue.enqueue(signal)
      } else {
        debug("SignalQueue: Placing " + signal + " in side queue")
        sideQueue.enqueue(signal)
      }
      if (!animation.isAlive) {
        animation = new Thread(new Animatable())
        animation.start
      }
    }
  }

  def stop: Unit = {
    queue.clear
    animation.interrupt
  }
}
