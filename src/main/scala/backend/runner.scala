package backend

import java.util.concurrent.ConcurrentSkipListSet
import java.util.concurrent.ConcurrentLinkedQueue

import scala.collection.JavaConverters._

import library._

class CircuitRunner(delayTime: Int, callback: Seq[(String, Boolean)] => Unit) {
  private val loadingDock = new ConcurrentLinkedQueue[Signal] // loaded into the signal queue for firing
  private val stabilizer = new Stabilizer[Signal] // stabilize entries in the priority queue
  private val signalQueue = new ConcurrentSkipListSet[stabilizer.entry] // stabilized priority queue
  private val clocks = scala.collection.mutable.ListBuffer.empty[Thread]
  private var initialized = false
  private var finished = false

  private val loader = new Thread(new Runnable {
    def run {
      waitFor(initialized, CircuitRunner.this)
      debug("Starting to load")
      while (!Thread.interrupted) {
        waitFor(!loadingDock.isEmpty, loadingDock)
        (!Thread.currentThread.isInterrupted).optionally(loadingDock.poll).foreach(signal => {
          signalQueue.synchronized {
            signalQueue.add(signal)
            signalQueue.notifyAll
          }
        })
      }
      debug("Loader: All done!")
    }
  })

  private val launcher = new Thread(new Runnable {
    def run {
      while (!Thread.interrupted) {
        if (!initialized && signalQueue.isEmpty) {
          CircuitRunner.this.synchronized {
            debug("Finished initializing")
            initialized = true
            CircuitRunner.this.notifyAll
          }
        }
        waitFor(!signalQueue.isEmpty, signalQueue)
        val receivers = scala.collection.mutable.SortedSet.empty[(String, Boolean)]
        while (!signalQueue.isEmpty && signalQueue.first.delay == 0) {
          val signal = signalQueue.pollFirst
          signal.fire(signal => signalQueue.add(signal))
          if (signal.target.isInstanceOf[Socket]) {
            signal.target.asInstanceOf[Socket].idOpt.foreach(name => receivers += (name -> signal.value))
          }
        }
        signalQueue.forEach((signal: stabilizer.entry) => signal.decrement)
        safeSleep(delayTime)
        if (receivers.nonEmpty) {
          callback(receivers.toList)
        }
      }
      loader.join
      loadingDock.clear
      signalQueue.clear
      debug("Launcher: All done!")
    }
  })

  def start(
    inputSockets: Seq[Socket],
    trueSocketOpt: Option[Socket] = None,
    falseSocketOpt: Option[Socket] = None,
    clocks: Seq[(Int, Socket)] = Nil
  ) {
    this.clocks ++= clocks.map({ case (milliseconds, socket) => new Thread(new Runnable {
      var value = false
      def run {
        while (!Thread.interrupted) {
          update(socket, value)
          value = !value
          safeSleep(milliseconds / 2)
        }
      }
    })})
    trueSocketOpt.foreach(socket => signalQueue.add(new Signal(socket, true, 0)))
    falseSocketOpt.foreach(socket => signalQueue.add(new Signal(socket, false, 0)))
    loader.start
    launcher.start
    this.clocks.foreach(_.start)
  }

  def update(socket: Socket, value: Boolean) {
    loadingDock.synchronized {
      if (!finished) {
        val signal = new Signal(socket, value, 1)
        loadingDock.add(signal)
        loadingDock.notifyAll
      }
    }
  }

  def stop {
    loadingDock.synchronized {
      debug("Quitting")
      finished = true
      loader.interrupt
      launcher.interrupt
      clocks.foreach(_.interrupt)
    }
  }
}
