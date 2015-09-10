import java.util.concurrent.atomic.AtomicLong

import language.higherKinds
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.{
  Builder => MBuilder,
  HashMap => MHashMap,
  HashSet => MHashSet,
  ListBuffer => MListBuffer,
  Queue => MQueue,
  Set => MSet
}

package library {
  class Identity[A](val _value: A) extends AnyVal {
    def ==>(pred: => Boolean)(implicit ev: A =:= Boolean): Boolean = {
      return !_value || pred
    }

    def !=>(pred: => Boolean)(implicit ev: A =:= Boolean): Boolean = {
      return _value && !pred
    }

    def applyIf[B >: A](pred: => Boolean)(func: A => B): B = {
      return if (pred) func(_value) else _value
    }

    def applyIf[B >: A](cond: A => Boolean)(func: A => B): B = {
      return if (cond(_value)) func(_value) else _value
    }

    def optionally[B](block: => B)(implicit ev: A =:= Boolean): Option[B] = {
      return if (_value) Option(block) else None
    }

    def optionalList[B](block: => B)(implicit ev: A =:= Boolean): List[B] = {
      return if (_value) List(block) else Nil
    }
  }

  class MyString(val str: String) extends AnyVal {
    def decapitalize: String = if (str.nonEmpty && str.charAt(0).isUpper) {
      val chars = str.toCharArray
      chars(0) = chars(0).toLower
      new String(chars)
    } else {
      str
    }

    def toMixedCase: String = toCamelCase.decapitalize

    def toCamelCase: String = (for {
      word <- "_+".r.split(str)
      if !word.isEmpty
    } yield {
      word.toLowerCase.capitalize
    }).mkString("")
  }

  class MySeq[A](val _seq: Seq[A]) extends AnyVal {
    def mapBy[B](func: A => B): Map[B, A] = _seq.map(x => (func(x), x)).toMap

    def toMultiMap[T1, T2](
      implicit ev1: Seq[A] <:< Seq[(T1, T2)]
    ): Map[T1, List[T2]] = {
      val multiMap = MHashMap.empty[T1, MBuilder[T2, List[T2]]]
        .withDefault(x => List.newBuilder[T2])
      for ((x,y) <- _seq: Seq[(T1, T2)]) {
        multiMap(x) = multiMap(x) // register the key
        multiMap(x) += y
      }
      return multiMap.mapValues(_.result).toMap
    }

    def flatUnzip[T1, T2, CC1[T1], CC2[T2]](
      implicit ev1: Seq[A] <:< Seq[(CC1[T1], CC2[T2])],
      ev2: CC1[T1] <:< TraversableOnce[T1],
      ev3: CC2[T2] <:< TraversableOnce[T2],
      cbf1: CanBuildFrom[CC1[T1], T1, CC1[T1]],
      cbf2: CanBuildFrom[CC2[T2], T2, CC2[T2]]
    ): (CC1[T1], CC2[T2]) = {
      val list1 = cbf1()
      val list2 = cbf2()
      for ((xs, ys) <- _seq: Seq[(CC1[T1], CC2[T2])]) {
        list1 ++= xs
        list2 ++= ys
      }
      return (list1.result, list2.result)
    }
  }

  class MyMap[A, B](val _map: Map[A, B]) extends AnyVal {
    def invert[E, CC[E]](
      implicit ev1: B =:= CC[E],
      ev2: CC[E] <:< TraversableOnce[E],
      cbf: CanBuildFrom[CC[A], A, CC[A]]
    ): Map[E, CC[A]] = {
      val inverted = MHashMap.empty[E, MBuilder[A, CC[A]]]
      for {
        (key, values) <- _map
        value <- values.asInstanceOf[CC[E]]
      } {
        if (!inverted.contains(value)) {
          inverted += (value -> cbf())
        }
        inverted.get(value).foreach(_ += key)
      }
      return inverted.map({ case (k,v) => (k -> v.result) }).toMap
    }
  }

  class MyTuple2[T1, T2](val _tup: Tuple2[T1, T2]) extends AnyVal {
    def map1[A](func: T1 => A): Tuple2[A, T2] = (func(_tup._1), _tup._2)
    def map2[A](func: T2 => A): Tuple2[T1, A] = (_tup._1, func(_tup._2))
    def mapLeft[A](func: T1 => A): Tuple2[A, T2] = map1(func)
    def mapRight[A](func: T2 => A): Tuple2[T1, A] = map2(func)
    def toSeq(implicit ev: T1 =:= T2): Seq[T1] = Seq(
      _tup._1,
      _tup._2.asInstanceOf[T1]
    )
  }

  class MyTuple3[T1, T2, T3](val _tup: Tuple3[T1, T2, T3]) extends AnyVal {
    def map1[A](func: T1 => A): Tuple3[A, T2, T3] = {
      return (func(_tup._1), _tup._2, _tup._3)
    }

    def map2[A](func: T2 => A): Tuple3[T1, A, T3] = {
      return (_tup._1, func(_tup._2), _tup._3)
    }

    def map3[A](func: T3 => A): Tuple3[T1, T2, A] = {
      return (_tup._1, _tup._2, func(_tup._3))
    }

    def toSeq(
      implicit ev: T1 =:= T2,
      ev2: T2 =:= T3
    ): Seq[T1] = Seq(
      _tup._1,
      _tup._2.asInstanceOf[T1],
      _tup._3.asInstanceOf[T1]
    )
  }

  class MyTuple4[T1, T2, T3, T4](
    val _tup: Tuple4[T1, T2, T3, T4]
  ) extends AnyVal {
    def map1[A](func: T1 => A): Tuple4[A, T2, T3, T4] = {
      return (func(_tup._1), _tup._2, _tup._3, _tup._4)
    }

    def map2[A](func: T2 => A): Tuple4[T1, A, T3, T4] = {
      return (_tup._1, func(_tup._2), _tup._3, _tup._4)
    }

    def map3[A](func: T3 => A): Tuple4[T1, T2, A, T4] = {
      return (_tup._1, _tup._2, func(_tup._3), _tup._4)
    }

    def map4[A](func: T4 => A): Tuple4[T1, T2, T3, A] = {
      return (_tup._1, _tup._2, _tup._3, func(_tup._4))
    }

    def toSeq(
      implicit ev: T1 =:= T2,
      ev2: T2 =:= T3,
      ev3: T3 =:= T4
    ): Seq[T1] = Seq(
      _tup._1,
      _tup._2.asInstanceOf[T1],
      _tup._3.asInstanceOf[T1],
      _tup._4.asInstanceOf[T1]
    )
  }

  class Stabilizer[A <: Ordered[A]] {
    type entry = Entry

    object Entry {
      import scala.language.implicitConversions
      implicit def entry2Value(entry: Entry): A = entry._value
      implicit def value2Entry(value: A): Entry = new Entry(value)
      private val counter = new AtomicLong
    }

    class Entry private (val _value: A) extends Ordered[Entry] {
      private val id = Entry.counter.getAndIncrement
      def compare(that: Entry): Int = {
        val valueComparison = this._value.compare(that._value)
        if (valueComparison != 0) {
          return valueComparison
        } else if (this.id - that.id > 0) {
          return 1
        } else {
          return -1
        }
      }
      override def toString: String = _value.toString
    }
  }
}

package object library {
  import java.util.function.{
    BiConsumer,
    BiFunction,
    BiPredicate,
    Consumer,
    Function,
    Predicate,
    Supplier
  }

  import scala.language.implicitConversions

  implicit def seq2MySeq[A](_seq: Seq[A]): MySeq[A] = new MySeq(_seq)
  implicit def map2MyMap[A, B](_map: Map[A, B]): MyMap[A, B] = new MyMap(_map)
  implicit def value2Identity[A](_value: A): Identity[A] = new Identity(_value)
  implicit def string2MyString(str: String): MyString = new MyString(str)
  implicit def tuple22MyTuple2[A, B](_tup: Tuple2[A, B]): MyTuple2[A, B] = {
    return new MyTuple2(_tup)
  }

  implicit def tuple32MyTuple3[A, B, C](
    _tup: Tuple3[A, B, C]
  ): MyTuple3[A, B, C] = {
    return new MyTuple3(_tup)
  }

  implicit def tuple42MyTuple4[A, B, C, D](
    _tup: Tuple4[A, B, C, D]
  ): MyTuple4[A, B, C, D] = {
    return new MyTuple4(_tup)
  }

  implicit def scala2javaBiConsumer[T, U](
    func: (T, U) => Unit
  ): BiConsumer[T, U] = {
    return new BiConsumer[T, U] {
      override def accept(x:T, y:U): Unit = func(x, y)
    }
  }

  implicit def scala2javaBiFunction[T, U, R](
    func: (T, U) => R
  ): BiFunction[T, U, R] = {
    return new BiFunction[T, U, R] {
      override def apply(x:T, y:U): R = func(x, y)
    }
  }

  implicit def scala2javaBiPredicate[T, U](
    func: (T, U) => Boolean
  ): BiPredicate[T, U] = {
    return new BiPredicate[T, U] {
      override def test(x:T, y:U): Boolean = func(x, y)
    }
  }

  implicit def scala2javaConsumer[T](
    func: T => Unit
  ): Consumer[T] = {
    return new Consumer[T] {
      override def accept(x:T): Unit = func(x)
    }
  }

  implicit def scala2javaFunction[T, R](
    func: T => R
  ): Function[T, R] = {
    return new Function[T, R] {
      override def apply(x:T): R = func(x)
    }
  }

  implicit def scala2javaPredicate[T](
    func: T => Boolean
  ): Predicate[T] = {
    return new Predicate[T] {
      override def test(x:T): Boolean = func(x)
    }
  }

  implicit def scala2javaSupplier[T](
    func: => T
  ): Supplier[T] = {
    return new Supplier[T] {
      override def get: T = func
    }
  }

  def waitFor(cond: => Boolean, monitor: AnyRef) {
    monitor.synchronized {
      try {
        while (!cond) {
          monitor.wait
        }
      } catch {
        // restore interrupted signal
        case e: InterruptedException => Thread.currentThread.interrupt
      }
    }
  }

  def safeSleep(milliseconds: Long) {
    try {
      Thread.sleep(milliseconds)
    } catch {
      // restore interrupted signal
      case e: InterruptedException => Thread.currentThread.interrupt
    }
  }

  // Precondition: Graph contains no cycles
  def topologicalSort[A, CC[A] <: TraversableOnce[A]](
    dependencyGraph: Map[A, CC[A]]
  ): Seq[A] = {
    val nodeSet = dependencyGraph.keys.toSet
    // ensure that the only dependencies that are counted are the ones that have
    // real nodes associated with them
    val mutableGraph = MHashMap.empty[A, MHashSet[A]] ++
      nodeSet.map(_ -> MHashSet.empty[A]).toMap ++
      new MySeq(for {
        (node, dependencies) <- dependencyGraph.toList
        dependency <- dependencies
        if nodeSet.contains(dependency)
      } yield {
        (node -> dependency)
      }).toMultiMap.mapValues(MHashSet.empty[A] ++ _)

    val linearizedDependencies = MListBuffer.empty[A]
    val independentNodes = MSet.empty[A]
    independentNodes ++= mutableGraph
      .filter({ case (_, deps) => deps.isEmpty })
      .keys
    mutableGraph --= independentNodes
    while (independentNodes.nonEmpty) {
      val node = independentNodes.head
      independentNodes -= node
      linearizedDependencies += node
      mutableGraph.values.foreach(_ -= node)
      val newIndependentNodes = mutableGraph
        .filter({ case (_, deps) => deps.isEmpty })
        .keys
      mutableGraph --= newIndependentNodes
      independentNodes ++= newIndependentNodes
    }
    return linearizedDependencies.toList
  }

  def getTotalDependencyMap[A](
    dependencyGraph: Map[A, Set[A]]
  ): Map[A, Set[A]] = {
    val nodeSet = dependencyGraph.keys.toSet
    for ((node, dependencies) <- dependencyGraph) yield {
      val processedDependencies = MHashSet.empty[A]
      val unprocessedDependencies = MQueue.empty[A] ++ dependencies
      while (unprocessedDependencies.nonEmpty) {
        val dependency = unprocessedDependencies.dequeue
        processedDependencies += dependency
        unprocessedDependencies ++=
          dependencyGraph(dependency) &~ processedDependencies
      }
      node -> (processedDependencies & nodeSet).toSet
    }
  }
}
