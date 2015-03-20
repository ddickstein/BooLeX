import java.util.concurrent.atomic.AtomicLong

import language.higherKinds
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

package library {
  class Identity[A](val _value: A) extends AnyVal {
    def ==>(pred: => Boolean)(implicit ev: A =:= Boolean): Boolean = !_value || pred;
    def !=>(pred: => Boolean)(implicit ev: A =:= Boolean): Boolean = _value && !pred;
  
    def applyIf[B >: A](pred: => Boolean)(func: A => B): B = if (pred) func(_value) else _value
    def applyIf[B >: A](cond: A => Boolean)(func: A => B): B = if (cond(_value)) func(_value) else _value

    def optionally[B](block: => B)(implicit ev: A =:= Boolean): Option[B] = if (_value) Some(block) else None
  }

  class MySeq[A](val _seq: Seq[A]) extends AnyVal {
    def mapBy[B](func: A => B): Map[B, A] = _seq.map(x => (func(x), x)).toMap
  }

  class MyMap[A, B](val _map: Map[A, B]) extends AnyVal {
    def invert[E, CC[E]](
      implicit ev1: B =:= CC[E],
      ev2: CC[E] <:< TraversableOnce[E],
      cbf: CanBuildFrom[CC[A], A, CC[A]]
    ): Map[E, CC[A]] = {
      val inverted = scala.collection.mutable.Map.empty[E, Builder[A, CC[A]]]
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

  class MyTuple2[A, B](val _tup: Tuple2[A, B]) extends AnyVal {
    def mapLeft[C](func: A => C): Tuple2[C, B] = (func(_tup._1) -> _tup._2)
    def mapRight[C](func: B => C): Tuple2[A, C] = (_tup._1 -> func(_tup._2))
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
  import java.util.function.{BiConsumer, BiFunction, BiPredicate, Consumer, Function, Predicate, Supplier}
  import scala.language.implicitConversions
  implicit def seq2MySeq[A](_seq: Seq[A]) = new MySeq(_seq)
  implicit def map2MyMap[A, B](_map: Map[A, B]) = new MyMap(_map)
  implicit def tuple22MyTuple2[A, B](_tup: Tuple2[A, B]) = new MyTuple2(_tup)
  implicit def value2Identity[A](_value: A) = new Identity(_value)

  implicit def scala2javaBiConsumer[T, U](func: (T, U) => Unit): BiConsumer[T, U] = new BiConsumer[T, U] {
    override def accept(x:T, y:U): Unit = func(x, y)
  }

  implicit def scala2javaBiFunction[T, U, R](func: (T, U) => R): BiFunction[T, U, R] = new BiFunction[T, U, R] {
    override def apply(x:T, y:U): R = func(x, y)
  }

  implicit def scala2javaBiPredicate[T, U](func: (T, U) => Boolean): BiPredicate[T, U] = new BiPredicate[T, U] {
    override def test(x:T, y:U): Boolean = func(x, y)
  }

  implicit def scala2javaConsumer[T](func: T => Unit): Consumer[T] = new Consumer[T] {
    override def accept(x:T): Unit = func(x)
  }

  implicit def scala2javaFunction[T, R](func: T => R): Function[T, R] = new Function[T, R] {
    override def apply(x:T): R = func(x)
  }

  implicit def scala2javaPredicate[T](func: T => Boolean): Predicate[T] = new Predicate[T] {
    override def test(x:T): Boolean = func(x)
  }

  implicit def scala2javaSupplier[T](func: => T): Supplier[T] = new Supplier[T] {
    override def get: T = func
  }

  val debugging1 = false
  val debugging2 = false
  val debugging3 = true
  def debug(msg: String): Unit = if (debugging1) { println(Console.YELLOW + msg + Console.RESET) }
  def debug2(msg: String): Unit = if (debugging2) { println(Console.MAGENTA + msg + Console.RESET) }
  def debug3(msg: String): Unit = if (debugging3) { println(Console.CYAN + msg + Console.RESET) }
}
