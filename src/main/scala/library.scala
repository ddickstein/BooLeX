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
}

package object library {
  import scala.language.implicitConversions;
  implicit def seq2MySeq[A](_seq: Seq[A]) = new MySeq(_seq)
  implicit def map2MyMap[A, B](_map: Map[A, B]) = new MyMap(_map)
  implicit def tuple22MyTuple2[A, B](_tup: Tuple2[A, B]) = new MyTuple2(_tup)
  implicit def value2Identity[A](_value: A) = new Identity(_value)

  val debugging = true
  def debug(msg: String): Unit = if (debugging) { println(Console.YELLOW + msg + Console.RESET) }
}



