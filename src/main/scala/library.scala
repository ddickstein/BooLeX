import language.higherKinds
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

package library {
  class MySeq[A](val _seq: Seq[A]) extends AnyVal {
    def mapBy[B](func: A => B): Map[B, A] = _seq.map(x => (func(x), x)).toMap
  }

  class MyMap[A, B](val _map: Map[A, B]) {
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
}

package object library {
  import scala.language.implicitConversions;
  implicit def seq2MySeq[A](_seq: Seq[A]) = new MySeq(_seq)
  implicit def map2MyMap[A, B](_map: Map[A, B]) = new MyMap(_map)
}



