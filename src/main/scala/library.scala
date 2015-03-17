package library {
  class MySeq[A](val _seq: Seq[A]) extends AnyVal {
    def mapBy[B](func: A => B): Map[B, A] = _seq.map(x => (func(x), x)).toMap
  }
}

package object library {
  import scala.language.implicitConversions;
  implicit def seq2MySeq[A](_seq: Seq[A]) = new MySeq(_seq)
}
