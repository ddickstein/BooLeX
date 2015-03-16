package identity {
  class Identity[A](val _value: A) extends AnyVal {
    def ==>(pred: => Boolean)(implicit ev: A =:= Boolean): Boolean = !_value || pred;
    def !=>(pred: => Boolean)(implicit ev: A =:= Boolean): Boolean = _value && !pred;
  
    def applyIf[B >: A](pred: => Boolean)(func: A => B): B = if (pred) func(_value) else _value
    def applyIf[B >: A](cond: A => Boolean)(func: A => B): B = if (cond(_value)) func(_value) else _value

    def optionally[B](block: => B)(implicit ev: A =:= Boolean): Option[B] = if (_value) Some(block) else None
  }
}

package object identity {
  import scala.language.implicitConversions;
  implicit def value2Identity[A](_value: A) = new Identity(_value)
  implicit def identity2Value[A](id: Identity[A]) = id._value
}
