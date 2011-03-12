package mobi.pereira.`4feun`

sealed abstract class Option[+A] extends Monad[Option, A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case _ => None
  }
}

case class Some[A](a: A) extends Option[A] {
  def unit[A](value: A): Option[A] = Some(value)
}

case object None extends Option[Nothing] {
  def unit[Nothing](value: Nothing): Option[Nothing] = None
}
