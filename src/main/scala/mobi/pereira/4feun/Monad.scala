package mobi.pereira.`4feun`

trait Monad[M[_], +A] {

  def unit[A](a: A): M[A]

  def map[B](f: A => B): M[B]

  def flatMap[B](f: A => M[B]): M[B]

}
