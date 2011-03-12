package mobi.pereira.`4feun`

sealed abstract class IOMonad[+A] extends Monad[IOMonad, A] {
  def unit[A](value: A): IOMonad[A] = Const(value)
}

case class Const[A](a: A) extends IOMonad[A] {
  def map[B](f: A => B): IOMonad[B] = unit(f(a))

  def flatMap[B](f: A => IOMonad[B]): IOMonad[B] = f(a)
}

case object ReadLn extends IOMonad[String] {
  def map[B](f: String => B): IOMonad[B] = unit(f(readLine()))

  def flatMap[B](f: String => IOMonad[B]): IOMonad[B] = f(readLine())
}

case class WriteLn(a: String) extends IOMonad[Unit] {
  def map[B](f: (Unit) => B): IOMonad[B] = {
    println(a)
    unit(f())
  }

  def flatMap[B](f: (Unit) => IOMonad[B]): IOMonad[B] = {
    println(a)
    f()
  }
}

case class Write(a: String) extends IOMonad[Unit] {
  def map[B](f: (Unit) => B): IOMonad[B] = {
    print(a)
    unit(f())
  }

  def flatMap[B](f: (Unit) => IOMonad[B]): IOMonad[B] = {
    print(a)
    f()
  }
}


