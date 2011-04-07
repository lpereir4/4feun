package mobi.pereira.`4feun`.exo2

// 1. Start here. Observe this trait
trait Monad[M[_]] {
  def flatMap[A, B](a: M[A], f: A => M[B]): M[B]
  def unital[A](a: A): M[A]
}

// A simple data type, which turns out to satisfy the above trait
case class Inter[A](f: Int => A)

// So does this.
case class Identity[A](a: A)

// Monad implementations
object Monad {
  // 2. Replace error("todo") with an implementation
  def ListMonad: Monad[List] = new Monad[List] {
    def flatMap[A, B](a: List[A], f: A => List[B]): List[B] =
      a.map(f).foldLeft[List[B]](Nil)(_ ::: _)
    def unital[A](a: A): List[A] = a::Nil
  }

  // 3. Replace error("todo") with an implementation
  def OptionMonad: Monad[Option] = new Monad[Option] {
    def unital[A](a: A): Option[A] = Some(a)
    def flatMap[A, B](a: Option[A], f: A => Option[B]): Option[B] = a.flatMap(f)
  }

  // 4. Replace error("todo") with an implementation
  def InterMonad: Monad[Inter] = new Monad[Inter] {
    def unital[A](a: A): Inter[A] = Inter(_ => a)
    def flatMap[A, B](a: Inter[A], f2: A => Inter[B]): Inter[B] = Inter(i => f2(a.f(i)).f(i))
  }

  // 5. Replace error("todo") with an implementation
  def IdentityMonad: Monad[Identity] = new Monad[Identity] {
    def unital[A](a: A): Identity[A] = Identity(a)
    def flatMap[A, B](a: Identity[A], f: A => Identity[B]): Identity[B] = f(a.a)
  }
}
