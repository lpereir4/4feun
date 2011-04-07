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
    def unital[A](a: A): List[A] = a :: Nil
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

object MonadicFunctions {
  // 6. Replace error("todo") with an implementation
  def sequence[M[_], A](as: List[M[A]], m: Monad[M]): M[List[A]] =
    as.foldRight(m.unital(Nil: List[A]))(
      (a: M[A], la: M[List[A]]) =>
        m.flatMap(a, (x: A) =>
          m.flatMap(la, (y: List[A]) => m.unital(x :: y))))

  // 7. Replace error("todo") with an implementation
  def fmap[M[_], A, B](a: M[A], f: A => B, m: Monad[M]): M[B] =
    m.flatMap(a, (aa: A) => m.unital(f(aa)))

  // 8. Replace error("todo") with an implementation
  def flatten[M[_], A](a: M[M[A]], m: Monad[M]): M[A] = m.flatMap(a, (ma: M[A]) => ma)

  // 9. Replace error("todo") with an implementation
  def apply[M[_], A, B](f: M[A => B], a: M[A], m: Monad[M]): M[B] =
    m.flatMap(f, (ff: A => B) => m.flatMap(a, (aa: A) => m.unital(ff(aa))))

  // 10. Replace error("todo") with an implementation
  def filterM[M[_], A](f: A => M[Boolean], as: List[A], m: Monad[M]): M[List[A]] =
    as.foldRight(m.unital(Nil: List[A]))(
      (a: A, la: M[List[A]]) =>
        m.flatMap(f(a),
          (b: Boolean) => if (b) la
          else m.flatMap(la, (tail: List[A]) => m.unital(a :: tail))))

  // 11. flatMap n times to produce a list
  def replicateM[M[_], A](n: Int, a: M[A], m: Monad[M]): M[List[A]] =
    (1 to n).foldRight(m.unital(Nil: List[A]))(
      (_, la: M[List[A]]) =>
        m.flatMap(a, (aa: A) => m.flatMap(la, (tail: List[A]) => m.unital(aa :: tail))))

  // 12. Replace error("todo") with an implementation
  def lift2[M[_], A, B, C](f: (A, B) => C, a: M[A], b: M[B], m: Monad[M]): M[C] =
    sys.error("todo")

  // lift3, lift4, etc. Interesting question: Can we have liftN?
}
