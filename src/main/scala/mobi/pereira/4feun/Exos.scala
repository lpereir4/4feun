package mobi.pereira.`4feun`

// http://blog.tmorris.net/understanding-monads-using-scala-part-1/

// A typical data type with a single abstract method
case class Inter[A](f: Int => A) {
  // which is a functor
  def map[B](g: A => B): Inter[B] =
    Inter(n => g(f(n)))

  // and a monad (see unital below)
  def flatMap[B](g: A => Inter[B]): Inter[B] =
    Inter(n => g(f(n)).f(n))
}

// unital: A => F[A]
// Implementations for F=Option and F=Inter
object Unitals {
  def unitalOption[A](a: A): Option[A] =
    Some(a)

  def unitalInter[A](a: A): Inter[A] =
    Inter(_ => a)
}

// Exercises
// 
// It is recommended to use only map, flatMap and unital* for
// Option or Inter when implementing the exercises below.
// Any other libraries are acceptable (e.g. List functions).
object Sequencing {
  import Unitals._

  // Exercise 1 of 3
  // ===============
  // Implement a function that returns None if the given list
  // contains any None values, otherwise, all the Some values.
  def sequenceOption[A](x: List[Option[A]]): Option[List[A]] =
    x.foldRight[Option[List[A]]](unitalOption(Nil))((o: Option[A], ol: Option[List[A]]) =>
      ol.flatMap(
        list => o.map(_ :: list)))

  // Exercise 2 of 3
  // ===============
  // Implement a function that returns an Inter that applies an Int
  // to all the Inter implementations in the List of Inters and returns
  // all the results.
  def sequenceInter[A](x: List[Inter[A]]): Inter[List[A]] =
    x.foldRight[Inter[List[A]]](unitalInter(Nil))((i: Inter[A], il: Inter[List[A]]) =>
      il.flatMap(
        list => i.map(_ :: list)))

  def main(args: Array[String]) {
    def assertEquals[A](a1: A, a2: A) {
      if (a1 != a2)
        sys.error("Assertion error. Expected: " + a1 + " Actual: " + a2)
    }

    def assertInterEquals[A](a1: Inter[A], a2: Inter[A]) {
      val testInts = List(1, 2, 0, -7, -9, 113, -2048)
      assertEquals(testInts.map(a1.f(_)), testInts.map(a2.f(_)))
    }

    // sequenceOption
    assertEquals(sequenceOption(List(Some(7),
      Some(8), Some(9))), Some(List(7, 8, 9)))
    assertEquals(sequenceOption(List(Some(7), None, Some(9))),
      None)
    assertEquals(sequenceOption(List()),
      Some(List()))

    // sequenceInter
    assertInterEquals(sequenceInter(List()),
      Inter(_ => List()))
    assertInterEquals(sequenceInter(List(Inter(1+),
      Inter(2*))), Inter(n => List(1 + n, 2 * n)))
  }
}
