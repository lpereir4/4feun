package mobi.pereira.console

object S99 {

  def main(args: Array[String]) {
    println(last(List(1, 1, 2, 3, 5, 8)))
    println(penultimate(List(1, 1, 2, 3, 5, 8)))
    println(nth(2, List(1, 1, 2, 3, 5, 8)))
    println(length(List(1, 1, 2, 3, 5, 8)))
    println(reverse(List(1, 1, 2, 3, 5, 8)))
    println(isPalindrome(List(1, 2, 1)))
    println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
    println(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))
    println(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(duplicate(List('a, 'b, 'c, 'c, 'd)))
    println(duplicateN(3, List('a, 'b, 'c, 'c, 'd)))
    println(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  /* P01 (*) Find the last element of a list.
   * Example:
   * 
   * scala> last(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 8
   */
  def last[A](l: List[A]): Option[A] = l match {
    case Nil => None
    case l :: Nil => Some(l)
    case head :: tail => last(tail)
  }

  /* P02 (*) Find the last but one element of a list.
   * Example:
   *
   * scala> penultimate(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 5
   */
  def penultimate[A](l: List[A]): Option[A] = l match {
    case Nil => None
    case a :: Nil => None
    case a :: b :: Nil => Some(a)
    case head :: tail => penultimate(tail)
  }

  /* P03 (*) Find the Kth element of a list.
   * By convention, the first element in the list is element 0.
   * Example:
   *
   * scala> nth(2, List(1, 1, 2, 3, 5, 8))
   * res0: Int = 2
   */
  @annotation.tailrec
  def nth[A](n: Int, l: List[A]): Option[A] = (n, l) match {
    case (0, head :: tail) => Some(head)
    case (_, head :: tail) => nth(n - 1, tail)
    case _ => None
  }

  /* P04 (*) Find the number of elements of a list.
   * Example:
   *
   * scala> length(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 6
   */
  def length[A](l: List[A]): Int = {
    @annotation.tailrec
    def lengthTC[A](l: List[A], accu: Int): Int = l match {
      case Nil => accu
      case head :: tail => lengthTC(tail, accu + 1)
    }
    lengthTC(l, 0)
  }

  /* P05 (*) Reverse a list.
   * Example:
   *
   * scala> reverse(List(1, 1, 2, 3, 5, 8))
   * res0: List[Int] = List(8, 5, 3, 2, 1, 1)
   */
  def reverse[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def reverseTC[A](l: List[A], accu: List[A]): List[A] = l match {
      case Nil => accu
      case head :: tail => reverseTC(tail, head :: accu)
    }
    reverseTC(l, Nil)
  }

  /* P06 (*) Find out whether a list is a palindrome.
   * Example:
   *
   * scala> isPalindrome(List(1, 2, 3, 2, 1))
   * res0: Boolean = true
   */
  def isPalindrome[A](l: List[A]): Boolean = {
    l equals reverse(l)
  }

  /* P07 (**) Flatten a nested list structure.
   * Example:
   *
   * scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
   * res0: List[Any] = List(1, 1, 2, 3, 5, 8)
   */
  def flatten(l: List[_]): List[Any] = {
    @annotation.tailrec
    def flattenTC(l: List[_], accu: List[Any]): List[Any] = l match {
      case Nil => accu
      case (head: List[_]) :: tail => flattenTC(tail, flatten(head) ::: accu)
      case head :: tail => flattenTC(tail, head :: accu)
    }
    flattenTC(l, Nil)
  }

  /* P08 (**) Eliminate consecutive duplicates of list elements.
   * If a list contains repeated elements they should be replaced
   * with a single copy of the element.
   * The order of the elements should not be changed.
   * Example:
   *
   * scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
   */
  def compress[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def compressTC[A](l: List[A], accu: List[A]): List[A] = l match {
      case Nil => reverse(accu)
      case head :: tail =>
        if (accu != Nil && (accu.head equals head)) compressTC(tail, accu)
        else compressTC(tail, head :: accu)
    }
    compressTC(l, Nil)
  }

  /* P09 (**) Pack consecutive duplicates of list elements into sublists.
   * If a list contains repeated elements they should be placed in separate sublists.
   * Example:
   *
   * scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
   */
  def pack[A](l: List[A]): List[List[A]] = {
    @annotation.tailrec
    def packTC[A](l: List[A], accu1: List[A], accu2: List[List[A]]): List[List[A]] = l match {
      case Nil => reverse(accu1 :: accu2)
      case head :: tail =>
        if (accu1 == Nil || (accu1.head equals head)) packTC(tail, head :: accu1, accu2)
        else packTC(tail, head :: Nil, accu1 :: accu2)
    }
    packTC(l, Nil, Nil)
  }

  /* P10 (*) Run-length encoding of a list.
   * Use the result of problem P09 to implement the so-called run-length encoding
   * data compression method. Consecutive duplicates of elements are encoded as
   * tuples (N, E) where N is the number of duplicates of the element E.
   * Example:
   *
   * scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   */
  def encode[A](l: List[A]): List[(Int, A)] = pack(l).map(l => (length(l), l.head))

  /* P11 (*) Modified run-length encoding.
   * Modify the result of problem P10 in such a way that if an element has no
   * duplicates it is simply copied into the result list. Only elements with
   * duplicates are transferred as (N, E) terms.
   * Example:
   *
   * scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
   */
  def encodeModified[A](l: List[A]): List[Any] = pack(l).map({ l =>
    if (1 == length(l)) l.head
    else (length(l), l.head)
  })

  /* P12 (**) Decode a run-length encoded list.
   * Given a run-length code list generated as specified in problem P10, construct
   * its uncompressed version.
   * Example:
   *
   * scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
   * res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
   */
  def decode[A](l: List[(Int, A)]): List[A] = {
    @annotation.tailrec
    def decodeTC[A](l: List[(Int, A)], accu: List[A]): List[A] = l match {
      case Nil => reverse(accu)
      case head :: tail => decodeTC(tail, (1 to head._1).map(_ => head._2).toList ::: accu)
    }
    decodeTC(l, Nil)
  }

  /* P13 (**) Run-length encoding of a list (direct solution).
   * Implement the so-called run-length encoding data compression
   * method directly. I.e. don't use other methods you've written
   * (like P09's pack); do all the work directly.
   * Example:
   *
   * scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   */
  def encodeDirect[A](l: List[A]): List[(Int, A)] = {
    @annotation.tailrec
    def join[A](l: List[A], accu: (Int, A)): (List[A], (Int, A)) = l match {
      case Nil => (Nil, accu)
      case head :: tail => {
        if (head == accu._2) join(tail, (accu._1 + 1, accu._2))
        else (l, accu)
      }
    }

    @annotation.tailrec
    def encodeDirectTC[A](l: List[A], accu: List[(Int, A)]): List[(Int, A)] =
      l match {
        case Nil => reverse(accu)
        case head :: tail => {
          val (remaining, chunk) = join(tail, (1, head))
          encodeDirectTC(remaining, chunk :: accu)
        }
      }
    encodeDirectTC(l, Nil)
  }

  /* P14 (*) Duplicate the elements of a list.
   * Example:
   *
   * scala> duplicate(List('a, 'b, 'c, 'c, 'd))
   * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
   */
  def duplicate[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def duplicateTC[A](l: List[A], accu: List[A]): List[A] = l match {
      case Nil => reverse(accu)
      case head :: tail => duplicateTC(tail, head :: head :: accu)
    }
    duplicateTC(l, Nil)
  }

  /* P15 (**) Duplicate the elements of a list a given number of times.
   * Example:
   *
   * scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
   * res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
   */
  def duplicateN[A](n: Int, l: List[A]): List[A] = {
    @annotation.tailrec
    def duplicateNTC[A](l: List[A], accu: List[A]): List[A] = l match {
      case Nil => reverse(accu)
      case head :: tail => duplicateNTC(tail, (1 to n).map(_ => head).toList ::: accu)
    }
    duplicateNTC(l, Nil)
  }

  /* P16 (**) Drop every Nth element from a list.
   * Example:
   *
   * scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
   */
  def drop[A](n: Int, l: List[A]): List[A] = {
    @annotation.tailrec
    def dropTC[A](i: Int, l: List[A], accu: List[A]): List[A] = l match {
      case Nil => reverse(accu)
      case head :: tail => {
        if (0 == i) dropTC(n - 1, tail, accu)
        else dropTC(i - 1, tail, head :: accu)
      }
    }
    dropTC(n - 1, l, Nil)
  }

  /* P17 (*) Split a list into two parts.
   * The length of the first part is given. Use a Tuple for your result.
   * Example:
   *
   * scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   */
  def split[A](n: Int, l: List[A]): (List[A], List[A]) = {
    @annotation.tailrec
    def splitTC[A](n: Int, l: List[A], accu: List[A]): (List[A], List[A]) = (n, l) match {
      case (_, Nil) => (reverse(accu), l)
      case (0, _) => (reverse(accu), l)
      case (_, head :: tail) => splitTC(n - 1, tail, head :: accu)
    }
    splitTC(n, l, Nil)
  }

  /* P18 (**) Extract a slice from a list.
   * Given two indices, I and K, the slice is the list containing the elements
   * from and including the Ith element up to but not including the Kth element
   * of the original list. Start counting the elements with 0.
   *
   * Example:
   * scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: List[Symbol] = List('d, 'e, 'f, 'g)
   */
  def slice[A](start: Int, end: Int, l: List[A]): List[A] = {
    split(end - start, split(start, l)._2)._1
  }

  /* P19 (**) Rotate a list N places to the left.
   * Examples:
   *
   * scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
   *
   * scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
   */
  def rotate[A](n: Int, l: List[A]): List[A] = {
    val size = length(l)
    val (second, first) = split((size + n) % size, l)
    first ::: second
  }
}
