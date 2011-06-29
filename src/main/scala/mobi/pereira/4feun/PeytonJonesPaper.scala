package mobi.pereira.`4feun`

trait World {

  def println(s: String): Unit

  def readLine(): String

}

object ConsoleWorld extends World {

  override def println(s: String): Unit = Console.println(s)

  override def readLine(): String = Console.readLine()

}

object MockedWorld extends World {

  var in: List[String] = List("Foo", "Bar")
  var out: List[String] = Nil

  override def println(s: String) {
    out = s :: out
  }

  override def readLine(): String = in match {
    case Nil => sys.error("This is the apocamix.")
    case head :: tail => {
      in = tail
      head
    }
  }

}

object PeytonJonesPaper {

  type IO[A] = World => (A, World)

  def getString: IO[String] = { w =>
    (w.readLine(), w)
  }

  def putString: String => IO[Unit] = { s =>
    w => (w.println(s), w)
  }

  def bind[A, B]: IO[A] => (A => IO[B]) => IO[B] = { ioa =>
    g => w => {
      val (a, newW) = ioa(w)
      g(a)(newW)
    }
  }

  def echo: IO[Unit] = bind(getString)(putString)

  def echoTwice = then(echo)(echo)

  def printTwice = bind(getString)(x => then(putString(x))(putString(x)))

  def mixed = bind(getString)(x => then(putString(x))(bind(getString)(x => then(putString(x))(putString(x)))))

  def then[A, B]: IO[A] => IO[B] => IO[B] = { ioa =>
    iob => bind(ioa)(_ => iob)
  }

  def main(args: Array[String]) {
    mixed(MockedWorld)._2 match {
      case MockedWorld => {
        println("IN:")
        MockedWorld.in.reverse foreach println
        println("==============")
        println("OUT:")
        MockedWorld.out.reverse foreach println
      }
      case _ => ()
    }
  }
}
