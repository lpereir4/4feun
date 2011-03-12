package mobi.pereira.`4feun`

object Main {

  def getLogin = () => Some("lpereira")
  def getPassword = () => None

  def transform(line: String) = "{ " + line + " }"

  def main(args: Array[String]) {
    val infos = for {
      login <- getLogin()
      password <- getPassword()
    } yield (login, password)

    lazy val program: Function1[Unit,IOMonad[_]] = (Unit) => for {
      _ <- WriteLn("Bonjour,")
      _ <- Write("veuillez saisir votre login :")
      l <- ReadLn
      _ <- Write("veuillez saisir votre mot de passe :")
      p <- ReadLn
      tmp = for {
        login <- if ("" == l) None else Some(l)
        password <- if ("" == p) None else Some(p)
      } yield (login, password)
      creds <- tmp match {
        case Some((login, password)) => Const(println("Bienvenue " + login))
        case _ => {
          println("Données erronées.")
          program()
        }
      }
    } yield creds

    val a = program()
    println(a)
  }
}
