package smash

import scalanative.native._
import scalanative.posix

trait Expand {

  final def expand(argument: AST.Argument): String =
    (tilde andThen variables)(argument.parts).mkString("")

  final val tilde: Function[List[AST.Part], List[AST.Part]] =
    parts =>
      parts match {
        case AST.Part.Plain(s) :: tail if s startsWith "~" =>
          AST.Part.Plain(tilde(s)) :: tail

        case _ =>
          parts
    }

  final def tilde(path: String): String =
    if (path.startsWith("~")) {
      val home: String = sys.env("HOME")
      path.replaceFirst("~", home)
    } else {
      path
    }

  final val variables: Function[List[AST.Part], List[String]] =
    _ map {
      case AST.Part.Plain(text)         => text
      case AST.Part.SingleQuoted(text)  => text
      case AST.Part.DoubleQuoted(parts) => variables(parts).mkString("")
      case AST.Part.Var(name, _)        => variable(name)
    }

  final def variable(name: String): String =
    Zone { implicit z =>
      val n = toCString(name)
      val o = stdlib.getenv(n) match {
        case null  => None
        case value => Some(fromCString(value))
      }
      o.getOrElse("")
    }

}
