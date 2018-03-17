package smash

import scalanative.native._
import scalanative.posix

/*

 $ function foo { echo foo ; echo bar >&2 ; }
 $ foo
 foo
 bar
 $ foo 2> /dev/null
 foo
 $ foo 2> /asoetuh
 bash: /asoetuh: Permission denied

 */

sealed abstract class Builtin(val name: String) {
  // TODO should also have environment/vars as implicit argument
  def execute(arguments: List[String], streams: Streams): Int
}

object Builtin {

  val builtins: Map[String, Builtin] =
    Map(("cd", cd), ("echo", echo), ("exit", exit), ("pwd", pwd))

  def unapply(name: String): Option[Builtin] =
    builtins.get(name)

  case object cd extends Builtin("cd") {
    def execute(path: String, streams: Streams): Int = Zone { implicit Z =>
      import streams._

      val dir = toCString(path)

      val old = native.std.get_current_dir_name()

      val ret = if (posix.unistd.chdir(dir) == -1) {
        errno.errno match {
          case e if e == posix.errno.ENOENT =>
            err.println(
              s"$name: $path: this is not the directory you are looking for"
            )

          case e if e == posix.errno.ENOTDIR =>
            err.println(
              s"$name: $path: only Chuck Norris can chdir here"
            )

          case e if e == posix.errno.EACCES =>
            err.println(
              s"$name: $path: you are just a padawan"
            )

          case errno =>
            val msg = fromCString(string.strerror(errno))
            err.println(s"${BuildInfo.name}: $name: $path $msg")
        }
        1
      } else {
        val wd = native.std.get_current_dir_name()
        posix.stdlib.setenv(c"PWD", wd, 1)
        stdlib.free(wd)
        posix.stdlib.setenv(c"OLDPWD", old, 1)
        0
      }

      stdlib.free(old)

      ret
    }

    def execute(arguments: List[String], streams: Streams): Int =
      arguments match {
        case Nil | List("~") =>
          execute(fromCString(stdlib.getenv(c"HOME")), streams)

        case List("-") =>
          execute(fromCString(stdlib.getenv(c"OLDPWD")), streams)

        case List(path) =>
          execute(path, streams)

        case _ =>
          streams.err.println(
            s"${BuildInfo.name}: ${name}: too many arguments")
          1
      }
  }

  case object echo extends Builtin("echo") {
    def execute(arguments: List[String], streams: Streams): Int = {
      streams.out.println(arguments.mkString(" "))
      0
    }
  }

  case object exit extends Builtin("exit") {
    def execute(arguments: List[String], streams: Streams): Int = {
      import streams._

      val ret = arguments match {
        case Nil =>
          // TODO should use exit status of previous command
          // TODO check $?
          scala.sys.exit(0)

        case List(code) =>
          // TODO this should be checked in parser
          util.Try(code.toInt).toOption.filter(_ >= 0).filter(_ <= 255) match {
            case Some(code) =>
              scala.sys exit code
            case None =>
              err.println(s"${BuildInfo.name}: ${name}: not a valid number")
              1
          }

        case _ =>
          err.println(s"${BuildInfo.name}: ${name}: too many arguments")
          1
      }

      ret
    }
  }

  case object pwd extends Builtin("pwd") {
    def execute(arguments: List[String], streams: Streams): Int = {
      import streams._

      val ret = arguments match {
        case Nil =>
          val wd = native.std.get_current_dir_name()
          val dir = fromCString(wd)
          out.println(s"$dir")
          stdlib.free(wd)
          0

        case _ =>
          err.println(s"${BuildInfo.name}: ${name}: too many arguments")
          1
      }

      ret
    }
  }

}
