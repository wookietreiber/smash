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
  def execute(arguments: List[String]): Int
}

object Builtin {

  val builtins: Map[String, Builtin] =
    Map(("cd", cd), ("echo", echo), ("exit", exit), ("pwd", pwd))

  def unapply(name: String): Option[Builtin] =
    builtins.get(name)

  case object cd extends Builtin("cd") {
    def execute(path: String): Int = Zone { implicit Z =>
      val dir = toCString(path)

      val old = native.std.get_current_dir_name()

      val ret = if (posix.unistd.chdir(dir) == -1) {
        errno.errno match {
          case e if e == posix.errno.ENOENT =>
            Console.err.println(
              s"$name: $path: this is not the directory you are looking for"
            )

          case e if e == posix.errno.ENOTDIR =>
            Console.err.println(
              s"$name: $path: only Chuck Norris can chdir here"
            )

          case e if e == posix.errno.EACCES =>
            Console.err.println(
              s"$name: $path: you are just a padawan"
            )

          case errno =>
            stdio.perror(toCString(s"${BuildInfo.name}: $name: $path"))
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

    def execute(arguments: List[String]): Int = arguments match {
      case Nil | List("~") =>
        execute(fromCString(stdlib.getenv(c"HOME")))

      case List("-") =>
        execute(fromCString(stdlib.getenv(c"OLDPWD")))

      case List(path) =>
        execute(path)

      case _ =>
        Console.err.println(s"${BuildInfo.name}: ${name}: too many arguments")
        1
    }
  }

  case object echo extends Builtin("echo") {
    def execute(arguments: List[String]): Int = {
      println(arguments.mkString(" "))
      0
    }
  }

  case object exit extends Builtin("exit") {
    def execute(arguments: List[String]): Int = arguments match {
      case Nil =>
        // TODO should use exit status of previous command
        // TODO check $?
        scala.sys.exit(0)

      case List(code) =>
        util.Try(code.toInt).toOption.filter(_ >= 0).filter(_ <= 255) match {
          case Some(code) =>
            scala.sys exit code
          case None =>
            Console.err.println(
              s"${BuildInfo.name}: ${name}: not a valid number")
            1
        }

      case _ =>
        Console.err.println(s"${BuildInfo.name}: ${name}: too many arguments")
        1
    }
  }

  case object pwd extends Builtin("pwd") {
    def execute(arguments: List[String]): Int = arguments match {
      case Nil =>
        val wd = native.std.get_current_dir_name()
        val dir = fromCString(wd)
        Console.out.println(s"$dir")
        stdlib.free(wd)
        0

      case _ =>
        Console.err.println(s"${BuildInfo.name}: ${name}: too many arguments")
        1
    }
  }

}
