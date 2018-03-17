package smash

import fastparse.all.Parsed
import scala.io.Source
import scalanative.native._

abstract class Interpreter extends Expand {

  final def interpret(parser: SmashParser,
                      cont: Option[Continuation],
                      line: String): Either[Continuation, Option[Int]] = {
    val p = cont.fold(parser.Line)(_.parser)

    p.parse(line) match {
      case Parsed.Success(Left(continuation), index) =>
        Left(continuation)

      case Parsed.Success(_, index) =>
        // got empty line or comment, do nothing
        Right(None)

      case failure @ Parsed.Failure(expected, index, extra) =>
        Console.err.println(failure)
        Right(None)
    }
  }

}

object Interpreter {

  object interactive extends Interpreter with Prompt {
    final def run(): Int = {
      val parser = new SmashParser()
      var last = 0
      var cont: Option[Continuation] = None
      var promptsize = 0

      val history = tilde("~/.smash_history")

      Zone { implicit Z =>
        if (native.readline.read_history(toCString(history)) != 0) {
          stdio.perror(toCString(s"${BuildInfo.name}: read_history"))
        }
      }

      val b = scala.util.control.Breaks
      import b.break
      import b.breakable

      breakable {
        while (true) {
          val p =
            if (cont.nonEmpty) {
              continuationPrompt(prefix = promptsize, indent = 1)
            } else {
              val p = prompt
              promptsize = p.replaceAll("\u001b\\[\\d+m", "").size
              p
            }

          Zone { implicit Z =>
            native.readline.readline(toCString(p)) match {
              case null =>
                // print final new line
                println()

                if (errno.errno != 0) {
                  stdio.perror(toCString(BuildInfo.name))
                }

                break()

              case line =>
                val xline = fromCString(line)

                if (xline.trim.nonEmpty && !xline.startsWith(" ")) {
                  // TODO HISTCONTROL ignorespace
                  native.readline.add_history(line)
                }

                try {
                  interpret(parser, cont, xline) match {
                    case Left(continuation) =>
                      cont = Some(continuation)

                    case Right(status) =>
                      cont = None
                      for (s <- status)
                        last = s
                  }
                } catch {
                  case e: Exception =>
                    Console.err.println(e.getMessage)
                    last = 1
                }

                stdlib.free(line)
            }
          }
        }
      }

      Zone { implicit Z =>
        if (native.readline.write_history(toCString(history)) != 0) {
          stdio.perror(toCString(s"${BuildInfo.name}: write_history"))
        }
      }

      last
    }
  }

  object noninteractive extends Interpreter {
    final def run(source: Source): Int = {
      val parser = new SmashParser()
      var last = 0
      var cont: Option[Continuation] = None

      for (line <- source.getLines) {
        // TODO expand print line with PS4 prompt
        interpret(parser, cont, line) match {
          case Left(continuation) =>
            cont = Some(continuation)

          case Right(status) =>
            cont = None
            for (s <- status)
              last = s
        }
      }

      last
    }
  }
}
