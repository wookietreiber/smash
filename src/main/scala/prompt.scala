package smash

import scalanative.native._
import scalanative.posix

import scala.io.AnsiColor._

trait Prompt {

  final private def hostname: String = {
    val size = 1024
    val buf = stackalloc[CChar](size)

    if (posix.unistd.gethostname(buf, size - 1) == 0) {
      fromCString(buf)
    } else {
      Zone { implicit Z =>
        stdio.perror(toCString(s"${BuildInfo.name}: gethostname"))
      }
      "localhost"
    }
  }

  final def prompt: String = {
    val home = fromCString(stdlib.getenv(c"HOME"))
    val user = fromCString(stdlib.getenv(c"USER"))

    val wd = native.std.get_current_dir_name()
    val cwd = fromCString(wd).replaceFirst(home, "~")
    stdlib.free(wd)

    s"""${CYAN}${user}@${hostname} ${cwd} $$${RESET} """
  }

  final def continuationPrompt(prefix: Int, indent: Int): String =
    " " * (prefix + indent * 4)

}
