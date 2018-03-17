package smash

import scalanative.native._
import scalanative.posix

// TODO should also have environment/vars as implicit argument

final case class Command(name: String) {
  private def toCArray(a: IndexedSeq[String])(
      implicit Z: Zone): Ptr[CString] = {
    val ptr: Ptr[CString] = alloc[CString](a.size + 1)
    var i = 0
    while (i < a.size) {
      ptr(i) = toCString(a(i))
      i += 1
    }
    ptr(a.size) = null
    ptr
  }

  private def dup2in(path: String, newfd: CInt)(implicit Z: Zone) = {
    import posix.fcntl._
    import posix.sys.stat._

    val fd = open(toCString(path), O_RDONLY)

    if (fd == -1) {
      val msg = fromCString(string.strerror(errno.errno))
      Console.err.println(s"${BuildInfo.name}: $msg")
    }

    if (posix.unistd.dup2(fd, newfd) == -1) {
      val msg = fromCString(string.strerror(errno.errno))
      Console.err.println(s"${BuildInfo.name}: $msg")
    }

    posix.unistd.close(fd)
  }

  private def dup2out(path: String, newfd: CInt, append: Boolean)(
      implicit Z: Zone) = {
    import posix.fcntl._
    import posix.sys.stat._

    val flags = if (append) {
      O_CREAT | O_WRONLY | O_APPEND
    } else {
      O_CREAT | O_WRONLY | O_TRUNC
    }

    val mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH

    val fd = open(toCString(path), flags, mode)

    if (fd == -1) {
      val msg = fromCString(string.strerror(errno.errno))
      Console.err.println(s"${BuildInfo.name}: $msg")
    }

    if (posix.unistd.dup2(fd, newfd) == -1) {
      val msg = fromCString(string.strerror(errno.errno))
      Console.err.println(s"${BuildInfo.name}: $msg")
    }

    posix.unistd.close(fd)
  }

  def execute(arguments: List[String],
              in: Option[(String, Boolean)],
              out: Option[(String, Boolean)],
              err: Option[(String, Boolean)]): Int =
    posix.unistd.fork() match {
      case 0 => // child process
        val cmd = Vector(name) ++ arguments

        Zone { implicit z =>
          val args = toCArray(cmd)

          for ((path, _) <- in)
            dup2in(path, posix.unistd.STDIN_FILENO)

          for ((path, append) <- out)
            dup2out(path, posix.unistd.STDOUT_FILENO, append)

          for ((path, append) <- err)
            dup2out(path, posix.unistd.STDERR_FILENO, append)

          if (native.std.execvp(args(0), args) == -1) {
            errno.errno match {
              case e if e == posix.errno.ENOENT =>
                // TODO bash returns 127 here, we too?
                Console.err.println(
                  s"$name: this is not the command you are looking for"
                )

              case e if e == posix.errno.EACCES =>
                // TODO bash returns 126 here, we too?
                Console.err.println(s"$name: you are just a padawan")

              case errno =>
                stdio.perror(toCString(s"${BuildInfo.name}: $name: exec"))
            }
          }

          // TODO last handling!
          scala.sys exit 1
        }

      case pid if pid < 0 =>
        Zone { implicit Z =>
          stdio.perror(toCString(s"${BuildInfo.name}: $name: fork"))
        }
        42

      case pid => // parent process
        val wstatus: Ptr[CInt] = stackalloc[CInt]
        !wstatus = 0

        // TODO we would actually need macros here
        // do {
        //   wpid = waitpid(pid, &status, WUNTRACED);
        // } while (!WIFEXITED(status) && !WIFSIGNALED(status));

        while (native.std.wait(wstatus) != pid) {}
        !wstatus
    }
}
