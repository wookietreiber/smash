package smash

import scalanative.native._
import scalanative.posix

final case class Command(name: String) {
  def toCArray(a: IndexedSeq[String])(implicit Z: Zone): Ptr[CString] = {
    val ptr: Ptr[CString] = alloc[CString](a.size + 1)
    var i = 0
    while (i < a.size) {
      ptr(i) = toCString(a(i))
      i += 1
    }
    ptr(a.size) = null
    ptr
  }

  def execute(arguments: List[String]): Int =
    posix.unistd.fork() match {
      case 0 => // child process
        val cmd = Vector(name) ++ arguments

        Zone { implicit z =>
          val args = toCArray(cmd)

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
