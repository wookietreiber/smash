package smash

import scalanative.native._
import scalanative.posix

trait Expand {

  final def tilde(path: String): String =
    if (path.startsWith("~")) {
      val home: String = sys.env("HOME")
      path.replaceFirst("~", home)
    } else {
      path
    }

}
