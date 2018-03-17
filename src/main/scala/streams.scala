package smash

import java.io.BufferedReader
import java.io.Closeable
import java.io.FileOutputStream
import java.io.FileReader
import java.io.PrintStream

// TODO use native with changeable buffer size
// TODO default to fs block size
final case class Streams(in: BufferedReader,
                         inc: Option[Closeable],
                         out: PrintStream,
                         outc: Option[Closeable],
                         err: PrintStream,
                         errc: Option[Closeable]) {

  def close(): Unit = {
    inc.foreach(_.close())
    outc.foreach(_.close())
    errc.foreach(_.close())
  }
}

object Streams {
  def apply(i: Option[(String, Boolean)],
            o: Option[(String, Boolean)],
            e: Option[(String, Boolean)]): Streams = {

    val (in, inc) =
      i.map({
          case (file, _) =>
            val fr = new FileReader(file)
            val br = new BufferedReader(fr)
            br -> Some(br)
        })
        .getOrElse(Console.in -> None)

    val (out, outc) =
      o.map({
          case (file, append) =>
            val ps = new PrintStream(new FileOutputStream(file, append))
            ps -> Some(ps)
        })
        .getOrElse(Console.out -> None)

    val (err, errc) =
      e.map({
          case (file, append) =>
            val ps = new PrintStream(new FileOutputStream(file, append))
            ps -> Some(ps)
        })
        .getOrElse(Console.err -> None)

    new Streams(in, inc, out, outc, err, errc)
  }
}
