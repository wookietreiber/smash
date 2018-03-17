package smash

import java.nio.file._
import scalanative.native._
import utest._

object RedirectionTests extends TestSuite {

  def mktemp(): String = {
    val template = "/tmp/smash-test.XXXXXX"

    Zone { implicit Z =>
      val t = toCString(template)
      val _ = native.std.mkstemp(t)
      fromCString(t)
    }
  }

  val tests = Tests {
    val interpreter = Interpreter.noninteractive

    'out - {
      val content = "bippy"
      val file = mktemp()
      val line = s"echo $content > $file\n"
      val source = io.Source.fromString(line)
      val ret = interpreter.run(source)
      val c2 = io.Source.fromFile(file).getLines.mkString("")

      Files.delete(Paths.get(file))

      assert(ret == 0 && c2 == content)
    }

    'outappend - {
      val c1 = "foo"
      val c2 = "bar"

      val file = mktemp()

      val l1 = s"echo $c1 > $file"
      val l2 = s"echo $c2 >> $file"

      val s1 = io.Source.fromString(l1)
      val s2 = io.Source.fromString(l2)

      val r1 = interpreter.run(s1)
      val r2 = interpreter.run(s2)

      val cc = io.Source.fromFile(file).getLines.mkString("")

      Files.delete(Paths.get(file))

      assert(r1 == 0 && r2 == 0 && cc == "foobar")
    }
  }
}
