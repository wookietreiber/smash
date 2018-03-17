package smash

import fastparse.all._
import utest._

object ExpandTests extends TestSuite with Expand {
  val tests = Tests {
    'tilde - {
      val ast = AST.Argument(
        List(
          AST.Part.Plain("~/foo/"),
          AST.Part.DoubleQuoted(List(AST.Part.Plain("bar baz"))),
          AST.Part.Plain("~/bippy")
        )
      )

      val s = expand(ast)

      val home = util.Properties.userHome
      val expected = s"$home/foo/bar baz~/bippy"

      assert(s == expected)
    }

    'variable - {
      val ast = AST.Argument(
        List(
          AST.Part.Var("USER", braces = false),
          AST.Part.Plain(" "),
          AST.Part.SingleQuoted("$USER")
        )
      )

      val s = expand(ast)

      val user = sys.env("USER")
      val expected = s"$user $$USER"

      assert(s == expected)
    }
  }
}
