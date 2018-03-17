package smash

import fastparse.all._
import utest._

object BasicParserTests extends TestSuite {
  def wrap[A](parser: P[A]) =
    P(Start ~ parser ~ End)

  val tests = Tests {
    val parser = new SmashParser

    'var1 - {
      val Parsed.Success(ast, _) = wrap(parser.Var).parse("$USER")
      assert(ast == AST.Part.Var("USER", braces = false))
    }

    'var2 - {
      val Parsed.Success(ast, _) = wrap(parser.Var).parse("${HOME}")
      assert(ast == AST.Part.Var("HOME", braces = true))
    }

    'multipartargument1 - {
      val Parsed.Success(ast, _) =
        wrap(parser.Argument).parse("${HOME}/foo")
      assert(
        ast == AST.Argument(
          List(
            AST.Part.Var("HOME", braces = true),
            AST.Part.Plain("/foo")
          )
        )
      )
    }

    'multipartargument2 - {
      val Parsed.Success(ast, _) =
        wrap(parser.Argument).parse("""${HOME}/foo'$bar_'"baz"""")
      assert(
        ast == AST.Argument(
          List(
            AST.Part.Var("HOME", braces = true),
            AST.Part.Plain("/foo"),
            AST.Part.SingleQuoted("$bar_"),
            AST.Part.DoubleQuoted(List(AST.Part.Plain("baz")))
          )
        )
      )
    }
  }
}
