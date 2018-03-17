package smash

import fastparse.all._
import utest._

object ParserTests extends TestSuite {
  val tests = Tests {
    val parser = new SmashParser

    'empty - {
      val Parsed.Success(Right(ast), _) = parser.Line.parse("")
      assert(ast == AST.Empty)
    }

    'cmd - {
      val Parsed.Success(Right(ast), _) = parser.Line.parse("echo")
      assert(ast == AST.Command("echo"))
    }

    'cmdargs - {
      val Parsed.Success(Right(ast), _) =
        parser.Line.parse("cp -a README.md $HOME")
      assert(
        ast == AST.Command(
          AST.Argument.Plain("cp"),
          List(
            AST.Argument.Plain("-a"),
            AST.Argument.Plain("README.md"),
            AST.Argument.Var("HOME", braces = false)
          )
        )
      )
    }

    'quot - {
      val Parsed.Success(Right(ast), _) =
        parser.Line.parse("""echo $HOME "$HOME" '$HOME'""")
      assert(
        ast == AST.Command(
          AST.Argument.Plain("echo"),
          List(
            AST.Argument.Var("HOME", braces = false),
            AST.Argument.DoubleVar("HOME", braces = false),
            AST.Argument.Single("$HOME")
          )
        )
      )
    }

    'comment1 - {
      val Parsed.Success(Right(ast), _) = parser.Line.parse("#")
      assert(ast == AST.Comment("#"))
    }

    'comment2 - {
      val Parsed.Success(Right(ast), _) = parser.Line.parse("# blah")
      assert(ast == AST.Comment("# blah"))
    }
  }
}
