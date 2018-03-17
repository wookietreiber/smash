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

    'redir1 - {
      val Parsed.Success(Right(ast), _) =
        parser.Line.parse("echo &> file")
      assert(
        ast == AST.Command.redir(
          "echo",
          AST.Redirection(
            out = Some(AST.Redir(AST.Argument.Plain("file"), false)),
            err = Some(AST.Redir(AST.Argument.Plain("file"), false))
          )
        )
      )
    }

    'redir2 - {
      val Parsed.Success(Right(ast), _) =
        parser.Line.parse("echo < foo > file 2> /dev/null")
      assert(
        ast == AST.Command.redir(
          "echo",
          AST.Redirection(
            in = Some(AST.Redir(AST.Argument.Plain("foo"), false)),
            out = Some(AST.Redir(AST.Argument.Plain("file"), false)),
            err = Some(AST.Redir(AST.Argument.Plain("/dev/null"), false))
          )
        )
      )
    }

    'redir3 - {
      val Parsed.Success(Right(ast), _) =
        parser.Line.parse("echo >> file 2>> /dev/null")
      assert(
        ast == AST.Command.redir(
          "echo",
          AST.Redirection(
            out = Some(AST.Redir(AST.Argument.Plain("file"), true)),
            err = Some(AST.Redir(AST.Argument.Plain("/dev/null"), true))
          )
        )
      )
    }

    'redir4 - {
      val Parsed.Success(Right(ast), _) =
        parser.Line.parse("echo &>> file")
      assert(
        ast == AST.Command.redir(
          "echo",
          AST.Redirection(
            out = Some(AST.Redir(AST.Argument.Plain("file"), true)),
            err = Some(AST.Redir(AST.Argument.Plain("file"), true))
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
