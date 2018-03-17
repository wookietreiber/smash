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
