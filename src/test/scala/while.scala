package smash

import fastparse.all._
import utest._

object WhileTests extends TestSuite with Util {
  val tests = Tests {
    val parser = new SmashParser()

    'while - {
      val code = List("while true", "    echo yeah")
      val ast = chain(code, parser.Line)

      assert(
        ast == Some(
          AST.While(
            condition = AST.Command("true"),
            body = AST.Command(
              AST.Argument.Plain("echo"),
              List(AST.Argument.Plain("yeah"))
            )
          )
        )
      )
    }
  }
}
