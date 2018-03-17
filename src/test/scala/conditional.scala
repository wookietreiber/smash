package smash

import fastparse.all._
import utest._

object ConditionalTests extends TestSuite with Util {
  val tests = Tests {
    val parser = new SmashParser()

    'cond1 - {
      val code = List("if true", "    echo yeah", "")
      val ast = chain(code, parser.Line)

      assert(
        ast == Some(
          AST.Conditional(
            condition = AST.Command("true"),
            consequence = AST.Command(
              AST.Argument.Plain("echo"),
              List(AST.Argument.Plain("yeah"))
            ),
            alternative = None
          )
        )
      )
    }

    'cond2 - {
      val code = List("if true", "    echo yeah", "else", "    echo meh")
      val ast = chain(code, parser.Line)

      assert(
        ast == Some(
          AST.Conditional(
            condition = AST.Command(AST.Argument.Plain("true")),
            consequence = AST.Command(
              AST.Argument.Plain("echo"),
              List(AST.Argument.Plain("yeah"))
            ),
            alternative = Some(
              AST.Command(
                AST.Argument.Plain("echo"),
                List(AST.Argument.Plain("meh"))
              )
            )
          )
        )
      )
    }
  }
}
