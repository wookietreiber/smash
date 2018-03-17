package smash

import fastparse.all._

sealed abstract class Continuation {
  type A <: AST
  type Result[A] = Either[Continuation, A]
  def parser: P[Result[A]]
}

// TODO if parser encounters \r or \t: print:
// TODO you are using tabs / windows newlines, the force is not with you
class SmashParser extends SmashBasicParser {

  type Result[A <: AST] = Either[Continuation, A]

  // TODO no trailing spaces in scripts, interactive is ok
  // TODO maybe just don't allow and trim line in interactive mode
  // TODO trim removes space, \n, \r, \t
  val Line: P[Result[AST]] = P(
    Start ~ Main ~ Spaces.? ~ Newline.? ~ End
  )

  // --------------------------------------------------------------------------
  // main ast
  // --------------------------------------------------------------------------

  val Main: P[Result[AST]] = P(
    Comment | Empty
  )

  val Comment: P[Result[AST.Comment]] = P(
    Hash ~/ CharsWhile(_ != '\n', min = 0)
  ).! map { text =>
    Right(AST.Comment(text))
  }

  val Empty: P[Result[AST.Empty.type]] = P(
    Spaces.?
  ) map { _ =>
    Right(AST.Empty)
  }

}

trait SmashBasicParser {

  // --------------------------------------------------------------------------
  // naming things
  // --------------------------------------------------------------------------

  val Hash = P("#")

  // --------------------------------------------------------------------------
  // space
  // --------------------------------------------------------------------------

  val Space = P(" ")

  val Spaces = P(CharsWhileIn(" "))

  val Newline = P("\n")

}
