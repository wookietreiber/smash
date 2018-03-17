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
    Comment | Command | Empty
  )

  val Command: P[Result[AST.Command]] = P(
    Cmd
  ) map { cmd =>
    Right(cmd)
  }

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
  // ast components
  // --------------------------------------------------------------------------

  val Cmd: P[AST.Command] = P(
    Argument ~ (Space ~ Arguments).?
  ) map {
    case (name, args) =>
      AST.Command(name, args.getOrElse(Nil))
  }

  val Argument: P[AST.Argument] = P(
    (Plain | SingleQuoted | DoubleQuoted | Var).rep(min = 1)
  ) map { parts =>
    AST.Argument(parts.toList)
  }

  val Arguments: P[List[AST.Argument]] = P(
    Argument.rep(sep = " ", min = 1)
  ) map { args =>
    args.toList
  }

  val Plain: P[AST.Part.Plain] = P(
    (Letter | Digit | Symbol).rep(min = 1).!
  ) map { text =>
    AST.Part.Plain(text)
  }

  val SingleQuoted: P[AST.Part.SingleQuoted] = P(
    "'" ~ CharsWhile(_ != '\'', min = 0).! ~ "'"
  ) map { text =>
    AST.Part.SingleQuoted(text)
  }

  val DoubleQuoted: P[AST.Part.DoubleQuoted] = P(
    "\"" ~ (Plain | Var | SpacePart).rep(min = 0) ~ "\""
  ) map { parts =>
    AST.Part.DoubleQuoted(parts.toList)
  }

  val SpacePart: P[AST.Part.Plain] = P(
    Spaces.!
  ) map { spaces =>
    AST.Part.Plain(spaces)
  }

  val Var: P[AST.Part.Var] = P(
    "$" ~ (UnbracedVar | BracedVar)
  )

  val BracedVar: P[AST.Part.Var] = P(
    "{" ~/ Identifier.! ~ "}"
  ) map { name =>
    AST.Part.Var(name, braces = true)
  }

  val UnbracedVar: P[AST.Part.Var] = P(
    Identifier.!
  ) map { name =>
    AST.Part.Var(name, braces = false)
  }

  // --------------------------------------------------------------------------
  // naming things
  // --------------------------------------------------------------------------

  val LetterLower = P(CharIn('a' to 'z'))
  val LetterUpper = P(CharIn('A' to 'Z'))

  val Letter = P(LetterLower | LetterUpper)

  val Underscore = P("_")

  val Hash = P("#")

  val Symbol = P(CharIn("./-_=+~"))

  val keywords = Set("case", "def", "else", "for", "if", "match", "then")

  val Identifier =
    P((Letter | Underscore) ~ (Letter | Digit | Underscore).rep).! filter {
      !keywords.contains(_)
    }

  // --------------------------------------------------------------------------
  // space
  // --------------------------------------------------------------------------

  val Space = P(" ")

  val Spaces = P(CharsWhileIn(" "))

  val Newline = P("\n")

  // --------------------------------------------------------------------------
  // numbers
  // --------------------------------------------------------------------------

  val Digit = P(CharIn('0' to '9'))
  val Digits = P(CharsWhileIn("0123456789"))

  val Integral = P(("0" ~ !Digits) | CharIn('1' to '9') ~ Digits.?)

}
