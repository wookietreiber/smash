package smash

import fastparse.all._

sealed abstract class Continuation {
  type A <: AST
  type Result[A] = Either[Continuation, A]
  def parser: P[Result[A]]
}

object Continuation {

  final case class Consequence(condition: AST.Expression)
      extends Continuation
      with SmashBasicParser {

    override type A = AST.Conditional

    // TODO do not require spaces in interactive mode
    // TODO the cursor is already at the correct position
    override val parser: P[Result[A]] = P(
      Spaces ~ Cmd
    ) map { consequence =>
      Left(Else(condition, consequence))
    }
  }

  final case class Else(condition: AST.Expression, consequence: AST.Expression)
      extends Continuation
      with SmashBasicParser {

    override type A = AST.Conditional

    override val parser: P[Result[A]] = P(
      "else".?.!
    ) map {
      case "" =>
        Right(AST.Conditional(condition, consequence, alternative = None))
      case "else" =>
        Left(Alternative(condition, consequence))
    }
  }

  final case class Alternative(condition: AST.Expression,
                               consequence: AST.Expression)
      extends Continuation
      with SmashBasicParser {

    override type A = AST.Conditional

    // TODO do not require spaces in interactive mode
    // TODO the cursor is already at the correct position
    override val parser: P[Result[A]] = P(
      Spaces ~ Cmd
    ) map { alternative =>
      Right(AST.Conditional(condition, consequence, Some(alternative)))
    }
  }

  final case class While(condition: AST.Expression)
      extends Continuation
      with SmashBasicParser {

    override type A = AST.While

    // TODO do not require spaces in interactive mode
    // TODO the cursor is already at the correct position
    override val parser: P[Result[A]] = P(
      Spaces ~ Cmd
    ) map { body =>
      Right(AST.While(condition, body))
    }
  }

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
    Comment | Conditional | While | Command | Empty
  )

  val Command: P[Result[AST.Command]] = P(
    Cmd
  ) map { cmd =>
    Right(cmd)
  }

  val Conditional: P[Result[AST.Conditional]] = P(
    "if" ~ Space ~ Cmd
  ) map { condition =>
    Left(Continuation.Consequence(condition))
  }

  val While: P[Result[AST.While]] = P(
    "while" ~ Space ~ Cmd
  ) map { condition =>
    Left(Continuation.While(condition))
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
    Argument ~ (Space ~ Arguments).? ~ (Space ~ Redirection).?
  ) map {
    case (name, args, redirection) =>
      AST.Command(
        name,
        args.getOrElse(Nil),
        redirection.getOrElse(AST.Redirection())
      )
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

  val Redirection: P[AST.Redirection] = P(
    ((">>" | "2>>" | "&>>" | "<" | ">" | "2>" | "&>").! ~ Space ~ Argument)
      .rep(min = 1, sep = " ")
  ) map { rs =>
    val xs = rs map {
      _ match {
        case ("<", argument) =>
          AST.Redirection(in = Some(AST.Redir(argument, append = false)))

        case (">", argument) =>
          AST.Redirection(out = Some(AST.Redir(argument, append = false)))

        case ("2>", argument) =>
          AST.Redirection(err = Some(AST.Redir(argument, append = false)))

        case ("&>", argument) =>
          val r = Some(AST.Redir(argument, append = false))
          AST.Redirection(out = r, err = r)

        case (">>", argument) =>
          AST.Redirection(out = Some(AST.Redir(argument, append = true)))

        case ("2>>", argument) =>
          AST.Redirection(err = Some(AST.Redir(argument, append = true)))

        case ("&>>", argument) =>
          val r = Some(AST.Redir(argument, append = true))
          AST.Redirection(out = r, err = r)
      }
    }

    xs.foldLeft(AST.Redirection()) { (acc, r) =>
      acc.copy(in = r.in orElse acc.in,
               out = r.out orElse acc.out,
               err = r.err orElse acc.err)
    }
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

  val keywords = Set(
    "case",
    "def",
    "else",
    "for",
    "if",
    "match",
    "then",
    "while"
  )

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
