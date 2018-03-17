package smash

sealed abstract class AST

object AST {

  final case class Comment(text: String) extends AST

  case object Empty extends AST

  sealed abstract class Expression extends AST

  final case class Command(
      name: Argument,
      arguments: List[Argument] = Nil,
      redirection: Redirection = Redirection()
  ) extends Expression

  object Command {
    def apply(name: String): Command =
      new Command(Argument.Plain(name))

    def redir(name: String, r: Redirection = Redirection()): Command =
      new Command(Argument.Plain(name), redirection = r)
  }

  // --------------------------------------------------------------------------
  // redirection
  // --------------------------------------------------------------------------

  final case class Redir(argument: Argument, append: Boolean)

  final case class Redirection(
      in: Option[Redir] = None,
      out: Option[Redir] = None,
      err: Option[Redir] = None
  ) {
    def map[A](f: Redir => A): (Option[A], Option[A], Option[A]) =
      (in.map(f), out.map(f), err.map(f))
  }

  // --------------------------------------------------------------------------
  // control flow
  // --------------------------------------------------------------------------

  final case class Conditional(
      condition: Expression,
      consequence: Expression,
      alternative: Option[Expression]
  ) extends Expression

  final case class While(
      condition: Expression,
      body: Expression
  ) extends Expression

  // --------------------------------------------------------------------------
  // arguments
  // --------------------------------------------------------------------------

  final case class Argument(parts: List[Part])

  object Argument {
    def Plain(arg: String): Argument =
      Argument(List(Part.Plain(arg)))

    def Single(arg: String): Argument =
      Argument(List(Part.SingleQuoted(arg)))

    def DoublePlain(arg: String): Argument =
      Argument(List(Part.DoubleQuoted(List(Part.Plain(arg)))))

    def DoubleVar(arg: String, braces: Boolean): Argument =
      Argument(List(Part.DoubleQuoted(List(Part.Var(arg, braces)))))

    def Var(arg: String, braces: Boolean): Argument =
      Argument(List(Part.Var(arg, braces)))
  }

  sealed abstract class Part

  object Part {
    sealed abstract class Unquoted extends Part

    // plain, unquoted word: substitution applies
    final case class Plain(text: String) extends Unquoted

    // single quoted: no substitutions allowed
    final case class SingleQuoted(text: String) extends Part

    // a variable: substitutions apply
    final case class Var(name: String, braces: Boolean) extends Unquoted

    // double quoted: substitutions allowed
    final case class DoubleQuoted(parts: List[Unquoted]) extends Part
  }

}
