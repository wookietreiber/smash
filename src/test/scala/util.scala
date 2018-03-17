package smash

import fastparse.all._
import utest._

trait Util {

  type Result[A] = Either[Continuation, A]

  def chain[A <: AST](lines: List[String],
                      parser: P[Result[A]]): Option[AST] = {
    @annotation.tailrec
    def subchain(lines: List[String],
                 p: P[Either[Continuation, AST]]): Option[AST] =
      lines match {
        case Nil =>
          None

        case line :: lines =>
          p.parse(line) match {
            case Parsed.Success(Right(ast), _) =>
              Some(ast)

            case Parsed.Success(Left(cont), _) =>
              subchain(lines, cont.parser)

            case _ =>
              None
          }
      }

    subchain(lines, parser)
  }

}
