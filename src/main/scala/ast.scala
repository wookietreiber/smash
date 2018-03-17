package smash

sealed abstract class AST

object AST {

  final case class Comment(text: String) extends AST

  case object Empty extends AST

}
