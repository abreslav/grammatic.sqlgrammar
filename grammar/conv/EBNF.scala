package ebnf {
  case class Grammar(val symbols : List[Symbol])
  
  sealed abstract class SymbolKind(val lexical : Boolean)
  object SymbolKind {
    case object Nonterminal extends SymbolKind(false)
    case object LexicalHelper extends SymbolKind(true)
    case object Terminal extends SymbolKind(true)
    case object NoKind extends SymbolKind(false)
  }
  
  case class Symbol(val kind : SymbolKind, val name : String, val definition : Expression)
  
  sealed abstract class Expression
  case class SymbolReference(name : String) extends Expression
  case class Sequence(expression : List[Expression]) extends Expression
  case class Alternative(expression : List[Expression]) extends Expression
  case class Literal(contents : String) extends Expression
  case class Unknown(contents : String) extends Expression
  case class Optional(expression : Expression) extends Expression
  case class Iterated(expression : Expression) extends Expression
  case class OptIter(expression : Expression) extends Expression
  case class Range(negative : Boolean, subranges : List[(Char, Char)]) extends Expression
  
  object Many {
    def unapply(e : Expression) = e match {
      case Alternative(l) => Some(l)
      case Sequence(l) => Some(l)
      case _ => None
    }
  }
  
  object Wrapper {
    def unapply(e : Expression) = e match {
      case Iterated(e) => Some(e)
      case Optional(e) => Some(e)
      case OptIter(e) => Some(e)
      case _ => None
    }
  }
}

