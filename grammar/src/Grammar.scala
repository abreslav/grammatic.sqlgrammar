package grammatic.grammar

//type Grammar = List[Nonterminal]

abstract class Nonterminal(val name : String) {
  def productions : List[Production]
}

object Nonterminal {
  private case class Nont(
      override val name : String,
      override val productions : List[Production]
    ) extends Nonterminal(name) 

  private case class NontR(
      override val name : String,
      p : Nonterminal => List[Production]
  ) extends Nonterminal(name) {
    override val productions : List[Production] = p(this)
  }
  
  def apply(name : String, productions : List[Production]) : Nonterminal = Nont(name, productions)
  
  def unapply(n : Nonterminal) : Option[(String, List[Production])] = Some((n.name, n.productions))
  
  def apply(name : String)(productions : Nonterminal => List[Production]) : Nonterminal = NontR(name, productions)
}

case class Production(val expression : Expression)
sealed abstract class Expression
abstract class SymbolReference extends Expression {
  def symbol : Nonterminal
}
object SymbolReference {
  
  private case class SymRef(override val symbol : Nonterminal) extends SymbolReference
  
  def apply(symbol : Nonterminal) : SymbolReference = SymRef(symbol)
  
  def unapply(ref : SymbolReference) : Option[Nonterminal] = Some(ref.symbol)
}
case class StringLiteral(val value : String) extends Expression
case class CharacterRange(val lowerBound : Char, val upperBound : Char) extends Expression
sealed abstract case class ExpressionCollection(val expressions : List[Expression]) extends Expression
case class Sequence(override val expressions : List[Expression]) extends ExpressionCollection(expressions)
case class Alternative(override val expressions : List[Expression]) extends ExpressionCollection(expressions)
case class Iteration(val expression : Expression, val multiplicity : Multiplicity) extends Expression
case object Empty extends Expression

sealed abstract class Multiplicity(val char : String)
case object ZERO_OR_ONE extends Multiplicity("?")
case object ZERO_OR_MORE extends Multiplicity("*")
case object ONE_OR_MORE extends Multiplicity("+")

