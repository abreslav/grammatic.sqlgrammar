package grammatic.sdt.grammar

case class FunctionSignature(
    val name : String,
    val parameters : List[Attribute],
    val results : List[Attribute]
)
case class Attribute(val name : String, val `type` : TypeTerm)

sealed abstract class Statement
case class Block(val statements : List[Statement]) extends Statement
case class Assignment(val attributes : List[Attribute], val expression : Expression) extends Statement

sealed abstract class Expression
case class AttributeReference(val attribute : Attribute) extends Expression
case class FunctionCall(val function : FunctionSignature, val arguments : List[Expression]) extends Expression
// Text?

case class TypeTerm(val name : String, val arguments : List[TypeTerm])