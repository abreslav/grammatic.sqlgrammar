package grammatic.sdt.grammar.parser

import grammatic.grammar.parser.BasicGrammarParsers

class SDTGrammarParsers extends BasicGrammarParsers {
  import ast._
  
  type ExpressionMetadataT = Block
  type SymbolMetadataT = FunctionSignature

  def block = "{"~>rep(statement)<~"}"^^{Block(_)}
  def statement : Parser[Statement] 
    = block | 
    assignment | 
    attributeDeclaration
  def attributeDeclaration = ident~":"~typeTerm^^{case id~_~t => AttributeDeclaration(id, t)}
  def typeTerm : Parser[TypeTerm] = ident~optd("<"~>repsep(typeTerm, ",")<~">", Nil)^^{
    case id~list => TypeTerm(id, list)
  }
  def assignment = tuple~"="~codeExpression^^{case tuple~_~expr => Assignment(tuple, expr)}
  def tuple = 
    attributeReference^^{_.attribute::Nil} | 
    "("~>repsep(attributeReference, ",")<~")"^^{_.map{_.attribute}}
  def codeExpression : Parser[Expression] = 
    functionCall | 
    attributeReference
  def attributeReference = ident^^{AttributeReference(_)}
  def functionCall = ident~("("~>repsep(codeExpression, ",")<~")")^^{
    case id~args => FunctionCall(id, args)
  }
  def functionSignature = "::"~>optList(tupleDeclaration)~optList("->"~>tupleDeclaration)^^{
    case in~out => FunctionSignature(in, out)
  }
  def tupleDeclaration = "("~>optrepsep(attributeDeclaration, ",")<~")"
  
  def optd[T](p : Parser[T], default: => T) = opt(p)^^{_.getOrElse(default)}
  def optList[T](p : Parser[List[T]]) = optd(p, Nil)
  def optrepsep[T, S](p : Parser[T], s : Parser[S]) = optList(repsep(p, s))
  
  override def expressionMetadata : Parser[ExpressionMetadataT] = optd(block, Block(Nil))

  override def symbolMetadata : Parser[SymbolMetadataT] = optd(functionSignature, FunctionSignature(Nil, Nil))

  protected class SDTGrammarBuilder extends BasicGrammarBuilder {
    override def sequenceWithMD(before : ExpressionMetadataT, expressions : List[(ExpressionT, ExpressionMetadataT)]) : ExpressionT = { 
      if (before != null) 
        println("before: " + before)
      sequence(expressions.map{case (e, s) => {if (s != null) println(s); e}})
    }

    override def nonterminalWithMD(name : String, productions : List[ProductionT], metadata : SymbolMetadataT) : NonterminalT = {
      if (metadata != null)
        println("symbol: " + metadata)
      nonterminal(name, productions)
    }
  
  }
  
  protected override val grammarBuilder = new SDTGrammarBuilder
  
}

object SDTGrammarParsers {
  def parseGrammar(text : String) = new SDTGrammarParsers().parseGrammar(text)
}

package ast {
  case class FunctionSignature(
      val parameters : List[AttributeDeclaration],
      val results : List[AttributeDeclaration]
  )
  
  sealed abstract class Statement
  case class Block(val statements : List[Statement]) extends Statement
  case class Assignment(val attributes : List[String], val expression : Expression) extends Statement
  case class AttributeDeclaration(val name : String, val `type` : TypeTerm) extends Statement
  
  sealed abstract class Expression
  case class AttributeReference(val attribute : String) extends Expression
  case class FunctionCall(val function : String, val arguments : List[Expression]) extends Expression
  
  case class TypeTerm(val name : String, val arguments : List[TypeTerm])

}
