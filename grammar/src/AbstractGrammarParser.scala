package grammatic.grammar.parser

import util.parsing.combinator.JavaTokenParsers
import scala.collection.mutable.ListBuffer


/**
 * @author abreslav
 */

abstract class AbstractGrammarParsers extends JavaTokenParsers {
  
  type ExpressionMetadataT >: Null
  type SymbolMetadataT >: Null
  type GrammarT
  type NonterminalT
  type ExpressionT
  type ProductionT
  type MultiplicityT
  
  trait GrammarBuilder {
    def grammar(symbols : List[NonterminalT]) : GrammarT
    def nonterminalWithMD(name : String, productions : List[ProductionT], metadata : SymbolMetadataT) : NonterminalT = 
      nonterminal(name, productions)
    def nonterminal(name : String, productions : List[ProductionT]) : NonterminalT
    def production(expression : ExpressionT) : ProductionT
    def symbolReference(name : String) : ExpressionT
    def stringLiteral(value : String) : ExpressionT
    def characterRange(lowerBound : Char, upperBound : Char) : ExpressionT
    def sequenceWithMD(before : ExpressionMetadataT, expressions : List[(ExpressionT, ExpressionMetadataT)]) : ExpressionT = 
      sequence(expressions.map{case (e, _) => e})
    def sequence(expressions : List[ExpressionT]) : ExpressionT
    def alternative(expressions : List[ExpressionT]) : ExpressionT
    def iteration(expression : ExpressionT, multiplicity : MultiplicityT) : ExpressionT
    def noIteration(expression : ExpressionT) : ExpressionT
    def empty : ExpressionT
    
    def zeroOrOne : MultiplicityT
    def zeroOrMore : MultiplicityT
    def oneOrMore : MultiplicityT
  }
  
  protected def grammarBuilder : GrammarBuilder 
  
  def parseGrammar(text : String) : Either[GrammarT, String] = parseToEither(grammar, text) 
  
  def parseToEither[A](p : Parser[A], text : String) : Either[A, String] = 
    parseAll(p, text) match {
    case Success(r, _) => Left(r)
    case NoSuccess(s, _) => Right(s)
  }
    
  protected override val whiteSpace = """(\s|(//.*?(\n|\r))|/\\*(.|\n|\r)*?\\*/)+""".r

  def expressionMetadata : Parser[ExpressionMetadataT] = success(null)
  def symbolMetadata : Parser[SymbolMetadataT] = success(null)
  
  def grammar : Parser[GrammarT]
    = rep(nonterminal)^^{list => grammarBuilder.grammar(list)}
    
  def nonterminal : Parser[NonterminalT]
    = ident~symbolMetadata~rep(production)~";"^^{case id~md~prods~_ => grammarBuilder.nonterminalWithMD(id, prods, md)}
  
  def production : Parser[ProductionT]
    = ":"~alternative^^{case _~alt => grammarBuilder.production(alt)}

  def alternative : Parser[ExpressionT] = sequence~rep("|"~sequence)^^{
    case seq~list => grammarBuilder.alternative(seq :: list.map{case _~x => x})
  }
  
  def sequence : Parser[ExpressionT] = expressionMetadata~rep1(iteration~expressionMetadata) ^^ {
    case before~list => grammarBuilder.sequenceWithMD(before, list.map{case e~m => (e, m)})
  }
  
  def iteration : Parser[ExpressionT] = expression~opt(multiplicity)^^{
    case e~None => grammarBuilder.noIteration(e)
    case e~Some(m) => grammarBuilder.iteration(e, m)
  } 
  
  def multiplicity : Parser[MultiplicityT] = 
    "?"^^{_ => grammarBuilder.zeroOrOne} | 
    "*"^^{_ => grammarBuilder.zeroOrMore} | 
    "+"^^{_ => grammarBuilder.oneOrMore}
    
  def expression : Parser[ExpressionT] =
    "("~alternative~")"^^{case _~a~_ => a} |
    ident^^{id => grammarBuilder.symbolReference(id)} |
    stringLiteral^^{l => grammarBuilder.stringLiteral(l)} |
    characterRange^^{x => x} |
    empty
  
  def empty = "empty"^^{_ => grammarBuilder.empty}
    
  def characterRange : Parser[ExpressionT] = 
    characterLiteral^^{x => grammarBuilder.characterRange(x, x)} | 
    "["~characterLiteral~"-"~characterLiteral~"]"^^{case _~f~_~t~_ => grammarBuilder.characterRange(f, t)}
  
  def characterLiteral : Parser[Char] = ("\'"+"""([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})"""+"\'").r^^{
    s => if (s.length == 3)
        s.charAt(1)
      else throw new UnsupportedOperationException();
  }

}