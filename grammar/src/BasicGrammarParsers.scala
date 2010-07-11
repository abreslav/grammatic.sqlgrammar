package grammatic.grammar.parser

class BasicGrammarParsers extends AbstractGrammarParsers {
  import grammatic.grammar._
  
  type GrammarT      = List[Nonterminal]
  type NonterminalT  = Nonterminal
  type ExpressionT   = Expression
  type ProductionT   = Production
  type MultiplicityT = Multiplicity
  
  override def parseGrammar(text : String) : Either[GrammarT, String] = 
    parseAll(grammar, text) match {
      case Success(g, _) => {
        val unresolved = symbolTable.unresolvedNames
        val redefined = symbolTable.redefinedNames 
        if (unresolved.isEmpty && redefined.isEmpty)
            Left(g)
        else
          Right(
              (if (!unresolved.isEmpty)
                unresolved.mkString("Unresolved names: ", ",", " ")
              else "") +
              (if (!redefined.isEmpty)
                redefined.mkString("Redefined names: ", ",", "")
              else
                "")
          )
      }
      case ns @ NoSuccess(s, _) => Right(ns.toString) 
    }
  
  protected val symbolTable = new SymbolTable()
  
  protected class BasicGrammarBuilder extends GrammarBuilder {
    
    def grammar(symbols : List[NonterminalT]) : GrammarT 
      = symbols
    
    def nonterminal(name : String, productions : List[ProductionT]) : NonterminalT = {
      val n = Nonterminal(name, productions)
      symbolTable.registerSymbol(n)
      n
    }
      
    def production(expression : ExpressionT) : ProductionT
      = Production(expression)
    
    def symbolReference(name : String) : ExpressionT
      = symbolTable.referenceTo(name)
    
    def stringLiteral(value : String) : ExpressionT
      = StringLiteral(value.substring(1, value.length - 1))
    
    def characterRange(lowerBound : Char, upperBound : Char) : ExpressionT
      = CharacterRange(lowerBound, upperBound)
    
    def sequence(expressions : List[ExpressionT]) : ExpressionT
      = expressions match {
        case e :: Nil => e
        case list => Sequence(list) 
      }
    
    def alternative(expressions : List[ExpressionT]) : ExpressionT
      = expressions match {
        case e :: Nil => e
        case list => Alternative(list) 
      }
    
    def iteration(expression : ExpressionT, multiplicity : MultiplicityT) : ExpressionT
      = Iteration(expression, multiplicity)
    
    def noIteration(expression : ExpressionT) : ExpressionT
      = expression
      
    def empty : ExpressionT
      = Empty
    
    def zeroOrOne : MultiplicityT
      = ZERO_OR_ONE
      
    def zeroOrMore : MultiplicityT
      = ZERO_OR_MORE
      
    def oneOrMore : MultiplicityT
      = ONE_OR_MORE
  }
  
  protected override val grammarBuilder = new BasicGrammarBuilder()
}

class SymbolTable {
  import scala.collection.mutable.{Map, HashMap, Set, HashSet}
  import grammatic.grammar._
  
  private class SymRefM extends SymbolReference {
    var sym : Nonterminal = null
    
    override def symbol : Nonterminal = 
      if (sym == null)
        throw new IllegalStateException()
      else 
        sym
  }
  
  private val symbolTable : Map[String, Nonterminal] = new HashMap()
  private val resolveRequests : Map[String, List[SymRefM]] = new HashMap()
  private val redefinedSymbols : Set[String] = new HashSet()
  private val definedSymbols : Set[String] = new HashSet()
  
  def referenceTo(id : String) : SymbolReference = symbolTable.get(id) match {
    case Some(n) => SymbolReference(n)
    case None => {
      val ref = new SymRefM
      resolveRequests.put(id, ref :: resolveRequests.getOrElse(id, Nil))
      ref
    }
  }
  
  def registerSymbol(n : Nonterminal) {
    if (!definedSymbols.add(n.name)) 
      redefinedSymbols.add(n.name)
    resolveRequests.getOrElse(n.name, Nil).foreach {
      ref => {ref.sym = n}
    }
    resolveRequests.removeKey(n.name)
    symbolTable.put(n.name, n)
  }
  
  def unresolvedNames = resolveRequests.keys
  def redefinedNames = redefinedSymbols.toSet 
}

object BasicGrammarParsers {
  def parseGrammar(text : String) = new BasicGrammarParsers().parseGrammar(text)
}