package bison

import grammatic.grammar.parser._

class BisonParsers extends BasicGrammarParsers {
  import grammatic.grammar._
  
  private val nontermPrecedence = collection.mutable.Map[NonterminalT, (Associativity, Int)]()
  private val literalPrecedence = collection.mutable.Map[ExpressionT, (Associativity, Int)]()
  private val definedTokens = collection.mutable.Map[String, NonterminalT]()
  private var currentPrecedence = 0;

  def parseBisonSpec(text : String) : Either[(GrammarT, PrecedenceInfo), String] = parseAll(bisonSpec, text) match {
    case Success(r, _) => Left((r, 
        new PrecedenceInfo(
            nontermPrecedence.toMap,
            literalPrecedence.toMap)))
    case NoSuccess(s, _) => Right(s) 
  }
  
  def bisonSpec : Parser[GrammarT] = declarations~"%%"~grammar^^{
    case d~_~g => d.filter(_ != null).flatten{identity _}.filter(_ != null) ::: g
  }
  
  def declarations = rep(declaration)
  
  def declaration =  
    ("%\\{(.|\n|\r)*?%\\}".r^^{_ => null} |
    "%code"~bracedCode^^{_ => null} |
    "%define"~ident~stringLiteral^^{_ => null}) |
    precedenceDeclaration |
    token 
    
  def bracedCode : Parser[Any] = "{"~opt(rep("[^\\}\\{]+".r | bracedCode))~"}"^^{_ => null}
  
  def token = 
    "%token"~>rep(
        ident~opt(tokenLiteral)^^{
          case id~Some(s) => registerToken(id, s :: Nil)
          case id~None => registerToken(id, Nil)
        }
    )
    
  def precedenceDeclaration = assoc~rep(ident | tokenLiteral)^^{
      case a~list => {
        val r = list.map {
          case c @ CharacterRange(_, _) => registerPrecedence(c, a)
          case s @ StringLiteral(_) => registerPrecedence(s, a)
          case s : String => registerTokenPrecedence(s, a)
        }
        currentPrecedence += 1
        r
      }
    }
  def assoc = "%left"^^{_ => LEFT} | "%right"^^{_ => RIGHT} | "%nonassoc"^^{_ => NONASSOC}
  def tokenLiteral : Parser[ExpressionT] = 
    characterLiteral^^{c => grammarBuilder.characterRange(c, c)} | 
    stringLiteral^^{grammarBuilder.stringLiteral(_)}
    
  override def sequence = super.sequence | success(grammarBuilder.empty)
  
  private def registerPrecedence(e : Expression, assoc : Associativity) : NonterminalT = {
    literalPrecedence(e) = (assoc, currentPrecedence)
    null
  }
  
  private def registerTokenPrecedence(name : String, assoc : Associativity) = {
    val r = if (definedTokens.contains(name)) 
      null
    else {
      registerToken(name, Nil)
    }
    nontermPrecedence(definedTokens(name)) = (assoc, currentPrecedence)
    r
  }
  
  private def registerToken(name : String, expressions : List[Expression]) = {
    val n = grammarBuilder.nonterminal(name, expressions.map{grammarBuilder.production(_)})
    definedTokens(name) = n 
    symbolTable.registerSymbol(n)
    n
  }
}

object BisonParsers {
  def parseBisonSpec(text : String) = new BisonParsers().parseBisonSpec(text)
}

class PrecedenceInfo(
    val nontermPrecedence : Map[grammatic.grammar.Nonterminal, (Associativity, Int)],
    val literalPrecedence : Map[grammatic.grammar.Expression, (Associativity, Int)]
  ) {
  override def toString = 
    nontermPrecedence.map {
      case (n, (a, p)) => "%" + a.str + "(" + p + ") " + n.name
    }.mkString("", "\n", "\n") +
    literalPrecedence.map {
      case (l, (a, p)) => "%" + a.str + "(" + p + ") " + grammatic.grammar.GrammarPrinter.printExpression(l)
    }.mkString("\n")
}

sealed abstract class Associativity(val str : String)
case object LEFT extends Associativity("left")
case object RIGHT extends Associativity("right")
case object NONASSOC extends Associativity("nonassoc")