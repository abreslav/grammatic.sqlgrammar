package ebnf

import scala.io.Source
import java.io.File

class EBNFExtraProcessor(fileName : String, grammar : Grammar) {
  var lexicals : Set[String] = null
  var redefinitions : List[Symbol] = null
  
  private val extra = Source.fromFile(new File(fileName)).getLines().toList.map{s => s.trim()}.mkString("\n")
  ExtraParser.parseExtra(extra) match {
    case Left((red, lex)) => {
      lexicals = lex.getOrElse(Nil).map{_.name}.toSet
      redefinitions = red.getOrElse(Nil) 
    }
    case Right(e) => println(e)
  }
  val nonterminals = grammar.symbols.filterNot{s => lexicals(s.name)}
  val nonterminalNames = nonterminals.map{_.name}.toSet
  val referencedNames : Set[String] = nonterminals.map{s => EBNFParser.symbolReferences(s.definition).map{_.name}}.flatten.toSet 
  val terminalNames = referencedNames -- nonterminalNames
  val nameToDef : Map[String, Expression] = redefinitions.map{s => (s.name, s.definition)}(scala.collection.breakOut)
  
  object ExtraParser extends EBNFParsers {
    def extra = opt(redefinitions)~opt(lexical)^^{case a~b => (a, b)}
    
    def lexical = "lexical"~>rep1(symbolReference)
      
    def redefinitions = "redefinitions"~>rep1(rule)
    
    override def expression = hexCodeExpr | range | super.expression

    def hexCodeExpr = hexCode^^{s => Literal(fromHex(s.substring(2)) + "")}
    
    def hexCode = """0x[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]""".r
    
    def range = "["~>opt("^")~rep1(charRange)<~ "]"^^{
      case opt~l => Range(opt.isDefined, l)
    }
    
    def charRange = (char~"-"~char)^^{case c1~_~c2 => (c1, c2)} | char^^{c => (c, c)}
    
    def char = hexCode^^{fromHex(_)} | """[^\n\r \t\]]""".r^^{_.charAt(0)}
    
    def fromHex(s : String) : Char = {
      var result : Char = 0
      s.toUpperCase().foreach{c =>
        if (c >= '0' && c <= '9')
          result = (result * 16 + c - '0').asInstanceOf[Char]
        else
          result = (result * 16 + c - 'A' + 10).asInstanceOf[Char]
      }
      return result
    }
    
    def parseExtra(in : CharSequence) = parseAll(extra, in) match {
      case Success(r, _) => Left(r)
      case ns @ NoSuccess(e, _) => Right(ns.toString)
    }
      
  }
  
  def kind(sym : Symbol) : SymbolKind = {
    if (terminalNames(sym.name))
      SymbolKind.Terminal
    else if (nonterminalNames(sym.name))
      SymbolKind.Nonterminal
    else SymbolKind.LexicalHelper 
  }
  
  def define(sym : Symbol) : Symbol = 
    Symbol(
        kind(sym), 
        sym.name, 
        if (nameToDef.keySet(sym.name)) 
          nameToDef(sym.name) 
        else sym.definition)
        
  def apply(g : Grammar) = Grammar(g.symbols.map{sym => define(sym)})
}
  
