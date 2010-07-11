import util.parsing.combinator.JavaTokenParsers
import util.parsing.combinator.Parsers
import scala.util.parsing.input._

sealed abstract class Expression
case class SymbolReference(name : String) extends Expression
case class Sequence(expression : List[Expression]) extends Expression
case class Alternative(expression : List[Expression]) extends Expression
case class Literal(contents : String) extends Expression
case class Unknown(contents : String) extends Expression
case class Optional(expression : Expression) extends Expression
case class Iterated(expression : Expression) extends Expression
case class Symbol(val name : String, val definition : Expression)
case class Grammar(val symbols : List[Symbol])

object SQLGrammarParser extends JavaTokenParsers {

  protected override val whiteSpace = """(\s|/\*(.|\n|\r)*?\*/)+""".r
//  protected override val whiteSpace = """(\s|(\-\-p(.|\n|\r)*?\-\-/p)|(\-\-.*?(\n|\r)))+""".r
//  protected override val whiteSpace = """\s+""".r
  
  protected override def handleWhiteSpace(source : java.lang.CharSequence, offset : Int) : Int = {
    val upcoming = source.subSequence(offset, source.length)
    (doubleNewline.findPrefixMatchOf(upcoming)) match {
      case Some(m) => offset
      case None => super.handleWhiteSpace(source, offset);
    }
  }
  
  def doubleNewline = """\n\s*?\n""".r 
  
  def grammar = ((rule | (parcomment | comment | text | doubleNewline)^^{_ => null})*)^^{l => l.filter{_ != null}}
  
  def comment = """\-\-.*?(\n|\r|\z)""".r
    
  def parcomment = "--p"~Until(elem("", {_ => true}), "--/p")
  
  def text = """.+(\n|\z)""".r
  
  def rule = id~"::="~expression~("""\z""".r | doubleNewline)^^{case s~_~e~_ => Symbol(s, e)} 
  
  def expression : Parser[Expression] = unknown | alternative | terminal
                   
  def unknown = """\!\![^\n\r]*""".r^^{Unknown(_)}
  
  def alternative = rep1sep(sequence, "|")^^{
    case e :: Nil => e
    case l => Alternative(l)
  }
  
  def sequence = (seqItem+)^^{
    case e :: Nil => e
    case l => Sequence(l)
  }
  
  def seqItem : Parser[Expression] = (symbolReference | keyword | optional | braced)~opt("...")^^{
    case e~None => e
    case e~Some(_) => Iterated(e)
  }
  
  def braced = "{"~>alternative<~"}"
  
  def symbolReference = id^^{SymbolReference(_)}
  
  def optional = "["~>alternative<~"]"^^{Optional(_)}
  
  def keyword = """\w+(\-\w+)*""".r^^{s => Literal(s)}
  
  def terminal = """[\"%&\'\(\)\*\+,\-\./:;<>\?=\[\]\^\{\}\|]+""".r^^{s => Literal(s)}
  
  def id = """<[^>\n\r]+>""".r
//  def id = "<"~rep(ident | ("""\d+""".r) | "-" | ":" | "/")~">"
  
  def optd[T](p : Parser[T], default: => T) = opt(p)^^{_.getOrElse(default)}
  def optList[T](p : Parser[List[T]]) = optd(p, Nil)
  def optrepsep[T, S](p : Parser[T], s : Parser[S]) = optList(repsep(p, s))
  
  case class Until[T](body : Parser[T], condition : Parser[_]) extends Parser[List[T]] {
    override def apply(in : Input) : ParseResult[List[T]] = {
      var result : List[T] = Nil
      var inp = in
      while (true) {
        condition(inp) match {
          case NoSuccess(_, _) => 
            body(inp) match {
              case Failure(a, b) => return Failure(a, b)
              case Error(a, b) => return Error(a, b)
              case Success(r, n) => {
                result = r :: result
                inp = n
              }
            }
          case Success(_, n) => return Success(result.reverse, n)
        }
      }
      Success(result.reverse, inp)
    }
  }
  
  def printSymbol(s : Symbol) : String = {
    s.name + " ::= " + printExpression(s.definition)
  }
  
  def printExpression(e : Expression) : String = {
    e match {
      case SymbolReference(n) => n
      case Sequence(l) => l.map{printExpression(_)}.mkString(" ")
      case Alternative(l) => l.map{printExpression(_)}.mkString(" | ")
      case Literal(s) => "\"" + s + "\""
      case Unknown(s) => s
      case Optional(e) => "(" + printExpression(e) + ")?"
      case Iterated(e) => "(" + printExpression(e) + ")+"
    }
  }
  
  def symbolReferences(e : Expression) : Set[SymbolReference] = {
    e match {
      case sr @ SymbolReference(_) => Set(sr)
      case Sequence(l) => l.map(symbolReferences(_)).toSet.flatten
      case Alternative(l) => l.map(symbolReferences(_)).toSet.flatten
      case Optional(e) => symbolReferences(e)
      case Iterated(e) => symbolReferences(e)
      case _ => Set()
    }
  }
  
  def main(s : Array[String]) {
    import scala.io.Source
    import java.io.File
    val sql = 
//      Source.fromFile(new File("smalltest.bnf")).getLines().toList.map{s => s.trim()}.mkString("\n")
      Source.fromFile(new File("sql-2003-2.bnf")).getLines().toList.map{s => s.trim()}.mkString("\n")
    
//    println(parseAll(grammar, sql))
    val r = parseAll(grammar, sql)
    val g = r match {
      case Success(l, _) => { 
        val names = l.map{_.name}.toSet 
        
        if (names.size < l.size) {
          println("Redefinitions")
          var defs = Set[String]()
          for (sym <- l) {
            if (defs(sym.name)) {
              println(sym.name)
            }
            defs += sym.name
          }
        }
        
        val refs = l.map{s => symbolReferences(s.definition)}.toSet.flatten.map{_.name}
        
        val unresolved = refs -- names
        if (!unresolved.isEmpty) {
          println("Unresolved")
          unresolved.foreach{println(_)}
        }
        
        l.filter{s => s.definition.isInstanceOf[Unknown]}.map{s => printSymbol(s)}.mkString("\n") + "\n" +
         l.filter{s => !s.definition.isInstanceOf[Unknown]}.map{s => printSymbol(s)}.mkString("\n")
      }
      case e => e.toString
    }

      
    val w = new java.io.PrintStream("out.bnf")
    w.print(g)
    w.close()
//    println(r)
    
    println("Done")
  }
}
