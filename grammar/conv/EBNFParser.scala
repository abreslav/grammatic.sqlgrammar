package ebnf

import util.parsing.combinator.JavaTokenParsers
import util.parsing.combinator.Parsers
import scala.util.parsing.input._

class EBNFParsers extends JavaTokenParsers {
  
  protected override val whiteSpace = """(\s|/\*(.|\n|\r)*?\*/)+""".r
  
  protected override def handleWhiteSpace(source : java.lang.CharSequence, offset : Int) : Int = {
    val upcoming = source.subSequence(offset, source.length)
    (doubleNewline.findPrefixMatchOf(upcoming)) match {
      case Some(m) => offset
      case None => super.handleWhiteSpace(source, offset);
    }
  }
  
  lazy val doubleNewline = """\n\s*?\n""".r 
  
  lazy val grammar = ((rule | (parcomment | comment | text | doubleNewline)^^{_ => null})*)^^{l => Grammar(l.filter{_ != null})}
  
  lazy val comment = """\-\-.*?(\n|\r|\z)""".r
    
  lazy val parcomment = "--p"~Until(elem("", {_ => true}), "--/p")
  
  lazy val text = """.+(\n|\z)""".r^^{s => 
    if (id.findPrefixMatchOf(s).isDefined) {
      println("Warning! Text starts with id: " + s)
    }
    s
  }
  
  lazy val rule = id~"::="~expression~("""\z""".r | doubleNewline)^^{case s~_~e~_ => Symbol(SymbolKind.NoKind, s, e)} 
  
  def expression : Parser[Expression] = unknown | alternative | terminal
                   
  lazy val unknown = """\!\![^\n\r]*""".r^^{Unknown(_)}
  
  lazy val alternative = rep1sep(sequence, "|")^^{
    case e :: Nil => e
    case l => Alternative(l)
  }
  
  lazy val sequence = (seqItem+)^^{
    case e :: Nil => e
    case l => Sequence(l)
  }
  
  lazy val seqItem : Parser[Expression] = (symbolReference | keyword | optional | braced)~opt("...")^^{
    case e~None => e
    case e~Some(_) => Iterated(e)
  }
  
  lazy val braced = "{"~>alternative<~"}"
  
  lazy val symbolReference = id^^{SymbolReference(_)}
  
  lazy val optional = "["~>alternative<~"]"^^{Optional(_)}
  
  lazy val keyword = """\w+(\-\w+)*""".r^^{s => Literal(s)}
  
  lazy val terminal = """[\"%&\'\(\)\*\+,\-\./:;<>\?=\[\]\^\{\}\|]+""".r^^{s => Literal(s)}
  
  lazy val id = """<[^>\n\r]+>""".r
  
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
  
  def symbolReferences(e : Expression) : Set[SymbolReference] = {
    e match {
      case sr @ SymbolReference(_) => Set(sr)
      case Many(l) => l.map(symbolReferences(_)).toSet.flatten
      case Wrapper(e) => symbolReferences(e)
      case _ => Set()
    }
  }

  def parseWith[T](parser : Parser[T], in : CharSequence) : Either[T, String] = 
    parseAll(parser, in) match {
      case Success(t, _) => Left(t)
      case NoSuccess(s, _) => Right(s)
    }
  
  def parseGrammar(in : CharSequence) = parseWith(grammar, in) match {
    case Left(g) => {
      val symbols = g.symbols
      val names = symbols.map{_.name}.toSet 
      
      if (names.size < symbols.size) {
        val buffer = new StringBuilder("Redefinitions")
        var defs = Set[String]()
        for (sym <- symbols) {
          if (defs(sym.name)) {
            buffer.append(sym.name)
          }
          defs += sym.name
        }
        Right(buffer.result)
      } else {
        val refs = symbols.map{s => symbolReferences(s.definition)}.toSet.flatten.map{_.name}
        val unresolved = refs -- names
        if (!unresolved.isEmpty) {
          Right("Unresolved" + unresolved.mkString("\n"))
        } else {
          Left(g)
        }
      }
    }
    case r => r
  }
}

object EBNFParser extends EBNFParsers