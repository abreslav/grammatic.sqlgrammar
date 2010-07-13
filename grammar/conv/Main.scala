import scala.io.Source
import java.io.File
import ebnf._

object Main {
  
  def optimizeExpression(e : Expression) : Expression = e match {
    case Sequence(l) => Sequence(l.map{optimizeExpression(_)})
    case Alternative(l) => Alternative(l.map{optimizeExpression(_)})
    case Optional(Iterated(e)) => OptIter(optimizeExpression(e))
    case Optional(e) => Optional(optimizeExpression(e))
    case Iterated(e) => Iterated(optimizeExpression(e))
    case OptIter(e) => OptIter(optimizeExpression(e))
    case _ => e
  }

  def convertName(s : String) : String = {
    val wordChar = (new CharClass("""[A-Za-z_]""".r))
    s.map {
      case wordChar(c) => 'x'
      case c => c
    }
  }
  
  class CharClass(re : scala.util.matching.Regex) {
    def unapply(c : Char) : Option[Char] = if (re.findFirstIn(c + "").isDefined) Some(c) else None
  }
  
  def main(args : Array[String]) {
    val sql = 
//      Source.fromFile(new File("smalltest.bnf")).getLines().toList.map{s => s.trim()}.mkString("\n")
      Source.fromFile(new File("sql-2003-2.bnf")).getLines().toList.map{s => s.trim()}.mkString("\n")
    
    val result = EBNFParser.parseGrammar(sql)
    val text = result match {
      case Left(g) => {
        val extraProcessor = new EBNFExtraProcessor("sql-2003-2.extra", g)

        val kinded = extraProcessor(g)
        
        val optimized = Grammar(kinded.symbols.map{sym =>
          Symbol(sym.kind, sym.name, optimizeExpression(sym.definition))
        })
        
        val lexcalSymbols = optimized.symbols.filter{_.kind.lexical}
        
        
        def unknown(s : Symbol) = s.definition.isInstanceOf[Unknown]
        def render(symbols : List[Symbol]) = symbols.map{s => EBNFPrinter.renderSymbol(s)}.mkString("\n")
        render(optimized.symbols.filter{unknown}) + "\n" + render(optimized.symbols.filterNot{unknown})
      }
      case Right(e) => e.toString
    }

      
    val w = new java.io.PrintStream("out.bnf")
    w.print(text)
    w.close()
    
    println("Done")    
  }
}