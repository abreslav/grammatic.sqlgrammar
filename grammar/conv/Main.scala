import scala.io.Source
import java.io.File
import ebnf._

object Main {
  
  def main(args : Array[String]) {
    val sql = 
//      Source.fromFile(new File("smalltest.bnf")).getLines().toList.map{s => s.trim()}.mkString("\n")
      Source.fromFile(new File("sql-2003-2.bnf")).getLines().toList.map{s => s.trim()}.mkString("\n")
    
    val result = EBNFParser.parseGrammar(sql)
    val text = result match {
      case Left(g) => {
        val extraProcessor = new EBNFExtraProcessor("sql-2003-2.extra", g)

        val kinded = extraProcessor(g)
        
        def unknown(s : Symbol) = s.definition.isInstanceOf[Unknown]
        def render(symbols : List[Symbol]) = symbols.map{s => EBNFPrinter.renderSymbol(s)}.mkString("\n")
        render(kinded.symbols.filter{unknown}) + "\n" + render(kinded.symbols.filterNot{unknown})
      }
      case Right(e) => e.toString
    }

      
    val w = new java.io.PrintStream("out.bnf")
    w.print(text)
    w.close()
    
    println("Done")    
  }
}