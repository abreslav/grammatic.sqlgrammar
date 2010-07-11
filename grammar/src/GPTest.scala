import grammatic.grammar._
import grammatic.grammar.parser._
import grammatic.sdt.grammar.parser._

object GPTest extends Application {
  val text = 
"""
  s : m ('+' m)*;
  m : f ('*' f)*;
  f
    : INT
    : '(' s ')';
  INT : ['0'-'9']+;
"""
  println("Basic")
  BasicGrammarParsers.parseGrammar(text) match {
    case Left(g) => println(GrammarPrinter(IterationEliminator(g)))
    case Right(s) => println(s)
  }
  
  
//  import scala.io.Source
//  import java.io.File
//  val sql = Source.fromFile(new File("sql92.g")).getLines().toList.mkString("\n")
//  BasicGrammarParsers.parseGrammar(sql) match {
//    case Left(g) => {
//      val ng = IterationEliminator(
//            GrammarOptimizer.optimizeIteration(g))
//      println(ng.length)
//      val writer = new java.io.PrintWriter("sql92.bg")
//      writer.println("%%\n" + BisonGrammarPrinter(ng))
//      writer.close()
//      
//      g.foreach { n =>
//        if (n.name == "tableDefinition") {
//          var sub = SubgrammarSelector.selectSubgrammarStartingWith(n)
//          val subf = sub.filterNot(n => n.name == n.name .toUpperCase)
//          println(subf.size)
//          val writer = new java.io.PrintWriter("sql92sub.bg")
//          writer.println("%%\n" + BisonGrammarPrinter(IterationEliminator(
//            GrammarOptimizer.optimizeIteration(subf))))
//          writer.close()
//        }
//      }
//    }
//    case Right(s) => println(s)
//  }
//  
//  
//  
//  
//
//  val text1 = 
//"""
//  s :: -> (r : Int) : {r = x} m ({xx : A<B<C>, D>}'+' m)*;
//  m : f ('*' f{(a, b) = gf(x, f(x, y), z)})*;
//  f
//    : INT
//    : '(' s ')';
//  INT :: (x : String) -> (y : A) : ['0'-'9']+;
//"""
    
//  println("SDT text")
//  SDTGrammarParsers.parseGrammar(text) match {
//    case Left(g) => println(GrammarPrinter(g))
//    case Right(s) => println(s)
//  }
//  
//  println("SDT text1")
//  SDTGrammarParsers.parseGrammar(text1) match {
//    case Left(g) => println(GrammarPrinter(g))
//    case Right(s) => println(s)
//  }
//  
//  import scala.io.Source
//  import java.io.File
//  
//  val s = Source.fromFile(new File("sql.bg")).getLines().toList.mkString("\n")
//  bison.BisonParsers.parseBisonSpec(s) match {
//    case Left((g, p)) => {
//      println(p)
//      println(GrammarPrinter(g))
//    }
//    case Right(s) => println(s)
//  }
  
}
