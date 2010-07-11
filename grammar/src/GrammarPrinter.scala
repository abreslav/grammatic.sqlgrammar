package grammatic.grammar

object GrammarPrinter {
  def apply(grammar : List[Nonterminal]) : String = grammar.map{n => printNonterminal(n)}.mkString("\n")
  
  def printNonterminal(n : Nonterminal) : String = 
    n.name + n.productions.map{p => printProduction(p)}.mkString("\n", "\n", "\n    ;")

  def printProduction(p : Production) : String = 
    "    | " + printExpression(p.expression)
    
  def printExpression(e : Expression) : String = e match {
    case SymbolReference(n) => if (n == null) "null" else n.name 
    case StringLiteral(v) => "\"" + v + "\"" // Escaping
    case CharacterRange(f, t) => 
      if (f == t) 
        quoteChar(f) 
      else 
        "[" + quoteChar(f) + "-" + quoteChar(t) + "]"
    case Sequence(l) => l.map{e => printExpression(e)}.mkString(" ")
    case Alternative(l) => l.map{e => printExpression(e)}.mkString(" | ")
    case Iteration(e, m) => printWithParenthesesIfNeeded(e) + m.char
    case Empty => "empty"
  }
    
  def printWithParenthesesIfNeeded(e : Expression) = e match {
    case Sequence(a :: b :: rest) => "(" + printExpression(e) + ")"
    case Alternative(a :: b :: rest) => "(" + printExpression(e) + ")"
    case _ => printExpression(e)
  }

  def quoteChar(c : Char) = "'" + c + "'"
}
