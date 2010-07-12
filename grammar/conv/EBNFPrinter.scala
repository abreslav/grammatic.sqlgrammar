package ebnf

object EBNFPrinter {
    
  def renderSymbol(s : Symbol) : String = {
    s.kind + " " + s.name + " ::= " + renderExpression(s.definition)
  }
  
  def renderExpression(e : Expression) : String = {
    e match {
      case SymbolReference(n) => n
      case Sequence(l) => l.map{renderExpression(_)}.mkString(" ")
      case Alternative(l) => l.map{renderExpression(_)}.mkString(" | ")
      case Literal(s) => "\"" + s + "\""
      case Unknown(s) => s
      case Optional(e) => "(" + renderExpression(e) + ")?"
      case Iterated(e) => "(" + renderExpression(e) + ")+"
      case Range(n, subs) => "[" + (if (n) "^" else "") + subs.map{
        case (a, b) => if (a == b) renderChar(a) + "" else renderChar(a) + "-" + renderChar(b)}.mkString("") + "]"
    }
  }
  
  def renderChar(c : Char) = c match {
    case '\n' => "\\n"
    case '\r' => "\\r"
    case c => c + ""
  }
  
  def optimizeExpression(e : Expression) : Expression = e match {
    case Sequence(l) => Sequence(l.map{optimizeExpression(_)})
    case Alternative(l) => Alternative(l.map{optimizeExpression(_)})
    case Optional(e) => Optional(optimizeExpression(e))
    case Iterated(e) => Iterated(optimizeExpression(e))
    case _ => e
  }
}