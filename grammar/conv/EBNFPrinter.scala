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
      case OptIter(e) => "(" + renderExpression(e) + ")*"
      case Range(n, subs) => "[" + (if (n) "^" else "") + subs.map{
        case (a, b) => if (a == b) renderChar(a) + "" else renderChar(a) + "-" + renderChar(b)}.mkString("") + "]"
    }
  }
  
  def renderChar(c : Char) = c match {
    case '\n' => "\\n"
    case '\r' => "\\r"
    case c => c + ""
  }
}