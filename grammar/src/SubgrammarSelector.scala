package grammatic.grammar

object SubgrammarSelector {
  import scala.collection.mutable.Builder
  import scala.collection.mutable.Set

  def selectSubgrammarStartingWith(n : Nonterminal) : List[Nonterminal] = {
    val list = List.newBuilder[Nonterminal]
    list += n
    val visited = Set(n)
    addReferenced(n, visited, list)
    list.result
  }
  
  private def addReferenced(n : Nonterminal, visited : Set[Nonterminal], res : Builder[Nonterminal, List[Nonterminal]]) {
    n.productions.foreach { p => addReferenced(p.expression, visited, res) }
  }

  private def addReferenced(e : Expression, visited : Set[Nonterminal], res : Builder[Nonterminal, List[Nonterminal]]) {
    e match {
      case ExpressionCollection(l) => l.foreach { addReferenced(_, visited, res) }
      case Iteration(e, _) => addReferenced(e, visited, res)
      case SymbolReference(n) => 
        if (!visited(n)) {
          res += n
          visited += n
          addReferenced(n, visited, res)
        }
      case _ => ()
    }
  }
}