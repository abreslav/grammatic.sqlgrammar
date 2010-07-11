package grammatic.grammar

object GrammarOptimizer {
  
  def optimizeIteration(g : List[Nonterminal]) : List[Nonterminal] = g.map{optimizeIteration _}
  
  def optimizeIteration(n : Nonterminal) : Nonterminal = Nonterminal(n.name, n.productions.map{p => Production(optimizeIteration(p.expression))})
  
  def optimizeIteration(e : Expression) : Expression = e match {
    case Empty => e
    case StringLiteral(_) => e
    case CharacterRange(_, _) => e
    case SymbolReference(_) => e
    case Sequence(l) => Sequence(l.map{optimizeIteration _})
    case Alternative(l) => Alternative(l.map{optimizeIteration _})
    case Iteration(ex, ZERO_OR_ONE) => ex match {
      case Iteration(e, ZERO_OR_ONE) => optimizeIteration(ex)
      case Iteration(e, ZERO_OR_MORE) => optimizeIteration(ex)
      case Iteration(e, ONE_OR_MORE) => optimizeIteration(Iteration(e, ZERO_OR_MORE))
      case e => Iteration(optimizeIteration(e), ZERO_OR_ONE)
    }
    case Iteration(ex, ZERO_OR_MORE) => ex match {
      case Iteration(e, ZERO_OR_ONE) => optimizeIteration(Iteration(e, ZERO_OR_MORE))
      case Iteration(e, ZERO_OR_MORE) => optimizeIteration(ex)
      case Iteration(e, ONE_OR_MORE) => optimizeIteration(Iteration(e, ZERO_OR_MORE))
      case e => Iteration(optimizeIteration(e), ZERO_OR_MORE)
    }
    case Iteration(ex, ONE_OR_MORE) => ex match {
      case Iteration(e, ZERO_OR_ONE) => optimizeIteration(Iteration(e, ZERO_OR_MORE))
      case Iteration(e, ZERO_OR_MORE) => optimizeIteration(ex)
      case Iteration(e, ONE_OR_MORE) => optimizeIteration(ex)
      case e => Iteration(optimizeIteration(e), ONE_OR_MORE)
    }
  }
}