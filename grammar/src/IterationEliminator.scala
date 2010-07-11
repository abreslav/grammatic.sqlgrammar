package grammatic.grammar

class IterationEliminator {
  
  def apply(g : List[Nonterminal]) : List[Nonterminal] = {
    val list = for (val (n, l) <- g.map{apply _})
      yield n :: l
    list.flatten
  }
  
  def apply(n : Nonterminal) : (Nonterminal, List[Nonterminal]) = {
    val nonterminals = List.newBuilder[Nonterminal]
    val productions = n.productions.map{
      p => {
        baseName = n.name
        val res = apply(p.expression)
        baseName = null
        nonterminals ++= res._2
        Production(res._1)
      }
    } 
    (Nonterminal(n.name, productions), nonterminals.result)
  }
  
  private var usedNames = collection.mutable.Set[String]()
  private var baseName : String = null
  
  private def getNewName() = { 
    var i : Int = 1
    while (usedNames(baseName + i)) {
      i += 1
    }
    val res = baseName + i
    usedNames += res
    res
  }
  
  def apply(e : Expression) : (Expression, List[Nonterminal]) = e match {
    case Empty => (e, Nil)
    case StringLiteral(_) => (e, Nil)
    case CharacterRange(_, _) => (e, Nil)
    case SymbolReference(_) => (e, Nil)
    case Sequence(l) => {
      val res = l.map{apply  _}
      val (nex, n) = res.unzip(identity _)
      (Sequence(nex), n.flatten)
    }
    case Alternative(l) => {
      val res = l.map{apply  _}
      val (nex, n) = res.unzip(identity _)
      (Alternative(nex), n.flatten)
    }
    case Iteration(ex, ZERO_OR_ONE) => {
      val (optEx, list) = apply(ex)
      val n = Nonterminal(getNewName(), 
          Production(Empty) ::
          Production(optEx) :: Nil)
      (SymbolReference(n), n :: list)
    }
    case Iteration(ex, ZERO_OR_MORE) => {
      val (optEx, list) = apply(ex)
      val n = Nonterminal(getNewName()){ 
          n => Production(Empty) ::
          Production(Sequence(SymbolReference(n) :: optEx :: Nil)) ::  
          Nil
      }
      (SymbolReference(n), n :: list)
    }
    case Iteration(ex, ONE_OR_MORE) => {
      val (optEx, list) = apply(ex)
      val n = Nonterminal(getNewName()){ 
        n => Production(optEx) :: 
             Production(Sequence(SymbolReference(n) :: optEx :: Nil)) ::  
             Nil
      }
      (SymbolReference(n), n :: list)
    }
  }
}

object IterationEliminator {
  def apply(g : List[Nonterminal]) : List[Nonterminal] = new IterationEliminator()(g)
}