package grammatic.sdt.grammar.parser

class SDTTreeChecker {
  
  import grammatic.sdt.grammar._
  
  type Error = String
  type CheckRes[T] = Either[T, Error]
  
  def checkFunctionSignature(tree : ast.FunctionSignature, name : String) : CheckRes[FunctionSignature] = {
    
    val usedNames = tree.parameters.foldLeft(Left(Set[String]()).asInstanceOf[CheckRes[Set[String]]]){checker _}
    tree.results.foldLeft(usedNames){checker _}.fold(
        {_ => Left(FunctionSignature(
            name, 
            tree.parameters.map{transformAttribute _}, 
            tree.results.map{transformAttribute _}))},
        {s => Right(s)}
    )
  }
  
  def transformAttribute(decl : ast.AttributeDeclaration) = Attribute(decl.name, transformType(decl.`type`))
  
  def transformType(tt : ast.TypeTerm) : TypeTerm = TypeTerm(tt.name, tt.arguments.map{transformType _})

  
  
  private def checker(used : CheckRes[Set[String]], decl : ast.AttributeDeclaration) : CheckRes[Set[String]] = used match {
    case Left(used) => if (used(decl.name)) Right("Duplicate name: " + decl.name) else Left(used + decl.name)
    case r => r
  }
  
}