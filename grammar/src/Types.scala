class Types {

}

case class TypeParameter(name : String)
case class Constraint(name : String, arguments : List[TypeTerm])
case class TypeDeclaration(name : String, parameters : List[TypeParameter], constraints : List[Constraint])

abstract class TypeTerm
abstract case class CompoundTypeTerm[+T <: TypeTerm](typeDeclaration : TypeDeclaration, arguments : List[T]) extends TypeTerm
case class TypeParameterReference(typeParameter : TypeParameter) extends TypeTerm

case class SubtypingRule(subtype : TypeTerm, supertype : TypeTerm, quantification : List[TypeParameter])