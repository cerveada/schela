package cz.cvut.fit.cerveada.schela

import scala.collection.mutable.ArraySeq

trait Form  {
  def typeName(): String
}

case class Number(value: Int) extends Form {
  def typeName = "number"
}

case class Unspecified() extends Form {
  def typeName = "unspecified"
}

abstract class Procedure() extends Form {
  def call(params: List[Form]): Form;
  def typeName = "procedure"
}

case class SProcedure(paramNames: List[Symbol], body:List[Form], homeEnvironment: Environment) extends Procedure {
  def call(params: List[Form]) = {
    if (paramNames.size != params.size)
      throw new UnexpectedNumberOfArguments(params.size, paramNames.size)

    val localEnvironment = new LocalEnvironment(homeEnvironment)
    paramNames.zip(params).foreach { case (n, v) => localEnvironment.define(n, v) }
    
    Evaluator.evalAll(body, localEnvironment)
  }
}

case class NativeProcedure(func: List[Form] => Form) extends Procedure {
  def call(params: List[Form]) = func(params);
}

case class Bool(b: Boolean) extends Form {
  def typeName = "boolean"
}



case class SChar(value: Char) extends Form {
	def typeName = "char"
}
object SChar {
  def mapping = Map("""space""" -> ' ', """newline""" -> '\n')

  def fromString(s: String) = {
    if (s.length() == 1)
      apply(s.toCharArray()(0))
    else
      mapping.get(s) match {
        case Some(ch) => apply(ch)
        case None     => throw new SyntaxException("char parsing")
      }
  }
}

case class SString(value: String) extends Form {
  def typeName = "string"
}

case class Quote(form:Form) extends Form {
  def typeName = "quote"

}

case class SSymbol(name:Symbol) extends Form {
    def typeName = "symbol"
}

case class SList(l:List[Form] = Nil) extends Form {
  def typeName = "list"
  override def toString = {
    val strings = l.map { _.toString }
    "SList(" + strings.mkString(", ") + ")"
  }
  
}

object SList {
	implicit def schemeToList(slist:SList) = slist.l
}

case class SVector(l:ArraySeq[Form]) extends Form {
  def typeName = "vector"  
  override def toString = {
    val strings = l.map { _.toString }
    "Vector(" + strings.mkString(", ") + ")"
  }
  
  var isMutable =  true;
  def mutable = isMutable;
}

object SVector {
  def constant(l: ArraySeq[Form]) = {
    val instance = apply(l)
    instance.isMutable = false
    instance
  }
}

