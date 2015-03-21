package cz.cvut.fit.cerveada.schela

trait Form  {
  def evaluate(environment: Environment) = this
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

case class SProcedure(paramNames: List[String], body: Form, homeEnvironment: Environment) extends Procedure {
  def call(params: List[Form]) = {
    if (paramNames.size != params.size)
      throw new UnexpectedNumberOfArguments(params.size, paramNames.size)

    val localEnvironment = new LocalEnvironment(homeEnvironment)
    paramNames.zip(params).foreach { case (n, v) => localEnvironment.define(n, v) }
    Evaluator.eval(body, localEnvironment);
  }
}

case class Bool(b: Boolean) extends Form {
  def typeName = "boolean"
}

case class SString(value: String) extends Form {
  def typeName = "string"
}

case class SList(values: List[Form]) extends Form {
  def typeName = "list"

}

case class Quote(form:Form) extends Form {
  def typeName = "quote"

}

case class Symbol(name:String) extends Form {
    def typeName = "symvbol"
}

