package cz.cvut.fit.cerveada.schela

abstract class Value extends Form {
  def evaluate(environment: Environment) = this
  def typeName(): String
}

case class Number(value: Int) extends Value {
  def typeName = "number"
}

case class Unspecified() extends Value {
  def typeName = "unspecified"
}

abstract class Procedure() extends Value {
  def call(params: List[Value]): Value;
  def typeName = "procedure"
}

case class SProcedure(paramNames: List[String], body: Form, homeEnvironment: Environment) extends Procedure {
  def call(params: List[Value]) = {
    if (paramNames.size != params.size)
      throw new UnexpectedNumberOfArguments(params.size, paramNames.size)

    val localEnvironment = new LocalEnvironment(homeEnvironment)
    paramNames.zip(params).foreach { case (n, v) => localEnvironment.define(n, v) }
    Evaluator.eval(body, localEnvironment);
  }
}

case class Bool(b: Boolean) extends Value {
  def typeName = "boolean"
}

case class SString(value: String) extends Value {
  def typeName = "string"
}

case class SList(values: List[Form]) extends Value {
  def typeName = "list"

}

case class Quote(form:Value) extends Value {
  def typeName = "quote"

}

case class Symbol(name:String) extends Value {
    def typeName = "symvbol"
}

