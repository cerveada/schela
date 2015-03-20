package cz.cvut.fit.cerveada.schela

abstract class Form {
  def evaluate(context:Environment):Value
}

case class VarDef(name:String, body:Form) extends Form {
    def evaluate(context:Environment) = {
      context.define(name, body.evaluate(context));
      println("defining variable " + name);
      Unspecified();
    }  
}

case class FunCall(name:String, params:List[Form]) extends Form {
  def evaluate(context:Environment) = {
    println("calling fubnction " + name);
    
    val executedParams = params.map { _.evaluate(context) }
    
    context.get(name) match {
      case p:Procedure => p.evaluate(context); p.call(executedParams)
      case _           => throw new LispException(s"$name is not a function")
    }
  }
}


case class Lambda(paramNames:List[String], body:Form) extends Form {
    def evaluate(environment:Environment) = {
      SProcedure(paramNames, body, environment)
    } 
}

case class IfElse(predicate: Form, thanExp: Form, elseExp: Option[Form]) extends Form {
  def evaluate(environment: Environment) = {
    predicate.evaluate(environment) match {
      case Bool(true)  => thanExp.evaluate(environment)
      case Bool(false) => evaluateElse(elseExp, environment)
      case _       => throw new LispException("predicate must return boolean")
    }
  }

  def evaluateElse(elseExp: Option[Form], environment: Environment) = elseExp match {
    case Some(e) => e.evaluate(environment)
    case None    => Unspecified()
  }
}

case class Set(variable:String, expr:Form) extends Form {
  def evaluate(environment: Environment) = {
    val res = expr.evaluate(environment)
    environment.set(variable, res);
    Unspecified()
  }
}
