package cz.cvut.fit.cerveada.schela

abstract class Environment {
	protected val variables = scala.collection.mutable.Map[String, Value]();
  
  def define(name: String, value: Value) {
    variables.contains(name) match {
      case true  => throw new VariableAlreadyDefined(name)
      case false => variables += (name -> value); 
    }
  }
  
  def get(name:String): Value;
  def set(name:String, value:Value)
}

class LocalEnvironment(parent: Environment) extends Environment {

  def get(name: String): Value = {
    variables.get(name) match {
      case Some(v) => v
      case None    => parent.get(name)
    }
  }
  
  def set(name: String, value: Value) {
    variables.contains(name) match {
      case true  => variables(name) = value;
      case false => parent.set(name, value)
    }
  }
}

class TopEnvironment extends Environment {
  
  def get(name: String): Value = {
    variables.get(name) match {
      case Some(v) => v
      case None    => throw new VariableNotBound(name)
    }
  }

  def set(name: String, value: Value) {
    variables.contains(name) match {
      case true  => variables(name) = value;
      case false => throw new VariableNotBound(name)
    }
  }
  
  variables += ("+" -> Sum());  
  variables += ("*" -> Mul());  
  variables += ("-" -> Subtraction());
  variables += ("/" -> Division());  
  variables += ("eq?" -> Eq());  
  variables += ("eql?" -> Eql());  
  variables += ("display" -> Display());  
}