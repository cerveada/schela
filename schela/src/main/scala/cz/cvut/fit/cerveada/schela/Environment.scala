package cz.cvut.fit.cerveada.schela

import cz.cvut.fit.cerveada.schela.natives.BooleanNatives
import cz.cvut.fit.cerveada.schela.natives.NumberNatives
import cz.cvut.fit.cerveada.schela.natives.Eql
import cz.cvut.fit.cerveada.schela.natives.Eq
import cz.cvut.fit.cerveada.schela.natives.Display

abstract class Environment {
	protected val variables = scala.collection.mutable.Map[String, Form]();
  
  def define(name: String, value: Form) {
    variables.contains(name) match {
      case true  => throw new VariableAlreadyDefined(name)
      case false => variables += (name -> value); 
    }
  }
  
  def get(name:String): Form;
  def set(name:String, value:Form)
}

class LocalEnvironment(parent: Environment) extends Environment {

  def get(name: String): Form = {
    variables.get(name) match {
      case Some(v) => v
      case None    => parent.get(name)
    }
  }
  
  def set(name: String, value: Form) {
    variables.contains(name) match {
      case true  => variables(name) = value;
      case false => parent.set(name, value)
    }
  }
}

class TopEnvironment extends Environment {
  
  def get(name: String): Form = {
    variables.get(name) match {
      case Some(v) => v
      case None    => throw new VariableNotBound(name)
    }
  }

  def set(name: String, value: Form) {
    variables.contains(name) match {
      case true  => variables(name) = value;
      case false => throw new VariableNotBound(name)
    }
  }
  variables += ("eq?" -> Eq());  
  variables += ("eql?" -> Eql());  
  variables += ("display" -> Display());  
  variables ++= BooleanNatives.natives.mapValues(NativeProcedure(_))
  variables ++= NumberNatives.natives.mapValues(NativeProcedure(_))
  
}