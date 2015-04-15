package cz.cvut.fit.cerveada.schela

import cz.cvut.fit.cerveada.schela.natives.BooleanNatives
import cz.cvut.fit.cerveada.schela.natives.NumberNatives
import cz.cvut.fit.cerveada.schela.natives.SymbolNatives
import cz.cvut.fit.cerveada.schela.natives.EquivalenceNatives
import cz.cvut.fit.cerveada.schela.natives.InputOutputNatives
import cz.cvut.fit.cerveada.schela.natives.ListNatives
import cz.cvut.fit.cerveada.schela.natives.VectorNatives

abstract class Environment {
	protected val variables = scala.collection.mutable.Map[Symbol, Form]();
  
  def define(name: Symbol, value: Form) {
    variables.contains(name) match {
      case true  => throw new VariableAlreadyDefined(name.name)
      case false => variables += (name -> value); 
    }
  }
  
  def get(name:Symbol): Form;
  def set(name:Symbol, value:Form)
}

class LocalEnvironment(parent: Environment) extends Environment {

  def get(name: Symbol): Form = {
    variables.get(name) match {
      case Some(v) => v
      case None    => parent.get(name)
    }
  }
  
  def set(name: Symbol, value: Form) {
    variables.contains(name) match {
      case true  => variables(name) = value;
      case false => parent.set(name, value)
    }
  }
}

class TopEnvironment extends Environment {
  
  def get(name: Symbol): Form = {
    variables.get(name) match {
      case Some(v) => v
      case None    => throw new VariableNotBound(name.name)
    }
  }

  def set(name: Symbol, value: Form) {
    variables.contains(name) match {
      case true  => variables(name) = value;
      case false => throw new VariableNotBound(name.name)
    }
  }

  addVariables(BooleanNatives.natives)
  addVariables(NumberNatives.natives)
  addVariables(SymbolNatives.natives)
  addVariables(EquivalenceNatives.natives)
  addVariables(InputOutputNatives.natives)
  addVariables(ListNatives.natives)
  addVariables(VectorNatives.natives)
  
  
  private type procedureType = List[Form] => Form
  private type inMap = scala.collection.mutable.Map[String,procedureType]
  private type outMap = scala.collection.mutable.Map[Symbol,procedureType]
  private def toSymbolMap(map:inMap):outMap = {
    map.map{case(k,v) => Symbol(k) -> v}
  }
  
  private def addVariables(map:inMap) {
      variables ++= toSymbolMap(map).mapValues(NativeProcedure(_))
  }
  
  
}