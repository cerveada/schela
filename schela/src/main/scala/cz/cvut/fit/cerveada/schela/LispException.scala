package cz.cvut.fit.cerveada.schela

import scala.util.control.Exception

class LispException(val message:String) extends Exception 

class UnexpectedNumberOfArguments(val given:Int, val expected:Int) 
extends LispException(s"Unexpected number of arguments: ${given} expected: ${expected}")

class UnexpectedType(val given:Value, val expected:Value)
extends LispException(s"Unexpected type, found: ${given.typeName} expected: ${expected.typeName}")

class VariableNotBound(name:String)
extends LispException(s"$name is not bound")

class VariableAlreadyDefined(name:String)
extends LispException(s"$name is already defined")
