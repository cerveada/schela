package cz.cvut.fit.cerveada.schela

import scala.util.control.Exception

class LispException(val message:String) extends Exception 

class UnexpectedNumberOfArguments(val given:Int, val expected:Int) 
extends LispException(s"Unexpected number of arguments: ${given} expected: ${expected}")

class UnexpectedType(val given:Form, val expected:Form)
extends LispException(s"Unexpected type, found: ${given.typeName} expected: ${expected.typeName}")

class VariableNotBound(name:String)
extends LispException(s"$name is not bound")

class VariableAlreadyDefined(name:String)
extends LispException(s"$name is already defined")

class SyntaxException(syntaxName:String)
extends LispException(s"wrong $syntaxName syntax")

class ImmutableModification(name:String)
extends LispException(s"attempt to modify immutable $name")

