package cz.cvut.fit.cerveada.schela.natives

import cz.cvut.fit.cerveada.schela.Bool
import cz.cvut.fit.cerveada.schela.Form
import cz.cvut.fit.cerveada.schela.SString
import cz.cvut.fit.cerveada.schela.SList
import cz.cvut.fit.cerveada.schela.Symbol
import cz.cvut.fit.cerveada.schela.Number
import cz.cvut.fit.cerveada.schela.TopEnvironment
import cz.cvut.fit.cerveada.schela.UnexpectedNumberOfArguments
import cz.cvut.fit.cerveada.schela.UnexpectedType
import cz.cvut.fit.cerveada.schela.UnitSpec
import cz.cvut.fit.cerveada.schela.LispException

class LisSpec extends UnitSpec {

  "A car" should "return the contents of the car field of pair. " + 
  "It is an error to take the car of the empty list. " in {

    val env = new TopEnvironment()

    an[LispException] should be thrownBy eval("(car '())", env)
    an[UnexpectedType] should be thrownBy eval("(car 42)", env)
    an[UnexpectedNumberOfArguments] should be thrownBy eval("(car '(5 2) '(4 5))", env)
    
    eval("(car '(a b c))", env) should be(Symbol("a"))
    eval("(car '((a) b c d)) ", env) should be(SList(Symbol("a") :: Nil))
    // TODO
    //eval("(car '(1 . 2))", env) should be(Number(1))
    eval("(car '(3 4 5))", env) should be(Number(3))
  }
  
  "A cdr" should "return the contents of the cdr field of pair. " + 
  "It is an error to take the cdr of the empty list. " in {
	  
	  val env = new TopEnvironment()
	  
	  an[LispException] should be thrownBy eval("(cdr '())", env)
	  an[UnexpectedType] should be thrownBy eval("(cdr 42)", env)
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(cdr '(5 2) '(4 5))", env)
	  
	  eval("(cdr '(a b))", env) should be(SList(Symbol("b") :: Nil))
	  eval("(cdr '((a) b c d)) ", env) should be(SList(Symbol("b") :: Symbol("c") :: Symbol("d") :: Nil))
	  // TODO
	  //eval("(cdr '(1 . 2))", env) should be(Number(2))
	  eval("(cdr '(3 4))", env) should be(SList(Number(4) :: Nil))

  }
  
  
  
}