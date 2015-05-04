package cz.cvut.fit.cerveada.schela.natives

import cz.cvut.fit.cerveada.schela.Bool
import cz.cvut.fit.cerveada.schela.Form
import cz.cvut.fit.cerveada.schela.TopEnvironment
import cz.cvut.fit.cerveada.schela.UnexpectedNumberOfArguments
import cz.cvut.fit.cerveada.schela.UnexpectedNumberOfArguments
import cz.cvut.fit.cerveada.schela.UnexpectedType
import cz.cvut.fit.cerveada.schela.UnitSpec

class BooleanSpec extends UnitSpec{

  "A boolean? " should "work corecty" in {

    val environment = new TopEnvironment()

    eval("(boolean? #t)", environment) should be(Bool(true))
    eval("(boolean? #f)", environment) should be(Bool(true))
    
    eval("(boolean? '())", environment) should be(Bool(false))
    eval("(boolean? 'a)", environment) should be(Bool(false))
    eval("(boolean? 0)", environment) should be(Bool(false))
  }
  
  "A not " should "work corecty" in {

    val environment = new TopEnvironment()

    an[UnexpectedNumberOfArguments] should be thrownBy eval("(not)", environment)
    an[UnexpectedNumberOfArguments] should be thrownBy eval("(not #t #t)", environment)
    resultOf("(not #t)", environment) should be(Bool(false))
    resultOf("(not #f)", environment) should be(Bool(true))
  }
}