package cz.cvut.fit.cerveada.schela.natives

import cz.cvut.fit.cerveada.schela.Bool
import cz.cvut.fit.cerveada.schela.Form
import cz.cvut.fit.cerveada.schela.TopEnvironment
import cz.cvut.fit.cerveada.schela.UnexpectedNumberOfArguments
import cz.cvut.fit.cerveada.schela.UnexpectedNumberOfArguments
import cz.cvut.fit.cerveada.schela.UnexpectedType
import cz.cvut.fit.cerveada.schela.UnitSpec

class EquivalenceSpec extends UnitSpec{

  "A eqv? " should "work corecty" in {

    val env = new TopEnvironment()
    
     eval("(eqv? 'a 'a)", env) should be(Bool(true))
     eval("(eqv? 'a 'b)", env) should be(Bool(false))
     eval("(eqv? 2 2)", env) should be(Bool(true))
     eval("(eqv? '() '())", env) should be(Bool(true))
     eval("(eqv? 100000000 100000000)", env) should be(Bool(true))
     //eval("(eqv? (cons 1 2) (cons 1 2))", env) should be(Bool(false))
     eval("""
       (eqv? (lambda () 1)
          (lambda () 2))
       """, env) should be(Bool(false))
     eval("(eqv? #f 'nil)", env) should be(Bool(false))
     eval("""
      (let ((p (lambda (x) x)))
        (eqv? p p))               
        """, env) should be(Bool(true))
  }
  

}