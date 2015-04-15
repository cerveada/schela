package cz.cvut.fit.cerveada.schela.natives

import cz.cvut.fit.cerveada.schela.Bool
import cz.cvut.fit.cerveada.schela.Form
import cz.cvut.fit.cerveada.schela.TopEnvironment
import cz.cvut.fit.cerveada.schela.UnexpectedNumberOfArguments
import cz.cvut.fit.cerveada.schela.UnexpectedNumberOfArguments
import cz.cvut.fit.cerveada.schela.UnexpectedType
import cz.cvut.fit.cerveada.schela.UnitSpec

class EquivalenceSpec extends UnitSpec{

  "An eqv? " should "work corectly" in {

    val env = new TopEnvironment()
    
     eval("(eqv? 'a 'a)", env) should be(Bool(true))
     eval("(eqv? 'a 'b)", env) should be(Bool(false))
     eval("(eqv? 2 2)", env) should be(Bool(true))
     eval("(eqv? '() '())", env) should be(Bool(true))
     eval("(eqv? 100000000 100000000)", env) should be(Bool(true))
     //eval("(eqv? (cons 1 2) (cons 1 2))", env) should be(Bool(false)) //TODO cons
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
  
  "An eq? " should "work corectly" in {
	  
	  val env = new TopEnvironment()
	  
    eval("(eq? 'a 'a)", env) should be(Bool(true))
    //eval("(eq? (list 'a) (list 'a))", env) should be(Bool(false))  //TODO list
    eval("(eq? '() '())", env) should be(Bool(true))
    eval("(eq? car car)", env) should be(Bool(true))
    eval("""
      (let ((x '(a)))
        (eq? x x))                
      """, env) should be(Bool(true))
    eval("""
      (let ((x '#()))
        (eq? x x))                
     """, env) should be(Bool(true))
     eval("""
      (let ((p (lambda (x) x)))
        (eq? p p))                
      """, env) should be(Bool(true))
  }
  
  "An equal? " should "recursively compare the contents" in {
	  
	  val env = new TopEnvironment()
	  
    
    eval("(equal? 'a 'a)", env) should be(Bool(true))
    eval("(equal? '(a) '(a))", env) should be(Bool(true))
    eval("""
      (equal? '(a (b) c)
        '(a (b) c))
      """, env) should be(Bool(true))
    eval("(equal? \"abc\" \"abc\")", env) should be(Bool(true))
    eval("(equal? 2 2)", env) should be(Bool(true))

    eval("""
      (equal? (make-vector 5 'a)
        (make-vector 5 'a))
      """, env) should be(Bool(true))
  }
  

}