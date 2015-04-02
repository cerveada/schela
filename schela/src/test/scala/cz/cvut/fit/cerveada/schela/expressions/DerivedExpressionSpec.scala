package cz.cvut.fit.cerveada.schela.expressions

import cz.cvut.fit.cerveada.schela.Bool
import cz.cvut.fit.cerveada.schela.Number
import cz.cvut.fit.cerveada.schela.Unspecified
import cz.cvut.fit.cerveada.schela.SList
import cz.cvut.fit.cerveada.schela.Form
import cz.cvut.fit.cerveada.schela.UnitSpec
import cz.cvut.fit.cerveada.schela.TopEnvironment

class DerivedExpressionSpec extends UnitSpec{

    
  "An And " should "work corecty" in {

    val env = new TopEnvironment()
    
    eval("(and)", env) should be(Bool(true))
    
    eval("(and #t)", env) should be(Bool(true))
    eval("(and #f)", env) should be(Bool(false))
    
    eval("(and #t #t)", env) should be(Bool(true))
    eval("(and #f #t)", env) should be(Bool(false))
    eval("(and #t #f)", env) should be(Bool(false))
    eval("(and #f #f)", env) should be(Bool(false))
    
    eval("(and #f #f #f #f)", env) should be(Bool(false))
    eval("(and #t #t #f #t #t)", env) should be(Bool(false))
    eval("(and #t #t #t #t #t #t)", env) should be(Bool(true))
       
    eval("(define x 5)", env)
    eval("(define y 5)", env)
    eval("(define z 5)", env)
    eval("(define (set-x) (set! x 42) #t)", env)
    eval("(define (set-y) (set! y 42) #t)", env)

    eval("(and #t (set-x) #f (set-y) #t)", env) should be(Bool(false))
    eval("x", env) should be(Number(42))
    eval("y", env) should be(Number(5))
    eval("z", env) should be(Number(5))
    
  }
  
  "An Or " should "work corecty" in {
    
    val env = new TopEnvironment()
    
    eval("(or)", env) should be(Bool(false))
    
    eval("(or #t)", env) should be(Bool(true))
    eval("(or #f)", env) should be(Bool(false))
    
    eval("(or #t #t)", env) should be(Bool(true))
    eval("(or #f #t)", env) should be(Bool(true))
    eval("(or #t #f)", env) should be(Bool(true))
    eval("(or #f #f)", env) should be(Bool(false))
    
    eval("(or #f #f #f #f)", env) should be(Bool(false))
    eval("(or #t #t #f #t #t)", env) should be(Bool(true))
    eval("(or #f #f #f #t #f #f)", env) should be(Bool(true))
    
    eval("(define x 5)", env)
    eval("(define y 5)", env)
    eval("(define z 5)", env)
    eval("(define (set-x) (set! x 42) #f)", env)
    eval("(define (set-y) (set! y 42) #f)", env)

    eval("(or #f (set-x) #t (set-y) #f)", env) should be(Bool(true))
    eval("x", env) should be(Number(42))
    eval("y", env) should be(Number(5))
    eval("z", env) should be(Number(5))
  }

  "A let " should "work corecty" in {

    val env = new TopEnvironment();

    eval("(define x 101)", env)
    eval("(define y 333)", env)
    eval("(define z 666)", env)
    
    eval("""    
    (let ((x 2) (y 3))
      (* x y))
    """, env) should be(Number(6))

    eval("""
    (let ((x 2) (y 3))
      (let ((x 7)
            (z (+ x y)))
        (* z x)))
    """, env) should be(Number(35))
  }
  
  "A let* " should "work corecty" in {
	  
	  val env = new TopEnvironment();
	  
	  eval("(define x 101)", env)
	  eval("(define y 333)", env)
	  eval("(define z 666)", env)
	  
	  eval("""    
    (let ((x 2) (y 3))
      (let* ((x 7)
             (z (+ x y)))
        (* z x)))
	  """, env) should be(Number(70))
  }
}