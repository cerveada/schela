package cz.cvut.fit.cerveada.schela.expressions

import cz.cvut.fit.cerveada.schela.Bool
import cz.cvut.fit.cerveada.schela.Number
import cz.cvut.fit.cerveada.schela.Unspecified
import cz.cvut.fit.cerveada.schela.SList
import cz.cvut.fit.cerveada.schela.Symbol
import cz.cvut.fit.cerveada.schela.Form
import cz.cvut.fit.cerveada.schela.UnitSpec
import cz.cvut.fit.cerveada.schela.TopEnvironment
import cz.cvut.fit.cerveada.schela.Unspecified

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
    
    eval("(let ((x 5)) (set! x (+ x 2)) x)", env) should be(Number(7))
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
  
  "A letrec " should "work corecty" in {
	  
	  val env = new TopEnvironment();
	  
	  eval("""    
    (letrec ((even?
              (lambda (n)
                (if (zero? n)
                    #t
                    (odd? (- n 1)))))
             (odd?
              (lambda (n)
                (if (zero? n)
                    #f
                    (even? (- n 1))))))
      (even? 88))
		""", env) should be(Bool(true))
  }
  
  "A begin " should "evaluate expressions sequentially from left to right," +
  "and the value of the last expression is returned" in {
	  
	  val env = new TopEnvironment();
    
	  eval("(define x 0)", env)
	  eval("""    
    (begin (set! x 5)
       (+ x 1)) 
	  """, env) should be(Number(6))
  }

  "A cond" should "work coreclty" in {

    val env = new TopEnvironment();

    eval("""    
    (cond ((> 3 2) 'greater)
          ((< 3 2) 'less))) 
    """, env) should be(Symbol("greater"))

    eval("""    
    (cond ((> 3 3) 'greater)
          ((< 3 3) 'less)
          (else 'equal)) 
    """, env) should be(Symbol("equal"))
  }
  
  "A case" should "work coreclty" in {
	  
	  val env = new TopEnvironment();
    
	  eval("""    
    (case (* 2 3)
      ((2 3 5 7) 'prime)
      ((1 4 6 8 9) 'composite))
		""", env) should be(Symbol("composite"))
    
		eval("""    
    (case (car '(c d))
      ((a) 'a)
      ((b) 'b))
		""", env) should be(Unspecified())
		
		eval("""    
    (case (car '(c d))
      ((a e i o u) 'vowel)
      ((w y) 'semivowel)
      (else 'consonant))
		""", env) should be(Symbol("consonant"))
  }
}