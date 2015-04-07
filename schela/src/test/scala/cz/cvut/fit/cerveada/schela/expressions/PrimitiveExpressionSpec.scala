package cz.cvut.fit.cerveada.schela.expressions

import cz.cvut.fit.cerveada.schela.Bool
import cz.cvut.fit.cerveada.schela.Number
import cz.cvut.fit.cerveada.schela.Unspecified
import cz.cvut.fit.cerveada.schela.SList
import cz.cvut.fit.cerveada.schela.SProcedure
import cz.cvut.fit.cerveada.schela.Form
import cz.cvut.fit.cerveada.schela.UnitSpec
import cz.cvut.fit.cerveada.schela.TopEnvironment


class PrimitiveExpressionSpec extends UnitSpec{
  
  "A IfElse" should "evaluate corecty" in {
    val enviroment = new TopEnvironment

    eval("(if #t (+ 5 15) (* 5 5))", enviroment) should be(Number(20))

    eval("(if #f (+ 5 15) (* 5 5))", enviroment) should be(Number(25))

    eval("(if #f (+ 5 15))", enviroment) should be(Unspecified())
  }

  "Application" should "work" in {
    val env = new TopEnvironment()

    eval("( (lambda (a) (* a a)) 5 )", env) should be(Number(25))
    
    eval("(define x 1)", env)
    eval("( (lambda (a) (set! x 42) 8 (+ a a)) 5 )", env) should be(Number(10))
    eval("x", env) should be(Number(42))
    
    eval("((lambda (x) (+ x x)) 4)", env) should be(Number(8))
    
    eval("(define reverse-subtract (lambda (x y) (- y x)))", env)
    eval("(reverse-subtract 7 10)", env) should be(Number(3))
    
    eval("""
    (define add4
      (let ((x 4))
        (lambda (y) (+ x y))))
     """, env)
    eval("(add4 6)", env) should be(Number(10))
    
  }

  "Quote" should "evaluate in it's content" in {
    val enviroment = new TopEnvironment()

    eval("(quote ())", enviroment) should be(SList(List()))
    eval("'()", enviroment) should be(SList(List()))

    eval("'(4 5)", enviroment) should be(SList(List(Number(4), Number(5))))
    eval("(quote (4 5))", enviroment) should be(SList(List(Number(4), Number(5))))
  }
  
   "A set!" should "evaluate corecty" in {
    val env = new TopEnvironment

    eval("(define x 2)", env) 
    eval("(+ x 1)", env) should be(Number(3))
    eval("(set! x 4)", env) should be(Unspecified())
    eval("(+ x 1)", env) should be(Number(5))
  }
}