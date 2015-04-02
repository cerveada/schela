package cz.cvut.fit.cerveada.schela.expressions

import cz.cvut.fit.cerveada.schela.Bool
import cz.cvut.fit.cerveada.schela.Number
import cz.cvut.fit.cerveada.schela.Unspecified
import cz.cvut.fit.cerveada.schela.SList
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
    val enviroment = new TopEnvironment()

    eval("( (lambda (a) (* a a)) 5 )", enviroment) should be(Number(25))
    
    eval("(define x 1)", enviroment)
    eval("( (lambda (a) (set! x 42) 8 (+ a a)) 5 )", enviroment) should be(Number(10))
    eval("x", enviroment) should be(Number(42))

  }

  "Quote" should "evaluate in it's content" in {
    val enviroment = new TopEnvironment()

    eval("(quote ())", enviroment) should be(SList(List()))
    eval("'()", enviroment) should be(SList(List()))

    eval("'(4 5)", enviroment) should be(SList(List(Number(4), Number(5))))
    eval("(quote (4 5))", enviroment) should be(SList(List(Number(4), Number(5))))
  }
}