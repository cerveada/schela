package cz.cvut.fit.cerveada.schela

class ContextSpec extends UnitSpec{
  
  "Each procedure" should "have it's own context" in {    
    
    val environment = new TopEnvironment()

    eval("(define a 5)", environment)
    eval("(define foo (lambda (a b) a))", environment)
    eval("(foo 42 52)", environment)
    eval("a", environment) should be(Number(5));
  }
  
  "Closures" should "work properly" in {    
    
    val environment = new TopEnvironment()

    eval("(define a 5)", environment)
    eval("(define foo (lambda () a))", environment)
    eval("(define bar (lambda (a) (foo)))", environment)
    eval("(bar 42)", environment) should be(Number(5))
    
    eval("(set! a 666)", environment)
    eval("(bar 42)", environment) should be(Number(666))
  }
}