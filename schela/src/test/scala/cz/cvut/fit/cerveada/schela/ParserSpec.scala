package cz.cvut.fit.cerveada.schela

import scala.collection.mutable.ArraySeq

class ParserSpec extends UnitSpec {

  "A Parser" should "parse single number" in {    
    
    val result = Parser.parseItem("158") 
    result.get should be(Number(158));
  }
  
  "A Parser" should "parse expresion with adding and multiplication" in {
    
    val result = Parser.parseItem("(+ (* 4 5) (+ 42 6))")
    result.get should be(SList(SSymbol('+) :: 
      SList(SSymbol('*) :: Number(4) :: Number(5) :: Nil) ::
      SList(SSymbol('+) :: Number(42) :: Number(6) :: Nil) :: 
    Nil))
  }
  
  "A Parser" should "parse booleans" in {
    
    val result = Parser.parseItem("#t")
    result.get should be(Bool(true))
    
    val result2 = Parser.parseItem("#f")
    result2.get should be(Bool(false))
  }
  
  "A Parser" should "not be case sensitive" in {
    
    val env = new TopEnvironment;
    
    eval("(define xXx 42)", env)
    eval("XxX", env) should be(Number(42))
    
    eval("(define (foOBaR x) (+ x 42))", env)
    eval("(foOBaR 4)", env) should be(Number(46))
    
    eval("'abCdEFgh", env) should be(SSymbol('abcdefgh))
  }

  "A Parser" should "parse vector" in {

    val env = new TopEnvironment;

    eval("'#(1 2 3)", env) should equal(SVector(ArraySeq(Number(1), Number(2), Number(3))))
  }
  
  "A Char" should "be parsed corectly" in {
	  
	  val env = new TopEnvironment;
	  
	  eval("""#\a""", env) should be(SChar('a'))
	  eval("""#\A""", env) should be(SChar('A'))
	  eval("""#\(""", env) should be(SChar('('))
	  eval("""#\space""", env) should be(SChar(' '))
	  eval("""#\newline""", env) should be(SChar('\n'))
  }
  
 
  
}