package cz.cvut.fit.cerveada.schela

class ParserSpec extends UnitSpec {

  "A CalcParser" should "parse single number" in {    
    
    val result = CalcParser.parseItem("158") 
    result.get should be(Number(158));
  }
  
  "A CalcParser" should "parse expresion with adding and multiplication" in {
    
    val result = CalcParser.parseItem("(+ (* 4 5) (+ 42 6))")
    result.get should be(FunCall("+", 
    List(
        FunCall("*", List(Number(4), Number(5))),
        FunCall("+", List(Number(42), Number(6)))
    )))
  }
  
  "A CalcParser" should "parse booleans" in {
    
    val result = CalcParser.parseItem("#t")
    result.get should be(Bool(true))
    
    val result2 = CalcParser.parseItem("#f")
    result2.get should be(Bool(false))
  }
  
}