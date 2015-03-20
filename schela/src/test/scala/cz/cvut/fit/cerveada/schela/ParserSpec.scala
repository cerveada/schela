package cz.cvut.fit.cerveada.schela

class ParserSpec extends UnitSpec {

  "A CalcParser" should "parse single number" in {    
    
    val result = TokenParser.parseItem("158") 
    result.get should be(Number(158));
  }
  
  "A CalcParser" should "parse expresion with adding and multiplication" in {
    
    val result = TokenParser.parseItem("(+ (* 4 5) (+ 42 6))")
    result.get should be(SList(Symbol("+") :: 
      SList(Symbol("*") :: Number(4) :: Number(5) :: Nil) ::
      SList(Symbol("+") :: Number(42) :: Number(6) :: Nil) :: 
    Nil))
  }
  
  "A CalcParser" should "parse booleans" in {
    
    val result = TokenParser.parseItem("#t")
    result.get should be(Bool(true))
    
    val result2 = TokenParser.parseItem("#f")
    result2.get should be(Bool(false))
  }
  
}