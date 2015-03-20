package cz.cvut.fit.cerveada.schela

class ContextSpec extends UnitSpec{
  
  "Each procedure" should "have it's own context" in {    
    
    val environment = new TopEnvironment()

    VarDef("a",Number(5)).evaluate(environment)
    VarDef("foo", Lambda(List("a","b"), Symbol("a"))).evaluate(environment)
    FunCall("foo", List(Number(42), Number(52))).evaluate(environment)
    val result = Symbol("a").evaluate(environment)
     
    result should be(Number(5));
  }
  
  "Closures" should "work properly" in {    
    
    val environment = new TopEnvironment()

    VarDef("a",Number(5)).evaluate(environment)
    VarDef("foo", Lambda(List(), Symbol("a"))).evaluate(environment)
    VarDef("bar", Lambda(List("a"), FunCall("foo", List()))).evaluate(environment)
    
    
    var result = FunCall("bar", List(Number(42))).evaluate(environment)
    result should be(Number(5));
    
    Set("a",Number(666)).evaluate(environment)
    result = FunCall("bar", List(Number(42))).evaluate(environment)
    result should be(Number(666));
  }
}