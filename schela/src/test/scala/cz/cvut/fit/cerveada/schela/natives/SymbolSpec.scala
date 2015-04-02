package cz.cvut.fit.cerveada.schela.natives

import cz.cvut.fit.cerveada.schela.Bool
import cz.cvut.fit.cerveada.schela.Form
import cz.cvut.fit.cerveada.schela.SString
import cz.cvut.fit.cerveada.schela.TopEnvironment
import cz.cvut.fit.cerveada.schela.UnexpectedNumberOfArguments
import cz.cvut.fit.cerveada.schela.UnexpectedType
import cz.cvut.fit.cerveada.schela.UnitSpec

class SymbolSpec extends UnitSpec {

  "A symbol?" should "return #t if obj is a symbol, otherwise #f." in {

    val env = new TopEnvironment()

    an[UnexpectedNumberOfArguments] should be thrownBy eval("(symbol?)", env)
    an[UnexpectedNumberOfArguments] should be thrownBy eval("(symbol? 5 45)", env)
    eval("(symbol? 5)", env) should be(Bool(false))
    eval("(symbol? 'foo)", env) should be(Bool(true))
    //eval("(symbol? (car '(a b)))", env) should be(Bool(true))
    eval("(symbol? \"bar\")", env) should be(Bool(false))
    eval("(symbol? 'nil)", env) should be(Bool(true))
    eval("(symbol? '())", env) should be(Bool(false))
    eval("(symbol? #f)", env) should be(Bool(false))

  }

  "A symbol->string" should "return the name of symbol as a string" in {
	  
	  val env = new TopEnvironment()
	  
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(symbol->string)", env)
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(symbol->string 'a 'b)", env)
	  an[UnexpectedType] should be thrownBy eval("(symbol->string 42)", env)

    eval("(symbol->string 'flying-fish)", env) should be(SString("flying-fish"))
    eval("(symbol->string 'Martin)", env) should be(SString("martin"))
    //eval("(symbol->string (string->symbol \"Malvina\"))", env) should be(SString("Malvina"))
  }
  
  "A string->symbol" should "return the symbol whose name is string." in {
	  
	  val env = new TopEnvironment()
	  
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(string->symbol)", env)
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(string->symbol 'a 'b)", env)
	  an[UnexpectedType] should be thrownBy eval("(string->symbol 42)", env)
	  /*
	  eval("(string->symbol \"56\")", env) should be(Symbol("56"))
	  eval("(string->symbol \"-666\")", env) should be(Symbol("-666"))
    * 
    */
  }

}