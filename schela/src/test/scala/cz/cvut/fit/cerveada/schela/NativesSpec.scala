package cz.cvut.fit.cerveada.schela

class NativesSpec extends UnitSpec{

  "A +" should "work corecty" in {

    val environment = new TopEnvironment()

    eval("(+)", environment) should be(Number(0))
    eval("(+ 15)", environment) should be(Number(15))
    eval("(+ 5 15)", environment) should be(Number(20))
    eval("(+ 5 3 500 1)", environment) should be(Number(509))
  }

  "A *" should "work corecty" in {

    val environment = new TopEnvironment()

    eval("(*)", environment) should be(Number(1))
    eval("(* 15)", environment) should be(Number(15))
    eval("(* 5 15)", environment) should be(Number(75))
    eval("(* 5 3 500 1)", environment) should be(Number(7500))
  }

  "A -" should "work corecty" in {

    val environment = new TopEnvironment()
    
    an [UnexpectedNumberOfArguments] should be thrownBy eval("(-)", environment)
    eval("(- 15)", environment) should be(Number(-15))
    eval("(- 5 15)", environment) should be(Number(-10))
    eval("(- 5 3 500 1)", environment) should be(Number(-499))
  }

  /*
   * TODO - hard to test for integers only
   * 
   */
  "A /" should "work corecty" in {

    val environment = new TopEnvironment()
    
    an [UnexpectedNumberOfArguments] should be thrownBy eval("(/)", environment)
    eval("(/ 15)", environment) should be(Number(0))
    eval("(/ 15 5)", environment) should be(Number(3))
    eval("(/ 3000 3 500 1)", environment) should be(Number(2))
  }

  "A boolean? " should "work corecty" in {

    val environment = new TopEnvironment()

    eval("(boolean? #t)", environment) should be(Bool(true))
    eval("(boolean? #f)", environment) should be(Bool(true))
    
    eval("(boolean? '())", environment) should be(Bool(false))
    eval("(boolean? 'a)", environment) should be(Bool(false))
    eval("(boolean? 0)", environment) should be(Bool(false))
  }
  
  "A not " should "work corecty" in {

    val environment = new TopEnvironment()

    an[UnexpectedNumberOfArguments] should be thrownBy eval("(not)", environment)
    an[UnexpectedNumberOfArguments] should be thrownBy eval("(not #t #t)", environment)
    eval("(not #t)", environment) should be(Bool(false))
    eval("(not #f)", environment) should be(Bool(true))
  }
  
  "An And " should "work corecty" in {

    val environment = new TopEnvironment()
    
    eval("(and)", environment) should be(Bool(true))
    
    eval("(and #t)", environment) should be(Bool(true))
    eval("(and #f)", environment) should be(Bool(false))
    
    eval("(and #t #t)", environment) should be(Bool(true))
    eval("(and #f #t)", environment) should be(Bool(false))
    eval("(and #t #f)", environment) should be(Bool(false))
    eval("(and #f #f)", environment) should be(Bool(false))
    
    eval("(and #f #f #f #f)", environment) should be(Bool(false))
    eval("(and #t #t #f #t #t)", environment) should be(Bool(false))
    eval("(and #t #t #t #t #t #t)", environment) should be(Bool(true))
  }
  
  "An Or " should "work corecty" in {
	  
	  val environment = new TopEnvironment()
	  
    eval("(or)", environment) should be(Bool(false))
    
    eval("(or #t)", environment) should be(Bool(true))
    eval("(or #f)", environment) should be(Bool(false))
    
	  eval("(or #t #t)", environment) should be(Bool(true))
	  eval("(or #f #t)", environment) should be(Bool(true))
	  eval("(or #t #f)", environment) should be(Bool(true))
	  eval("(or #f #f)", environment) should be(Bool(false))
	  
	  eval("(or #f #f #f #f)", environment) should be(Bool(false))
	  eval("(or #t #t #f #t #t)", environment) should be(Bool(true))
	  eval("(or #f #f #f #t #f #f)", environment) should be(Bool(true))
  }

}