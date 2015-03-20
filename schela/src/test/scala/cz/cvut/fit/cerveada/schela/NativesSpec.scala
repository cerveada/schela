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

}