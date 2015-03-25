package cz.cvut.fit.cerveada.schela

class NumberSpec extends UnitSpec {

  "A =" should "work corecty" in {

    val env = new TopEnvironment()

    an[UnexpectedNumberOfArguments] should be thrownBy eval("(= 5)", env)
    an[UnexpectedType] should be thrownBy eval("(= 5 'a)", env)

    eval("(= 5 5)", env) should be(Bool(true))
    eval("(= 200 200)", env) should be(Bool(true))
    eval("(= 5 6)", env) should be(Bool(false))

    eval("(= 5 6 7 8 9)", env) should be(Bool(false))
    eval("(= 9 9 9 9 9)", env) should be(Bool(true))
  }

  "A <" should "work corecty" in {

    val env = new TopEnvironment()

    an[UnexpectedNumberOfArguments] should be thrownBy eval("(< 5)", env)
    an[UnexpectedType] should be thrownBy eval("(< 5 'a)", env)
    eval("(< 5 5)", env) should be(Bool(false))
    eval("(< 199 200)", env) should be(Bool(true))
    eval("(< 8 6)", env) should be(Bool(false))

    eval("(< 5 6 7 8 9)", env) should be(Bool(true))
    eval("(< 9 9 9 9 9)", env) should be(Bool(false))
    eval("(< 5 6 4 3 9 10)", env) should be(Bool(false))
  }

  "A <=" should "work corecty" in {

    val env = new TopEnvironment()

    an[UnexpectedNumberOfArguments] should be thrownBy eval("(<= 5)", env)
    an[UnexpectedType] should be thrownBy eval("(<= 5 'a)", env)
    eval("(<= 5 5)", env) should be(Bool(true))
    eval("(<= 199 200)", env) should be(Bool(true))
    eval("(<= 8 6)", env) should be(Bool(false))

    eval("(<= 5 6 7 8 9)", env) should be(Bool(true))
    eval("(<= 9 9 9 9 9)", env) should be(Bool(true))
    eval("(<= 5 6 4 3 9 10)", env) should be(Bool(false))
  }

  "A >" should "work corecty" in {

    val env = new TopEnvironment()

    an[UnexpectedNumberOfArguments] should be thrownBy eval("(> 5)", env)
    an[UnexpectedType] should be thrownBy eval("(> 5 'a)", env)
    eval("(> 5 5)", env) should be(Bool(false))
    eval("(> 199 200)", env) should be(Bool(false))
    eval("(> 8 6)", env) should be(Bool(true))

    eval("(> 9 8 7 6 5 4 3)", env) should be(Bool(true))
    eval("(> 9 9 9 9 9)", env) should be(Bool(false))
    eval("(> 5 6 4 3 9 10)", env) should be(Bool(false))
  }

  "A >=" should "work corecty" in {

    val env = new TopEnvironment()

    an[UnexpectedNumberOfArguments] should be thrownBy eval("(>= 5)", env)
    an[UnexpectedType] should be thrownBy eval("(>= 5 'a)", env)
    eval("(>= 5 5)", env) should be(Bool(true))
    eval("(>= 199 200)", env) should be(Bool(false))
    eval("(>= 8 6)", env) should be(Bool(true))

    eval("(>= 7 6 3 2 1)", env) should be(Bool(true))
    eval("(>= 9 9 9 9 9)", env) should be(Bool(true))
    eval("(>= 5 6 4 3 9 10)", env) should be(Bool(false))
  }
  
  "A zero?" should "work corectly" in {
	  
	  val env = new TopEnvironment()
	  
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(zero? 5 6)", env)
	  an[UnexpectedType] should be thrownBy eval("(zero? 'a)", env)
	  eval("(zero? 1)", env) should be(Bool(false))
	  eval("(zero? -1)", env) should be(Bool(false))
	  eval("(zero? 0)", env) should be(Bool(true))
  }
 
  "A positive?" should "work corectly" in {
	  
	  val env = new TopEnvironment()
	  
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(positive? 5 6)", env)
    an[UnexpectedType] should be thrownBy eval("(positive? 'a)", env)
    eval("(positive? 1)", env) should be(Bool(true))
    eval("(positive? -1)", env) should be(Bool(false))
    eval("(positive? 0)", env) should be(Bool(false))
  }
  
  "A negative?" should "work corectly" in {
	  
	  val env = new TopEnvironment()
	  
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(negative? 5 6)", env)
	  an[UnexpectedType] should be thrownBy eval("(negative? 'a)", env)
	  eval("(negative? 1)", env) should be(Bool(false))
	  eval("(negative? -1)", env) should be(Bool(true))
	  eval("(negative? 0)", env) should be(Bool(false))
  }
  
  "A odd?" should "work corectly" in {
	  
	  val env = new TopEnvironment()
	  
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(odd? 5 6)", env)
	  an[UnexpectedType] should be thrownBy eval("(odd? 'a)", env)
    
	  eval("(odd? 2)", env) should be(Bool(false))
	  eval("(odd? 1)", env) should be(Bool(true))
	  eval("(odd? 0)", env) should be(Bool(false))
	  eval("(odd? -1)", env) should be(Bool(true))
	  eval("(odd? -2)", env) should be(Bool(false))
  }
  
  "A even?" should "work corectly" in {
	  
	  val env = new TopEnvironment()
	  
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(even? 5 6)", env)
	  an[UnexpectedType] should be thrownBy eval("(even? 'a)", env)
	  
	  eval("(even? 2)", env) should be(Bool(true))
	  eval("(even? 1)", env) should be(Bool(false))
	  eval("(even? 0)", env) should be(Bool(true))
	  eval("(even? -1)", env) should be(Bool(false))
	  eval("(even? -2)", env) should be(Bool(true))
  }

  "A max" should " return the maximum of its arguments" in {

    val env = new TopEnvironment()

    an[UnexpectedNumberOfArguments] should be thrownBy eval("(max)", env)
    an[UnexpectedType] should be thrownBy eval("(max 5 10 23 'a 56)", env)

    eval("(max 156 556 6 4 619 89 11)", env) should be(Number(619))
    eval("(max 156 -556 6 4 -619 89 -11)", env) should be(Number(156))
    eval("(max -156 -556 6 4 -619 -89 -11)", env) should be(Number(6))
  }
  
  "A min" should "return the minimum of its arguments" in {
	  
	  val env = new TopEnvironment()
	  
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(min)", env)
	  an[UnexpectedType] should be thrownBy eval("(min 5 10 23 'a 56)", env)
	  
	  eval("(min 156 556 6 4 619 89 11)", env) should be(Number(4))
	  eval("(min 156 -556 6 4 -619 89 -11)", env) should be(Number(-619))
	  eval("(min -156 556 6 4 619 -89 -11)", env) should be(Number(-156))
  }
  
  
}