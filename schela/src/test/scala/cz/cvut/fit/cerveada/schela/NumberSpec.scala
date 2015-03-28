package cz.cvut.fit.cerveada.schela

class NumberSpec extends UnitSpec {

  "A number?" should "work corecty" in {

    val env = new TopEnvironment()

    an[UnexpectedNumberOfArguments] should be thrownBy eval("(number?)", env)
    an[UnexpectedNumberOfArguments] should be thrownBy eval("(number? 5 45)", env)
    eval("(number? 5)", env) should be(Bool(true))
    eval("(number? 199)", env) should be(Bool(true))
    eval("(number? -89)", env) should be(Bool(true))

    eval("(number? 'a)", env) should be(Bool(false))
    eval("(number? \"ahoj\")", env) should be(Bool(false))
    eval("(number? #t)", env) should be(Bool(false))
  }

  "An exact?" should "work corectly" in {

    val env = new TopEnvironment()

    an[UnexpectedNumberOfArguments] should be thrownBy eval("(exact?)", env)
    an[UnexpectedNumberOfArguments] should be thrownBy eval("(exact? 5 45)", env)
    an[UnexpectedType] should be thrownBy eval("(exact? 'a)", env)

    eval("(exact? 5)", env) should be(Bool(true))
    eval("(exact? 199)", env) should be(Bool(true))
    eval("(exact? -89)", env) should be(Bool(true))
  }

  "An inexact?" should "work corectly" in {

    val env = new TopEnvironment()

    an[UnexpectedNumberOfArguments] should be thrownBy eval("(inexact?)", env)
    an[UnexpectedNumberOfArguments] should be thrownBy eval("(inexact? 5 45)", env)
    an[UnexpectedType] should be thrownBy eval("(inexact? 'a)", env)

    eval("(inexact? 5)", env) should be(Bool(false))
    eval("(inexact? 199)", env) should be(Bool(false))
    eval("(inexact? -89)", env) should be(Bool(false))
  }

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

    an[UnexpectedNumberOfArguments] should be thrownBy eval("(-)", environment)
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

    an[UnexpectedNumberOfArguments] should be thrownBy eval("(/)", environment)
    eval("(/ 15)", environment) should be(Number(0))
    eval("(/ 15 5)", environment) should be(Number(3))
    eval("(/ 3000 3 500 1)", environment) should be(Number(2))
  }

  "A abs" should "return the magnitude of its argument. " in {

    val env = new TopEnvironment()

    an[UnexpectedNumberOfArguments] should be thrownBy eval("(abs)", env)
    an[UnexpectedNumberOfArguments] should be thrownBy eval("(abs 1 6)", env)
    an[UnexpectedType] should be thrownBy eval("(abs 'a)", env)

    eval("(abs 1)", env) should be(Number(1))
    eval("(abs -1)", env) should be(Number(1))
    eval("(abs 42)", env) should be(Number(42))
    eval("(abs -42)", env) should be(Number(42))
  }

  "A quotient" should "return corect result" in {

    val env = new TopEnvironment()

    an[UnexpectedNumberOfArguments] should be thrownBy eval("(quotient)", env)
    an[UnexpectedNumberOfArguments] should be thrownBy eval("(quotient 1 6 5)", env)
    an[UnexpectedType] should be thrownBy eval("(quotient 'a 5)", env)

    eval("(quotient 13 4)", env) should be(Number(3))
    eval("(quotient 3 4)", env) should be(Number(0))
    eval("(quotient 19 8)", env) should be(Number(2))
    eval("(quotient -13 -4)", env) should be(Number(3))
    eval("(quotient -13 4)", env) should be(Number(-3))
    eval("(quotient 13 -4)", env) should be(Number(-3))
  }

  "A modulo" should "return corect result" in {

    val env = new TopEnvironment()

    an[UnexpectedNumberOfArguments] should be thrownBy eval("(modulo)", env)
    an[UnexpectedNumberOfArguments] should be thrownBy eval("(modulo 1 6 5)", env)
    an[UnexpectedType] should be thrownBy eval("(modulo 'a 5)", env)

    eval("(modulo 13 4)", env) should be(Number(1))
    eval("(modulo -13 4)", env) should be(Number(3))
    eval("(modulo 13 -4)", env) should be(Number(-3))
    eval("(modulo -13 -4)", env) should be(Number(-1))
  }

  "A remainder" should "return corect result" in {

    val env = new TopEnvironment()

    an[UnexpectedNumberOfArguments] should be thrownBy eval("(remainder)", env)
    an[UnexpectedNumberOfArguments] should be thrownBy eval("(remainder 1 6 5)", env)
    an[UnexpectedType] should be thrownBy eval("(remainder 'a 5)", env)

    eval("(remainder 13 4)", env) should be(Number(1))
    eval("(remainder -13 4)", env) should be(Number(-1))
    eval("(remainder 13 -4)", env) should be(Number(1))
    eval("(remainder -13 -4)", env) should be(Number(-1))
  }
  
  "A gcd" should "return the greatest common divisor of two or more integers" in {
	  
	  val env = new TopEnvironment()
	  
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(gcd)", env)
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(gcd 6)", env)
	  an[UnexpectedType] should be thrownBy eval("(gcd 8 'a 5)", env)
	  
	  eval("(gcd 56 42)", env) should be(Number(14))
	  eval("(gcd 666 128)", env) should be(Number(2))
	  eval("(gcd 1024 997)", env) should be(Number(1))
	  eval("(gcd 36 27 45 81 )", env) should be(Number(9))
  }
  
  "A lcm" should "return the least common multiple of two or more integers" in {
	  
	  val env = new TopEnvironment()
	  
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(lcm)", env)
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(lcm 6)", env)
	  an[UnexpectedType] should be thrownBy eval("(lcm 8 'a 5)", env)
	  
	  eval("(lcm 56 42)", env) should be(Number(168))
	  eval("(lcm 666 128)", env) should be(Number(42624))
	  eval("(lcm 1024 997)", env) should be(Number(1020928))
	  eval("(lcm 36 27 45 81 )", env) should be(Number(1620))
  }
  
  "A floor" should "return the largest integer not larger than x" in {
	  
	  val env = new TopEnvironment()
	  
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(floor)", env)
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(floor 6 9)", env)
	  an[UnexpectedType] should be thrownBy eval("(floor 'a)", env)
	  
	  eval("(floor 56)", env) should be(Number(56))
	  eval("(floor -666)", env) should be(Number(-666))
  }
  
  
  "A number->string" should "return the string representation fo number" in {
	  
	  val env = new TopEnvironment()
	  
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(number->string)", env)
	  an[UnexpectedType] should be thrownBy eval("(number->string 'a)", env)
	  
	  eval("(number->string 56)", env) should be(SString("56"))
	  eval("(number->string -666)", env) should be(SString("-666"))
  }
  
  "A string->number" should "return the string representation fo number" in {
	  
	  val env = new TopEnvironment()
	  
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(string->number)", env)
	  an[UnexpectedType] should be thrownBy eval("(string->number 'a)", env)
	  an[LispException] should be thrownBy eval("(string->number \"olgojchotchoj\")", env)
	  
	  eval("(string->number \"56\")", env) should be(Number(56))
	  eval("(string->number \"-666\")", env) should be(Number(-666))
  }

}