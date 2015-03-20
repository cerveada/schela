package cz.cvut.fit.cerveada.schela

class NativesSpec extends UnitSpec{

  "A +" should "work corecty" in {

    val enviroment = new TopEnvironment()

    val zeroNumbers = FunCall("+", List())
    zeroNumbers.evaluate(enviroment) should be(Number(0))

    val oneNumber = FunCall("+", List(Number(15)))
    oneNumber.evaluate(enviroment) should be(Number(15))
    
    val twoNumber = FunCall("+", List(Number(5), Number(15)))
    twoNumber.evaluate(enviroment) should be(Number(20))

    val fourNumbers = FunCall("+", List(Number(5), Number(3), Number(500), Number(1)))
    fourNumbers.evaluate(enviroment) should be(Number(509))
  }

  "A *" should "work corecty" in {

    val enviroment = new TopEnvironment()

    val zeroNumbers = FunCall("*", List())
    zeroNumbers.evaluate(enviroment) should be(Number(1))
    
    val oneNumber = FunCall("*", List(Number(15)))
    oneNumber.evaluate(enviroment) should be(Number(15))

    val twoNumber = FunCall("*", List(Number(5), Number(15)))
    twoNumber.evaluate(enviroment) should be(Number(75))

    val fourNumbers = FunCall("*", List(Number(5), Number(3), Number(500), Number(1)))
    fourNumbers.evaluate(enviroment) should be(Number(7500))
  }

  "A -" should "work corecty" in {

    val enviroment = new TopEnvironment()

    val zeroNumbers = FunCall("-", List())
    an [UnexpectedNumberOfArguments] should be thrownBy zeroNumbers.evaluate(enviroment)

    val oneNumber = FunCall("-", List(Number(15)))
    oneNumber.evaluate(enviroment) should be(Number(-15))

    val twoNumber = FunCall("-", List(Number(5), Number(15)))
    twoNumber.evaluate(enviroment) should be(Number(-10))

    val fourNumbers = FunCall("-", List(Number(5), Number(3), Number(500), Number(1)))
    fourNumbers.evaluate(enviroment) should be(Number(-499))
  }

  /*
   * TODO - hard to test for integers only
   * 
   */
  "A /" should "work corecty" in {

    val enviroment = new TopEnvironment()

    val zeroNumbers = FunCall("/", List())
    an [UnexpectedNumberOfArguments] should be thrownBy zeroNumbers.evaluate(enviroment)

    val oneNumber = FunCall("/", List(Number(15)))
    oneNumber.evaluate(enviroment) should be(Number(0))

    val twoNumber = FunCall("/", List(Number(15), Number(5)))
    twoNumber.evaluate(enviroment) should be(Number(3))

    val fourNumbers = FunCall("/", List(Number(3000), Number(3), Number(500), Number(1)))
    fourNumbers.evaluate(enviroment) should be(Number(2))
  }

}