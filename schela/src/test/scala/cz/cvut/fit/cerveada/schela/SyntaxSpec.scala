package cz.cvut.fit.cerveada.schela

class SyntaxSpec extends UnitSpec {

  "A IfElse" should "evaluate corecty" in {
    val enviroment = new TopEnvironment()

    eval("(if #t (+ 5 15) (* 5 5))", enviroment) should be(Number(20))

    eval("(if #f (+ 5 15) (* 5 5))", enviroment) should be(Number(25))

    eval("(if #f (+ 5 15))", enviroment) should be(Unspecified())
  }

}