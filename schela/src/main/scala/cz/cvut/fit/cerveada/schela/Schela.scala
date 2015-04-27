package cz.cvut.fit.cerveada.schela

object Schela {
  def main(args: Array[String]) {
    println("Welcome to Schela! Scheme interpreter in Scala")
    repl(new TopEnvironment)
  }

  def repl(environment: Environment) = while (true) {
    try {
      val input = scala.io.StdIn.readLine()
      val tree = parse(input)
      val result = Evaluator.eval(tree, environment)
      println("> " + result)
    } catch {
      case e: LispException => println(e.message)
    }
  }

  def parse(s: String) = {
    val result = Parser.parseItem(s)
    result match {
      case Parser.Success(result, _) => println(result.toString)
      case Parser.Failure(msg, _)    => throw new LispException("parsing failed: " + msg)
      case Parser.Error(msg, _)      => throw new LispException("parsing error: " + msg)
    }
    result.get
  }
}