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
    val result = TokenParser.parseItem(s)
    result match {
      case TokenParser.Success(result, _) => println(result.toString)
      case TokenParser.Failure(msg, _)    => throw new LispException("parsing failed: " + msg)
      case TokenParser.Error(msg, _)      => throw new LispException("parsing error: " + msg)
    }
    result.get
  }
}