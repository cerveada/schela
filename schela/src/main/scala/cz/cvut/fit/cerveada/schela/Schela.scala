package cz.cvut.fit.cerveada.schela

object Schela {
  def main(args: Array[String]) {
    println("Welcome to Schela! Scheme interpreter in Scala")
    repl(new TopEnvironment)
  }

  def repl(environment: Environment) = while (true) {
    val input = scala.io.StdIn.readLine()
    val tree = parse(input)
    evaluate(tree, environment)
  }

  def parse(s: String) = {
    val result = TokenParser.parseItem(s)
    result match {
      case TokenParser.Success(result, _) => println(result.toString)
      case TokenParser.Failure(msg, _)    => throw new Exception("parse failed: " + msg)
      case TokenParser.Error(msg, _)      => throw new Exception("parse error: " + msg)
    }
    result.get
  }

  def evaluate(expr: Form, environment: Environment) = {
    try {
      val result = Evaluator.eval(expr, environment)
      println("> " + result)
    } catch {
      case e: LispException => println(e.message)
    }
  }
}