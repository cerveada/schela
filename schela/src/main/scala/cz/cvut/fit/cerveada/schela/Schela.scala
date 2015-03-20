package cz.cvut.fit.cerveada.schela

/**
 * Hello world!
 *
 */
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
    val result = CalcParser.parseItem(s)
    result match {
      case CalcParser.Success(result, _) => println(result.toString)
      case CalcParser.Failure(msg, _)    => throw new Exception("parse failed: " + msg)
      case CalcParser.Error(msg, _)      => throw new Exception("parse error: " + msg)
    }
    result.get
  }

  def evaluate(expr: Form, environment: Environment) = {
    try {
      val result = expr.evaluate(environment)
      println(result)
    } catch {
      case e: LispException => println(e.message)
    }
  }
}