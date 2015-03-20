package cz.cvut.fit.cerveada.schela

import scala.util.parsing.combinator._
import scala.util.matching.Regex

class TokenParser extends JavaTokenParsers  {
  override def skipWhitespace = true
  
  //Data
  def datum = (number | boolean | string | symbol)
  def number:Parser[Number] = regex(new Regex("[0-9]+")) ^^ (s => Number(s.toInt))
  def boolean = ("#t" | "#f") ^^ { case "#t" => Bool(true); case "#f" => Bool(false) }
  //private def string = /*"\"" ~>*/ stringLiteral/* <~ "\""*/ ^^ { case s => SString(s) }
  def string = "\"" ~> """([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""".r <~ "\"" ^^ 
    { case s => SString(s) }
  def symbol = identifier ^^ {case v => Symbol(v)}
  def identifier:Parser[String] =  """[A-Za-z+*-/][A-Za-z0-9+*?!_]*""".r
  
}