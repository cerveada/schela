package cz.cvut.fit.cerveada.schela

import scala.util.parsing.combinator._
import scala.util.matching.Regex

object TokenParser extends JavaTokenParsers  {
  override def skipWhitespace = true
  
  def parseItem(str: String) = parse(code, str) 
  
  def code:Parser[Form] = (datum | quote | list | vector)
  
  //Data
  def datum = (number | boolean | string | symbol)
  def number:Parser[Number] = regex(new Regex("-?[0-9]+")) ^^ (s => Number(s.toInt))
  def boolean = ("#t" | "#f") ^^ { case "#t" => Bool(true); case "#f" => Bool(false) }
  //private def string = /*"\"" ~>*/ stringLiteral/* <~ "\""*/ ^^ { case s => SString(s) }
  def string = "\"" ~> """([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""".r <~ "\"" ^^ 
    { case s => SString(s) }
  def symbol = identifier ^^ {case v => Symbol(v.toLowerCase())}
  def identifier:Parser[String] = (identInitial | "+" | "-")
  def identInitial = """[A-Za-z!$%&*/:<=>?~_^][A-Za-z!$%&*/:<=>?~_^0-9.+-]*""".r
  
  // | ! | $ | % | & | * | / | : | < | = | > | ? | ~ | _ | ^
  
  def list = "(" ~> listContent <~ ")" ^^ {case v => SList(v)}
  def listContent:Parser[List[Form]] = rep(code)  ^^ (List() ++ _)
  
  def quote = (quoteChar | quoteWord)
  def quoteChar:Parser[Form] = "'" ~> code ^^ (s => Quote(s)) 
  def quoteWord:Parser[Form] = "(" ~> "quote" ~> code <~ ")" ^^ (s => Quote(s)) 
  
  def vector = "#(" ~> listContent <~ ")" ^^ {case v => Vector(v)}
}