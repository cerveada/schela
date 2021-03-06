package cz.cvut.fit.cerveada.schela

import scala.util.parsing.combinator._
import scala.util.matching.Regex
import scala.collection.mutable.ArraySeq
import scala.collection.mutable.ArrayBuffer


object Parser extends JavaTokenParsers  {
  override def skipWhitespace = true
  
  def parseItem(str: String) = parse(code, str) 
  
  def code:Parser[Form] = (datum | quote | list | hash)
  def datum = (number | string | symbol)
  def hash = "#" ~> (vector | char | boolean)

  
  def number:Parser[Number] = regex(new Regex("-?[0-9]+")) ^^ (s => Number(s.toInt))
  
  def boolean = ("t" | "f") ^^ { case "t" => Bool(true); case "f" => Bool(false) }
  
  def string = "\"" ~> """([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""".r <~ "\"" ^^ 
    { case s => SString(s) }
  
  def symbol = identifier ^^ {case v => SSymbol(Symbol(v.toLowerCase()))}
  
  def identifier:Parser[String] = (identInitial | "+" | "-")
  def identInitial = """[A-Za-z!$%&*/:<=>?~_^][A-Za-z!$%&*/:<=>?~_^0-9.+-]*""".r
  
  def char = """\""" ~> """[^\s]+""".r ^^ {case v => SChar.fromString(v)}
  
  def list = "(" ~> listContent <~ ")" ^^ {case v => SList(v)}
  def listContent:Parser[List[Form]] = rep(code)  ^^ (List() ++ _)
  
  def quote = (quoteChar | quoteWord)
  def quoteChar:Parser[Form] = "'" ~> code ^^ (s => Quote(s)) 
  def quoteWord:Parser[Form] = "(" ~> "quote" ~> code <~ ")" ^^ (s => Quote(s)) 
  
  def vector = "(" ~> vectorContent <~ ")" ^^ {case v => (SVector.constant(v))}
  def vectorContent:Parser[ArraySeq[Form]] = rep(code)  ^^ (ArraySeq() ++ _)

}