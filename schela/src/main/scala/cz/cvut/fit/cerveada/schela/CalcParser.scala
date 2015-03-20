package cz.cvut.fit.cerveada.schela

import scala.util.parsing.combinator._
import scala.util.matching.Regex
/*
object CalcParser extends TokenParser {
  override def skipWhitespace = true
  
  def parseItem(str: String) = parse(expr, str) 
  
    
  private def expr:Parser[Form] = (definition | expression)

  //Definitions
  private def definition = "(" ~> (variableDef | functionDef) <~ ")"
  
  private def variableDef = "define" ~> identifier ~ expr ^^ { case n ~ b  => VarDef(n, b) }
  
  private def functionDef:Parser[VarDef] = "define" ~> funcParams ~ expr ^^ 
    { case n ~ p ~ b  => VarDef(n, Lambda(p, b)) }
  private def funcParams = "(" ~> identifier ~ rep(identifier) <~ ")" 
  
  private def variable = symbol
  
  //Expressions
  private def expression = (constant | variable | expParns)
  private def expParns = "(" ~> (lambda | ifExp | set | functionCall) <~ ")"
  private def constant = ( boolean | number | string)
  
  
  private def lambda = "lambda" ~> lambdaParams ~ expr ^^ { case p ~ b  => Lambda(p, b) }
  private def lambdaParams = "(" ~> rep(identifier) <~ ")" 
  
  private def ifExp = "if" ~> (ifThanElse | ifThan)
  private def ifThan:Parser[IfElse] = expression ~ expression ^^ { case p ~ t => IfElse(p,t,None) }
  private def ifThanElse:Parser[IfElse] = expression ~ expression ~ expression ^^ 
    { case p ~ t ~ e => IfElse(p,t,Some(e)) }
  
  private def set = "set!" ~> identifier ~ expr ^^ {case v ~ e => Set(v, e) }
  
  private def functionCall = identifier ~ callParams ^^ { case n ~ p  =>  FunCall(n,p) }
  private def callParams:Parser[List[Form]] = rep(expr) ^^ (List() ++ _)
  
}
*/