package cz.cvut.fit.cerveada.schela

import org.scalatest._

abstract class UnitSpec extends FlatSpec with Matchers with
  OptionValues with Inside with Inspectors {

 
  def eval(code:String, environment:Environment):Value = {
    val syntacticTree = TokenParser.parseItem(code).get
    Evaluator.eval(syntacticTree, environment)
  }
  
}