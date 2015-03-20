package cz.cvut.fit.cerveada.schela

import org.scalatest._

abstract class UnitSpec extends FlatSpec with Matchers with
  OptionValues with Inside with Inspectors {

 
  def eval(code:String, environment:Environment):Value = {
    val syntacticTree = CalcParser.parseItem(code).get
    syntacticTree.evaluate(environment)
  }
  
}