package cz.cvut.fit.cerveada.schela.natives

import cz.cvut.fit.cerveada.schela.Bool
import cz.cvut.fit.cerveada.schela.Number
import cz.cvut.fit.cerveada.schela.SVector
import cz.cvut.fit.cerveada.schela.SSymbol
import cz.cvut.fit.cerveada.schela.Form
import cz.cvut.fit.cerveada.schela.TopEnvironment
import cz.cvut.fit.cerveada.schela.UnexpectedNumberOfArguments
import cz.cvut.fit.cerveada.schela.UnexpectedNumberOfArguments
import cz.cvut.fit.cerveada.schela.UnexpectedType
import cz.cvut.fit.cerveada.schela.UnitSpec
import scala.collection.mutable.ArraySeq


class VectorSpec extends UnitSpec{

  "A vector? " should "return #t if obj is a vector, otherwise should return #f. " in {

    val env = new TopEnvironment()
    
     an[UnexpectedNumberOfArguments] should be thrownBy eval("(vector? #(5 6) #(14 56))", env)
     an[UnexpectedNumberOfArguments] should be thrownBy eval("(vector?)", env)
    
     eval("(vector? #(1 2 3))", env) should be(Bool(true))
     eval("(vector? 'ahoj)", env) should be(Bool(false))
  }
  
  "A vector-length " should "return the number of elements in vector." in {
	  
	  val env = new TopEnvironment()
	  
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(vector-length #(5 6) #(14 56))", env)
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(vector-length)", env)
	  
	  eval("(vector-length #(1 2 3))", env) should be(Number(3))
	  eval("(vector-length #(1 2 3 5 6))", env) should be(Number(5))
  }
  
  "A make-vector" should "return a newly allocated vector of k elements." in {
	  
	  val env = new TopEnvironment()
	  
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(make-vector 3 4 5)", env)
	  an[UnexpectedNumberOfArguments] should be thrownBy eval("(make-vector)", env)
	  
	  eval("(make-vector 3 #t)", env) should be(SVector(ArraySeq(Bool(true), Bool(true), Bool(true))))
	  eval("(vector-length (make-vector 5))", env) should be(Number(5))
  }
  
  "A vector" should "return a newly allocated vector whose elements contain the given arguments." in {
	  
	  val env = new TopEnvironment()
	  
	  eval("(vector 'a 'b 'c)", env) should be(SVector(ArraySeq(SSymbol('a), SSymbol('b), SSymbol('c))))
	  eval("(vector 42)", env) should be(SVector(ArraySeq(Number(42))))
  }
  
  

}