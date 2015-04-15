package cz.cvut.fit.cerveada.schela.natives

import cz.cvut.fit.cerveada.schela._
import cz.cvut.fit.cerveada.schela.Number
import cz.cvut.fit.cerveada.schela.SVector
import cz.cvut.fit.cerveada.schela.Procedure
import cz.cvut.fit.cerveada.schela.SString
import cz.cvut.fit.cerveada.schela.UnexpectedNumberOfArguments
import cz.cvut.fit.cerveada.schela.UnexpectedType

object EquivalenceNatives {
  type procedureType = List[Form] => Form
  val natives = scala.collection.mutable.Map[String, procedureType]();
  
  natives("eq?") = (params: List[Form]) => {
    val pair = params match {
      case l :: r :: Nil => (l,r)
      case _             => throw new UnexpectedNumberOfArguments(params.size, 2)
    }
    
    schemeEq(pair);
  }

  def schemeEq(pair: (Form, Form)):Bool = pair match {
    case (SSymbol(l), SSymbol(r)) => Bool(l eq r)
    case (SList(Nil), SList(Nil)) => Bool(true)
    case (l, r)                   => Bool(l eq r)
  }
  
  natives("eqv?") = eqv
  def eqv(params: List[Form]) = {
    val pair = params match {
      case l :: r :: Nil => (l, r)
      case _             => throw new UnexpectedNumberOfArguments(params.size, 2)
    }
    
    pair match {
      case(SList(_), SList(_)) => schemeEq(pair)
      case(SVector(_), SVector(_)) => schemeEq(pair)
      case(SString(_), SString(_)) => schemeEq(pair)
      case(l,r) => Bool(l == r)
    }
  }
  
  natives("equal?") = (params: List[Form]) => params match {
	  case l :: r :: Nil => Bool(l == r)
	  case _             => throw new UnexpectedNumberOfArguments(params.size, 2)
  }
}