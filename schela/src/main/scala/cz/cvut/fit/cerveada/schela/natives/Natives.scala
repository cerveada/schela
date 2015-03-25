package cz.cvut.fit.cerveada.schela.natives

import cz.cvut.fit.cerveada.schela._
import cz.cvut.fit.cerveada.schela.Number
import cz.cvut.fit.cerveada.schela.Procedure
import cz.cvut.fit.cerveada.schela.SString
import cz.cvut.fit.cerveada.schela.UnexpectedNumberOfArguments
import cz.cvut.fit.cerveada.schela.UnexpectedType


case class Eq() extends Procedure {

  def call(params: List[Form]) = {
    params match {
      case l :: r :: Nil => Bool(l eq r)
      case _             => throw new UnexpectedNumberOfArguments(params.size, 2)
    }
  }
}

case class Eql() extends Procedure {

  def call(params: List[Form]) = {
    params match {
      case l :: r :: Nil => Bool(l == r)
      case _             => throw new UnexpectedNumberOfArguments(params.size, 2)
    }
  }
}

case class Display() extends Procedure {

  def call(params: List[Form]) = {
    params match {
      case SString(v) :: Nil => print(v) 
      case _                 => throw new UnexpectedNumberOfArguments(params.size, 1)
    }
    Unspecified()
  }
}