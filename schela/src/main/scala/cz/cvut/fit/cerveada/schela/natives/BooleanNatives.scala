package cz.cvut.fit.cerveada.schela.natives

import cz.cvut.fit.cerveada.schela.Bool
import cz.cvut.fit.cerveada.schela.Form
import cz.cvut.fit.cerveada.schela.UnexpectedNumberOfArguments
import cz.cvut.fit.cerveada.schela.UnexpectedType

object BooleanNatives {
  type procedureType = List[Form] => Form
  val natives = scala.collection.mutable.Map[String, procedureType]();

  natives("not") = (params: List[Form]) => params match {
    case Bool(b) :: Nil => Bool(!b)
    case a :: Nil       => throw new UnexpectedType(a, Bool(true))
    case l              => throw new UnexpectedNumberOfArguments(l.size, 1)
  }

  natives("boolean?") = (params: List[Form]) => params match {
    case Bool(b) :: Nil => Bool(true)
    case a :: Nil       => Bool(false)
    case l              => throw new UnexpectedNumberOfArguments(l.size, 1)
  }
}