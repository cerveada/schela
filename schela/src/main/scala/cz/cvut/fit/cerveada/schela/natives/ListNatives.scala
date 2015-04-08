package cz.cvut.fit.cerveada.schela.natives

import cz.cvut.fit.cerveada.schela.Bool
import cz.cvut.fit.cerveada.schela.SList
import cz.cvut.fit.cerveada.schela.Form
import cz.cvut.fit.cerveada.schela.UnexpectedNumberOfArguments
import cz.cvut.fit.cerveada.schela.UnexpectedType
import cz.cvut.fit.cerveada.schela.LispException

object ListNatives {
  type procedureType = List[Form] => Form
  val natives = scala.collection.mutable.Map[String, procedureType]();

  natives("car") = (params: List[Form]) => params match {
    case SList(Nil) :: Nil       => throw new LispException("car on ()")
    case SList(head :: _) :: Nil => head
    case a :: Nil                => throw new UnexpectedType(a, SList())
    case l                       => throw new UnexpectedNumberOfArguments(l.size, 1)
  }

  natives("cdr") = (params: List[Form]) => params match {
    case SList(Nil) :: Nil       => throw new LispException("car on ()")
    case SList(_ :: rest) :: Nil => SList(rest)
    case a :: Nil                => throw new UnexpectedType(a, SList())
    case l                       => throw new UnexpectedNumberOfArguments(l.size, 1)
  }
}