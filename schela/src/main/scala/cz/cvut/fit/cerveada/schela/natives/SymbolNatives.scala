package cz.cvut.fit.cerveada.schela.natives

import cz.cvut.fit.cerveada.schela.Bool
import cz.cvut.fit.cerveada.schela.Symbol
import cz.cvut.fit.cerveada.schela.SString
import cz.cvut.fit.cerveada.schela.Form
import cz.cvut.fit.cerveada.schela.UnexpectedNumberOfArguments
import cz.cvut.fit.cerveada.schela.UnexpectedType

object SymbolNatives {
  type procedureType = List[Form] => Form
  val natives = scala.collection.mutable.Map[String, procedureType]();

  natives("symbol?") = (params: List[Form]) => params match {
    case Symbol(_) :: Nil => Bool(true)
    case t :: Nil         => Bool(false)
    case _                => throw new UnexpectedNumberOfArguments(params.size, 1)
  }

  natives("symbol->string") = (params: List[Form]) => params match {
    case Symbol(n) :: Nil => SString(n)
    case t :: Nil         => throw new UnexpectedType(Symbol(""), t);
    case _                => throw new UnexpectedNumberOfArguments(params.size, 1)
  }
  
  //TODO string->symbol
}