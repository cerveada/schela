package cz.cvut.fit.cerveada.schela.natives

import cz.cvut.fit.cerveada.schela._
import cz.cvut.fit.cerveada.schela.Number
import cz.cvut.fit.cerveada.schela.SVector
import cz.cvut.fit.cerveada.schela.Procedure
import cz.cvut.fit.cerveada.schela.SString
import cz.cvut.fit.cerveada.schela.UnexpectedNumberOfArguments
import cz.cvut.fit.cerveada.schela.UnexpectedType
import scala.collection.mutable.ArraySeq


object VectorNatives {
  type procedureType = List[Form] => Form
  val natives = scala.collection.mutable.Map[String, procedureType]();

  natives("vector?") = (params: List[Form]) => {
    params match {
      case (t: SVector) :: Nil => Bool(true)
      case t :: Nil            => Bool(false)
      case _                   => throw new UnexpectedNumberOfArguments(params.size, 1)
    }
  }

  natives("vector-length") = (params: List[Form]) => params match {
    case SVector(a) :: Nil => Number(a.length)
    //case t :: Nil          => throw new UnexpectedType(t, SVector(ArraySeq()))
    case _                 => throw new UnexpectedNumberOfArguments(params.size, 1)
  }
  
  natives("vector") = (params: List[Form]) => SVector(ArraySeq(params:_*))

  natives("make-vector") = (params: List[Form]) => params match {
    case Number(n) :: (f: Form) :: Nil => SVector(ArraySeq.fill(n)(f))
    case Number(n) :: Nil              => SVector(ArraySeq.fill(n)(Unspecified()))
    case t :: Nil                      => throw new UnexpectedType(t, Number(0))
    case t :: _ :: Nil                 => throw new UnexpectedType(t, Number(0))
    case _                             => throw new UnexpectedNumberOfArguments(params.size, 1)
  }

 
}