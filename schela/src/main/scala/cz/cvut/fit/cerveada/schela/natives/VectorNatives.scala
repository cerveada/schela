package cz.cvut.fit.cerveada.schela.natives

import cz.cvut.fit.cerveada.schela._
import cz.cvut.fit.cerveada.schela.Number
import cz.cvut.fit.cerveada.schela.SVector
import cz.cvut.fit.cerveada.schela.Procedure
import cz.cvut.fit.cerveada.schela.SString
import cz.cvut.fit.cerveada.schela.UnexpectedNumberOfArguments
import cz.cvut.fit.cerveada.schela.UnexpectedType
import cz.cvut.fit.cerveada.schela.ImmutableModification
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
    case t :: Nil          => throw new UnexpectedType(t, SVector(ArraySeq()))
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
  
  natives("is-mutable?") = (params: List[Form]) => params match {
      case (v:SVector) :: Nil  => Bool(v.mutable)
      case t :: Nil            => throw new UnexpectedType(t, SVector(ArraySeq()))
      case _                   => throw new UnexpectedNumberOfArguments(params.size, 1)
  }
  
  natives("vector-ref") = (params: List[Form]) => params match {
    case SVector(a) :: Number(n) :: Nil => a(n)
    case t :: Number(_) :: Nil          => throw new UnexpectedType(t, Number(0))
    case SVector(_) :: t :: Nil         => throw new UnexpectedType(t, SVector(ArraySeq()))
    case _                              => throw new UnexpectedNumberOfArguments(params.size, 1)
  }
  
  natives("vector-set!") = (params: List[Form]) => params match {
    case (v:SVector) :: _ if !v.mutable => throw new ImmutableModification("vector")
    case SVector(a) :: Number(n) :: (f: Form) :: Nil => a(n) = f; Unspecified()
    case t :: _ :: _ :: Nil => throw new UnexpectedType(t, SVector(ArraySeq()))
    case _ => throw new UnexpectedNumberOfArguments(params.size, 1)
  }

 
}