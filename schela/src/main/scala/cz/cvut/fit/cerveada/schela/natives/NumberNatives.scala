package cz.cvut.fit.cerveada.schela.natives

import cz.cvut.fit.cerveada.schela._

object NumberNatives {
  type procedureType = List[Form] => Form
  val natives = scala.collection.mutable.Map[String, procedureType]();

  
  natives("+") = plusCall
  def plusCall(params: List[Form]): Number = {
    if (params.isEmpty)
      return Number(0);

    val values = params.map {
      case x: Number => x.value
      case t         => throw new UnexpectedType(t, Number(0));
    }
    Number(values.reduce(_ + _));
  }

  
  natives("*") = mulCall
  def mulCall(params: List[Form]): Number = {
    if (params.isEmpty)
      return Number(1);

    val values = params.map {
      case x: Number => x.value
      case t         => throw new UnexpectedType(t, Number(0));
    }
    Number(values.reduce(_ * _));
  }

  
  natives("-") = subCall
  def subCall(params: List[Form]): Form = params match {
    case Nil               => throw new UnexpectedNumberOfArguments(0, 1)
    case Number(v) :: Nil  => Number(-v)
    case Number(v) :: tail => Number(v - plusCall(tail).value)
    case t :: _            => throw new UnexpectedType(t, Number(0));
  }

  
  natives("/") = divCall
  def divCall(params: List[Form]): Form = params match {
    case Nil              => throw new UnexpectedNumberOfArguments(0, 1)
    case Number(v) :: Nil => Number(0)
    case l                => divListCall(l)
  }

  def divListCall(params: List[Form]) = {
    val values = params.map {
      case x: Number => x.value
      case v         => throw new UnexpectedType(v, Number(0))
    }
    Number(values.reduce(_ / _));
  }

  natives("=") = comparatorCall((a,b) => a == b) 
  natives("<") = comparatorCall((a,b) => a < b) 
	natives(">") = comparatorCall((a,b) => a > b) 
	natives("<=") = comparatorCall((a,b) => a <= b) 
	natives(">=") = comparatorCall((a,b) => a >= b) 
  def comparatorCall(fun:(Int,Int) => Boolean)(params: List[Form]): Bool = {
    if (params.size < 2)
      throw new UnexpectedNumberOfArguments(params.size, 2)

    val values = params.map {
      case Number(v) => v
      case t         => throw new UnexpectedType(t, Number(0));
    }
    val result = values.sliding(2).forall {
      case a :: b :: Nil => fun(a,b)
    }
    Bool(result)
  }


}