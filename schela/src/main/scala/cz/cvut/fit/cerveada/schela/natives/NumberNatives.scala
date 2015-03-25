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


  natives("zero?") = numberPropertyCall((a) => a == 0)
  natives("positive?") = numberPropertyCall((a) => a > 0)
  natives("negative?") = numberPropertyCall((a) => a < 0)
  natives("odd?") = numberPropertyCall((a) => a % 2 != 0)
  natives("even?") = numberPropertyCall((a) => a % 2 == 0)
  def numberPropertyCall(fun:(Int) => Boolean)(params: List[Form]): Form = params match {
    case Number(v) :: Nil => if(fun(v)) Bool(true) else Bool(false)
    case t :: Nil         => throw new UnexpectedType(t, Number(0));
    case _                => throw new UnexpectedNumberOfArguments(params.size, 1)
  }

  
  natives("max") = maxCall
  def maxCall(params: List[Form]): Number = {
    if (params.isEmpty)
      throw new UnexpectedNumberOfArguments(params.size, 1)

    val values = params.map {
      case x: Number => x.value
      case t         => throw new UnexpectedType(t, Number(0));
    }
    Number(values.max);
  }

  
  natives("min") = minCall
  def minCall(params: List[Form]): Number = {
    if (params.isEmpty)
      throw new UnexpectedNumberOfArguments(params.size, 1)

    val values = params.map {
      case x: Number => x.value
      case t         => throw new UnexpectedType(t, Number(0));
    }
    Number(values.min);
  }
}