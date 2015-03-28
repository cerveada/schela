package cz.cvut.fit.cerveada.schela.natives

import cz.cvut.fit.cerveada.schela._

object NumberNatives {
  type procedureType = List[Form] => Form
  val natives = scala.collection.mutable.Map[String, procedureType]();

  natives("number?") = isNumberCall
  natives("complex?") = isNumberCall
  natives("real?") = isNumberCall
  natives("rational?") = isNumberCall
  natives("integer?") = isNumberCall
  def isNumberCall(params: List[Form]): Form = params match {
    case Number(_) :: Nil => Bool(true)
    case t :: Nil         => Bool(false)
    case _                => throw new UnexpectedNumberOfArguments(params.size, 1)
  }

  natives("exact?") = numberPropertyCall(_ => true)
  natives("inexact?") = numberPropertyCall(_ => false)

  natives("=") = comparatorCall((a, b) => a == b)
  natives("<") = comparatorCall((a, b) => a < b)
  natives(">") = comparatorCall((a, b) => a > b)
  natives("<=") = comparatorCall((a, b) => a <= b)
  natives(">=") = comparatorCall((a, b) => a >= b)
  def comparatorCall(fun: (Int, Int) => Boolean)(params: List[Form]): Bool = {
    if (params.size < 2)
      throw new UnexpectedNumberOfArguments(params.size, 2)

    val values = params.map {
      case Number(v) => v
      case t         => throw new UnexpectedType(t, Number(0));
    }
    val result = values.sliding(2).forall {
      case a :: b :: Nil => fun(a, b)
    }
    Bool(result)
  }

  natives("zero?") = numberPropertyCall((a) => a == 0)
  natives("positive?") = numberPropertyCall((a) => a > 0)
  natives("negative?") = numberPropertyCall((a) => a < 0)
  natives("odd?") = numberPropertyCall((a) => a % 2 != 0)
  natives("even?") = numberPropertyCall((a) => a % 2 == 0)
  def numberPropertyCall(fun: (Int) => Boolean)(params: List[Form]): Form = params match {
    case Number(v) :: Nil => if (fun(v)) Bool(true) else Bool(false)
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

  natives("abs") = absCall
  def absCall(params: List[Form]): Number = params match {
    case Number(v) :: Nil => Number(scala.math.abs(v))
    case t :: Nil         => throw new UnexpectedType(t, Number(0));
    case _                => throw new UnexpectedNumberOfArguments(params.size, 1)
  }

  natives("quotient") = integerDivisionCall((n1, n2) => n1 / n2)
  natives("remainder") = integerDivisionCall((n1, n2) => n1 % n2)
  natives("modulo") = integerDivisionCall((n1, n2) => (n1 % n2 + n2) % n2)
  def integerDivisionCall(fun: (Int, Int) => Int)(params: List[Form]): Number =
    params match {
      case Number(n1) :: Number(n2) :: Nil => Number(fun(n1, n2))
      case Number(_) :: t :: Nil           => throw new UnexpectedType(t, Number(0));
      case t :: Number(_) :: Nil           => throw new UnexpectedType(t, Number(0));
      case t :: t2 :: Nil                  => throw new UnexpectedType(t, Number(0));
      case _                               => throw new UnexpectedNumberOfArguments(params.size, 1)
    }

  natives("gcd") = reduceCall(gcd)
  natives("lcm") = reduceCall(lcm)
  def reduceCall(fun: (Int, Int) => Int)(params: List[Form]): Form = params match {
    case Nil      => throw new UnexpectedNumberOfArguments(0, 2)
    case t :: Nil => throw new UnexpectedNumberOfArguments(1, 2)
    case l        => reduceListCall(fun)(l)
  }
  def reduceListCall(fun: (Int, Int) => Int)(params: List[Form]) = {
    val values = params.map {
      case x: Number => x.value
      case v         => throw new UnexpectedType(v, Number(0))
    }
    Number(values.reduce(fun(_, _)));
  }
  def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)
  def lcm(a: Int, b: Int) = (a * b).abs / gcd(a, b)
  
}