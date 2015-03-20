package cz.cvut.fit.cerveada.schela

case class Sum() extends Procedure {

  def call(params: List[Value]):Value = {
    if (params.isEmpty)
      return Number(0);
    
    val values = params.map { x =>
      x match {
        case x: Number => x.value
        case _         => throw new ClassCastException
      }
    }

    Number(values.reduce(_ + _));
  }
}

case class Mul() extends Procedure {
 
  def call(params:List[Value]):Value = {
    if (params.isEmpty)
      return Number(1);
    
    val values = params.map { x => x match {
      case x: Number => x.value
      case _         => throw new ClassCastException
    }}
    
    Number(values.reduce(_*_));
  }
}


case class Subtraction() extends Procedure {

  def call(params: List[Value]): Value = params match {
    case Nil      => throw new UnexpectedNumberOfArguments(0, 1)
    case h :: Nil => negate(h)
    case l        => listCall(l)
  }

  def listCall(params: List[Value]) = {
    val values = params.map { x =>
      x match {
        case x: Number => x.value
        case v         => throw new UnexpectedType(v, Number(0))
      }
    }
    Number(values.reduce(_ - _));
  }

  def negate(v: Value) = v match {
    case v: Number => Number(-v.value)
    case v         => throw new UnexpectedType(v, Number(0))
  }
}

/**
 * works only for integers
 */
case class Division() extends Procedure {
  def call(params: List[Value]): Value = params match {
    case Nil      => throw new UnexpectedNumberOfArguments(0, 1)
    case h :: Nil => fraction(h)
    case l        => listCall(l)
  }

  def listCall(params: List[Value]) = {
    val values = params.map { x =>
      x match {
        case x: Number => x.value
        case v         => throw new UnexpectedType(v, Number(0))
      }
    }
    Number(values.reduce(_ / _));
  }

  def fraction(v: Value) = v match {
    case v: Number => Number(0)
    case v         => throw new UnexpectedType(v, Number(0))
  }
}

case class Eq() extends Procedure {

  def call(params: List[Value]) = {
    params match {
      case l :: r :: Nil => Bool(l eq r)
      case _             => throw new UnexpectedNumberOfArguments(params.size, 2)
    }
  }
}

case class Eql() extends Procedure {

  def call(params: List[Value]) = {
    params match {
      case l :: r :: Nil => Bool(l == r)
      case _             => throw new UnexpectedNumberOfArguments(params.size, 2)
    }
  }
}

case class Display() extends Procedure {

  def call(params: List[Value]) = {
    params match {
      case SString(v) :: Nil => print(v) 
      case _                 => throw new UnexpectedNumberOfArguments(params.size, 1)
    }
    Unspecified()
  }
}