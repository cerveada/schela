package cz.cvut.fit.cerveada.schela

object Evaluator {
  
  def eval(code:Form, environment:Environment):Value = code match {
    case b:Bool => b
    case b:Number => b
    case b:SString => b    
    case Quote(c) => c
    case Symbol(n) => environment.get(n)
    case SList(l) => evalList(l, environment)
  }
  
  def evalList(l:List[Form], env:Environment) = l match {
    case Symbol("define") :: rest => define(rest, env)
    case Symbol("set!") :: rest => set(rest, env)
    case Symbol("lambda") :: SList(params) :: expr :: Nil => lambda(params, expr, env)      
    case Symbol("if") :: rest => ifElse(rest, env)
    case Symbol(a) :: rest => evalApplication(a, rest, env) 
  }

  def evalApplication(name: String, params: List[Form], environment: Environment): Value = {
    println("calling fubnction " + name);

    val executedParams = params.map { eval(_, environment) }

    environment.get(name) match {
      case p: Procedure =>
        p.evaluate(environment); p.call(executedParams)
      case _            => throw new LispException(s"$name is not a function")
    }
  }

  def define(l: List[Form], env: Environment) = {
    l match {
      case Symbol(name) :: expr :: Nil => env.define(name, eval(expr, env))
      case SList(Symbol(name) :: params) :: expr :: Nil => env.define(name, lambda(params, expr, env))
      case _                           => throw new LispException("parsing exception")
    }
    Unspecified()
  }

  def set(l: List[Form], env: Environment) = {
    l match {
      case Symbol(name) :: expr :: Nil => env.set(name, eval(expr, env))
      case _                           => throw new LispException("parsing exception")
    }
    Unspecified()
  }

  def lambda(params: List[Form], body:Form, env: Environment) = {
    /*
    val (params, body) = l match {
      case SList(params) :: expr :: Nil => (params, expr)
      case _                            => throw new LispException("parsing exception")

    }*/
    val paramNames = params.map { x =>
      x match {
        case Symbol(x) => x
        case _         => throw new LispException("parsing exception")
      }
    }
    SProcedure(paramNames, body, env)
  }

  def ifElse(l: List[Form], env: Environment) = {
    val (predicate, thanExp, elseExp) = l match {
      case predicate :: thanExp :: elseExp :: Nil => (predicate, thanExp, Some(elseExp))
      case predicate :: thanExp :: Nil            => (predicate, thanExp, None)
      case _                                    => throw new LispException("parsing exception")
    }
    eval(predicate, env) match {
      case Bool(true)  => eval(thanExp, env)
      case Bool(false) => evaluateElse(elseExp, env)
      case _           => throw new LispException("predicate must return boolean")
    }
  }

  def evaluateElse(elseExp: Option[Form], environment: Environment) = elseExp match {
    case Some(e) => eval(e,environment)
    case None    => Unspecified()
  }
}