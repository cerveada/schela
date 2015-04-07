package cz.cvut.fit.cerveada.schela

object Evaluator {

  def evalAll(code: List[Form], environment: Environment) = {
    code.foldLeft(code.head)((_, c) => eval(c, environment))
  }
  
  def eval(code:Form, environment:Environment):Form = code match {
    case b:Bool => b
    case b:Number => b
    case b:SString => b    
    case Quote(c) => c
    case Symbol(n) => environment.get(n)
    case SList(l) => evalList(l, environment)
  }
  
  def evalList(l:List[Form], env:Environment) = l match {
    case Symbol("define") :: rest => define(rest, env)
    //Primitive expression types
    case Symbol("set!") :: rest => set(rest, env)
    case Symbol("lambda") :: SList(params) :: exprList => lambda(params, exprList, env)      
    case Symbol("if") :: rest => ifElse(rest, env)
    //Derived expression types 
    case Symbol("and") :: rest => evalAnd(rest, env)
    case Symbol("or") :: rest => evalOr(rest, env)
    case Symbol("or") :: rest => evalOr(rest, env)
    case Symbol("let") :: rest => evalLet(rest, env)
    case a :: rest => evalApplication(a, rest, env) 
  }

  def evalApplication(fun: Form, params: List[Form], env: Environment): Form = {

    val executedParams = params.map { eval(_, env) }

    eval(fun, env) match {
      case p: Procedure => p.call(executedParams)
      case x            => throw new LispException(s"$x is not a procedure")
    }
  }

  def define(l: List[Form], env: Environment) = {
    l match {
      case Symbol(name) :: expr :: Nil => env.define(name, eval(expr, env))
      case SList(Symbol(name) :: params) :: expr => env.define(name, lambda(params, expr, env))
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

  def lambda(params: List[Form], body:List[Form], env: Environment) = {
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
      case _                                      => throw new LispException("parsing exception")
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

  def evalAnd(l: List[Form], env: Environment): Form = {
    if (l.isEmpty) return Bool(true)

    eval(l.head, env) match {
      case Bool(true)  => evalAnd(l.tail, env);
      case Bool(false) => Bool(false);
      case t           => throw new UnexpectedType(Bool(true), t);
    }
  }

  def evalOr(l: List[Form], env: Environment): Form = {
    if (l.isEmpty) return Bool(false)

    eval(l.head, env) match {
      case Bool(false)  => evalOr(l.tail, env);
      case Bool(true) => Bool(true);
      case t           => throw new UnexpectedType(Bool(true), t);
    }
  }
  
  def evalLet(l: List[Form], env: Environment): Form = {
    val (bindings, body) = l match {
      case SList(bindings) :: rest if !rest.isEmpty => (bindings, rest)
      case _                                        => throw new SyntaxException("let")
    }
    
    val newEnvironment = new LocalEnvironment(env)

    bindings.foreach { binding =>
      binding match {
        case SList(List(Symbol(n), body: Form /*body @ _**/ )) => newEnvironment.define(n, eval(body, env))
        case _ => throw new SyntaxException("let")
      }
    }
    
    evalAll(body, newEnvironment)
  }
}