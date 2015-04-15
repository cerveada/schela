package cz.cvut.fit.cerveada.schela

import cz.cvut.fit.cerveada.schela.natives.EquivalenceNatives.eqv

object Evaluator {

  def evalAll(code: List[Form], environment: Environment) = {
    code.foldLeft(code.head)((_, c) => eval(c, environment))
  }
  
  def eval(code:Form, environment:Environment):Form = code match {
    case b:Bool => b
    case b:Number => b
    case b:SString => b    
    case Quote(c) => c
    case SSymbol(n) => environment.get(n)
    case SList(l) => evalList(l, environment)
  }

  def evalList(l: List[Form], env: Environment) = l match {
    case SSymbol('define) :: rest => define(rest, env)
    //Primitive expression types
    case SSymbol(Symbol("set!")) :: rest => set(rest, env)
    case SSymbol('lambda) :: SList(params) :: exprList => lambda(params, exprList, env)
    case SSymbol('if) :: rest => ifElse(rest, env)
    //Derived expression types 
    case SSymbol('and) :: rest => evalAnd(rest, env)
    case SSymbol('or) :: rest => evalOr(rest, env)
    case SSymbol('let) :: rest => evalLet(rest, env)
    case SSymbol(Symbol("let*")) :: rest => evalLetStar(rest, env)
    case SSymbol('letrec) :: rest => evalLetStar(rest, env)
    case SSymbol('begin) :: rest => evalBegin(rest, env)
    case SSymbol('cond) :: rest => evalCond(rest, env)
    case SSymbol('case) :: rest => evalCase(rest, env)
    case a :: rest => evalApplication(a, rest, env)
    case _ => throw new SyntaxException("expression")
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
      case SSymbol(name) :: expr :: Nil => env.define(name, eval(expr, env))
      case SList(SSymbol(name) :: params) :: expr => env.define(name, lambda(params, expr, env))
      case _                           => throw new LispException("parsing exception")
    }
    Unspecified()
  }

  def set(l: List[Form], env: Environment) = {
    l match {
      case SSymbol(name) :: expr :: Nil => env.set(name, eval(expr, env))
      case _                           => throw new LispException("parsing exception")
    }
    Unspecified()
  }

  def lambda(params: List[Form], body:List[Form], env: Environment) = {
    val paramNames = params.map { x =>
      x match {
        case SSymbol(x) => x
        case _         => throw new SyntaxException("lambda")
      }
    }
    if (body.isEmpty) throw new SyntaxException("lambda")
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
        case SList(List(SSymbol(n), expr: Form)) => newEnvironment.define(n, eval(expr, env))
        case _ => throw new SyntaxException("let")
      }
    }
    
    evalAll(body, newEnvironment)
  }

  def evalLetStar(l: List[Form], env: Environment): Form = {
    val (bindings, body) = l match {
      case SList(bindings) :: rest if !rest.isEmpty => (bindings, rest)
      case _                                        => throw new SyntaxException("let")
    }

    val newEnvironment = new LocalEnvironment(env)

    bindings.foreach { binding =>
      binding match {
        case SList(List(SSymbol(n), expr: Form)) => newEnvironment.define(n, eval(expr, newEnvironment))
        case _                                  => throw new SyntaxException("let")
      }
    }

    evalAll(body, newEnvironment)
  }

  def evalBegin(body: List[Form], env: Environment): Form = {
    evalAll(body, env);
  }

  def evalCond(body: List[Form], env: Environment): Form = body match {
    case Nil => Unspecified()
    case SList(SSymbol('else) :: rest) :: Nil => evalAll(rest, env)
    case SList(test :: rest) :: _ if evalsToTrue(test, env) => evalAll(rest, env)
    case SList(_) :: tail => evalCond(tail, env)
    case _ => throw new SyntaxException("cond")
  }

  def evalCase(body: List[Form], env: Environment): Form = {
    val (key, rest) = body match {
      case (expr:Form) :: rest => (eval(expr, env), rest)
      case _ => throw new SyntaxException("case")
    }

    rest.foreach {
      case SList(SList(l) :: clauseRest) if containsKey(l, key) => return evalAll(clauseRest, env)
      case SList(SList(_) :: _) => Nil
      case SList(SSymbol('else) :: clauseRest) => return evalAll(clauseRest, env)
      case _ => throw new SyntaxException("case")
    }
        
    Unspecified()
  }

  def containsKey(list:List[Form], key:Form):Boolean = {
    list.exists { x => eqv(x :: key :: Nil).b }
  }
  
  def evalsToTrue(expr: Form, env: Environment): Boolean = {
    eval(expr, env) match {
      case Bool(b) => b
      case _       => throw new LispException("predicate must return boolean")
    }
  }

}