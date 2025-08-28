package sutd.compiler

import sutd.compiler.LambdaCalculus.*
import sutd.compiler.Monad.*
import sutd.compiler.StateT.*


object BigStepEval {
    import Term.*
    import Value.*


    // A substitution 
    type Subst = (Var,Term) 

    type Err = String

    case class StateInfo(nextNum:Int)

    enum Result[+A] {
        case Error(msg:String) extends Result[A]
        case Ok[A](result:A) extends Result[A]
    }


    given resultMonadError: MonadError[Result, Err] = 
        new MonadError[Result, String] {
            override def bind[A, B](fa: Result[A])(f: A => Result[B]): Result[B] = fa match {
                case Result.Ok(a)    => f(a)
                case Result.Error(s) => Result.Error(s)
            }

            override def pure[A](x: A): Result[A] = Result.Ok(x)
            override def raiseError[A](e:String):Result[A] = Result.Error(e)
            override def handleErrorWith[A](fa: Result[A])(f:Err => Result[A]) : Result[A] = fa match {
                case Result.Error(s) => f(s)
                case Result.Ok(a)    => Result.Ok(a)
            }
        }

    
    trait StateResultMonadError[S] extends StateTMonadError[S, Result, Err] { 
        override def M0 = resultMonadError
        override def raiseError[A](e:Err):StateT[S,Result,A] = {
            StateT(st => Result.Error(e))
        }
        override def handleErrorWith[A](fa:StateT[S,Result,A])(f:Err => StateT[S,Result,A]): StateT[S,Result,A] = {
            StateT(st => fa.run(st) match {
                case Result.Error(s) => f(s).run(st)
                case Result.Ok(a)    => Result.Ok(a)
            })
        }
    }

    given stateResultMonadError[S]:StateResultMonadError[S] = new StateResultMonadError[S]{}


    def get: StateT[StateInfo, Result, StateInfo] = StateT{ st => Result.Ok(st, st) }
    def put(st:StateInfo): StateT[StateInfo, Result, Unit] = StateT{ _ => Result.Ok(st, ())}

    type StateResult[A] = StateT[StateInfo, Result, A]

    /** 
     * issue a new name and increment the nextNum in the state
     * */

    def newName:StateResult[String] = for {
        st <- get
        _  <- put(st.copy(nextNum= st.nextNum+1))
    } yield (s"_x_${st.nextNum}")
    

    /**
      * apply substituion s to lambda term t
      *
      * @param s substitution
      * @param t lambda term
      * @param m StateResultMonad dictionary containing all the Monad api
      * @return a StateResult, containing Result[StateInfo,Term]
      */
    def appSubst(s:Subst, t:Term)(using m:StateResultMonadError[StateInfo]):StateResult[Term] = t match {
        case ConstTerm(c) => m.pure(ConstTerm(c))
        case VarTerm(y)   => s match {
            case (x,u) if y == x => m.pure(u)
            case (x,u) => m.pure(VarTerm(y))
        }
        case AppTerm(t1,t2) => for { 
            t3 <- appSubst(s, t1)
            t4 <- appSubst(s, t2)
        } yield AppTerm(t3,t4)
        
        // TODO: Exercise 3.3
        case IfTerm(t1, t2, t3) => m.raiseError("TODO")

        case LetTerm(y, t2,t3) => s match {
            case (x, t1) if ((y !=x) && !(fv(t1).contains(y))) => for {
                t4 <- appSubst(s, t2)
                t5 <- appSubst(s, t3)
            } yield LetTerm(y, t4, t5)
            case (x, t1) => for {
                /* Substitution Application would fail because let bound variable is clashing with the substitution domain. 
                 * or substitution domain is captured in the RHS of let binding. 
                 * instead of failing, we apply alpha renaming to y and t3 immediately
                 * */ 
                n   <- newName
                z   <- m.pure(Var(n))
                s2  <- m.pure((y,VarTerm(z))) // [z/y]
                t3p <- appSubst(s2, t3) // alpha renaming
                t4  <- appSubst(s, t2)  // subst after alpha renaming
                t5  <- appSubst(s, t3p) // subst after alpha renaming
            } yield LetTerm(z, t4, t5)
        }
        case LambdaTerm(y, t2) => s match {
            case (x,t1) if ((y != x) && !(fv(t1).contains(y))) => for {
                t3 <- appSubst(s, t2)
            } yield LambdaTerm(y, t3)
            case (x, t1) => for {
                /* Substitution Application would fail because lambda bound variable is clashing with the substitution domain. 
                 * or substitution domain is captured in the body of lambda abstraction. 
                 * instead of failing, we apply alpha renaming to y and t2 immediately
                 * */ 
                n   <- newName
                z   <- m.pure(Var(n))
                s2  <- m.pure((y,VarTerm(z))) // [z/y]
                t2p <- appSubst(s2, t2) // alpha renaming
                t3  <- appSubst(s, t2p) // subst after alpha renaming
            } yield LambdaTerm(z, t3)
        } 
        case FixTerm(t) =>  for {
            tp <- appSubst(s, t)
        } yield FixTerm(tp)

        // TODO: Exercise 3.3
        case OpTerm(t1, op, t2) => m.raiseError("TODO")

    }

    /**
      * implementing big step operational semantics for lambda calculus
      *
      * @param t - a lambda calculus term
      * @param m - the StateResultMonadError dictionary with all the monad error API
      * @return - stateful result which is either an error message or a value
      */
    def eval(t:Term)(using m:StateResultMonadError[StateInfo]):StateResult[Value] = t match {
        case ConstTerm(c) => m.pure(ConstValue(c))
        case VarTerm(v)   => m.raiseError("Unbound variable.")
        case LambdaTerm(_,_) => m.pure(LambdaValue(t))
        case FixTerm(t1) => for {
            v1   <- eval(t1)
            t1pp <- v1 match {
                case LambdaValue(LambdaTerm(f,t1p)) => appSubst((f, FixTerm(t1)), t1p)
                case _ => m.raiseError("fix is applied to a non-lambda term.")
            }
            v2 <- eval(t1pp)
        } yield v2
        case AppTerm(t1,t2) => for {
            v1  <- eval(t1) 
            t3p <- v1 match {
                case LambdaValue(LambdaTerm(x,t3)) => appSubst((x, t2), t3)
                case _ => m.raiseError("the left subterm of a function application does not evaluate to a lambda value.")
            }
            v2 <- eval(t3p)
        } yield v2
        // TODO: Exercise 3.4
        case IfTerm(t1, t2, t3) => m.raiseError("TODO")
        // TODO: Exercise 3.4
        case LetTerm(x, t1, t2) => m.raiseError("TODO")
        // TODO: Exercise 3.4
        case OpTerm(t1, op, t2) => m.raiseError("TODO")
    }

    import Const.*

    /**
      * check whether the two input values v1 and v2 are the same type and equal.
      *
      * @param v1
      * @param v2
      * @param m - the StateResultMonadError dictionary with all the monad error API
      * @return either an error or a value
      */
    def equal(v1:Value, v2:Value)(using m:StateResultMonadError[StateInfo]):StateResult[Value] = (v1,v2) match {
        case (ConstValue(IntConst(u1)), ConstValue(IntConst(u2))) => m.pure(ConstValue(BoolConst(u1 == u2)))
        case (ConstValue(BoolConst(u1)), ConstValue(BoolConst(u2))) => m.pure(ConstValue(BoolConst(u1 == u2)))
        case (_,_) => m.raiseError("type mismatch for equality test.")
    }

    /**
      * compute the sum of two input values v1 and v2 if they are of type int.
      *
      * @param v1
      * @param v2
      * @param m - the StateResultMonadError dictionary with all the monad error API
      * @return either an error or a value
      */
    def plus(v1:Value, v2:Value)(using m:StateResultMonadError[StateInfo]):StateResult[Value] = (v1,v2) match {
        case (ConstValue(IntConst(u1)), ConstValue(IntConst(u2))) => m.pure(ConstValue(IntConst(u1 + u2)))
        case (_,_) => m.raiseError("type mismatch for plus operation.")
    }


    /**
      * compute the difference of two input values v1 and v2 if they are of type int.
      *
      * @param v1
      * @param v2
      * @param m - the StateResultMonadError dictionary with all the monad error API
      * @return either an error or a value
      */
    def minus(v1:Value, v2:Value)(using m:StateResultMonadError[StateInfo]):StateResult[Value]  = (v1,v2) match {
        case (ConstValue(IntConst(u1)), ConstValue(IntConst(u2))) => m.pure(ConstValue(IntConst(u1 - u2)))
        case (_,_) => m.raiseError("type mismatch for minus operation.")
    }

    /**
      * compute the product of two input values v1 and v2 if they are of type int.
      *
      * @param v1
      * @param v2
      * @param m - the StateResultMonadError dictionary with all the monad error API
      * @return either an error or a value
      */
    def mult(v1:Value, v2:Value)(using m:StateResultMonadError[StateInfo]):StateResult[Value]  = (v1,v2) match {
        case (ConstValue(IntConst(u1)), ConstValue(IntConst(u2))) => m.pure(ConstValue(IntConst(u1 * u2)))
        case (_,_) => m.raiseError("type mismatch for mult operation.")
    }

    /**
      * compute the quotient of two input values v1 and v2 if they are of type int.
      *
      * @param v1
      * @param v2
      * @param m - the StateResultMonadError dictionary with all the monad error API
      * @return either an error or a value
      */
    def div(v1:Value, v2:Value)(using m:StateResultMonadError[StateInfo]):StateResult[Value]  = (v1,v2) match {
        case (ConstValue(IntConst(u1)), ConstValue(IntConst(u2))) if u2 != 0 => m.pure(ConstValue(IntConst(u1 / u2)))
        case (ConstValue(IntConst(u1)), ConstValue(IntConst(u2)))            => m.raiseError("div by zero error.")
        case (_,_) => m.raiseError("type mismatch for div operation.")
    }

    /**
      * isTrue returns true if the input value is a boolean value and it is true.
      *
      * @param v - a value, either a constant or a lambda abstraction
      * @return boolean
      */
    def isTrue(v:Value):Boolean = v match {
        case ConstValue(c) => c match {
            case BoolConst(v) => v
            case IntConst(v) => false
        }
        case LambdaValue(l) => false
    }


}