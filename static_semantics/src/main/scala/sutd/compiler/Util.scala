package sutd.compiler

import sutd.compiler.SimplyTypedLambdaCalculus.*
import sutd.compiler.Monad.*
import sutd.compiler.StateT.*


object Util {
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
        case IfTerm(t1, t2, t3) => for {
            t4 <- appSubst(s, t1)
            t5 <- appSubst(s, t2)
            t6 <- appSubst(s, t3)
        } yield IfTerm(t4,t5,t6)
        case LetTerm(y, ty, t2,t3) => s match {
            case (x, t1) if ((y !=x) && !(fv(t1).contains(y))) => for {
                t4 <- appSubst(s, t2)
                t5 <- appSubst(s, t3)
            } yield LetTerm(y, ty, t4, t5)
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
            } yield LetTerm(z, ty, t4, t5)
        }
        case LambdaTerm(y, ty, t2) => s match {
            case (x,t1) if ((y != x) && !(fv(t1).contains(y))) => for {
                t3 <- appSubst(s, t2)
            } yield LambdaTerm(y, ty, t3)
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
            } yield LambdaTerm(z, ty, t3)
        } 
        case FixTerm(t) =>  for {
            tp <- appSubst(s, t)
        } yield FixTerm(tp)

        case OpTerm(t1, op, t2) => for {
            tp1 <- appSubst(s, t1) 
            tp2 <- appSubst(s, t2) 
        } yield OpTerm(tp1, op, tp2)

    }

}