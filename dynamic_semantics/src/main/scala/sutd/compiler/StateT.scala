package sutd.compiler


import sutd.compiler.Functor.*
import sutd.compiler.Applicative.*
import sutd.compiler.Monad.*

object StateT {
    case class StateT[S, M[_], A](run: S => M[(S, A)])(using m:Monad[M]) {
        def flatMap[B](f: A => StateT[S, M, B]): StateT[S, M, B] = this match {
            case StateT(ssa) =>
                StateT(s => m.bind(ssa(s))
                    (sa => sa match {
                        case (s1,a) => f(a) match {
                            case StateT(ssb) => ssb(s1)
                            }
                        }
                    )
                ) 
            }
        
        def map[B](f: A => B): StateT[S, M, B] = this match {
            case StateT(ssa) =>
                StateT(s => m.bind(ssa(s))
                    (sa => sa match {
                        case (s1, a) => m.pure((s1, f(a)))
                    })
                )
        }
    }

    type StateTM = [S] =>> [M[_]] =>> [A] =>> StateT[S, M, A]

    trait StateTMonad[S,M[_]] extends Monad[StateTM[S][M]]  {
        implicit def M0:Monad[M]
        override def pure[A](v: A): StateT[S, M, A] = StateT(s => M0.pure((s, v)))
        override def bind[A, B](
            fa: StateT[S, M, A]
        )(
            ff: A => StateT[S, M, B]
        ): StateT[S, M, B] = fa.flatMap(ff)
        def get: StateT[S, M, S] = StateT(s => M0.pure((s, s)))
        def set(v: S): StateT[S, M, Unit] = StateT(s => M0.pure(v, ()))
    }



    case class Identity[A](run:A) {
        def flatMap[B](f:A=>Identity[B]):Identity[B] = this match {
            case Identity(a) => f(a)
        }
        def map[B](f:A=>B):Identity[B] = this match {
            case Identity(a) => Identity(f(a))
        }
    }

    given identityMonad:Monad[Identity] = new Monad[Identity] {
        override def pure[A](v:A):Identity[A] = Identity(v)
        override def bind[A,B](fa:Identity[A])(f: A => Identity[B]):Identity[B] = fa.flatMap(f)
    }

    trait StateMonad[S] extends StateTMonad[S, Identity] { 
        override def M0 = identityMonad
    }

    trait StateTMonadError[S,M[_],E] extends MonadError[StateTM[S][M],E] {
        implicit def M0:MonadError[M,E]
        override def pure[A](v: A): StateT[S, M, A] = StateT(s => M0.pure((s, v)))
        override def bind[A, B](
            fa: StateT[S, M, A]
        )(
            ff: A => StateT[S, M, B]
        ): StateT[S, M, B] = fa.flatMap(ff)
        def get: StateT[S, M, S] = StateT(s => M0.pure((s, s)))
        def set(v: S): StateT[S, M, Unit] = StateT(s => M0.pure(v, ()))
        override def raiseError[A](e: E): StateT[S, M, A]
        override def handleErrorWith[A](fa: StateT[S, M, A])(f: E => StateT[S, M, A]): StateT[S, M, A]
    }


}