package sutd.compiler

object Monad {
    trait Functor[T[_]] {
        def map[A,B](t:T[A])(f:A => B):T[B]
    }

    trait Applicative[F[_]] extends Functor[F] {
        def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
        def pure[A](a: A): F[A]
        def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa) 
    }

    trait Monad[F[_]] extends Applicative[F] {
        def bind[A,B](fa:F[A])(f:A => F[B]):F[B]
        override def pure[A](v:A):F[A]
        def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = 
            bind(ff)((f:A=>B) => bind(fa)((a:A)=> pure(f(a))))
    }

    case class Reader[R, A] (run: R=>A) { 
        // we need flatMap and map for for-comprehension
        def flatMap[B](f:A =>Reader[R,B]):Reader[R,B] = this match {
            case Reader(ra) => Reader {
                r => f(ra(r)) match {
                    case Reader(rb) => rb(r)
                }
            }
        }
        def map[B](f:A=>B):Reader[R, B] = this match {
            case Reader(ra) => Reader {
                r => f(ra(r))
            }
        }
    }

    type ReaderM = [R] =>> [A] =>> Reader[R, A]

    trait ReaderMonad[R] extends Monad[ReaderM[R]] {
        override def pure[A](v:A):Reader[R, A] = Reader (r => v)
        override def bind[A,B](fa:Reader[R, A])(f:A=>Reader[R,B]):Reader[R,B] = fa match {
            case Reader(ra) => Reader (
                r=> f(ra(r)) match {
                    case Reader(rb) => rb(r)
                }
            ) 
        }
        def ask:Reader[R,R] = Reader( r => r)
        def local[A](f:R=>R)(r:Reader[R,A]):Reader[R,A] = r match {
            case Reader(ra) => Reader( r => {
                val localR = f(r)
                ra(localR)
            })
        }    
    }

}