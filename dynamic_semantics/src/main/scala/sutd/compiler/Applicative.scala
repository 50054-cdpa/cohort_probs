package sutd.compiler

import sutd.compiler.Functor.*

object Applicative {
    trait Applicative[F[_]] extends Functor[F] {
        def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
        def pure[A](a: A): F[A]
        def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)
    }

    given listApplicative: Applicative[List] = new Applicative[List] {
        def pure[A](a: A): List[A] = List(a)
        def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
            ff.map(f => fa.map(f)).flatten
    }

    trait ApplicativeError[F[_], E] extends Applicative[F] {
        def raiseError[A](e: E): F[A]
        def handleErrorWith[A](fa: F[A])(f: E => F[A]):F[A]
    }

}
