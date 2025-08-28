package sutd.compiler

object Functor {
    trait Functor[T[_]] {
        def map[A, B](t: T[A])(f: A => B): T[B]
    }

    given listFunctor: Functor[List] = new Functor[List] {
        def map[A, B](t: List[A])(f: A => B): List[B] = t.map(f)
    }

}
