package sutd.compiler

import sutd.compiler.Monad.*
import sutd.compiler.Ex4.*

object Ex5 {
    import Mk.*
    type MkM = [S] =>> [A] =>> Mk[S,A]

    trait MkMonad[S] extends Monad[MkM[S]] {
        override def pure[A](v:A):Mk[S,A] = pure(v) // TODO:fixme
        override def bind[A,B](
            fa:Mk[S,A]
            )(
                ff:A => Mk[S,B]
            ):Mk[S,B] = bind(fa)(ff) // TODO:fixme
        def get:Mk[S, S] = Mk(s => (s,Some(s)))
        def set(v:S):Mk[S,Unit] = Mk(s => (v,Some(())))
    }


}