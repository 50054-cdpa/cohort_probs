package sutd.compiler

import sutd.compiler.Monad.*

object Ex2 {
    type PP = [B] =>> [A] =>> (A,B)
    given pairFunctor[C]:Functor[PP[C]] = new Functor[PP[C]]  {
        def map[A,B](fa:(A,C))(f:A=>B):(B,C) = map(fa)(f) // TODO:fixme
    }
}