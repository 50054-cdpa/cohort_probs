package sutd.compiler

import sutd.compiler.Monad.*

object Ex4 {
    case class Mk[S,A]( f : (S =>(S, Option[A]) )) {
        def map[B](f:A=>B):Mk[S,B] = this.map(f) // TODO: fixme
        def flatMap[B](f:A=>Mk[S,B]):Mk[S,B] = this.flatMap(f) // TODO: fixme
    }

    def runMk[S,A](mk:Mk[S,A])(s:S):Option[A] = mk match {
        case Mk(f) => f(s) match {
            case (_,o) => o
        }
    }
}