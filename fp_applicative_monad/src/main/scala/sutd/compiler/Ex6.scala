package sutd.compiler

import sutd.compiler.Monad.{given, *}

object Ex6 {
    enum BTree[+A] {
        case Empty
        case Node(v:A, lft:BTree[A], rght:BTree[A])
    }

    import BTree.*

    case class Env(indent:Int)

    given EnvReader:ReaderMonad[Env] = new ReaderMonad[Env] {}

    def indentEnv(env:Env):Env = env match {
        case Env(x) => Env(x + 4);
    }


    def rmPrint(t:BTree[Int])(using pr:ReaderMonad[Env]):Reader[Env, String] = Reader(env => "") // TODO:fixme

}