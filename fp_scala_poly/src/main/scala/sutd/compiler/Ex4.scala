package sutd.compiler
import Ex3.*

object Ex4 {
    trait Foldable[T[_]]{
        def foldLeft[A,B](t:T[B])(acc:A)(f:(A,B)=>A):A
        def foldRight[A,B](t:T[A])(acc:B)(f:(A,B)=>B):B
        // TODO: Add foldRight
    }

    given listFoldable:Foldable[List] = new Foldable[List] {
        def foldLeft[A,B](t:List[B])(acc:A)(f:(A,B)=>A):A = t.foldLeft(acc)(f)
        def foldRight[A,B](t:List[A])(acc:B)(f:(A,B)=>B):B = acc
        // TODO: Add foldRight
    }

    given bstFoldable: Foldable[BST] = new Foldable[BST] {
        def foldLeft[A,B](t:BST[B])(acc:A)(f:(A,B)=>A):A = acc
        def foldRight[A,B](t:BST[A])(acc:B)(f:(A,B)=>B):B = acc
    } // TODO: Fixme
}