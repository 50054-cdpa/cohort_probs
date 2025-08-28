package sutd.compiler

object Ex3 {
    enum BST[+A] {
        case Empty
        case Node(v:A, lft:BST[A], rgt:BST[A])
    }

    def insert[A](k:A, t:BST[A])(using orda:Ordering[A]):BST[A] = t // TODO: Fixme
}