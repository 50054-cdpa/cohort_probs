package sutd.compiler

object Ex1 {
    given booleanOrd:Ordering[Boolean] = new Ordering[Boolean] {
        def compare(x: Boolean, y: Boolean): Int = 0
    }
}