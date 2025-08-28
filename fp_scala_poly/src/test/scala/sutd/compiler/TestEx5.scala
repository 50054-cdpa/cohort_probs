package sutd.compiler
import scala.language.adhocExtensions
import org.scalatest.funsuite
import org.scalatest.matchers
import sutd.compiler.Ex3.*
import sutd.compiler.Ex5.*

class TestEx5 extends funsuite.AnyFunSuite {
    test("test ex5: subtree(5, insert(4, insert(5, insert(3, Empty)))) == Some(Node(5, Node(4, Empty, Empty), Empty))") {
        import Ex3.BST.*
        val t0 = insert(3, Empty)
        val t1 = insert(5, t0)
        val t2 = insert(4, t1)
        assert(subtree(5,t2) == Some(Node(5,Node(4,Empty,Empty),Empty)))
    }
}