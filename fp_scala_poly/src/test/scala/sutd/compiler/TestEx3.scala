package sutd.compiler
import scala.language.adhocExtensions
import org.scalatest.funsuite
import org.scalatest.matchers
import sutd.compiler.Ex3.*
import sutd.compiler.Ex3.BST.*

class TestEx3 extends funsuite.AnyFunSuite {
    test("test ex3: insert(3, Empty) == Node(3,Empty,Empty) and insert(5, Node(3, Empty, Empty)) == Node(3, Empty, Node(5, Empty, Empty))") {
        val t0 = insert(3, Empty)
        assert(t0 == Node(3,Empty,Empty))
        val t1 = insert(5, t0)
        assert(t1 == Node(3,Empty, Node(5, Empty, Empty)))
    }
    test("test ex3: insert(false, insert(false, insert(true, insert(true, Empty)))) == Node(true, Empty, Node(false, Empty, Empty))") {
        import  Ex1.{given, *}
        val t2 = insert(true, Empty)
        val t3 = insert(false, insert(true, t2))
        assert(t3 == Node(true, Empty, Node(false, Empty, Empty)))
    }
}