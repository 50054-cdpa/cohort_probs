package sutd.compiler
import scala.language.adhocExtensions
import org.scalatest.funsuite
import org.scalatest.matchers
import sutd.compiler.Ex2.*

class TestEx2 extends funsuite.AnyFunSuite {
    test("test ex2: qsort(List(3,5,2,1)) == List(1,2,3,5)") {
        val result = qsort(List(3,5,2,1))
        val expected = List(1,2,3,5)
        assert(result == expected)
    }
    test("test ex2: qsort(List(false, false, true)) == List(true, false, false)") {
        import sutd.compiler.Ex1.{given, *}
        val result = qsort(List(false, false, true))
        val expected = List(true, false, false)
        assert(result == expected)
    }
}