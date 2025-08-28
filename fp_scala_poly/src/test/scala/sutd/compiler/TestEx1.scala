package sutd.compiler
import scala.language.adhocExtensions
import org.scalatest.funsuite
import org.scalatest.matchers
import sutd.compiler.Ex1.{given, *}

class TestEx1 extends funsuite.AnyFunSuite {
    test("test ex1: booleanOrd.compare(true, false) == -1") {
        val result = booleanOrd.compare(true, false)
        val expected = -1 
        assert(result == expected)
    }
    test("test ex1: booleanOrd.compare(true,true) == 0") {
        val result = booleanOrd.compare(true,true)
        val expected = 0
        assert(result == expected)
    }
    test("test ex1: booleanOrd.compare(false,true) == 1") {
        val result = booleanOrd.compare(false,true)
        val expected = 1
        assert(result == expected)
    }
    test("test ex1: booleanOrd.compare(false,false) == 0") {
        val result = booleanOrd.compare(false,false)
        val expected = 0
        assert(result == expected)
    }
}