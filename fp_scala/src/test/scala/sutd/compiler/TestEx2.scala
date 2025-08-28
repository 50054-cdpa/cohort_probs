package sutd.compiler

import org.scalatest.funsuite
import org.scalatest.matchers
import sutd.compiler.Ex2.*

class TestEx2 extends funsuite.AnyFunSuite {
    test("test ex2: length(Nil) == 0") {
        val result = length(Nil)
        val expected = 0
        assert(result == expected)
    }
    test("test ex2: length(List(1,2,3)) == 3") {
        val result = length(List(1,2,3))
        val expected = 3
        assert(result == expected)
    }
    test("test ex2: length(List('a','b','c','d')) == 4") {
        val result = length(List('a','b','c','d'))
        val expected = 4
        assert(result == expected)
    }
}