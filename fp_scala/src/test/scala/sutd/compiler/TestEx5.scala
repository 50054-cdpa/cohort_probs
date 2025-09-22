package sutd.compiler

import org.scalatest.funsuite
import org.scalatest.matchers
import sutd.compiler.Ex5.*

class TestEx5 extends funsuite.AnyFunSuite {
    test("test ex5: mergesort(List(3,17,8,9,11,200,0,5)) == List(0,3,5,8,9,11,17,200)") {
        val result = mergesort(List(3,17,8,9,11,200,0,5))
        val expected = List(0, 3, 5, 8, 9, 11, 17, 200)
        assert(result == expected)
    }
}