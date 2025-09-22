package sutd.compiler

import org.scalatest.funsuite
import org.scalatest.matchers
import sutd.compiler.Ex6.*

class TestEx6 extends funsuite.AnyFunSuite {
    test("test ex6: rotate(List(List(1,2,3), List(4,5,6), List(7,8,9))) == List(List(7,4,1), List(8,5,2), List(9,6,3))") {
        val input = List(
            List(1, 2, 3),
            List(4, 5, 6),
            List(7, 8, 9)
        )
        val result = rotate(input)
        val expected = List(
            List(7, 4, 1),
            List(8, 5, 2),
            List(9, 6, 3)
        )
        assert(result == expected)
    }
}