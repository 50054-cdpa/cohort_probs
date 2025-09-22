package sutd.compiler

import org.scalatest.funsuite
import org.scalatest.matchers
import sutd.compiler.Ex4.*

class TestEx4 extends funsuite.AnyFunSuite {
    test("test ex4: flatten(List()) == List()") {
        val result = flatten(List[List[Int]]())
        val expected = List[Int]()
        assert(result == expected)
    }

    test("test ex4: flatten(List(List(1), List(2), List(3))) == List(1,2,3)") {
        val result = flatten(List(List(1), List(2), List(3)))
        val expected = List(1, 2, 3)
        assert(result == expected)
    }

    test("test ex4: flatten(List(List('a'), List('b'), List('c'), List('d'))) == List('a','b','c','d')") {
        val result = flatten(List(List('a'), List('b'), List('c'), List('d')))
        val expected = List('a','b','c','d')
        assert(result == expected)
    }
}