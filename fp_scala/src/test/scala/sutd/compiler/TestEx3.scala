package sutd.compiler

import org.scalatest.funsuite
import org.scalatest.matchers
import sutd.compiler.Ex3.*

class TestEx3 extends funsuite.AnyFunSuite {
    test("test ex3: lift(List(1,2,3)) == List(List(1), List(2), List(3))") {
        val result = lift(List(1,2,3))
        val expected = List(List(1), List(2), List(3))
        assert(result == expected)
    }
    test("test ex3: lift(List('a','b','c','d')) == List(List('a'), List('b'), List('c'), List('d'))") {
        val result = lift(List('a','b','c','d'))
        val expected = List(List('a'), List('b'), List('c'), List('d'))
        assert(result == expected)
    }
}