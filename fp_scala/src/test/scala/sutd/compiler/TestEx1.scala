package sutd.compiler

import org.scalatest.funsuite
import org.scalatest.matchers
import sutd.compiler.Ex1.*

class TestEx1 extends funsuite.AnyFunSuite {
    test("test ex1: fib(1) == 1") {
        val result = fib(1)
        val expected = 1 
        assert(result == expected)
    }
    test("test ex1: fib(5) == 8") {
        val result = fib(5)
        val expected = 8
        assert(result == expected)
    }
    test("test ex1: fib(10) == 89") {
        val result = fib(10)
        val expected = 89
        assert(result == expected)
    }
}