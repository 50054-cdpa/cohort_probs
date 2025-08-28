package sutd.compiler

import scala.language.adhocExtensions
import org.scalatest.funsuite 
import org.scalatest.matchers
import sutd.compiler.Monad.*
import sutd.compiler.Ex2.{given,*} 



class TestEx2 extends funsuite.AnyFunSuite {
    
    test("TestEx2 map((1, \"A\"))(x => x + 1) == (2, \"A\")") {
        val pair = (1, "A")
        val expected = (2, "A")
        val result = pairFunctor.map(pair)(x => x + 1)
        assert(expected == result)
    }
        
}