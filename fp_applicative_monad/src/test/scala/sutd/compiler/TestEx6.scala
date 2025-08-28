package sutd.compiler

import scala.language.adhocExtensions
import org.scalatest.funsuite 
import org.scalatest.matchers
import sutd.compiler.Monad.*
import sutd.compiler.Ex6.* 



class TestEx6 extends funsuite.AnyFunSuite {

    import BTree.*
    
    test("TestEx6 reader monad") {
        val tree = Node(5, Node(3, Node(1, Empty, Empty), Node(4, Empty, Empty)), Empty)
        val expected = """5
    3
        1
            <empty>
            <empty>
        4
            <empty>
            <empty>
    <empty>"""
        rmPrint(tree) match {
            case Reader(run) => { 
                val result = run(Env(0))
                assert(expected == result)
            }
        }   
    
    }
}