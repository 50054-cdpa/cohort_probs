package sutd.compiler

import scala.language.adhocExtensions
import org.scalatest.funsuite 
import org.scalatest.matchers
import sutd.compiler.Monad.*
import sutd.compiler.Ex4.{given,*} 



class TestEx4 extends funsuite.AnyFunSuite {



    test("TestEx4 testing map") {
        val m = Mk( s => (s, Some(1)) )
        val result:Option[Int] = runMk(m.map( x => x + 1))(()) 
        val expected:Option[Int] = Some(2)
        assert(expected == result)
    }

    test("TestEx4 testing flatMap Some") {
        val m = Mk( s => (s, Some(1)) )
        val result:Option[Int] = runMk(m.flatMap( x => Mk(s => (s,Some(x + 1)))))(())
        val expected:Option[Int] = Some(2)
        assert(expected == result)
    }

    test("TestEx4 testing flatMap None") {
        val m = Mk( s => (s, Some(1)) )
        val result:Option[Int] = runMk(m.flatMap( x => Mk(s => (s,None))))(())
        val expected:Option[Int] = None
        assert(expected == result)
    }

    test("TestEx4 testing flatMap state return") {
        val m:Mk[Int, Int] = Mk( s => (s, Some(s)) )
        val result:Option[Int] = runMk(m.flatMap( x => Mk(s => (s,Some(x + 1)))))(3)
        val expected:Option[Int] = Some(4)
        assert(expected == result)
    }
}