package sutd.compiler


import scala.language.adhocExtensions
import org.scalatest.funsuite 
import org.scalatest.matchers
import sutd.compiler.Monad.*
import sutd.compiler.Ex4.*
import sutd.compiler.Ex5.{given,*} 



class TestEx5 extends funsuite.AnyFunSuite {


    test("TestEx5 testing increase State") {
        given mkMonadInt:MkMonad[Int] = new MkMonad[Int]{} 
        def incrState(using i:MkMonad[Int]):Mk[Int,Int] = for {
            x <- i.get
            _ <- i.set(x+1)
            y <- i.get
        } yield y
        val result:Option[Int] = runMk(incrState)(1)
        val expected:Option[Int] = Some(2)
        assert(expected == result)
    }

    test("TestEx5 testing Fib State") {
        case class FibState(f:Int, s:Int)
        given mkMonadFibState:MkMonad[FibState] = new MkMonad[FibState]{} 
        def getFib(n:Int)(using i:MkMonad[FibState]):Mk[FibState,Int] = n match {
            case 0 => for {
                FibState(f,s) <- i.get
            } yield s
            case n => for {
                FibState(f,s) <- i.get
                _ <- i.set(FibState(s, f+s))
                r <- getFib(n-1)
            } yield r
        }
        // desugared version of getFib
        def getFib2(n:Int)(using i:MkMonad[FibState]):Mk[FibState,Int] = n match { 
            case 0 => (i.get).map( fs => fs match { case FibState(f,s) => s } )
            case n => (i.get).flatMap( fs => fs match {
                case FibState(f,s) => i.set(FibState(s,f+s)).flatMap( unit => unit match {
                    case _ => getFib(n-1).map( r=> r)
                })
            })
        }
        val result:Option[Int] = runMk(getFib(4))(FibState(0,1))
        val expected:Option[Int] = Some(5)
        assert(expected == result)
    }
}