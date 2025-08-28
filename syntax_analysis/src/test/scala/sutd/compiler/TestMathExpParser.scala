package sutd.compiler

import org.scalatest.funsuite 
import org.scalatest.matchers
import scala.language.adhocExtensions
import sutd.compiler.MathExpToken.*
import sutd.compiler.MathExpParser.*
import sutd.compiler.BacktrackParsec.*



class TestMathExpParser extends funsuite.AnyFunSuite {
    import LToken.*
    import Result.*
    import Exp.*
    import Term.* 
    import Factor.*
    import TermLEP.*
    
    test("test_mathexp_parse") {
        // val s = "1+2*3"
        val toks = List(IntTok(1), PlusTok, IntTok(2), AsterixTok, IntTok(3))
        val result = BacktrackParsec.run(parseExp)(toks)
        val expected = PlusExp(FactorTerm(Factor(1)),TermExp(MultTerm(FactorTerm(Factor(2)),Factor(3))))
        result match {
            case Ok((t, Nil)) =>  assert(t == expected)
            case _ => assert(false)
        }
    }
}