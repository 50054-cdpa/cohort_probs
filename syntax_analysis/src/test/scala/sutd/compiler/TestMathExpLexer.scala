package sutd.compiler

import org.scalatest.funsuite
import org.scalatest.matchers
import scala.language.adhocExtensions
import sutd.compiler.MathExpToken.*
import sutd.compiler.MathExpLexerWithRegex.*

class TestMathExpLexer extends funsuite.AnyFunSuite {
    import LToken.*

    test("test_mathexp_lex") {
        val s = "1+2*3"
        val result = lex(s)
        assert(
          result == Right(
            List(IntTok(1), PlusTok, IntTok(2), AsterixTok, IntTok(3))
          )
        )
    }
}
