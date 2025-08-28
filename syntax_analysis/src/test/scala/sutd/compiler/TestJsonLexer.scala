package sutd.compiler


import org.scalatest.funsuite
import org.scalatest.matchers
import scala.language.adhocExtensions
import sutd.compiler.JsonToken.*
import sutd.compiler.JsonLexerWithRegex.*

class TestJsonLexer extends funsuite.AnyFunSuite {
    import LToken.*

    test("test json lexer") {
        val input = "{'k1':1,'k2':[]}"
        val expected = List(LBrace,SQuote,StrTok("k1"),SQuote,Colon,IntTok(1),Comma,SQuote,StrTok("k2"),SQuote,Colon,LBracket, RBracket,RBrace)
        lex(input) match {
            case Left(err) => {
                println(err) 
                assert(false)
            } 
            case Right(toks) => assert(toks == expected)
        }
    }
}
