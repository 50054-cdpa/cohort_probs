package sutd.compiler

import scala.language.adhocExtensions
import org.scalatest.funsuite 
import org.scalatest.matchers
import sutd.compiler.JsonToken.*
import sutd.compiler.JsonParser.*
import sutd.compiler.BacktrackParsec.*



class TestJsonParser extends funsuite.AnyFunSuite {
    import LToken.*
    import Result.*
    import Json.*
    

    test("test_json_empty_list_parse") {
        val input = List(LBracket, RBracket)
        val result = BacktrackParsec.run(parseJList)(input)
        val expected = JsonList(Nil) 
        result match {
            case Ok((t, Nil)) =>  assert(t == expected)
            case rest => {
                println(rest)
                assert(false)
            }
        }
    }


    test("test_json_list_parse") {
        val input = List(LBracket, IntTok(1), Comma, IntTok(2), RBracket)
        val result = BacktrackParsec.run(parseJList)(input)
        val expected = JsonList(List(IntLit(1),IntLit(2))) 
        result match {
            case Ok((t, Nil)) =>  assert(t == expected)
            case rest => {
                println(rest)
                assert(false)
            }
        }
    }
        
    test("test_json_parse") {
        // {'k1':1,'k2':[]}
        val input = List(LBrace,SQuote,StrTok("k1"),SQuote,Colon,IntTok(1),Comma,SQuote,StrTok("k2"),SQuote,Colon,LBracket, RBracket,RBrace)
        val result = BacktrackParsec.run(parseJSON)(input)
        val expected = JsonObject(
            Map(
                "k1" -> IntLit(1),
                "k2" -> JsonList(Nil)
        ))
        result match {
            case Ok((t, Nil))  =>  assert(t == expected)
            case rest => {
                println(rest)
                assert(false)
            }
        }
    }
}