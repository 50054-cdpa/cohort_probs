package sutd.compiler

import scala.util.matching.Regex
import sutd.compiler.JsonToken.*

object JsonLexerWithRegex {

    type Error = String

    def lex(src:String):Either[Error, List[LToken]] = {
        def go(src:String, acc:List[LToken]):Either[Error, List[LToken]] = {
            if (src.length == 0)  
            {
                Right(acc)
            } 
            else 
            {
                lex_one(src) match {
                    case Left(error) => Left(error)
                    case Right((ltoken, rest)) => go(rest, acc++List(ltoken))
                }
            }
        }
        go(src, List())
    }

    val integer = raw"(\d+)(.*)".r
    val string = raw"([^']*)(.*)".r
    val squote = raw"(')(.*)".r
    val lbracket = raw"(\[)(.*)".r
    val rbracket = raw"(\])(.*)".r
    val lbrace = raw"(\{)(.*)".r
    val rbrace = raw"(\})(.*)".r
    val colon = raw"(:)(.*)".r
    val comma = raw"(,)(.*)".r

    import LToken.*
    def lex_one(src:String):Either[Error, (LToken, String)] = src match {
        // TODO: Exercise 1
        // more cases here.
        case _ => Left(s"lexer error: unexpected token at ${src}")
    }

    val jsonstr = "{'k1':1,'k2':[]}"
}
