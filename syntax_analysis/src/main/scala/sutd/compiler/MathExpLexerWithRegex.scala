package sutd.compiler

import scala.util.matching.Regex
import sutd.compiler.MathExpToken.* 
object MathExpLexerWithRegex {
    import LToken.*
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
    val plus = raw"(\+)(.*)".r
    val asterix = raw"(\*)(.*)".r

    import LToken.*
    def lex_one(src:String):Either[Error, (LToken, String)] = src match {
        case integer(s, rest) => Right((IntTok(s.toInt), rest))
        case plus(_, rest) => Right((PlusTok, rest))
        case asterix(_, rest) => Right((AsterixTok, rest))
        case _ => Left(s"lexer error: unexpected token at ${src}")
    }

    val mathexpstr = "1+2*3"
}

