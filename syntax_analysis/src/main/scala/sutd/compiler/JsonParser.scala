package sutd.compiler

import sutd.compiler.JsonToken.*
import sutd.compiler.BacktrackParsec.*

object JsonParser {
    enum Json {
        case IntLit(v:Int)
        case StrLit(v:String)
        case JsonList(vs:List[Json])
        case JsonObject(flds:Map[String,Json])
    }


    // Exericse 2 - complete the following parser
    def parseJSON:Parser[LToken, Json] = 
        choice(parseJInt)
            (parseJStr)  // TODO:fixme, more choices required

    import LToken.*
    import Json.*
    
    def parseJInt:Parser[LToken, Json] = for {
        t <- sat( (x:LToken) => x match {
            case IntTok(v) => true
            case _         => false  
        })
        jint <- someOrFail(t)(s => s match {
            case IntTok(v) => Some(IntLit(v))
            case _ => None
        })("parseJInt() failed: sat() should have extracted an IntTok, but it did not.")
    } yield jint
    
    def parseJStr:Parser[LToken, Json] = for {
        _ <- parseSQuote 
        t <- sat( (x:LToken) => x match {
            case StrTok(v) => true
            case _         => false  
        })
        _ <- parseSQuote
        jstr <- someOrFail(t)((s:LToken) => s match {
            case StrTok(v) => Some(StrLit(v))
            case _ => None
        })("parseJStr() failed: sat() should have extracted an StrTok, but it did not.")
    } yield jstr
    
    def parseJList:Parser[LToken, Json] = for {
        _ <- parseLBracket
        ojs <- optional(interleave(parseJSON)(parseComma))
        _ <- parseRBracket
    } yield JsonList( ojs match {
        case Right(js) => js
        case Left(_) => Nil
    } )

    // Exericse 2 - complete the following parser
    def parseJObj:Parser[LToken, Json] = 
        // delete the following
        BacktrackParsec.parsecMonadError.pure(JsonObject(Map():Map[String,Json]))
        // your code here

    def parseNVP:Parser[LToken, (String, Json)] = for {
        jstr <- parseJStr
        name <- someOrFail(jstr)( js => js match {
            case StrLit(s) => Some(s)
            case _ => None
        })("parseNVP() failed, couldn't extract the string from a JStr object")
        _    <- parseColon
        obj  <- parseJSON
    } yield ((name, obj))

    def parseSQuote:Parser[LToken, LToken] = sat( x => x match {
        case SQuote => true 
        case _      => false
    })

    def parseComma:Parser[LToken, LToken] = sat( x => x match {
        case Comma => true
        case _     => false
    })

    def parseColon:Parser[LToken, LToken] = sat( x => x match {
        case Colon => true
        case _     => false
    })

    def parseLBracket:Parser[LToken, LToken] = sat( x => x match {
        case LBracket => true
        case _        => false
    })

    def parseRBracket:Parser[LToken, LToken] = sat( x => x match {
        case RBracket => true
        case _        => false
    })

    def parseLBrace:Parser[LToken, LToken] = sat( x => x match {
        case LBrace => true
        case _      => false
    })

    def parseRBrace:Parser[LToken, LToken] = sat( x => x match {
        case RBrace => true
        case _      => false
    })


}