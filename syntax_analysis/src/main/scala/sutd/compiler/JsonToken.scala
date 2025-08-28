package sutd.compiler

object JsonToken {
    enum LToken { // lexical Tokens
        case IntTok(v: Int)
        case StrTok(s: String)
        case SQuote
        case LBracket
        case RBracket
        case LBrace
        case RBrace
        case Colon
        case Comma
        case WhiteSpace
    }
}