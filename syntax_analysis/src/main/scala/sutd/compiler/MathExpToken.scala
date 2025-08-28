package sutd.compiler

object MathExpToken {
    enum LToken { // lexical Tokens
        case IntTok(v: Int)
        case PlusTok
        case AsterixTok
    }
}