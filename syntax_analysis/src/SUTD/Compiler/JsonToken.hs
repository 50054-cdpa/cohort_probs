module SUTD.Compiler.JsonToken where

data LToken = 
    IntTok Int |
    StrTok String | 
    SQuote | 
    LBracket | 
    RBracket | 
    LBrace |
    RBrace | 
    Colon  |
    Comma  | 
    WhiteSpace 
    deriving (Show, Eq) 