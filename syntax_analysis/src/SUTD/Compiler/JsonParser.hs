{-# LANGUAGE MultiParamTypeClasses #-}
module SUTD.Compiler.JsonParser where 

import Data.Map
import SUTD.Compiler.JsonToken 
import SUTD.Compiler.ParserEnv 
import SUTD.Compiler.BacktrackParsec


data PEnv = PEnv { toks:: [LToken]} deriving (Show, Eq) 

instance ParserEnv PEnv LToken where
    getCol penv           = -1 -- ^ not in used. 
    getLine penv          = -1 -- ^ not in used. 
    setTokens ts penv     = penv{toks= ts}
    setLine _ penv        = penv -- ^ not in used.
    setCol _ penv         = penv -- ^ not in used. 
    isNextTokNewLine penv = False -- ^ not in used. 
    getTokens             = toks   


data Json = 
    IntLit Int | 
    StrLit String | 
    JsonList [Json] |
    JsonObject (Map String Json)
    deriving (Show, Eq)


parseJSON :: Parser PEnv Json 
parseJSON = undefined -- fixme

parseJInt :: Parser PEnv Json
parseJInt = do 
    t <- sat ( \x -> case x of 
        { IntTok v -> True 
        ; _        -> False}) "Expecting an int token."
    jint <- justOrFail t (\s -> case s of 
        { IntTok v -> Just (IntLit v) 
        ; _        -> Nothing}) "parseJInt failed: sat should have extracted an IntTok, but it did not."
    return jint 

parseJStr :: Parser PEnv Json 
parseJStr = do 
    parseSQuote 
    t <- sat ( \x -> case x of 
        { StrTok v -> True
        ; _        -> False}) "Expecting a string token." 
    parseSQuote 
    jstr <- justOrFail t (\s -> case s of 
        { StrTok v -> Just (StrLit v) 
        ; _        -> Nothing }) "parseJStr failed: sat() should have extracted an StrTok, but it did not."
    return jstr

parseJList :: Parser PEnv Json 
parseJList = undefined -- fixme 

parseJObj :: Parser PEnv Json
parseJObj = do 
    parseLBrace
    njs <- interleave parseNVP parseComma
    parseRBrace
    return (JsonObject (Data.Map.fromList njs)) 


parseNVP :: Parser PEnv (String, Json) 
parseNVP = do 
    jstr <- parseJStr 
    name <- justOrFail jstr (\js -> case js of 
        { StrLit s -> Just s
        ; _ -> Nothing}) "parseNVP failed, couldn't extract the string from a JStr object"
    parseColon 
    obj <- parseJSON 
    return (name,obj)


parseSQuote :: Parser PEnv LToken 
parseSQuote = sat (\x -> case x of 
    { SQuote -> True 
    ; _      -> False}) "Expecting a (') token."

parseComma :: Parser PEnv LToken 
parseComma = sat (\x -> case x of 
    { Comma -> True 
    ; _     -> False}) "Expecting a (,) token."

parseColon :: Parser PEnv LToken 
parseColon = sat (\x -> case x of 
    { Colon -> True 
    ; _     -> False}) "Expecting a : token."


parseLBracket :: Parser PEnv LToken 
parseLBracket = sat (\x -> case x of 
    { LBracket -> True 
    ; _        -> False}) "Expecting a [ token."

parseRBracket :: Parser PEnv LToken 
parseRBracket = sat (\x -> case x of 
    { RBracket -> True 
    ; _        -> False}) "Expecting a ] token."

parseLBrace :: Parser PEnv LToken 
parseLBrace = sat (\x -> case x of 
    { LBrace -> True 
    ; _      -> False}) "Expecting a ( token."

parseRBrace :: Parser PEnv LToken 
parseRBrace = sat (\x -> case x of 
    { RBrace -> True 
    ; _      -> False}) "Expecting a ) token."
