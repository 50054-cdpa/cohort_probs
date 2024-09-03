{-# LANGUAGE ScopedTypeVariables #-}
module SUTD.Compiler.MathExpLexerWithRegex where 


import Text.Regex.TDFA
import SUTD.Compiler.MathExpToken 


integer = "^([[:digit:]]+)(.*)"
plus  = "^(\\+)(.*)"
asterix = "^(\\*)(.*)"


type Error = String


(=~=) :: String -> String -> Maybe (String,String)
(=~=) input regex = case input =~~ regex of 
    Nothing -> Nothing 
    Just (_::String,_::String,_::String,[tokStr,rest]) -> Just (tokStr, rest)
    Just _ -> Nothing

lexOne :: String -> Either Error (LToken, String)
lexOne = undefined -- fixme

lex :: String -> Either Error [LToken] 
lex src = go src []
    where go :: String -> [LToken] -> Either Error [LToken] 
          go []  acc = Right acc 
          go xs acc = case lexOne xs of
            Left err -> Left err
            Right (tok, rest) -> go rest (acc ++ [tok])