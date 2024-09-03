{-# LANGUAGE ScopedTypeVariables #-}
module SUTD.Compiler.JsonLexerWithRegex where


import Text.Regex.TDFA
import SUTD.Compiler.JsonToken 


integer = "^([[:digit:]]+)(.*)"
string  = "^([^']*)(.*)"
squote  = "^(')(.*)"
lbracket = "^(\\[)(.*)"
rbracket = "^(\\])(.*)"
lbrace = "^(\\{)(.*)"
rbrace = "^(\\})(.*)"
colon = "^(:)(.*)"
comma = "^(,)(.*)"


type Error = String


(=~=) :: String -> String -> Maybe (String,String)
(=~=) input regex = case input =~~ regex of 
    Nothing -> Nothing 
    Just (_::String,_::String,_::String,[tokStr,rest]) -> Just (tokStr, rest)
    Just _ -> Nothing

lexOne :: String -> Either Error (LToken, String)
lexOne input =
    case input =~= integer of
        Just (i,rest) -> Right (IntTok (read i), rest)
        Nothing -> case input =~= squote of
            Just (_,rest) -> Right (SQuote, rest)
            Nothing -> case input =~= lbracket of 
                Just (_,rest) -> Right (LBracket, rest) 
                Nothing -> case input =~= rbracket of 
                    Just (_, rest) -> Right (RBracket, rest)
                    Nothing -> case input =~= lbrace of 
                        Just (_, rest) -> Right (LBrace, rest)
                        Nothing -> case input =~= rbrace of
                            Just (_, rest) -> Right (RBrace, rest)
                            Nothing -> case input =~= colon of
                                Just (_, rest) -> Right (Colon, rest) 
                                Nothing -> case input =~= comma of 
                                    Just (_, rest) -> Right (Comma, rest)
                                    Nothing -> case input =~= string of
                                        Just (s,rest) -> Right (StrTok s, rest)
                                        Nothing -> Left "lexOne failed."
 

lex :: String -> Either Error [LToken] 
lex src = go src []
    where go :: String -> [LToken] -> Either Error [LToken] 
          go []  acc = Right acc 
          go xs acc = case lexOne xs of
            Left err -> Left err
            Right (tok, rest) -> go rest (acc ++ [tok])
