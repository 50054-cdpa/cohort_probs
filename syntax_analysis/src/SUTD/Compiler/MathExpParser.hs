{-# LANGUAGE MultiParamTypeClasses #-}
module SUTD.Compiler.MathExpParser where 

import SUTD.Compiler.MathExpToken 
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

{- grammar 4 before left recursion elimination 
    E::= T + E
    E::= T
    T::= T * F 
    T::= F
    F::= i
-}


data Exp = TermExp Term | PlusExp Term Exp 
    deriving (Show, Eq) 

data Term = FactorTerm Factor | MultTerm Term Factor 
    deriving (Show, Eq)


data Factor = Factor Int
    deriving (Show, Eq)


{- after left recursion elimination
    E  ::= T + E
    E  ::= T
    T  ::= FT'     <-- left recursion eliminated
    T' ::= *FT'
    T' ::= epsilon
    F  ::=i
-}


data TermLE = TermLE Factor TermLEP 


data TermLEP = MultTermLEP Factor TermLEP | Eps 

parseExp :: Parser PEnv Exp 
parseExp = choice parsePlusExp parseTermExp

parsePlusExp :: Parser PEnv Exp
parsePlusExp = do
    t    <- parseTerm 
    plus <- parsePlusTok
    e    <- parseExp 
    return (PlusExp t e)

parseTermExp :: Parser PEnv Exp 
parseTermExp = do 
    t <- parseTerm 
    return (TermExp t)

parseTerm :: Parser PEnv Term 
parseTerm = do 
    tle <- parseTermLE 
    return (fromTermLE tle) 

parseTermLE :: Parser PEnv TermLE 
parseTermLE = do 
    f  <- parseFactor
    tp <- parseTermP 
    return (TermLE f tp)

parseTermP :: Parser PEnv TermLEP 
parseTermP = do 
    omt <- optional parseMultTermP
    case omt of 
        Left _ -> return Eps
        Right t -> return t


parseMultTermP :: Parser PEnv TermLEP 
parseMultTermP = do 
    asterix <- parseAsterixTok 
    f       <- parseFactor
    tp      <- parseTermP 
    return (MultTermLEP f tp)

parseFactor :: Parser PEnv Factor 
parseFactor = do 
    i <- parseIntTok 
    f <- justOrFail i ( \ itok -> case itok of 
        { IntTok v -> Just (Factor v)
        ; _ -> Nothing 
        }) "parseFactor fail: expect to parse an integer token but it is not an integer."
    return f


parsePlusTok :: Parser PEnv LToken
parsePlusTok = sat ( \x -> case x of 
    { PlusTok -> True
    ; _       -> False
    }) "parsePlusTok failed, expecting a plus token."

parseAsterixTok :: Parser PEnv LToken 
parseAsterixTok = sat ( \x -> case x of 
    AsterixTok -> True
    _          -> False ) "parseAsterixTok failed, expecting an asterix token."


parseIntTok :: Parser PEnv LToken 
parseIntTok = sat ( \x -> case x of 
    IntTok v -> True
    _        -> False ) "parseIntTok failed, expecting an integer token."

{-
    parse tree with left recursion
        T
       / \
      T   f
     / \
     f  f 

     parse tree with left recursion eliminated
      Tp
     / \
     f  Tp
       / \
       f  Tp
          / \
          f  eps
-}

fromTermLE :: TermLE -> Term 
fromTermLE (TermLE f tep) = fromTermLEP (FactorTerm f) tep 

fromTermLEP :: Term -> TermLEP -> Term 
fromTermLEP t1 Eps = t1 
fromTermLEP t1 (MultTermLEP f2 tp2) = 
    let t2 = MultTerm t1 f2
    in fromTermLEP t2 tp2


