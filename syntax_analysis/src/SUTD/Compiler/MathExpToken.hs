module SUTD.Compiler.MathExpToken where

data LToken = 
    IntTok Int |
    PlusTok | 
    AsterixTok
    deriving (Show, Eq) 