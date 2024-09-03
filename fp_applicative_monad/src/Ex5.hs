module Ex5 where

import Control.Monad.Reader

data BTree a =
    Empty | Node a (BTree a) (BTree a)
    deriving Show

data Env = Env { indent :: Int}


indentEnv :: Env -> Env
indentEnv (Env i) = Env (i + 4)

spaces :: Int -> String
spaces n = replicate n ' '

rmPrint :: (Show a) => BTree a -> Reader Env String
rmPrint = undefined -- fixme
