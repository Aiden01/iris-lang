{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Language.Interpreter.Types
    ( Scope
    , Value(..)
    , VResult
    , VError(..)
    , Env
    , insert
    , get
    , modify
    )
where

import qualified Data.Map                      as M
import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                )
import           Data.Maybe                     ( isJust )

type Scope a = M.Map String a
type VResult = ExceptT VError IO Value

class Env env a where
    insert :: String -> a -> env -> env
    get :: String -> env -> Maybe a
    modify :: String -> a -> env -> env

instance Env (Scope a) a where
    insert = M.insert
    get = M.lookup
    modify key value env = M.adjust (\ _ -> value) key env

data Value
    = VInt Integer
    | VString String
    | VFloat Double
    | VChar Char
    | VBool Bool
    | VoidV
    | VList [Value]
    | VObject (M.Map String Value)
    | Fn ([Value] -> VResult)


data VError
    = Unbound String
    | Custom String

instance Show VError where
    show (Unbound x ) =" Cannot use unbound variable " <> x
    show (Custom x ) =  x


instance Show Value where
    show (VInt x) = show x
    show (VString x) = "\"" <> x <> "\""
    show (VFloat x) = show x
    show (VChar x) = show x
    show (VList x) = show x
    show (VBool x) = show x
    show (VObject x) = M.foldrWithKey (\ k v acc -> acc <> "{" <> k <> ":" <> show v <> "},") [] x
    show VoidV = "()"
    show (Fn _) = "FN"
