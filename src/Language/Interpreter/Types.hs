{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FunctionalDependencies #-}

module Language.Interpreter.Types
    ( Scope
    , Value(..)
    , EvalState
    , VError(..)
    , Env
    , insert
    , get
    , modify
    , exists
    )
where

import qualified Data.Map                      as M
import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                )
import           Data.Maybe                     ( isJust )
import           Text.Megaparsec.Pos            ( SourcePos
                                                , sourceName
                                                , sourceLine
                                                , sourceColumn
                                                , sourcePosPretty
                                                )
import           Control.Monad.State            ( StateT )

type Scope a = M.Map String a
type EvalState a = StateT [Scope Value] (ExceptT VError IO) a

class Env env a | env -> a where
    insert :: String -> a -> env -> env
    get :: String -> env -> Maybe a
    modify :: String -> a -> env -> env
    exists :: String -> env -> Bool

instance Env (Scope a) a where
    insert = M.insert
    get = M.lookup
    modify key value env = M.adjust (\ _ -> value) key env
    exists k env = isJust $ M.lookup k env

data Value
    = VInt Integer
    | VString String
    | VFloat Double
    | VChar Char
    | VBool Bool
    | VoidV
    | VList [Value]
    | VObject (M.Map String Value)
    | Fn ([Value] -> EvalState Value)


data VError
    = Unbound SourcePos String
    | Custom SourcePos String

showPos :: SourcePos -> String
showPos pos = "Error in " <> sourcePosPretty pos

instance Show VError where
    show (Unbound pos x) = showPos pos <> ": cannot use unbound variable " <> x
    show (Custom pos x) = showPos pos <> ": " <> x


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
