

module Language.Interpreter.Types
    ( Scope
    , Value(..)
    , VResult
    )
where

import qualified Data.Map                      as M
import           Control.Monad.Except           ( ExceptT )



type Scope a = M.Map String a
type VResult = ExceptT String IO Value


data Value
    = VInt Integer
    | VString String
    | VFloat Double
    | VChar Char
    | VBool Bool
    | VoidV
    | VList [Value]
    | BFn (Value -> Value -> ExceptT String IO Value)
    | Fn (Value -> ExceptT String IO Value)

instance Show Value where
    show (VInt x) = show x
    show (VString x) = x
    show (VFloat x) = show x
    show (VChar x) = show x
    show VoidV = "()"
    show (BFn _) = "BFN"
    show (Fn _) = "BFN"
