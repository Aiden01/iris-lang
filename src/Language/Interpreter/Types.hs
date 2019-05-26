

module Language.Interpreter.Types
    ( Scope
    , Value(..)
    , VResult
    , VError(..)
    )
where

import qualified Data.Map                      as M
import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                )



type Scope a = M.Map String a
type VResult = ExceptT VError IO Value


data Value
    = VInt Integer
    | VString String
    | VFloat Double
    | VChar Char
    | VBool Bool
    | VoidV
    | VList [Value]
    | Fn ([Value] -> ExceptT VError IO Value)

data VError
    = Unbound String
    | Custom String

instance Show VError where
    show (Unbound x) = "Cannot use unbound variable " <> x
    show (Custom x) = x


instance Show Value where
    show (VInt x) = show x
    show (VString x) = x
    show (VFloat x) = show x
    show (VChar x) = show x
    show (VList x) = show $ map show x
    show (VBool x) = show x
    show VoidV = "()"
    show (Fn _) = "FN"
