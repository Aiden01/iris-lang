{-# LANGUAGE LambdaCase #-}
module Language.Interpreter.DefaultEnv
    ( defaultEnv
    , makeTruthy
    )
where

import           Language.Interpreter.Types
import           Control.Monad.Except           ( ExceptT
                                                , throwError
                                                )
import           Control.Monad.Trans            ( lift )
import qualified Data.Map                      as M
import           Text.Read                      ( readMaybe )

makeTruthy :: Value -> Bool
makeTruthy = \case
    VBool   False -> False
    VInt    0     -> False
    VFloat  0.0   -> False
    VString []    -> False
    VList   []    -> False
    VoidV         -> False
    _             -> True

defaultEnv :: Scope Value
defaultEnv = M.fromList
    [ ("+"    , addV)
    , ("-"    , subV)
    , ("/"    , divV)
    , ("*"    , multV)
    , ("^"    , powV)
    , ("and"  , andV)
    , ("&&"   , andV)
    , ("or"   , orV)
    , ("||"   , orV)
    , ("neg"  , negV)
    , ("!"    , notV)
    , ("print", printV)
    , ("str"  , strV)
    , ("int"  , intV)
    , ("bool" , boolV)
    ]
  where
    addV = Fn $ \([a, b]) -> case (a, b) of
        (VInt   x, VInt y  ) -> return $ VInt (x + y)
        (VInt   x, VFloat y) -> return $ VFloat (fromInteger x + y)
        (VFloat x, VInt y  ) -> return $ VFloat (x + fromInteger y)
        (VFloat x, VFloat y) -> return $ VFloat (x + y)
        _                    -> throwError $ Custom "Expected Integer or Float"
    subV = Fn $ \([a, b]) -> case (a, b) of
        (VInt   x, VInt y  ) -> return $ VInt (x - y)
        (VInt   x, VFloat y) -> return $ VFloat (fromInteger x - y)
        (VFloat x, VInt y  ) -> return $ VFloat (x - fromInteger y)
        (VFloat x, VFloat y) -> return $ VFloat (x - y)
        _                    -> throwError $ Custom "Expected Integer or Float"
    divV = Fn $ \([a, b]) -> case (a, b) of
        (VInt   x, VInt y  ) -> return $ VFloat (fromInteger x / fromInteger y)
        (VInt   x, VFloat y) -> return $ VFloat (fromInteger x / y)
        (VFloat x, VInt y  ) -> return $ VFloat (x / fromInteger y)
        (VFloat x, VFloat y) -> return $ VFloat (x / y)
        _                    -> throwError $ Custom "Expected Integer or Float"
    multV = Fn $ \([a, b]) -> case (a, b) of
        (VInt   x, VInt y  ) -> return $ VInt (x * y)
        (VInt   x, VFloat y) -> return $ VFloat (fromInteger x * y)
        (VFloat x, VInt y  ) -> return $ VFloat (x * fromInteger y)
        (VFloat x, VFloat y) -> return $ VFloat (x * y)
        _                    -> throwError $ Custom "Expected Integer or Float"
    powV = Fn $ \([a, b]) -> case (a, b) of
        (VInt   x, VInt y  ) -> return $ VInt (x ^ y)
        (VInt   x, VFloat y) -> return $ VInt (x ^ round y)
        (VFloat x, VInt y  ) -> return $ VInt (round x ^ y)
        (VFloat x, VFloat y) -> return $ VInt (round x ^ round y)
        _                    -> throwError $ Custom "Expected Integer or Float"
    andV = Fn $ \([a, b]) -> return $ VBool (makeTruthy a && makeTruthy b)

    orV  = Fn $ \([a, b]) -> return $ VBool (makeTruthy a || makeTruthy b)

    negV = Fn $ \([x]) -> case x of
        VInt   x -> return $ VInt (negate x)
        VFloat x -> return $ VFloat (negate x)
        _        -> throwError $ Custom "Expected Integer or Float"

    notV   = Fn $ \([x]) -> return $ VBool (not $ makeTruthy x)

    printV = Fn $ \([x]) -> lift (print x) >> return VoidV
    strV   = Fn $ \([x]) -> return $ VString $ show x
    intV   = Fn $ \([x]) -> case x of
        y@(VInt _)  -> return y
        VFloat  y   -> return $ VInt $ round y
        VString str -> case readMaybe str of
            Just y -> return $ VInt y
            Nothing ->
                throwError
                    $  Custom
                    $  "Cannot convert "
                    <> str
                    <> " to an integer"

        _ ->
            throwError
                $  Custom
                $  "Cannot convert "
                <> show x
                <> " to an integer"
    boolV = Fn $ \([x]) -> return $ VBool $ makeTruthy x
