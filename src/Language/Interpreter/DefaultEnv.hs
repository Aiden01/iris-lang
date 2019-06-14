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
    , ("&&"   , andV)
    , ("||"   , orV)
    , ("neg"  , negV)
    , ("!"    , notV)
    , ("!="   , notEq)
    , ("=="   , eq)
    , ("print", printV)
    , ("str"  , strV)
    , ("int"  , intV)
    , ("bool" , boolV)
    , ("<"    , lowerV)
    , (">"    , greaterV)
    , ("++"   , concatV)
    , ("."    , attrV)
    , ("len"  , lenV)
    , ("input", input)
    ]
  where
    addV = Fn $ \([a, b]) -> case (a, b) of
        (VInt   x, VInt y  ) -> return $ (VInt (x + y))
        (VInt   x, VFloat y) -> return $ (VFloat (fromInteger x + y))
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
        (VInt   x, VFloat y) -> return $ VFloat (fromIntegral x ** y)
        (VFloat x, VInt y  ) -> return $ VFloat (x ** fromIntegral y)
        (VFloat x, VFloat y) -> return $ VFloat (x ** y)
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
    boolV  = Fn $ \([x]) -> return $ VBool $ makeTruthy x
    lowerV = Fn $ \([x, y]) -> case (x, y) of
        (VInt    a, VInt b   ) -> return $ VBool $ a < b
        (VFloat  a, VFloat b ) -> return $ VBool $ a < b
        (VString a, VString b) -> return $ VBool $ a < b
        (VChar   a, VChar b  ) -> return $ VBool $ a < b
        _                      -> throwError $ Custom "Mismatched types"
    greaterV = Fn $ \([x, y]) -> case (x, y) of
        (VInt    a, VInt b   ) -> return $ VBool $ a > b
        (VFloat  a, VFloat b ) -> return $ VBool $ a > b
        (VString a, VString b) -> return $ VBool $ a > b
        (VChar   a, VChar b  ) -> return $ VBool $ a > b
        _                      -> throwError $ Custom "Mismatched types"
    concatV = Fn $ \([x, y]) -> case (x, y) of
        (VString a, VString b) -> return $ VString (a <> b)
        (VList   a, VList b  ) -> return $ VList (a <> b)
        _                      -> throwError $ Custom "Mismatched types"
    attrV = Fn $ \([x, y]) -> case (x, y) of
        (VObject obj, VString attr) -> case M.lookup attr obj of
            Just val -> return val
            Nothing  -> throwError $ Custom $ "Object has to attribute " <> attr
        _ -> throwError $ Custom "Mismatched types"

    lenV = Fn $ \([x]) -> case x of
        VString str -> return $ VInt $ toInteger (length str)
        VList   xs  -> return $ VInt $ toInteger (length xs)
        _           -> throwError $ Custom "Mismatched types"
    input = Fn $ \([x]) -> case x of
        VString str -> do
            lift $ putStrLn str
            stdin <- lift getLine
            return $ VString stdin
        _ -> throwError $ Custom "Mismatched types"
    notEq = Fn $ \([x, y]) -> case (x, y) of
        (VInt    a, VInt b   ) -> return $ VBool $ a /= b
        (VInt    a, VFloat b ) -> return $ VBool $ (fromIntegral a) /= b
        (VFloat  a, VInt b   ) -> return $ VBool $ a /= (fromIntegral b)
        (VFloat  a, VFloat b ) -> return $ VBool $ a /= b
        (VString a, VString b) -> return $ VBool $ a /= b
        (VChar   a, VChar b  ) -> return $ VBool $ a /= b
        _                      -> throwError $ Custom "Mismatched types"
    eq = Fn $ \([x, y]) -> case (x, y) of
        (VInt    a, VInt b   ) -> return $ VBool $ a == b
        (VInt    a, VFloat b ) -> return $ VBool $ (fromIntegral a) == b
        (VFloat  a, VInt b   ) -> return $ VBool $ a == (fromIntegral b)
        (VFloat  a, VFloat b ) -> return $ VBool $ a == b
        (VString a, VString b) -> return $ VBool $ a == b
        (VChar   a, VChar b  ) -> return $ VBool $ a == b
        _                      -> throwError $ Custom "Mismatched types"


