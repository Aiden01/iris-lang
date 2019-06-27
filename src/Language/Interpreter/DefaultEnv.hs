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
import qualified Data.Map                      as M
import           Text.Read                      ( readMaybe )
import           System.Random                  ( randomRIO
                                                , Random
                                                )
import           Control.Monad.IO.Class
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
    , ("len"  , lenV)
    -- , ("input"  , input)
    -- , ("randInt", randInt)
    ]
  where
    addV = Fn $ \([a, b]) -> case (a, b) of
        (VInt   x, VInt y  ) -> return $ (VInt (x + y))
        (VInt   x, VFloat y) -> return $ (VFloat (fromInteger x + y))
        (VFloat x, VInt y  ) -> return $ VFloat (x + fromInteger y)
        (VFloat x, VFloat y) -> return $ VFloat (x + y)
    subV = Fn $ \([a, b]) -> case (a, b) of
        (VInt   x, VInt y  ) -> return $ VInt (x - y)
        (VInt   x, VFloat y) -> return $ VFloat (fromInteger x - y)
        (VFloat x, VInt y  ) -> return $ VFloat (x - fromInteger y)
        (VFloat x, VFloat y) -> return $ VFloat (x - y)
    divV = Fn $ \([a, b]) -> case (a, b) of
        (VInt   x, VInt y  ) -> return $ VFloat (fromInteger x / fromInteger y)
        (VInt   x, VFloat y) -> return $ VFloat (fromInteger x / y)
        (VFloat x, VInt y  ) -> return $ VFloat (x / fromInteger y)
        (VFloat x, VFloat y) -> return $ VFloat (x / y)
    multV = Fn $ \([a, b]) -> case (a, b) of
        (VInt   x, VInt y  ) -> return $ VInt (x * y)
        (VInt   x, VFloat y) -> return $ VFloat (fromInteger x * y)
        (VFloat x, VInt y  ) -> return $ VFloat (x * fromInteger y)
        (VFloat x, VFloat y) -> return $ VFloat (x * y)
    powV = Fn $ \([a, b]) -> case (a, b) of
        (VInt   x, VInt y  ) -> return $ VInt (x ^ y)
        (VInt   x, VFloat y) -> return $ VFloat (fromIntegral x ** y)
        (VFloat x, VInt y  ) -> return $ VFloat (x ** fromIntegral y)
        (VFloat x, VFloat y) -> return $ VFloat (x ** y)
    andV = Fn $ \([a, b]) -> return $ VBool (makeTruthy a && makeTruthy b)

    orV  = Fn $ \([a, b]) -> return $ VBool (makeTruthy a || makeTruthy b)

    negV = Fn $ \([x]) -> case x of
        VInt   x -> return $ VInt (negate x)
        VFloat x -> return $ VFloat (negate x)

    notV   = Fn $ \([x]) -> return $ VBool (not $ makeTruthy x)

    printV = Fn $ \([x]) -> do
        liftIO $ print x
        pure VoidV
    strV = Fn $ \([x]) -> return $ VString $ show x
    intV = Fn $ \([x]) -> case x of
        y@(VInt _)  -> return y
        VFloat  y   -> return $ VInt $ round y
        VString str -> return $ VInt (read str)

    boolV  = Fn $ \([x]) -> return $ VBool $ makeTruthy x
    lowerV = Fn $ \([x, y]) -> case (x, y) of
        (VInt    a, VInt b   ) -> return $ VBool $ a < b
        (VFloat  a, VFloat b ) -> return $ VBool $ a < b
        (VString a, VString b) -> return $ VBool $ a < b
        (VChar   a, VChar b  ) -> return $ VBool $ a < b
    greaterV = Fn $ \([x, y]) -> case (x, y) of
        (VInt    a, VInt b   ) -> return $ VBool $ a > b
        (VFloat  a, VFloat b ) -> return $ VBool $ a > b
        (VString a, VString b) -> return $ VBool $ a > b
        (VChar   a, VChar b  ) -> return $ VBool $ a > b
    concatV = Fn $ \([x, y]) -> case (x, y) of
        (VString a, VString b) -> return $ VString (a <> b)
        (VList   a, VList b  ) -> return $ VList (a <> b)
    -- attrV = Fn $ \([x, y]) -> case (x, y) of
    --     (VObject obj, VString attr) -> case M.lookup attr obj of
    --         Just val -> return val
    --         Nothing  -> throwError $ Custom $ "Object has to attribute " <> attr
    --     _ -> throwError $ Custom "Mismatched types"

    lenV = Fn $ \([x]) -> case x of
        VString str -> return $ VInt $ toInteger (length str)
        VList   xs  -> return $ VInt $ toInteger (length xs)
    -- input = Fn $ \([x]) -> case x of
    --     VString str -> do
    --         lift $ putStrLn str
    --         stdin <- lift getLine
    --         return $ VString stdin
    notEq = Fn $ \([x, y]) -> case (x, y) of
        (VInt    a, VInt b   ) -> return $ VBool $ a /= b
        (VInt    a, VFloat b ) -> return $ VBool $ (fromIntegral a) /= b
        (VFloat  a, VInt b   ) -> return $ VBool $ a /= (fromIntegral b)
        (VFloat  a, VFloat b ) -> return $ VBool $ a /= b
        (VString a, VString b) -> return $ VBool $ a /= b
        (VChar   a, VChar b  ) -> return $ VBool $ a /= b
    eq = Fn $ \([x, y]) -> case (x, y) of
        (VInt    a, VInt b   ) -> return $ VBool $ a == b
        (VInt    a, VFloat b ) -> return $ VBool $ (fromIntegral a) == b
        (VFloat  a, VInt b   ) -> return $ VBool $ a == (fromIntegral b)
        (VFloat  a, VFloat b ) -> return $ VBool $ a == b
        (VString a, VString b) -> return $ VBool $ a == b
        (VChar   a, VChar b  ) -> return $ VBool $ a == b
    -- randInt = Fn $ \([x, y]) -> case (x, y) of
    --     (VInt   a, VInt b  ) -> mkRandom a b VInt
    --     (VFloat a, VFloat b) -> mkRandom a b VFloat

    -- mkRandom :: Random a => a -> a -> (a -> Value) -> EvalState Value
    -- mkRandom x y constr = lift (randomRIO (x, y)) >>= return . constr


