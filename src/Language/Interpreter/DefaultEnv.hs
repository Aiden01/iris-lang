{-# LANGUAGE LambdaCase #-}
module Language.Interpreter.DefaultEnv
    ( defaultEnv
    )
where

import           Language.Interpreter.Types
import           Control.Monad.Except           ( ExceptT
                                                , throwError
                                                )
import           Control.Monad.Trans            ( lift )
import qualified Data.Map                      as M

defaultEnv :: Scope Value
defaultEnv = M.fromList
    [ ("+"    , addV)
    , ("-"    , subV)
    , ("/"    , divV)
    , ("*"    , multV)
    , ("and"  , andV)
    , ("&&"   , andV)
    , ("or"   , orV)
    , ("||"   , orV)
    , ("neg"  , negV)
    , ("!"    , notV)
    , ("print", printV)
    ]
  where
    addV = BFn $ \a b -> case (a, b) of
        (VInt   x, VInt y  ) -> return $ VInt (x + y)
        (VInt   x, VFloat y) -> return $ VFloat (fromInteger x + y)
        (VFloat x, VInt y  ) -> return $ VFloat (x + fromInteger y)
        (VFloat x, VFloat y) -> return $ VFloat (x + y)
        _                    -> throwError "Expected Integer or Float"
    subV = BFn $ \a b -> case (a, b) of
        (VInt   x, VInt y  ) -> return $ VInt (x - y)
        (VInt   x, VFloat y) -> return $ VFloat (fromInteger x - y)
        (VFloat x, VInt y  ) -> return $ VFloat (x - fromInteger y)
        (VFloat x, VFloat y) -> return $ VFloat (x - y)
        _                    -> throwError "Expected Integer or Float"
    divV = BFn $ \a b -> case (a, b) of
        (VInt   x, VInt y  ) -> return $ VFloat (fromInteger x / fromInteger y)
        (VInt   x, VFloat y) -> return $ VFloat (fromInteger x / y)
        (VFloat x, VInt y  ) -> return $ VFloat (x / fromInteger y)
        (VFloat x, VFloat y) -> return $ VFloat (x / y)
        _                    -> throwError "Expected Integer or Float"
    multV = BFn $ \a b -> case (a, b) of
        (VInt   x, VInt y  ) -> return $ VInt (x * y)
        (VInt   x, VFloat y) -> return $ VFloat (fromInteger x * y)
        (VFloat x, VInt y  ) -> return $ VFloat (x * fromInteger y)
        (VFloat x, VFloat y) -> return $ VFloat (x * y)
        _                    -> throwError "Expected Integer or Float"
    andV = BFn $ \a b -> case (a, b) of
        (VBool True , VBool True ) -> return $ VBool True
        (VBool False, VBool False) -> return $ VBool False
        (VBool False, VBool True ) -> return $ VBool False
        (VBool True , VBool False) -> return $ VBool False
        _                          -> throwError "Expected boolean"

    orV = BFn $ \a b -> case (a, b) of
        (VBool True , VBool True ) -> return $ VBool True
        (VBool False, VBool False) -> return $ VBool False
        (VBool False, VBool True ) -> return $ VBool True
        (VBool True , VBool False) -> return $ VBool True
        _                          -> throwError "Expected boolean"
    negV = Fn $ \x -> case x of
        VInt   x -> return $ VInt (negate x)
        VFloat x -> return $ VFloat (negate x)
        _        -> throwError "Expected Integer or Float"

    notV = Fn $ \x -> case x of
        VBool True  -> return $ VBool False
        VBool False -> return $ VBool True
        _           -> throwError "Expected boolean"

    printV = Fn $ \x -> lift (print x) >> return VoidV

