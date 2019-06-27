module Language.Typing.DefaultEnvType
    ( defaultEnvType
    )
where

import           Language.Parser.AST
import           Language.Typing.Types
import           Data.Map                       ( fromList )

defaultEnvType :: TypeEnv
defaultEnvType = fromList [("print", print), ("str", str)]
  where
    print = Fn [TAny] VoidT
    str   = Fn [TAny] TString
