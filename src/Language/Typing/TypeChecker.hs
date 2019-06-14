module Language.Typing.TypeChecker
    ()
where

import           Language.Parser.AST            ( Lit(..) )
import           Language.Typing.Types


litType :: Lit -> Type
litType (Number  _) = TInt
litType (Str     _) = TString
litType (Boolean _) = TBool
litType (Char'   _) = TChar
litType (Float   _) = TFloat
litType Void        = VoidT
