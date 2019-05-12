module Language.Transpiler
    ()
where

import           Language.JavaScript.Parser
import           Language.JavaScript.Parser.AST
import           Language.Parser.AST
import           Data.Maybe                     ( fromMaybe )

transpileProgram :: Program -> JSAST
transpileProgram (Program topLevelStatements) = JSAstProgram
    (map transpileTopLevelStatements topLevelStatements)
    JSAnnotSpace

transpileTopLevelStatements :: TopLevelStatement -> JSStatement
transpileTopLevelStatements (FunctionDeclaration name params block) =
    JSFunction JSAnnotSpace
               (JSIdentName JSAnnotSpace name)
               JSAnnotSpace
               (transpileParams params)
               JSAnnotSpace
               (JSBlock JSAnnotSpace (transpileBlock block) JSAnnotSpace)
               JSSemiAuto
  where
    transpileParams :: [String] -> JSCommaList String
    transpileParams params =
        let (p : ps) = reverse params
        in  JSLCons (transpileParams ps) JSNoAnnot p
    transpileBlock :: Maybe Program -> [JSStatement]
    transpileBlock Nothing = []
    transpileBlock (Just (Program stmts)) =
        map transpileTopLevelStatements stmts

