-- MIT License

-- Copyright (c) 2020 Abhiroop Sarkar

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.


module Generate.Generator where

import Control.Monad

import Language.C99.Simple.AST

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax


translExpr :: HsExp -> Expr
translExpr (HsVar qname) = Ident $ translQName qname
translExpr (HsCon qname) = Ident ""
translExpr (HsLit lit)   = translLit lit
translExpr (HsInfixApp e1 qop e2) =
  infixapp qop [translExpr e1, translExpr e2]
translExpr (HsApp e1 e2) =
  Funcall (translExpr e1) [translExpr e2]
translExpr (HsNegApp _) = unsupported "Negation application"

infixapp :: HsQOp -> [Expr] -> Expr
infixapp (HsQVarOp (UnQual (HsSymbol "+"))) (e1:e2:_) =
  BinaryOp Add e1 e2


translLit :: HsLiteral -> Expr
translLit (HsChar char) = Ident [char]
translLit (HsString string) = Ident string
translLit (HsInt i) = LitInt i
translLit (HsFrac r) = LitDouble $ fromRational r
translLit (HsCharPrim c) = Ident [c]
translLit (HsStringPrim s) = Ident s
translLit (HsIntPrim i) = LitInt i
translLit (HsFloatPrim r) = LitFloat $ fromRational r
translLit (HsDoublePrim r) = LitDouble $ fromRational r

translQName :: HsQName -> Ident
translQName (Qual _ _) = unsupported "Qualified names"
translQName (UnQual hsname) = translHsName hsname
translQName (Special special) = translSpecial special

translHsName :: HsName -> Ident
translHsName (HsIdent s) = s
translHsName (HsSymbol s) = unsupported "Symbols for variable names"

translSpecial :: HsSpecialCon -> Ident
translSpecial HsUnitCon  = "()"
translSpecial _ = unsupported "Other special constructors"

unsupported :: String -> a
unsupported x = error $ x <> " not supported"






{-

data HsModule =
  HsModule _ _ _ _ [HsDecl]

data HsDecl = ...
    .
    HsTypeSig _ [HsName] HsQualType
    HsFunBind  [HsMatch]             -- functions with args
    HsPatBind _ HsPat HsRhs [HsDecl] -- functions with 0 args
    .
    .

data HsMatch =
  HsMatch SrcLoc
    (HsIdent <function name>)
    [HsPat] -- arguments; HsPat is extremely rich we need to disqualify it
     HsRhs   -- the function body
    [HsDecl] -- not sure what this is doing

data HsRhs =
    HsUnGuardedRhs HsExp
  | HsGuardedRhss  [HsGuardedRhs] -- maybe we should ban guards to reduce complexity

data HsExp
        = HsVar HsQName                 -- ^ variable
        | HsCon HsQName                 -- ^ data constructor
        | HsLit HsLiteral               -- ^ literal constant
        | HsInfixApp HsExp HsQOp HsExp  -- ^ infix application
        | HsApp HsExp HsExp             -- ^ ordinary application
        | HsNegApp HsExp                -- ^ negation expression @-@ /exp/
        | HsLambda SrcLoc [HsPat] HsExp -- ^ lambda expression
        | HsLet [HsDecl] HsExp          -- ^ local declarations with @let@
        | HsIf HsExp HsExp HsExp        -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
        | HsCase HsExp [HsAlt]          -- ^ @case@ /exp/ @of@ /alts/
        | HsDo [HsStmt]                 -- ^ @do@-expression:
                                        -- the last statement in the list
                                        -- should be an expression.
        | HsTuple [HsExp]               -- ^ tuple expression
        | HsList [HsExp]                -- ^ list expression
        | HsParen HsExp                 -- ^ parenthesized expression
        | HsLeftSection HsExp HsQOp     -- ^ left section @(@/exp/ /qop/@)@
        | HsRightSection HsQOp HsExp    -- ^ right section @(@/qop/ /exp/@)@
        | HsRecConstr HsQName [HsFieldUpdate]
                                        -- ^ record construction expression
        | HsRecUpdate HsExp [HsFieldUpdate]
                                        -- ^ record update expression
        | HsEnumFrom HsExp              -- ^ unbounded arithmetic sequence,
                                        -- incrementing by 1
        | HsEnumFromTo HsExp HsExp      -- ^ bounded arithmetic sequence,
                                        -- incrementing by 1
        | HsEnumFromThen HsExp HsExp    -- ^ unbounded arithmetic sequence,
                                        -- with first two elements given
        | HsEnumFromThenTo HsExp HsExp HsExp
                                        -- ^ bounded arithmetic sequence,
                                        -- with first two elements given
        | HsListComp HsExp [HsStmt]     -- ^ list comprehension
        | HsExpTypeSig SrcLoc HsExp HsQualType
                                        -- ^ expression type signature
        | HsAsPat HsName HsExp          -- ^ patterns only
        | HsWildCard                    -- ^ patterns only
        | HsIrrPat HsExp                -- ^ patterns only


From do expression
data HsStmt
        = HsGenerator SrcLoc HsPat HsExp
                                -- ^ a generator /pat/ @<-@ /exp/
        | HsQualifier HsExp     -- ^ an /exp/ by itself: in a @do@-expression,
                                -- an action whose result is discarded;
                                -- in a list comprehension, a guard expression
        | HsLetStmt [HsDecl]    -- ^ local bindings


data HsAlt =
  HsAlt SrcLoc HsPat HsGuardedAlts [HsDecl]
-}

location = "/Users/abhiroopsarkar/C/Sense-VM/experiments/cam/app/Bytecode/InterpreterModel.hs"

parseInterpreter = do
  interpreter <- readFile location
  let result = case parseModule interpreter of
                 ParseOk a -> a
                 ParseFailed srcloc s -> error $ show srcloc
  let (HsModule _ _ _ importDecls allDecls) = result
  let foo = prettyPrint result
  let z   = join $ map (\a -> case a of
                         HsFunBind matches -> matches
                         _           -> []
                       ) allDecls
  let r   = filter (\a -> case a of
                            HsPatBind _ (HsPVar (HsIdent "push")) _ _ -> True
                            _           -> False
                   ) allDecls

  let y   = filter (\a -> case a of
                            (HsTypeSig _ [(HsIdent "push")] _) -> True
                            _           -> False
                       ) allDecls

  -- putStrLn $ show $ filter (\r -> case r of
  --                                   (HsMatch _ (HsIdent "push") _ _ _) -> True
  --                                   _ -> False
  --                              ) z
  putStrLn $ show y
  print r
