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

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax


data Lang

lower :: HsModule -> Lang
lower = undefined

{-

data HsModule =
  HsModule _ _ _ _ [HsDecl]

data HsDecl = ...
    .
    HsTypeSig _ [HsName] HsQualType
    HsFunBin  [HsMatch]
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

-}

-- parseInterpreter = do
--   interpreter <- readFile location
--   let result = case parseModule interpreter of
--                  ParseOk a -> a
--                  ParseFailed srcloc s -> error $ show srcloc
--   let (HsModule _ _ _ importDecls allDecls) = result
--   let foo = prettyPrint result
--   let z   = join $ map (\a -> case a of
--                          HsFunBind matches -> matches
--                          _           -> []
--                        ) allDecls
--   let y   = filter (\a -> case a of
--                             (HsTypeSig _ [(HsIdent "loadi")] _) -> True
--                             _           -> False
--                        ) allDecls

--   putStrLn $ show $ filter (\r -> case r of
--                                     (HsMatch _ (HsIdent "loadi") _ _ _) -> True
--                                     _ -> False
--                                ) z
--   putStrLn $ show y
