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

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module Bytecode.GeneratorTH where

import Control.Monad.State.Strict
import Data.List(find, intercalate)
import Language.Haskell.TH
import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Syntax

type Var = String

data ScopedVar = LocalVar Var
               | GlobalVar Var
               deriving (Ord, Show, Eq)

data Expr = Var Var
          | LitInt Int
          | LitBool Bool
          | Let Var Expr
          | Call Expr [Expr]
          | Seq Expr Expr
          | StructIndex Expr Int
          deriving (Ord,Eq)

instance Show Expr where
  show (Var v) = v
  show (LitInt i) = show i
  show (LitBool b) = show b
  show (Let v e) =  "let " <> v <> " = " <> show e
  show (Call e es) = show e <> "(" <> intercalate ", " (map show es) <> ")"
  show (Seq e1 e2) = show e1 <> ";\n" <> show e2
  show (StructIndex e i) = show e <> " -> " <> show i

type Scope = [ScopedVar]

newtype Lower a =
  Lower
    { runLower :: State Scope a
    }
  deriving (Functor, Applicative, Monad, MonadState Scope)


lower :: DExp -> Expr
lower dexp = evalState (runLower (lowerExp dexp)) globals
  where
    globals = [] -- needs to be calculated in one pass


lowerExp :: DExp -> Lower Expr
lowerExp (Varname n _) = pure $! Var n -- qualified names and a lot of other cases not handled
lowerExp (DLitE l) = pure $! lowerLit l
lowerExp app@(DAppE e1 e2) =
  if isSequence e1
  then
    case e1 of
      (Bind e _) -> do
        let (var, body) = splitLambda e2
        e' <- if isVar e
              then do
               f <- lowerExp e
               pure $! Call f []
              else lowerExp e
        extendScope var
        body' <- lowerExp body
        pure $! Seq (Let var e') body'
      (BindSeq e _) -> do
        e' <- if isVar e
              then do
               f <- lowerExp e
               pure $! Call f []
              else lowerExp e
        e2' <- lowerExp e2
        pure $! Seq e' e2'
      _ -> error "Invalid sequence operation"
  else do
    let (f, args) = unfoldApp app
    f'    <- lowerExp f
    args' <- traverse lowerExp args
    pure $! Call f' args'
lowerExp (DCaseE exp matches) =
  if isPatternMatchOnTuples matches
  then do
    let (DMatch pat e) = head matches -- guaranteed to have one element here
    exprs <- splitTuple pat exp
    e' <- lowerExp e
    pure $! buildExpr exprs e'
  else error "Yet to handle pattern Match on constructors"
  where
    buildExpr exprs rest
      | length exprs == 0 = rest
      | length exprs == 1 = Seq (head exprs) rest
      | otherwise = Seq (head exprs) (buildExpr (tail exprs) rest)

lowerExp e = error $ "Yet to handle " <> show e


lowerLit :: Lit -> Expr
lowerLit (IntegerL i) = LitInt $ fromInteger i
lowerLit (IntPrimL i) = LitInt $ fromInteger i



splitTuple :: DPat -> DExp -> Lower [Expr]
splitTuple (Tuple1 _ f s _) e = do
  e' <- lowerExp e
  let PatVarName n1 _ = f
  let PatVarName n2 _ = s
  pure $! [ Let n1 (StructIndex e' 0)
          , Let n2 (StructIndex e' 1)
          ]
splitTuple (Tuple2 _ f s t _) e = do
  e' <- lowerExp e
  let PatVarName n1 _ = f
  let PatVarName n2 _ = s
  let PatVarName n3 _ = t
  pure $! [ Let n1 (StructIndex e' 0)
          , Let n2 (StructIndex e' 1)
          , Let n3 (StructIndex e' 2)
          ]
splitTuple (Tuple3 _ f s t fr _) e = do
  e' <- lowerExp e
  let PatVarName n1 _ = f
  let PatVarName n2 _ = s
  let PatVarName n3 _ = t
  let PatVarName n4 _ = fr
  pure $! [ Let n1 (StructIndex e' 0)
          , Let n2 (StructIndex e' 1)
          , Let n3 (StructIndex e' 2)
          , Let n4 (StructIndex e' 3)
          ]
splitTuple (Tuple4 _ f s t fr fv _) e = do
  e' <- lowerExp e
  let PatVarName n1 _ = f
  let PatVarName n2 _ = s
  let PatVarName n3 _ = t
  let PatVarName n4 _ = fr
  let PatVarName n5 _ = fv
  pure $! [ Let n1 (StructIndex e' 0)
          , Let n2 (StructIndex e' 1)
          , Let n3 (StructIndex e' 2)
          , Let n4 (StructIndex e' 3)
          , Let n5 (StructIndex e' 4)
          ]
splitTuple (Tuple5 _ f s t fr fv sx _) e = do
  e' <- lowerExp e
  let PatVarName n1 _ = f
  let PatVarName n2 _ = s
  let PatVarName n3 _ = t
  let PatVarName n4 _ = fr
  let PatVarName n5 _ = fv
  let PatVarName n6 _ = sx
  pure $! [ Let n1 (StructIndex e' 0)
          , Let n2 (StructIndex e' 1)
          , Let n3 (StructIndex e' 2)
          , Let n4 (StructIndex e' 3)
          , Let n5 (StructIndex e' 4)
          , Let n6 (StructIndex e' 5)
          ]
splitTuple t _ = error $ "Tuple " <> show t <> "not handled"

pattern PatVarName name nf =
  DVarPa (Name (OccName name) nf)

-- XXX:GHC supports tuples with 62 constructors
-- Need to support all of them
pattern Tuple1 modname v1 v2 xs =
  DConPa (Name (OccName "(,)") modname) (v1:v2:xs)

pattern Tuple2 modname v1 v2 v3 xs =
  DConPa (Name (OccName "(,,)") modname) (v1:v2:v3:xs)

pattern Tuple3 modname v1 v2 v3 v4 xs =
  DConPa (Name (OccName "(,,,)") modname) (v1:v2:v3:v4:xs)

pattern Tuple4 modname v1 v2 v3 v4 v5 xs =
  DConPa (Name (OccName "(,,,,)") modname) (v1:v2:v3:v4:v5:xs)

pattern Tuple5 modname v1 v2 v3 v4 v5 v6 xs =
  DConPa (Name (OccName "(,,,,,)") modname) (v1:v2:v3:v4:v5:v6:xs)

isSequence :: DExp -> Bool
isSequence (Bind _ _) = True
isSequence (BindSeq _ _) = True
isSequence _ = False

pattern Bind e modname =
  DAppE (DVarE (Name (OccName ">>=") modname)) e

pattern BindSeq e modname =
  DAppE (DVarE (Name (OccName ">>") modname)) e

pattern Varname n q = DVarE (Name (OccName n) q)

-- XXX: An extremely partial function assuming this is only
-- called from the application of a bind operation
splitLambda :: DExp -> (String, DExp)
splitLambda (DLamE names e) = (s, e)
  where
    (n:_) = names
    (Name (OccName s) _) = n
splitLambda _ = error "Sequencing an incorrect operation"

extendScope :: Var -> Lower ()
extendScope v = modify $ \s -> LocalVar v : s

isVar :: DExp -> Bool
isVar (DVarE _) = True
isVar _ = False

isGlobal :: ScopedVar -> Bool
isGlobal (GlobalVar _) = True
isGlobal (LocalVar _)  = False

isLocal :: ScopedVar -> Bool
isLocal = not . isGlobal

unfoldApp :: DExp -> (DExp, [DExp])
unfoldApp = go []
  where
    go args (DAppE fun arg) = go (arg:args) fun
    go args fun = (fun, args)

isPatternMatchOnTuples :: [DMatch] -> Bool
isPatternMatchOnTuples matches
  | length matches == 1 =
    case head matches of
      DMatch (DConPa (Name _ TupleModName) _) _ -> True
      _ -> False
  | otherwise = False


pattern TupleModName =
  NameG DataName (PkgName "ghc-prim") (ModName "GHC.Tuple")

generateC :: Q [Dec] -> Q [Dec]
generateC x = do
  x' <- x
  foo <- dsDecs x' :: Q [DDec]
  let (DLetDec (DValD _ exp)) = foo !! 3
  runIO $ do
    putStrLn "\nGenerating C now :"
    -- putStrLn $ show $ exp
    putStrLn $ show $ lower exp -- $ pprint x' ++ "\n"
    putStrLn "\n"
  return x'
