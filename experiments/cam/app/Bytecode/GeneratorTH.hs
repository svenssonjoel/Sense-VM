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
type Tag = String

data ScopedVar = LocalVar Var
               | GlobalVar Var
               deriving (Ord, Show, Eq)

data Expr = Var Var
          | LitInt Int
          | LitBool Bool
          | LitString String
          | Let Var Expr
          | Cond Cond
          | Call Expr [Expr]
          | Seq Expr Expr
          | StructIndex Expr Int -- extract nth index of `Expr` where
                                 -- `Expr` is guaranteed to be struct
          | TypeOf Expr Tag -- used for pattern matching on constructors
                            -- in the generated C the relevant union type
                            -- should have a typeof function
          deriving (Ord,Eq)

instance Show Expr where
  show (Var v) = v
  show (LitInt i) = show i
  show (LitBool b) = show b
  show (LitString s) = show s
  show (Let v e) =  "let " <> v <> " = " <> show e
  show (Cond c)  = show c
  show (Call e es) = show e <> "(" <> intercalate ", " (map show es) <> ")"
  show (Seq e1 e2) = show e1 <> ";\n" <> show e2
  show (StructIndex e i) = show e <> " -> " <> show i
  show (TypeOf e t) = "typeof " <> show e <> " is " <> show t



-- conditional expressions expressed in a way which
-- makes `else` branch optional and not compulsory
-- eg:
-- (IfThen (TypeOf (Var "e") "VPair") (LitInt 5)) `Else` (LitInt 3)
data Cond = IfThen Expr Expr
          | Else Cond Expr
          deriving (Ord, Eq)

instance Show Cond where
  show (IfThen e1 e2) = "if "   <> show e1 <> " {\n" <>
                        show e2 <> "\n"   <>
                        "}"
  show (Else e1 e2) =  show e1 <> " else { \n"    <>
                       show e2 <> "\n" <>
                       "}"

type Scope = [ScopedVar]

data LowerState =
  LowerState { scope :: Scope
             , count :: Int
             }

newtype Lower a =
  Lower
    { runLower :: State LowerState a
    }
  deriving (Functor, Applicative, Monad, MonadState LowerState)


lower :: DExp -> Expr
lower dexp = evalState (runLower (lowerExp dexp)) globals
  where
    globals = LowerState {scope = []
                         , count=  0} -- needs to be calculated in one pass


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
  else patternMatchCon exp matches
  where
    buildExpr exprs rest
      | length exprs == 0 = rest
      | length exprs == 1 = Seq (head exprs) rest
      | otherwise = Seq (head exprs) (buildExpr (tail exprs) rest)

lowerExp e = error $ "Yet to handle " <> show e


patternMatchCon :: DExp -> [DMatch] -> Lower Expr
patternMatchCon e allMatches = do
  e' <- lowerExp e
  case (last allMatches) of
    (DMatch DWildPa exp) -> do
      tagExprs <- mapM (patternMatchBranch e) (init allMatches)
      let (Cond matches) = builder e' tagExprs
      exp' <- lowerExp exp
      pure $! Cond $ Else matches exp'
    _ -> do
      tagExprs <- mapM (patternMatchBranch e) allMatches
      pure $! builder e' tagExprs
  where
    builder v [] = error "Impossible case for recursion"
    builder v [(tag, expr)] = Cond $ IfThen (TypeOf v tag) expr
    builder v ((tag,expr):ms) = Seq (Cond $ IfThen (TypeOf v tag) expr) (builder v ms)


patternMatchBranch :: DExp -> DMatch -> Lower (Tag, Expr)
patternMatchBranch e (DMatch (DConPa (Name (OccName s) _) pats) exp) = do
  e' <- lowerExp e
  pExp <- patternHandler e' (zip pats [0..]) exp
  pure $ (s, pExp)
patternMatchBranch _ m =
  error $! "Pattern match on single constructor \
           \ called for wrong clause " <> show m

patternHandler :: Expr -> [(DPat, Int)] -> DExp -> Lower Expr
patternHandler _ [] tailExpr = do
  tailExpr' <- lowerExp tailExpr
  pure tailExpr'
patternHandler e ((p1,i):ps) tailExpr =
  case p1 of
    DVarPa (Name (OccName n) _) -> do
      rest <- patternHandler e ps tailExpr
      pure $! Seq (Let n (StructIndex e i)) rest
    DConPa (Name (OccName t) _) pats -> do
      n <- newVarName
      rest <- patternHandler (Var n) ((zip pats [0..]) ++ ps) tailExpr
      pure $! (Let n (StructIndex e i)) `Seq`
              (Cond $ IfThen (TypeOf (Var n) t) rest)
    DWildPa -> patternHandler e ps tailExpr
    DLitPa _ -> error "Literal patterns not supported"
    p -> error $! "Does not handle pattern " <> show p


newVarName :: Lower String
newVarName = do
  i <- gets count
  modify $ \s -> s {count = 1 + i}
  return $ "temp_" <> (show i)

lowerLit :: Lit -> Expr
lowerLit (IntegerL i) = LitInt $ fromInteger i
lowerLit (IntPrimL i) = LitInt $ fromInteger i
lowerLit (StringL  s) = LitString s


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
extendScope v = do
  sc <- gets scope
  modify $ \s -> s {scope = LocalVar v : sc}

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
  let (DLetDec (DValD _ exp)) = foo !! 7
  let (DLetDec (DFunD _ clauses)) = foo !! 9
  let (DClause _ exp') = clauses !! 0
  runIO $ do
    putStrLn "\nGenerating C now :"
    -- putStrLn $ show exp
    putStrLn $ show $ lower exp -- $ pprint x' ++ "\n"
    putStrLn "\n"
  return x'
