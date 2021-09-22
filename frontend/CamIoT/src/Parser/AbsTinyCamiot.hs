{-# LANGUAGE DeriveFunctor #-}
-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser.AbsTinyCamiot where

import Prelude (Char, Double, Integer, String, map, fmap)
import qualified Prelude as C (Eq, Ord, Show, Read, Functor)
import qualified Data.String

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype UIdent = UIdent String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

data Def a
    = DEquation a Ident [Pat a] (Exp a)
    | DTypeSig Ident Type
    | DDataDec UIdent [Ident] [ConstructorDec]
    | DMutRec [(Def a, [Def a])]
    --           ^       ^
    --           |    mutally recursive definitions
    --       type sig of a mutually recursive fun
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor)

-- instance C.Functor Def where
--     fmap f x = case x of
--         DEquation a ident pats exp -> DEquation (f a) ident (map (fmap f) pats) (fmap f exp)
--         DTypeSig ident type_ -> DTypeSig ident type_
--         DDataDec uident idents constructordecs -> DDataDec uident idents constructordecs
--         DMutRec defs -> DMutRec (fmap f defs)

data ConstructorDec = ConstDec UIdent Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Type
    = TLam Type Type
    | TVar Ident
    | TNil 
    | TAdt UIdent [Type]
    | TTup [Type]
    | TBool
    | TInt
    | TFloat
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Exp a
    = ECase a (Exp a) [PatMatch a]
    | ELet a (Pat a) (Exp a) (Exp a)
    | ELetR a (Pat a) (Exp a) (Exp a)
    | ELam a (Pat a) (Exp a)
    | EIf a (Exp a) (Exp a) (Exp a)
    | EApp a (Exp a) (Exp a)
    | EOr a (Exp a) (Exp a)
    | EAnd a (Exp a) (Exp a)
    | ERel a (Exp a) (RelOp a) (Exp a)
    | EAdd a (Exp a) (AddOp a) (Exp a)
    | EMul a (Exp a) (MulOp a) (Exp a)
    | ETup a [Exp a]
    | ENot a (Exp a)
    | EVar a Ident
    | EUVar a UIdent
    | EConst a Const
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor Exp where
    fmap f x = case x of
        ECase a exp patmatchs -> ECase (f a) (fmap f exp) (map (fmap f) patmatchs)
        ELet a pat exp1 exp2 -> ELet (f a) (fmap f pat) (fmap f exp1) (fmap f exp2)
        ELetR a pat exp1 exp2 -> ELetR (f a) (fmap f pat) (fmap f exp1) (fmap f exp2)
        ELam a pat exp -> ELam (f a) (fmap f pat) (fmap f exp)
        EIf a exp1 exp2 exp3 -> EIf (f a) (fmap f exp1) (fmap f exp2) (fmap f exp3)
        EApp a exp1 exp2 -> EApp (f a) (fmap f exp1) (fmap f exp2)
        EOr a exp1 exp2 -> EOr (f a) (fmap f exp1) (fmap f exp2)
        EAnd a exp1 exp2 -> EAnd (f a) (fmap f exp1) (fmap f exp2)
        ERel a exp1 relop exp2 -> ERel (f a) (fmap f exp1) (fmap f relop) (fmap f exp2)
        EAdd a exp1 addop exp2 -> EAdd (f a) (fmap f exp1) (fmap f addop) (fmap f exp2)
        EMul a exp1 mulop exp2 -> EMul (f a) (fmap f exp1) (fmap f mulop) (fmap f exp2)
        ETup a tupexps -> ETup (f a) (map (fmap f) tupexps)
        ENot a exp -> ENot (f a) (fmap f exp)
        EVar a ident -> EVar (f a) ident
        EUVar a uident -> EUVar (f a) uident
        EConst a const -> EConst (f a) const

data AddOp a = Plus a | Minus a
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor AddOp where
    fmap f x = case x of
        Plus a -> Plus (f a)
        Minus a -> Minus (f a)

data MulOp a = Times a | Div a | MulOpTyped a (MulOp a) Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor MulOp where
    fmap f x = case x of
        Times a -> Times (f a)
        Div a -> Div (f a)

data RelOp a
    = LTC a
    | LEC a
    | GTC a
    | GEC a
    | EQC a
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor RelOp where
    fmap f x = case x of
        LTC a -> LTC (f a)
        LEC a -> LEC (f a)
        GTC a -> GTC (f a)
        GEC a -> GEC (f a)
        EQC a -> EQC (f a)

data Const
    = CInt Integer | CFloat Double | CTrue | CFalse | CNil
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Pat a
    = PConst a Const
    | PVar a Ident
    | PZAdt a UIdent
    | PNAdt a UIdent [Pat a]
    | PWild a
    | PNil a
    | PTup a [Pat a]
    | PLay a Ident (Pat a)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor Pat where
    fmap f x = case x of
        PConst a const -> PConst (f a) const
        PVar a ident -> PVar (f a) ident
        PZAdt a uident -> PZAdt (f a) uident
        PNAdt a uident adtpats -> PNAdt (f a) uident (map (fmap f) adtpats)
        PWild a -> PWild (f a)
        PNil a -> PNil (f a)
        PTup a tuppats -> PTup (f a) (map (fmap f) tuppats)
        PLay a ident pat -> PLay (f a) ident (fmap f pat)

data PatMatch a = PM (Pat a) (Exp a)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor PatMatch where
    fmap f x = case x of
        PM pat exp -> PM (fmap f pat) (fmap f exp)

