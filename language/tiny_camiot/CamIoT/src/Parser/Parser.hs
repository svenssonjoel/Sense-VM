{-# LANGUAGE OverloadedStrings #-}
module Parser.Parser (pProgram, Parser) where

import Prelude hiding (unlines)
import Parser.AbsTinyCamiot

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec.Debug

import Control.Applicative hiding (many, some, Const)
import Control.Monad.Combinators.Expr
import Control.Monad.Identity
import Data.Void
import Data.Text (Text, unpack, unlines, intercalate)

type Parser a = Parsec Void Text a

-- interesting points
  -- * how to make the parser work by the proper precendense
  -- * left factoring
  -- * handling keywords
  -- * parse by tokens, whitespaces are ignored
  --   how does this affect line sensitive parsing, such as datatype declarations?
  -- * _what_ are closed expressions? (Either single tokens or other
  --   stuff that is enclosed in both opening and closing tokens, whatever
  --   they may be)
  -- * We have to preprocess the file to rid the developer of nasty clutter!

-- parse programs
pProgram :: Parser [Def ()]
pProgram = many $ pDataDec <|> try pTypeSignature <|> pEquation

-- parse types
pClosed :: Parser Type
pClosed = choice [ TInt    <$ pSymbol "Int"
                 , TBool   <$ pSymbol "Bool"
                 , TFloat  <$ pSymbol "Float"
                 , TVar    <$> pIdent
                 , do pChar '('
                      ts <- sepBy pFun (pChar ',') <* pChar ')'
                      case ts of
                          []  -> pure TNil
                          [t] -> pure t
                          _   -> pure (TTup ts)
                 ]

pApp :: Parser Type
pApp = choice [TAdt <$> pUIdent <*> many pClosed, pClosed]

pFun :: Parser Type
pFun = foldr1 TLam <$> sepBy1 pApp (pSymbol "->")

pType :: Parser Type
pType = pSpace *> pFun

-- parse expressions

pExpClosed :: Parser (Exp ())
pExpClosed = choice [ EConst () <$> pConst
                    , EVar   () <$> pIdent
                    , EUVar  () <$> pUIdent
                    , do pChar '('
                         es <- sepBy1 pExpVerbose (pChar ',') <* pChar ')'
                         case es of
                             []  -> undefined -- should not end up here
                             [e] -> pure e
                             _   -> pure (ETup () es)]

pExpApp :: Parser (Exp ())
pExpApp = foldl1 (EApp ()) <$> some pExpClosed

pExpNot :: Parser (Exp ())
pExpNot = ENot () <$> (pChar '!' *> pExpApp) 
      <|> pExpApp

pExpMul :: Parser (Exp ())
pExpMul = pExpNot >>= go where
  go e1 =
        (pChar '*' >> (EMul () e1 (Times ()) <$> pExpNot) >>= go)
    <|> (pChar '/' >> (EMul () e1 (Div ())   <$> pExpNot) >>= go)
    <|> pure e1

pExpAdd :: Parser (Exp ())
pExpAdd = pExpMul >>= go where
  go e1 =
        (pChar '+' >> (EAdd () e1 (Plus ())  <$> pExpMul) >>= go)
    <|> (pChar '-' >> (EAdd () e1 (Minus ()) <$> pExpMul) >>= go)
    <|> pure e1

pExpRel :: Parser (Exp ())
pExpRel = pExpAdd >>= go where
    go e1 =
           (pChar '<' >> ((pChar '=' >> ERel () e1 (LEC ()) <$> pExpAdd)
                         <|>
                         (ERel () e1 (LTC ()) <$> pExpAdd)))
       <|> (pChar '>' >> ((pChar '=' >> ERel () e1 (GEC ()) <$> pExpAdd)
                         <|>
                         (ERel () e1 (GTC ()) <$> pExpAdd)))
       <|> (pSymbol "==" >> (ERel () e1 (EQC ()) <$> pExpAdd))
       <|> pure e1

pExpAnd :: Parser (Exp ())
pExpAnd = foldr1 (EAnd ()) <$> sepBy1 pExpRel (pSymbol "&&")

pExpOr :: Parser (Exp ())
pExpOr = foldr1 (EOr ()) <$> sepBy1 pExpAnd (pSymbol "||")

pExpVerbose :: Parser (Exp ())
pExpVerbose = choice [
    do pSymbol "let"
       p <- pPat False False
       pSymbol "="
       e1 <- pExpVerbose
       pSymbol "in"
       ELet () p e1 <$> pExpVerbose
  , do pChar '\\'
       p <- pPat False False
       pSymbol "->"
       ELam () p <$> pExpVerbose
  , do pSymbol "if"
       e1 <- pExpVerbose
       pSymbol "then"
       e2 <- pExpVerbose
       pSymbol "else"
       EIf () e1 e2 <$> pExpVerbose
  , do pSymbol "case"
       e <- pExpVerbose
       pSymbol "of"
       pChar '{'
       branches <- sepBy1 (do
           pat <- pPat True True
           pSymbol "->"
           PM () pat <$> pExpVerbose) (pChar ';')
       pChar '}'
       return $ ECase () e branches
  , pExpOr]

pExp :: Parser (Exp ())
pExp = pSpace *> pExpVerbose

-- parse definitions
  -- parse data type declarations

pDataDec :: Parser (Def ())
pDataDec = do
    pSymbol "data"
    tycon <- pUIdent
    variables <- many pIdent
    pSymbol "where"
    pChar '{'
    constructors <- sepBy (do
      con <- pUIdent
      pChar ':'
      ConstDec () con <$> pType  
      ) (pSymbol ";")
    pChar '}'
    pChar ';'
    return $ DDataDec () tycon variables constructors

  -- parse type signatures
pTypeSignature :: Parser (Def ())
pTypeSignature = do
    name <- pIdent
    pChar ':'
    t <- pType
    pChar ';'
    return $ DTypeSig () name t

  -- parse function clauses
pEquation :: Parser (Def ())
pEquation = do
    name <- pIdent
    patterns <- many (pPat True False)
    pSymbol "="
    exp <- pExp
    pChar ';'
    return $ DEquation () name patterns exp

-- parse patterns

pPatClosed :: Bool -> Bool -> Parser (Pat ())
pPatClosed allowConstants allowNAry = choice $ maybe ++ always
  where maybe  = [PConst () <$> pConst | allowConstants]
        always = [ PVar  () <$> pIdent
                 , PWild () <$ pChar '_'
                 , do pChar '('
                      ps <- sepBy1 (pPatAs allowConstants allowNAry) (pChar ',') <* pChar ')'
                      case ps of
                        [p] -> pure p
                        _   -> pure (PTup () ps)]

--pPatApp :: Bool -> Parser (Pat ())
--pPatApp allowConstants = choice [ do
--  con <- pUIdent
--  vars <- many (pPatClosed allowConstants)
--  case vars of
--    [] -> return $ PZAdt () con
--    _  -> return $ PNAdt () con (map (PAdtPat ()) vars)
--  ,
--
--  pPatClosed allowConstants
--  ]

pPatApp :: Bool -> Bool -> Parser (Pat ())
pPatApp allowConstants allowNAry = choice $ adt' ++ [pPatClosed allowConstants allowNAry]
  where adt' = if allowNAry
                then [adt]
                else [try (pChar '(' >> adt <* pChar ')'), PZAdt () <$> pUIdent]
        adt = do con <- pUIdent
                 vars <- many (pPatClosed allowConstants False)
                 case vars of
                   [] -> return $ PZAdt () con
                   _ -> return $ PNAdt () con vars

--pPatApp allowConstants = choice [ PAdt () <$> pUIdent <*> many (pPatClosed allowConstants)
--                                , pPatClosed allowConstants]

pPatAs :: Bool -> Bool -> Parser (Pat ())
pPatAs allowConstants allowNAry = choice [try $ do
    x <- pIdent
    pSymbol "as"
    p <- pPatApp allowConstants False
    return $ PLay () x p,

    pPatApp allowConstants allowNAry]

pPat :: Bool -> Bool -> Parser (Pat ())
pPat allowConstants allowNAry = pSpace *> pPatAs allowConstants allowNAry

-- parse constants

pConst :: Parser (Const ())
pConst = choice [
    try $ CFloat () <$> Lexer.lexeme pSpace Lexer.float
  , CInt   ()       <$> Lexer.lexeme pSpace Lexer.decimal
  , CTrue  ()       <$  pSymbol "True"
  , CFalse ()       <$  pSymbol "False"
  , CNil   ()       <$  pSymbol "()" 
  ]

-- parser utilities

parens :: Parser a -> Parser a
parens p = label "parse a type wrapped in parentheses" $ do
    pSymbol "("
    a <- p
    pSymbol ")"
    return a

pUIdent :: Parser UIdent
pUIdent = try $ do
    a <- upperChar
    rest <- many $ choice [letterChar, digitChar, char '_']
    pSpace
    let x = a:rest
    if x `elem` keywords
        then fail "expected an ADT name"
        else pure $ UIdent x

pIdent :: Parser Ident
pIdent = try $ do
    a <- lowerChar
    rest <- many $ choice [letterChar, digitChar, char '_']
    trailings <- many (char '\'')
    pSpace
    let x = a:(rest++trailings)
    if x `elem` keywords
        then fail "found keyword, expected identifier"
        else return $ Ident x

pSymbol :: Text -> Parser Text
pSymbol = Lexer.symbol pSpace

pChar :: Char -> Parser ()
pChar c = void (char c <* pSpace)

pSpace :: Parser ()
pSpace = Lexer.space 
           (void spaceChar) 
           (Lexer.skipLineComment "--") 
           (Lexer.skipBlockComment "{-" "-}")

keywords :: [String]
keywords = [
  -- types
    "Bool"
  , "Int"
  , "Float"
  
  -- constants
  , "True"
  , "False"
  
  -- misc
  , "data"
  , "where"
  , "case"
  , "of"
  , "let"
  , "in"
  , "if"
  , "then"
  , "else"]