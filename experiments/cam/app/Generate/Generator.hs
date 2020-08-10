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
