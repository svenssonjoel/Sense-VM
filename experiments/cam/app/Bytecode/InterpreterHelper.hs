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

{-# LANGUAGE MultiParamTypeClasses #-}
module Bytecode.InterpreterHelper where

import CAM
import GHC.Arr
import Control.Monad.State.Strict(MonadState, StateT (..), State,
                                  get, put)

{-
NOTE: This is not the actual "byte"code interpreter.
This takes the Haskell `Instruction` datatype and interprets that.
This module would be primarily used for experimentation with VM designs

NOTE 2: Efficiency of the data structures like symbol table
doesn't matter because we will generate more efficient C types
-}

type Stack = [Val]

type Environment = Val -- the environment register

type Index = Int

type SymbolTable = [(Label, Index)]

data Code = Code { instrs :: Array Index Instruction
                 , symbolTable :: SymbolTable
                 , prevJump    :: [Index]
                 , environment :: Environment
                 , stack       :: Stack
                 , programCounter :: Index
                 } deriving Show

newtype Evaluate a =
  Evaluate
    { runEvaluate :: State Code a
    }
  deriving (Functor, Applicative, Monad)

instance MonadState Code Evaluate where
  get   = Evaluate (StateT (\s -> return (s,s)))
  put s = Evaluate (StateT (\_ -> return ((),s)))

-- Val is basically Weak Head Normal Form
data Val = VInt  Int  -- constants s(0)
         | VBool Bool -- constants s(0)
         | VEmpty     -- empty tuple
         | VPair Val Val -- Pair
         | VCon Tag Val  -- first arg is the tag second is value with tag
         | VClosure Val Label -- closure; Val is the environment
         | VComb Label        -- closure of a combinator; no free variables
         deriving (Ord, Eq)

-- using Hinze's notation
instance Show Val where
  show (VInt i)  = show i
  show (VBool b) = show b
  show  VEmpty   = "()"
  show (VPair v1 v2) =
    "(" <> show v1 <> ", " <> show v2 <> ")"
  show (VCon t v) =
    "(" <> show t <> " : " <> show v  <> ")"
  show (VClosure v l) =
    "[" <> show v <> " : " <> show l  <> "]"
  show (VComb l) =
    "[" <> show l <> "]"


-- Symbol Table operations

-- In this case we have a common error message
-- because this operation is only on a symbol table
(~>) :: SymbolTable -> Label -> Index
(~>) [] _ = error "Label missing in symbol table"
(~>) ((label,idx):st) l
  | l == label = idx
  | otherwise  = st ~> l

put :: SymbolTable -> Label -> Index -> SymbolTable
put st l idx = (l,idx) : st


emptyST :: SymbolTable
emptyST = []
