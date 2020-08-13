import Bytecode.InterpreterHelper
import CAM
import Data.List (find)
import GHC.Arr
import Control.Monad.State.Strict(evalState, gets, modify)

{-
NOTE: This is not the actual "byte"code interpreter.
This takes the Haskell `Instruction` datatype and interprets that.
This module would be primarily used for experimentation with VM designs

NOTE 2: Efficiency of the data structures like symbol table
doesn't matter because we will generate more efficient C types
-}


-- Take a sequence of stack machine instructions
-- and evaluate them to their normal form
evaluate :: CAM -> Val
evaluate cam = val
  where
    code = initCode cam
    val  = evalState (runEvaluate eval) code

initCode :: CAM -> Code
initCode cam = Code { instrs = listArray (1, totalInstrs) caminstrs
                    , symbolTable = filteredEntries
                    , prevJump    = []
                    , environment = VEmpty
                    , stack       = []
                    , programCounter = 1
                    }
  where
    instrsLabs = genInstrs cam dummyLabel
    indexedinstrsLabs = zip instrsLabs [1..]
    caminstrs = map (fst . fst) indexedinstrsLabs
    entries   = map (\((_,l),idx) -> (l,idx)) indexedinstrsLabs
    filteredEntries = filter (\(l,_) -> l /= dummyLabel) entries
    totalInstrs     = length caminstrs

genInstrs :: CAM -> Label -> [(Instruction, Label)]
genInstrs (Ins i) l = [(i, l)]
genInstrs (Seq c1 c2) l = genInstrs c1 l ++ genInstrs c2 dummyLabel
genInstrs (Lab l c) _   = genInstrs c l

eval :: Evaluate Val
eval = do
  currentInstr <- readCurrent
  case currentInstr of
    FST ->
      do { incPC; fstEnv; eval }
    SND ->
      do { incPC; sndEnv; eval }
    ACC n  ->
      do { incPC; accessnth n; eval }
    REST n ->
      do { incPC; restnth n; eval }
    PUSH ->
      do { incPC; push; eval }
    SWAP ->
      do { incPC; swap; eval }
    QUOTE (LInt i)  ->
      do { incPC; loadi i; eval }
    QUOTE (LBool b) ->
      do { incPC; loadb b; eval }
    CLEAR ->
      do { incPC; clear; eval }
    PRIM1 uop ->
      do { incPC; unaryop uop; eval }
    PRIM2 bop ->
      do { incPC; binaryop bop; eval }
    CONS ->
      do { incPC; cons; eval }
    CUR l  ->
      do { incPC; cur l; eval }
    PACK c ->
      do { incPC; pack c; eval }
    SKIP ->
      do { incPC; eval}
    STOP -> getEnv -- base case
    APP    -> do {      app; eval }
    RETURN -> do {  retBack; eval }
    CALL l -> do { jumpTo l; eval }
    GOTO l -> do {   goto l; eval }
    GOTOFALSE l ->
      do { gotofalse l; eval; }
    SWITCH conds ->
      do { switch conds; eval; }
    i ->
      error ("Unsupported Instruction : " <> show i)

-- Code operations

readCurrent :: Evaluate Instruction
readCurrent = do
  is <- gets instrs
  pc <- gets programCounter
  pure (is ! pc)

incPC :: Evaluate ()
incPC  = do
  pc <- gets programCounter
  mutatePC (pc + 1)

jumpTo :: Label -> Evaluate ()
jumpTo l = do
  pc <- gets programCounter
  st <- gets symbolTable
  pj <- gets prevJump
  let ix = st ~> l
  mutatePC ix
  mutatePJ (pc : pj)

-- return back to the previous "jumped from" label
-- and increment the program counter by 1
retBack :: Evaluate ()
retBack = do
  pj <- gets prevJump
  mutatePC (head pj + 1)
  mutatePJ (tail pj)

getEnv :: Evaluate Environment
getEnv = gets environment

getStack :: Evaluate Stack
getStack = gets stack

popAndRest :: Evaluate (Val, Stack)
popAndRest = do
  st <- getStack
  let (h:t) = st
  return (h, t)

fstEnv :: Evaluate ()
fstEnv = do
  e <- getEnv
  case e of
    VPair v _ -> mutateEnv v
    _ -> error "first operation on incorrect value type"

sndEnv :: Evaluate ()
sndEnv = do
  e <- getEnv
  case e of
    VPair _ v -> mutateEnv v
    _ -> error "second operation on incorrect value type"

accessnth :: Int -> Evaluate ()
accessnth 0 = sndEnv
accessnth n = do
  fstEnv
  accessnth (n - 1)

restnth :: Int -> Evaluate ()
restnth 0 = pure ()
restnth n = do
  fstEnv
  restnth (n - 1)

push :: Evaluate ()
push = do
  e  <- getEnv
  st <- getStack
  mutateStack (e : st)

swap :: Evaluate ()
swap = do
  e      <- getEnv
  (h, t) <- popAndRest
  mutateEnv h
  mutateStack (e : t)

loadi :: Int -> Evaluate ()
loadi i = mutateEnv (VInt i)


loadb :: Bool -> Evaluate ()
loadb b = mutateEnv (VBool b)

clear :: Evaluate ()
clear = mutateEnv VEmpty

unaryop :: UnaryOp -> Evaluate ()
unaryop uop = do
  e <- getEnv
  case uop of
    Abs -> do
      let (VInt i) = e -- XXX: Partial
      mutateEnv (VInt (abs i))
    Neg -> do
      let (VInt i) = e
      mutateEnv (VInt (negate i))
    NOT -> do
      let (VBool b) = e
      mutateEnv (VBool (not b))
    DEC -> do
      let (VInt i) = e
      mutateEnv (VInt (i - 1))

binaryop :: BinOp -> Evaluate ()
binaryop bop = do
  e      <- getEnv
  (h, t) <- popAndRest
  case bop of
    Plus -> do
      let (VInt i1) = e -- XXX: Partial
      let (VInt i2) = h
      mutateEnv (VInt (i2 + i1))
      mutateStack t

    Multiply -> do
      let (VInt i1) = e
      let (VInt i2) = h
      mutateEnv (VInt (i2 * i1))
      mutateStack t

    Minus -> do
      let (VInt i1) = e
      let (VInt i2) = h
      mutateEnv (VInt (i2 - i1))
      mutateStack t

    BGT -> do
      let (VInt i1) = e
      let (VInt i2) = h
      mutateEnv (VBool (i2 > i1))
      mutateStack t

    BLT -> do
      let (VInt i1) = e
      let (VInt i2) = h
      mutateEnv (VBool (i2 < i1))
      mutateStack t

    BGE -> do
      let (VInt i1) = e
      let (VInt i2) = h
      mutateEnv (VBool (i2 >= i1))
      mutateStack t

    BLE -> do
      let (VInt i1) = e
      let (VInt i2) = h
      mutateEnv (VBool (i2 <= i1))
      mutateStack t

    BEQ -> do
      case (e,h) of
        (VInt i1, VInt i2) -> do
          mutateEnv (VBool (i2 == i1))
          mutateStack t
        (VBool b1, VBool b2) -> do
          mutateEnv (VBool (b2 == b1))
          mutateStack t
        (VEmpty, VEmpty) -> do
          mutateEnv (VBool True)
          mutateStack t
        (VPair v1 v2, VPair v3 v4) -> do
          mutateEnv (VBool (v1 == v3 && v2 == v4))
          mutateStack t
        (VCon t1 v1, VCon t2 v2) -> do
          mutateEnv (VBool (t1 == t2 && v1 == v2))
          mutateStack t
        _ -> error "Equality not supported for other whnf types"

cons :: Evaluate ()
cons = do
  e      <- getEnv
  (h, t) <- popAndRest
  mutateEnv (VPair h e)
  mutateStack t

cur :: Label -> Evaluate ()
cur l = do
  e <- getEnv
  mutateEnv (VClosure e l)

pack :: Tag -> Evaluate ()
pack t = do
  e <- getEnv
  mutateEnv (VCon t e)

app :: Evaluate ()
app = do
  e      <- getEnv
  (h, t) <- popAndRest
  let (VClosure val label) = e -- XXX: Partial
  mutateEnv (VPair val h)
  mutateStack t
  jumpTo label

goto :: Label -> Evaluate ()
goto l = do
  pc <- gets programCounter
  st <- gets symbolTable
  let ix = st ~> l
  mutatePC ix

gotofalse :: Label -> Evaluate ()
gotofalse l = do
  e      <- getEnv
  (h, t) <- popAndRest
  case e of
    VBool True -> do
      incPC
      mutateEnv h
      mutateStack t
    VBool False -> do
      mutateEnv h
      mutateStack t
      goto l
    _ -> error "GOTOFALSE instuction applied to incorrect operand"

switch :: [(Tag, Label)] -> Evaluate ()
switch conds = do
  e      <- getEnv
  (h, t) <- popAndRest
  let VCon ci v1 = e
  let label =
        case find (\(c,_) -> c == ci) conds of
          Just (_, lf) -> lf
          Nothing -> error ("Missing constructor" <> show ci)
  mutateEnv (VPair h v1)
  mutateStack t
  jumpTo label


-- Could the following four be autogenerated?
mutateEnv :: Val -> Evaluate ()
mutateEnv v =
  modify (\s -> s { environment = v })

mutateStack :: [Val] -> Evaluate ()
mutateStack st =
  modify (\s -> s { stack = st })

mutatePC :: Int -> Evaluate ()
mutatePC c =
  modify (\s -> s { programCounter = c })

mutatePJ :: [Index] -> Evaluate ()
mutatePJ newIdxs =
  modify (\s -> s { prevJump = newIdxs })

dummyLabel = "dummy"

-- NOTE:
{-
1. Use the stack for intermediate storage of environment;
   Always PUSH before beginning computation
2. BinOp (s(2)) expects first argument on stack and second on register
-}

example8 = Let (PatVar "y") (Sys $ LInt 1)
           (Lam (PatVar "x") (Sys $ Sys2 Plus (Var "x") (Var "y")))

run :: Exp -> Val
run = evaluate . interpret

main :: IO ()
main = putStrLn $ show $ run example8
