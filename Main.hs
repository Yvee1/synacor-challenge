module Main where

import qualified Data.ByteString.Lazy as B
import Data.List.Split
import Data.List hiding (and, or)
import Data.Bits ((.&.), (.|.))
import Data.Char (chr, ord)
import Data.Either
import Data.Functor
import Data.Maybe
import Data.Word
import Data.IntMap.Strict (IntMap)
import Data.Vector (Vector, (//), (!))
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Prelude hiding (mod, and, or, not)
import qualified Prelude
import qualified Data.Binary.Get as B
import qualified Data.Vector as V
import qualified Data.IntMap.Strict as M

main :: IO ()
main = challenge $> ()

csv :: String -> IO VM
csv = start . map read . splitOn ","

challenge :: IO VM
challenge = do
  bs <- B.readFile "challenge.bin"
  start $ B.runGet getWord16s bs

start :: [Word16] -> IO VM
start ws = do
  let vm = VM { memory=M.fromDistinctAscList (zip [0..] ws)
              , registers=V.replicate 8 (Value 0)
              , stack=[]
              , programCounter = Value 0 }
  (_, vm') <- flip runStateT vm . runMaybeT $ run
  putStrLn ""
  return vm'

getWord16s :: B.Get [Word16]
getWord16s = do
  empty <- B.isEmpty
  if empty
    then return []
    else do w  <- B.getWord16le
            ws <- getWord16s
            return (w:ws)

data VM = VM
  { memory         :: Memory
  , registers      :: Vector Value
  , stack          :: Stack
  , programCounter :: ProgramCounter
  }
  deriving (Show)

type Stack = [Value]
instance Show Value where
  show (Value n) = show n

newtype Id = Id Word16
newtype Value = Value Word16
type Memory = IntMap Word16
type ProgramCounter = Value

type Compute a = MaybeT (StateT VM IO) a

getVM :: Compute VM
getVM = lift get

putVM :: VM -> Compute ()
putVM = lift . put

run :: Compute ()
run = do
  mCommand <- safeNext
  maybe quit (\cmd -> act cmd *> run) mCommand

incrementCounter :: Compute (Maybe ProgramCounter)
incrementCounter = do
  vm <- getVM
  let old@(Value n) = programCounter vm 
  let mpc = if n + 1 < 32767 then Just (Value (n + 1)) else Nothing
  maybe (return ()) (\pc -> putVM vm {programCounter = pc }) mpc
  return $ (Just . const old) =<< mpc

safeNext :: Compute (Maybe Word16)
safeNext = do
  vm <- getVM
  mpc <- incrementCounter

  return ((\(Value pc) -> Just (M.findWithDefault 0 (fromIntegral pc) (memory vm))) =<< mpc)
    
next :: Compute Word16
next =  fromMaybe (error "Unexpected end of program")
    <$> safeNext

nextValue :: Compute Value
nextValue = do
  eNum <- parseValue <$> next
  return $ either (error . ("Invalid value: " <>) . show) id eNum 

nextId :: Compute Id
nextId = do
  eReg <- parseId <$> next
  return $ either (error . ("Invalid register: " <>) . show) id eReg 

nextValueOrRegister :: Compute (Either Value Id)
nextValueOrRegister = do
  num <- next
  if isValue num
    then return . Left  $ Value num 
    else return . Right $ either (error . ("Invalid number: " <>) . show) id $ parseId num

-- next value or the value in the next register
nextValue' :: Compute Value
nextValue' = do
  vOrR <- nextValueOrRegister
  either return getRegister vOrR

-- common arguments for operations
opArgs :: Compute (Id, Word16, Word16)
opArgs = do
  i        <- nextId
  Value n1 <- nextValue'
  Value n2 <- nextValue'
  return (i, n1, n2)

quit :: Compute ()
quit = mzero

act :: Word16 -> Compute ()
act 0  = halt
act 1  = set
act 2  = push
act 3  = pop
act 4  = eq
act 5  = gt
act 6  = jmp
act 7  = jt
act 8  = jf
act 9  = add
act 10 = mult
act 11 = mod
act 12 = and
act 13 = or
act 14 = not
act 15 = rmem
act 16 = wmem
act 17 = call
act 18 = ret
act 19 = out
act 20 = in'
act 21 = noop
act n = do
  vm <- getVM
  error $ "unsupported operation: " <> show n

halt :: Compute ()
halt = quit

set :: Compute ()
set = do
  i     <- nextId
  value <- nextValue'
  setRegister i value
  return ()

push :: Compute ()
push = do
  value <- nextValue'
  vm <- getVM
  let vm' = vm { stack = pushStack value (stack vm) }
  putVM vm'

pop :: Compute ()
pop = do
  i <- nextId
  vm <- getVM
  let (v, s') = popStack (stack vm)
  putVM vm { stack = s' }
  setRegister i v

eq :: Compute ()
eq = do
  (i, n1, n2) <- opArgs
  let value = Value $ if n1 == n2 then 1 else 0
  setRegister i value

gt :: Compute ()
gt = do
  (i, n1, n2) <- opArgs
  let value = Value $ if n1 > n2 then 1 else 0
  setRegister i value

jmpTo :: Value -> Compute ()
jmpTo loc = do
  vm <- getVM
  putVM (vm {programCounter = loc })

jmp :: Compute ()
jmp = do
  loc <- nextValue'
  jmpTo loc

jt :: Compute ()
jt = do
  (Value n) <- nextValue'
  when (n /= 0) jmp

jf :: Compute ()
jf = do
  (Value n) <- nextValue'
  when (n == 0) jmp

add :: Compute ()
add = do
  (i, n1, n2) <- opArgs
  setRegister i $ Value $ (n1 + n2) `rem` 32768

mult :: Compute ()
mult = do
  (i, n1, n2) <- opArgs
  setRegister i $ Value $ (n1 * n2) `rem` 32768

mod :: Compute ()
mod = do
  (i, n1, n2) <- opArgs
  setRegister i $ Value (n1 `rem` n2)

and :: Compute ()
and = do
  (i, n1, n2) <- opArgs
  setRegister i $ Value (n1 .&. n2)

or :: Compute ()
or = do
  (i, n1, n2) <- opArgs
  setRegister i $ Value (n1 .|. n2)

not :: Compute ()
not = do
  i <- nextId
  Value n <- nextValue'
  setRegister i $ Value . fromBits . map flipBit . to15Bits $ n

rmem :: Compute ()
rmem = do
  i <- nextId
  v <- nextValue'
  setRegister i . Value =<< readMemory v 

wmem :: Compute ()
wmem = do
  i         <- nextValue'
  (Value n) <- nextValue'
  setMemory i n

call :: Compute ()
call = do
  loc <- nextValue'
  vm <- getVM
  let pc = programCounter vm
  putVM vm { programCounter = pc, stack = pushStack pc (stack vm) }
  jmpTo loc

ret :: Compute ()
ret = do
  vm <- getVM
  let (v, s) = popStack $ stack vm
  putVM vm { stack = s }
  jmpTo v

out :: Compute ()
out = do
  Value word <- nextValue'
  liftIO . putChar . chr $ fromIntegral word
  return ()

-- in is a reserved keyword :/
in' :: Compute ()
in' = do
  i <- nextId
  input <- liftIO getChar
  setRegister i $ Value (fromIntegral (ord input))

noop :: Compute ()
noop = return ()

isId :: Word16 -> Bool
isId n = let i = n - 32768
  in i >= 0 && i < 8

parseId :: Word16 -> Either Word16 Id
parseId n = if isId n then Right (Id (n - 32768)) else Left n

isValue :: Word16 -> Bool
isValue n = n >= 0 && n < 32768

parseValue :: Word16 -> Either Word16 Value
parseValue n = if isValue n
  then Right (Value n)
  else Left n

setRegister :: Id -> Value -> Compute ()
setRegister (Id i) v = do
  vm <- getVM
  let rs  = registers vm
      rs' = rs // [(fromIntegral i, v)]
  putVM vm { registers=rs' }

getRegister :: Id -> Compute Value
getRegister (Id i) = do
  vm <- getVM
  return (registers vm ! fromIntegral i)

to15Bits :: Word16 -> [Word16]
to15Bits = toNBits 15

toNBits :: Word16 -> Word16 -> [Word16]
toNBits n = toNBits' 0
  where
    toNBits' i x | i < n    = (x `rem` 2) : toNBits' (i+1) (x `div` 2)
                 | otherwise = []

fromBits :: [Word16] -> Word16
fromBits []     = 0
fromBits (x:xs) = x + 2 * fromBits xs

flipBit :: Word16 -> Word16
flipBit 0 = 1
flipBit _ = 0

pushStack :: Value -> Stack -> Stack
pushStack = (:)

safePopStack :: Stack -> Maybe (Value, Stack)
safePopStack = uncons

popStack :: Stack -> (Value, Stack)
popStack = fromJust . safePopStack

readMemory :: Value -> Compute Word16
readMemory (Value i) = M.findWithDefault 0 (fromIntegral i) . memory <$> getVM

setMemory :: Value -> Word16 -> Compute ()
setMemory (Value i) n = do
  vm <- getVM
  let m  = memory vm
      m' = M.insert (fromIntegral i) n m
  putVM vm { memory = m' }