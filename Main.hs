module Main where

import qualified Data.ByteString as B
import Data.List.Split
import Data.List hiding (and, or)
import Data.Bits ((.&.), (.|.))
import Data.Char (chr)
import Data.Either
import Data.Functor
import Data.Maybe
import Data.IntMap.Strict (IntMap)
import Data.Vector (Vector, (//), (!))
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Prelude hiding (mod, and, or, not)
import qualified Prelude
import qualified Data.Vector as V
import qualified Data.IntMap.Strict as M

main :: IO ()
main = test $> ()

test :: IO VM
test = csv "19, 65, 21, 19, 66, 1, 32768, 42, 9, 32768, 32768, 3, 4, 32769, 32768, 45, 2, 1337, 3, 32772"

csv :: String -> IO VM
csv str = do
  let ints = map read $ splitOn "," str
      vm = VM { memory=M.fromDistinctAscList (zip [0..] ints)
              , registers=V.replicate 8 (Value 0)
              , stack=[]
              , programCounter = Value 0 }
  (_, vm') <- flip runStateT vm . runMaybeT $ run
  putStrLn ""
  return vm'

-- challenge = do
  -- words <- B.readFile "challenge.bin"

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

newtype Id = Id Int
newtype Value = Value Int
type Memory = IntMap Int
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
  maybe (return ()) (\pc -> putVM vm {programCounter = pc}) mpc
  return $ (Just . const old) =<< mpc

safeNext :: Compute (Maybe Int)
safeNext = do
  vm <- getVM
  mpc <- incrementCounter

  return ((\(Value pc) -> Just (M.findWithDefault 0 pc (memory vm))) =<< mpc)
    
next :: Compute Int
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
opArgs :: Compute (Id, Int, Int)
opArgs = do
  i        <- nextId
  Value n1 <- nextValue'
  Value n2 <- nextValue'
  return (i, n1, n2)

quit :: Compute ()
quit = mzero

act :: Int -> Compute ()
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
act 19 = out
act 21 = noop
act n = error $ "unsupported operation: " <> show n

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

jmp :: Compute ()
jmp = do
  loc <- nextValue'
  vm <- getVM
  putVM (vm {programCounter = loc })

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

out :: Compute ()
out = do
  Value int <- nextValue'
  liftIO . putChar $ chr int
  return ()

noop :: Compute ()
noop = return ()

isId :: Int -> Bool
isId n = let i = n - 32768
  in i >= 0 && i < 8

parseId :: Int -> Either Int Id
parseId n = if isId n then Right (Id (n - 32768)) else Left n

isValue :: Int -> Bool
isValue n = n >= 0 && n < 32768

parseValue :: Int -> Either Int Value
parseValue n = if isValue n
  then Right (Value n)
  else Left n

setRegister :: Id -> Value -> Compute ()
setRegister (Id i) v = do
  vm <- getVM
  let rs  = registers vm
      rs' = rs // [(i, v)]
  putVM vm { registers=rs' }

getRegister :: Id -> Compute Value
getRegister (Id i) = do
  vm <- getVM
  return (registers vm ! i)

to15Bits :: Int -> [Int]
to15Bits = toNBits 15

toNBits :: Int -> Int -> [Int]
toNBits n = toNBits' 0
  where
    toNBits' i x | i < n    = (x `rem` 2) : toNBits' (i+1) (x `div` 2)
                 | otherwise = []

fromBits :: [Int] -> Int
fromBits []     = 0
fromBits (x:xs) = x + 2 * fromBits xs

flipBit :: Int -> Int
flipBit 0 = 1
flipBit _ = 0

pushStack :: Value -> Stack -> Stack
pushStack = (:)

safePopStack :: Stack -> Maybe (Value, Stack)
safePopStack = uncons

popStack :: Stack -> (Value, Stack)
popStack = fromJust . safePopStack