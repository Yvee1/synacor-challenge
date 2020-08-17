{-# LANGUAGE TemplateHaskell #-}
module VM (
  -- runners
  csv,
  challenge,
  challenge',
  
  -- types
  VM(..),
  Compute,
  
  -- lenses
  memory,
  registers,
  stack,
  programCounter,

  -- compute actions
  start,
  start',
  run,
  quit,
  runCompute,
  safeNext,
  act,
  getVM
  ) where

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
import Control.Concurrent
import Prelude hiding (mod, and, or, not)
import System.IO
import Lens.Micro.Platform hiding (set)
import qualified Prelude
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V
import qualified Data.IntMap.Strict as M
import qualified Lens.Micro.Platform as L

data VM = VM
  { _memory         :: Memory
  , _registers      :: Vector Value
  , _stack          :: Stack
  , _programCounter :: ProgramCounter
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
makeLenses ''VM

csv :: String -> IO VM
csv = start . map read . splitOn ","

challenge' :: (Char -> IO ()) -> IO Char -> IO VM
challenge' pcf gcf = do
  bs <- B.readFile "challenge.bin"
  start' pcf gcf $ B.runGet getWord16s bs

challenge :: IO VM
challenge = do
  bs <- B.readFile "challenge.bin"
  start $ B.runGet getWord16s bs

runCompute :: VM -> Compute a -> IO (Maybe a, VM)
runCompute vm = flip runStateT vm . runMaybeT

start :: [Word16] -> IO VM
start = start' putChar getChar

start' :: (Char -> IO ()) -> IO Char -> [Word16] -> IO VM
start' pcf gcf ws = do
  let vm = VM { _memory = M.fromDistinctAscList (zip [0..] ws)
              , _registers = V.replicate 8 (Value 0)
              , _stack = []
              , _programCounter = Value 0 }
  (_, vm') <- runCompute vm (run pcf gcf)
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

getVM :: Compute VM
getVM = lift get

putVM :: VM -> Compute ()
putVM = lift . put

run :: (Char -> IO ()) -> IO Char -> Compute ()
run pcf gcf = 
  maybe quit (\cmd -> act pcf gcf cmd *> run pcf gcf) =<< safeNext

incrementCounter :: Compute (Maybe ProgramCounter)
incrementCounter = do
  vm <- getVM
  let old@(Value n) = vm ^. programCounter
  let mpc = if n + 1 < 32767 then Just (Value (n + 1)) else Nothing
  maybe (return ()) (putVM . flip (L.set programCounter) vm) mpc
  return $ (Just . const old) =<< mpc

safeNext :: Compute (Maybe Word16)
safeNext = do
  vm <- getVM
  mpc <- incrementCounter

  return ((\(Value pc) -> Just (M.findWithDefault 0 (fromIntegral pc) (vm ^. memory))) =<< mpc)
    
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

act :: (Char -> IO ()) -> IO Char -> Word16 -> Compute ()
act putCharFunction getCharFunction w =
  case w of
    0  -> halt
    1  -> set
    2  -> push
    3  -> pop
    4  -> eq
    5  -> gt
    6  -> jmp
    7  -> jt
    8  -> jf
    9  -> add
    10 -> mult
    11 -> mod
    12 -> and
    13 -> or
    14 -> not
    15 -> rmem
    16 -> wmem
    17 -> call
    18 -> ret
    19 -> out putCharFunction
    20 -> in' getCharFunction
    21 -> noop
    _  -> noop
-- act n = do
--   vm <- getVM
--   error $ "unsupported operation: " <> show n

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
  putVM $ vm & stack %~ pushStack value

pop :: Compute ()
pop = do
  i <- nextId
  vm <- getVM
  let (v, s) = popStack $ vm ^. stack
  putVM $ vm & stack .~ s
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
  putVM $ vm & programCounter .~ loc

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
  let pc = vm ^. programCounter
  -- putVM vm { programCounter = pc, stack = pushStack pc (stack vm) }
  putVM $ vm & stack %~ pushStack pc
  jmpTo loc

ret :: Compute ()
ret = do
  vm <- getVM
  let (v, s) = popStack $ vm ^. stack
  putVM $ vm & stack .~ s
  jmpTo v

out :: (Char -> IO ()) -> Compute ()
out putCharFunction = do
  Value word <- nextValue'
  liftIO . putCharFunction . chr $ fromIntegral word
  return ()

-- in is a reserved keyword :/
in' :: IO Char -> Compute ()
in' getCharFunction = do
  i <- nextId
  input <- liftIO getCharFunction
  -- liftIO $ appendFile "input.txt" [input]
  -- case input of
  --   '!' -> debugProgramToFile "test.txt"
  --   '#' -> setRegister (Id 7) (Value 2)
  --   '$' -> liftIO $ putStrLn "Received debug message"
    -- _   -> setRegister i $ Value (fromIntegral (ord input))
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
  let rs  = vm ^. registers
      rs' = rs // [(fromIntegral i, v)]
  putVM $ vm & registers .~ rs'

getRegister :: Id -> Compute Value
getRegister (Id i) = do
  vm <- getVM
  return ((vm ^. registers) ! fromIntegral i)

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
readMemory (Value i) = M.findWithDefault 0 (fromIntegral i) . view memory <$> getVM

setMemory :: Value -> Word16 -> Compute ()
setMemory (Value i) n = do
  vm <- getVM
  let m  = vm ^. memory
      m' = M.insert (fromIntegral i) n m
  putVM $ vm & memory .~ m'

disassembleChallenge :: IO ()
disassembleChallenge = do
  bs <- B.readFile "challenge.bin"
  let ws = B.runGet getWord16s bs
  writeFile "output.txt" $ disassembleProgram ws

disassembleProgram :: [Word16] -> String
disassembleProgram = concatMap ((++" ") . toString)

toString :: Word16 -> String
toString 0 = "\nhalt"
toString 1 = "\nset"
toString 2 = "\npush"
toString 3 = "\npop"
toString 4 = "\neq"
toString 5 = "\ngt"
toString 6 = "\njmp"
toString 7 = "\njt"
toString 8 = "\njf"
toString 9 = "\nadd"
toString 10 = "\nmult"
toString 11 = "\nmod"
toString 12 = "\nand"
toString 13 = "\nor"
toString 14 = "\nnot"
toString 15 = "\nrmem"
toString 16  = "\nwmem"
toString 17 = "\ncall"
toString 18 = "\nret"
toString 19 = "\nout"
toString 20 = "\nin"
toString 21 = "\nnoop"
toString n = [chr (fromIntegral n)]

debugProgramToFile :: FilePath -> Compute ()
debugProgramToFile fp =
  liftIO . writeFile fp . disassembleProgram =<< (map snd . M.toList . view memory <$> getVM)

writeVMState :: FilePath -> Compute ()
writeVMState fp = do
  vm <- getVM
  let pc@(Value i) = vm ^. programCounter
  let s = "Registers: " <> show (vm ^. registers) <> "\n"
       <> "Stack: " <> show (vm ^. stack) <> "\n"
       <> "Program counter: " <> show pc <> "\n"
       <> "Instruction: " <> toString (M.findWithDefault 0 (fromIntegral i) (vm ^. memory))
  liftIO $ writeFile fp s
