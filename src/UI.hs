{-# LANGUAGE TupleSections #-}
module UI where

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Monad
import Data.Functor
import VM
import Control.Concurrent
import Control.Monad.IO.Class
import Lens.Micro.Platform
import Graphics.Vty (mkVty, defaultConfig, defAttr, charFill)
import qualified Graphics.Vty as V

data State = State
  { iMvar       :: MVar (Action, String)
  , iaMvar      :: MVar ()
  , aMvar       :: MVar Action
  , output      :: String
  , input       :: String
  , registers   :: [Value]
  , stack       :: [Value]
  , instruction :: String
  , debugging   :: Bool
  , sendInput   :: Bool }

type Event = VMEvent
data Name  = Output
           | Input
  deriving (Eq, Ord, Show)

app :: App State Event Name
app = App
  { appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

drawUI :: State -> [Widget Name]
drawUI s = 
  [ vCenter $
    (drawOutputBox (output s)
     <=>
     drawInputBox (input s))
    <+>
    if debugging s then
      drawRegisters (registers s)
      <=>
      drawInstruction (instruction s)
    else emptyWidget
  ]

drawOutputBox :: String -> Widget Name
drawOutputBox s =
  hCenter $
    vLimit 15 $
    hLimit 50 $
    borderWithLabel (str "Output") $
    drawVMOutput s

drawInputBox :: String -> Widget Name
drawInputBox s =
  hCenter $
    vLimit 3 $
    hLimit 50 $
    borderWithLabel (str "Input") $
    drawInput s

drawInput :: String -> Widget Name
drawInput s = showCursor Input (Location (length s, 0)) $ (str . reverse) s <+> hFill ' '

drawVMOutput :: String -> Widget Name
drawVMOutput = viewport Output Vertical . strWrap . reverse

drawRegisters :: [Value] -> Widget Name
drawRegisters = vBox . map drawRegister
  where drawRegister (Value n) = str (show n)

drawInstruction :: String -> Widget Name
drawInstruction = str

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s@State {output=xs, input=ys, aMvar = am, debugging = b} e = case e of
  VtyEvent ev -> case ev of
    V.EvKey  V.KEsc       []             -> halt s
    V.EvKey (V.KChar 'c') [V.MCtrl]      -> halt s
    V.EvKey (V.KFun 1   ) []             -> continue (s{debugging=not b})
    V.EvKey  V.KRight     []             -> (if b && not (sendInput s) then liftIO (putMVar am (Continue b)) else pure ()) *> continue s
    V.EvKey (V.KChar c  ) []             -> vScrollToEnd (viewportScroll Output) *> writeCharacter s c
    V.EvKey  V.KEnter     []             -> if sendInput s then submitInput s else continue s
    V.EvKey  V.KBS        []             -> continue s {input=safeTail ys}
    V.EvKey  V.KDown      []             -> vScrollBy (viewportScroll Output) 1 *> continue s
    V.EvKey  V.KUp        []             -> vScrollBy (viewportScroll Output) (-1) *> continue s
    _                                    -> continue s

  AppEvent (WriteCharacter c)            -> vScrollToEnd (viewportScroll Output) *> continue s {output=c:xs}
  AppEvent FinishedInstruction           -> liftIO (putMVar am (Continue b)) *> continue s 
  AppEvent (Debug i rs st)               -> continue s {instruction=i, registers=rs, stack=st, sendInput=False}
  AppEvent ReadyForInput                 -> continue s {sendInput=True}
  _                                      -> continue s

writeCharacter :: State -> Char -> EventM Name (Next State)
writeCharacter s@State{input=ys, iMvar=m, iaMvar=m', debugging=b} c = 
  if Prelude.not b
    then continue s {input=c:ys}
    else liftIO (putMVar m' () *> modifyMVar_ m (\(_, xs) -> pure (Continue b, xs ++ [c]))) *> continue s

submitInput :: State -> EventM Name (Next State)
submitInput s@State{input=ys, iMvar=m, iaMvar=m', debugging=b} =
  if Prelude.not b
    then do liftIO $ modifyMVar_ m (\_ -> pure (Continue b, reverse ('\n':ys)))
            liftIO $ forM_ ('\n':ys) (const (putMVar m' ()))
            continue s { input = "" }
    else liftIO (putMVar m' () *> modifyMVar_ m (\(_, xs) -> pure (Continue b, xs ++ "\n"))) *> continue s

theMap :: AttrMap
theMap = attrMap defAttr []

runUI :: FilePath -> IO ()
runUI file = do
  eventChan <- newBChan 10
  inputMVar <- newEmptyMVar :: IO (MVar (Action, String))
  inputAvailableMVar <- newEmptyMVar :: IO (MVar ())

  putMVar inputMVar (Continue False, "")
  actionMVar <- newEmptyMVar :: IO (MVar Action)
  let initialState = State
                      { iMvar=inputMVar
                      , iaMvar=inputAvailableMVar
                      , aMvar=actionMVar
                      , output=[]
                      , input=[]
                      , registers=[]
                      , stack=[]
                      , instruction="..."
                      , debugging=False
                      , sendInput=False
                      }
      pcf char = writeBChan eventChan (WriteCharacter char)
      gcf = writeBChan eventChan ReadyForInput
         *> takeMVar inputAvailableMVar 
         *> modifyMVar inputMVar (\(a, s) -> pure ((a, tail s), (a, head s)))

  forkIO $ debuggedRunFile file pcf gcf actionMVar eventChan $> ()

  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  finalState <- customMain initialVty buildVty
                  (Just eventChan) app initialState

  return ()


-- | Fill all available space with the specified character. Grows only
-- horizontally.
hFill :: Char -> Widget n
hFill ch =
    Widget Greedy Fixed $ do
      c <- getContext
      return $ emptyResult & imageL .~ charFill (c^.attrL) ch (c^.availWidthL) 1