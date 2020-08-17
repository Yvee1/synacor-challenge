{-# LANGUAGE TemplateHaskell #-}
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

newtype VMEvent = WriteCharacter Char

data State = State
  { mvar   :: MVar Char
  , output :: String
  , input  :: String }

type Event = VMEvent
data Name  = Output
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
    drawOutputBox s
    <=>
    drawInputBox s
  ]

drawOutputBox :: State -> Widget Name
drawOutputBox s =
  hCenter $
    vLimit 15 $
    hLimit 50 $
    borderWithLabel (str "Output") $
    drawVMOutput s

drawInputBox :: State -> Widget Name
drawInputBox s =
  hCenter $
    vLimit 3 $
    hLimit 50 $
    borderWithLabel (str "Input") $
    drawInput s

drawInput :: State -> Widget Name
drawInput s = (str . reverse . input) s <+> hFill ' '

drawVMOutput :: State -> Widget Name
drawVMOutput = viewport Output Vertical . strWrap . reverse . output

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s@State {output=xs, input=ys} e = case e of
  VtyEvent ev -> case ev of
    V.EvKey  V.KEsc       []        -> halt s
    V.EvKey (V.KChar 'c') [V.MCtrl] -> halt s
    V.EvKey (V.KChar c  ) []        -> vScrollToEnd (viewportScroll Output) *> continue s {input=c:ys}
    V.EvKey  V.KEnter     []        -> submitInput s *> continue s {input=""}
    V.EvKey  V.KBS        []        -> continue s {input=tail ys}
    V.EvKey V.KDown []              -> vScrollBy (viewportScroll Output) 1 *> continue s
    V.EvKey V.KUp []                -> vScrollBy (viewportScroll Output) (-1) *> continue s
    _                               -> continue s

  AppEvent (WriteCharacter c)       -> vScrollToEnd (viewportScroll Output) *> continue s {output=c:xs}
  _                                 -> continue s

submitInput :: State -> EventM Name ()
submitInput State{input=xs, mvar=m} = liftIO $ forM_ (reverse xs ++ "\n") (putMVar m) 

theMap :: AttrMap
theMap = attrMap defAttr []

runUI :: IO ()
runUI = do
  eventChan <- Brick.BChan.newBChan 10
  m <- newEmptyMVar :: IO (MVar Char)
  let initialState = ()
      pcf char = writeBChan eventChan (WriteCharacter char)
      gcf = takeMVar m
  forkIO $ challenge' pcf gcf $> ()
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  finalState <- customMain initialVty buildVty
                  (Just eventChan) app (State {mvar=m, output=[], input=[]})

  return ()


-- | Fill all available space with the specified character. Grows only
-- horizontally.
hFill :: Char -> Widget n
hFill ch =
    Widget Greedy Fixed $ do
      c <- getContext
      return $ emptyResult & imageL .~ charFill (c^.attrL) ch (c^.availWidthL) 1