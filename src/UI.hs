{-# LANGUAGE TemplateHaskell #-}
module UI where

import Brick
import VM
import Lens.Micro.Platform

data VMEvent = FinishedInstruction

data State = State
  { _vm :: VM
  , _output :: String }
type Event = VMEvent
type Name  = ()

makeLenses ''State

app :: App State Event Name
app = App
  { appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

drawUI :: State -> [Widget Name]
drawUI s = [drawVMOutput s]

drawVMOutput :: State -> Widget Name
drawVMOutput s = undefined

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent = undefined

theMap :: AttrMap
theMap = undefined

runUI :: IO ()
runUI = do
  let initialState = State 
  undefined