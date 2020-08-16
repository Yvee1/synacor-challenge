module Main where

import UI
import VM

main :: IO ()
main = do
  -- hSetBuffering stdin NoBuffering
  challenge
  return ()