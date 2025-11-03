module Main where

import File.LexerFmt
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let fin = args !! 1
  let fout = args !! 2
  lexffmt fin fout
