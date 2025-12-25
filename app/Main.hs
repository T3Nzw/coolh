module Main where

import System.Environment (getArgs)

import File.LexerFmt
import File.ParserFmt

main :: IO ()
main = do
  args <- getArgs
  let flag = args !! 1
  let fin = args !! 2
  let fout = args !! 3
  case flag of
    "--lexer" -> lexffmt fin fout
    "--parser" -> parffmt fin fout
    _ -> putStrLn "flag not recognised"
