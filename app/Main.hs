module Main where

import File.LexerFmt
import File.ParserFmt
import System.Environment (getArgs)

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
