module File.LexerFmt where

import GHC.IO.Handle.FD (openBinaryFile)
import GHC.IO.IOMode (IOMode (ReadMode))

import qualified Data.ByteString as B

import Lexer.Util

writeLexemes :: FilePath -> B.ByteString -> IO ()
writeLexemes f s = writeFile f lexemes
 where
  lexemes = lexfmt . lexer $ s

lexffmt :: FilePath -> FilePath -> IO ()
lexffmt fin fout = do
  -- supposedly this doesn't read virtual newlines from editors
  h <- openBinaryFile fin ReadMode
  contents <- B.hGetContents h
  writeLexemes fout contents
