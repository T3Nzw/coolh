module File.ParserFmt where

import qualified Data.ByteString as B
import GHC.IO.Handle.FD (openBinaryFile)
import GHC.IO.IOMode (IOMode (ReadMode))
import Lexer.Util
import Parser.Grammar.Grammar (parse)
import Parser.Parser (treefmt)

writeAST :: FilePath -> FilePath -> B.ByteString -> IO ()
writeAST fin fout s = writeFile fout $ either id id parsed
  where
    parsed = treefmt "" <$> parse fin (lexer s)

parffmt :: FilePath -> FilePath -> IO ()
parffmt fin fout = do
  h <- openBinaryFile fin ReadMode
  contents <- B.hGetContents h
  writeAST fin fout contents
