module Debug.Flags where

import Data.ByteString (ByteString)

import Parser.Core (Parser, string)

data FlagError = UnrecognisedFlag
  deriving Eq

instance Semigroup FlagError where
  lhs <> _ = lhs

instance Monoid FlagError where
  mempty = UnrecognisedFlag
  mappend = (<>)

data DebugFlag
  = LEXDUMP
  | PARDUMP
  deriving Eq

parseLexDump :: Parser FlagError ByteString DebugFlag
parseLexDump = string "--lexer" >> pure LEXDUMP

parseFlags :: Parser FlagError ByteString DebugFlag
parseFlags = parseLexDump

flags :: [ByteString] -> Parser FlagError ByteString [DebugFlag]
flags = mapM $ const parseFlags

fcont :: DebugFlag -> (DebugFlag -> r) -> r
fcont flag cont = cont flag
