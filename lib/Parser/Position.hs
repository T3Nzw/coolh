module Parser.Position where

import Control.Lens (makeLenses)

data SourcePos = SourcePos {_absOffset :: Int, _lineNumber :: Int, _colNumber :: Int, _tabWidth :: Int}
  deriving (Show)

data State s = State {_input :: s, _pos :: SourcePos}
  deriving (Show)

makeLenses ''SourcePos
makeLenses ''State

initial :: s -> State s
initial s = State {_input = s, _pos = SourcePos {_absOffset = 0, _lineNumber = 1, _colNumber = 1, _tabWidth = 4}}
