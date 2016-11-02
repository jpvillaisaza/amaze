module Data.Maze.Cell
  ( Cell (..)
  , def
  )
  where


-- |
--
--

data Cell =
  Cell
    { left :: Bool
    , right :: Bool
    , top :: Bool
    , bottom :: Bool
    , visited :: Bool
    }


-- |
--
--

def
  :: Cell

def =
  Cell
    { left = True
    , right = True
    , top = True
    , bottom = True
    , visited = False
    }
