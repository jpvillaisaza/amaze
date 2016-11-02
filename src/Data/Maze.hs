{-# LANGUAGE RecordWildCards #-}

module Data.Maze
  ( Maze
  , def
  , generate
  )
  where

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- maze
import Data.Maze.Cell (Cell (..))
import qualified Data.Maze.Cell as Cell
import Data.Stack

-- random
import qualified System.Random as Random


-- |
--
--

data Maze =
  Maze
    { cells :: Map (Int, Int) Cell
    , rows :: Int
    , columns :: Int
    }


-- |
--
--

instance Show Maze where
  show Maze {..} =
    unlines $ flip fmap [columns, columns - 1..0] $
      \j -> flip concatMap [0..rows-1] $
        \i ->
          if j == columns then (if i /= 0 then "_ " else "   ") else
          let Cell {..} = cells Map.! (i,j)
          in
            (if i == 0 then "|" else "") ++
            (if bottom && (i, j) /= (rows - 1, 0) then "_" else " ") ++
            (if right then "|" else " ")

-- |
--
--

def
  :: Int
  -> Int
  -> Maze

def rows columns =
  let
    cells =
      Map.fromList
        [((x, y), Cell.def) | x <- [0..rows - 1], y <- [0..columns-1]]

  in
    Maze {rows = rows, cells = cells, columns = columns}


-- |
--
--

unvisitedNeighbors
  :: (Int, Int)
  -> Maze
  -> [(Int, Int)]

unvisitedNeighbors pos maze@Maze {..} =
  filter
    (not . visited . (Map.!) cells)
    (neighbors pos maze)


-- |
--
--

neighbors
  :: (Int, Int)
  -> Maze
  -> [(Int, Int)]

neighbors (x, y) Maze {..} =
  filter
    (\(i, j) -> i >= 0 && j >= 0 && i < rows && j < columns)
    [ (x, y + 1)
    , (x, y - 1)
    , (x + 1, y)
    , (x - 1, y)
    ]


-- |
--
--

generate
  :: Int
  -> Int
  -> IO Maze

generate rows cols = do
  pos <-
    (,)
      <$> Random.randomRIO (0, rows - 1)
      <*> Random.randomRIO (0, cols - 1)

  generate' pos (push pos mempty) (def rows cols)


-- |
--
--

generate'
  :: (Int, Int)
  -> Stack (Int, Int)
  -> Maze
  -> IO Maze

generate' pos s maze@Maze {..} = do
  unvisitedNeighbor <- randomElem (unvisitedNeighbors pos maze)

  case unvisitedNeighbor of
    Nothing ->
      case pop s of
        Nothing ->
          return maze

        Just (pos', s') ->
          generate' pos' s' maze

    Just pos' -> do
      let
        cells' = Map.adjust (visit pos pos') pos cells
        cells'' = Map.adjust (visit pos' pos) pos' cells'

      generate' pos' (push pos' s) maze { cells = cells'' }


-- |
--
--

visit
  :: (Int, Int)
  -> (Int, Int)
  -> Cell
  -> Cell

visit (i, j) (x, y) cell =
  let
    visit'
      | i == x && j < y = cell { top = False }
      | i == x && j > y = cell { bottom = False }
      | j == y && i < x = cell { right = False }
      | j == y && i > x = cell { left = False }
      | otherwise = cell

  in
    visit' { visited = True }


-- |
--
--

randomElem
  :: [a]
  -> IO (Maybe a)

randomElem xs =
  case xs of
    [] ->
      return Nothing

    _ ->
      Just . (xs !!)
        <$> Random.randomRIO (0, length xs - 1)
