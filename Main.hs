module Main
  ( main
  )
  where

-- base
import System.Environment

-- maze
import qualified Data.Maze as Maze


main
  :: IO ()

main = do
  [rows, cols] <- fmap (fmap read) getArgs

  Maze.generate rows cols
    >>= print
