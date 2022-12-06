module Main where
import System.Environment
import Control.Monad
import SimpleSearch
import System.IO

loop :: Index -> IO ()
loop index = do
  putStr "Search: "
  hFlush stdout
  req <- getLine
  print (search req index)
  loop index

main :: IO ()
main = do
  paths <- getArgs
  index <- foldM addFile (mkIndex words) paths
  loop index