module RubikMain where

import Prelude
import System.IO (readFile, putStrLn)
import System.Environment (getArgs)
import RubikTypes
import RubikParser
import RubikSolver
import RubikUtils

-- | Główna funkcja 'main' - wczytuje nazwę pliku z argumentów,
--   czyta zawartość, parsuje kostkę i uruchamia solver.
main :: IO ()
main = do
  args <- getArgs
  case args of
    (fileName:_) -> do
      content <- readFile fileName
      let cubeParsed = parseRubikFile content
      let (finalCube, moves) = solveEntireCube cubeParsed
      putStrLn "=== WYNIK ==="
      printCube finalCube
      putStrLn "\n=== LISTA RUCHÓW ==="
      print moves
    _ -> putStrLn "Użycie: ./RubikMain2 <plik_z_kostka>"
