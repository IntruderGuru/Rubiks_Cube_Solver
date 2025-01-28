module Main where

import AppUtils
import Control.Monad (when)
import Rotations (moveCube)
import Scramble (generateScramble)
import Solving (solveCubePhases)
import System.Environment (getArgs)
import Utils
import Prelude hiding (Left, Right)

main :: IO ()
main = do
  args <- getArgs
  let detailed = "--detailed" `elem` args
  if "--help" `elem` args
    then do
      putStrLn "Instrukcja użycia:"
      putStrLn "  .\rubikSolver <plik_z_kostka.txt>"
      putStrLn "  .\rubikSolver --scramble <liczba_ruchów> <plik_wyjściowy>"
      putStrLn "  .\rubikSolver <plik_z_kostka.txt> --detailed"
      putStrLn "  W pliku powinny znajdować się wiersze z: <Side> <9 kolorów> ..."
      putStrLn "  1. Ustaw kostkę fizycznie tak, aby górna (Up) była biała, a frontowa (Front) czerwona."
      putStrLn "  2. W pliku tekstowym wpisz kolejno ściany i 9 kolorów każdej z nich, np.:"
      putStrLn "   Front Blue Red Red Red Red White Green Orange White"
      putStrLn "   Right Blue Yellow Green Green Blue Blue Red Orange Blue"
      putStrLn "   ... i tak dalej dla Back, Left, Up, Down."
      putStrLn "Dostępne strony: Front, Right, Back, Left, Up, Down"
      putStrLn "Dostępne kolory: White, Yellow, Orange, Green, Red, Blue"
    else case args of
      ("--scramble" : numMovesStr : rest) -> do
        let numMoves = read numMovesStr :: Int
        let solvedCube = getSolvedCube
        let (moves, scrambledCube) = generateScramble numMoves solvedCube
        -- Sprawdzenie, czy użytkownik podał nazwę pliku do zapisu
        case rest of
          (outputFile : _) -> do
            -- Zapisuje kostkę do pliku
            let cubeStr = unlines (formatCube scrambledCube)
            writeFile ("../data/" ++ outputFile) cubeStr
            putStrLn $ "Scramble zapisany do pliku: " ++ outputFile
            putStrLn "=== Losowe Ułożenie Kostki ==="
            mapM_ putStrLn (formatCube scrambledCube)
          [] -> do
            -- Jeśli plik nie został podany, wyświetl kostkę i ruchy na ekranie
            putStrLn "=== Losowe Ułożenie Kostki ==="
            mapM_ putStrLn (formatCube scrambledCube)
            putStrLn "\n=== Ruchy Scrambla ==="
            putStrLn $ unwords (map show moves)

      -- Obsługa standardowa: plik z kostką do rozwiązania (oraz opcjonalne --detailed)
      (fileName : _) -> do
        content <- readFile ("../data/" ++ fileName)
        let fileLines = lines content
        let initialCube = readCube fileLines []

        putStrLn "=== Kostka do rozwiązania przez CFOP ==="
        mapM_ putStrLn (formatCube initialCube)

        let (_, taggedMoves) = solveCubePhases initialCube
        let simplifiedTagged = simplifyTaggedMoves taggedMoves
        let groupedByPhase = groupByPhase simplifiedTagged

        putStrLn "\n=== Ruchy ==="
        mapM_ printPhaseMoves groupedByPhase

        ---------------------------------------------------------------------
        -- Sekcja: wyświetlanie "krok po kroku" (tryb --detailed)
        ---------------------------------------------------------------------
        when detailed $ do
          putStrLn "\n=== Szczegółowy przebieg wykonywania ruchów (step-by-step) ==="
          let finalSolutionMoves = map snd simplifiedTagged
              states = scanl (flip moveCube) initialCube finalSolutionMoves

          let movesWithStates = zip3 ([1 ..] :: [Int]) finalSolutionMoves (tail states)

          mapM_
            ( \(i, mv, cubeAfter) -> do
                putStrLn $ show i ++ ". Ruch: " ++ show mv
                mapM_ putStrLn (formatCube cubeAfter)
                putStrLn "---------------------------------"
            )
            movesWithStates
      [] -> putStrLn "Error: Please provide a file name or '--scramble <number_of_moves> [<output_file>]' as an argument."
