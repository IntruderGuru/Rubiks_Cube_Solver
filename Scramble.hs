module Scramble
  ( generateScramble,
  )
where

import Rotations
import Types
import Utils

-- | Lista wszystkich możliwych ruchów.
allMoves :: [Move]
allMoves = [U, U', D, D', L, L', R, R', F, F', B, B']

-- | Implementacja prostego generatora liczb pseudolosowych (Linear Congruential Generator).
lcg :: Int -> [Int]
lcg seed =
  let a = 1664525
      c = 1013904223
      m = 2 ^ 32
   in iterate (\x -> (a * x + c) `mod` m) seed

-- | Konwertuje liczbę całkowitą na indeks w zakresie dostępnych ruchów.
toMoveIndex :: Int -> Int
toMoveIndex num = num `mod` length allMoves

-- | Generuje listę pseudolosowych ruchów o zadanej długości.
generateRandomMoves :: Int -> [Move]
generateRandomMoves n = map ((allMoves !!) . toMoveIndex) (take n $ lcg seed)
  where
    -- Możemy użyć stałego ziarna lub innej metody deterministycznej.
    -- Na przykład, użyj sumy kodów znaków z nazwiska autora:
    seed = sum $ map fromEnum "defaultSeed"

-- | Generuje losowe ułożenie kostki poprzez zastosowanie losowych ruchów.
-- Zwraca zarówno listę ruchów, jak i wynikową kostkę.
generateScramble :: Int -> Cube -> ([Move], Cube)
generateScramble numMoves solvedCube =
  let moves = generateRandomMoves numMoves
      scrambled = foldl (flip moveCube) solvedCube moves
   in (moves, scrambled)
