module Solving where

import Rotations
import SolveF2L (solveF2L)
import SolveOLL (solveOLL)
import SolvePLL (solvePLL)
import SolveWhiteCross (solveWhiteCross)
import Types
import Prelude hiding (Left, Right)

-- | Rozwiązuje kostkę metodą CFOP i zwraca:
--   (finalCube, [(phaseName, Move)])
solveCubePhases :: Cube -> (Cube, [(String, Move)])
solveCubePhases c0 =
  -- Faza 1: White Cross
  let (c1, movesCross) = solveWhiteCross (c0, [])
      taggedCross = map (\mv -> ("White Cross", mv)) movesCross

      -- Faza 2: F2L
      (c2, movesF2L) = solveF2L (c1, [])
      taggedF2L = map (\mv -> ("F2L", mv)) movesF2L

      -- Faza 3: OLL
      (c3, movesOLL) = solveOLL (c2, [])
      taggedOLL = map (\mv -> ("OLL", mv)) movesOLL

      -- Faza 4: PLL
      (c4, movesPLL) = solvePLL (c3, [])
      taggedPLL = map (\mv -> ("PLL", mv)) movesPLL

      allTagged = taggedCross ++ taggedF2L ++ taggedOLL ++ taggedPLL
   in (c4, allTagged)
