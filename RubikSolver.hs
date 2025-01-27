module RubikSolver
  ( solveEntireCube
  ) where

import Prelude
import RubikTypes
import Stage1_WhiteCross
import Stage2_WhiteCorners
import Stage3_MiddleLayer
import Stage4_LastLayer

-- | Główny solver - wywołuje 4 fazy po kolei.
--   Zwraca finalną kostkę i listę wykonanych ruchów.
solveEntireCube :: CubeX -> (CubeX, [MoveX])
solveEntireCube cube =
  let (c1, m1) = solveWhiteCrossPhase (cube, [])
      (c2, m2) = solveWhiteCornersPhase (c1, m1)
      (c3, m3) = solveMiddleLayerPhase (c2, m2)
      (c4, m4) = solveLastLayerPhase (c3, m3)
  in (c4, m4)
