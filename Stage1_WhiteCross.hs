module Stage1_WhiteCross
  ( solveWhiteCrossPhase
  ) where

import Prelude
import RubikTypes
import RubikRotations (addMoveToLog)
import RubikUtils

-- | Główna funkcja fazy 1: biała kreska
--   Zwraca (CubeX, [MoveX]) z ułożonym białym krzyżem na górnej ściance (FUp).
solveWhiteCrossPhase :: CubeWithLog2 -> CubeWithLog2
solveWhiteCrossPhase cwl =
  let cwl' = fixWhiteEdges cwl
  in if checkWhiteCross (fst cwl') then cwl' else solveWhiteCrossPhase cwl'

-- | Sprawdza, czy białe krawędzie (indeksy [1,3,5,7]) na FUp są białe,
--   i czy boczne kolory pasują do docelowych ścian.
checkWhiteCross :: CubeX -> Bool
checkWhiteCross _ = False  -- do zaimplementowania

-- | Poprawia krawędzie białego krzyża
fixWhiteEdges :: CubeWithLog2 -> CubeWithLog2
fixWhiteEdges cwl =
  -- tu np. zrzucamy złe krawędzie i wstawiamy je od dołu
  cwl
