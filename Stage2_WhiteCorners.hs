module Stage2_WhiteCorners
  ( solveWhiteCornersPhase
  ) where

import Prelude
import RubikTypes
import RubikRotations
import RubikUtils

solveWhiteCornersPhase :: CubeWithLog2 -> CubeWithLog2
solveWhiteCornersPhase cwl =
  let next = fixWhiteCorners cwl
  in if checkWhiteCorners (fst next) then next
                                     else solveWhiteCornersPhase next

checkWhiteCorners :: CubeX -> Bool
checkWhiteCorners _ = False  -- do wypełnienia

fixWhiteCorners :: CubeWithLog2 -> CubeWithLog2
fixWhiteCorners cwl = cwl  -- do wypełnienia
