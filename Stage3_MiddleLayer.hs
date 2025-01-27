module Stage3_MiddleLayer
  ( solveMiddleLayerPhase
  ) where

import Prelude
import RubikTypes
import RubikRotations
import RubikUtils

solveMiddleLayerPhase :: CubeWithLog2 -> CubeWithLog2
solveMiddleLayerPhase cwl =
  let attempt = fixMidlayer cwl
  in if checkMidlayer (fst attempt) then attempt
                                    else solveMiddleLayerPhase attempt

checkMidlayer :: CubeX -> Bool
checkMidlayer _ = False  -- do wypełnienia

fixMidlayer :: CubeWithLog2 -> CubeWithLog2
fixMidlayer cwl = cwl  -- do wypełnienia
