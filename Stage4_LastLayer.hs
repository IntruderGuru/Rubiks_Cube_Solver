module Stage4_LastLayer
  ( solveLastLayerPhase
  ) where

import Prelude
import RubikTypes
import RubikRotations
import RubikUtils

solveLastLayerPhase :: CubeWithLog2 -> CubeWithLog2
solveLastLayerPhase cwl =
  let afterCross    = fixYellowCross cwl
      afterCorners  = fixYellowCorners afterCross
      afterEdges    = fixYellowEdges afterCorners
  in afterEdges

fixYellowCross :: CubeWithLog2 -> CubeWithLog2
fixYellowCross cwl = cwl -- do wypełnienia

fixYellowCorners :: CubeWithLog2 -> CubeWithLog2
fixYellowCorners cwl = cwl

fixYellowEdges :: CubeWithLog2 -> CubeWithLog2
fixYellowEdges cwl = cwl
