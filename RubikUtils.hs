module RubikUtils
  ( emptyCubeX
  , replaceFace
  , rotateFaceCW
  , rotateFaceCCW
  , printCube
  ) where

import Prelude
import RubikTypes

-- | Tworzy "pustą" kostkę: 6 ścian w standardowej kolejności,
--   każda wypełniona np. kolorem White - do nadpisania przez parser.
emptyCubeX :: CubeX
emptyCubeX =
  [ (FFront, replicate 9 CWhite),
    (FRight, replicate 9 CWhite),
    (FBack, replicate 9 CWhite),
    (FLeft, replicate 9 CWhite),
    (FUp, replicate 9 CWhite),
    (FDown, replicate 9 CWhite)
  ]

-- | Podmienia daną ściankę w strukturze 'CubeX' na nową (side, [ColorX]).
replaceFace :: FaceSide -> FaceX -> CubeX -> CubeX
replaceFace _    _    [] = []
replaceFace side newFace (f@(s,_):rest)
  | s == side  = newFace : rest
  | otherwise  = f : replaceFace side newFace rest

-- | Obrót 9-elementowej listy zgodnie z ruchem wskazówek zegara (3x3).
--   Indeksy w 3x3 macierzy:
--     0 1 2
--     3 4 5
--     6 7 8
rotateFaceCW :: [ColorX] -> [ColorX]
rotateFaceCW face =
  [ face !! 6, face !! 3, face !! 0
  , face !! 7, face !! 4, face !! 1
  , face !! 8, face !! 5, face !! 2
  ]

-- | Obrót 3x3 w stronę przeciwną
rotateFaceCCW :: [ColorX] -> [ColorX]
rotateFaceCCW face =
  [ face !! 2, face !! 5, face !! 8
  , face !! 1, face !! 4, face !! 7
  , face !! 0, face !! 3, face !! 6
  ]

-- | Proste wypisanie kostki na ekran
printCube :: CubeX -> IO ()
printCube [] = return ()
printCube ((side, cols):xs) = do
  putStrLn (show side ++ ": " ++ showColors cols)
  printCube xs

showColors :: [ColorX] -> String
showColors [] = ""
showColors (c:cs) = show c ++ " " ++ showColors cs
