module RubikParser
  ( parseRubikFile
  ) where

import Prelude
import RubikTypes (CubeX, FaceX, FaceSide(..), ColorX(..))
import RubikUtils (replaceFace, emptyCubeX)

{-
  Moduł odpowiedzialny za wczytanie i sparsowanie pliku.
  Zakładamy, że plik zawiera 6 linii w postaci:
     Front White White White White White White White White White
     Right ...
     Back ...
     Left ...
     Up ...
     Down ...
  Gdzie w jednej linii jest nazwa ścianki + 9 kolorów w postaci stringów
-}

parseRubikFile :: String -> CubeX
parseRubikFile contents =
  let ls = lines contents
      c0 = emptyCubeX
  in fillCube c0 ls

-- | Wypełnia pustą kostkę danymi z linii.
fillCube :: CubeX -> [String] -> CubeX
fillCube cube [] = cube
fillCube cube (l:ls) =
  let faceParsed = parseLine l
      newCube    = insertFace cube faceParsed
  in fillCube newCube ls

parseLine :: String -> FaceX
parseLine line =
  let ws = words line
  in case ws of
       (sideName : rest) ->
         let side   = parseFaceSide sideName
             colors = map parseColorX rest
         in (side, colors)
       _ -> error "Linia niepoprawna"

parseFaceSide :: String -> FaceSide
parseFaceSide nm =
  case nm of
    "Front" -> FFront
    "Back"  -> FBack
    "Left"  -> FLeft
    "Right" -> FRight
    "Up"    -> FUp
    "Down"  -> FDown
    _       -> error "Nieznana nazwa ścianki"

parseColorX :: String -> ColorX
parseColorX str =
  case str of
    "White"  -> CWhite
    "Yellow" -> CYellow
    "Red"    -> CRed
    "Orange" -> COrange
    "Green"  -> CGreen
    "Blue"   -> CBlue
    _        -> error ("Nieznany kolor: " ++ str)

-- | Wstawia gotową ściankę (side, [ColorX]) do kostki.
insertFace :: CubeX -> FaceX -> CubeX
insertFace cube face@(side, _) =
  replaceFace side face cube
