module SolveWhiteCross where

import Rotations
import Types
import Utils
import Prelude hiding (Left, Right)

-------------------------------------------------------------------------------
-- Faza 1: White Cross (Biały Krzyż)
-------------------------------------------------------------------------------

-- | Główna funkcja układająca biały krzyż na górnej ściance.
-- Sprawdza, czy krzyż jest już ułożony ('checkCrossAndSides').
-- Jeśli tak, zwraca kostkę bez zmian.
-- Jeśli nie, rekurencyjnie wykonuje 'fixWhiteEdges'.
solveWhiteCross :: CubeWithMoves -> CubeWithMoves
solveWhiteCross cw
  | checkCrossAndSides currentCube = cw
  | otherwise = solveWhiteCross $ fixWhiteEdges cw
  where
    (currentCube, _) = cw

-- | Sprawdza, czy górna ścianka ma biały krzyż (indeksy 1,3,5,7 białe).
checkWhiteCross :: Cube -> Side -> Bool
checkWhiteCross cube side =
  up !! 1 == White
    && up !! 3 == White
    && up !! 5 == White
    && up !! 7 == White
  where
    up = getSide side cube

-- | Sprawdza, czy biały krzyż jest poprawnie ułożony:
--   1) 4 krawędzie w ściance 'Up' mają kolor biały (checkWhiteCross),
--   2) Środkowe pola sąsiadujących ścian (Front->Red, Right->Blue, Left->Green, Back->Orange).
checkCrossAndSides :: Cube -> Bool
checkCrossAndSides cube =
  ( front !! 1 == Red
      && right !! 1 == Blue
      && left !! 1 == Green
      && back !! 1 == Orange
  )
    && checkWhiteCross cube Up
  where
    (front, left, back, right, up, down) = getSides cube

-- | Naprawia białe krawędzie w górnej warstwie (White Cross),
--   wywołując kolejno funkcje zajmujące się krawędziami:
--   - Red (Front),
--   - Blue (Right),
--   - Green (Left),
--   - Orange (Back).
fixWhiteEdges :: CubeWithMoves -> CubeWithMoves
fixWhiteEdges =
  fixOrangeEdgeOnUp
    . fixGreenEdgeOnUp
    . fixBlueEdgeOnUp
    . fixRedEdgeOnUp

-------------------------------------------------------------------------------
-- Naprawa krawędzi RED (Front)
-------------------------------------------------------------------------------

fixRedEdgeOnUp :: CubeWithMoves -> CubeWithMoves
fixRedEdgeOnUp cw
  | front !! 1 == White = go $ moveRedEdgeFromDown $ applyMovesWhiteUp Front [F, F] cw
  | front !! 3 == White = go $ moveRedEdgeFromDown $ applyMovesWhiteUp Front [F'] cw
  | front !! 5 == White = go $ moveRedEdgeFromDown $ applyMovesWhiteUp Front [F] cw
  | front !! 7 == White = go $ moveRedEdgeFromDown cw
  | down !! 1 == White = go $ finalizeRedEdgeOnDown cw
  | otherwise = cw
  where
    (currentCube, _) = cw
    front = getSide Front currentCube
    down = getSide Down currentCube
    go = fixRedEdgeOnUp -- rekurencja

-- | Jeśli w dolnej ściance (down !! 1) znajduje się biała krawędź z kolorem Red,
--   przesuwa ją do górnej warstwy.
moveRedEdgeFromDown :: CubeWithMoves -> CubeWithMoves
moveRedEdgeFromDown cw
  | down !! 1 == Red = applyMovesWhiteUp Front [D', L', F, L] cw
  | otherwise = fixRightEdgeFromDown $ applyMovesWhiteUp Front [D] cw
  where
    (c, _) = cw
    down = getSide Down c

-- | Jeżeli krawędź jest już w pozycji 'front !! 7' z kolorem Red,
--   to przesuwamy ją dwukrotnym F. W przeciwnym razie idziemy do kolejnej ścianki.
finalizeRedEdgeOnDown :: CubeWithMoves -> CubeWithMoves
finalizeRedEdgeOnDown cw
  | front !! 7 == Red = applyMovesWhiteUp Front [F, F] cw
  | otherwise = finalizeBlueEdgeOnDown $ applyMovesWhiteUp Front [D] cw
  where
    (c, _) = cw
    front = getSide Front c

-------------------------------------------------------------------------------
-- Naprawa krawędzi BLUE (Right)
-------------------------------------------------------------------------------

fixBlueEdgeOnUp :: CubeWithMoves -> CubeWithMoves
fixBlueEdgeOnUp cw
  | right !! 1 == White = go $ moveBlueEdgeFromDown $ applyMovesWhiteUp Right [F, F] cw
  | right !! 3 == White = go $ moveBlueEdgeFromDown $ applyMovesWhiteUp Right [F'] cw
  | right !! 5 == White = go $ moveBlueEdgeFromDown $ applyMovesWhiteUp Right [F] cw
  | right !! 7 == White = go $ moveBlueEdgeFromDown cw
  | down !! 5 == White = go $ finalizeBlueEdgeOnDown cw
  | otherwise = cw
  where
    (c, _) = cw
    right = getSide Right c
    down = getSide Down c
    go = fixBlueEdgeOnUp

moveBlueEdgeFromDown :: CubeWithMoves -> CubeWithMoves
moveBlueEdgeFromDown cw
  | down !! 5 == Blue = applyMovesWhiteUp Right [D', L', F, L] cw
  | otherwise = fixBackEdgeFromDown $ applyMovesWhiteUp Front [D] cw
  where
    (c, _) = cw
    down = getSide Down c

finalizeBlueEdgeOnDown :: CubeWithMoves -> CubeWithMoves
finalizeBlueEdgeOnDown cw
  | right !! 7 == Blue = applyMovesWhiteUp Right [F, F] cw
  | otherwise = fixBackEdgeOnDown $ applyMovesWhiteUp Front [D] cw
  where
    (c, _) = cw
    right = getSide Right c

-------------------------------------------------------------------------------
-- Naprawa krawędzi GREEN (Left)
-------------------------------------------------------------------------------

fixGreenEdgeOnUp :: CubeWithMoves -> CubeWithMoves
fixGreenEdgeOnUp cw
  | left !! 1 == White = go $ moveGreenEdgeFromDown $ applyMovesWhiteUp Left [F, F] cw
  | left !! 3 == White = go $ moveGreenEdgeFromDown $ applyMovesWhiteUp Left [F'] cw
  | left !! 5 == White = go $ moveGreenEdgeFromDown $ applyMovesWhiteUp Left [F] cw
  | left !! 7 == White = go $ moveGreenEdgeFromDown cw
  | down !! 3 == White = go $ finalizeGreenEdgeOnDown cw
  | otherwise = cw
  where
    (c, _) = cw
    left = getSide Left c
    down = getSide Down c
    go = fixGreenEdgeOnUp

moveGreenEdgeFromDown :: CubeWithMoves -> CubeWithMoves
moveGreenEdgeFromDown cw
  | down !! 3 == Green = applyMovesWhiteUp Left [D', L', F, L] cw
  | otherwise = moveRedEdgeFromDown $ applyMovesWhiteUp Front [D] cw
  where
    (c, _) = cw
    down = getSide Down c

finalizeGreenEdgeOnDown :: CubeWithMoves -> CubeWithMoves
finalizeGreenEdgeOnDown cw
  | left !! 7 == Green = applyMovesWhiteUp Left [F, F] cw
  | otherwise = finalizeRedEdgeOnDown $ applyMovesWhiteUp Front [D] cw
  where
    (c, _) = cw
    left = getSide Left c

-------------------------------------------------------------------------------
-- Naprawa krawędzi ORANGE (Back)
-------------------------------------------------------------------------------

fixOrangeEdgeOnUp :: CubeWithMoves -> CubeWithMoves
fixOrangeEdgeOnUp cw
  | back !! 1 == White = go $ moveOrangeEdgeFromDown $ applyMovesWhiteUp Back [F, F] cw
  | back !! 3 == White = go $ moveOrangeEdgeFromDown $ applyMovesWhiteUp Back [F'] cw
  | back !! 5 == White = go $ moveOrangeEdgeFromDown $ applyMovesWhiteUp Back [F] cw
  | back !! 7 == White = go $ moveOrangeEdgeFromDown cw
  | down !! 7 == White = go $ finalizeOrangeEdgeOnDown cw
  | otherwise = cw
  where
    (c, _) = cw
    back = getSide Back c
    down = getSide Down c
    go = fixOrangeEdgeOnUp

moveOrangeEdgeFromDown :: CubeWithMoves -> CubeWithMoves
moveOrangeEdgeFromDown cw
  | down !! 7 == Orange = applyMovesWhiteUp Back [D', L', F, L] cw
  | otherwise = moveGreenEdgeFromDown $ applyMovesWhiteUp Front [D] cw
  where
    (c, _) = cw
    down = getSide Down c

finalizeOrangeEdgeOnDown :: CubeWithMoves -> CubeWithMoves
finalizeOrangeEdgeOnDown cw
  | back !! 7 == Orange = applyMovesWhiteUp Back [F, F] cw
  | otherwise = finalizeGreenEdgeOnDown $ applyMovesWhiteUp Front [D] cw
  where
    (c, _) = cw
    back = getSide Back c

-------------------------------------------------------------------------------
-- Pomocnicze funkcje łączące krawędzie
-- (np. z Right przechodzimy do Back itd.)
-------------------------------------------------------------------------------

-- | Gdy przeniesienie Red z dołu się nie powiedzie, próbujemy Blue (Right).
fixRightEdgeFromDown :: CubeWithMoves -> CubeWithMoves
fixRightEdgeFromDown = fixBlueEdgeOnUp

-- | Gdy przeniesienie Blue z dołu się nie powiedzie, próbujemy Back (Orange).
fixBackEdgeFromDown :: CubeWithMoves -> CubeWithMoves
fixBackEdgeFromDown = fixOrangeEdgeOnUp

-- | Gdy naprawa Blue na dole się nie uda, idziemy do Orange
fixBackEdgeOnDown :: CubeWithMoves -> CubeWithMoves
fixBackEdgeOnDown = finalizeOrangeEdgeOnDown
