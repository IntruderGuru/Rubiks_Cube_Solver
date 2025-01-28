module SolveF2L
  ( solveF2L,
  )
where

import Rotations
import Types
import Utils
import Prelude hiding (Left, Right)

-------------------------------------------------------------------------------
-- Faza 2: F2L (First Two Layers) - Białe rogi + Środkowa warstwa
-------------------------------------------------------------------------------

-- | Główna funkcja scalająca etap F2L:
--   1. Ułożenie białych rogów
--   2. Ułożenie środkowej warstwy
solveF2L :: CubeWithMoves -> CubeWithMoves
solveF2L cw =
  let cwCorners = solveWhiteCorners cw
   in solveMidLayerEdges cwCorners

-------------------------------------------------------------------------------
-- 1) Białe rogi (White Corners)
-------------------------------------------------------------------------------

-- |
--  Główna funkcja do układania białych rogów na górnej ściance.
--  * Jeśli rogi są już poprawnie ułożone (sprawdza 'areWhiteCornersAligned'),
--    zwraca stan bez zmian.
--  * W przeciwnym razie rekurencyjnie wywołuje 'adjustWhiteCorners'.
solveWhiteCorners :: CubeWithMoves -> CubeWithMoves
solveWhiteCorners cw
  | areWhiteCornersAligned cw = cw
  | otherwise = solveWhiteCorners (adjustWhiteCorners cw)

-- |
--  Sprawdza, czy wszystkie białe rogi są już ułożone:
--  1. Czy cztery rogi górnej ścianki (Up) - indeksy 0,2,6,8 - są białe.
--  2. Czy odpowiadające im rogi na ścianach (Front, Right, Left, Back)
--     mają właściwy kolor na pozycjach 0 i 2.
areWhiteCornersAligned :: CubeWithMoves -> Bool
areWhiteCornersAligned cw =
  checkWhiteUp cw
    && cornerHasProperColors Front front
    && cornerHasProperColors Right right
    && cornerHasProperColors Left left
    && cornerHasProperColors Back back
  where
    (cube, _) = cw
    (front, left, back, right, up, down) = getSides cube

    cornerHasProperColors :: Side -> [Color] -> Bool
    cornerHasProperColors side colors =
      colors !! 0 == getTargetSideColor side
        && colors !! 2 == getTargetSideColor side

-- | Sprawdza, czy cztery rogi Up (0,2,6,8) są białe.
checkWhiteUp :: CubeWithMoves -> Bool
checkWhiteUp (c, _) =
  up !! 0 == White
    && up !! 2 == White
    && up !! 6 == White
    && up !! 8 == White
  where
    up = getSide Up c

-- |
--  Główna funkcja naprawcza (rekurencyjna) dla białych rogów:
--  1) Zrzuca niepoprawnie ułożone białe rogi z góry na dół ('dropWrongWhiteCorners'),
--  2) Następnie poprawia każdy róg (Front, Right, Left, Back).
adjustWhiteCorners :: CubeWithMoves -> CubeWithMoves
adjustWhiteCorners =
  fixCornerBackUp
    . fixCornerLeftUp
    . fixCornerRightUp
    . fixCornerFrontUp
    . dropWrongWhiteCorners

-------------------------------------------------------------------------------
-- a) Zrzucanie błędnych rogów na dół
-------------------------------------------------------------------------------

-- |
--  Jeśli w rogu górnej ścianki pojawia się biel, ale są tam nieprawidłowe kolory
--  (np. powinna być para Red/Green, a mamy inne kolory),
--  wykonujemy sekwencję ruchów (z translacją white-up), by zrzucić ten róg na dół.
--
--  Funkcja wywołuje się rekurencyjnie, aż wszystkie błędne rogi zostaną zrzucone.
dropWrongWhiteCorners :: CubeWithMoves -> CubeWithMoves
dropWrongWhiteCorners cw
  | up !! 6 == White && (front !! 0 /= Red || left !! 2 /= Green) =
      go $ applyMovesWhiteUp Front [L, D, L'] cw
  | up !! 8 == White && (front !! 2 /= Red || right !! 0 /= Blue) =
      go $ applyMovesWhiteUp Right [L, D, L'] cw
  | up !! 0 == White && (back !! 0 /= Orange || left !! 0 /= Green) =
      go $ applyMovesWhiteUp Left [L, D, L'] cw
  | up !! 2 == White && (right !! 2 /= Blue || back !! 0 /= Orange) =
      go $ applyMovesWhiteUp Back [L, D, L'] cw
  | otherwise = cw
  where
    (cube, _) = cw
    (front, left, back, right, up, down) = getSides cube
    go = dropWrongWhiteCorners -- rekurencja

-------------------------------------------------------------------------------
-- b) Poprawianie rogów na ściankach (Front, Right, Left, Back)
-------------------------------------------------------------------------------

---------------------------------------
-- FRONT
---------------------------------------

fixCornerFrontUp :: CubeWithMoves -> CubeWithMoves
fixCornerFrontUp cw
  | front !! 0 == White = go $ fixCornerFrontDownLeft $ applyMovesWhiteUp Front [F', D', F, D] cw
  | front !! 2 == White = go $ fixCornerFrontDownRight $ applyMovesWhiteUp Front [F, D, F', D'] cw
  | front !! 6 == White = go $ fixCornerFrontDownLeft cw
  | front !! 8 == White = go $ fixCornerFrontDownRight cw
  | down !! 0 == White = go $ shiftCornerDownToFront cw
  | down !! 2 == White = go $ applyMovesWhiteUp Front [D'] cw
  | otherwise = cw
  where
    (c, _) = cw
    front = getSide Front c
    down = getSide Down c
    go = fixCornerFrontUp

fixCornerFrontDownLeft :: CubeWithMoves -> CubeWithMoves
fixCornerFrontDownLeft cw
  | down !! 0 == Red = applyMovesWhiteUp Front [D, L, D', L'] cw
  | otherwise = fixCornerRightDownLeft $ applyMovesWhiteUp Front [D] cw
  where
    (c, _) = cw
    down = getSide Down c

fixCornerFrontDownRight :: CubeWithMoves -> CubeWithMoves
fixCornerFrontDownRight cw
  | down !! 2 == Red = applyMovesWhiteUp Front [D', R', D, R] cw
  | otherwise = fixCornerRightDownRight $ applyMovesWhiteUp Front [D] cw
  where
    (c, _) = cw
    down = getSide Down c

-- | Jeśli na dole mamy biały róg dla Front,
--   sprawdzamy, czy jest już w odpowiednim miejscu (left !! 8 == Red, front !! 6 == Green).
shiftCornerDownToFront :: CubeWithMoves -> CubeWithMoves
shiftCornerDownToFront cw
  | left !! 8 == Red && front !! 6 == Green = applyMovesWhiteUp Front [D, L, D', L'] cw
  | otherwise = shiftCornerDownToRight $ applyMovesWhiteUp Front [D] cw
  where
    (c, _) = cw
    left = getSide Left c
    front = getSide Front c

---------------------------------------
-- RIGHT
---------------------------------------

fixCornerRightUp :: CubeWithMoves -> CubeWithMoves
fixCornerRightUp cw
  | right !! 0 == White = go $ fixCornerRightDownLeft $ applyMovesWhiteUp Right [F', D', F, D] cw
  | right !! 2 == White = go $ fixCornerRightDownRight $ applyMovesWhiteUp Right [F, D, F', D'] cw
  | right !! 6 == White = go $ fixCornerRightDownLeft cw
  | right !! 8 == White = go $ fixCornerRightDownRight cw
  | down !! 2 == White = go $ shiftCornerDownToRight cw
  | down !! 8 == White = go $ applyMovesWhiteUp Right [D'] cw
  | otherwise = cw
  where
    (c, _) = cw
    right = getSide Right c
    down = getSide Down c
    go = fixCornerRightUp

fixCornerRightDownLeft :: CubeWithMoves -> CubeWithMoves
fixCornerRightDownLeft cw
  | down !! 2 == Blue = applyMovesWhiteUp Right [D, L, D', L'] cw
  | otherwise = fixCornerBackDownLeft $ applyMovesWhiteUp Right [D] cw
  where
    (c, _) = cw
    down = getSide Down c

fixCornerRightDownRight :: CubeWithMoves -> CubeWithMoves
fixCornerRightDownRight cw
  | down !! 8 == Blue = applyMovesWhiteUp Right [D', R', D, R] cw
  | otherwise = fixCornerBackDownRight $ applyMovesWhiteUp Right [D] cw
  where
    (c, _) = cw
    down = getSide Down c

-- | Jeśli na dole mamy biały róg do Right, sprawdzamy, czy front !! 8 == Blue, right !! 6 == Red.
shiftCornerDownToRight :: CubeWithMoves -> CubeWithMoves
shiftCornerDownToRight cw
  | front !! 8 == Blue && right !! 6 == Red = applyMovesWhiteUp Right [D, L, D', L'] cw
  | otherwise = shiftCornerDownToBack (applyMovesWhiteUp Right [D] cw)
  where
    (c, _) = cw
    front = getSide Front c
    right = getSide Right c

---------------------------------------
-- LEFT
---------------------------------------

fixCornerLeftUp :: CubeWithMoves -> CubeWithMoves
fixCornerLeftUp cw
  | left !! 0 == White = go $ fixCornerLeftDownLeft $ applyMovesWhiteUp Left [F', D', F, D] cw
  | left !! 2 == White = go $ fixCornerLeftDownRight $ applyMovesWhiteUp Left [F, D, F', D'] cw
  | left !! 6 == White = go $ fixCornerLeftDownLeft cw
  | left !! 8 == White = go $ fixCornerLeftDownRight cw
  | down !! 6 == White = go $ shiftCornerDownToLeft cw
  | down !! 0 == White = go $ applyMovesWhiteUp Left [D'] cw
  | otherwise = cw
  where
    (c, _) = cw
    left = getSide Left c
    down = getSide Down c
    go = fixCornerLeftUp

fixCornerLeftDownLeft :: CubeWithMoves -> CubeWithMoves
fixCornerLeftDownLeft cw
  | down !! 6 == Green = applyMovesWhiteUp Left [D, L, D', L'] cw
  | otherwise = fixCornerFrontDownLeft $ applyMovesWhiteUp Left [D] cw
  where
    (c, _) = cw
    down = getSide Down c

fixCornerLeftDownRight :: CubeWithMoves -> CubeWithMoves
fixCornerLeftDownRight cw
  | down !! 0 == Green = applyMovesWhiteUp Left [D', R', D, R] cw
  | otherwise = fixCornerFrontDownRight $ applyMovesWhiteUp Left [D] cw
  where
    (c, _) = cw
    down = getSide Down c

shiftCornerDownToLeft :: CubeWithMoves -> CubeWithMoves
shiftCornerDownToLeft cw
  | back !! 8 == Green && left !! 6 == Orange = applyMovesWhiteUp Left [D, L, D', L'] cw
  | otherwise = shiftCornerDownToFront (applyMovesWhiteUp Left [D] cw)
  where
    (c, _) = cw
    back = getSide Back c
    left = getSide Left c

---------------------------------------
-- BACK
---------------------------------------

fixCornerBackUp :: CubeWithMoves -> CubeWithMoves
fixCornerBackUp cw
  | back !! 0 == White = go $ fixCornerBackDownLeft $ applyMovesWhiteUp Back [F', D', F, D] cw
  | back !! 2 == White = go $ fixCornerBackDownRight $ applyMovesWhiteUp Back [F, D, F', D'] cw
  | back !! 6 == White = go $ fixCornerBackDownLeft cw
  | back !! 8 == White = go $ fixCornerBackDownRight cw
  | down !! 8 == White = go $ shiftCornerDownToBack cw
  | down !! 6 == White = go $ applyMovesWhiteUp Back [D'] cw
  | otherwise = cw
  where
    (c, _) = cw
    back = getSide Back c
    down = getSide Down c
    go = fixCornerBackUp

fixCornerBackDownLeft :: CubeWithMoves -> CubeWithMoves
fixCornerBackDownLeft cw
  | down !! 8 == Orange = applyMovesWhiteUp Back [D, L, D', L'] cw
  | otherwise = fixCornerLeftDownLeft $ applyMovesWhiteUp Back [D] cw
  where
    (c, _) = cw
    down = getSide Down c

fixCornerBackDownRight :: CubeWithMoves -> CubeWithMoves
fixCornerBackDownRight cw
  | down !! 6 == Orange = applyMovesWhiteUp Back [D', R', D, R] cw
  | otherwise = fixCornerLeftDownRight $ applyMovesWhiteUp Back [D] cw
  where
    (c, _) = cw
    down = getSide Down c

shiftCornerDownToBack :: CubeWithMoves -> CubeWithMoves
shiftCornerDownToBack cw
  | right !! 8 == Orange && back !! 6 == Blue = applyMovesWhiteUp Back [D, L, D', L'] cw
  | otherwise = shiftCornerDownToLeft (applyMovesWhiteUp Back [D] cw)
  where
    (c, _) = cw
    right = getSide Right c
    back = getSide Back c

-------------------------------------------------------------------------------
-- 2) Środkowa warstwa (Mid Layer)
-------------------------------------------------------------------------------

-- | Układanie środkowej warstwy:
--   - sprawdza, czy jest ułożona (areMidLayerEdgesAligned),
--   - jeśli nie, rekurencyjnie wywołuje 'fixMidLayerEdges'.
solveMidLayerEdges :: CubeWithMoves -> CubeWithMoves
solveMidLayerEdges cw
  | areMidLayerEdgesAligned cw = cw
  | otherwise = solveMidLayerEdges (fixMidLayerEdges cw)

-- | Sprawdza, czy środkowa warstwa (indeksy 3,5 na Front,Left,Right,Back) jest OK.
--   * Front (3,5) = Red
--   * Left  (3,5) = Green
--   * Back  (3,5) = Orange
--   * Right (3,5) = Blue
areMidLayerEdgesAligned :: CubeWithMoves -> Bool
areMidLayerEdgesAligned (cube, _) =
  front !! 3 == Red
    && front !! 5 == Red
    && left !! 3 == Green
    && left !! 5 == Green
    && back !! 3 == Orange
    && back !! 5 == Orange
    && right !! 3 == Blue
    && right !! 5 == Blue
  where
    (front, left, back, right, _, _) = getSides cube

-- | Główna funkcja naprawcza dla środkowej warstwy:
--   1) przenosi nieprawidłowe krawędzie do góry (moveInvalidEdges...),
--   2) próbuje wstawić je poprawnie (fix...MidLayer).
fixMidLayerEdges :: CubeWithMoves -> CubeWithMoves
fixMidLayerEdges cw =
  moveInvalidEdgesFront2Up $
    fixLeftMidLayerEdge $
      fixBackMidLayerEdge $
        fixRightMidLayerEdge $
          fixFrontMidLayerEdge cw

-------------------------------------------------------------------------------
-- a) Przenoszenie błędnych krawędzi do góry
-------------------------------------------------------------------------------

moveInvalidEdgesFront2Up :: CubeWithMoves -> CubeWithMoves
moveInvalidEdgesFront2Up cw
  | front !! 3 /= Red || left !! 5 /= Green =
      fixFrontMidLayerEdge $ applyMovesWhiteDown Front [U, R, U', R', U', F', U, F] cw
  | otherwise =
      moveInvalidEdgesRight2Up cw
  where
    (cube, _) = cw
    (front, left, back, right, _, _) = getSides cube

moveInvalidEdgesRight2Up :: CubeWithMoves -> CubeWithMoves
moveInvalidEdgesRight2Up cw
  | front !! 5 /= Red || right !! 3 /= Blue =
      fixRightMidLayerEdge $ applyMovesWhiteDown Right [U, R, U', R', U', F', U, F] cw
  | otherwise =
      moveInvalidEdgesBack2Up cw
  where
    (cube, _) = cw
    (front, left, back, right, _, _) = getSides cube

moveInvalidEdgesBack2Up :: CubeWithMoves -> CubeWithMoves
moveInvalidEdgesBack2Up cw
  | right !! 5 /= Blue || back !! 3 /= Orange =
      fixBackMidLayerEdge $ applyMovesWhiteDown Back [U, R, U', R', U', F', U, F] cw
  | otherwise =
      moveInvalidEdgesLeft2Up cw
  where
    (cube, _) = cw
    (front, left, back, right, _, _) = getSides cube

moveInvalidEdgesLeft2Up :: CubeWithMoves -> CubeWithMoves
moveInvalidEdgesLeft2Up cw
  | back !! 5 /= Orange || left !! 3 /= Green =
      fixLeftMidLayerEdge $ applyMovesWhiteDown Left [U, R, U', R', U', F', U, F] cw
  | otherwise = cw
  where
    (cube, _) = cw
    (front, left, back, right, _, _) = getSides cube

-------------------------------------------------------------------------------
-- b) Wstawianie krawędzi w środkową warstwę (Front / Right / Back / Left)
-------------------------------------------------------------------------------

fixFrontMidLayerEdge :: CubeWithMoves -> CubeWithMoves
fixFrontMidLayerEdge cw
  | front !! 7 /= Yellow && down !! 1 /= Yellow = insertEdgeFrontMidLayer cw
  | otherwise = cw
  where
    (cube, _) = cw
    front = getSide Front cube
    down = getSide Down cube

fixRightMidLayerEdge :: CubeWithMoves -> CubeWithMoves
fixRightMidLayerEdge cw
  | right !! 7 /= Yellow && down !! 5 /= Yellow = insertEdgeRightMidLayer cw
  | otherwise = cw
  where
    (cube, _) = cw
    right = getSide Right cube
    down = getSide Down cube

fixBackMidLayerEdge :: CubeWithMoves -> CubeWithMoves
fixBackMidLayerEdge cw
  | back !! 7 /= Yellow && down !! 7 /= Yellow = insertEdgeBackMidLayer cw
  | otherwise = cw
  where
    (cube, _) = cw
    back = getSide Back cube
    down = getSide Down cube

fixLeftMidLayerEdge :: CubeWithMoves -> CubeWithMoves
fixLeftMidLayerEdge cw
  | left !! 7 /= Yellow && down !! 3 /= Yellow = insertEdgeLeftMidLayer cw
  | otherwise = cw
  where
    (cube, _) = cw
    left = getSide Left cube
    down = getSide Down cube

-------------------------------------------------------------------------------
-- c) Konkretne sekwencje wstawiające krawędź w odpowiednie miejsce
-------------------------------------------------------------------------------

-- |
--  Jeśli 'front !! 7 == Red' i 'down !! 1 == Blue',
--     wykonujemy [U', L', U, L, U, F, U', F'] (przesuwa krawędź do Right).
--  Jeśli 'front !! 7 == Red' i 'down !! 1 == Green',
--     wykonujemy [U, R, U', R', U', F', U, F] (przesuwa krawędź do Left).
--  W innym wypadku obracamy dolną warstwę ([D]) i próbujemy wstawić krawędź do Right.
insertEdgeFrontMidLayer :: CubeWithMoves -> CubeWithMoves
insertEdgeFrontMidLayer cw
  | front !! 7 == Red && down !! 1 == Blue =
      applyMovesWhiteDown Front [U', L', U, L, U, F, U', F'] cw
  | front !! 7 == Red && down !! 1 == Green =
      applyMovesWhiteDown Front [U, R, U', R', U', F', U, F] cw
  | otherwise =
      insertEdgeRightMidLayer (applyMovesWhiteUp Front [D] cw)
  where
    (c, _) = cw
    front = getSide Front c
    down = getSide Down c

insertEdgeRightMidLayer :: CubeWithMoves -> CubeWithMoves
insertEdgeRightMidLayer cw
  | right !! 7 == Blue && down !! 5 == Orange =
      applyMovesWhiteDown Right [U', L', U, L, U, F, U', F'] cw
  | right !! 7 == Blue && down !! 5 == Red =
      applyMovesWhiteDown Right [U, R, U', R', U', F', U, F] cw
  | otherwise =
      insertEdgeBackMidLayer (applyMovesWhiteUp Front [D] cw)
  where
    (c, _) = cw
    right = getSide Right c
    down = getSide Down c

insertEdgeBackMidLayer :: CubeWithMoves -> CubeWithMoves
insertEdgeBackMidLayer cw
  | back !! 7 == Orange && down !! 7 == Green =
      applyMovesWhiteDown Back [U', L', U, L, U, F, U', F'] cw
  | back !! 7 == Orange && down !! 7 == Blue =
      applyMovesWhiteDown Back [U, R, U', R', U', F', U, F] cw
  | otherwise =
      insertEdgeLeftMidLayer (applyMovesWhiteUp Front [D] cw)
  where
    (c, _) = cw
    back = getSide Back c
    down = getSide Down c

insertEdgeLeftMidLayer :: CubeWithMoves -> CubeWithMoves
insertEdgeLeftMidLayer cw
  | left !! 7 == Green && down !! 3 == Red =
      applyMovesWhiteDown Left [U', L', U, L, U, F, U', F'] cw
  | left !! 7 == Green && down !! 3 == Orange =
      applyMovesWhiteDown Left [U, R, U', R', U', F', U, F] cw
  | otherwise =
      insertEdgeFrontMidLayer (applyMovesWhiteUp Front [D] cw)
  where
    (c, _) = cw
    left = getSide Left c
    down = getSide Down c
