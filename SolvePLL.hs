module SolvePLL
  ( solvePLL,
  )
where

import Rotations
import Types
import Utils
import Prelude hiding (Left, Right)

-------------------------------------------------------------------------------
-- Faza 4: PLL (Permutation of the Last Layer)
-------------------------------------------------------------------------------

-- |
--  Główna funkcja wykonująca etap PLL (Permutation of the Last Layer):
--    1) Permutacja żółtych rogów
--    2) Permutacja żółtych krawędzi
solvePLL :: CubeWithMoves -> CubeWithMoves
solvePLL cw =
  let cwCorners = permuteYellowCorners cw
      cwEdges = permuteYellowEdges cwCorners
   in cwEdges

-------------------------------------------------------------------------------
-- 4.1: Permutacja żółtych rogów
-------------------------------------------------------------------------------

-- |
--  Sprawdza, czy rogi (Front, Left, Right, Back) w indeksach 6 i 8 są już
--  we właściwych kolorach. Jeśli tak – kończy, w przeciwnym razie
--  rekurencyjnie wywołuje 'permuteCornersStep'.
permuteYellowCorners :: CubeWithMoves -> CubeWithMoves
permuteYellowCorners cw
  | areCornersCorrectlyPermuted cw = cw
  | otherwise = permuteYellowCorners (permuteCornersStep cw)

-- |
--  Sprawdza, czy rogi w indeksach 6 i 8 na ścianach (Front, Left, Right, Back)
--  mają docelowe kolory:
--    * Front: (6,8) = Red
--    * Left:  (6,8) = Green
--    * Right: (6,8) = Blue
--    * Back:  (6,8) = Orange
areCornersCorrectlyPermuted :: CubeWithMoves -> Bool
areCornersCorrectlyPermuted cw =
  front !! 6 == Red
    && front !! 8 == Red
    && left !! 6 == Green
    && left !! 8 == Green
    && right !! 6 == Blue
    && right !! 8 == Blue
    && back !! 6 == Orange
    && back !! 8 == Orange
  where
    (front, left, back, right, _, _) = getSides (fst cw)

-- |
--  Sprawdza, czy dwie pary rogów są poprawnie ustawione na przeciwległych krawędziach:
--     - (Front 6 == Red,   Left 8 == Green,  Back 6 == Orange, Right 8 == Blue)
--     - (Front 8 == Red,   Right 6 == Blue,  Back 8 == Orange, Left 6 == Green)
hasTwoCorrectCorners :: CubeWithMoves -> Bool
hasTwoCorrectCorners cw =
  ( front !! 6 == Red
      && left !! 8 == Green
      && back !! 6 == Orange
      && right !! 8 == Blue
  )
    || ( front !! 8 == Red
           && right !! 6 == Blue
           && back !! 8 == Orange
           && left !! 6 == Green
       )
  where
    (front, left, back, right, _, _) = getSides (fst cw)

-- |
--  Jednokrokowa funkcja (rekurencyjnie wywoływana w 'permuteYellowCorners')
--  która:
--   1) sprawdza, czy któraś ściana (Back/Front/Right/Left) ma już poprawne rogi,
--      i wówczas wykonuje ruch `[R', F, R', B, B, R, F', R', B, B, R, R]`.
--   2) jeśli nie, sprawdza 'hasTwoCorrectCorners', i też wykonuje powyższą sekwencję,
--   3) w przeciwnym razie obraca dolną warstwę ([D]) i próbuje ponownie.
permuteCornersStep :: CubeWithMoves -> CubeWithMoves
permuteCornersStep cw
  | back !! 6 == Orange && back !! 8 == Orange =
      applyMovesWhiteDown Front movement cw
  | front !! 6 == Red && front !! 8 == Red =
      applyMovesWhiteDown Back movement cw
  | right !! 6 == Blue && right !! 8 == Blue =
      applyMovesWhiteDown Left movement cw
  | left !! 6 == Green && left !! 8 == Green =
      applyMovesWhiteDown Right movement cw
  | hasTwoCorrectCorners cw =
      applyMovesWhiteDown Front movement cw
  | otherwise =
      applyMovesWhiteUp Front [D] cw
  where
    (front, left, back, right, _, _) = getSides (fst cw)
    movement = [R', F, R', B, B, R, F', R', B, B, R, R]

-------------------------------------------------------------------------------
-- 4.2: Permutacja żółtych krawędzi
-------------------------------------------------------------------------------

-- |
--  Sprawdza, czy krawędzie (Front, Left, Right, Back) w indeksie 7
--  mają już kolory docelowe:
--    * Front(7) = Red
--    * Left(7)  = Green
--    * Right(7) = Blue
--    * Back(7)  = Orange
--
--  Jeśli tak, kończy; w przeciwnym razie rekurencyjnie wywołuje `permuteEdgesStep`.
positionYellowEdges :: CubeWithMoves -> CubeWithMoves
positionYellowEdges cw
  | areEdgesCorrectlyPermuted cw = cw
  | otherwise = positionYellowEdges (permuteEdgesStep cw)

-- |
--  Pomocnicza funkcja, aby utrzymać spójność nazewnictwa:
--  "permuteYellowEdges" = "positionYellowEdges"
--  Zostaje w solvePLL c: cwEdges = positionYellowEdges cwCorners
areEdgesCorrectlyPermuted :: CubeWithMoves -> Bool
areEdgesCorrectlyPermuted cw =
  front !! 7 == Red
    && left !! 7 == Green
    && right !! 7 == Blue
    && back !! 7 == Orange
  where
    (front, left, back, right, _, _) = getSides (fst cw)

-- |
--  Główna funkcja do permutacji (przesunięcia) żółtych krawędzi:
--  - Jeśli któraś ściana jest już w pełni gotowa (`isSideFullyColored`),
--    wykonujemy sekwencję [F,F, U, L, R', F,F, L', R, U, F,F] z translacją
--    na ściance przeciwległej.
--  - Jeśli żadna nie jest gotowa, wykonujemy ruch domyślny na Front.
permuteEdgesStep :: CubeWithMoves -> CubeWithMoves
permuteEdgesStep cw
  | isSideFullyColored cw Front =
      applyMovesWhiteDown Back movement cw
  | isSideFullyColored cw Right =
      applyMovesWhiteDown Left movement cw
  | isSideFullyColored cw Back =
      applyMovesWhiteDown Front movement cw
  | isSideFullyColored cw Left =
      applyMovesWhiteDown Right movement cw
  | otherwise =
      applyMovesWhiteDown Front movement cw
  where
    movement = [F, F, U, L, R', F, F, L', R, U, F, F]

permuteYellowEdges :: CubeWithMoves -> CubeWithMoves
permuteYellowEdges cw
  | areEdgesCorrectlyPermuted cw = cw
  | otherwise = permuteYellowEdges (permuteEdgesStep cw)

-------------------------------------------------------------------------------
-- Dodatkowe funkcje pomocnicze
-------------------------------------------------------------------------------

-- |
--  Sprawdza, czy dana ścianka jest w pełni gotowa (9 pól ma właściwy kolor).
--  Używane w 'permuteEdgesStep'.
isSideFullyColored :: CubeWithMoves -> Side -> Bool
isSideFullyColored cw side =
  all (== targetColor) (getSide side (fst cw))
  where
    targetColor = getTargetSideColor side
