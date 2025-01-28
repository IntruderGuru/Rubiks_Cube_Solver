module SolveOLL
  ( solveOLL,
  )
where

import Rotations
import Types
import Utils
import Prelude hiding (Left, Right)

-------------------------------------------------------------------------------
-- Faza 3: OLL (Orientation of the Last Layer)
-------------------------------------------------------------------------------

-- |
--  Główna funkcja wykonująca etap OLL (Orientacja Ostatniej Warstwy):
--   1) Ułożenie żółtego krzyża (krawędzi),
--   2) Ułożenie (zorientowanie) żółtych rogów.
solveOLL :: CubeWithMoves -> CubeWithMoves
solveOLL cw =
  let cwCross = completeYellowCross cw -- ułożenie żółtego krzyża
      cwCorners = orientYellowCorners cwCross -- orientacja żółtych rogów
   in cwCorners

-------------------------------------------------------------------------------
-- 3.1 - Ułożenie (orientacja) żółtego krzyża
-------------------------------------------------------------------------------

-- |
--  Główna funkcja do ułożenia żółtego krzyża na dolnej ściance (Down).
--  Sprawdza, czy krawędzie Down (indeksy 1,3,5,7) są koloru Yellow (`isYellowCrossDone`).
--  Jeśli tak, zwraca stan bez zmian.
--  W przeciwnym razie wywołuje rekurencyjnie `fixYellowEdgesCross`.
completeYellowCross :: CubeWithMoves -> CubeWithMoves
completeYellowCross cw
  | isYellowCrossDone cw = cw
  | otherwise = completeYellowCross (fixYellowEdgesCross cw)

-- | Sprawdza, czy 4 krawędzie dolnej ścianki (Down) - indeksy 1,3,5,7 - są żółte.
isYellowCrossDone :: CubeWithMoves -> Bool
isYellowCrossDone cw =
  down !! 1 == Yellow
    && down !! 3 == Yellow
    && down !! 5 == Yellow
    && down !! 7 == Yellow
  where
    down = getSide Down (fst cw)

-- |
--  Naprawia (ustawia) żółty krzyż na dolnej ściance,
--  używając sekwencji ruchów [F, U, R, U', R', F'].
--
--  W zależności od tego, które krawędzie są już żółte (down !! 1,3,5,7),
--  wybiera odpowiednią ściankę (Front, Right, Left, Back), na której wykonuje ruch.
--
--  Jeśli żadna konfiguracja nie pasuje (else), domyślnie używa Front.
fixYellowEdgesCross :: CubeWithMoves -> CubeWithMoves
fixYellowEdgesCross cw
  | down !! 1 == Yellow && down !! 3 == Yellow =
      applyMovesWhiteDown Back [F, U, R, U', R', F'] cw
  | down !! 1 == Yellow && down !! 5 == Yellow =
      applyMovesWhiteDown Left [F, U, R, U', R', F'] cw
  | down !! 5 == Yellow && down !! 7 == Yellow =
      applyMovesWhiteDown Front [F, U, R, U', R', F'] cw
  | down !! 3 == Yellow && down !! 7 == Yellow =
      applyMovesWhiteDown Right [F, U, R, U', R', F'] cw
  | down !! 1 == Yellow && down !! 7 == Yellow =
      applyMovesWhiteDown Left [F, U, R, U', R', F'] cw
  | down !! 3 == Yellow && down !! 5 == Yellow =
      applyMovesWhiteDown Front [F, U, R, U', R', F'] cw
  | otherwise =
      applyMovesWhiteDown Front [F, U, R, U', R', F'] cw
  where
    down = getSide Down (fst cw)

-------------------------------------------------------------------------------
-- 3.2 - Ułożenie (orientacja) żółtych rogów
-------------------------------------------------------------------------------

-- |
--  Główna funkcja do orientowania żółtych rogów.
--  Sprawdza, czy ścianka 'Down' jest już cała żółta (indeksy 0..8),
--  jeśli tak - nic nie robi.
--  W przeciwnym razie wywołuje rekurencyjnie `fixYellowCornersOrientation`.
orientYellowCorners :: CubeWithMoves -> CubeWithMoves
orientYellowCorners cw
  | isYellowFaceComplete cw = cw
  | otherwise = orientYellowCorners (fixYellowCornersOrientation cw)

-- |
--  Sprawdza, czy wszystkie pola ścianki Down (0..8) są żółte.
--  Czyli czy dolna ścianka jest w całości żółta.
isYellowFaceComplete :: CubeWithMoves -> Bool
isYellowFaceComplete cw =
  all (== Yellow) [down !! i | i <- [0 .. 8], i /= 4]
  where
    -- ewentualnie: [0..8] bo w typowym notacji rubik'a środek i tak jest żółty

    down = getSide Down (fst cw)

-- |
--  Liczy, ile rogów ścianki Down (pozycje 0,2,6,8) jest żółtych,
--  zwraca ich liczbę.
countYellowCorners :: CubeWithMoves -> Int
countYellowCorners cw =
  length $
    filter
      (== Yellow)
      [down !! 0, down !! 2, down !! 6, down !! 8]
  where
    down = getSide Down (fst cw)

-- |
--  Sprawdza, czy cztery rogi ścian (Front, Left, Right, Back)
--  są w prawidłowych kolorach (front=Red, left=Green, right=Blue, back=Orange)
--  na indeksach 6 i 8.
areCornersProperlyColored :: CubeWithMoves -> Bool
areCornersProperlyColored cw =
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
--  Naprawia orientację żółtych rogów według liczby już żółtych rogów.
--  - Jeśli down !! 2 jest żółty i mamy tylko 1 żółty róg, robimy [R, U, R', U, R, U, U, R']
--  - Jeśli right !! 6 jest żółty i (countYellowCorners==0), robimy ...
--  - Jeśli front !! 8 jest żółty i (countYellowCorners==2), robimy ...
--  - inaczej obracamy dół [D] i próbujemy ponownie.
fixYellowCornersOrientation :: CubeWithMoves -> CubeWithMoves
fixYellowCornersOrientation cw
  | down !! 2 == Yellow && (countYellowCorners cw == 1) =
      applyMovesWhiteDown Front [R, U, R', U, R, U, U, R'] cw
  | right !! 6 == Yellow && (countYellowCorners cw == 0) =
      applyMovesWhiteDown Front [R, U, R', U, R, U, U, R'] cw
  | front !! 8 == Yellow && (countYellowCorners cw == 2) =
      applyMovesWhiteDown Front [R, U, R', U, R, U, U, R'] cw
  | otherwise =
      applyMovesWhiteUp Front [D] cw
  where
    (front, left, back, right, up, down) = getSides (fst cw)

-------------------------------------------------------------------------------
-- Pomocnicze Funkcje
-------------------------------------------------------------------------------

-- |
--  Sprawdza, czy dana ściana jest w pełni ułożona (9 pól) w docelowym kolorze
--  (wynik `getTargetSideColor side`).
isSideSolved :: CubeWithMoves -> Side -> Bool
isSideSolved cw side =
  all (== targetColor) (getSide side (fst cw))
  where
    targetColor = getTargetSideColor side

-- |
--  Sprawdza, czy mamy "podwójnie dobrze" ustawione rogi na przeciwległych krawędziach:
--    - (front !! 6 == Red && left !! 8 == Green && back !! 6 == Orange && right !! 8 == Blue)
--    - (front !! 8 == Red && right !! 6 == Blue && back !! 8 == Orange && left !! 6 == Green)
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
