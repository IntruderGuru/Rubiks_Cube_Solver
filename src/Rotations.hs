module Rotations where

import Types
import Utils
import Prelude hiding (Left, Right)

--------------------------------------------------------------------------------
-- Zmodyfikowane nazwy i struktura kodu, aby kod był bardziej czytelny i spójny
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Funkcje główne — zarządzanie ruchami, historia ruchów i składanie ruchów
--------------------------------------------------------------------------------

-- |
--  Dla kostki i historii ruchów (`CubeWithMoves` = `(Cube, [Move])`)
--  aplikuje listę ruchów `movesToApply`, najpierw tłumacząc je tak,
--  by ścianka z kolorem białym znalazła się „u góry” (ang. White Up).
--
--  W wyniku zwraca nową krotkę:
--  1. Nowy stan kostki po wykonaniu wszystkich ruchów
--  2. Zaktualizowaną historię ruchów
applyMovesWhiteUp :: Side -> [Move] -> CubeWithMoves -> CubeWithMoves
applyMovesWhiteUp side movesToApply (c, history) =
  (makeMoves translatedMoves c, history ++ translatedMoves)
  where
    translatedMoves = translateMovesWhiteUp side movesToApply

-- |
--  Analogiczne do 'applyMovesWhiteUp', z tą różnicą, że ruchy tłumaczone są
--  tak, by ścianka z kolorem białym wylądowała na dole (ang. White Down).
applyMovesWhiteDown :: Side -> [Move] -> CubeWithMoves -> CubeWithMoves
applyMovesWhiteDown side movesToApply (c, history) =
  (makeMoves translatedMoves c, history ++ translatedMoves)
  where
    translatedMoves = translateMovesWhiteDown side movesToApply

-- |
--  Wykonuje sekwencję ruchów na kostce 'Cube'. Wykorzystuje foldl,
--  który przechodzi przez listę ruchów i nakłada je kolejno na kostkę.
makeMoves :: [Move] -> Cube -> Cube
makeMoves movesToApply c = foldl (flip moveCube) c movesToApply

--------------------------------------------------------------------------------
-- Funkcja 'moveCube' rozdziela rodzaje ruchów na konkretne transformatory stanu
--------------------------------------------------------------------------------

-- |
--  Wykonuje pojedynczy ruch 'Move' na kostce.
--  Każdy konstruktor typu 'Move' ma przypisaną odpowiednią funkcję
--  obracającą właściwe ściany/wiersze/kolumny.
moveCube :: Move -> Cube -> Cube
moveCube U c = moveUp c
moveCube U' c = moveUp' c
moveCube D c = moveDown c
moveCube D' c = moveDown' c
moveCube L c = moveLeft c
moveCube L' c = moveLeft' c
moveCube R c = moveRight c
moveCube R' c = moveRight' c
moveCube B c = moveBack c
moveCube B' c = moveBack' c
moveCube F c = moveFront c
moveCube F' c = moveFront' c

--------------------------------------------------------------------------------
-- Górna ścianka (Up)
--------------------------------------------------------------------------------

-- | Obrót górnej ścianki zgodnie z ruchem zegara
moveUp :: Cube -> Cube
moveUp c = rotateTopSideClockwise $ rotateTopRowClockwise c

-- | Obrót górnej ścianki przeciwnie do ruchu zegara
moveUp' :: Cube -> Cube
moveUp' c = rotateTopRowCounterClockwise $ rotateTopSideCounterClockwise c

--------------------------------------------------------------------------------
-- Dolna ścianka (Down)
--------------------------------------------------------------------------------

-- | Obrót dolnej ścianki zgodnie z ruchem zegara
moveDown :: Cube -> Cube
moveDown c = rotateDownRowCounterClockwise $ rotateDownSideClockwise c

-- | Obrót dolnej ścianki przeciwnie do ruchu zegara
moveDown' :: Cube -> Cube
moveDown' c = rotateDownRowClockwise $ rotateDownSideCounterClockwise c

--------------------------------------------------------------------------------
-- Lewa ścianka (Left)
--------------------------------------------------------------------------------

-- | Obrót lewej ścianki do przodu (z perspektywy gracza patrzącego na Front)
moveLeft :: Cube -> Cube
moveLeft c = rotateLeftSideToFront $ rotateLeftColumnToFront c

-- | Obrót lewej ścianki do tyłu
moveLeft' :: Cube -> Cube
moveLeft' c = rotateLeftSideToBack $ rotateLeftColumnToBack c

--------------------------------------------------------------------------------
-- Prawa ścianka (Right)
--------------------------------------------------------------------------------

-- | Obrót prawej ścianki do tyłu (z perspektywy gracza)
moveRight :: Cube -> Cube
moveRight c = rotateRightSideToBack $ rotateRightColumnToBack c

-- | Obrót prawej ścianki do przodu
moveRight' :: Cube -> Cube
moveRight' c = rotateRightSideToFront $ rotateRightColumnToFront c

--------------------------------------------------------------------------------
-- Przednia ścianka (Front)
--------------------------------------------------------------------------------

-- | Obrót przedniej ścianki zgodnie z ruchem zegara
moveFront :: Cube -> Cube
moveFront c = rotateFrontSideClockwise $ rotateFrontRowsClockwise c

-- | Obrót przedniej ścianki przeciwnie do ruchu zegara
moveFront' :: Cube -> Cube
moveFront' c = rotateFrontSideCounterClockwise $ rotateFrontRowsCounterClockwise c

--------------------------------------------------------------------------------
-- Tylna ścianka (Back)
--------------------------------------------------------------------------------

-- | Obrót tylnej ścianki zgodnie z ruchem zegara
moveBack :: Cube -> Cube
moveBack c = rotateBackSideClockwise $ rotateBackRowsClockwise c

-- | Obrót tylnej ścianki przeciwnie do ruchu zegara
moveBack' :: Cube -> Cube
moveBack' c = rotateBackSideCounterClockwise $ rotateBackRowsCounterClockwise c

--------------------------------------------------------------------------------
-- Obrót całej ścianki (9-elementowa lista kolorów) w obie strony
--------------------------------------------------------------------------------

-- |
--  Obraca listę 9 kolorów (reprezentującą ściankę kostki) zgodnie z ruchem zegara.
--  Zastosowano pattern matching, aby unikać wielokrotnych wywołań '!!'.
rotateSideClockwise :: [Color] -> [Color]
rotateSideClockwise [c0, c1, c2, c3, c4, c5, c6, c7, c8] =
  [ c6,
    c3,
    c0,
    c7,
    c4,
    c1,
    c8,
    c5,
    c2
  ]
-- Jeśli z jakiegoś powodu lista nie ma 9 elementów, zwracamy ją bez zmian
rotateSideClockwise side = side

-- |
--  Obraca listę 9 kolorów (reprezentującą ściankę kostki) przeciwnie do ruchu zegara.
rotateSideCounterClockwise :: [Color] -> [Color]
rotateSideCounterClockwise [c0, c1, c2, c3, c4, c5, c6, c7, c8] =
  [ c2,
    c5,
    c8,
    c1,
    c4,
    c7,
    c0,
    c3,
    c6
  ]
rotateSideCounterClockwise side = side

--------------------------------------------------------------------------------
-- TOP (górne wiersze i ścianka)
--------------------------------------------------------------------------------

rotateTopSideClockwise :: Cube -> Cube
rotateTopSideClockwise c =
  [ (Front, front),
    (Right, right),
    (Back, back),
    (Left, left),
    (Up, rotateSideClockwise up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

rotateTopSideCounterClockwise :: Cube -> Cube
rotateTopSideCounterClockwise c =
  [ (Front, front),
    (Right, right),
    (Back, back),
    (Left, left),
    (Up, rotateSideCounterClockwise up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

rotateTopRowClockwise :: Cube -> Cube
rotateTopRowClockwise c =
  [ (Front, triplet (right !! 0, right !! 1, right !! 2) (0, 1, 2) front),
    (Right, triplet (back !! 0, back !! 1, back !! 2) (0, 1, 2) right),
    (Back, triplet (left !! 0, left !! 1, left !! 2) (0, 1, 2) back),
    (Left, triplet (front !! 0, front !! 1, front !! 2) (0, 1, 2) left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

rotateTopRowCounterClockwise :: Cube -> Cube
rotateTopRowCounterClockwise c =
  [ (Front, triplet (left !! 0, left !! 1, left !! 2) (0, 1, 2) front),
    (Right, triplet (front !! 0, front !! 1, front !! 2) (0, 1, 2) right),
    (Back, triplet (right !! 0, right !! 1, right !! 2) (0, 1, 2) back),
    (Left, triplet (back !! 0, back !! 1, back !! 2) (0, 1, 2) left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

--------------------------------------------------------------------------------
-- DOWN (dolne wiersze i ścianka)
--------------------------------------------------------------------------------

rotateDownSideClockwise :: Cube -> Cube
rotateDownSideClockwise c =
  [ (Front, front),
    (Right, right),
    (Back, back),
    (Left, left),
    (Up, up),
    (Down, rotateSideClockwise down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

rotateDownSideCounterClockwise :: Cube -> Cube
rotateDownSideCounterClockwise c =
  [ (Front, front),
    (Right, right),
    (Back, back),
    (Left, left),
    (Up, up),
    (Down, rotateSideCounterClockwise down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

rotateDownRowClockwise :: Cube -> Cube
rotateDownRowClockwise c =
  [ (Front, triplet (right !! 6, right !! 7, right !! 8) (6, 7, 8) front),
    (Right, triplet (back !! 6, back !! 7, back !! 8) (6, 7, 8) right),
    (Back, triplet (left !! 6, left !! 7, left !! 8) (6, 7, 8) back),
    (Left, triplet (front !! 6, front !! 7, front !! 8) (6, 7, 8) left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

rotateDownRowCounterClockwise :: Cube -> Cube
rotateDownRowCounterClockwise c =
  [ (Front, triplet (left !! 6, left !! 7, left !! 8) (6, 7, 8) front),
    (Right, triplet (front !! 6, front !! 7, front !! 8) (6, 7, 8) right),
    (Back, triplet (right !! 6, right !! 7, right !! 8) (6, 7, 8) back),
    (Left, triplet (back !! 6, back !! 7, back !! 8) (6, 7, 8) left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

--------------------------------------------------------------------------------
-- LEFT (lewe kolumny i ścianka)
--------------------------------------------------------------------------------

rotateLeftColumnToFront :: Cube -> Cube
rotateLeftColumnToFront c =
  [ (Front, triplet (up !! 0, up !! 3, up !! 6) (0, 3, 6) front),
    (Right, right),
    (Back, triplet (down !! 6, down !! 3, down !! 0) (2, 5, 8) back),
    (Left, left),
    (Up, triplet (back !! 8, back !! 5, back !! 2) (0, 3, 6) up),
    (Down, triplet (front !! 0, front !! 3, front !! 6) (0, 3, 6) down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

rotateLeftColumnToBack :: Cube -> Cube
rotateLeftColumnToBack c =
  [ (Front, triplet (down !! 0, down !! 3, down !! 6) (0, 3, 6) front),
    (Right, right),
    (Back, triplet (up !! 6, up !! 3, up !! 0) (2, 5, 8) back),
    (Left, left),
    (Up, triplet (front !! 0, front !! 3, front !! 6) (0, 3, 6) up),
    (Down, triplet (back !! 8, back !! 5, back !! 2) (0, 3, 6) down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

rotateLeftSideToFront :: Cube -> Cube
rotateLeftSideToFront c =
  [ (Front, front),
    (Right, right),
    (Back, back),
    (Left, rotateSideClockwise left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

rotateLeftSideToBack :: Cube -> Cube
rotateLeftSideToBack c =
  [ (Front, front),
    (Right, right),
    (Back, back),
    (Left, rotateSideCounterClockwise left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

--------------------------------------------------------------------------------
-- RIGHT (prawe kolumny i ścianka)
--------------------------------------------------------------------------------

rotateRightColumnToFront :: Cube -> Cube
rotateRightColumnToFront c =
  [ (Front, triplet (up !! 2, up !! 5, up !! 8) (2, 5, 8) front),
    (Right, right),
    (Back, triplet (down !! 8, down !! 5, down !! 2) (0, 3, 6) back),
    (Left, left),
    (Up, triplet (back !! 6, back !! 3, back !! 0) (2, 5, 8) up),
    (Down, triplet (front !! 2, front !! 5, front !! 8) (2, 5, 8) down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

rotateRightColumnToBack :: Cube -> Cube
rotateRightColumnToBack c =
  [ (Front, triplet (down !! 2, down !! 5, down !! 8) (2, 5, 8) front),
    (Right, right),
    (Back, triplet (up !! 8, up !! 5, up !! 2) (0, 3, 6) back),
    (Left, left),
    (Up, triplet (front !! 2, front !! 5, front !! 8) (2, 5, 8) up),
    (Down, triplet (back !! 6, back !! 3, back !! 0) (2, 5, 8) down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

rotateRightSideToFront :: Cube -> Cube
rotateRightSideToFront c =
  [ (Front, front),
    (Right, rotateSideCounterClockwise right),
    (Back, back),
    (Left, left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

rotateRightSideToBack :: Cube -> Cube
rotateRightSideToBack c =
  [ (Front, front),
    (Right, rotateSideClockwise right),
    (Back, back),
    (Left, left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

--------------------------------------------------------------------------------
-- FRONT (przednie wiersze i ścianka)
--------------------------------------------------------------------------------

rotateFrontRowsClockwise :: Cube -> Cube
rotateFrontRowsClockwise c =
  [ (Front, front),
    (Right, triplet (up !! 6, up !! 7, up !! 8) (0, 3, 6) right),
    (Back, back),
    (Left, triplet (down !! 0, down !! 1, down !! 2) (2, 5, 8) left),
    (Up, triplet (left !! 2, left !! 5, left !! 8) (8, 7, 6) up),
    (Down, triplet (right !! 6, right !! 3, right !! 0) (0, 1, 2) down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

rotateFrontRowsCounterClockwise :: Cube -> Cube
rotateFrontRowsCounterClockwise c =
  [ (Front, front),
    (Right, triplet (down !! 2, down !! 1, down !! 0) (0, 3, 6) right),
    (Back, back),
    (Left, triplet (up !! 8, up !! 7, up !! 6) (2, 5, 8) left),
    (Up, triplet (right !! 0, right !! 3, right !! 6) (6, 7, 8) up),
    (Down, triplet (left !! 2, left !! 5, left !! 8) (0, 1, 2) down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

rotateFrontSideClockwise :: Cube -> Cube
rotateFrontSideClockwise c =
  [ (Front, rotateSideClockwise front),
    (Right, right),
    (Back, back),
    (Left, left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

rotateFrontSideCounterClockwise :: Cube -> Cube
rotateFrontSideCounterClockwise c =
  [ (Front, rotateSideCounterClockwise front),
    (Right, right),
    (Back, back),
    (Left, left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

--------------------------------------------------------------------------------
-- BACK (tylne wiersze i ścianka)
--------------------------------------------------------------------------------

rotateBackRowsClockwise :: Cube -> Cube
rotateBackRowsClockwise c =
  [ (Front, front),
    (Right, triplet (down !! 8, down !! 7, down !! 6) (2, 5, 8) right),
    (Back, back),
    (Left, triplet (up !! 2, up !! 1, up !! 0) (0, 3, 6) left),
    (Up, triplet (right !! 2, right !! 5, right !! 8) (0, 1, 2) up),
    (Down, triplet (left !! 0, left !! 3, left !! 6) (6, 7, 8) down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

rotateBackRowsCounterClockwise :: Cube -> Cube
rotateBackRowsCounterClockwise c =
  [ (Front, front),
    (Right, triplet (up !! 0, up !! 1, up !! 2) (2, 5, 8) right),
    (Back, back),
    (Left, triplet (down !! 6, down !! 7, down !! 8) (0, 3, 6) left),
    (Up, triplet (left !! 6, left !! 3, left !! 0) (0, 1, 2) up),
    (Down, triplet (right !! 8, right !! 5, right !! 2) (6, 7, 8) down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

rotateBackSideClockwise :: Cube -> Cube
rotateBackSideClockwise c =
  [ (Front, front),
    (Right, right),
    (Back, rotateSideClockwise back),
    (Left, left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides c

rotateBackSideCounterClockwise :: Cube -> Cube
rotateBackSideCounterClockwise c =
  [ (Front, front),
    (Right, right),
    (Back, rotateSideCounterClockwise back),
    (Left, left),
    (Up, up),
    (Down, down)
  ]
  where
    (front, left, back, right, up, down) = getSides c
