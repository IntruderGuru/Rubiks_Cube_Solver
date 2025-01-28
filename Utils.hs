module Utils where

import Types
import Prelude hiding (Left, Right)

-- | Zastępuje w liście do trzech elementów, które znajdują się
--   pod wskazanymi indeksami. Krotka '(v1, v2, v3)' to nowe wartości,
--   natomiast '(t1, t2, t3)' to indeksy, które mają zostać zastąpione.
replace3 :: (a, a, a) -> (Int, Int, Int) -> [a] -> [a]
replace3 (v1, v2, v3) (t1, t2, t3) target =
  replace v3 t3 (replace v2 t2 (replace v1 t1 target))

-- | Zastępuje w liście element o indeksie 'n' nową wartością 'value'.
--   Gdy lista jest pusta lub 'n' jest poza jej zakresem, zwraca listę bez zmian.
replace :: a -> Int -> [a] -> [a]
replace _ _ [] = []
replace value n (x : xs) =
  if n == 0
    then value : xs
    else x : replace value (n - 1) xs

{- Gettery i funkcje pomocnicze -}

-- | Zwraca kostkę Rubika w stanie "ułożonym" w standardowej konfiguracji kolorów.
getSolvedCube :: Cube
getSolvedCube =
  [ (Front, [Red, Red, Red, Red, Red, Red, Red, Red, Red]),
    (Right, [Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue]),
    (Back, [Orange, Orange, Orange, Orange, Orange, Orange, Orange, Orange, Orange]),
    (Left, [Green, Green, Green, Green, Green, Green, Green, Green, Green]),
    (Up, [White, White, White, White, White, White, White, White, White]),
    (Down, [Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow])
  ]

-- | Dla podanej ścianki 'Side' zwraca kolor, jaki standardowo powinien się tam znajdować.
getTargetSideColor :: Side -> Color
getTargetSideColor Up    = White
getTargetSideColor Down  = Yellow
getTargetSideColor Left  = Green
getTargetSideColor Right = Blue
getTargetSideColor Front = Red
getTargetSideColor Back  = Orange

-- | Zwraca listę kolorów znajdujących się na wskazanej ściance kostki.
getSide :: Side -> Cube -> [Color]
getSide side cube =
  concat [snd face | face <- cube, side == fst face]

-- | Zwraca krotkę zawierającą listy kolorów wszystkich sześciu ścianek kostki
--   w ustalonej kolejności: Front, Left, Back, Right, Up, Down.
getSides :: Cube -> ([Color], [Color], [Color], [Color], [Color], [Color])
getSides cube =
  ( getSide Front cube,
    getSide Left  cube,
    getSide Back  cube,
    getSide Right cube,
    getSide Up    cube,
    getSide Down  cube
  )

{- Translacja ruchów w zależności od położenia białej ścianki -}

-- | Tłumaczy ruchy kostki tak, aby ścianka z kolorem białym
--   (aktualnie na podanej 'Side') została obrócona do góry.
translateMoveWhiteUp :: Side -> Move -> Move
translateMoveWhiteUp Front move = move
translateMoveWhiteUp Up    move = move
translateMoveWhiteUp Down  move = move
translateMoveWhiteUp Right move
  | move == L   = F
  | move == L'  = F'
  | move == R   = B
  | move == R'  = B'
  | move == F   = R
  | move == F'  = R'
  | move == B   = L
  | move == B'  = L'
  | otherwise   = move
translateMoveWhiteUp Left move
  | move == L   = B
  | move == L'  = B'
  | move == R   = F
  | move == R'  = F'
  | move == F   = L
  | move == F'  = L'
  | move == B   = R
  | move == B'  = R'
  | otherwise   = move
translateMoveWhiteUp Back move
  | move == L   = R
  | move == L'  = R'
  | move == R   = L
  | move == R'  = L'
  | move == F   = B
  | move == F'  = B'
  | move == B   = F
  | move == B'  = F'
  | otherwise   = move

-- | Tłumaczy ruchy kostki tak, aby ścianka z kolorem białym
--   (aktualnie na podanej 'Side') została obrócona na dół.
translateMoveWhiteDown :: Side -> Move -> Move
translateMoveWhiteDown Up   move = move
translateMoveWhiteDown Down move = move
translateMoveWhiteDown Front move
  | move == U   = D
  | move == U'  = D'
  | move == D   = U
  | move == D'  = U'
  | move == L   = R
  | move == L'  = R'
  | move == R   = L
  | move == R'  = L'
  | otherwise   = move
translateMoveWhiteDown Right move
  | move == U   = D
  | move == U'  = D'
  | move == D   = U
  | move == D'  = U'
  | move == L   = B
  | move == L'  = B'
  | move == R   = F
  | move == R'  = F'
  | move == F   = R
  | move == F'  = R'
  | move == B   = L
  | move == B'  = L'
  | otherwise   = move
translateMoveWhiteDown Left move
  | move == U   = D
  | move == U'  = D'
  | move == D   = U
  | move == D'  = U'
  | move == L   = F
  | move == L'  = F'
  | move == R   = B
  | move == R'  = B'
  | move == F   = L
  | move == F'  = L'
  | move == B   = R
  | move == B'  = R'
  | otherwise   = move
translateMoveWhiteDown Back move
  | move == U   = D
  | move == U'  = D'
  | move == D   = U
  | move == D'  = U'
  | move == F   = B
  | move == F'  = B'
  | move == B   = F
  | move == B'  = F'
  | otherwise   = move

-- | Mapuje listę ruchów na nową listę, stosując 'translateMoveWhiteUp'.
translateMovesWhiteUp :: Side -> [Move] -> [Move]
translateMovesWhiteUp side = map (translateMoveWhiteUp side)

-- | Mapuje listę ruchów na nową listę, stosując 'translateMoveWhiteDown'.
translateMovesWhiteDown :: Side -> [Move] -> [Move]
translateMovesWhiteDown side = map (translateMoveWhiteDown side)
