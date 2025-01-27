module RubikRotations
  ( makeMove
  , makeMoves
  , addMoveToLog
  ) where

import Prelude
import RubikTypes

{-
  W tym module definiujemy, jak wygląda transformacja kostki
  przy wykonaniu konkretnego ruchu MU, MU', MD, MD' itd.

  Każda z funkcji 'applyU', 'applyU'' itp.:
    - zmienia zawartość ścianki górnej (FUp) zgodnie z obrotem 90° w lewo/prawo,
    - przenosi kolory między front, right, back, left w "górnym wierszu".

  Podobnie D, L, R, F, B – tyle że w innych miejscach.

  Na końcu 'makeMove' jest dispatch: sprawdza, jaki ruch wykonać, i stosuje daną funkcję.
-}

makeMove :: MoveX -> CubeX -> CubeX
makeMove mv cube =
  case mv of
    MU  -> applyU cube
    MU' -> applyU' cube
    MD  -> applyD cube
    MD' -> applyD' cube
    ML  -> applyL cube
    ML' -> applyL' cube
    MR  -> applyR cube
    MR' -> applyR' cube
    MF  -> applyF cube
    MF' -> applyF' cube
    MB  -> applyB cube
    MB' -> applyB' cube

-- | Wykonuje całą listę ruchów na kostce, sekwencyjnie.
makeMoves :: [MoveX] -> CubeX -> CubeX
makeMoves moves cube = foldl (\acc mv -> makeMove mv acc) cube moves

-- | Dopisuje ruch do historii i aktualizuje kostkę.
addMoveToLog :: MoveX -> CubeWithLog2 -> CubeWithLog2
addMoveToLog mv (cb, past) = (makeMove mv cb, past ++ [mv])

--------------------------------------
-- Szkielet do wypełnienia:
--------------------------------------
applyU :: CubeX -> CubeX
applyU c = c  -- TODO: implementacja

applyU' :: CubeX -> CubeX
applyU' c = c -- TODO

applyD :: CubeX -> CubeX
applyD c = c  -- TODO

applyD' :: CubeX -> CubeX
applyD' c = c -- TODO

applyL :: CubeX -> CubeX
applyL c = c -- TODO

applyL' :: CubeX -> CubeX
applyL' c = c -- TODO

applyR :: CubeX -> CubeX
applyR c = c -- TODO

applyR' :: CubeX -> CubeX
applyR' c = c -- TODO

applyF :: CubeX -> CubeX
applyF c = c -- TODO

applyF' :: CubeX -> CubeX
applyF' c = c -- TODO

applyB :: CubeX -> CubeX
applyB c = c -- TODO

applyB' :: CubeX -> CubeX
applyB' c = c -- TODO
