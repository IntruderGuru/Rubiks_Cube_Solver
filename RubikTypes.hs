module RubikTypes where

import Prelude

-- | Dostępne kolory
data ColorX
  = CWhite
  | CYellow
  | CRed
  | COrange
  | CGreen
  | CBlue
  deriving (Eq, Show)

-- | Nazwa ścianki
data FaceSide
  = FFront
  | FRight
  | FBack
  | FLeft
  | FUp
  | FDown
  deriving (Eq, Show)

-- | Pojedyncza ścianka: (nazwa, 9 kolorów)
type FaceX = (FaceSide, [ColorX])

-- | Cała kostka: 6 ścian
type CubeX = [FaceX]

-- | Ruch w notacji
data MoveX
  = MU | MU'
  | MD | MD'
  | ML | ML'
  | MR | MR'
  | MF | MF'
  | MB | MB'
  deriving (Eq, Show)

-- | Kostka + log ruchów
type CubeWithLog2 = (CubeX, [MoveX])
