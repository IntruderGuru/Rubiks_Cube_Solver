module Types where

import Prelude hiding (Left, Right)

-- | Typ reprezentujący dostępne kolory ścianek kostki Rubika.
data Color
  = White
  | Yellow
  | Orange
  | Green
  | Red
  | Blue
  deriving (Eq, Show)

-- | Typ reprezentujący poszczególne ścianki kostki Rubika.
data Side
  = Front
  | Up
  | Left
  | Right
  | Back
  | Down
  deriving (Eq, Show)

-- | Krotka, w której:
--   * 'Side' opisuje konkretną ściankę kostki
--   * '[Color]' to lista kolorów poszczególnych pól na tej ściance
type Face = (Side, [Color])

-- | Lista wszystkich ścianek; reprezentacja całej kostki Rubika.
type Cube = [Face]

-- | Typ wyliczeniowy dla możliwych ruchów kostką Rubika.
--   Apostrof (') oznacza obrót w przeciwną stronę niż standardowy (np. odwrotnie do wskazówek zegara).
data Move
  = -- | Obrót górnej ścianki zgodnie z ruchem wskazówek zegara
    U
  | -- | Obrót górnej ścianki przeciwnie do ruchu wskazówek zegara
    U'
  | -- | Obrót dolnej ścianki zgodnie z ruchem wskazówek zegara
    D
  | -- | Obrót dolnej ścianki przeciwnie do ruchu wskazówek zegara
    D'
  | -- | Obrót lewej ścianki zgodnie z ruchem wskazówek zegara
    L
  | -- | Obrót lewej ścianki przeciwnie do ruchu wskazówek zegara
    L'
  | -- | Obrót prawej ścianki zgodnie z ruchem wskazówek zegara
    R
  | -- | Obrót prawej ścianki przeciwnie do ruchu wskazówek zegara
    R'
  | -- | Obrót przedniej ścianki zgodnie z ruchem wskazówek zegara
    F
  | -- | Obrót przedniej ścianki przeciwnie do ruchu wskazówek zegara
    F'
  | -- | Obrót tylnej ścianki zgodnie z ruchem wskazówek zegara
    B
  | -- | Obrót tylnej ścianki przeciwnie do ruchu wskazówek zegara
    B'
  deriving (Eq, Show)

type CubeWithMoves = (Cube, [Move])