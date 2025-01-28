module AppUtils
  ( readCube
  , formatCube
  , simplifyTaggedMoves
  , groupByPhase
  , printPhaseMoves
  ) where

import Types
import Utils (getSide)  -- getSide do formatCube
import Prelude hiding (Left, Right)

--------------------------------------------------------------------------------
-- Wczytywanie kostki
--------------------------------------------------------------------------------

readCube :: [String] -> Cube -> Cube
readCube [] cube = cube
readCube (line : xs) cube =
  let ws         = words line
      sideString = head ws
      colorsStr  = tail ws
      face       = (stringToSide sideString, map stringToColor colorsStr)
  in readCube xs (face : cube)

stringToSide :: String -> Side
stringToSide "Down"  = Down
stringToSide "Up"    = Up
stringToSide "Left"  = Left
stringToSide "Right" = Right
stringToSide "Front" = Front
stringToSide "Back"  = Back
stringToSide _       = Up

stringToColor :: String -> Color
stringToColor "Blue"   = Blue
stringToColor "Red"    = Red
stringToColor "Yellow" = Yellow
stringToColor "White"  = White
stringToColor "Orange" = Orange
stringToColor "Green"  = Green
stringToColor _        = White

--------------------------------------------------------------------------------
-- Formatowanie kostki w stylu wejściowym
--------------------------------------------------------------------------------

-- | Zamienia 'Cube' na listę linijek w stylu "Side Color1 Color2 ... Color9".
formatCube :: Cube -> [String]
formatCube c =
  map formatSide [Front, Right, Back, Left, Up, Down]
  where
    formatSide side =
      let colors = getSide side c
       in unwords (show side : map show colors)

--------------------------------------------------------------------------------
-- Upraszczanie ruchów 
--------------------------------------------------------------------------------

-- | Zwraca odwrotny ruch do danego: U -> U', U' -> U, ...
inverseMove :: Move -> Move
inverseMove U  = U'
inverseMove U' = U
inverseMove D  = D'
inverseMove D' = D
inverseMove L  = L'
inverseMove L' = L
inverseMove R  = R'
inverseMove R' = R
inverseMove F  = F'
inverseMove F' = F
inverseMove B  = B'
inverseMove B' = B

-- | Upraszczanie:
--     - pary odwrotnych ruchów (R, R' → usuń)
--     - potrójne te same ruchy (R, R, R → R')
--     - poczwórne te same ruchy (R, R, R, R → usuń)
simplifyTaggedMoves :: [(String, Move)] -> [(String, Move)]
simplifyTaggedMoves moves =
  -- na koniec odwracamy, bo stos budujemy "od lewej"
  reverse (foldl step [] moves)
  where
    -- „step” odkłada nowy ruch (cur) na stos (acc),
    -- a następnie próbuje go maksymalnie uprościć (reduceTop).
    step :: [(String, Move)] -> (String, Move) -> [(String, Move)]
    step acc cur = reduceTop (cur : acc)

    reduceTop :: [(String, Move)] -> [(String, Move)]
    reduceTop stack =
      case stack of
        --------------------------------------------------
        -- 1) Para odwrotnych ruchów obok siebie => usuń
        (x1@(phase1,mv1) : x2@(phase2,mv2) : xs)
          | mv1 == inverseMove mv2
            -> reduceTop xs  -- usuwamy oba z wierzchołka

        --------------------------------------------------
        -- 2) Trzy takie same ruchy z rzędu => zamień na 1 ruch odwrotny
        (x1@(ph1,mv1) : x2@(ph2,mv2) : x3@(ph3,mv3) : xs)
          | mv1 == mv2 && mv2 == mv3
            -> -- np. R, R, R => R'
               -- Dla nazwy fazy możesz wybrać np. ph3 (ostatni) lub ph1 (pierwszy)
               reduceTop ((ph3, inverseMove mv1) : xs)

        --------------------------------------------------
        -- 3) Cztery takie same ruchy => usuwamy je całkowicie
        (x1@(ph1,mv1) : x2@(ph2,mv2) : x3@(ph3,mv3) : x4@(ph4,mv4) : xs)
          | mv1 == mv2 && mv2 == mv3 && mv3 == mv4
            -> -- np. R, R, R, R => brak ruchu
               reduceTop xs

        --------------------------------------------------
        _ -> stack


--------------------------------------------------------------------------------
-- Grupowanie ruchów według faz
--------------------------------------------------------------------------------

groupByPhase :: [(String, Move)] -> [(String, [Move])]
groupByPhase =
  foldr insertPh [] 
  where
    insertPh (ph, mv) [] = [(ph, [mv])]
    insertPh (ph, mv) ((curPh, mvs):rest)
      | ph == curPh = (curPh, mv:mvs) : rest
      | otherwise   = (ph, [mv]) : (curPh, mvs) : rest

--------------------------------------------------------------------------------
-- Drukowanie faz
--------------------------------------------------------------------------------

printPhaseMoves :: (String, [Move]) -> IO ()
printPhaseMoves (phaseName, moves) = do
  putStr (phaseName ++ ": [")
  putStr (unwords (map show moves))
  putStrLn "]"

