## Opis

Program wczytuje opis kostki Rubika z pliku tekstowego (6 wierszy, po 9 kolorów każdy) i rozwiązuje ją w kilku fazach:
1. White Cross
2. F2L (First Two Layers)
3. OLL (Orientation of the Last Layer)
4. PLL (Permutation of the Last Layer)

Sekwencje ruchów są następnie wyświetlane na ekran, opcjonalnie w trybie szczegółowym (`--detailed`) również **z kolejnymi stanami kostki** po każdym ruchu.

---

## Kompilacja
ghc Main.hs -o rubikSolver

W efekcie powstaje plik wykonywalny rubikSolver.



Main.hs – punkt wejścia, parsowanie argumentów, główne uruchamianie rozwiązywania.

AppUtils.hs – funkcje pomocnicze, wczytywanie/zapisywanie/formatowanie kostki.

Rotations.hs – implementacje poszczególnych ruchów (U, R, F, itd.).

Types.hs – definicje typów (Color, Side, Move, itp.).

Utils.hs – funkcje pomocnicze (pobieranie ścian, getSide, replace itp.).

Scramble.hs – generowanie scramble (losowych sekwencji ruchów).

SolveWhiteCross.hs, SolveF2L.hs, SolveOLL.hs, SolvePLL.hs – etapy metody CFOP.

Solving.hs – łączy poszczególne fazy w jedną funkcję solveCubePhases.


## Format pliku wejściowego
Plik tekstowy powinien zawierać 6 wierszy, z czego każdy opisuje jedną ściankę kostki.

Format wiersza:

Side Kolor1 Kolor2 Kolor3 Kolor4 Kolor5 Kolor6 Kolor7 Kolor8 Kolor9
Gdzie:

Side to jeden z: Front, Right, Back, Left, Up, Down.
KolorX to jeden z: White, Yellow, Red, Orange, Blue, Green.

## Sposób użycia

Standardowe rozwiązanie kostki:

rubikSolver <plik_z_kostka.txt>
Wczyta kostkę i wypisze sekwencję ruchów (w 4 fazach CFOP).

Wyświetlenie stanu kostki po każdym ruchu (--detailed):

Uwaga: najpierw podaj nazwę pliku, potem flagę --detailed!

rubikSolver <plik_z_kostka.txt> --detailed

Wypisze standardową sekwencję ruchów, a następnie szczegółową listę:
Numer ruchu,
Nazwę ruchu,
Stan kostki (6 wierszy x 9 kolorów) po każdym ruchu.


Generowanie scramble (losowe ułożenie):

rubikSolver --scramble <liczba_ruchów> [nazwa_pliku_wyjściowego]

Jeśli podasz nazwę pliku, wylosowana kostka zostanie zapisana do tego pliku.
Jeśli nie podasz, program tylko wyświetli kostkę i ruchy na ekranie.

Pomoc:

rubikSolver --help

Wyświetla krótką instrukcję.
