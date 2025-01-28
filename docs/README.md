# RubikSolver - Solver Kostki Rubika

## 📌 Opis

Program wczytuje opis kostki Rubika z pliku tekstowego (6 wierszy, po 9 kolorów każdy) i rozwiązuje ją w kilku fazach zgodnie z metodą **CFOP**:

1. **White Cross** - biała krzyżówka
2. **F2L** (First Two Layers) - pierwsze dwie warstwy
3. **OLL** (Orientation of the Last Layer) - orientacja ostatniej warstwy
4. **PLL** (Permutation of the Last Layer) - permutacja ostatniej warstwy

Sekwencje ruchów są następnie wyświetlane na ekran. Opcjonalnie w trybie szczegółowym (`--detailed`) można zobaczyć **kolejne stany kostki** po każdym ruchu.

- **⚠️ Uwaga:** Warto wygenerować plik, a następnie użyć go do testów z opcją `--detailed`, gdyż układanie kostki fizycznej może być żmudne i podatne na pomyłki. **Patrz: Generowanie scramble**

---

## 🔧 Kompilacja

Aby skompilować program, będąc w katalogu src użyj komendy:
```bash
ghc Main.hs -o rubikSolver
```
W efekcie powstanie plik wykonywalny `rubikSolver`.


### 📁 Struktura kodu

- **Main.hs** – punkt wejścia programu, parsowanie argumentów, uruchamianie rozwiązywania.
- **AppUtils.hs** – funkcje pomocnicze, wczytywanie/zapisywanie/formatowanie kostki.
- **Rotations.hs** – implementacje poszczególnych ruchów (U, R, F, itd.).
- **Types.hs** – definicje typów (`Color`, `Side`, `Move`, itp.).
- **Utils.hs** – funkcje pomocnicze (pobieranie ścian, `getSide`, `replace`, itp.).
- **Scramble.hs** – generowanie scramble (losowych sekwencji ruchów).
- **SolveWhiteCross.hs, SolveF2L.hs, SolveOLL.hs, SolvePLL.hs** – poszczególne etapy metody CFOP.
- **Solving.hs** – łączy poszczególne fazy w jedną funkcję `solveCubePhases`.

---

## 📄 Format pliku wejściowego

Plik tekstowy powinien zawierać **6 wierszy**, z których każdy opisuje jedną ściankę kostki w formacie:

```
Side Kolor1 Kolor2 Kolor3 Kolor4 Kolor5 Kolor6 Kolor7 Kolor8 Kolor9
```
Gdzie:
- **Side** to jedna z wartości: `Front`, `Right`, `Back`, `Left`, `Up`, `Down`.
- **ColorX** to jeden z kolorów:
  - ⚪ **White**
  - 🟡 **Yellow**
  - 🔴 **Red**
  - 🟠 **Orange**
  - 🔵 **Blue**
  - 🟢 **Green**


Przykładowy plik wejściowy:
```
Front  White White White  White White White  White White White
Right  Red Red Red  Red Red Red  Red Red Red
Back   Yellow Yellow Yellow  Yellow Yellow Yellow  Yellow Yellow Yellow
Left   Orange Orange Orange  Orange Orange Orange  Orange Orange Orange
Up     Blue Blue Blue  Blue Blue Blue  Blue Blue Blue
Down   Green Green Green  Green Green Green  Green Green Green
```

---

## 🛠 Sposób użycia

### ✅ Standardowe rozwiązanie kostki będąc w katalogu src

```bash
.\rubikSolver <plik_z_kostka.txt>
```
Wczyta kostkę i wypisze sekwencję ruchów w 4 fazach CFOP.

### 🔍 Tryb szczegółowy (`--detailed`)

Aby wyświetlić stan kostki po każdym ruchu:

```bash
.\rubikSolver <plik_z_kostka.txt> --detailed
```

Program wypisze standardową sekwencję ruchów, a następnie szczegółową listę zawierającą:
1. Numer ruchu
2. Nazwę ruchu
3. Stan kostki (6 wierszy x 9 kolorów) po każdym ruchu

🔹 **Uwaga:** Najpierw podaj nazwę pliku, potem flagę `--detailed`!

### 🎲 Generowanie scramble (losowego ułożenia)

```bash
.\rubikSolver --scramble <liczba_ruchów> <nazwa_pliku_wyjściowego>
```

- Jeśli podasz nazwę pliku, wylosowana kostka zostanie zapisana do tego pliku.
- Jeśli nie podasz nazwy pliku, program tylko wyświetli kostkę i ruchy na ekranie.

### ℹ️ Pomoc

```bash
rubikSolver --help
```

Wyświetla krótką instrukcję obsługi programu.

---
