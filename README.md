# Advent of Code - PL/I Solutions

Repozitorij s rješenjima za **Advent of Code** izazove, implementiranim u **PL/I** programskom jeziku.

## 📋 Sadržaj

- [O projektu](#o-projektu)
- [Struktura repozitorija](#struktura-repozitorija)
- [Riješeni izazovi](#riješeni-izazovi)
- [Dokumentacija](#dokumentacija)
- [Pokretanje programa](#pokretanje-programa)
- [Tehnički detalji](#tehnički-detalji)

## 🎯 O projektu

Ovaj repozitorij sadrži moja rješenja za Advent of Code izazove, napisana u PL/I-u - programskom jeziku koji se tradicionalno koristi u mainframe okruženjima. Svako rješenje je potpuno dokumentirano s detaljnim objašnjenjima algoritama i logike.

## 📁 Struktura repozitorija

```
.
├── source/
│   ├── 2024/          # Rješenja za 2024. godinu
│   │   ├── AD01PLI.pli
│   │   ├── AD02PLI.pli
│   │   ├── AD04PLI.pli
│   │   ├── AD05PLI.pli
│   │   ├── AD06PLI.pli
│   │   ├── AD07PLI.pli
│   │   ├── AD08PLI.pli
│   │   ├── AD09PLI.pli
│   │   └── AD10PLI.pli
│   └── 2025/          # Rješenja za 2025. godinu
│       ├── AD01PLI.pli
│       └── AD03PLI.pli
└── documentation/     # Detaljne dokumentacije
    ├── AD10PLI documentation
    ├── ADV5PLI documentation
    ├── ADV6PLI documentation
    ├── ADV7PLI documentation
    ├── ADV8PLI documentation
    └── ADV9PLI documentation
```

## ✅ Riješeni izazovi

### 2024

| Dan | Naziv | Opis | Dokumentacija |
|-----|-------|------|---------------|
| 01 | **Location Lists** | Usporedba i sortiranje lista brojeva, izračun similarity score-a | - |
| 02 | **Safe Reports** | Analiza stabilnosti nizova s "Problem Dampener" algoritamom | - |
| 04 | **Word Search** | 2D pretraga riječi u svim smjerovima (vodoravno, okomito, dijagonalno) | - |
| 05 | **Page Ordering** | Validacija i sortiranje redoslijeda prema pravilima | [📄](documentation/ADV5PLI%20documentation) |
| 06 | **Guard Patrol** | Simulacija kretanja i detekcija petlji u 2D mreži | [📄](documentation/ADV6PLI%20documentation) |
| 07 | **Bridge Calibration** | Kombinatorika operatora (+, *, \|\|) za postizanje ciljne vrijednosti | [📄](documentation/ADV7PLI%20documentation) |
| 08 | **Antinode Detection** | Geometrijska analiza pozicija antena i njihovih antinoda | [📄](documentation/ADV8PLI%20documentation) |
| 09 | **Disk Fragmenter** | Simulacija disk kompaktiranja i checksum kalkulacija | [📄](documentation/ADV9PLI%20documentation) |
| 10 | **Hiking Trails** | Rekurzivna pretraga hiking trail-ova na topografskoj mapi | [📄](documentation/AD10PLI%20documentation) |

### 2025

| Dan | Naziv | Opis |
|-----|-------|------|
| 01 | **Circular Navigation** | Kretanje po kružnoj listi s brojenjem prelaska nule |
| 03 | **Digit Selection** | Greedy algoritam za pronalazak najvećeg 12-cifrenog broja |

## 📚 Dokumentacija

Svaki kompleksniji program ima detaljnu dokumentaciju koja uključuje:

- **Pregled programa** - svrha i cilj
- **Deklaracije varijabli** - objašnjenje svih korištenih varijabli
- **Strukture podataka** - nizovi, matrice, strukture
- **Algoritmi** - detaljno objašnjenje logike
- **PL/I specifičnosti** - posebnosti jezika
- **Primjeri izvođenja** - step-by-step primjeri

## 🚀 Pokretanje programa

### Preduvjeti

- PL/I kompajler (npr. IBM Enterprise PL/I)
- Mainframe okruženje ili PL/I emulator

### Kompajliranje

```bash
# Primjer kompajliranja (ovisi o okruženju)
pli -o AD01PLI source/2024/AD01PLI.pli
```

### Izvršavanje

```bash
# Izvršavanje programa
./AD01PLI < input.txt
```

## 🔧 Tehnički detalji

### Korištene tehnike

- **Rekurzivne procedure** - za pretragu putanja i stabala
- **Bitovne operacije** - za kombinatoriku i optimizaciju memorije
- **2D/3D nizovi** - za reprezentaciju mreža i mapa
- **String manipulacija** - parsing i transformacije
- **Sortiranje** - QuickSort, Bubble Sort
- **Depth-First Search (DFS)** - za pretragu grafova
- **Greedy algoritmi** - za optimizacijske probleme

### PL/I specifičnosti

```pli
/* Deklaracija datoteka */
DCL INFL FILE RECORD INPUT;

/* Strukture */
DCL 1 FOUND_NINES(500),
    2 X FIXED BINARY(31),
    2 Y FIXED BINARY(31);

/* Rekurzivne procedure */
FIND_PATH: PROCEDURE(MAP, X, Y, Z) RECURSIVE;

/* Bitovne varijable */
DCL VISITED(130,130) BIT(1) INIT((130*130)'0'b);
```

## 📊 Statistika

- **Ukupno programa**: 11
- **Redaka koda**: ~2000+
- **Dokumentacija**: 6 detaljnih dokumenata
- **Algoritmi**: DFS, BFS, Greedy, Dynamic Programming

## 🎓 Učenje

Ovaj repozitorij je izvrstan resurs za učenje:

- **PL/I sintakse** i idioma
- **Mainframe programiranja**
- **Algoritamskog razmišljanja**
- **Rješavanja problema** s ograničenjima

## 📝 Napomene

- Svi programi su testirani s Advent of Code službenim inputima
- Dokumentacija je na **hrvatskom jeziku**
- Kod slijedi PL/I najbolje prakse i standarde
- Posebna pažnja posvećena čitljivosti i održavanju koda