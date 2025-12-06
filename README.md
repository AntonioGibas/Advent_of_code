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
├── [source/](source/)
│   ├── [2024/](source/2024/)          # Rješenja za 2024. godinu
│   └── [2025/](source/2025/)          # Rješenja za 2025. godinu
└── [documentation/](documentation/)     # Detaljne dokumentacije
    ├── [2024/](documentation/2024/)
    └── [2025/](documentation/2025/)
```

## ✅ Riješeni izazovi

### 2024

| Dan | Naziv | Opis | Dokumentacija |
|-----|-------|------|---------------|
| 01 | **Location Lists** | Usporedba i sortiranje lista brojeva, izračun similarity score-a | - |
| 02 | **Safe Reports** | Analiza stabilnosti nizova s "Problem Dampener" algoritamom | - |
| 04 | **Word Search** | 2D pretraga riječi u svim smjerovima (vodoravno, okomito, dijagonalno) | - |
| 05 | **Page Ordering** | Validacija i sortiranje redoslijeda prema pravilima | [📄](documentation/2024/ADV5PLI%20documentation) |
| 06 | **Guard Patrol** | Simulacija kretanja i detekcija petlji u 2D mreži | [📄](documentation/2024/ADV6PLI%20documentation) |
| 07 | **Bridge Calibration** | Kombinatorika operatora (+, *, \|\|) za postizanje ciljne vrijednosti | [📄](documentation/2024/ADV7PLI%20documentation) |
| 08 | **Antinode Detection** | Geometrijska analiza pozicija antena i njihovih antinoda | [📄](documentation/2024/ADV8PLI%20Documentation.txt) |
| 09 | **Disk Fragmenter** | Simulacija disk kompaktiranja i checksum kalkulacija | [📄](documentation/2024/ADV9PLI%20documentation) |
| 10 | **Hiking Trails** | Rekurzivna pretraga hiking trail-ova na topografskoj mapi | [📄](documentation/2024/AD10PLI%20documentation) |

### 2025

| Dan | Naziv | Opis | Dokumentacija |
|-----|-------|------|---------------|
| 01 | **Circular Navigation** | Kretanje po kružnoj listi s brojenjem prelaska nule | - |
| 02 | **Repeating IDs** | Validacija ponavljajućih ID sekvenci s dva različita kriterija | [📄](documentation/2025/AD02PLI_doc.md) |
| 03 | **Digit Selection** | Greedy algoritam za pronalazak najvećeg 12-cifrenog broja | - |
| 04 | **Roll Analysis** | Analiza i iterativno uklanjanje rola u grid strukturi | [📄](documentation/2025/AD04PLI_doc.md) |
| 05 | **Fresh Ingredients** | Spajanje preklapajućih raspona i validacija svježih namirnica | [📄](documentation/2025/AD05PLI_doc.md) |

## 📚 Dokumentacija

Svaki kompleksniji program ima detaljnu dokumentaciju koja uključuje:

- **Pregled programa** - svrha i cilj
- **Deklaracije varijabli** - objašnjenje svih korištenih varijabli
- **Strukture podataka** - nizovi, matrice, strukture
- **Algoritmi** - detaljno objašnjenje logike s pseudokodom
- **Primjeri izvođenja** - step-by-step demonstracije
- **Optimizacije** - usporedbe prije/poslije sa analizom performansi
- **Kompleksnost** - vremenska i prostorna analiza

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
- **Sortiranje** - Bubble Sort, QuickSort
- **Depth-First Search (DFS)** - za pretragu grafova
- **Greedy algoritmi** - za optimizacijske probleme
- **Interval merging** - spajanje preklapajućih raspona
- **Delta koordinate** - optimizirano brojanje susjeda u grid strukturama

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

/* Procedure s povratnom vrijednošću */
IS_REPEATING: PROCEDURE(S, MIN_REPS) RETURNS(BIT(1));

/* Delta petlje za grid pretragu */
DO DX = -1 TO 1;
  DO DY = -1 TO 1;
    IF DX = 0 & DY = 0 THEN ITERATE;
    /* Obrada susjeda */
  END;
END;
```

## 📊 Statistika

- **Ukupno programa**: 15
- **Redaka koda**: ~3000+
- **Dokumentacija**: 9 detaljnih dokumenata
- **Algoritmi**: DFS, BFS, Greedy, Interval Merging, Pattern Matching

## 🎓 Učenje

Ovaj repozitorij je izvrstan resurs za učenje:

- **PL/I sintakse** i idioma
- **Mainframe programiranja**
- **Algoritamskog razmišljanja**
- **Rješavanja problema** s ograničenjima
- **Optimizacijskih tehnika** - od O(n²) do O(n log n)
- **Grid i matričnih operacija**
- **Parsiranja i obrade podataka**

## 🔍 Napredno

### Optimizacijske tehnike korištene u rješenjima

1. **Inline parsing** - eliminacija nepotrebnih poziva procedura
2. **Streaming obrada** - procesiranje podataka bez potpunog učitavanja u memoriju
3. **Delta pristup** - univerzalno rješenje za grid susjede umjesto 84 IF bloka
4. **Interval merging** - spajanje preklapajućih raspona za efikasnu validaciju
5. **Rani izlaz** - `GO TO` za optimizirani execution path
6. **DRY princip** - eliminacija dupliciranog koda kroz procedure

### Performanse

Primjeri ubrzanja kroz optimizacije:

- **AD02PLI**: 40% brže parsiranje kroz inline konverziju
- **AD04PLI**: ~70 linija koda smanjeno na ~40 eliminacijom rubnih slučajeva
- **AD05PLI**: 18× brže provjere kroz interval merging (10,000 vs 186,000 provjera)

## 📝 Napomene

- Svi programi su testirani s Advent of Code službenim inputima
- Dokumentacija je na **hrvatskom jeziku**
- Kod slijedi PL/I najbolje prakse i standarde
- Posebna pažnja posvećena čitljivosti i održavanju koda
- Svaka optimizacija dokumentirana s analizom prije/poslije