# AD05PLI - Validacija Svježih Namirnica

## Sadržaj

- [Pregled](#pregled)
- [Definicija Problema](#definicija-problema)
- [Deklaracije](#deklaracije)
- [Algoritam](#algoritam)
- [Spajanje Raspona](#spajanje-raspona)
- [Primjeri Izvođenja](#primjeri-izvođenja)
- [Optimizacije](#optimizacije)
- [Kompleksnost](#kompleksnost)

## Pregled

Program analizira raspone ID-eva svježih namirnica i provjerava koja skladišna ID-eva se smatraju svježima. ID je svjež ako se nalazi u **bilo kojem** rasponu.

**Ulazi**: 
- `RANGES`: 186 raspona svježih ID-eva (format: `START-END`)
- `INGID`: 1000 skladišnih ID-eva namirnica

**Izlazi**: 
- **SVJEZIH**: Broj skladišnih ID-eva koji su svježi
- **UKUPNO**: Ukupan broj jedinstvenih svježih ID-eva u svim rasponima

## Definicija Problema

### Problem Preklapajućih Raspona

Rasponi mogu se **preklapati** ili **dodirivati**, što znači da isti ID može biti u više raspona:

```
Primjer:
3-5      ●●●
10-14        ●●●●●
12-18          ●●●●●●●
16-20              ●●●●●
```

**Zadatak**:
1. Spojiti sve preklapajuće raspone u jedinstvene intervale
2. Prebrojati sve jedinstvene ID-eve
3. Provjeriti koji skladišni ID-evi pripadaju tim intervalima

### Pravila Spajanja

Dva raspona se **spajaju** ako:
- Preklapaju se: `[10,14]` i `[12,18]` → `[10,18]`
- Dodiruju se: `[10,14]` i `[15,20]` → `[10,20]`
- **NE spajaju** ako imaju gap: `[10,14]` i `[16,20]` → ostaju odvojeni

## Deklaracije

### Datoteke

| Datoteka | Tip | Sadržaj |
|----------|-----|---------|
| `RANGES` | Record Input | 186 raspona (format `START-END`) |
| `INGID` | Record Input | 1000 skladišnih ID-eva |

### Varijable - Rasponi

| Varijabla | Tip | Dimenzija | Svrha |
|-----------|-----|-----------|-------|
| `RASPON_OD` | `FIXED BINARY(63)` | 186 | START vrijednosti originalnih raspona |
| `RASPON_DO` | `FIXED BINARY(63)` | 186 | END vrijednosti originalnih raspona |
| `MERGED_OD` | `FIXED BINARY(63)` | 186 | START vrijednosti spojenih raspona |
| `MERGED_DO` | `FIXED BINARY(63)` | 186 | END vrijednosti spojenih raspona |

### Varijable - Brojači

| Varijabla | Tip | Svrha |
|-----------|-----|-------|
| `FRESH_NUM` | `FIXED BINARY(15)` | Broj svježih skladišnih ID-eva |
| `BR_MERGED` | `FIXED BINARY(15)` | Broj spojenih raspona |
| `SUMA` | `FIXED BINARY(63)` | Ukupan broj jedinstvenih svježih ID-eva |

### Procedure

| Procedura | Parametri | Povratna Vrijednost | Svrha |
|-----------|-----------|---------------------|-------|
| `SORTIRAJ_RASPONE` | - | - | Sortira raspone po START vrijednosti |
| `SPOJI_RASPONE` | - | - | Spaja preklapajuće/dodirujuće raspone |
| `PROVJERI_ID` | `BIN_ID` | `BIT(1)` | Provjerava je li ID u spojenim rasponima |

## Algoritam

### 1. Učitavanje Raspona

```
DO I = 1 TO 186:
    Učitaj liniju (format: "START-END")
    Nađi poziciju '-'
    RASPON_OD(I) = dio prije '-'
    RASPON_DO(I) = dio nakon '-'
```

**Primjer parsiranja**:
```
Input: "3-5"
POS = 2 (pozicija '-')
RASPON_OD = BINARY("3") = 3
RASPON_DO = BINARY("5") = 5
```

### 2. Sortiranje Raspona

```
SORTIRAJ_RASPONE:
    DO I = 1 TO 185:
        DO J = I + 1 TO 186:
            AKO RASPON_OD(J) < RASPON_OD(I):
                Zamijeni RASPON_OD(I) ↔ RASPON_OD(J)
                Zamijeni RASPON_DO(I) ↔ RASPON_DO(J)
```

**Bubble sort** algoritam sortira po START vrijednosti uzlazno.

**Primjer**:
```
Prije:
[16-20]
[3-5]
[10-14]
[12-18]

Poslije:
[3-5]
[10-14]
[12-18]
[16-20]
```

### 3. Spajanje Raspona

```
SPOJI_RASPONE:
    
    // Inicijalizacija
    BR_MERGED = 1
    MERGED_OD(1) = RASPON_OD(1)
    MERGED_DO(1) = RASPON_DO(1)
    
    // Iteracija kroz sortirane raspone
    DO I = 2 TO 186:
        
        // Provjeri preklapanje ili dodir
        AKO RASPON_OD(I) <= MERGED_DO(BR_MERGED) + 1:
            
            // Proširi trenutni merged raspon
            AKO RASPON_DO(I) > MERGED_DO(BR_MERGED):
                MERGED_DO(BR_MERGED) = RASPON_DO(I)
        
        INAČE:
            // Novi neovisni raspon
            BR_MERGED += 1
            MERGED_OD(BR_MERGED) = RASPON_OD(I)
            MERGED_DO(BR_MERGED) = RASPON_DO(I)
```

**Ključni uvjet**: `RASPON_OD(I) <= MERGED_DO(BR_MERGED) + 1`
- `<=` znači preklapanje
- `+ 1` znači dodirivanje (npr. [10,14] i [15,20])

### 4. Validacija Skladišnih ID-eva

```
DO J = 1 TO 1000:
    Učitaj ID_LINE
    BIN_ID = pretvori u broj
    
    AKO PROVJERI_ID(BIN_ID):
        FRESH_NUM += 1
```

### 5. Procedura: PROVJERI_ID

```
PROVJERI_ID(BIN_ID):
    
    DO K = 1 TO BR_MERGED:
        AKO BIN_ID >= MERGED_OD(K) & BIN_ID <= MERGED_DO(K):
            VRATI '1'b  // ID je svjež
    
    VRATI '0'b  // ID nije svjež
```

**Optimizacija**: Provjerava samo **spojene** raspone, ne svih 186 originalnih!

### 6. Brojanje Ukupnih Svježih ID-eva

```
SUMA = 0
DO I = 1 TO BR_MERGED:
    SUMA += (MERGED_DO(I) - MERGED_OD(I) + 1)
```

Za svaki spojeni raspon: `END - START + 1`

## Spajanje Raspona

### Primjer 1: Potpuno Preklapanje

```
Input:
[10-14]
[12-18]

Sortiran:
[10-14]  ●●●●●
[12-18]    ●●●●●●●

Provjera: 12 <= 14 + 1 ✓ (12 <= 15)
Rezultat: [10-18]  ●●●●●●●●●
```

### Primjer 2: Dodirivanje

```
Input:
[10-14]
[15-20]

Sortiran:
[10-14]  ●●●●●
[15-20]       ●●●●●●

Provjera: 15 <= 14 + 1 ✓ (15 <= 15)
Rezultat: [10-20]  ●●●●●●●●●●●
```

### Primjer 3: Gap (Ne Spajaju Se)

```
Input:
[10-14]
[16-20]

Sortiran:
[10-14]  ●●●●●
[16-20]        ●●●●●

Provjera: 16 <= 14 + 1 ✗ (16 > 15)
Rezultat: 
[10-14]  ●●●●●
[16-20]        ●●●●●
```

### Primjer 4: Kompleksni Slučaj

```
Input rasponi:
3-5
10-14
16-20
12-18

Korak 1 - Sortiraj:
[3-5]    ●●●
[10-14]      ●●●●●
[12-18]        ●●●●●●●
[16-20]            ●●●●●

Korak 2 - Merge iteracija po iteracija:

Iter 1: MERGED[1] = [3-5]

Iter 2: [10-14]
  10 <= 5+1? 10 <= 6? ✗
  MERGED[2] = [10-14]

Iter 3: [12-18]
  12 <= 14+1? 12 <= 15? ✓
  Proširi: MERGED[2] = [10-18]

Iter 4: [16-20]
  16 <= 18+1? 16 <= 19? ✓
  Proširi: MERGED[2] = [10-20]

Finalni rezultat:
MERGED[1] = [3-5]    ●●●       = 3 ID-a
MERGED[2] = [10-20]     ●●●●●●●●●●● = 11 ID-a
                                -------
                                14 UKUPNO
```

## Primjeri Izvođenja

### Test Case 1

**Input RANGES**:
```
3-5
10-14
16-20
12-18
```

**Proces**:
1. Sortiraj: `[3-5], [10-14], [12-18], [16-20]`
2. Merge: `[3-5], [10-20]`
3. Ukupno: `(5-3+1) + (20-10+1) = 3 + 11 = 14`

**Input INGID** (primjeri):
```
4    → ✓ u [3-5]
7    → ✗
15   → ✓ u [10-20]
25   → ✗
```

**Output**:
```
SVJEZIH: 2
UKUPNO: 14
```

### Test Case 2: Svi Odvojeni Rasponi

**Input RANGES**:
```
1-3
10-12
20-22
```

**Merge**: Nema preklapanja
```
MERGED[1] = [1-3]
MERGED[2] = [10-12]
MERGED[3] = [20-22]
```

**Ukupno**: `3 + 3 + 3 = 9`

### Test Case 3: Jedan Veliki Raspon

**Input RANGES**:
```
1-10
5-15
12-20
18-25
```

**Merge**: Svi se spajaju
```
MERGED[1] = [1-25]
```

**Ukupno**: `25 - 1 + 1 = 25`

## Optimizacije

### 1. Inline Parsing

**Prije** (zasebna procedura):
```pl1
CALL UCITAJ_OD_DO;

UCITAJ_OD_DO:PROCEDURE;
  POS_CRT = INDEX(RASPON(I),'-',1);
  RASPON_OD(I) = BINARY(SUBSTR(...));
  ...
END;
```

**Poslije** (direktno):
```pl1
POS = INDEX(RASPON_LINE, '-');
RASPON_OD(I) = BINARY(SUBSTR(RASPON_LINE, 1, POS - 1));
RASPON_DO(I) = BINARY(SUBSTR(RASPON_LINE, POS + 1));
```

**Prednost**: Eliminira 186 poziva procedure

### 2. Streaming Validacija

**Prije** (čuva sve u memoriji):
```pl1
DCL NAM_ID(1000) CHAR(15);
DO J = 1 TO 1000;
  READ FILE(INGID) INTO(NAM_ID(J));
END;
DO J = 1 TO 1000;
  CALL PROVJERI_ID(NAM_ID(J));
END;
```

**Poslije** (obrađuje odmah):
```pl1
DO J = 1 TO 1000;
  READ FILE(INGID) INTO(ID_LINE);
  IF PROVJERI_ID(BINARY(ID_LINE)) THEN FRESH_NUM += 1;
END;
```

**Prednost**: Štedi 15KB memorije (1000 × 15 bytes)

### 3. Merge Prije Validacije

**Ključna optimizacija**: Spajanje raspona **prije** čitanja skladišnih ID-eva.

```
Bez merge (loše):
- 1000 ID-eva × 186 raspona = 186,000 provjera

Sa merge (dobro):
- 1000 ID-eva × ~10 merged raspona = ~10,000 provjera
```

**Poboljšanje**: ~18× brže provjere!

### 4. Boolean Return

**Prije**:
```pl1
PROVJERI_ID:PROCEDURE(ID);
  DCL FRESH FIXED BINARY(63) INIT(0);
  ...
  IF (...) THEN FRESH = BIN_ID;
  ...
  IF FRESH > 0 THEN FRESH_NUM += 1;
END;
```

**Poslije**:
```pl1
PROVJERI_ID:PROCEDURE(BIN_ID) RETURNS(BIT(1));
  ...
  IF (...) THEN RETURN('1'b);
  ...
  RETURN('0'b);
END;

IF PROVJERI_ID(BINARY(ID_LINE)) THEN FRESH_NUM += 1;
```

**Prednost**: Jasnije namjere, manje varijabli, rani return

## Kompleksnost

### Vremenska Kompleksnost

**Učitavanje raspona**: O(186)
- Parsiranje po liniji: O(k) gdje je k duljina stringa (~10-20 znakova)
- **Ukupno**: O(186k) ≈ O(1,860-3,720)

**Sortiranje**: O(186²) = O(34,596)
- Bubble sort
- Moguća optimizacija: Quick sort O(186 log 186) ≈ O(1,400)

**Spajanje**: O(186)
- Jedan prolazak kroz sortirani niz

**Validacija ID-eva**: O(1000 × m)
- m = broj merged raspona (prosječno 5-20)
- **Ukupno**: O(1000 × 10) ≈ O(10,000)

**Brojanje ukupnih**: O(m) gdje je m << 186

**Cjelokupan program**:
- Bez optimizacije: O(1000 × 186) = O(186,000)
- Sa optimizacijom: O(186² + 1000m) ≈ O(45,000)
- **Ubrzanje**: ~4× brže

### Prostorna Kompleksnost

**Originalni rasponi**: 186 × 2 × 8 bytes = 2,976 bytes  
**Merged rasponi**: 186 × 2 × 8 bytes = 2,976 bytes (worst case)  
**Privremene varijable**: ~100 bytes  
**Ukupno**: ~6KB konstantna memorija

**Eliminirana memorija** (optimizacijom):
- `NAM_ID(1000)`: 1000 × 15 = 15,000 bytes
- `RASPON(186)`: 186 × 31 = 5,766 bytes
- **Ukupno**: ~20KB uštede

## Napomene

- PL/I indeksiranje počinje od 1
- `BINARY()` automatski ignorira whitespace
- `INDEX(S, '-')` vraća poziciju prvog pojavljivanja
- Sortiranje je **ključno** za ispravan merge algoritam
- Merge algoritam pretpostavlja **sortirane** raspone
- Worst case: 186 merged raspona (svi odvojeni)
- Best case: 1 merged raspon (svi spojeni)
- Prosječan broj merged raspona: 5-20