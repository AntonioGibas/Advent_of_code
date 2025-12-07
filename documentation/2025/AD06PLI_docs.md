# AD06PLI - Analiza Numeričkih Blokova u ASCII Art Strukturi

## Sadržaj

- [Pregled](#pregled)
- [Definicija Problema](#definicija-problema)
- [Deklaracije](#deklaracije)
- [Algoritam](#algoritam)
- [Detekcija Blokova](#detekcija-blokova)
- [Obrada Brojeva](#obrada-brojeva)
- [Primjeri Izvođenja](#primjeri-izvođenja)
- [Kompleksnost](#kompleksnost)

## Pregled

Program analizira ASCII art strukturu koja sadrži vertikalne stupce brojeva s operatorima u zadnjem redu. Svaki blok brojeva se obrađuje prema specificiranom operatoru (+ ili *), a rezultati se zbrajaju u grand total.

**Ulaz**: Tekstualna datoteka do 100 redaka, širine do 32,000 znakova  
**Izlaz**: `GRAND TOTAL` - suma svih izračunatih blokova

## Definicija Problema

### Struktura Ulaznih Podataka

```
  123  456    789
  111  222    333
  +++  ***    +++
```

**Interpretacija**:
- **Stupci 1-5**: Blok brojeva (123, 111) s operatorom `+`
- **Stupci 7-11**: Blok brojeva (456, 222) s operatorom `*`
- **Stupci 13-17**: Blok brojeva (789, 333) s operatorom `+`

### Pravila Obrade

1. **Detekcija blokova**: Stupci se grupiraju u blokove gdje postoje znakovi (> ASCII space)
2. **Identifikacija operatora**: Zadnji red sadrži operator (`+` ili `*`) za taj blok
3. **Ekstrakcija brojeva**: Brojevi se čitaju **odozdo prema gore** (desno-lijevo u obrnutom redoslijedu)
4. **Izračun**: Brojevi se kombiniraju prema operatoru
5. **Akumulacija**: Rezultat svakog bloka dodaje se u grand total

## Deklaracije

### Datoteke

| Datoteka | Tip | Sadržaj |
|----------|-----|---------|
| `INFL` | Record Input | Tekstualni ulaz s ASCII art strukturom |

### Varijable - Datotečna Obrada

| Varijabla | Tip | Dimenzija | Svrha |
|-----------|-----|-----------|-------|
| `FILE_DATA` | `CHAR(32000) VARYING` | 100 | Spremanje svih redaka datoteke |
| `LINE_BUF` | `CHAR(32000) VARYING` | - | Buffer za čitanje pojedinačnog reda |
| `NUM_ROWS` | `FIXED BINARY(31)` | - | Broj pročitanih redaka |
| `MAX_WIDTH` | `FIXED BINARY(31)` | - | Maksimalna širina redaka |

### Varijable - Detekcija Blokova

| Varijabla | Tip | Svrha |
|-----------|-----|-------|
| `START_COL` | `FIXED BINARY(31)` | Početni stupac trenutnog bloka |
| `IN_PROBLEM` | `BIT(1)` | Flag: trenutno unutar bloka? |
| `C` | `FIXED BINARY(31)` | Iterator stupaca |

### Varijable - Obrada Brojeva

| Varijabla | Tip | Dimenzija | Svrha |
|-----------|-----|-----------|-------|
| `NUMBERS` | `FIXED DECIMAL(31)` | 100 | Ekstraktirani brojevi iz bloka |
| `CNT` | `FIXED BINARY(15)` | - | Broj brojeva u bloku |
| `OPERATOR` | `CHAR(1)` | - | Operator za blok (`+` ili `*`) |
| `BLOCK_SUM` | `FIXED DECIMAL(31)` | - | Rezultat izračuna bloka |
| `GRAND_TOTAL` | `FIXED DECIMAL(31)` | - | Ukupna suma svih blokova |

### Procedure

| Procedura | Parametri | Povratna Vrijednost | Svrha |
|-----------|-----------|---------------------|-------|
| `CHECK_COLUMN` | `COL_IDX` | `BIT(1)` | Provjerava ima li znakova u stupcu |
| `PROCESS_BLOCK` | `C_START`, `C_END` | - | Obrađuje jedan blok brojeva |

## Algoritam

### 1. Učitavanje Datoteke

```
NUM_ROWS = 0
MAX_WIDTH = 0

DO I = 1 TO 100:
    READ LINE_BUF
    FILE_DATA(I) = LINE_BUF
    NUM_ROWS = I
    
    AKO LENGTH(LINE_BUF) > MAX_WIDTH:
        MAX_WIDTH = LENGTH(LINE_BUF)
```

**Karakteristike**:
- Čita maksimalno 100 redaka
- Prati maksimalnu širinu za iteraciju kroz stupce
- Koristi `VARYING` stringove za fleksibilnu duljinu

### 2. Detekcija Blokova (State Machine)

```
IN_PROBLEM = '0'b  // Početno: izvan bloka

DO C = 1 TO MAX_WIDTH + 1:
    
    AKO CHECK_COLUMN(C) = '1'b:  // Stupac ima znakove
        
        AKO IN_PROBLEM = '0'b:
            START_COL = C
            IN_PROBLEM = '1'b  // Ulazak u blok
    
    INAČE:  // Stupac je prazan
        
        AKO IN_PROBLEM = '1'b:
            PROCESS_BLOCK(START_COL, C - 1)
            IN_PROBLEM = '0'b  // Izlazak iz bloka
```

**State Machine Dijagram**:
```
            CHECK_COLUMN = '1'b
┌─────────┐  ───────────────→  ┌─────────┐
│  IZVAN  │                    │ UNUTAR  │
│  BLOKA  │  ←───────────────  │  BLOKA  │
└─────────┘  CHECK_COLUMN='0'b └─────────┘
              (process block)
```

### 3. Procedura: CHECK_COLUMN

Provjerava ima li **bilo koji** ne-space znak u stupcu preko svih redaka.

```
CHECK_COLUMN(COL_IDX):
    
    DO RR = 1 TO NUM_ROWS:
        
        AKO COL_IDX <= LENGTH(FILE_DATA(RR)):
            
            AKO SUBSTR(FILE_DATA(RR), COL_IDX, 1) > ' ':
                VRATI '1'b  // Pronađen znak
    
    VRATI '0'b  // Stupac prazan
```

**Ključni detalj**: `> ' '` provjerava sve znakove s ASCII kodom većim od space (32), uključujući brojeve i operatore.

### 4. Procedura: PROCESS_BLOCK

Obrađuje jedan blok brojeva između stupaca `C_START` i `C_END`.

#### 4.1 Identifikacija Operatora

```
OPERATOR = ' '  // Default: nepoznat

DO CURR_C = C_START TO C_END:
    
    AKO CURR_C <= LENGTH(FILE_DATA(NUM_ROWS)):
        CHAR_NOW = SUBSTR(FILE_DATA(NUM_ROWS), CURR_C, 1)
        
        AKO CHAR_NOW = '+' ILI CHAR_NOW = '*':
            OPERATOR = CHAR_NOW

AKO OPERATOR = ' ':
    OPERATOR = '+'  // Fallback: zbrajanje
```

**Pretpostavka**: Operator se nalazi u zadnjem redu (`NUM_ROWS`).

#### 4.2 Ekstrakcija Brojeva (Desno-Lijevo, Odozdo-Gore)

```
CNT = 0

DO CURR_C = C_END TO C_START BY -1:  // Desno → Lijevo
    
    TEMP_STR = ''
    
    DO RR = 1 TO NUM_ROWS - 1:  // Sve osim zadnjeg reda
        
        AKO CURR_C <= LENGTH(FILE_DATA(RR)):
            CHAR_NOW = SUBSTR(FILE_DATA(RR), CURR_C, 1)
            
            AKO CHAR_NOW >= '0' & CHAR_NOW <= '9':
                TEMP_STR = TEMP_STR || CHAR_NOW
    
    AKO LENGTH(TEMP_STR) > 0:
        CNT += 1
        NUMBERS(CNT) = DECIMAL(TEMP_STR)
```

**Primjer ekstrakcije**:
```
Ulaz:
  123
  456
  +++

Stupac 5 (kraj): '3' || '6' → "36" → NUMBERS(1) = 36
Stupac 4:        '2' || '5' → "25" → NUMBERS(2) = 25
Stupac 3:        '1' || '4' → "14" → NUMBERS(3) = 14
```

#### 4.3 Izračun Rezultata

```
AKO CNT > 0:
    
    BLOCK_SUM = NUMBERS(1)  // Početna vrijednost
    
    DO K = 2 TO CNT:
        
        AKO OPERATOR = '+':
            BLOCK_SUM += NUMBERS(K)
        INAČE:  // OPERATOR = '*'
            BLOCK_SUM *= NUMBERS(K)
    
    GRAND_TOTAL += BLOCK_SUM
```

**Primjer izračuna** (nastavljajući gornji primjer):
```
Operator: '+'
NUMBERS = [36, 25, 14]

BLOCK_SUM = 36
BLOCK_SUM = 36 + 25 = 61
BLOCK_SUM = 61 + 14 = 75

GRAND_TOTAL += 75
```

## Detekcija Blokova

### Primjer 1: Jedan Blok

```
Input:
  123
  456
  +++

Stupci: 1 2 3 4 5 6 7 ...
        · · 1 2 3 · · ...  (· = space)

Detekcija:
C=1,2: CHECK_COLUMN='0'b, IN_PROBLEM='0'b
C=3:   CHECK_COLUMN='1'b, IN_PROBLEM='1'b, START_COL=3
C=4,5: CHECK_COLUMN='1'b, IN_PROBLEM='1'b
C=6:   CHECK_COLUMN='0'b → PROCESS_BLOCK(3, 5)
```

### Primjer 2: Više Blokova

```
Input:
  12  34
  56  78
  ++  **

Stupci: 1 2 3 4 5 6 7 ...
        · 1 2 · 3 4 · ...

Detekcija:
C=2:  START_COL=2, IN_PROBLEM='1'b
C=4:  PROCESS_BLOCK(2,3) → rezultat bloka 1
C=5:  START_COL=5, IN_PROBLEM='1'b
C=7:  PROCESS_BLOCK(5,6) → rezultat bloka 2
```

## Obrada Brojeva

### Primjer 1: Zbrajanje

```
Blok:
  1  2  3
  4  5  6
  +  +  +

Ekstrakcija (desno-lijevo):
Stupac 5: '3' + '6' → "36"
Stupac 3: '2' + '5' → "25"
Stupac 1: '1' + '4' → "14"

NUMBERS = [36, 25, 14]

Izračun:
36 + 25 + 14 = 75
```

### Primjer 2: Množenje

```
Blok:
  2  3
  4  5
  *  *

Ekstrakcija:
Stupac 4: '3' + '5' → "35"
Stupac 2: '2' + '4' → "24"

NUMBERS = [35, 24]

Izračun:
35 * 24 = 840
```

### Primjer 3: Multi-Digit Brojevi

```
Blok:
  1
  2
  3
  +

Ekstrakcija:
Stupac 1: '1' + '2' + '3' → "123"

NUMBERS = [123]

Izračun:
123 (jedan broj → nema dodatnih operacija)
```

## Primjeri Izvođenja

### Test Case 1: Jednostavni Blokovi

```
Input:
  10  20
  30  40
  ++  **

Blok 1 (stupci 1-4):
- Operator: '+'
- Brojevi: ["40", "10"] (desno-lijevo: stupac 4, stupac 2)
- Konvertirano: [40, 10]
- Rezultat: 40 + 10 = 50

Blok 2 (stupci 6-9):
- Operator: '*'
- Brojevi: ["60", "20"] (stupac 9, stupac 7)
- Konvertirano: [60, 20]
- Rezultat: 60 * 20 = 1200

GRAND TOTAL: 50 + 1200 = 1250
```

### Test Case 2: Različite Visine

```
Input:
  1
  2
  3
  4
  +

Blok 1 (stupac 1):
- Operator: '+'
- Brojevi: ["1234"] (svi redovi konkaternirani)
- Konvertirano: [1234]
- Rezultat: 1234

GRAND TOTAL: 1234
```

### Test Case 3: Bez Operatora (Fallback)

```
Input:
  100
  200

Blok 1:
- Operator: '+' (default fallback)
- Brojevi: ["200", "100"]
- Rezultat: 200 + 100 = 300

GRAND TOTAL: 300
```

## Kompleksnost

### Vremenska Kompleksnost

**Čitanje datoteke**: O(n × m)
- n = broj redaka (max 100)
- m = prosječna duljina reda
- **Ukupno**: O(100 × 32,000) = O(3,200,000)

**Detekcija blokova**: O(w × r)
- w = MAX_WIDTH (širina najduljeg reda)
- r = NUM_ROWS (broj redaka)
- Za svaki stupac: CHECK_COLUMN prolazi kroz sve redove
- **Ukupno**: O(w × r) = O(32,000 × 100) = O(3,200,000)

**Obrada jednog bloka**: O(w × r)
- w_block = širina bloka
- Iteracija kroz stupce bloka: w_block
- Za svaki stupac iteracija kroz redove: r
- **Po bloku**: O(w_block × r)

**Cjelokupan program**: O(w × r + b × w_avg × r)
- b = broj blokova
- w_avg = prosječna širina bloka
- **Worst case**: O(w × r) ako je cijela datoteka jedan blok

**Praktična analiza**:
- Tipično: 100 redaka × 200 znakova = 20,000 operacija po bloku
- S 10 blokova: ~200,000 operacija

### Prostorna Kompleksnost

**FILE_DATA**: O(n × m)
- 100 redaka × 32,000 znakova = **3.2 MB maksimalno**
- Realna upotreba: 100 × 200 = **20 KB**

**NUMBERS niz**: O(k)
- k = broj stupaca u bloku (max 100)
- 100 × 31 bytes = **3.1 KB**

**Ostale varijable**: O(1)
- Privremeni stringovi: ~100 bytes

**Ukupno**: O(n × m) = **~20 KB - 3.2 MB**

## Napomene

### Pretpostavke

1. **Zadnji red sadrži operatore**: Program pretpostavlja da se operator nalazi u zadnjem redu datoteke
2. **Brojevi su pozitivni**: Algoritam ne rukuje negativnim brojevima
3. **Jedan operator po bloku**: Svaki blok ima samo jedan operator

### Rubni Slučajevi

**Prazna datoteka**:
```
GRAND TOTAL: 0
```

**Redak bez brojeva**:
```
  + + +
→ CNT = 0, blok se preskače
```

**Blok samo s operatorom**:
```
  +
→ CNT = 0, blok se preskače
```

**Multi-width brojevi**:
```
  1 2 3
  4 5 6
  + + +

→ Svaki stupac se obrađuje zasebno
→ 3 zasebna bloka: [14], [25], [36]
```

### Optimizacije

**Moguće ali neuključene**:
1. **Paralelna obrada blokova**: Blokovi su neovisni
2. **Bufferirana I/O**: Smanjenje poziva READ FILE
3. **Range detection**: Pronaći MIN/MAX stupaca s podacima

**Razlog**: Kod je dovoljno brz za maksimalne ulaze (100×32,000), dodatna kompleksnost nije potrebna.

### Ograničenja

- Maksimalno **100 redaka** (hardkodirano)
- Maksimalno **32,000 znakova po redu** (CHAR dimenzija)
- Maksimalno **100 brojeva po bloku** (NUMBERS dimenzija)
- Podržava samo **+ i *** operatore
- **Decimal overflow**: Rezultati veći od ~2×10³¹ mogu izazvati grešku