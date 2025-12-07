# AD01PLI - Simulacija Kružnog Brojčanika (Circular Dial)

## Sadržaj

- [Pregled](#pregled)
- [Definicija Problema](#definicija-problema)
- [Deklaracije](#deklaracije)
- [Algoritam](#algoritam)
- [Kružna Aritmetika](#kružna-aritmetika)
- [Primjeri Izvođenja](#primjeri-izvođenja)
- [Kompleksnost](#kompleksnost)

## Pregled

Program simulira rotaciju brojčanika sa 100 pozicija (0-99) i broji koliko puta pokazivač prolazi kroz poziciju 0 tijekom izvršavanja niza rotacija.

**Ulaz**: 4059 rotacijskih naredbi (format: `L123` ili `R456`)  
**Izlazi**: 
- **KONACNA POZICIJA**: Završna pozicija pokazivača
- **BROJ NULA**: Broj prelazaka kroz poziciju 0

## Definicija Problema

### Advent of Code 2025 - Day 1

**Kontekst**: Otvaranje sefa na sjevernom polu pomoću brojčanika.

### Part 1: Brojanje Završnih Pozicija
Broji koliko puta pokazivač **završi** na poziciji 0 nakon bilo koje rotacije.

### Part 2: Brojanje Svih Prelazaka (Password Method 0x434C49434B)
Broji **sve** prelazake kroz poziciju 0, uključujući prelazake **tijekom** rotacije.

**Primjer razlike**:
```
Pozicija: 50
Rotacija: L68

Part 1: Pokazivač završi na 82 (ne broji se)
Part 2: Tijekom rotacije pokazivač prolazi kroz 0 jednom
        (50 → 49 → ... → 1 → 0 → 99 → ... → 82)
```

## Deklaracije

### Datoteke

| Datoteka | Tip | Sadržaj |
|----------|-----|---------|
| `INFL` | Record Input | 4059 rotacijskih naredbi |

### Varijable - Ulazni Podaci

| Varijabla | Tip | Dimenzija | Svrha |
|-----------|-----|-----------|-------|
| `INPUT_ARRAY` | `CHAR(4) VARYING` | 4059 | Spremanje svih rotacijskih naredbi |
| `DIRECTION_INPUT` | `CHAR(1)` | - | Smjer rotacije ('L' ili 'R') |
| `VALUE_INPUT` | `FIXED BINARY(31)` | - | Broj klikova za rotaciju |

### Varijable - Stanje Brojčanika

| Varijabla | Tip | Inicijalizacija | Svrha |
|-----------|-----|-----------------|-------|
| `CURRENT_POSITION` | `FIXED BINARY(31)` | 50 | Trenutna pozicija pokazivača (0-99) |
| `COUNT_ZERO` | `FIXED BINARY(31)` | 0 | Brojač prelazaka kroz 0 |

### Varijable - Iteratori

| Varijabla | Tip | Svrha |
|-----------|-----|-------|
| `I` | `FIXED BINARY(31)` | Iterator za rotacije (1-4059) |
| `J` | `FIXED BINARY(31)` | Iterator za pojedinačne klikove |

### Nekorištene Varijable

| Varijabla | Deklaracija | Napomena |
|-----------|-------------|----------|
| `OLD_POS` | `FIXED BINARY(31)` | Vjerojatno ostatak od debug koda |
| `NEW_POS` | `FIXED BINARY(31)` | Nekorišteno |
| `CROSSES` | `FIXED BINARY(31)` | Nekorišteno |

## Algoritam

### 1. Učitavanje Rotacijskih Naredbi

```
OPEN FILE(INFL)

DO I = 1 TO 4059:
    READ FILE(INFL) INTO(INPUT_ARRAY(I))

CLOSE FILE(INFL)
```

**Format naredbe**:
- `L68`: Lijevo 68 klikova
- `R123`: Desno 123 klika

### 2. Inicijalizacija

```
CURRENT_POSITION = 50  // Početna pozicija
COUNT_ZERO = 0         // Brojač prelazaka kroz 0
```

**Početno stanje**: Pokazivač pokazuje na poziciju 50 na kružnom brojčaniku od 100 pozicija.

### 3. Procesiranje Svake Rotacije

```
DO I = 1 TO 4059:
    
    // Parsiranje naredbe
    DIRECTION_INPUT = SUBSTR(INPUT_ARRAY(I), 1, 1)
    VALUE_INPUT = BINARY(SCRUBOUT(SUBSTR(INPUT_ARRAY(I), 2), ' '))
    
    // Spremanje stare pozicije (nekorišteno)
    OLD_POS = CURRENT_POSITION
    
    // Izvršavanje rotacije
    AKO DIRECTION_INPUT = 'R':
        CALL ROTATE_RIGHT(VALUE_INPUT)
    INAČE AKO DIRECTION_INPUT = 'L':
        CALL ROTATE_LEFT(VALUE_INPUT)
```

**Parsiranje**:
- `SUBSTR(INPUT_ARRAY(I), 1, 1)`: Ekstraktira prvi znak (smjer)
- `SUBSTR(INPUT_ARRAY(I), 2)`: Ekstraktira ostatak (broj)
- `SCRUBOUT(..., ' ')`: Uklanja space znakove
- `BINARY(...)`: Konvertira string u broj

### 4. Rotacija Desno (R)

```
ROTATE_RIGHT(VALUE_INPUT):
    
    DO J = 1 TO VALUE_INPUT:
        
        CURRENT_POSITION = MOD(CURRENT_POSITION + 1, 100)
        
        AKO CURRENT_POSITION = 0:
            COUNT_ZERO = COUNT_ZERO + 1
```

**Logika po kliku**:
1. Pomakni pokazivač +1 poziciju
2. Primjeni modulo 100 (kružno ponašanje)
3. Ako si na 0, uvećaj brojač

**Primjer**:
```
Pozicija: 98
Rotacija: R3

Klik 1: 98 → 99
Klik 2: 99 → 0   ✓ COUNT_ZERO++
Klik 3: 0 → 1
```

### 5. Rotacija Lijevo (L)

```
ROTATE_LEFT(VALUE_INPUT):
    
    DO J = 1 TO VALUE_INPUT:
        
        CURRENT_POSITION = MOD(CURRENT_POSITION - 1 + 100, 100)
        
        AKO CURRENT_POSITION = 0:
            COUNT_ZERO = COUNT_ZERO + 1
```

**Kružna aritmetika lijevo**:
- `CURRENT_POSITION - 1`: Osnovna operacija
- `+ 100`: Osigurava pozitivan rezultat za MOD
- `MOD(..., 100)`: Wrap-around na 0-99 raspon

**Primjer**:
```
Pozicija: 2
Rotacija: L3

Klik 1: 2 → 1
Klik 2: 1 → 0   ✓ COUNT_ZERO++
Klik 3: 0 → 99
```

### 6. Ispis Rezultata

```
PUT SKIP EDIT(
    'KONACNA POZICIJA: ',
    SCRUBOUT(CHAR(CURRENT_POSITION), ' '),
    ', BROJ NULA: ',
    SCRUBOUT(CHAR(COUNT_ZERO), ' ')
)(A)
```

**Konverzije**:
- `CHAR(broj)`: Pretvara broj u string
- `SCRUBOUT(..., ' ')`: Uklanja leading/trailing space-ove
- `PUT SKIP EDIT(...)(A)`: Formatiran ispis

## Kružna Aritmetika

### Modulo Operacija za Kružni Brojčanik

Program koristi **modulo 100** za simulaciju kružnog brojčanika sa 100 pozicija (0-99).

### Rotacija Desno (Clockwise)

```
Nova_Pozicija = MOD(Trenutna_Pozicija + 1, 100)
```

**Primjeri**:
| Trenutna | +1 | MOD 100 | Nova |
|----------|----|---------|----- |
| 0        | 1  | 1       | 1    |
| 50       | 51 | 51      | 51   |
| 99       | 100| 0       | 0    |

### Rotacija Lijevo (Counterclockwise)

```
Nova_Pozicija = MOD(Trenutna_Pozicija - 1 + 100, 100)
```

**Zašto +100?**  
Da se izbjegnu negativni brojevi prije primjene modula.

**Primjeri**:
| Trenutna | -1 | +100 | MOD 100 | Nova |
|----------|----|------|---------|------|
| 1        | 0  | 100  | 0       | 0    |
| 0        | -1 | 99   | 99      | 99   |
| 50       | 49 | 149  | 49      | 49   |

**Alternativa bez +100** (ne radi u PL/I):
```
// Ovo može dati negativan rezultat u nekim jezicima:
MOD(-1, 100) = -1  (u Python)
MOD(-1, 100) = 99  (željeni rezultat)

// Sigurna verzija:
MOD(X - 1 + 100, 100)
```

## Primjeri Izvođenja

### Primjer 1: Jednostavna Rotacija

```
Input:
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82

Izvođenje:
Početak: 50
L68:  50 → ... → 0 (prelazak) → ... → 82  COUNT_ZERO = 1
L30:  82 → ... → 52                       COUNT_ZERO = 1
R48:  52 → ... → 0 (prelazak) → ... → 0  COUNT_ZERO = 2
L5:   0 → 99 → 98 → 97 → 96 → 95         COUNT_ZERO = 2
R60:  95 → ... → 0 (prelazak) → ... → 55 COUNT_ZERO = 3
L55:  55 → ... → 0                        COUNT_ZERO = 4
L1:   0 → 99                              COUNT_ZERO = 4
L99:  99 → ... → 0                        COUNT_ZERO = 5
R14:  0 → ... → 14                        COUNT_ZERO = 5
L82:  14 → ... → 0 (prelazak) → ... → 32 COUNT_ZERO = 6

Konačna pozicija: 32
Broj prelazaka kroz 0: 6
```

### Primjer 2: Višestruki Prelasci u Jednoj Rotaciji

```
Input:
R1000

Početak: 50

Analiza:
- VALUE_INPUT = 1000
- Svaki ciklus od 100 klikova prolazi kroz 0 jednom
- 1000 klikova = 10 ciklusa = 10 prelazaka kroz 0

Izvođenje:
50 → 51 → ... → 99 → 0 (1. prelazak)
0 → 1 → ... → 99 → 0 (2. prelazak)
...
0 → 1 → ... → 99 → 0 (10. prelazak)
0 → ... → 50 (konačna pozicija)

COUNT_ZERO = 10
```

### Primjer 3: Lijeva Rotacija Kroz 0

```
Input:
L10

Početak: 5

Izvođenje:
5 → 4 → 3 → 2 → 1 → 0 (prelazak, COUNT_ZERO++)
0 → 99 → 98 → 97 → 96 → 95

Konačna pozicija: 95
Broj prelazaka: 1
```

### Primjer 4: Klik-po-Klik Analiza

```
Input:
L3

Početak: 1

Klik-po-klik:
J=1: CURRENT_POSITION = MOD(1 - 1 + 100, 100) = MOD(100, 100) = 0
     IF 0 = 0 → COUNT_ZERO = 1

J=2: CURRENT_POSITION = MOD(0 - 1 + 100, 100) = MOD(99, 100) = 99
     IF 99 = 0 → FALSE

J=3: CURRENT_POSITION = MOD(99 - 1 + 100, 100) = MOD(198, 100) = 98
     IF 98 = 0 → FALSE

Konačna pozicija: 98
Broj prelazaka: 1
```

## Kompleksnost

### Vremenska Kompleksnost

**Učitavanje ulaza**: O(n)
- n = 4059 rotacijskih naredbi
- **Operacija**: O(4,059)

**Procesiranje rotacija**: O(n × m)
- n = 4059 rotacija
- m = prosječna vrijednost rotacije
- Za svaku rotaciju iteriramo `VALUE_INPUT` puta

**Worst case analiza**:
```
Ako svaka rotacija ima VALUE_INPUT = 1000:
4059 × 1000 = 4,059,000 operacija
```

**Best case**:
```
Ako svaka rotacija ima VALUE_INPUT = 1:
4059 × 1 = 4,059 operacija
```

**Prosječan slučaj** (pretpostavka VALUE_INPUT ≈ 500):
```
4059 × 500 = 2,029,500 operacija
```

**Cjelokupan program**: O(n × m) gdje je m prosječna vrijednost rotacije

### Prostorna Kompleksnost

**INPUT_ARRAY**: O(n)
- 4059 × 4 znaka = **16,236 bytes** (~16 KB)

**Ostale varijable**: O(1)
- CURRENT_POSITION: 4 bytes
- COUNT_ZERO: 4 bytes
- I, J, VALUE_INPUT: ~12 bytes

**Ukupno**: O(n) = **~16 KB**

### Optimizacija Mogućnosti

**Trenutna implementacija**: Klik-po-klik simulacija
```pl1
DO J = 1 TO VALUE_INPUT;
    CURRENT_POSITION = MOD(CURRENT_POSITION + 1, 100);
    IF CURRENT_POSITION = 0 THEN COUNT_ZERO += 1;
END;
```

**Optimizirana implementacija**: Matematički izračun
```pl1
// Izračunaj broj prelazaka bez petlje
NEW_POS = MOD(CURRENT_POSITION + VALUE_INPUT, 100);
CROSSES = VALUE_INPUT / 100;  // Puni ciklusi
IF CURRENT_POSITION + MOD(VALUE_INPUT, 100) >= 100 THEN
    CROSSES += 1;  // Dodatni prelazak
COUNT_ZERO += CROSSES;
CURRENT_POSITION = NEW_POS;
```

**Poboljšanje**: O(n × m) → O(n)
- Eliminira unutarnju petlju
- ~1000× brže za velike rotacije

## Napomene

### Advent of Code Rješenje

**Part 1 odgovor**: 997
- Broji samo završne pozicije na 0

**Part 2 odgovor**: 5978
- Broji sve prelazake kroz 0 (implementirano u ovom kodu)

### Implementacijske Detalji

**Parsiranje naredbi**:
```pl1
DIRECTION_INPUT = SUBSTR(INPUT_ARRAY(I), 1, 1)
VALUE_INPUT = BINARY(SCRUBOUT(SUBSTR(INPUT_ARRAY(I), 2), ' '))
```
- `SUBSTR(..., 1, 1)`: Prvi znak (L/R)
- `SUBSTR(..., 2)`: Od drugog znaka do kraja (broj)
- `SCRUBOUT`: Čisti space-ove (bitno za CHAR(4) VARYING)
- `BINARY`: Konverzija string → integer

**Modulo trick za lijevo**:
```pl1
MOD(CURRENT_POSITION - 1 + 100, 100)
```
- PL/I MOD može vratiti negativne vrijednosti
- +100 osigurava pozitivan argument
- Uvijek daje rezultat 0-99

### Rubni Slučajevi

**Pozicija 0, rotacija R100**:
```
0 → 1 → ... → 99 → 0 (prelazak) → 1 → ... → 99 → 0 (prelazak) → 0
COUNT_ZERO += 2
```

**Pozicija 0, rotacija L1**:
```
0 → 99
COUNT_ZERO += 0 (nema prelazaka)
```

**Pozicija 99, rotacija R1**:
```
99 → 0
COUNT_ZERO += 1
```

**Prazna rotacija (VALUE_INPUT = 0)**:
```
DO J = 1 TO 0;  // Petlja se ne izvršava
END;
COUNT_ZERO += 0
```

### Debugging Varijable

**Nekorištene varijable**:
```pl1
DCL (OLD_POS, NEW_POS, CROSSES) FIXED BINARY(31);
OLD_POS = CURRENT_POSITION;  // Jedina upotreba
```

Ove varijable su vjerojatno bile korištene za debugging ili raniju verziju algoritma.

### Performanse

**Stvarno izvođenje**:
- 4059 rotacija
- Prosječna vrijednost: ~500-1000 klikova
- **Ukupno operacija**: ~2-4 milijuna
- **Vrijeme izvođenja**: <1 sekunda na modernom mainframeu

### PL/I Specifičnosti

**SCRUBOUT funkcija**:
```pl1
SCRUBOUT(CHAR(broj), ' ')
```
- PL/I built-in funkcija
- Uklanja specificirane znakove iz stringa
- Ekvivalent `TRIM` funkciji u drugim jezicima

**MOD funkcija**:
```pl1
MOD(dividend, divisor)
```
- Vraća ostatak dijeljenja
- **Napomena**: Može vratiti negativan rezultat za negativan dividend
- Zato se koristi `+ 100` trik