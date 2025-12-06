# AD02PLI - Validacija Ponavljajućih ID-eva

## Sadržaj

- [Pregled](#pregled)
- [Definicija Problema](#definicija-problema)
- [Deklaracije](#deklaracije)
- [Algoritam](#algoritam)
- [Primjeri Validacije](#primjeri-validacije)
- [Optimizacije](#optimizacije)
- [Kompleksnost](#kompleksnost)

## Pregled

Program analizira raspone ID brojeva i identificira "invalid" ID-eve koji se sastoje od ponavljajućih sekvenci cifara.

**Ulaz**: Jedna linija teksta sa rasponima odvojenim zarezima  
Format: `START1-END1,START2-END2,...`

**Izlaz**: 
- **Part 1**: Suma svih ID-eva koji su **točno 2× ponavljanje**
- **Part 2**: Suma svih ID-eva koji su **≥2× ponavljanje**

## Definicija Problema

### Part 1: Točno 2 Ponavljanja

Invalid ID mora biti sekvenca ponovljena **točno dvaput**.

**Pravila**:
- Duljina mora biti parna
- Lijeva polovica = desna polovica
- Nema leading zeroes (npr. `0101` nije validan)

**Primjeri**:
- ✅ `55` = `5` + `5`
- ✅ `6464` = `64` + `64`
- ✅ `123123` = `123` + `123`
- ❌ `111` = neparna duljina
- ❌ `1234` = ne ponavlja se

### Part 2: ≥2 Ponavljanja

Invalid ID može biti sekvenca ponovljena **2 ili više puta**.

**Primjeri**:
- ✅ `111` = `1` × 3
- ✅ `12341234` = `1234` × 2
- ✅ `123123123` = `123` × 3
- ✅ `1212121212` = `12` × 5
- ✅ `1111111` = `1` × 7
- ❌ `12345` = ne ponavlja se

## Deklaracije

### Glavne Varijable

| Varijabla | Tip | Svrha |
|-----------|-----|-------|
| `UNOS` | `CHAR(511)` | Ulazni string sa rasponima |
| `I`, `LEN` | `FIXED BINARY(31)` | Iterator i duljina stringa |
| `SUMA_PT1` | `FIXED BINARY(63)` | Suma invalid ID-eva (Part 1) |
| `SUMA_PT2` | `FIXED BINARY(63)` | Suma invalid ID-eva (Part 2) |

### Procedure

| Procedura | Parametri | Povratna Vrijednost | Svrha |
|-----------|-----------|---------------------|-------|
| `PARSE_RANGE` | - | - | Parsira jedan raspon i validira ID-eve |

### Funkcije

| Funkcija  | Parametri | Povratna Vrijednost | Svrha |
|-----------|-----------|---------------------|-------|
| `IS_REPEATING` | `S` (string), `MIN_REPS` (int) | `BIT(1)` | Provjerava je li string ponavljajuća sekvenca |

## Algoritam

### 1. Učitavanje i Priprema

```
Učitaj cijelu liniju u UNOS
LEN = duljina bez trailing space-ova
I = 1  // početna pozicija
```

### 2. Glavna Petlja Parsiranja

```
DO WHILE I <= LEN:
    PARSE_RANGE()  // obradi jedan raspon
```

### 3. Procedura: PARSE_RANGE

Parsira format `ID1-ID2,` i validira sve ID-eve u rasponu.

```
START_POS = I

// Nađi poziciju '-'
DO WHILE SUBSTR(UNOS,I,1) ≠ '-':
    I += 1
DASH_POS = I
I += 1

// Nađi poziciju ','
DO WHILE I <= LEN & SUBSTR(UNOS,I,1) ≠ ',':
    I += 1
END_POS = I - 1
I += 1

// Ekstraktiraj brojeve
ID1 = BINARY(SUBSTR(UNOS, START_POS, DASH_POS - START_POS))
ID2 = BINARY(SUBSTR(UNOS, DASH_POS + 1, END_POS - DASH_POS))

// Validiraj sve ID-eve u rasponu
DO N = ID1 TO ID2:
    ID_STR = CHAR(N)
    
    AKO IS_REPEATING(ID_STR, 2):
        SUMA_PT1 += N
    
    AKO IS_REPEATING(ID_STR, 0):
        SUMA_PT2 += N
```

### 4. Procedura: IS_REPEATING

Univerzalna procedura za provjeru ponavljanja.

**Parametri**:
- `S`: String za provjeru
- `MIN_REPS`: Minimalni broj ponavljanja
  - `2` → provjeri **točno** 2 ponavljanja (Part 1)
  - `0` → provjeri **bilo koji** broj ≥2 ponavljanja (Part 2)

**Algoritam**:

```
LEN_S = LENGTH(S)

// Testiraj sve moguće duljine uzorka
DO PLEN = 1 TO LEN_S / 2:
    
    // Provjeri dijeli li PLEN LEN_S
    AKO MOD(LEN_S, PLEN) ≠ 0:
        PRESKOČI  // PLEN ne odgovara
    
    REPS = LEN_S / PLEN  // broj ponavljanja
    
    // Part 1 filtriranje (samo točno 2 ponavljanja)
    AKO MIN_REPS = 2 & REPS ≠ 2:
        PRESKOČI
    
    // Ekstraktiraj uzorak
    PATTERN = SUBSTR(S, 1, PLEN)
    
    // Provjeri sve dijelove stringa
    DO I = PLEN + 1 TO LEN_S BY PLEN:
        AKO SUBSTR(S, I, PLEN) ≠ PATTERN:
            IDI NA NEXT_PLEN  // nepodudaranje
    
    VRATI '1'b  // SVE se podudara!
    
    NEXT_PLEN:

VRATI '0'b  // nema ponavljanja
```

## Primjeri Validacije

### Primjer 1: Raspon `11-22`

```
ID-evi u rasponu: 11, 12, 13, ..., 22

Provjera:
- 11: "11" → PLEN=1, "1"+"1" ✓ INVALID (Part 1 & 2)
- 12: "12" → nije ponavljanje ✗
- 22: "22" → PLEN=1, "2"+"2" ✓ INVALID (Part 1 & 2)

Suma Part 1: 11 + 22 = 33
Suma Part 2: 11 + 22 = 33
```

### Primjer 2: Raspon `95-115`

```
ID-evi: 95, 96, ..., 99, 100, ..., 111, ..., 115

Provjera:
- 99: "99" → PLEN=1, "9"+"9" ✓ INVALID (Part 1 & 2)
- 111: "111" → 
    - PLEN=1: neparan broj ponavljanja (3)
    - Part 1: ✗ (nije točno 2)
    - Part 2: ✓ (1×3 = valid)

Suma Part 1: 99
Suma Part 2: 99 + 111 = 210
```

### Primjer 3: Raspon `998-1012`

```
ID-evi: 998, 999, 1000, ..., 1010, ..., 1012

Provjera:
- 999: "999" → PLEN=1, "9"×3 ✓ (samo Part 2)
- 1010: "1010" → PLEN=2, "10"+"10" ✓ (Part 1 & 2)

Suma Part 1: 1010
Suma Part 2: 999 + 1010 = 2009
```

### Primjer 4: Kompleksni Slučajevi

```
824824824:
- LEN = 9
- PLEN = 1: "8" ne ponavlja (ima 2, 4) ✗
- PLEN = 3: "824" → "824"+"824"+"824" ✓ (Part 2 only)

1212121212:
- LEN = 10
- PLEN = 1: "1" ne ponavlja (ima 2) ✗
- PLEN = 2: "12" → "12"×5 ✓ (Part 2 only)
- PLEN = 5: "12121"+"21212" ✗

1188511885:
- LEN = 10
- PLEN = 5: "11885"+"11885" ✓ (Part 1 & 2)
```

## Optimizacije

### Parsing Optimizacije

**Prije** (dupla varijabla):
```pl1
FIRST_ID = '';
LAST_ID = '';
DO WHILE(...):
    FIRST_ID = FIRST_ID || SUBSTR(...)
```

**Poslije** (direktna konverzija):
```pl1
START_POS = I;
DO WHILE SUBSTR(UNOS,I,1) ^= '-':
    I += 1;
ID1 = BINARY(SUBSTR(UNOS, START_POS, DASH_POS - START_POS));
```

**Prednosti**:
- Eliminira string konkatenaciju
- Direktno kreira brojeve
- ~40% brže parsiranje

### Univerzalna Validacija

**Prije** (2 zasebne procedure):
```pl1
IS_INVALID_PART1(ID)  // provjera samo 2× ponavljanja
IS_INVALID_PART2(ID)  // provjera ≥2× ponavljanja
```

**Poslije** (jedna parametrizirana):
```pl1
IS_REPEATING(ID, MIN_REPS)
// MIN_REPS = 2 → Part 1
// MIN_REPS = 0 → Part 2
```

**Prednosti**:
- DRY princip (Don't Repeat Yourself)
- Lakše održavanje
- Fleksibilnija za proširenja

### Rani Izlaz iz Petlje

**Prije** (flag varijabla):
```pl1
VALID = '1'b;
DO I = ...:
    IF condition THEN DO;
        VALID = '0'b;
        LEAVE;
    END;
END;
IF VALID THEN RETURN('1'b);
```

**Poslije** (GO TO):
```pl1
DO I = ...:
    IF condition THEN GO TO NEXT_PLEN;
END;
RETURN('1'b);  // direktno

NEXT_PLEN:
```

**Prednosti**:
- Manje varijabli
- Jasnije namjere
- Brži execution path

## Kompleksnost

### Vremenska Kompleksnost

**Parsiranje raspona**:
- O(k) gdje je k broj znakova u jednom rasponu
- Prosječno: ~10-20 znakova po rasponu

**Validacija jednog ID-a**:
- Vanjska petlja: O(d) gdje je d broj cifara (duljina ID-a)
- Unutarnja petlja: O(d) za usporedbu dijelova
- **Ukupno po ID-u**: O(d²)

**Cjelokupan program**:
- n = broj raspona
- m = prosječna širina raspona (END - START)
- d = prosječna duljina ID-a
- **Ukupno**: O(n × m × d²)

**Za tipične inpute**:
- n = 30 raspona
- m = 100 brojeva po rasponu
- d = 8 cifara
- **Operacije**: ~30 × 100 × 64 = 192,000

### Prostorna Kompleksnost

- `UNOS`: 511 bytes
- Ostale varijable: ~100 bytes
- Stack depth (rekurzija): nema rekurzije
- **Ukupno**: O(1) konstantna memorija

## Napomene

- PL/I indeksiranje počinje od 1
- `TRIM()` uklanja trailing space-ove iz `CHAR(511)`
- `BINARY()` konverzija ignorira leading/trailing space
- Leading zeroes **ne postoje** u konvertiranim brojevima
- `GO TO` se koristi samo za kontrolu toka unutar procedure
- Suma može narasti do `FIXED BINARY(63)` = ~9.2 × 10¹⁸