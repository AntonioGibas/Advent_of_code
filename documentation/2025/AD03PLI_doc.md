# AD03PLI - Maksimizacija Joltage Izlaza (Battery Bank Optimization)

## Sadržaj

- [Pregled](#pregled)
- [Definicija Problema](#definicija-problema)
- [Deklaracije](#deklaracije)
- [Algoritam](#algoritam)
- [Greedy Pristup](#greedy-pristup)
- [Primjeri Izvođenja](#primjeri-izvođenja)
- [Kompleksnost](#kompleksnost)

## Pregled

Program pronalazi maksimalni 12-cifreni broj koji se može formirati odabirom točno 12 cifara iz niza cifara, uz očuvanje originalnog redoslijeda. Koristi **greedy algoritam** za efikasnu selekciju cifara.

**Ulaz**: 200 redaka, svaki sadrži banku baterija (niz cifara)  
**Izlaz**: 
- **UKUPNA SUMA**: Zbroj maksimalnih joltage vrijednosti svih banki

## Definicija Problema

### Advent of Code 2025 - Day 3

**Kontekst**: Opskrba eskalatora električnom energijom pomoću baterija.

### Part 1: Dva Baterija (Nije implementirano)
Odaberi točno 2 baterije da formiraju najveći mogući 2-cifreni broj.

**Primjer**:
```
Bank: 987654321111111
Odabir: 9 i 8 (pozicije 1 i 2)
Joltage: 98
```

### Part 2: Dvanaest Baterija (Implementirano)
Odaberi točno **12 baterija** da formiraju najveći mogući 12-cifreni broj.

**Pravila**:
- Mora se odabrati točno 12 cifara
- Redoslijed mora ostati isti (ne smije se prerasporediti)
- Cilj: maksimizirati numeričku vrijednost

**Primjer**:
```
Bank: 987654321111111 (15 cifara)
Mora odabrati: 12 cifara
Preskače: 3 najlošije cifre

Optimalno: 987654321111 (sve osim zadnje 3 jedinice)
Rezultat: 987,654,321,111
```

## Deklaracije

### Datoteke

| Datoteka | Tip | Sadržaj |
|----------|-----|---------|
| `INFL` | Record Input | 200 redaka s bankama baterija |

### Varijable - Glavne

| Varijabla | Tip | Dimenzija | Inicijalizacija | Svrha |
|-----------|-----|-----------|-----------------|-------|
| `UNOS` | `CHAR(100)` | 200 | - | Spremanje redaka baterija |
| `SUMA` | `FIXED DECIMAL(18,0)` | - | 0 | Ukupna suma joltage vrijednosti |
| `DOBIVENI_BROJ_STR` | `CHAR(12)` | - | - | Rezultat za jedan red (12 cifara) |
| `I` | `FIXED BINARY(15)` | - | 0 | Iterator za redove |

### Varijable - Nekorištene (Legacy)

| Varijabla | Tip | Napomena |
|-----------|-----|----------|
| `DIGIT1`, `DIGIT2` | `CHAR(1)` | Ostaci od Part 1 implementacije |
| `CONCATENATED` | `CHAR(2)` | Ostaci od Part 1 implementacije |

### Procedura FIND_BEST_12 - Parametri

| Varijabla | Tip | Svrha |
|-----------|-----|-------|
| `DIGITS_RAW` | `CHAR(100)` | Ulazni niz cifara (jedan red) |
| `DIGITS` | `CHAR(100) VARYING` | Trimmed verzija ulaza |
| `RESULT` | `CHAR(12) VARYING` | Rezultirajući 12-cifreni string |

### Procedura FIND_BEST_12 - Lokalne Varijable

| Varijabla | Tip | Svrha |
|-----------|-----|-------|
| `LEN` | `FIXED BINARY(15)` | Duljina ulaznog niza |
| `REMAINING` | `FIXED BINARY(15)` | Broj preostalih cifara za odabir |
| `LIMIT` | `FIXED BINARY(15)` | Maksimalna pozicija za pretragu |
| `LAST_POS` | `FIXED BINARY(15)` | Pozicija posljednje odabrane cifre |
| `MAX_VAL` | `FIXED BINARY(15)` | Najveća pronađena cifra u trenutnoj iteraciji |
| `CURR_VAL` | `FIXED BINARY(15)` | Vrijednost trenutne cifre |
| `BEST_DIGIT` | `CHAR(1)` | Najbolja pronađena cifra |
| `BEST_POS` | `FIXED BINARY(15)` | Pozicija najbolje cifre |
| `K`, `J` | `FIXED BINARY(15)` | Iteratori |

## Algoritam

### 1. Učitavanje Podataka

```
OPEN FILE(INFL)

DO I = 1 TO 200:
    READ FILE(INFL) INTO(UNOS(I))

CLOSE FILE(INFL)
```

**Karakteristike**:
- Učitava točno 200 redaka
- Svaki red može imati do 100 znakova
- Redovi sadrže samo cifre (0-9)

### 2. Glavni Loop Obrade

```
SUMA = 0

DO I = 1 TO 200:
    
    // Pronađi najboljih 12 cifara
    DOBIVENI_BROJ_STR = FIND_BEST_12(UNOS(I))
    
    // Ispis za debugging
    PUT SKIP EDIT('Red ', I, ': ', DOBIVENI_BROJ_STR)
    
    // Konvertiraj u broj i dodaj u sumu
    SUMA = SUMA + DECIMAL(DOBIVENI_BROJ_STR, 15, 0)

PUT SKIP EDIT('UKUPNA SUMA: ', SUMA)
```

**Proces**:
1. Za svaki red pozovi `FIND_BEST_12`
2. Konvertiraj string rezultat u decimalni broj
3. Akumuliraj u ukupnu sumu
4. Isprintaj konačnu sumu

### 3. Procedura FIND_BEST_12: Greedy Algoritam

Ključna procedura koja implementira greedy pristup za odabir najboljh 12 cifara.

#### 3.1 Inicijalizacija

```
DIGITS = TRIM(DIGITS_RAW)
LEN = LENGTH(DIGITS)
RESULT = ''
LAST_POS = 0
```

**Setup**:
- Ukloni trailing space-ove
- Odredi duljinu niza
- Inicijaliziraj prazan rezultat
- Postavi početnu poziciju na 0

#### 3.2 Greedy Selection Loop

```
DO K = 1 TO 12:  // Za svaku od 12 pozicija
    
    REMAINING = 12 - K        // Koliko još cifara treba odabrati
    LIMIT = LEN - REMAINING   // Koliko daleko možemo tražiti
    
    MAX_VAL = -1
    BEST_DIGIT = ''
    
    // Pronađi najveću cifru u validnom rasponu
    DO J = LAST_POS + 1 TO LIMIT:
        
        CURR_VAL = BINARY(SUBSTR(DIGITS, J, 1))
        
        IF CURR_VAL > MAX_VAL:
            MAX_VAL = CURR_VAL
            BEST_POS = J
            BEST_DIGIT = SUBSTR(DIGITS, J, 1)
            
            IF MAX_VAL = 9:
                LEAVE  // Early exit: našli smo najbolju moguću cifru
    
    RESULT = RESULT || BEST_DIGIT
    LAST_POS = BEST_POS

RETURN(RESULT)
```

**Ključna Logika**:

1. **REMAINING**: Broj cifara koje još trebamo odabrati nakon trenutne
2. **LIMIT**: Najdalja pozicija gdje možemo tražiti trenutnu cifru
   - Mora ostaviti dovoljno prostora za preostale cifre
3. **Greedy izbor**: Odaberi najveću dostupnu cifru u validnom rasponu
4. **Early exit**: Ako nađemo 9, ne trebamo dalje tražiti

#### 3.3 Izračun LIMIT-a

```
K = trenutna iteracija (1-12)
REMAINING = 12 - K
LIMIT = LEN - REMAINING
```

**Primjer** (LEN = 15):
| K | REMAINING | LIMIT | Značenje |
|---|-----------|-------|----------|
| 1 | 11 | 4 | Za 1. cifru, pregledaj pozicije 1-4 |
| 2 | 10 | 5 | Za 2. cifru, pregledaj pozicije (LAST_POS+1)-5 |
| ... | ... | ... | ... |
| 11 | 1 | 14 | Za 11. cifru, pregledaj pozicije (LAST_POS+1)-14 |
| 12 | 0 | 15 | Za 12. cifru, pregledaj pozicije (LAST_POS+1)-15 |

**Zašto LIMIT?**  
Moramo osigurati da nakon odabira trenutne cifre, još uvijek imamo dovoljno preostalih cifara za odabir.

## Greedy Pristup

### Strategija Algoritma

**Greedy princip**: Za svaku poziciju, odaberi **najveću dostupnu cifru** koja ostavlja dovoljno mjesta za preostale cifre.

### Dokaz Optimalnosti

**Teorem**: Greedy algoritam uvijek pronalazi maksimalni 12-cifreni broj.

**Dokaz (kontradikcijom)**:

Pretpostavimo da greedy ne daje optimalno rješenje. Tada postoji bolji odabir gdje smo na nekoj poziciji `i` odabrali manju cifru `d1` umjesto veće `d2`.

```
Greedy: ...d2...
Bolje:  ...d1...  (d1 < d2)
```

Ali to znači da je na poziciji `i` greedy broj veći:
```
Greedy broj = X × 10^(12-i) + d2 × 10^(11-i) + Y
Bolje broj  = X × 10^(12-i) + d1 × 10^(11-i) + Z

Budući da d2 > d1:
d2 × 10^(11-i) > d1 × 10^(11-i)
```

Čak i ako je `Z > Y` (preostale cifre su bolje), razlika u višim pozicijama dominira:
```
(d2 - d1) × 10^(11-i) > bilo koja razlika u Z-Y
```

Dakle, greedy mora biti optimalan. **QED**

### Zašto LIMIT Osigurava Validnost

```
Na iteraciji K:
- Treba još odabrati: REMAINING = 12 - K
- Preostalo cifara nakon LIMIT: LEN - LIMIT = REMAINING

Ako odaberemo cifru na poziciji J <= LIMIT:
- Cifara koje ostaju: LEN - J >= LEN - LIMIT = REMAINING
- To je točno onoliko koliko nam treba!
```

## Primjeri Izvođenja

### Primjer 1: Osnovna Selekcija

```
Input: 987654321111111 (15 cifara)
Cilj: Odaberi 12 cifara

Izvođenje FIND_BEST_12:

K=1: REMAINING=11, LIMIT=4
     Pretraži pozicije 1-4: [9,8,7,6]
     MAX=9, BEST_POS=1, RESULT="9"
     LAST_POS=1

K=2: REMAINING=10, LIMIT=5
     Pretraži pozicije 2-5: [8,7,6,5]
     MAX=8, BEST_POS=2, RESULT="98"
     LAST_POS=2

K=3: REMAINING=9, LIMIT=6
     Pretraži pozicije 3-6: [7,6,5,4]
     MAX=7, BEST_POS=3, RESULT="987"
     LAST_POS=3

... (nastavlja se logikom) ...

K=12: REMAINING=0, LIMIT=15
      Pretraži pozicije 12-15: [1,1,1,1]
      MAX=1, BEST_POS=12, RESULT="987654321111"
      LAST_POS=12

Rezultat: "987654321111"
Vrijednost: 987,654,321,111
```

### Primjer 2: Skip Loših Cifara

```
Input: 811111111111119 (15 cifara)
Cilj: Odaberi 12 cifara

Izvođenje:

K=1: Pretraži pozicije 1-4: [8,1,1,1]
     MAX=8, RESULT="8"
     LAST_POS=1

K=2: Pretraži pozicije 2-5: [1,1,1,1]
     MAX=1, RESULT="81"
     LAST_POS=2

K=3-11: Svaki put odabire "1"
        RESULT="81111111111"

K=12: Pretraži pozicije 13-15: [1,1,9]
      MAX=9, RESULT="811111111119"
      LAST_POS=15

Rezultat: "811111111119"
Vrijednost: 811,111,111,119
```

### Primjer 3: Kompleksna Optimizacija

```
Input: 234234234234278 (15 cifara)
Cilj: Odaberi 12 cifara

Analiza:
- Treba preskočiti 3 cifre
- Greedy će preskočiti najlošije cifre s početka

Izvođenje:

K=1: Pretraži [2,3,4,2]
     MAX=4, RESULT="4", LAST_POS=3

K=2: Pretraži [2,3,4,2]
     MAX=4, RESULT="43", LAST_POS=6

K=3: Pretraži [2,3,4,2]
     MAX=4, RESULT="434", LAST_POS=9

... (nastavljaPattern) ...

Rezultat: "434234234278"
Vrijednost: 434,234,234,278
```

### Primjer 4: Early Exit Optimizacija

```
Input: 999999999999999 (15 devetki)
Cilj: Odaberi 12 cifara

Izvođenje:

K=1: Pretraži pozicije 1-4
     J=1: CURR_VAL=9
          MAX_VAL=9
          IF MAX_VAL = 9 THEN LEAVE SC4N  ← Early exit!
     RESULT="9", LAST_POS=1

K=2-12: Identičan proces, svaki put early exit na prvoj pronađenoj 9

Rezultat: "999999999999"
Vrijednost: 999,999,999,999

Ušteda: ~40% iteracija (exit nakon prvog J umjesto svih)
```

## Kompleksnost

### Vremenska Kompleksnost

**Glavni loop**: O(n)
- n = 200 redaka
- Za svaki red poziva `FIND_BEST_12`

**FIND_BEST_12 analiza**:

**Vanjska petlja**: O(12) = O(1)
- Točno 12 iteracija (konstanta)

**Unutarnja petlja** (SC4N):
- Iteracije: Od `LAST_POS + 1` do `LIMIT`
- Worst case: Cjelokupan niz (m znakova)
- Best case: Early exit na prvom J (O(1))

**Po iteraciji K**:
| K | Max pozicija | Pretraga |
|---|--------------|----------|
| 1 | LIMIT = m-11 | O(m-11) |
| 2 | LIMIT = m-10 | O(m-10-last) |
| ... | ... | ... |
| 12 | LIMIT = m | O(m-last) |

**Worst case** (bez early exit):
```
Σ(k=1 to 12) (m - 12 + k) = 12m - 66
= O(m)
```

**Best case** (sve devetke):
```
12 × O(1) = O(12) = O(1)
```

**Prosječan slučaj**:
```
~O(m/2) prosječnih iteracija po K
12 × m/2 = 6m = O(m)
```

**Cjelokupan program**: O(n × m)
- n = 200 redaka
- m = prosječna duljina reda (~50-100 znakova)
- **Ukupno**: O(200 × 75) ≈ **O(15,000) operacija**

### Prostorna Kompleksnost

**UNOS niz**: O(n × w)
- 200 redaka × 100 znakova = **20,000 bytes** = **~20 KB**

**FIND_BEST_12 varijable**: O(m)
- DIGITS: ~100 bytes (VARYING string)
- RESULT: 12 bytes (VARYING, max 12)
- Ostale varijable: ~50 bytes

**Ukupno**: O(n × w) = **~20 KB**

### Optimizacija: Early Exit

**Bez early exit**:
```pl1
DO J = LAST_POS + 1 TO LIMIT;
    CURR_VAL = BINARY(SUBSTR(DIGITS, J, 1));
    IF CURR_VAL > MAX_VAL THEN DO;
        MAX_VAL = CURR_VAL;
        BEST_POS = J;
        BEST_DIGIT = SUBSTR(DIGITS, J, 1);
    END;
END;
```

**S early exit**:
```pl1
SC4N: DO J = LAST_POS + 1 TO LIMIT;
    CURR_VAL = BINARY(SUBSTR(DIGITS, J, 1));
    IF CURR_VAL > MAX_VAL THEN DO;
        MAX_VAL = CURR_VAL;
        BEST_POS = J;
        BEST_DIGIT = SUBSTR(DIGITS, J, 1);
        
        IF MAX_VAL = 9 THEN LEAVE SC4N;  ← Optimizacija!
    END;
END;
```

**Poboljšanje**:
- Za visoke cifre (8-9): ~80% manje iteracija
- Za prosječne cifre (4-7): ~40% manje iteracija
- Za niske cifre (1-3): Minimalno poboljšanje

## Napomene

### Advent of Code Rješenje

**Part 1 odgovor**: 17,109
- Odabir 2 cifre (nije implementirano u ovom kodu)

**Part 2 odgovor**: 169,347,417,057,382
- Odabir 12 cifara (implementirano)
- Format: `FIXED DECIMAL(18,0)` dovoljno velik za ovaj rezultat

### Legacy Varijable

**Nekorištene deklaracije**:
```pl1
DCL (DIGIT1, DIGIT2) CHAR(1) INIT('');
DCL CONCATENATED CHAR(2) INIT('');
```

Ove varijable su vjerojatno ostale od Part 1 implementacije koja je radila s 2 cifre.

### Format Konverzije

**String → Decimal**:
```pl1
DECIMAL(DOBIVENI_BROJ_STR, 15, 0)
```
- `15`: Precision (maksimalno 15 cifara)
- `0`: Scale (bez decimalnih mjesta)
- **FIXED DECIMAL(18,0)**: Može držati do 18 cifara

**Primjer**:
```
"987654321111" → 987,654,321,111 (DECIMAL)
```

### Karakteristike VARYING Stringova

**Deklaracija**:
```pl1
DCL DIGITS CHAR(100) VARYING;
DCL RESULT CHAR(12) VARYING;
```

**Prednosti VARYING**:
- Automatsko praćenje duljine
- `LENGTH(DIGITS)` vraća stvarnu duljinu, ne maksimalnu
- Efikasnija konkatenacija: `RESULT = RESULT || BEST_DIGIT`
- Automatski resize pri dodavanju znakova

**Primjer**:
```pl1
RESULT = '';           // LENGTH(RESULT) = 0
RESULT = RESULT || '9'; // LENGTH(RESULT) = 1
RESULT = RESULT || '8'; // LENGTH(RESULT) = 2
```

### PL/I Specifičnosti

**TRIM funkcija**:
```pl1
DIGITS = TRIM(DIGITS_RAW);
```
- Uklanja trailing space-ove
- Bitno jer `CHAR(100)` uvijek ima 100 znakova

**SUBSTR za konverziju**:
```pl1
CURR_VAL = BINARY(SUBSTR(DIGITS, J, 1));
```
- `SUBSTR(DIGITS, J, 1)`: Ekstraktira jedan znak na poziciji J
- `BINARY(...)`: Konvertira '0'-'9' → 0-9

**Named loop labels**:
```pl1
SC4N: DO J = LAST_POS + 1 TO LIMIT;
    ...
    IF MAX_VAL = 9 THEN LEAVE SC4N;
END SC4N;
```
- `SC4N`: Label za petlju (vjerojatno "SCAN" skraćeno)
- `LEAVE SC4N`: Izlazi iz specifične petlje (kao `break` u C)

### Rubni Slučajevi

**Točno 12 cifara u ulazu**:
```
Input: "123456789012"
LEN = 12
K=1: LIMIT=12-11=1, odaberi poziciju 1
K=2: LIMIT=12-10=2, odaberi poziciju 2
...
Rezultat: Sve cifre (nema preskakanja)
```

**Sve iste cifre**:
```
Input: "555555555555555"
Greedy odabire prvih 12 petica
Rezultat: "555555555555"
```

**Cifre u opadajućem redoslijedu**:
```
Input: "987654321000000"
K=1-9: Odabire 9,8,7,6,5,4,3,2,1
K=10-12: Odabire 0,0,0
Rezultat: "987654321000"
```

**Manji broj cifara od potrebnih** (nedefiniran):
```
Input: "12345" (samo 5 cifara)
Program očekuje LEN >= 12
Ako LEN < 12 → LIMIT može biti negativan → greška
```

### Performanse

**Stvarno izvođenje**:
- 200 redaka
- Prosječna duljina: 50-100 znakova
- ~12 × 50 = 600 usporedbi po redu
- **Ukupno**: ~120,000 operacija
- **Vrijeme**: <0.1 sekunde