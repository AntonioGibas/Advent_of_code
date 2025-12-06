# AD04PLI - Analiza i Uklanjanje Rola u Grid Strukturi

## Sadržaj

- [Pregled](#pregled)
- [Deklaracije](#deklaracije)
- [Algoritam](#algoritam)
- [Part 1: Brojanje Dostupnih Rola](#part-1-brojanje-dostupnih-rola)
- [Part 2: Iterativno Uklanjanje](#part-2-iterativno-uklanjanje)
- [Primjer Izvođenja](#primjer-izvođenja)
- [Optimizacije](#optimizacije)
- [Kompleksnost](#kompleksnost)

## Pregled

Program analizira 135×135 grid strukturu sa dva različita cilja:

**Part 1**: Broji sve role (`@`) koje imaju manje od 4 susjedne role u 8 smjerova  
**Part 2**: Iterativno uklanja dostupne role dok god postoje (simulacija pristupa viličarima)

**Ulaz**: Tekstualna datoteka sa 135 redaka, svaki duljine 135 znakova  
**Izlaz**: Broj dostupnih rola (Part 1) i ukupno uklonjenih rola (Part 2)

## Deklaracije

### Varijable

| Varijabla | Tip | Svrha |
|-----------|-----|-------|
| `UNOS(135)` | `CHAR(135)` | Polje za čitanje redaka iz datoteke |
| `GRID(135,135)` | `CHAR(1)` | Originalna 2D matrica (za Part 1) |
| `R_GRID(135,135)` | `CHAR(1)` | Radna kopija matrice (za Part 2) |
| `I, J, X, Y` | `FIXED BINARY(31)` | Iteratori za petlje |
| `DX, DY` | `FIXED BINARY(31)` | Delta koordinate za provjeru susjeda |
| `NX, NY` | `FIXED BINARY(31)` | Privremene koordinate susjeda |
| `DOSP_ROL` | `FIXED BINARY(15)` | Brojač dostupnih rola (Part 1) |
| `UKLONJENE_U_ITR` | `FIXED BINARY(15)` | Uklonjene role u trenutnoj iteraciji |
| `UKLONJENE_TOTAL` | `FIXED BINARY(15)` | Ukupno uklonjene role (Part 2) |
| `ROLA` | `CHAR(1)` | Target znak (`@`) |
| `PRAZAN` | `CHAR(1)` | Znak za uklonjenu rolu (`.`) |
| `BROJAC` | `FIXED BINARY(15)` | Privremeni brojač susjeda |

### Datoteke

- `INFL`: Record input file za čitanje grid podataka

### Procedure

| Procedura | Parametri | Svrha |
|-----------|-----------|-------|
| `BROJI_DOSTUPNE` | `G(135,135)`, `CNT` | Broji sve role sa < 4 susjeda |
| `BROJI_SUSJEDE` | `G(135,135)`, `X`, `Y`, `CNT` | Broji susjede na poziciji (X,Y) |

## Algoritam

### 1. Učitavanje Podataka

```
ZA svaki red I od 1 do 135:
    Pročitaj redak u UNOS(I)
    ZA svaki stupac J od 1 do 135:
        GRID(I,J) = znak na poziciji J
        R_GRID(I,J) = GRID(I,J)  // Kopija za Part 2
```

### 2. Procedura: BROJI_SUSJEDE

Broji susjedne role oko pozicije (X,Y) koristeći delta petlju:

```
CNT = 0
ZA DX od -1 do 1:
    ZA DY od -1 do 1:
        AKO je (DX=0 I DY=0):
            Preskoči (trenutna pozicija)
        
        NX = X + DX
        NY = Y + DY
        
        AKO su (NX,NY) unutar granica [1,135]:
            AKO je G(NX,NY) = '@':
                CNT += 1
```

**Provjera 8 Smjerova**:
```
(-1,-1)  (-1,0)  (-1,+1)
( 0,-1)    @     ( 0,+1)
(+1,-1)  (+1,0)  (+1,+1)
```

### 3. Procedura: BROJI_DOSTUPNE

```
CNT = 0
ZA svaki X od 1 do 135:
    ZA svaki Y od 1 do 135:
        AKO je G(X,Y) = '@':
            BROJI_SUSJEDE(G, X, Y, N)
            AKO je N < 4:
                CNT += 1
```

## Part 1: Brojanje Dostupnih Rola

Analizira originalni grid i broji role dostupne viličarima:

```
BROJI_DOSTUPNE(GRID, DOSP_ROL)
Ispis: "PART 1: " + DOSP_ROL
```

**Definicija Dostupnosti**: Rola je dostupna ako ima **manje od 4** susjednih rola u bilo kojem od 8 smjerova.

## Part 2: Iterativno Uklanjanje

Simulira proces uklanjanja rola viličarima kroz iterativne cikluse:

```
DO WHILE beskonačno:
    
    UKLONJENE_U_ITR = 0
    
    // Prolazak kroz cijeli grid
    ZA svaki X od 1 do 135:
        ZA svaki Y od 1 do 135:
            AKO je R_GRID(X,Y) = '@':
                BROJI_SUSJEDE(R_GRID, X, Y, BROJAC)
                
                AKO je BROJAC < 4:
                    R_GRID(X,Y) = '.'    // Ukloni rolu
                    UKLONJENE_U_ITR += 1
    
    // Provjera izlaznog uvjeta
    AKO je UKLONJENE_U_ITR = 0:
        IZAĐI iz petlje
    
    UKLONJENE_TOTAL += UKLONJENE_U_ITR

Ispis: "PART 2: " + UKLONJENE_TOTAL
```

### Ključne Karakteristike

1. **Jednosmjerno uklanjanje**: Role se ne vraćaju nakon uklanjanja
2. **Kaskadni efekt**: Uklanjanje rola otvara pristup novim rolama
3. **Iterativna konvergencija**: Proces se ponavlja dok postoje dostupne role
4. **Izlazni uvjet**: Petlja staje kada nijedna rola nije uklonjena u iteraciji

## Primjer Izvođenja

### Part 1: Inicijalno Stanje

```
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
```

**Analiza**:
- Rola na (1,3): 1 susjed → **dostupna**
- Rola na (2,1): 2 susjeda → **dostupna**
- Rola na (3,3): 5 susjeda → **nije dostupna**

**Rezultat**: `PART 1: 13`

### Part 2: Iterativno Uklanjanje

**Iteracija 1** - Ukloni 13 rola:
```
..xx.xx@x.
x@@.@.@.@@
@@@@@.x.@@
```

**Iteracija 2** - Ukloni 12 rola:
```
.......x..
.@@.x.x.@x
x@@@@...@@
```

**Iteracija 3-8** - Nastavi dok se sve moguće role ne uklone

**Rezultat**: `PART 2: 43`

## Optimizacije

### Ekstrakcija Procedura

**Prije**:
```pl1
// Duplirani kod za brojanje susjeda (70+ linija)
DO X = 1 TO 135;
    DO Y = 1 TO 135;
        IF GRID(X,Y) = ROLA THEN DO;
            BROJAC = 0;
            DO DX = -1 TO 1;
                DO DY = -1 TO 1;
                    // ... ista logika ponovljena 2x
```

**Poslije**:
```pl1
// Jedna procedura, pozivana gdje treba (~15 linija)
CALL BROJI_SUSJEDE(GRID, X, Y, BROJAC);
```

### Delta Pristup vs Eksplicitni Uvjeti

**Stara verzija** (nepotrebno kompleksna):
```pl1
IF (DX <> 0 | DY <> 0) & 
   (X+DX >= 1 & X+DX <= 135) & 
   (Y+DY >= 1 & Y+DY <= 135) THEN DO;
```

**Nova verzija** (čitljivija):
```pl1
IF DX = 0 & DY = 0 THEN ITERATE;
NX = X + DX;
NY = Y + DY;
IF NX >= 1 & NX <= 135 & NY >= 1 & NY <= 135 THEN
```

### Prednosti Optimiziranog Koda

1. **DRY princip**: Eliminira duplikaciju logike
2. **Modularnost**: Jasna separacija odgovornosti
3. **Testabilnost**: Procedure se mogu testirati neovisno
4. **Čitljivost**: Glavni tok algoritma jasan na prvi pogled
5. **Maintainability**: Izmjene u logici brojanja na jednom mjestu

## Kompleksnost

### Part 1 Analiza

- **Vremenska**: O(n² × k) gdje je n=135, k=8 (susjedi)
  - Prolazak kroz grid: O(18,225)
  - Provjera susjeda po roli: O(8)
  - **Ukupno**: ~145,800 operacija
- **Prostorna**: O(n²) = 18,225 znakova

### Part 2 Analiza

- **Vremenska**: O(i × n² × k) gdje je i=broj iteracija
  - Prosječno: 5-15 iteracija
  - Po iteraciji: O(145,800)
  - **Worst case**: ~2,187,000 operacija
- **Prostorna**: O(n²) = 18,225 znakova (dodatna kopija grida)

### Optimizacije Performansi

**Moguće ali neuključene**:
1. Praćenje samo rubnih pozicija umjesto cijelog grida
2. Ažuriranje lokalnih susjeda umjesto ponovnog skeniranja
3. Kompresija praznih područja

**Razlog**: Kod je dovoljno brz za n=135, dodatna kompleksnost nije opravdana.

## Napomene

- Grid indeksiranje kreće od 1 (PL/I konvencija)
- Rubne pozicije imaju manje mogućih susjeda (3 u kutovima, 5 na rubovima)
- Part 1 ne mijenja GRID, Part 2 radi na R_GRID kopiji
- Ulazna datoteka mora biti točno 135×135 znakova
- Program ne koristi rekurziju ili dinamičku alokaciju