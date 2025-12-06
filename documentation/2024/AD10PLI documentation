# Detaljna dokumentacija PL/I Hiking Trail programa

## PREGLED PROGRAMA
Program rješava Advent of Code Day 10 problem pronalaska hiking trail-ova na topografskoj mapi. Cilj je pronaći sve "trailhead" pozicije (visina 0) i za svaku izračunati koliko različitih pozicija s visinom 9 može dosegnuti putem validnih putanja.

---

## DEKLARACIJE VARIJABLI

### **Datotečne varijable**
```pli
DCL INFL FILE RECORD INPUT;
```
- **INFL**: Datotečna varijabla za čitanje input datoteke
- **FILE**: Označava da je ovo datotečna varijabla
- **RECORD**: Datoteka se čita red po red (record by record)
- **INPUT**: Datoteka je otvorena samo za čitanje

### **Nizovi za čuvanje podataka**
```pli
DCL UNOS(57) CHAR(57) INIT((57)(''));
```
- **UNOS**: Niz od 57 elemenata, svaki element je string od 57 karaktera
- **CHAR(57)**: Svaki element može držati do 57 karaktera
- **INIT((57)(''))**: Inicijalizira svih 57 elemenata s praznim stringom
- Koristi se za privremeno čuvanje redova učitanih iz datoteke

```pli
DCL MAP(57,57) CHAR(1);
```
- **MAP**: Dvodimenzionalni niz 57x57 elemenata
- **CHAR(1)**: Svaki element drži točno jedan karakter
- Predstavlja topografsku mapu gdje svaki element je visina (0-9)

### **Radne varijable**
```pli
DCL (I,J) FIXED BINARY(31) INIT(0);
DCL (X,Y) FIXED BINARY(31) INIT(0);
```
- **I, J**: Brojači za petlje, 32-bitni cijeli brojevi
- **X, Y**: Koordinate na mapi (redak, stupac)
- **FIXED BINARY(31)**: 32-bitni cijeli broj s predznakom
- **INIT(0)**: Inicijalizirane na nulu

```pli
DCL TARGET(10) CHAR(1) STATIC INIT('0','1','2','3','4','5','6','7','8','9');
```
- **TARGET**: Niz od 10 karaktera koji sadrži cifre 0-9
- **STATIC**: Varijabla se inicijalizira samo jednom i zadržava vrijednost
- **Napomena**: Ova varijabla se ne koristi u programu - vjerojatno ostatak od ranijih verzija

```pli
DCL PATH_CNTR FIXED BINARY(31) INIT(0);
DCL AVG_NINES FIXED BINARY(31);
DCL TOTAL_SCORE FIXED BINARY(31) INIT(0);
```
- **PATH_CNTR**: Brojač putanja (ne koristi se u finalnoj verziji)
- **AVG_NINES**: Varijabla za prosječan broj devetki (ne koristi se)
- **TOTAL_SCORE**: Ukupni rezultat - zbroj svih trailhead score-ova

### **Struktura za čuvanje koordinata**
```pli
DCL 1 FOUND_NINES(500),
    2 X FIXED BINARY(31),
    2 Y FIXED BINARY(31);
```
- **FOUND_NINES**: Niz od 500 struktura
- **1** označava glavnu strukturu
- **2** označava pod-elemente strukture
- Svaka struktura sadrži X i Y koordinate
- Koristi se za čuvanje koordinata svih pronađenih pozicija s visinom 9

```pli
DCL NINES_COUNT FIXED BINARY(31) INIT(0);
```
- **NINES_COUNT**: Brojač koliko je trenutno pronađeno devetki za određeni trailhead
- Resetira se na 0 prije svakog novog trailhead-a

### **Kontrolne varijable**
```pli
DCL EOF BIT(1) INIT('0'b);
```
- **EOF**: End-of-File flag, jedan bit
- **BIT(1)**: Tip podatka koji može biti '0'b ili '1'b
- **INIT('0'b)**: Inicijaliziran na "false" (nema kraja datoteke)

---

## UGRADENE FUNKCIJE I NAREDBE

### **Datotečne operacije**
```pli
ON ENDFILE(INFL) EOF = '1'b;
```
- **ON ENDFILE**: Exception handler koji se aktivira kad se dosegne kraj datoteke
- Postavlja EOF flag na '1'b (true) kad nema više podataka za čitanje

```pli
OPEN FILE(INFL);
```
- **OPEN**: Otvara datoteku za pristup
- Priprema datoteku za čitanje

```pli
READ FILE(INFL) INTO(UNOS(I));
```
- **READ**: Čita jedan record (red) iz datoteke
- **INTO**: Sprema pročitani podatak u specificiranu varijablu
- Čita red po red i sprema u UNOS niz

```pli
CLOSE FILE(INFL);
```
- **CLOSE**: Zatvara datoteku i oslobađa resurse

### **String operacije**
```pli
MAP(I,J) = SUBSTR(UNOS(I),J,1);
```
- **SUBSTR**: Ekstraktira dio stringa
- **SUBSTR(string, start, length)**:
  - string: UNOS(I) - izvorni string
  - start: J - početna pozicija (1-based indeks)
  - length: 1 - broj karaktera za ekstraktiranje
- Uzima jedan karakter iz pozicije J u stringu UNOS(I)

### **Konverzijske funkcije**
```pli
CURRENT_HEIGHT = RANK(MAPA(X,Y)) - RANK('0');
```
- **RANK**: Vraća ASCII vrijednost karaktera
- **RANK('0')** = 48 (ASCII kod za '0')
- **RANK('1')** = 49 (ASCII kod za '1')
- Oduzimanjem RANK('0') konvertiramo karakter cifru u broj:
  - '0' → 48 - 48 = 0
  - '1' → 49 - 48 = 1
  - '9' → 57 - 48 = 9

### **Kontrolne naredbe**
```pli
REINIT I;
```
- **REINIT**: Postavlja varijablu na početnu vrijednost (INIT vrijednost)
- Ekvivalentno s `I = 0;`

```pli
RETURN;
```
- **RETURN**: Izlazi iz procedure i vraća kontrolu pozivaocu
- U rekurzivnoj funkciji završava trenutnu instancu

```pli
LEAVE;
```
- **LEAVE**: Izlazi iz trenutne petlje
- Ekvivalentno s `break` u C/Java

### **Ispis podataka**
```pli
PUT SKIP LIST('tekst' || varijabla);
```
- **PUT**: Ispisuje podatke
- **SKIP**: Dodaje novi red prije ispisa
- **LIST**: Formatira podatke kao listu
- **||**: Operator konkatenacije stringova

```pli
TRIM(CHAR(X))
```
- **CHAR**: Konvertira broj u string
- **TRIM**: Uklanja leading i trailing razmake

---

## LOGIKA PROGRAMA - KORAK PO KORAK

### **1. Inicijalizacija i čitanje datoteke**
```pli
PPLATE_UNOS:DO I = 1 TO 57;
 READ FILE(INFL) INTO(UNOS(I));
END PPLATE_UNOS;
```
**Logika:**
- Čita 57 redova iz input datoteke
- Svaki red sprema u UNOS[I] kao string od max 57 karaktera
- Nakon ovog koraka, UNOS niz sadrži sve redove mape

### **2. Pretvaranje stringova u 2D mapu**
```pli
PPLATE_MAP_X:DO I = 1 TO 57;
 PPLATE_MAP_Y:DO J = 1 TO 57;
  MAP(I,J) = SUBSTR(UNOS(I),J,1);
 END PPLATE_MAP_Y;
END PPLATE_MAP_X;
```
**Logika:**
- Dvostru petlja prolazi kroz sve pozicije (I,J)
- Za svaku poziciju uzima J-ti karakter iz I-tog reda
- Rezultat: MAP[I][J] sadrži jedan karakter (cifru 0-9)
- MAP sada predstavlja topografsku mapu u 2D formatu

### **3. Pronalaženje i procesiranje trailheads**
```pli
FIND_TRAILHEADS:DO X = 1 TO 57;
  DO Y = 1 TO 57;
    IF MAP(X,Y) = '0' THEN DO;
      NINES_COUNT = 0;
      CALL FIND_PATH(MAP, X, Y, 0);
      TOTAL_SCORE = TOTAL_SCORE + NINES_COUNT;
    END;
  END;
END FIND_TRAILHEADS;
```
**Logika:**
- Prolazi kroz sve pozicije na mapi
- Kada pronađe poziciju s visinom '0' (trailhead):
  1. Resetira brojač devetki na 0
  2. Poziva rekurzivnu funkciju FIND_PATH
  3. Dodaje broj pronađenih devetki u ukupni rezultat

---

## REKURZIVNA FUNKCIJA FIND_PATH

### **Parametri funkcije**
```pli
FIND_PATH:PROCEDURE(MAPA,X,Y,Z) RECURSIVE;
```
- **MAPA**: 2D niz mape (prenosi se po referenci)
- **X, Y**: Trenutne koordinate
- **Z**: Očekivana visina na toj poziciji
- **RECURSIVE**: Označava da funkcija može pozivati samu sebe

### **Provjera granica**
```pli
IF (X < 1) | (X > 57) | (Y < 1) | (Y > 57) THEN RETURN;
```
**Logika:**
- Provjerava da li su koordinate unutar validnog raspona
- PL/I nizovi obično počinju od 1, ne od 0
- Ako su koordinate van granica, prekida izvršavanje

### **Konverzija i provjera visine**
```pli
CURRENT_HEIGHT = RANK(MAPA(X,Y)) - RANK('0');
IF (CURRENT_HEIGHT <> Z) THEN RETURN;
```
**Logika:**
- Konvertira karakter u broj (npr. '5' → 5)
- Provjerava da li je stvarna visina jednaka očekivanoj
- Ako nije, putanja nije validna - prekida izvršavanje

### **Rukovanje krajnjom pozicijom (visina 9)**
```pli
IF Z = 9 THEN DO; 
   ALREADY_EXISTS = '0'B;
   DO I = 1 TO NINES_COUNT;
      IF FOUND_NINES(I).X = X & FOUND_NINES(I).Y = Y THEN DO;
         ALREADY_EXISTS = '1'B;
         LEAVE;
      END;
   END;
   
   IF ^ALREADY_EXISTS THEN DO;
      NINES_COUNT = NINES_COUNT + 1;
      FOUND_NINES(NINES_COUNT).X = X;
      FOUND_NINES(NINES_COUNT).Y = Y;
   END;
   RETURN;
END;
```
**Logika:**
- Ako smo došli do visine 9 (vrh), provjerava da li smo već registrirali tu poziciju
- Prolazi kroz sve dosad pronađene devetke
- Ako pozicija (X,Y) već postoji, ne dodaje ju ponovno
- Ako je nova, dodaje ju u FOUND_NINES niz i uvećava brojač
- **^** je operator negacije (NOT)

### **Rekurzivni pozivi**
```pli
CALL FIND_PATH(MAPA, X-1, Y, Z+1);  /* Gore */
CALL FIND_PATH(MAPA, X+1, Y, Z+1);  /* Dolje */
CALL FIND_PATH(MAPA, X, Y-1, Z+1);  /* Lijevo */
CALL FIND_PATH(MAPA, X, Y+1, Z+1);  /* Desno */
```
**Logika:**
- Poziva sebe za sve četiri susjedne pozicije
- Svaki poziv povećava očekivanu visinu za 1 (Z+1)
- Implementira "dubinsku pretragu" (Depth-First Search)
- Svaki put kad funkcija "sebe pozove", stvara se novi "stack frame"

---

## ALGORITAMSKA SLOŽENOST

### **Vremenska složenost**
- **O(4^n)** u najgorem slučaju, gdje je n duljina najdže putanje (9)
- U praksi mnogo bolja zbog ograničenja mape i invalid putanja

### **Prostorna složenost**
- **O(n)** za rekurzivni stack, gdje je n dubina rekurzije (max 9)
- **O(k)** za čuvanje koordinata devetki, gdje je k broj različitih devetki

---

## POSEBNE PL/I KARAKTERISTIKE

### **Indeksiranje nizova**
- PL/I nizovi obično počinju od 1, ne od 0
- `MAP(1,1)` je prva pozicija, `MAP(57,57)` je zadnja

### **Tipovi podataka**
- **CHAR(n)**: String fiksne duljine
- **FIXED BINARY(31)**: 32-bitni cijeli broj
- **BIT(1)**: Boolean vrijednost

### **Strukture**
- Numeriranje razina (1, 2, 3...) definira hijerarhiju
- `FOUND_NINES(I).X` pristupa X koordinati I-te strukture

### **Operatori**
- **^**: Logička negacija (NOT)
- **&**: Logički AND
- **|**: Logički OR
- **<>**: Nije jednako (NOT EQUAL)
- **||**: Konkatenacija stringova

---

## ZAKLJUČAK

Program koristi klasičnu rekurzivnu "dubinsku pretragu" za pronalaženje svih validnih putanja od visine 0 do visine 9. Ključne komponente su:

1. **Čitanje i parsiranje** topografske mape
2. **Identifikacija trailheads** (pozicije s visinom 0)
3. **Rekurzivna pretraga** validnih putanja
4. **Brojanje jedinstvenih odredišta** (pozicije s visinom 9)
5. **Agregacija rezultata** svih trailheads

Algoritam je efikasan za ovaj tip problema jer prirodno "podrezuje" nevalidne putanje i koristi jednostavnu strukturu podataka za čuvanje rezultata.