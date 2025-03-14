# Dokumentacija za program ADV6PLI

## Opis programa
Program simulira kretanje objekta kroz 2D mrežu (130x130) te analizira posjećene pozicije i moguće prepreke koje stvaraju petlje. 
Sastoji se od dva glavna dijela:
1. Simulacija kretanja od početne pozicije do izlaska iz mreže
2. Identifikacija pozicija gdje postavljanje prepreke stvara beskonačne petlje

---

## Deklaracije varijabli

### Datoteke
- **INFL**: Ulazna datoteka s početnom konfiguracijom mreže
- **OUTFL**: Izlazna datoteka za rezultate

### Kontrolne oznake
- **EOF**: Zastavica za kraj ulazne datoteke
- **EOG**: Zastavica za kraj simulacije

### Koordinate i smjerovi
- **X, Y**: Trenutna pozicija
- **NEW_X, NEW_Y**: Nova pozicija
- **DIRECTION**: Trenutni smjer (0-gore, 1-desno, 2-dolje, 3-lijevo)

### Brojači i mreže
- **BR_POZ**: Broj posjećenih pozicija
- **VISITED**: Matrica posjećenih pozicija (130x130)
- **LOOP_POS**: Matrica pozicija koje stvaraju petlje (130x130)
- **OBSTACLE_COUNT**: Broj validnih prepreka koje stvaraju petlje

### Ulazni/izlazni podaci
- **UNOS**: Ulazni stringovi s početnom mrežom
- **GRID**: Trenutno stanje mreže
- **ORIG_GRID**: Originalna mreža
- **PATH_GRID**: Mreža za prikaz putanje

---

## Glavni tok programa

### 1. Inicijalizacija
- Učitavanje ulazne datoteke u **UNOS** niz
- Popunjavanje **GRID**, **ORIG_GRID** i **PATH_GRID** matrica
- Pronalaženje početne pozicije (^, >, v, <) i postavljanje inicijalnog smjera

### 2. Simulacija kretanja (PLAY loop)
- Kalkulacija nove pozicije na osnovu trenutnog smjera
- Provjera granica mreže:
  ```pl/i
  IF (NEW_X < 1 | NEW_X > 130 | NEW_Y < 1 | NEW_Y > 130)
  ```
- Upravljanje preprekama (`#`):
  - Okretanje desno (`DIRECTION = MOD(DIRECTION + 1, 4)`)
- Ažuriranje pozicije i označavanje posjećenih polja

### 3. Detekcija petlji (FIND_LOOP_POS)
- Iteracija kroz sva polja mreže
- Za svako prazno polje (`.`) osim početnog:
  - Stvara privremenu mrežu **TEMP_GRID** s preprekom
  - Simulira kretanje od početne pozicije
  - Prati posjećene smjerove (**VISITED_SIM**)
  - Detektira petlje ako se isti smjer ponovi na istoj poziciji
  - Označava polje u **LOOP_POS** ako stvara petlju

---

## Ključne logičke strukture

### Upravljanje smjerovima
```pl/i
SELECT(DIRECTION)
  WHEN(0) NEW_X = X - 1; /* Gore */
  WHEN(1) NEW_Y = Y + 1; /* Desno */
  WHEN(2) NEW_X = X + 1; /* Dolje */
  WHEN(3) NEW_Y = Y - 1; /* Lijevo */
```

### Algoritam za izlazak iz petlje
```pl/i
IF VISITED_SIM(SIM_X, SIM_Y, SIM_DIR + 1) = '1'b THEN DO
  IN_LOOP = '1'b;
  LOOP_POS(I,J) = '1'b;
  OBSTACLE_COUNT += 1;
END;
```

---

## Izlazni podaci
- `Broj posjećenih pozicija`: Ukupno jedinstvenih polja posjećenih tokom simulacije
- `Broj mogućih pozicija za prepreku`: Polja gdje postavljanje prepreke stvara petlju

---

## Ograničenja
- Fiksna veličina mreže (130x130)
- Simulacija zaustavlja nakon prve detektirane petlje
- Samo prazna polja (`.`) se razmatraju za potencijalne prepreke

---

## Sigurnosne provjere
- Granične vrijednosti za koordinate
- Zaštita od izlaska iz mreže
- Sprečavanje modifikacije početne pozicije