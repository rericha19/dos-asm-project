

;PROGRAM ECHUJE NA OBRAZOVKU STLACENY KLAVES
;NEPRACUJE S DATAMI,PRETO NEMA DEKLAROVANY DATOVY SEGMENT



name echo          ;pseudoinstrukcia na pomenovanie tohoto programu-modulu
                   ;nie je to meno suboru,v ktorom je program ulozeny

 .model small       ; deklaracia maleho modulu

 .stack 100h        ; deklaracia zasobnikoveho segmentu velkosti 256B

 .code              ; zaciatok segmentu instrukcii

start:             ; navestie prvej instrukcie
      mov ah,1   ;ziadost o vstup znaku z klavesnice sluzbou cislo 1
      int 21h    ;prerusenie z klavesnice pre vstup znaku
      cmp al,13  ;bol stlaceny ENTER?
      jz koniec  ;ak ano, skonci program - vrat sa do MSDOS
      mov dl,al  ;uloz nacitany znak do registra dl
      mov ah,2   ;ziadost o vystup znaku na obrazovku sluzbou cislo 2
      int 21h    ;uskutocnenie vystupu na obrazovku
      jmp start  ;citaj dalsi znak z klavesnice
koniec:
      mov ah,4ch ;funkcia na ukoncenie programu a korektny
      int 21h    ;                        navrat do MS-DOS
      end start  ;program bude spusteny od navestia start



; ULOHY:


;1. Priradte ciselnemu kodu klavesy ENTER symbolicke meno ENT
;        (pozrite NG,pseudoinstrukcie,EQU)
;2. Definujte v datovom segmente premennu ENTR s obsahom 13
;     (DATA SEGMENT, DB, plnenie registra DS - pozrite priklady
;        v J:\TASM )
;3. Vypiste iba raz na obrazovku stlaceny klaves t.j. bez echa
;    ( napr. miesto sluzby 1 pri INT 21H pouzite inu vhodnu sluzbu -
;       pozrite J:\thelp resp. abshelp)
;4. Prepiste program z bodkoveho zapisu do zakladneho jednoducheho zapisu