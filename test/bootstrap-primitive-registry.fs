\ Host registry growth and emitted seed-record contract. The real
\ bootstrap-wide-memory fixture runs these controls before building its image.
require ../bootstrap/cg/forth.fs

257 constant BPR-TOTAL
create BPR-NAME DNAME-INL allot

: BPR= ( got want -- ) <> abort" bootstrap primitive registry mismatch" ;
: BPR-LETTER ( row at -- c ) + 26 mod 65 + ;
: BPR-APPEND ( row -- ) {: row :}
   DNAME-INL 0 ?do row i BPR-LETTER BPR-NAME i + c! loop
   BPR-NAME DNAME-INL row 1000 + row 1002 + REG-PRIM
   row 32 mod row PRIM-ROW 4 cells + ! ;

: BPR-CHECK ( row -- ) {: idx :}
   idx PRIM-ROW {: row :}
   row @ idx 1000 + BPR=  row cell+ @ idx 1002 + BPR=
   row 4 cells + @ idx 32 mod BPR=
   row PRIM-NAME$ {: name size :}
   size DNAME-INL BPR=
   size 0 ?do name i + c@ idx i BPR-LETTER BPR= loop ;

: BPR-ADD ( -- ) BPR-NAME DNAME-INL 1 2 REG-PRIM ;
: BPR-EMPTY ( -- ) BPR-NAME 0 1 2 REG-PRIM ;
: BPR-NEGATIVE ( -- ) BPR-NAME -1 1 2 REG-PRIM ;
: BPR-LONG ( -- ) s" primitive-name-17" 1 2 REG-PRIM ;

: BPR-REFUSALS ( -- )
   ['] BPR-EMPTY catch E-PRIM-SIZE BPR=
   ['] BPR-NEGATIVE catch E-PRIM-SIZE BPR=
   ['] BPR-LONG catch -2 BPR=
   #PL @ {: used :} PNP @ {: names :}
   PRIM-SIZE-MAX /PRIM / #PL !
   ['] BPR-ADD catch E-PRIM-SIZE BPR=
   #PL @ PRIM-SIZE-MAX /PRIM / BPR=  PNP @ names BPR=
   used #PL !
   PRIM-SIZE-MAX PNP !
   ['] BPR-ADD catch E-PRIM-SIZE BPR=
   #PL @ used BPR= PNP @ PRIM-SIZE-MAX BPR=
   names PNP !
   #PL @ BPR-TOTAL BPR= 0 BPR-CHECK BPR-TOTAL 1- BPR-CHECK ;

: BPR-DICT-BYTES ( -- n ) 0 #IC @ 0 ?do i IC-LEN 4 * + loop ;

: BPR-SHORT ( -- )
   \ A fresh one-byte name pool is smaller than the inline slot padding. The
   \ writer must use its own zero padding, never read beyond live name storage.
   EMIT-RESET-BUILDER s" p" 2 3 REG-PRIM
   0 LNCOUNT ! 1 LDICT ! EMIT-DICT
   6 IC-B 1 BPR= 6 IC-A c@ [char] p BPR=
   7 IC-A PRIM-NAME-PAD BPR= 7 IC-B 12 BPR=
   DNAME-INL 0 ?do PRIM-NAME-PAD i + c@ 0 BPR= loop
   BPR-DICT-BYTES DREC 8 + BPR= ;

: BPR-EMITTED ( -- )
   ICODE-RESET 0 LNCOUNT ! 1 LDICT ! EMIT-DICT
   1 IC-OP IOP-DCQ BPR= 1 IC-A BPR-TOTAL BPR=
   BPR-TOTAL 0 ?do
      i 5 * 3 + {: at :}
      at IC-OP IOP-DLBL BPR= at IC-A i 1000 + BPR=
      at 1+ IC-OP IOP-DLBL BPR= at 1+ IC-A i 1002 + BPR=
      at 2 + IC-A DNAME-INL BPR=
      at 3 + IC-OP IOP-BYTES BPR= at 3 + IC-B DNAME-INL BPR=
      at 3 + IC-A i PRIM-ROW PRIM-NAME$ drop BPR=
      at 4 + IC-A i 32 mod BPR=
   loop
   BPR-DICT-BYTES BPR-TOTAL DREC * 8 + BPR= ;

: BPR-RUN ( -- )
   BPR-SHORT
   EMIT-RESET-BUILDER
   BPR-TOTAL 0 ?do i BPR-APPEND loop
   #PL @ BPR-TOTAL BPR= PNP @ BPR-TOTAL DNAME-INL * BPR=
   BPR-TOTAL 0 ?do i BPR-CHECK loop
   BPR-REFUSALS BPR-EMITTED
   PRIM-ROWS-CAP @ {: rows :} PRIM-NAME-CAP @ {: names :}
   EMIT-RESET-BUILDER #PL @ 0 BPR= PNP @ 0 BPR=
   0 BPR-APPEND 0 BPR-CHECK #PL @ 1 BPR=
   PRIM-ROWS-CAP @ rows BPR= PRIM-NAME-CAP @ names BPR=
   EMIT-RESET-BUILDER ;

BPR-RUN
