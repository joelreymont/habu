\ ptx-ir.f - value-numbered PTX expression IR plus local optimizer rules.
\
\ This is the static optimizer seed: construction canonicalizes commutative
\ binary ops, folds constants, applies small peepholes, and reuses equivalent
\ nodes by value number. DCE is a live-mark pass over chosen roots.

0 constant PTXIR-K-INPUT
1 constant PTXIR-K-CONST
2 constant PTXIR-K-ADD
3 constant PTXIR-K-MUL
4 constant PTXIR-K-NEG
-1 constant PTXIR-NONE

64 constant PTXIR-MAX

BEGIN-STRUCTURE PTXIR-REC
   CELL +FIELD PTXIR.OP
   CELL +FIELD PTXIR.A
   CELL +FIELD PTXIR.B
   CELL +FIELD PTXIR.VAL
   CELL +FIELD PTXIR.LIVE
END-STRUCTURE

create PTXIR-NODES PTXIR-MAX PTXIR-REC * allot
variable PTXIR-N

: PTXIR-RESET ( -- )
   0 PTXIR-N ! ;

: PTXIR-COUNT ( -- n )
   PTXIR-N @ ;

: PTXIR-CAP-CHECK ( n -- )
   dup 0 < if E-PTX-IR-UNKNOWN throw then
   dup PTXIR-MAX >= if E-PTX-IR-OVERFLOW throw then
   drop ;

: PTXIR-ID-CHECK ( n -- )
   dup PTXIR-CAP-CHECK
   dup PTXIR-N @ >= if E-PTX-IR-UNKNOWN throw then
   drop ;

: PTXIR-REC@ ( n -- ptr a ) {: id:n :}
   id PTXIR-CAP-CHECK
   PTXIR-NODES id PTXIR-REC * + ;

: PTXIR-OP@ ( n -- n ) {: id:n :}
   id PTXIR-ID-CHECK
   id PTXIR-REC@ PTXIR.OP @ ;

: PTXIR-A@ ( n -- n ) {: id:n :}
   id PTXIR-ID-CHECK
   id PTXIR-REC@ PTXIR.A @ ;

: PTXIR-B@ ( n -- n ) {: id:n :}
   id PTXIR-ID-CHECK
   id PTXIR-REC@ PTXIR.B @ ;

: PTXIR-VAL@ ( n -- n ) {: id:n :}
   id PTXIR-ID-CHECK
   id PTXIR-REC@ PTXIR.VAL @ ;

: PTXIR-LIVE@ ( n -- bool ) {: id:n :}
   id PTXIR-ID-CHECK
   id PTXIR-REC@ PTXIR.LIVE @ ;

: PTXIR-LIVE! ( bool n -- ) {: live:bool id:n :}
   id PTXIR-ID-CHECK
   live id PTXIR-REC@ PTXIR.LIVE ! ;

: PTXIR-WRITE ( n n n n n -- ) {: op:n a:n b:n val:n id:n :}
   id PTXIR-REC@ {: rec:ptr :}
   op rec PTXIR.OP !
   a rec PTXIR.A !
   b rec PTXIR.B !
   val rec PTXIR.VAL !
   0 rec PTXIR.LIVE ! ;

: PTXIR-MATCH? ( n n n n n -- bool ) {: op:n a:n b:n val:n id:n :}
   id PTXIR-OP@ op <> if 0 0= 0= exit then
   id PTXIR-A@ a <> if 0 0= 0= exit then
   id PTXIR-B@ b <> if 0 0= 0= exit then
   id PTXIR-VAL@ val <> if 0 0= 0= exit then
   0 0= ;

: PTXIR-FIND ( n n n n -- n bool ) {: op:n a:n b:n val:n :}
   PTXIR-N @ 0 ?do
      op a b val i PTXIR-MATCH? if i 0 0= exit then
   loop
   PTXIR-NONE 0 0= 0= ;

: PTXIR-ROOM ( -- )
   PTXIR-N @ PTXIR-MAX >= if E-PTX-IR-OVERFLOW throw then ;

: PTXIR-INTERN ( n n n n -- n ) {: op:n a:n b:n val:n :}
   op a b val PTXIR-FIND if exit then drop
   PTXIR-ROOM
   PTXIR-N @ {: id:n :}
   op a b val id PTXIR-WRITE
   id 1+ PTXIR-N !
   id ;

: PTXIR-INPUT ( -- n )
   PTXIR-K-INPUT PTXIR-NONE PTXIR-NONE 0 PTXIR-INTERN ;

: PTXIR-CONST ( n -- n ) {: val:n :}
   PTXIR-K-CONST PTXIR-NONE PTXIR-NONE val PTXIR-INTERN ;

: PTXIR-CONST? ( n -- bool )
   PTXIR-OP@ PTXIR-K-CONST = ;

: PTXIR-CONST-VAL ( n -- n ) {: id:n :}
   id PTXIR-CONST? 0= if E-PTX-IR-UNKNOWN throw then
   id PTXIR-VAL@ ;

: PTXIR-CONST= ( n n -- bool ) {: id:n val:n :}
   id PTXIR-CONST? if id PTXIR-VAL@ val = exit then
   0 0= 0= ;

: PTXIR-CANON2 ( n n -- n n ) {: a:n b:n :}
   a b > if b a exit then
   a b ;

: PTXIR-ADD-NODE ( n n -- n )
   PTXIR-CANON2 {: a:n b:n :}
   PTXIR-K-ADD a b 0 PTXIR-INTERN ;

: PTXIR-MUL-NODE ( n n -- n )
   PTXIR-CANON2 {: a:n b:n :}
   PTXIR-K-MUL a b 0 PTXIR-INTERN ;

: PTXIR-ADD ( n n -- n ) {: a:n b:n :}
   a PTXIR-CONST? b PTXIR-CONST? and if
      a PTXIR-CONST-VAL b PTXIR-CONST-VAL + PTXIR-CONST exit
   then
   a 0 PTXIR-CONST= if b exit then
   b 0 PTXIR-CONST= if a exit then
   a b PTXIR-ADD-NODE ;

: PTXIR-MUL ( n n -- n ) {: a:n b:n :}
   a PTXIR-CONST? b PTXIR-CONST? and if
      a PTXIR-CONST-VAL b PTXIR-CONST-VAL * PTXIR-CONST exit
   then
   a 0 PTXIR-CONST= if 0 PTXIR-CONST exit then
   b 0 PTXIR-CONST= if 0 PTXIR-CONST exit then
   a 1 PTXIR-CONST= if b exit then
   b 1 PTXIR-CONST= if a exit then
   a b PTXIR-MUL-NODE ;

: PTXIR-NEG ( n -- n ) {: id:n :}
   id PTXIR-CONST? if 0 id PTXIR-CONST-VAL - PTXIR-CONST exit then
   id PTXIR-OP@ PTXIR-K-NEG = if id PTXIR-A@ exit then
   PTXIR-K-NEG id PTXIR-NONE 0 PTXIR-INTERN ;

: PTXIR-LIVE-CLEAR ( -- )
   PTXIR-N @ 0 ?do 0 0= 0= i PTXIR-LIVE! loop ;

: PTXIR-MARK ( n -- ) {: id:n :}
   id PTXIR-LIVE@ if exit then
   0 0= id PTXIR-LIVE!
   id PTXIR-OP@ PTXIR-K-ADD = if id PTXIR-A@ recurse id PTXIR-B@ recurse exit then
   id PTXIR-OP@ PTXIR-K-MUL = if id PTXIR-A@ recurse id PTXIR-B@ recurse exit then
   id PTXIR-OP@ PTXIR-K-NEG = if id PTXIR-A@ recurse exit then ;

: PTXIR-LIVE-COUNT ( n -- n ) {: root:n :}
   PTXIR-LIVE-CLEAR
   root PTXIR-MARK
   0 PTXIR-N @ 0 ?do
      i PTXIR-LIVE@ if 1+ then
   loop ;
