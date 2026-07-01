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
5 constant PTXIR-K-BSUM
6 constant PTXIR-K-BSUB
-1 constant PTXIR-NONE

64 constant PTXIR-MAX

VALUE-RECORD ptxir-node op n a n b n val n live n END-VALUE-RECORD

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

: >PTXIR-NODE ( n n n n n -- ptxir-node ) ;

: PTXIR-NODE> ( ptxir-node -- n n n n n ) ;

: PTXIR-NODE-DROP ( ptxir-node -- )
   drop drop drop drop drop ;

: PTXIR-NODE-DUP-RAW ( n n n n n -- ptxir-node ptxir-node )
   {: op:n a:n b:n val:n live:n :}
   op a b val live >PTXIR-NODE
   op a b val live >PTXIR-NODE ;

: PTXIR-NODE-DUP ( ptxir-node -- ptxir-node ptxir-node )
   PTXIR-NODE> PTXIR-NODE-DUP-RAW ;

: PTXIR-WRITE-RAW ( n n n n n n -- ) {: op:n a:n b:n val:n live:n id:n :}
   id PTXIR-REC@ {: rec:ptr :}
   op rec PTXIR.OP !
   a rec PTXIR.A !
   b rec PTXIR.B !
   val rec PTXIR.VAL !
   live rec PTXIR.LIVE ! ;

: PTXIR-WRITE ( ptxir-node n -- )
   >r PTXIR-NODE> r> PTXIR-WRITE-RAW ;

: PTXIR-MATCH-RAW? ( n n n n n n -- bool ) {: op:n a:n b:n val:n live:n id:n :}
   id PTXIR-OP@ op <> if 0 0= 0= exit then
   id PTXIR-A@ a <> if 0 0= 0= exit then
   id PTXIR-B@ b <> if 0 0= 0= exit then
   id PTXIR-VAL@ val <> if 0 0= 0= exit then
   id PTXIR-LIVE@ if 1 else 0 then live <> if 0 0= 0= exit then
   0 0= ;

: PTXIR-MATCH? ( ptxir-node n -- bool )
   >r PTXIR-NODE> r> PTXIR-MATCH-RAW? ;

: PTXIR-FIND-RAW ( n n n n n -- n bool ) {: op:n a:n b:n val:n live:n :}
   PTXIR-N @ 0 ?do
      op a b val live i PTXIR-MATCH-RAW? if i 0 0= exit then
   loop
   PTXIR-NONE 0 0= 0= ;

: PTXIR-FIND ( ptxir-node -- n bool )
   PTXIR-NODE> PTXIR-FIND-RAW ;

: PTXIR-ROOM ( -- )
   PTXIR-N @ PTXIR-MAX >= if E-PTX-IR-OVERFLOW throw then ;

: PTXIR-NODE-INTERN ( ptxir-node -- n )
   PTXIR-NODE-DUP PTXIR-FIND if
      >r PTXIR-NODE-DROP r> exit
   then drop
   PTXIR-ROOM
   PTXIR-N @ {: id:n :}
   id PTXIR-WRITE
   id 1+ PTXIR-N !
   id ;

: PTXIR-INTERN ( n n n n -- n )
   0 >PTXIR-NODE PTXIR-NODE-INTERN ;

: PTXIR-INPUT# ( n -- n ) {: sym:n :}
   PTXIR-K-INPUT PTXIR-NONE PTXIR-NONE sym PTXIR-INTERN ;

: PTXIR-INPUT ( -- n )
   0 PTXIR-INPUT# ;

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

: PTXIR-BSUM ( n -- n ) {: id:n :}
   PTXIR-K-BSUM id PTXIR-NONE 0 PTXIR-INTERN ;

: PTXIR-BSUB-NODE ( n n -- n ) {: tile:n unif:n :}
   PTXIR-K-BSUB tile unif 0 PTXIR-INTERN ;

: PTXIR-BSUB ( n n -- n ) {: tile:n unif:n :}
   unif 0 PTXIR-CONST= if tile exit then
   tile unif PTXIR-BSUB-NODE ;

: PTXIR-LIVE-CLEAR ( -- )
   PTXIR-N @ 0 ?do 0 0= 0= i PTXIR-LIVE! loop ;

: PTXIR-MARK ( n -- ) {: id:n :}
   id PTXIR-LIVE@ if exit then
   0 0= id PTXIR-LIVE!
   id PTXIR-OP@ PTXIR-K-ADD = if id PTXIR-A@ recurse id PTXIR-B@ recurse exit then
   id PTXIR-OP@ PTXIR-K-MUL = if id PTXIR-A@ recurse id PTXIR-B@ recurse exit then
   id PTXIR-OP@ PTXIR-K-NEG = if id PTXIR-A@ recurse exit then
   id PTXIR-OP@ PTXIR-K-BSUM = if id PTXIR-A@ recurse exit then
   id PTXIR-OP@ PTXIR-K-BSUB = if id PTXIR-A@ recurse id PTXIR-B@ recurse exit then ;

: PTXIR-LIVE-COUNT ( n -- n ) {: root:n :}
   PTXIR-LIVE-CLEAR
   root PTXIR-MARK
   0 PTXIR-N @ 0 ?do
      i PTXIR-LIVE@ if 1+ then
   loop ;

: PTXIR-SEP ( -- )
   SB$ nip 0 > if $20 SB-APPEND-C then ;

: PTXIR-TOK ( ptr u8 n -- )
   PTXIR-SEP SB-APPEND ;

: PTXIR-APPEND-INPUT ( n -- )
   case
      0 of s" y" PTXIR-TOK endof
      1 of s" dy" PTXIR-TOK endof
      PTXIR-SEP s" i" SB-APPEND dup SB-U
   endcase ;

: PTXIR-APPEND-CONST ( n -- )
   PTXIR-SEP SB-U ;

: PTXIR-RENDER-NODE ( n -- ) {: id:n :}
   id PTXIR-OP@
   case
      PTXIR-K-INPUT of id PTXIR-VAL@ PTXIR-APPEND-INPUT endof
      PTXIR-K-CONST of id PTXIR-VAL@ PTXIR-APPEND-CONST endof
      PTXIR-K-ADD   of id PTXIR-A@ recurse id PTXIR-B@ recurse s" +." PTXIR-TOK endof
      PTXIR-K-MUL   of id PTXIR-A@ recurse id PTXIR-B@ recurse s" *." PTXIR-TOK endof
      PTXIR-K-NEG   of id PTXIR-A@ recurse s" NEG" PTXIR-TOK endof
      PTXIR-K-BSUM  of id PTXIR-A@ recurse s" BLOCK-SUM" PTXIR-TOK endof
      PTXIR-K-BSUB  of id PTXIR-A@ recurse id PTXIR-B@ recurse s" PTX:B-" PTXIR-TOK endof
      drop E-PTX-IR-UNKNOWN throw
   endcase ;

: PTXIR-RENDER ( n -- ptr u8 n )
   SB-RESET
   PTXIR-RENDER-NODE
   SB$ ;
