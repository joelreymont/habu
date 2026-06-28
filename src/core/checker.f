0 constant T-CON   1 constant T-VAR   2 constant T-PTR
3 constant S-ROW   4 constant S-PUSH
5 constant T-QUOT  6 constant T-ATOM  7 constant T-PARAM
-1 constant UNBOUND
2048 constant MAXTV            \ typevar pool (engine-sized bodies allocate hundreds)
create TVT MAXTV cells allot   create RVT MAXTV cells allot

: TVINIT   \ unbind every type and row var
   0 BEGIN
     dup cells TVT + UNBOUND swap !
     dup cells RVT + UNBOUND swap !
     1 + dup MAXTV 1 - >
   UNTIL drop ;

: TAG 7 and ;

: PAY 3 rshift ;

: MK-CON 3 lshift ;

: MK-VAR 3 lshift T-VAR or ;

: MK-ROW 3 lshift S-ROW or ;

1024 constant MAXPTR
create PTRA MAXPTR cells allot   variable PTRN

: MK-PTR PTRN @ MAXPTR 1 - > IF s" checker: out of ptr terms" 76 die THEN
   PTRN @ cells PTRA + !  PTRN @ 3 lshift T-PTR or  PTRN @ 1 + PTRN ! ;

: PTR>INNER PAY cells PTRA + @ ;

: TV@ cells TVT + @ ;

: TV! cells TVT + ! ;

: RV@ cells RVT + @ ;

: RV! cells RVT + ! ;
256 constant MAXQE             \ quotation effects (din dout rin rout per record)
create QEA MAXQE 32 * allot
create QXDA MAXQE cells allot   create QXRA MAXQE cells allot
create QXHA MAXQE cells allot   create QXNA MAXQE cells allot   variable QEN
: MK-QUOT {: din dout rin rout :}   \ ( -- t ) allocate a quot<effect> term
   QEN @ MAXQE 1 - > IF s" checker: out of quot effects" 76 die THEN
   QEN @ 32 * QEA + {: a :}
   din a !  dout a 8 + !  rin a 16 + !  rout a 24 + !
   0 QEN @ cells QXHA + !
   0 QEN @ cells QXNA + !
   0 QEN @ cells QXDA + !
   0 QEN @ cells QXRA + !
   QEN @ 3 lshift T-QUOT or  QEN @ 1 + QEN ! ;
: Q>DIN  PAY 32 * QEA + @ ;
: Q>DOUT PAY 32 * QEA + 8 + @ ;
: Q>RIN  PAY 32 * QEA + 16 + @ ;
: Q>ROUT PAY 32 * QEA + 24 + @ ;
: Q>XHAS PAY cells QXHA + @ ;
: Q>XDEAD PAY cells QXNA + @ ;
: Q>XDOUT PAY cells QXDA + @ ;
: Q>XROUT PAY cells QXRA + @ ;
: QX! {: q xhas xdead xd xr :}
   xhas q PAY cells QXHA + !
   xdead q PAY cells QXNA + !
   xd q PAY cells QXDA + !
   xr q PAY cells QXRA + ! ;

512 constant MAXATOM
create ATOMA MAXATOM cells allot
create ATOMU MAXATOM cells allot
variable ATOMN
: ATOMA-FIELD ( n -- ptr ptr u8 )
   cells ATOMA + 0 ptr-field ;
: MK-ATOM {: a u :}
   ATOMN @ MAXATOM 1 - > IF s" checker: out of atom terms" 76 die THEN
   a ATOMN @ ATOMA-FIELD !
   u ATOMN @ cells ATOMU + !
   ATOMN @ 3 lshift T-ATOM or
   ATOMN @ 1 + ATOMN ! ;
: ATOM>A ( n -- ptr u8 ) PAY ATOMA-FIELD @ ;
: ATOM>U ( n -- n ) PAY cells ATOMU + @ ;

512 constant MAXPARAM
4 constant PARAM-MAX-ARGS
create PARAMA MAXPARAM cells allot
create PARAMU MAXPARAM cells allot
create PARAMC MAXPARAM cells allot
create PARAMARGS MAXPARAM PARAM-MAX-ARGS * cells allot
create PARAM-SCR PARAM-MAX-ARGS cells allot
variable PARAMN
variable PARAM-SCR-N
variable PARAM-I
: PARAMA-FIELD ( n -- ptr ptr u8 )
   cells PARAMA + 0 ptr-field ;
: PARAM>NAME-A ( n -- ptr u8 ) PAY PARAMA-FIELD @ ;
: PARAM>NAME-U ( n -- n ) PAY cells PARAMU + @ ;
: PARAM>ARGC ( n -- n ) PAY cells PARAMC + @ ;
: PARAM-ARG-IDX ( n n -- ptr n ) {: p idx :}
   p PAY PARAM-MAX-ARGS * idx + cells PARAMARGS + ;
: PARAM>ARG ( n n -- n ) PARAM-ARG-IDX @ ;
: PARAM-ARG-OR-DUMMY ( n n -- n ) {: p idx :}
   idx p PARAM>ARGC < IF p idx PARAM>ARG ELSE 1 MK-CON THEN ;
: PARAM-SCR-RESET ( -- ) 0 PARAM-SCR-N ! ;
: PARAM-SCR-FULL? ( -- bool )
   PARAM-SCR-N @ PARAM-MAX-ARGS >= ;
: PARAM-SCR+ ( t -- )
   PARAM-SCR-N @ cells PARAM-SCR + !
   PARAM-SCR-N @ 1 + PARAM-SCR-N ! ;
: MK-PARAM {: a u :}
   PARAMN @ MAXPARAM 1 - > IF s" checker: out of param terms" 76 die THEN
   a PARAMN @ PARAMA-FIELD !
   u PARAMN @ cells PARAMU + !
   PARAM-SCR-N @ PARAMN @ cells PARAMC + !
   0 PARAM-I !
   BEGIN PARAM-I @ PARAM-SCR-N @ < WHILE
      PARAM-I @ cells PARAM-SCR + @
      PARAMN @ PARAM-MAX-ARGS * PARAM-I @ + cells PARAMARGS + !
      PARAM-I @ 1 + PARAM-I !
   REPEAT
   PARAMN @ 3 lshift T-PARAM or
   PARAMN @ 1 + PARAMN ! ;

4096 constant MAXPUSH          \ push records (engine-sized bodies need hundreds; evaluate's recovery guards grew EM-COMPILE)
create SPA MAXPUSH 16 * allot   variable SPN

: MK-PUSH SPN @ MAXPUSH 1 - > IF s" checker: out of pushes" 76 die THEN
   SPN @ 2 * cells SPA + {: a :} a 8 + ! a ! SPN @ 3 lshift S-PUSH or SPN @ 1 + SPN ! ;

: P>TYPE PAY 2 * cells SPA + @ ;

: P>REST PAY 2 * cells SPA + 8 + @ ;

: ISVAR TAG T-VAR = ;

: ISROW TAG S-ROW = ;

: T-RES BEGIN dup ISVAR IF dup PAY TV@ dup UNBOUND = IF drop 0 0= 0= ELSE nip 0 0= THEN ELSE 0 0= 0= THEN WHILE REPEAT ;

: R-RES BEGIN dup ISROW IF dup PAY RV@ dup UNBOUND = IF drop 0 0= 0= ELSE nip 0 0= THEN ELSE 0 0= 0= THEN WHILE REPEAT ;
4096 constant MAXUWL           \ unify worklist cells (deep spines queue many pairs)
create UWL MAXUWL cells allot   variable USP   variable UOK

: U-PUSH USP @ MAXUWL 1 - > IF s" checker: unify worklist full" 76 die THEN
   USP @ cells UWL + ! USP @ 1 + USP ! ;

: U-POP USP @ 1 - USP ! USP @ cells UWL + @ ;

: PAIR swap U-PUSH U-PUSH ;

: UNPAIR U-POP U-POP swap ;

\ occurs check: binding a row var to a spine containing itself would make the
\ row cyclic — including THROUGH a quotation's effect rows (the ω-combinator
\ must reject, never loop). Recursion depth is bounded by term size; the
\ accumulator rides the stack (a shared variable would be clobbered by the
\ recursive calls).
: ROW-OCC? {: r s :}
   0  s                                  \ ( acc cur )
   BEGIN R-RES dup TAG S-PUSH = WHILE
     dup P>TYPE T-RES
     BEGIN dup TAG T-PTR = WHILE PTR>INNER T-RES REPEAT
     dup TAG T-QUOT = IF
       r over Q>DIN RECURSE  swap        \ ( acc cur f1 qt )
       r over Q>DOUT RECURSE  swap       \ ( acc cur f1 f2 qt )
       r over Q>RIN RECURSE  swap        \ ( acc cur f1 f2 f3 qt )
       r swap Q>ROUT RECURSE             \ ( acc cur f1 f2 f3 f4 )
       or or or  rot or swap             \ ( acc' cur )
     ELSE drop THEN
     P>REST
   REPEAT
   r = or ;

4 constant CC-I64   5 constant CC-U8    6 constant CC-U32   7 constant CC-CELL
8 constant CC-CHAR  9 constant CC-STR  10 constant CC-ADDR  11 constant CC-BOOL
12 constant CC-IDX  13 constant CC-LEN  14 constant CC-COUNT 15 constant CC-OFF
16 constant CC-FD   17 constant CC-RC   18 constant CC-PID   19 constant CC-MS
20 constant CC-NS   21 constant CC-TOK  22 constant CC-REG   23 constant CC-LABEL
24 constant CC-VA   25 constant CC-SYMIDX 26 constant CC-ASM
27 constant CC-IMG  28 constant CC-SNAP  29 constant CC-F32
30 constant CC-MAX
: INT-FAM? {: code :}
   code 1 = IF -1 EXIT THEN
   code CC-I64 = IF -1 EXIT THEN  code CC-U8 = IF -1 EXIT THEN
   code CC-U32 = IF -1 EXIT THEN  code CC-CELL = IF -1 EXIT THEN
   code CC-CHAR = IF -1 EXIT THEN code CC-ADDR = ;
\ CON-OK? ( t1 t2 -- f ) : two concrete cons unify iff equal, or one is the
\ generic int n(1) and the other is int-family (n subsumes any int width).
: CON-OK? {: t1 t2 :}
   t1 PAY t2 PAY = IF -1 EXIT THEN
   t1 PAY 1 = t2 PAY INT-FAM? and IF -1 EXIT THEN
   t2 PAY 1 = t1 PAY INT-FAM? and IF -1 EXIT THEN  0 ;

: ATOM-OK? {: t1 t2 :}
   t1 ATOM>A t1 ATOM>U t2 ATOM>A t2 ATOM>U STR= ;

: PARAM-NAME-OK? {: t1 t2 :}
   t1 PARAM>NAME-A t1 PARAM>NAME-U t2 PARAM>NAME-A t2 PARAM>NAME-U STR= ;

: PARAM-PAIR-ARGS {: t1 t2 :}
   t1 PARAM>ARGC t2 PARAM>ARGC <> IF 0 UOK ! EXIT THEN
   t1 t2 PARAM-NAME-OK? 0= IF 0 UOK ! EXIT THEN
   0 PARAM-I !
   BEGIN PARAM-I @ t1 PARAM>ARGC < WHILE
      t1 PARAM-I @ PARAM>ARG  t2 PARAM-I @ PARAM>ARG  PAIR
      PARAM-I @ 1 + PARAM-I !
   REPEAT ;

: U-ROW R-RES swap R-RES swap 2dup = IF 2drop ELSE
   over ISROW IF 2dup ROW-OCC? IF 2drop 0 UOK ! ELSE swap PAY RV! THEN ELSE
   dup ISROW IF 2dup swap ROW-OCC? IF 2drop 0 UOK ! ELSE PAY RV! THEN ELSE
   2dup P>TYPE swap P>TYPE swap PAIR P>REST swap P>REST swap PAIR THEN THEN THEN ;

variable TOCC  variable TODN  variable TOPARAM

\ TY-OCC? ( v t -- f ) : does tyvar v occur in t, descending through quot
\ effect rows? One worklist holds both terms and rows (disjoint tag spaces);
\ TODN counts pending items, the items ride the data stack.
: TY-OCC? {: v t :}
   0 TOCC !  1 TODN !  t
   BEGIN TODN @ 0 > WHILE
     TODN @ 1 - TODN !
     dup TAG S-ROW =  over TAG S-PUSH =  or IF
       R-RES dup TAG S-PUSH = IF
         dup P>TYPE swap P>REST
         TODN @ 2 + TODN !
       ELSE drop THEN
     ELSE
      T-RES
      dup TAG T-VAR = IF PAY v = IF -1 TOCC ! THEN ELSE
      dup TAG T-PTR = IF
        PTR>INNER
        TODN @ 1 + TODN !
      ELSE
      dup TAG T-QUOT = IF
        dup Q>DIN swap  dup Q>DOUT swap  dup Q>RIN swap  Q>ROUT
        TODN @ 4 + TODN !
      ELSE
      dup TAG T-PARAM = IF
        dup TOPARAM !  drop
        TOPARAM @ 0 PARAM-ARG-OR-DUMMY
        TOPARAM @ 1 PARAM-ARG-OR-DUMMY
        TOPARAM @ 2 PARAM-ARG-OR-DUMMY
        TOPARAM @ 3 PARAM-ARG-OR-DUMMY
        TODN @ PARAM-MAX-ARGS + TODN !
      ELSE drop THEN THEN THEN THEN
     THEN
   REPEAT
   TOCC @ ;

: U-TYPE   \ ( t1 t2 -- ) resolve both; bind a var side, or require equal cons
   T-RES swap T-RES swap
   2dup = IF 2drop ELSE
   over TAG T-QUOT =  over TAG T-QUOT =  and IF
     2dup Q>DIN swap Q>DIN swap PAIR
     2dup Q>DOUT swap Q>DOUT swap PAIR
     2dup Q>RIN swap Q>RIN swap PAIR
     Q>ROUT swap Q>ROUT swap PAIR ELSE
   over TAG T-PTR =  over TAG T-PTR =  and IF
     over PTR>INNER over PTR>INNER PAIR 2drop ELSE
   over TAG T-ATOM =  over TAG T-ATOM =  and IF
     2dup ATOM-OK? IF 2drop ELSE 2drop 0 UOK ! THEN ELSE
   over TAG T-PARAM =  over TAG T-PARAM =  and IF
     2dup PARAM-PAIR-ARGS 2drop ELSE
   over ISVAR IF
     over PAY over TY-OCC? IF 2drop 0 UOK ! ELSE swap PAY TV! THEN ELSE
   dup ISVAR IF
     dup PAY  rot  tuck TY-OCC? IF 2drop 0 UOK ! ELSE swap PAY TV! THEN ELSE
   over TAG T-CON =  over TAG T-CON =  and IF
     2dup CON-OK? IF 2drop ELSE 2drop 0 UOK ! THEN
   ELSE 2drop 0 UOK ! THEN THEN THEN THEN THEN THEN THEN THEN ;

: UNIFY   \ ( s1 s2 -- ok ) worklist-driven; rows and types interleave
   0 USP !  -1 UOK !  PAIR
   BEGIN USP @ UOK @ and WHILE
     UNPAIR  over TAG dup S-ROW = swap S-PUSH = or IF U-ROW ELSE U-TYPE THEN
   REPEAT
   UOK @ ;
variable FV

: FRESH FV @ MAXTV 1 - > IF s" checker: out of typevars" 76 die THEN  FV @ dup 1 + FV ! ;
variable OK   variable DCUR   variable UNCK   variable BROW
variable RCUR   variable RBROW
variable THDROW  variable THRROW  variable THSET
variable XROW  variable XRROW  variable XSET  variable DEADP
variable DEADERR  variable DEADTA  variable DEADTU

: NEW -1 OK ! 0 UNCK ! 0 SPN ! 0 USP ! TVINIT 0 FV ! 0 QEN ! 0 PTRN !
   0 ATOMN ! 0 PARAMN ! 0 PARAM-SCR-N !
   FRESH MK-ROW dup BROW ! DCUR !
   FRESH MK-ROW dup RBROW ! RCUR ! ;
variable WAS   variable DEXP   variable DACT   variable FAILSET
variable VSIG   variable SGSEEN   variable SGIN   variable SGOUT
variable SGRIN  variable SGROUT  variable SGDBASE  variable SGRBASE
variable SGA  variable SGU
$1000 constant TOKBUF-INIT-CAP
$10000 constant TOKBUF-GRAIN
$7FFFFFFFFFFFFFFF constant TOKBUF-MAX-CAP
3 constant TOKBUF-PROT-RW
$1002 constant TOKBUF-MAP-ANON
-1 constant TOKBUF-ANON-FD
0 constant TOKBUF-OFF-ZERO
create FAILTK-BOOT TOKBUF-INIT-CAP allot
create TKF-BOOT TOKBUF-INIT-CAP allot
create NMB-BOOT TOKBUF-INIT-CAP allot
variable FAILTK-P   variable TKF-P   variable NMB-P   variable TOKBUF-CAP-U
variable FAILTU
FAILTK-BOOT FAILTK-P !   TKF-BOOT TKF-P !   NMB-BOOT NMB-P !
TOKBUF-INIT-CAP TOKBUF-CAP-U !
\ FAILTK-FIELD/TKF-FIELD/NMB-FIELD ( -- ptr ptr u8 )
: FAILTK-FIELD FAILTK-P 0 ptr-field ;
: TKF-FIELD TKF-P 0 ptr-field ;
: NMB-FIELD NMB-P 0 ptr-field ;
\ FAILTK/TKF/NMB ( -- ptr u8 )
: FAILTK FAILTK-FIELD @ ;
: TKF TKF-FIELD @ ;
: NMB NMB-FIELD @ ;
\ FAILTK!/TKF!/NMB! ( ptr u8 -- )
: FAILTK! FAILTK-FIELD ! ;
: TKF! TKF-FIELD ! ;
: NMB! NMB-FIELD ! ;
: TOKBUF-ROUND-CAP {: need :}
   need 0 <= IF s" checker: bad token buffer cap" 76 die THEN
   need TOKBUF-MAX-CAP TOKBUF-GRAIN - > IF s" checker: token buffer too large" 76 die THEN
   need 1 - TOKBUF-GRAIN / 1 + TOKBUF-GRAIN * ;
: TOKBUF-ALLOC {: cap :}
   0 cap TOKBUF-PROT-RW TOKBUF-MAP-ANON TOKBUF-ANON-FD TOKBUF-OFF-ZERO mmap
   dup 0 < IF s" checker: token buffer mmap failed" 76 die THEN ;
: TOKBUF-GROW {: need :}
   need TOKBUF-ROUND-CAP {: cap :}
   cap TOKBUF-ALLOC FAILTK!
   cap TOKBUF-ALLOC TKF!
   cap TOKBUF-ALLOC NMB!
   cap TOKBUF-CAP-U ! ;
: TOKBUF-ENSURE {: need :}
   need TOKBUF-CAP-U @ <= IF exit THEN
   need TOKBUF-GROW ;

: TOKBUF-RESET ( -- )
   FAILTK-BOOT FAILTK!
   TKF-BOOT TKF!
   NMB-BOOT NMB!
   TOKBUF-INIT-CAP TOKBUF-CAP-U !
   0 FAILTU ! ;
variable TOKIX  variable FAILIX  variable DVERD
variable FAILB  variable FAILE
variable TBASE  variable TBLEN  variable TI  variable TSTART
variable JSON-DIAGS   0 JSON-DIAGS !

: CHECKER-STEP {: din dout :}
   DCUR @ WAS !
   DCUR @ din UNIFY
   dup 0=  FAILSET @ 0=  and  OK @ and  IF din DEXP !  WAS @ DACT !  -1 FAILSET ! THEN
   OK @ and OK !  dout DCUR ! ;

\ --- return row: >r r> r@ transfer types between DCUR and RCUR. A definition
\ must leave the return row exactly as it found it (ANS 3.2.3.3) — the final
\ balance check rejects net growth or borrowing; loop joins unify RCUR too.
: RS->R                                    \ >r : data top -> return row
   FRESH MK-VAR FRESH MK-ROW {: tv rest :}
   DCUR @  tv rest MK-PUSH  UNIFY OK @ and OK !
   rest DCUR !  tv RCUR @ MK-PUSH RCUR ! ;

: RSR>                                     \ r> : return top -> data row
   FRESH MK-VAR FRESH MK-ROW {: tv rest :}
   RCUR @  tv rest MK-PUSH  UNIFY OK @ and OK !
   rest RCUR !  tv DCUR @ MK-PUSH DCUR ! ;

: RSR@                                     \ r@ : peek return top
   FRESH MK-VAR FRESH MK-ROW {: tv rest :}
   RCUR @  tv rest MK-PUSH  UNIFY OK @ and OK !
   tv DCUR @ MK-PUSH DCUR ! ;

: RS2->R                                   \ 2>r : data pair -> return row
   FRESH MK-VAR FRESH MK-VAR FRESH MK-ROW {: t1 t2 rest :}
   DCUR @  t2 t1 rest MK-PUSH MK-PUSH  UNIFY OK @ and OK !
   rest DCUR !
   t1 RCUR @ MK-PUSH  t2 swap MK-PUSH  RCUR ! ;

: RS2R>                                    \ 2r> : return pair -> data row
   FRESH MK-VAR FRESH MK-VAR FRESH MK-ROW {: t1 t2 rest :}
   RCUR @  t2 t1 rest MK-PUSH MK-PUSH  UNIFY OK @ and OK !
   rest RCUR !
   t1 DCUR @ MK-PUSH  t2 swap MK-PUSH  DCUR ! ;

: RS2R@                                    \ 2r@ : peek return pair
   FRESH MK-VAR FRESH MK-VAR FRESH MK-ROW {: t1 t2 rest :}
   RCUR @  t2 t1 rest MK-PUSH MK-PUSH  UNIFY OK @ and OK !
   t1 DCUR @ MK-PUSH  t2 swap MK-PUSH  DCUR ! ;
variable QTT  variable QD2  variable QR2

: THROW-EDGE ( -- )
   THSET @ 0= IF DCUR @ THDROW !  RCUR @ THRROW ! THEN
   -1 THSET ! ;

: RSEXEC   \ execute: pop the xt; apply its quot effect (or bind a var to one)
   FRESH MK-VAR FRESH MK-ROW {: tv rest :}
   DCUR @  tv rest MK-PUSH  UNIFY OK @ and OK !
   rest DCUR !
   tv T-RES QTT !
   QTT @ TAG T-QUOT = IF
     DCUR @ QTT @ Q>DIN  UNIFY OK @ and OK !
     RCUR @ QTT @ Q>RIN  UNIFY OK @ and OK !
     QTT @ Q>XHAS IF
        THROW-EDGE
     THEN
     QTT @ Q>XDEAD IF
        -1 DEADP !
     ELSE
        QTT @ Q>DOUT DCUR !  QTT @ Q>ROUT RCUR !
     THEN
   ELSE QTT @ TAG T-VAR = IF
     \ unknown xt: bind it to a RETURN-PURE quot over the current state (a
     \ return-impure literal quot then fails to unify at the bind — sound).
     FRESH MK-ROW QD2 !
     DCUR @ QD2 @ RCUR @ RCUR @ MK-QUOT QR2 !
     QTT @ PAY QR2 @ TY-OCC? IF 0 OK ! ELSE
       QR2 @ QTT @ PAY TV!
      QD2 @ DCUR !
     THEN
   ELSE 0 OK ! THEN THEN ;

variable RSRET

: RSCATCH   \ catch: stack-preserving quotation -> same stack plus throw code
   \ Catchable `throw` is not process no-return. The checker tracks throw paths
   \ as an exceptional edge owned by `catch`; `die` remains separate no-return
   \ metadata because it cannot be recovered by a quotation catch.
   -1 RSRET !
   FRESH MK-VAR FRESH MK-ROW {: tv rest :}
   DCUR @  tv rest MK-PUSH  UNIFY OK @ and OK !
   rest DCUR !
   tv T-RES QTT !
   QTT @ TAG T-QUOT = IF
     DCUR @ QTT @ Q>DIN   UNIFY OK @ and OK !
     RCUR @ QTT @ Q>RIN   UNIFY OK @ and OK !
     QTT @ Q>XDEAD IF
        QTT @ Q>XHAS 0= IF 0 RSRET !  -1 DEADP ! THEN
     ELSE
        DCUR @ QTT @ Q>DOUT  UNIFY OK @ and OK !
        RCUR @ QTT @ Q>ROUT  UNIFY OK @ and OK !
     THEN
   ELSE QTT @ TAG T-VAR = IF
     DCUR @ DCUR @ RCUR @ RCUR @ MK-QUOT QR2 !
     QTT @ PAY QR2 @ TY-OCC? IF 0 OK ! ELSE
       QR2 @ QTT @ PAY TV!
     THEN
   ELSE 0 OK ! THEN THEN
   RSRET @ IF 1 MK-CON DCUR @ MK-PUSH DCUR ! THEN ;

variable RSH

: RS-TOK? {: a u :}
   -1 RSH !
   a u s" >r" STR= IF RS->R ELSE
   a u s" r>" STR= IF RSR> ELSE
   a u s" r@" STR= IF RSR@ ELSE
   a u s" 2>r" STR= IF RS2->R ELSE
   a u s" 2r>" STR= IF RS2R> ELSE
   a u s" 2r@" STR= IF RS2R@ ELSE
   a u s" execute" STR= IF RSEXEC ELSE
   a u s" catch" STR= IF RSCATCH ELSE
   0 RSH ! THEN THEN THEN THEN THEN THEN THEN THEN
   RSH @ ;

\ --- generic signature parser: build a step effect from a textual " in -- out "
\ stack effect. A single lowercase letter is a polymorphic type variable (shared
\ across in/out within one signature); `n` = int (con 1), `f` = flag (con 2).
\ Unknown multi-char tokens mark the signature malformed; row variables are
\ shared so the effect is row-polymorphic.
create NMAP 26 cells allot

: NMAP-RESET 0 BEGIN dup cells NMAP + UNBOUND swap ! 1 + dup 25 > UNTIL drop ;

: DIGIT? {: c :} c 47 > c 58 < and ;

: LOWER? {: c :} c 96 > c 123 < and ;
variable NRES  variable NDI  variable NDH
0 constant SGBAD-SYNTAX-KIND
1 constant SGBAD-UNKNOWN-KIND
variable SGBAD
variable SGBAD-A
variable SGBAD-U
variable SGBAD-KIND
variable UNSAFE
variable LOCALBAD

: HEXD? {: c :} c DIGIT?  c 96 > c 103 < and or  c 64 > c 71 < and or ;

\ int literal: d+ | -d+ | $h+ | -$h+ (the engine's number tokens)
: ALLDIG? {: a u :}
   0 NDI !  0 NDH !
   u 0 > IF a c@ 45 = IF 1 NDI ! THEN THEN
   u NDI @ > IF a NDI @ + c@ 36 = IF NDI @ 1 + NDI !  1 NDH ! THEN THEN
   u NDI @ - 0 > 0= IF 0 NRES ! ELSE -1 NRES !
     NDI @ BEGIN dup u < WHILE
       NDH @ IF dup a + c@ HEXD? 0= IF 0 NRES ! THEN
       ELSE dup a + c@ DIGIT? 0= IF 0 NRES ! THEN THEN
       1 + REPEAT drop THEN
   NRES @ ;

\ NB: avoid a 2nd {: :} group here — `{: c :} … {: i :}` mis-reads the slot in the
\ standalone, collapsing every var to one. Compute the slot address on the stack.
: VAR-OF {: c :}  c 97 - cells NMAP +  dup @ UNBOUND = IF FRESH over ! THEN  @ MK-VAR ;

\ NB: declare locals at word top, never inside IF/loop (corrupts the locals frame).
\ concrete width types get distinct con codes; n(1)/f(1) stay the GENERIC int
\ (the prim DB and the toolchain's own body use n), and the unifier lets n
\ subsume any int-family code (so '( i64 -- i64 )' over an n-typed prim still
\ checks). r(3)=float. Table-driven to keep the body small (inline-safe).
: ROLE-OF {: a u :}                     \ nominal scalar role -> con code, or 0
   a u s" idx"   STR= IF CC-IDX   EXIT THEN   a u s" len" STR= IF CC-LEN EXIT THEN
   a u s" count" STR= IF CC-COUNT EXIT THEN   a u s" off" STR= IF CC-OFF EXIT THEN
   a u s" fd"    STR= IF CC-FD    EXIT THEN   a u s" rc"  STR= IF CC-RC  EXIT THEN
   a u s" pid"   STR= IF CC-PID   EXIT THEN   a u s" ms"  STR= IF CC-MS  EXIT THEN
   a u s" ns"    STR= IF CC-NS    EXIT THEN   a u s" tok" STR= IF CC-TOK EXIT THEN
   a u s" reg"   STR= IF CC-REG   EXIT THEN   a u s" label" STR= IF CC-LABEL EXIT THEN
   a u s" va"    STR= IF CC-VA    EXIT THEN   a u s" symidx" STR= IF CC-SYMIDX EXIT THEN
   a u s" asm"   STR= IF CC-ASM   EXIT THEN   a u s" img" STR= IF CC-IMG EXIT THEN
   a u s" snap"  STR= IF CC-SNAP  EXIT THEN   0 ;

: CON-OF {: a u :}                      \ multi-char name -> con code, or 0
   a u s" i64"  STR= IF CC-I64  EXIT THEN   a u s" u8"   STR= IF CC-U8   EXIT THEN
   a u s" u32"  STR= IF CC-U32  EXIT THEN   a u s" cell" STR= IF CC-CELL EXIT THEN
   a u s" f32"  STR= IF CC-F32  EXIT THEN   a u s" char" STR= IF CC-CHAR EXIT THEN
   a u s" str"  STR= IF CC-STR  EXIT THEN
   a u s" addr" STR= IF CC-ADDR EXIT THEN   a u s" bool" STR= IF CC-BOOL EXIT THEN
   a u ROLE-OF ;
: SGBAD-CLEAR ( -- )
   0 SGBAD !
   0 SGBAD-A !
   0 SGBAD-U !
   SGBAD-SYNTAX-KIND SGBAD-KIND ! ;

: SGBAD-SET ( ptr u8 n n -- ) {: a u kind :}
   SGBAD @ IF exit THEN
   -1 SGBAD !
   a SGBAD-A !
   u SGBAD-U !
   kind SGBAD-KIND ! ;

: SGBAD-SYNTAX! ( ptr u8 n -- )
   SGBAD-SYNTAX-KIND SGBAD-SET ;

: SGBAD-UNKNOWN! ( ptr u8 n -- )
   SGBAD-UNKNOWN-KIND SGBAD-SET ;

: SGBAD-UNKNOWN? ( -- bool )
   SGBAD @ SGBAD-KIND @ SGBAD-UNKNOWN-KIND = and ;

: BAD-SIG-TYPE ( ptr u8 n -- type )
   SGBAD-UNKNOWN!
   1 MK-CON ;
: SIG-PREFIX? {: a u p v :}
   u v < IF 0 EXIT THEN
   a v p v STR= ;
: ATOM-TOK? {: a u :}
   a u s" space-" SIG-PREFIX? IF -1 EXIT THEN
   a u s" extent-" SIG-PREFIX? IF -1 EXIT THEN
   a u s" mask-" SIG-PREFIX? IF -1 EXIT THEN
   a u s" block-" SIG-PREFIX? IF -1 EXIT THEN
   a u s" align-" SIG-PREFIX? ;
: PARAM-CTOR? {: a u :}
   a u s" ptr" STR= IF -1 EXIT THEN
   a u s" span" STR= IF -1 EXIT THEN
   a u s" matrix" STR= IF -1 EXIT THEN
   a u s" gridctx" STR= IF -1 EXIT THEN
   a u s" rowctx" STR= IF -1 EXIT THEN
   a u s" tile" STR= IF -1 EXIT THEN
   a u s" acc" STR= IF -1 EXIT THEN
   a u s" uniform" STR= IF -1 EXIT THEN
   a u s" rowidx" STR= ;
: TOK-TYPE {: a u :}  a c@ {: c :}
   u 1 = c 110 = and IF 1 MK-CON ELSE          \ 'n' -> generic int (con 1)
   u 1 = c 102 = and IF CC-BOOL MK-CON ELSE     \ 'f' -> bool (a comparison result is a flag, not an int)
   u 1 = c 114 = and IF 3 MK-CON ELSE          \ 'r' -> real/float (con 3)
   a u CON-OF dup IF MK-CON ELSE drop          \ i64/u8/u32/cell/char/str/addr/bool
   a u ATOM-TOK? IF a u MK-ATOM ELSE
   u 1 = c LOWER? and IF c VAR-OF ELSE          \ single letter -> type var
   a u BAD-SIG-TYPE THEN THEN THEN THEN THEN THEN ;

: LOCAL-TYPE {: a u :}
   a u s" ptr" STR= IF FRESH MK-VAR MK-PTR ELSE a u TOK-TYPE THEN ;

variable SB variable SL variable SI variable SS
variable PKA  variable PKU  variable PKHAVE          \ one-token push-back

: PK!  PKU !  PKA !  -1 PKHAVE ! ;                   \ ( a u -- )
: PKRESET 0 PKHAVE ! ;
\ NEXT-SIG-TOK ( -- a u ) : next signature token over the SB/SL/SI cursor.
\ Whitespace separates tokens, and `<`, `>`, `,` are single-token delimiters so
\ parametric types can be written without spaces: `span<space-global,f32,extent-n>`.
\ ( a 0 ) at end. Honors one pushed-back token.
: SIG-DELIM-CHAR? {: c :}
   c 60 = IF -1 EXIT THEN
   c 62 = IF -1 EXIT THEN
   c 44 = ;
: NEXT-SIG-TOK
   PKHAVE @ IF 0 PKHAVE ! PKA @ PKU @ EXIT THEN
   BEGIN SI @ SL @ < SB @ SI @ + c@ 32 = and WHILE SI @ 1 + SI ! REPEAT
   SI @ SL @ < 0= IF SB @ 0 EXIT THEN
   SB @ SI @ + SS !
   SB @ SI @ + c@ SIG-DELIM-CHAR? IF SI @ 1 + SI ! SS @ 1 EXIT THEN
   BEGIN SI @ SL @ < SB @ SI @ + c@ 32 <> and
      SB @ SI @ + c@ SIG-DELIM-CHAR? 0= and WHILE SI @ 1 + SI ! REPEAT
   SS @ SB @ SI @ + SS @ - ;

: UPPER? {: c :} c 64 > c 91 < and ;
: ROW-LEAD? {: a u :} u 1 = a c@ UPPER? and ;        \ a single upper letter leads a row
: DELIM? {: a u :}                                   \ stack terminator
   u 0 = IF -1 EXIT THEN
   a u s" --" STR= IF -1 EXIT THEN
   a u s" ]"  STR= IF -1 EXIT THEN
   a u s" |"  STR= ;

: SIG-TYPE {: a u :}
   a u PARAM-CTOR? IF
      NEXT-SIG-TOK 2dup s" <" STR= IF
         2drop PARAM-SCR-RESET
         BEGIN
            NEXT-SIG-TOK 2dup s" >" STR= IF
               2drop a u MK-PARAM EXIT
            THEN
            2dup DELIM? IF SGBAD-SYNTAX! a u MK-PARAM EXIT THEN
            PARAM-SCR-FULL? IF SGBAD-SYNTAX! a u MK-PARAM EXIT THEN
            RECURSE PARAM-SCR+
            NEXT-SIG-TOK 2dup s" ," STR= IF 2drop ELSE
            2dup s" >" STR= IF 2drop a u MK-PARAM EXIT ELSE
               SGBAD-SYNTAX! a u MK-PARAM EXIT
            THEN THEN
         AGAIN
      ELSE
         PK!
      THEN
   THEN
   a u s" ptr" STR= IF
      NEXT-SIG-TOK 2dup DELIM? IF 2dup SGBAD-SYNTAX! PK! 1 MK-CON ELSE RECURSE MK-PTR THEN
   ELSE a u TOK-TYPE THEN ;

create ROWMAP 26 cells allot
: ROWMAP-RESET 0 BEGIN dup cells ROWMAP + UNBOUND swap ! 1 + dup 25 > UNTIL drop ;
: RVAR-OF {: c :}  c 65 - cells ROWMAP +  dup @ UNBOUND = IF FRESH over ! THEN  @ MK-ROW ;

\ SGBAD: the declared signature is malformed (a required '--'/']' delimiter was
\ missing or wrong). A malformed contract must REJECT, never silently parse as
\ some other effect. EXPECT-SIG consumes the next sig token and fails closed if
\ it is not the expected delimiter (EOF reads as a 0-length token -> mismatch).
: EXPECT-SIG {: ea eu :}
   NEXT-SIG-TOK 2dup ea eu STR= IF 2drop ELSE SGBAD-SYNTAX! THEN ;

\ PSTACK ( tail -- row ) : parse one stack onto a tail row. A leading single
\ upper-case token names the row (shared by letter); else the passed implicit
\ tail is used. Types fold bottom->top; '[' in -- out [ '|' rin -- rout ] ']'
\ is a quot<effect> (RECURSE for nested stacks; no '|' means rin=rout).
\ tail is a LOCAL so it survives RECURSE; the data stack holds only the row.
: PSTACK {: tail :}
   NEXT-SIG-TOK 2dup ROW-LEAD? IF
      drop c@ RVAR-OF                                 \ row = named var
   ELSE PK! tail THEN                                 \ push back token; row = tail
   BEGIN
     NEXT-SIG-TOK 2dup DELIM? IF PK! EXIT THEN        \ ( row a u )->PK!->( row ), return
     2dup s" [" STR= IF
        2drop
        FRESH MK-ROW                                  \ q data row
        FRESH MK-ROW                                  \ q return row
        over RECURSE                                  \ row qd qr qin
        s" --" EXPECT-SIG
        >r >r                                         \ park qin qr
        RECURSE                                       \ row qout
        r>
        NEXT-SIG-TOK 2dup s" |" STR= IF
           2drop
           dup RECURSE                                \ row qout qr qrin
           s" --" EXPECT-SIG
           >r dup RECURSE                             \ row qout qr qrout
           s" ]" EXPECT-SIG
           swap drop                                  \ row qout qrout
           r> r> 2swap >r rot r>                      \ row qin qout qrin qrout
        ELSE
           2dup s" ]" STR= IF
              2drop
           ELSE
              SGBAD-SYNTAX!
           THEN
           r> swap >r swap r> dup                     \ row qin qout qrin qrout
        THEN
        MK-QUOT
        swap MK-PUSH
     ELSE
        SIG-TYPE  swap MK-PUSH
     THEN
   AGAIN ;

variable SGHASR                          \ a return-stack clause ( ... | rin -- rout ) present?
variable RR-SHARED                       \ the shared return row, allocated lazily on '|'
variable PD-IN variable PR-IN variable PD-OUT variable PR-OUT variable PD-BASE

: RRTAIL ( -- rrow )                     \ the shared return row (allocate once, on demand)
   RR-SHARED @ dup 0= IF drop FRESH MK-ROW dup RR-SHARED ! THEN ;

\ PSIDE ( dtail -- drow rrow ) : one side = data stack [ '|' return stack ]. No
\ '|' -> rrow = the shared return row so far (0 if no clause anywhere) — CHECK
\ ignores it. The return row is allocated only when a '|' actually appears, so
\ ordinary sigs cost no extra typevars.
: PSIDE {: dtail :}
   dtail PSTACK                                   \ data part (stops at | -- ])
   NEXT-SIG-TOK 2dup s" |" STR= IF
      2drop  -1 SGHASR !  RRTAIL PSTACK           \ ( drow rrow ) explicit return
   ELSE PK! RR-SHARED @ THEN ;                    \ no | here -> shared tail (untouched)

\ PSIG ( -- din dout rin rout ) : data + return rows over the cursor.
: PSIG
   PKRESET NMAP-RESET ROWMAP-RESET  0 SGHASR !  0 RR-SHARED !
   FRESH MK-ROW dup PD-BASE ! {: dr :}
   dr PSIDE  PR-IN ! PD-IN !
   s" --" EXPECT-SIG                              \ require the top-level '--'
   dr PSIDE  PR-OUT ! PD-OUT !
   PD-IN @ PD-OUT @ PR-IN @ PR-OUT @ ;

: PARSE-SIG {: a u :}      a SB ! u SL ! 0 SI !  PSIG 2drop CHECKER-STEP ;

\ PARSE-SIG-RAW ( a u -- din dout rin rout ) : the declared effect as four rows
\ (no CHECKER-STEP), for verifying a definition's body against its own ( in -- out ).
: PARSE-SIG-RAW {: a u :}  a SB ! u SL ! 0 SI !  PSIG ;

\ --- prim table: name/sig pairs [nlen][name][slen][sig]...[0], scanned by FIND-SIG.
\ A data table (not a 26-branch word) because the standalone INLINES colon-word
\ bodies, so a dispatch word with many PARSE-SIG calls overflows. DO-TOK stays small.
\ prim sig table: records [nlen][name][slen][sig], 0-terminated — built from
\ readable strings (PT+ keeps the terminator as it appends).
3072 constant PTAB-CAP
create PTAB PTAB-CAP allot  variable PTP
create SDQN 2 allot   115 SDQN c!   34 SDQN 1 + c!    \ the two chars of `s"`
create CDQN 2 allot    99 CDQN c!   34 CDQN 1 + c!    \ the two chars of `c"`
create DOTQN 2 allot   46 DOTQN c!  34 DOTQN 1 + c!   \ the two chars of `."`

: PT2+ {: a u :}
   PTP @ u + 2 +  PTAB PTAB-CAP 2 - +  > IF s" checker: prim table full" 76 die THEN
   u PTP @ c!
   0 BEGIN dup u < WHILE  dup a + c@  over PTP @ + 1 + c!  1 + REPEAT drop
   PTP @ 1 + u + PTP !  0 PTP @ c! ;

: PT+ {: na nu sa su :}  na nu PT2+  sa su PT2+ ;

: PT-STACK-PRIMS ( -- )
   s" dup" s" a -- a a" PT+
   s" drop" s" a --" PT+
   s" swap" s" a b -- b a" PT+
   s" over" s" a b -- a b a" PT+
   s" nip" s" a b -- b" PT+
   s" tuck" s" a b -- b a b" PT+
   s" rot" s" a b c -- b c a" PT+
   s" -rot" s" a b c -- c a b" PT+
   s" 2dup" s" a b -- a b a b" PT+
   s" 2drop" s" a b --" PT+
   s" 2swap" s" a b c d -- c d a b" PT+
   s" 2over" s" a b c d -- a b c d a b" PT+ ;

: PT-NUMERIC-PRIMS ( -- )
   s" +" s" n n -- n" PT+
   s" +" s" ptr a n -- ptr a" PT+
   s" +" s" n ptr a -- ptr a" PT+
   s" -" s" n n -- n" PT+
   s" -" s" ptr a n -- ptr a" PT+
   s" -" s" ptr a ptr a -- n" PT+
   s" *" s" n n -- n" PT+
   s" and" s" n n -- n" PT+
   s" and" s" f f -- f" PT+
   s" or" s" n n -- n" PT+
   s" or" s" f f -- f" PT+
   s" xor" s" n n -- n" PT+
   s" xor" s" f f -- f" PT+
   s" 1+" s" n -- n" PT+
   s" 1+" s" ptr a -- ptr a" PT+
   s" 1-" s" n -- n" PT+
   s" 1-" s" ptr a -- ptr a" PT+
   s" negate" s" n -- n" PT+
   s" invert" s" n -- n" PT+
   s" 0=" s" a -- f" PT+
   s" 0<" s" n -- f" PT+
   s" =" s" n n -- f" PT+
   s" =" s" ptr a ptr a -- f" PT+
   s" <" s" n n -- f" PT+
   s" <" s" ptr a ptr a -- f" PT+
   s" >" s" n n -- f" PT+
   s" >" s" ptr a ptr a -- f" PT+
   s" <>" s" n n -- f" PT+
   s" <>" s" ptr a ptr a -- f" PT+
   s" <=" s" n n -- f" PT+
   s" <=" s" ptr a ptr a -- f" PT+
   s" >=" s" n n -- f" PT+
   s" >=" s" ptr a ptr a -- f" PT+
   s" /" s" n n -- n" PT+
   s" mod" s" n n -- n" PT+
   s" /mod" s" n n -- n n" PT+
   s" abs" s" n -- n" PT+
   s" min" s" n n -- n" PT+
   s" max" s" n n -- n" PT+
   s" lshift" s" n n -- n" PT+
   s" rshift" s" n n -- n" PT+
   s" cells" s" n -- n" PT+
   s" cell+" s" ptr a -- ptr a" PT+
   s" cell+" s" n -- n" PT+
   s" chars" s" n -- n" PT+
   s" char+" s" ptr a -- ptr a" PT+
   s" char+" s" n -- n" PT+ ;

: PT-MEMORY-PRIMS ( -- )
   s" @" s" ptr a -- a" PT+
   s" !" s" a ptr a --" PT+
   s" ptr-field" s" ptr a n -- ptr ptr b" PT+
   s" +!" s" n ptr n --" PT+
   s" c@" s" ptr u8 -- u8" PT+
   s" c!" s" u8 ptr u8 --" PT+
   s" count" s" ptr u8 -- ptr u8 n" PT+ ;

: PT-OUTPUT-PRIMS ( -- )
   s" ." s" n --" PT+
   s" .s" s" --" PT+
   s" depth" s" -- n" PT+
   s" here" s" -- ptr a" PT+
   s" allot" s" n --" PT+
   s" ," s" n --" PT+
   s" c," s" n --" PT+
   s" type" s" ptr u8 n --" PT+
   s" script-argc" s" -- n" PT+
   s" script-argv$" s" n -- ptr u8 n" PT+
   s" throw" s" n --" PT+
   s" die" s" ptr u8 n n --" PT+ ;

: PT-FS-PRIMS ( -- )
   s" open" s" ptr u8 n n -- n" PT+
   s" read" s" n ptr u8 n -- n" PT+
   s" ioctl" s" n n ptr a -- n" PT+
   s" mmap" s" n n n n n n -- n" PT+
   s" path0" s" ptr u8 n -- ptr u8" PT+
   s" open-rd" s" ptr u8 -- n" PT+
   s" access" s" ptr u8 n -- n" PT+
   s" unlink" s" ptr u8 -- n" PT+
   s" rename" s" ptr u8 ptr u8 -- n" PT+
   s" chmod" s" ptr u8 n -- n" PT+
   s" symlink" s" ptr u8 ptr u8 -- n" PT+
   s" readlink" s" ptr u8 ptr u8 n -- n" PT+
   s" mkdir" s" ptr u8 n -- n" PT+
   s" rmdir" s" ptr u8 -- n" PT+
   s" stat64" s" ptr u8 ptr u8 -- n" PT+
   s" lstat64" s" ptr u8 ptr u8 -- n" PT+
   s" getdirentries64" s" n ptr u8 n ptr n -- n" PT+
   s" pipe" s" -- n n n" PT+
   s" dup2" s" n n -- n" PT+
   s" fcntl" s" n n n -- n" PT+
   s" poll" s" ptr a n n -- n" PT+
   s" kill" s" n n -- n" PT+ ;

: PT-PROCESS-PRIMS ( -- )
   s" spawn-io" s" ptr u8 n n n -- n" PT+
   s" spawn-argv-io" s" ptr u8 ptr a n n n -- n" PT+
   s" spawn-argv-env-io" s" ptr u8 ptr a ptr a n n n -- n" PT+
   s" spawn-argv-env-cwd-io" s" ptr u8 ptr a ptr a ptr u8 n n n -- n" PT+
   s" wait-rc" s" n -- n" PT+
   s" wait-status" s" n -- n" PT+
   s" patch32" s" n n --" PT+
   s" write" s" n ptr u8 n -- n" PT+
   s" close" s" n --" PT+
   s" epoch-seconds" s" -- n" PT+
   s" mono-ns" s" -- n" PT+
   s" prof-on" s" n --" PT+
   s" prof-report" s" --" PT+ ;

: PT-SYSTEM-PRIMS ( -- )
   s" rbase" s" -- n" PT+
   s" cp@" s" -- n" PT+
   s" cp!" s" n --" PT+
   s" dbase@" s" -- n" PT+
   s" ndict@" s" -- n" PT+
   s" ndict!" s" n --" PT+
   s" data-base" s" -- ptr a" PT+
   s" wordlist" s" -- n" PT+
   s" get-current" s" -- n" PT+
   s" set-current" s" n --" PT+
   s" search-wl" s" ptr u8 n n -- n" PT+
   s" parse-name" s" -- ptr u8 n" PT+
   s" ffi-call" s" ptr a n -- n" PT+
   s" ffi-call-n" s" ptr a n n -- n" PT+
   s" ffi-call-abi" s" ptr a ptr b ptr c n n -- n" PT+
   s" ffi-call-abi-r" s" ptr a ptr b ptr c n n -- r" PT+ ;

: PT-FLOAT-PRIMS ( -- )
   s" f+" s" r r -- r" PT+    s" f-" s" r r -- r" PT+
   s" f*" s" r r -- r" PT+    s" f/" s" r r -- r" PT+
   s" fnegate" s" r -- r" PT+  s" fabs" s" r -- r" PT+  s" fsqrt" s" r -- r" PT+
   s" f<" s" r r -- f" PT+    s" f>" s" r r -- f" PT+   s" f=" s" r r -- f" PT+
   s" f0<" s" r -- f" PT+     s" f0=" s" r -- f" PT+
   s" s>f" s" n -- r" PT+     s" f>s" s" r -- n" PT+    s" f." s" r --" PT+ ;

: PT-LITERAL-PRIMS ( -- )
   SDQN 2 PT2+  s" -- ptr u8 n" PT2+
   CDQN 2 PT2+  s" -- ptr u8" PT2+
   DOTQN 2 PT2+ s" --" PT2+
   s" [']" s" -- n" PT+
   s" [char]" s" -- n" PT+
   s" emit" s" n --" PT+
   s" cr" s" --" PT+
   s" space" s" --" PT+
   s" u." s" n --" PT+ ;

: PT-DEFINER-PRIMS ( -- )
   s" create" s" -- ptr a" PT+
   s" variable" s" -- ptr a" PT+
   s" constant" s" -- a" PT+ ;

: PTABLE ( -- )
   PTAB PTP !  0 PTAB c!
   PT-STACK-PRIMS
   PT-NUMERIC-PRIMS
   PT-MEMORY-PRIMS
   PT-OUTPUT-PRIMS
   PT-FS-PRIMS
   PT-PROCESS-PRIMS
   PT-SYSTEM-PRIMS
   PT-FLOAT-PRIMS
   PT-LITERAL-PRIMS
   PT-DEFINER-PRIMS ;
PTABLE
variable FSA  variable FSU  variable FNL  variable FNP  variable FSL  variable FSP  variable FP
\ user sigs: certified words recorded as [ulen][name][ulen][sig]*, cell-0
\ terminated. Names are dictionary strings, not counted bytes.
\ Appended by the renderer (RECXT hook); scanned after PTAB so later wins.
$80000 constant USIGS-INIT-CAP
$10000 constant USIGS-GRAIN
$7FFFFFFFFFFFFFFF constant USIGS-MAX-CAP
3 constant USIGS-PROT-RW
$1002 constant USIGS-MAP-ANON
-1 constant USIGS-ANON-FD
0 constant USIGS-OFF-ZERO
create USIGS-BOOT USIGS-INIT-CAP allot
variable USIGS-P   variable USIGS-CAP-U   variable UEND
variable USIGS-GROW-CAP   variable USIGS-GROW-NEXT

: USIGS ( -- ptr u8 ) USIGS-P @ ;

USIGS-BOOT USIGS-P !   USIGS-INIT-CAP USIGS-CAP-U !   0 UEND !   0 USIGS !

: USIGS-COPY {: src:ptr dst:ptr n :}
   n 0 > IF n 0 DO src i + c@ dst i + c! LOOP THEN ;

: USIGS-RESET ( -- )
   USIGS-BOOT USIGS-P !
   USIGS-INIT-CAP USIGS-CAP-U !
   0 UEND !
   0 USIGS !
   0 USIGS-GROW-CAP !
   0 USIGS-GROW-NEXT ! ;

: USIGS-BOOT? ( -- bool )
   USIGS USIGS-BOOT = ;

: USIGS-SNAPSHOT-CAP ( -- )
   UEND @ cell+ USIGS-INIT-CAP > IF s" checker: user sigs snapshot too large" 76 die THEN ;

: USIGS-SNAPSHOT-PERSIST ( -- )
   USIGS-SNAPSHOT-CAP
   USIGS-BOOT? 0= IF USIGS USIGS-BOOT UEND @ cell+ USIGS-COPY THEN
   USIGS-BOOT USIGS-P !
   USIGS-INIT-CAP USIGS-CAP-U !
   0 USIGS-GROW-CAP !
   0 USIGS-GROW-NEXT ! ;

: USIGS-ROUND-CAP {: need :}
   need 0 <= IF s" checker: bad user sig cap" 76 die THEN
   need USIGS-MAX-CAP USIGS-GRAIN - > IF s" checker: user sigs too large" 76 die THEN
   need 1 - USIGS-GRAIN / 1 + USIGS-GRAIN * ;

: USIGS-ALLOC {: cap :}
   0 cap USIGS-PROT-RW USIGS-MAP-ANON USIGS-ANON-FD USIGS-OFF-ZERO mmap
   dup 0 < IF s" checker: user sigs mmap failed" 76 die THEN ;

: USIGS-GROW {: need :}
   need USIGS-ROUND-CAP USIGS-GROW-CAP !
   USIGS-GROW-CAP @ USIGS-ALLOC USIGS-GROW-NEXT !
   USIGS USIGS-GROW-NEXT @ UEND @ cell+ USIGS-COPY
   USIGS-GROW-NEXT @ USIGS-P !
   USIGS-GROW-CAP @ USIGS-CAP-U ! ;

: USIGS-ENSURE {: need :}
   need USIGS-CAP-U @ <= IF exit THEN
   need USIGS-GROW ;

: UB! {: c :}  c USIGS UEND @ + c!  UEND @ 1 + UEND ! ;

: UBS {: a u :}  0 BEGIN dup u < WHILE  dup a + c@ UB!  1 + REPEAT drop ;

\ UALIGN ( n -- n )
: UALIGN 7 + $FFFFFFFFFFFFFFF8 and ;

\ UALIGN! ( -- )
: UALIGN! UEND @ UALIGN UEND ! ;

: U!+ {: x :}  x USIGS UEND @ + !  UEND @ cell+ UEND ! ;

\ UTERM! ( -- )
: UTERM! 0 USIGS UEND @ + ! ;

: UREC-END {: su nu :}
   UEND @ cell+ nu + UALIGN cell+ su + UALIGN ;

: UREC-NEXT {: fp len :} fp USIGS - cell+ len + UALIGN USIGS + ;

: USIG-ADD {: sa su na nu :}
   su nu UREC-END cell+ USIGS-ENSURE
   nu U!+  na nu UBS  UALIGN!
   su U!+  sa su UBS  UALIGN!
   UTERM! ;

: USIG-NEXT ( ptr a -- ptr a )
   dup @ UREC-NEXT ;

: USIG-NAME$ ( ptr a -- ptr u8 n )
   dup cell+ swap @ ;

: USIG-OFF ( ptr a -- n )
   USIGS - ;

: USIG-END? ( ptr a -- bool )
   @ 0= ;

: USIG-FOLD-C ( n -- n ) {: c:n :}
   c $41 < if c exit then
   c $5A > if c exit then
   c $20 or ;

: USIG-STR=CI ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v <> if 0 exit then
   0 begin dup u < while
      dup a + c@ USIG-FOLD-C
      over b + c@ USIG-FOLD-C <> if drop 0 exit then
      1+
   repeat drop
   0 0= ;

: USIG-MATCH? ( ptr a ptr u8 n -- bool ) {: rec:ptr a:ptr u:n :}
   rec USIG-NAME$ a u USIG-STR=CI ;

: USIG-FIND-OFF-REC ( ptr a ptr u8 n -- n bool ) {: rec:ptr a:ptr u:n :}
   rec USIG-END? if 0 0 exit then
   rec a u USIG-MATCH? if rec USIG-OFF -1 exit then
   rec USIG-NEXT a u recurse ;

: USIG-FIND-OFF ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   USIGS a u USIG-FIND-OFF-REC ;

: CHECKER-USIGS-TRUNCATE-FROM ( ptr u8 n -- ) {: a:ptr u:n :}
   a u USIG-FIND-OFF 0= if s" checker: missing signature truncation mark" 76 die then
   UEND !
   UTERM! ;

: SCAN-SIGS {: tab a u :}  tab FP !
   BEGIN FP @ c@ dup WHILE                       \ no locals inside the loop (corrupts frame)
     FNL !  FP @ 1 + FNP !
     FNP @ FNL @ + dup c@ FSL ! 1 + FSP !
     a u FNP @ FNL @ STR= IF FSP @ FSA ! FSL @ FSU ! THEN
     FSP @ FSL @ + FP !
   REPEAT drop ;

: SCAN-USIGS {: a u :}  USIGS FP !
   BEGIN FP @ @ dup WHILE
     FNL !  FP @ cell+ FNP !
     FP @ FNL @ UREC-NEXT dup @ FSL ! cell+ FSP !
     a u FNP @ FNL @ STR= IF FSP @ FSA ! FSL @ FSU ! THEN
     FSP @ FSL @ + USIGS - UALIGN USIGS + FP !
   REPEAT drop ;

: FIND-SIG {: a u :}  0 FSU !  PTAB a u SCAN-SIGS  a u SCAN-USIGS  FSU @ ;

0 constant CHECKER-PACKAGE-NONE
1 constant CHECKER-PACKAGE-PRIVATE
2 constant CHECKER-PACKAGE-PUBLIC
$100 constant CHECKER-PACKAGE-CAP
$240 constant CHECKER-PACKAGE-TOKEN-CAP
create CHECKER-PACKAGE-NAME CHECKER-PACKAGE-CAP allot
create CHECKER-PACKAGE-TOKEN CHECKER-PACKAGE-TOKEN-CAP allot
variable CHECKER-PACKAGE-U
variable CHECKER-PACKAGE-MODE
variable CHECKER-TOKEN-U
variable CHECKER-COLON-N
variable CHECKER-COLON-I
variable CHECKER-REC-A
variable CHECKER-REC-U

: CHECKER-FOLD-C ( n -- n ) {: c:n :}
   c $41 < IF c EXIT THEN
   c $5A > IF c EXIT THEN
   c $20 or ;

: CHECKER-PACKAGE-ACTIVE? ( -- bool )
   CHECKER-PACKAGE-MODE @ CHECKER-PACKAGE-NONE <> ;

: CHECKER-PACKAGE-COPY-C ( ptr u8 n -- ) {: a:ptr i:n :}
   a i + c@ CHECKER-FOLD-C CHECKER-PACKAGE-NAME i + c! ;

: CHECKER-PACKAGE-COPY ( ptr u8 n -- ) {: a:ptr u:n :}
   u CHECKER-PACKAGE-CAP >= IF s" checker: package name too long" 76 die THEN
   0 BEGIN dup u < WHILE
      a over CHECKER-PACKAGE-COPY-C
      1 +
   REPEAT drop
   u CHECKER-PACKAGE-U ! ;

: CHECKER-PACKAGE ( ptr u8 n -- )
   CHECKER-PACKAGE-COPY
   CHECKER-PACKAGE-PRIVATE CHECKER-PACKAGE-MODE ! ;

: CHECKER-PUBLIC ( -- )
   CHECKER-PACKAGE-ACTIVE? IF CHECKER-PACKAGE-PUBLIC CHECKER-PACKAGE-MODE ! THEN ;

: CHECKER-PRIVATE ( -- )
   CHECKER-PACKAGE-ACTIVE? IF CHECKER-PACKAGE-PRIVATE CHECKER-PACKAGE-MODE ! THEN ;

: CHECKER-END-PACKAGE ( -- )
   CHECKER-PACKAGE-NONE CHECKER-PACKAGE-MODE !
   0 CHECKER-PACKAGE-U ! ;

: CHECKER-TOKEN-C ( n -- ) {: c:n :}
   CHECKER-TOKEN-U @ CHECKER-PACKAGE-TOKEN-CAP >= IF s" checker: package token too long" 76 die THEN
   c CHECKER-PACKAGE-TOKEN CHECKER-TOKEN-U @ + c!
   CHECKER-TOKEN-U @ 1+ CHECKER-TOKEN-U ! ;

: CHECKER-TOKEN+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 BEGIN dup u < WHILE
      a over + c@ CHECKER-TOKEN-C
      1 +
   REPEAT drop ;

: CHECKER-PACKAGE-PREFIX ( -- )
   CHECKER-PACKAGE-NAME CHECKER-PACKAGE-U @ CHECKER-TOKEN+
   $3A CHECKER-TOKEN-C ;

: CHECKER-BUILD-PUBLIC ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 CHECKER-TOKEN-U !
   CHECKER-PACKAGE-PREFIX
   a u CHECKER-TOKEN+
   CHECKER-PACKAGE-TOKEN CHECKER-TOKEN-U @ ;

: CHECKER-BUILD-PRIVATE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 CHECKER-TOKEN-U !
   CHECKER-PACKAGE-PREFIX
   s" private:" CHECKER-TOKEN+
   a u CHECKER-TOKEN+
   CHECKER-PACKAGE-TOKEN CHECKER-TOKEN-U @ ;

: CHECKER-COLON-SCAN ( ptr u8 n -- ) {: a:ptr u:n :}
   0 CHECKER-COLON-N !
   -1 CHECKER-COLON-I !
   0 BEGIN dup u < WHILE
      a over + c@ $3A = IF
         CHECKER-COLON-N @ 0= IF dup CHECKER-COLON-I ! THEN
         CHECKER-COLON-N @ 1+ CHECKER-COLON-N !
      THEN
      1 +
   REPEAT drop ;

: CHECKER-PACKAGE-MAP? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   CHECKER-PACKAGE-ACTIVE? 0= IF 0 EXIT THEN
   a u CHECKER-COLON-SCAN
   CHECKER-COLON-N @ 0= IF -1 EXIT THEN
   CHECKER-COLON-N @ 1 = IF
      CHECKER-COLON-I @ 0= IF -1 EXIT THEN
      CHECKER-COLON-I @ u 1- = IF -1 EXIT THEN
   THEN
   0 ;

: CHECKER-FIND-USIG ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 FSU !
   a u SCAN-USIGS
   FSU @ 0 <> ;

: CHECKER-FIND-ACTIVE-SIG ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CHECKER-PACKAGE-MAP? IF
      a u CHECKER-BUILD-PRIVATE CHECKER-FIND-USIG IF EXIT THEN
      a u CHECKER-BUILD-PUBLIC CHECKER-FIND-USIG IF EXIT THEN
   THEN
   a u CHECKER-FIND-USIG drop ;

: CHECKER-RECORD-NAME ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u CHECKER-PACKAGE-MAP? IF
      CHECKER-PACKAGE-MODE @ CHECKER-PACKAGE-PUBLIC = IF
         a u CHECKER-BUILD-PUBLIC EXIT
      THEN
      a u CHECKER-BUILD-PRIVATE EXIT
   THEN
   a u ;

: CHECKER-USIG-ADD ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n na:ptr nu:n :}
   sa su na nu CHECKER-RECORD-NAME USIG-ADD ;

: CHECKER-REC-NAME! ( ptr u8 n -- )
   CHECKER-RECORD-NAME CHECKER-REC-U ! CHECKER-REC-A ! ;

: CHECKER-REC-A@ ( -- ptr u8 )
   CHECKER-REC-A @ ;

: CHECKER-REC-U@ ( -- n )
   CHECKER-REC-U @ ;

: CHECKER-CERT-DUP? ( -- bool )
   CHECKER-REC-A@ CHECKER-REC-U@ CHECKER-FIND-USIG ;

: CHECKER-DUP-DEFINITION ( -- )
   $2 s" checker: duplicate definition: " write drop
   $2 CHECKER-REC-A@ CHECKER-REC-U@ write drop
   s" " $4E die ;

: CHECKER-USIG-CERT-ADD ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n na:ptr nu:n :}
   na nu CHECKER-REC-NAME!
   CHECKER-CERT-DUP? IF CHECKER-DUP-DEFINITION THEN
   sa su CHECKER-REC-A@ CHECKER-REC-U@ USIG-ADD ;

\ Control-effect flags are append-only and later-wins so redefinitions can clear
\ stale metadata. CTL-DEAD means a call has no normal continuation. CTL-THROW
\ means a call may reach a catchable throw edge.
1 constant CTL-DEAD
2 constant CTL-THROW
$10000 constant NORET-INIT-CAP
create NORET-BOOT NORET-INIT-CAP allot
variable NORET-P   variable NORET-CAP-U   variable NORET-END
NORET-BOOT NORET-P !   NORET-INIT-CAP NORET-CAP-U !   0 NORET-END !   0 NORET-BOOT c!
variable NORET-POS   variable NORET-LEN   variable NORET-FLAG
variable NORET-GROW-CAP   variable NORET-GROW-NEXT

: NORETS ( -- ptr u8 ) NORET-P @ ;

: NORET-RESET ( -- )
   NORET-BOOT NORET-P !
   NORET-INIT-CAP NORET-CAP-U !
   0 NORET-END !
   0 NORET-BOOT c!
   0 NORET-GROW-CAP !
   0 NORET-GROW-NEXT ! ;

: NORET-BOOT? ( -- bool )
   NORETS NORET-BOOT = ;

: NORET-SNAPSHOT-CAP ( -- )
   NORET-END @ 1 + NORET-INIT-CAP > IF s" checker: no-return snapshot too large" 76 die THEN ;

: NORET-SNAPSHOT-PERSIST ( -- )
   NORET-SNAPSHOT-CAP
   NORET-BOOT? 0= IF NORETS NORET-BOOT NORET-END @ 1 + USIGS-COPY THEN
   NORET-BOOT NORET-P !
   NORET-INIT-CAP NORET-CAP-U !
   0 NORET-GROW-CAP !
   0 NORET-GROW-NEXT ! ;

: NORET-GROW {: need :}
   need USIGS-ROUND-CAP NORET-GROW-CAP !
   NORET-GROW-CAP @ USIGS-ALLOC NORET-GROW-NEXT !
   NORETS NORET-GROW-NEXT @ NORET-END @ 1 + USIGS-COPY
   NORET-GROW-NEXT @ NORET-P !
   NORET-GROW-CAP @ NORET-CAP-U ! ;

: NORET-ENSURE {: need :}
   need NORET-CAP-U @ <= IF exit THEN
   need NORET-GROW ;

: CHECKER-SNAPSHOT-PREPARE ( -- )
   TOKBUF-RESET
   USIGS-SNAPSHOT-PERSIST
   NORET-SNAPSHOT-PERSIST ;

: NORET-ADD {: a u flag :}
   NORET-END @ u + 3 + NORET-ENSURE
   u NORETS NORET-END @ + c!
   NORET-END @ 1 + NORET-END !
   0 BEGIN dup u < WHILE
      dup a + c@ NORETS NORET-END @ + c!
      NORET-END @ 1 + NORET-END !
      1 +
   REPEAT drop
   flag NORETS NORET-END @ + c!
   NORET-END @ 1 + NORET-END !
   0 NORETS NORET-END @ + c! ;

: CTL-FLAGS {: a u :}
   0 NORET-FLAG !
   0 NORET-POS !
   BEGIN NORETS NORET-POS @ + c@ dup WHILE
      NORET-LEN !
      a u NORETS NORET-POS @ + 1 + NORET-LEN @ STR= IF
         NORETS NORET-POS @ + 1 + NORET-LEN @ + c@ NORET-FLAG !
      THEN
      NORET-POS @ 1 + NORET-LEN @ + 1 + NORET-POS !
   REPEAT drop
   NORET-FLAG @ ;

: NORET-USER? {: a u :}
   a u CTL-FLAGS CTL-DEAD and 0 <> ;

: THROW-USER? {: a u :}
   a u CTL-FLAGS CTL-THROW and 0 <> ;

: DEAD-TOK? {: a u :}
   a u s" die" STR= IF -1 EXIT THEN
   a u s" throw" STR= IF -1 EXIT THEN
   a u NORET-USER? ;

: THROW-TOK? {: a u :}
   a u s" throw" STR= IF -1 EXIT THEN
   a u THROW-USER? ;
create TVSAVE MAXTV cells allot   create RVSAVE MAXTV cells allot
variable SV-FV    variable SV-SPN   variable SV-QEN   variable SV-PTRN
variable SV-OK    variable SV-DCUR  variable SV-RCUR  variable SV-UNCK
variable SV-FSET  variable SV-DEXP  variable SV-DACT  variable SV-SGBAD
variable SV-SGBAD-A  variable SV-SGBAD-U  variable SV-SGBAD-KIND
variable SV-SGSEEN  variable SV-SGHASR  variable SV-SGIN  variable SV-SGOUT
variable SV-SGRIN   variable SV-SGROUT
variable SV-THDROW  variable SV-THRROW  variable SV-THSET

: COPY-CELLS {: src dst n :}
   0 BEGIN dup n < WHILE
      dup cells src + @  over cells dst + !
      1 +
   REPEAT drop ;

: TRIAL-SAVE
   FV @ SV-FV !  TVT TVSAVE SV-FV @ COPY-CELLS  RVT RVSAVE SV-FV @ COPY-CELLS
   SPN @ SV-SPN !  QEN @ SV-QEN !  PTRN @ SV-PTRN !
   OK @ SV-OK !  DCUR @ SV-DCUR !  RCUR @ SV-RCUR !  UNCK @ SV-UNCK !
   FAILSET @ SV-FSET !  DEXP @ SV-DEXP !  DACT @ SV-DACT !
   SGBAD @ SV-SGBAD !  SGBAD-A @ SV-SGBAD-A !
   SGBAD-U @ SV-SGBAD-U !  SGBAD-KIND @ SV-SGBAD-KIND !
   SGSEEN @ SV-SGSEEN !  SGHASR @ SV-SGHASR !
   SGIN @ SV-SGIN !  SGOUT @ SV-SGOUT !  SGRIN @ SV-SGRIN !  SGROUT @ SV-SGROUT !
   THDROW @ SV-THDROW !  THRROW @ SV-THRROW !  THSET @ SV-THSET ! ;

: TRIAL-CLEAR-NEW
   SV-FV @ BEGIN dup FV @ < WHILE
      UNBOUND over cells TVT + !  UNBOUND over cells RVT + !
      1 +
   REPEAT drop ;

: TRIAL-REST-SG
   SV-SGBAD @ SGBAD !  SV-SGBAD-A @ SGBAD-A !
   SV-SGBAD-U @ SGBAD-U !  SV-SGBAD-KIND @ SGBAD-KIND !
   SV-SGSEEN @ SGSEEN !  SV-SGHASR @ SGHASR !
   SV-SGIN @ SGIN !  SV-SGOUT @ SGOUT !  SV-SGRIN @ SGRIN !  SV-SGROUT @ SGROUT ! ;

: TRIAL-REST
   TRIAL-CLEAR-NEW
   TVSAVE TVT SV-FV @ COPY-CELLS  RVSAVE RVT SV-FV @ COPY-CELLS  SV-FV @ FV !
   SV-SPN @ SPN !  SV-QEN @ QEN !  SV-PTRN @ PTRN !
   SV-OK @ OK !  SV-DCUR @ DCUR !  SV-RCUR @ RCUR !  SV-UNCK @ UNCK !
   SV-FSET @ FAILSET !  SV-DEXP @ DEXP !  SV-DACT @ DACT !
   SV-THDROW @ THDROW !  SV-THRROW @ THRROW !  SV-THSET @ THSET !
   TRIAL-REST-SG ;

: TRY-SIG {: a u :}
   TRIAL-SAVE
   a u PARSE-SIG
   OK @ SGBAD @ 0= and IF TRIAL-REST-SG -1 ELSE TRIAL-REST 0 THEN ;

variable TSEEN  variable TSOK  variable TFA  variable TFU

: TRY-TAB {: tab a u :}
   0 TSEEN !  0 TSOK !  0 TFU !  tab FP !
   BEGIN FP @ c@ dup WHILE
     FNL !  FP @ 1 + FNP !
     FNP @ FNL @ + dup c@ FSL ! 1 + FSP !
     a u FNP @ FNL @ STR= IF
       TSEEN @ 0= IF FSP @ TFA !  FSL @ TFU ! THEN
       -1 TSEEN !
       TSOK @ 0= IF FSP @ FSL @ TRY-SIG IF -1 TSOK ! THEN THEN
     THEN
     FSP @ FSL @ + FP !
   REPEAT drop
   TSOK @ ;
variable FLD  variable FLI  variable FLO  variable FLC

: FLODIG? {: a u :}                        \ -?d+.d+ (one interior dot) -> float literal
   0 FLD !  0 FLI !  -1 FLO !
   u 3 < IF 0 FLO ! THEN
   a c@ 45 = IF 1 FLI ! THEN
   FLI @ BEGIN dup u < WHILE
     a over + c@ FLC !
     FLC @ 46 = IF FLD @ 0 > IF 0 FLO ! THEN FLD @ 1 + FLD !
     ELSE FLC @ 47 > FLC @ 58 < and 0= IF 0 FLO ! THEN THEN
     1 + REPEAT drop
   FLD @ 1 = FLO @ and
   u 0 > IF a u 1 - + c@ 46 = IF drop 0 THEN THEN
   a FLI @ + c@ 46 = IF drop 0 THEN ;

: DEFINER-TOK {: a u :}
   SGSEEN @ 0= IF 0 EXIT THEN
   a u s" create" STR= IF -1 EXIT THEN
   a u s" variable" STR= IF -1 EXIT THEN
   a u s" constant" STR= IF s" n --" PARSE-SIG -1 EXIT THEN
   0 ;

: LITERAL-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u ALLDIG? IF s" -- n" PARSE-SIG -1 EXIT THEN
   a u FLODIG? IF s" -- r" PARSE-SIG -1 EXIT THEN
   0 ;

: BYTE-CON? ( t -- bool )
   T-RES dup TAG T-CON = IF PAY CC-U8 = EXIT THEN drop 0 ;

: BYTE-PTR? ( t -- bool )
   T-RES dup TAG T-PTR = IF PTR>INNER BYTE-CON? EXIT THEN drop 0 ;

: ROW-TOP-BYTE-PTR? ( row -- bool )
   R-RES dup TAG S-PUSH = IF P>TYPE BYTE-PTR? EXIT THEN drop 0 ;

: CELL-FETCH-TOK ( -- )
   DCUR @ ROW-TOP-BYTE-PTR? {: bad :}
   s" ptr a -- a" PARSE-SIG
   bad IF 0 OK ! THEN ;

: CELL-STORE-TOK ( -- )
   DCUR @ ROW-TOP-BYTE-PTR? {: bad :}
   s" a ptr a --" PARSE-SIG
   bad IF 0 OK ! THEN ;

: CELL-MEMORY-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" @" STR= IF CELL-FETCH-TOK -1 EXIT THEN
   a u s" !" STR= IF CELL-STORE-TOK -1 EXIT THEN
   0 ;

: DO-TOK {: a u :}
   a u DEFINER-TOK IF EXIT THEN
   a u LITERAL-TOK? IF EXIT THEN
   a u CELL-MEMORY-TOK? IF EXIT THEN
   a u CHECKER-FIND-ACTIVE-SIG
   FSU @ IF FSA @ FSU @ PARSE-SIG ELSE
   PTAB a u TRY-TAB IF EXIT THEN
   TSEEN @ IF TFA @ TFU @ PARSE-SIG ELSE
   -1 UNCK ! THEN THEN ;

\ --- locals: {: a b :} pops and binds names to type vars; a reference pushes
\ its binding. Groups accumulate (a later group binds only its own names).
: CCOPY {: a d u :}  0 BEGIN dup u < WHILE  dup a + c@  over d + c!  1 + REPEAT drop ;
create LOCNB 1024 allot   create LOCLN 64 cells allot   create LOCTV 64 cells allot
variable #LOC  variable LMODE  variable LGRP  variable LROW  variable LCH  variable LI  variable LRF
variable #CFC
variable QDEPTH

variable LCO

: LCOLON {: a u :}   \ ( a u -- ) LCO = index of the first ':' in a/u, or u
   u LCO !
   0 BEGIN  dup u <  LCO @ u =  and WHILE
     dup a + c@ 58 = IF dup LCO ! THEN
     1 + REPEAT drop ;

\ a typed local `a:n` stores the BARE name (matching the engine) and unifies
\ the local's type var with the asserted type — a wrong use then rejects.
: LOC-ADD {: a u :}
   a u LCOLON
   #LOC @ 63 >  LCO @ 16 >  or IF -1 UNCK ! ELSE
     a  LOCNB #LOC @ 16 * +  LCO @ CCOPY
     LCO @ #LOC @ cells LOCLN + !
     FRESH MK-VAR #LOC @ cells LOCTV + !
     LCO @ u < IF
      a LCO @ + 1 +  u LCO @ - 1 -  LOCAL-TYPE
      #LOC @ cells LOCTV + @  UNIFY OK @ and OK !
     THEN
     #LOC @ 1 + #LOC ! THEN ;

: LOC-BIND
   FRESH dup LROW !  MK-ROW LCH !
   LGRP @ BEGIN dup #LOC @ < WHILE
     dup cells LOCTV + @  LCH @ MK-PUSH LCH !
     1 + REPEAT drop
   LCH @  LROW @ MK-ROW  CHECKER-STEP ;

: LOC-TOK {: a u :}
   a u s" :}" STR= IF 0 LMODE ! LOC-BIND ELSE
   a u s" --" STR= IF -1 UNCK ! ELSE
   a u LOC-ADD THEN THEN ;

: LOC-REJECT ( -- )
   0 OK !  -1 FAILSET !  -1 LOCALBAD ! ;

: LOC-BEGIN ( -- )
   #CFC @ 0 >  DEADP @ or IF LOC-REJECT ELSE
   1 LMODE !  #LOC @ LGRP ! THEN ;

: LOC-REF? {: a u :}
   0 LRF !  #LOC @ LI !
   BEGIN LI @ 0 >  LRF @ 0=  and WHILE
     LI @ 1 - LI !
     a u  LOCNB LI @ 16 * +  LI @ cells LOCLN + @  STR= IF
       QDEPTH @ 0 > IF
          LOC-REJECT
       ELSE
          LI @ cells LOCTV + @  DCUR @ MK-PUSH DCUR !
       THEN
       -1 LRF ! THEN
   REPEAT  LRF @ ;
\ --- control flow: branch states saved on a CF stack and unified at joins.
\ Both rows are snapshot: A/B = data, RA/RB = return (PLAN: net growth on
\ either row at a back edge is a row-occurs failure).
\ kinds: 1 if  2 if+else  3 begin  4 begin+while  5 do  6 quotation
create CFKND 32 cells allot   create CFSA 32 cells allot   create CFSB 32 cells allot
create CFRA 32 cells allot    create CFRB 32 cells allot   create CFDED 32 cells allot
\ exit-accumulator save slots: a [: ;] quotation is a nested scope, so its early
\ returns must NOT leak into the enclosing word's accumulator (CF-QUOT saves,
\ CF-SEMIQ folds the quote's own exits then restores).
create CFXRO 32 cells allot   create CFXRR 32 cells allot
create CFXST 32 cells allot    create CFXDP 32 cells allot
create CFTXD 32 cells allot   create CFTXR 32 cells allot
create CFTXS 32 cells allot
variable CTMP  variable RTMP  variable CFH  variable INDO
\ EXIT: an early return. XROW accumulates the data row at each exit (all returns,
\ incl. the fall-through at ';', must unify). DEADP marks the current linear path
\ terminated by exit, so the enclosing THEN excludes it from the branch join.
\ CFDED[i] saves the if-branch's deadness across CF-ELSE. (leave targets the
\ enclosing DO frame's loop-exit row; unloop is a typing no-op — loop control
\ isn't on the typed rows.)
variable RSHAS  variable RSGIN  variable RSGOUT  variable RSGRIN  variable RSGROUT
variable RHAS   variable RDIN   variable RDOUT   variable RRIN    variable RROUT
: CF@DED #CFC @ 1 - cells CFDED + @ ;

: CF-PUSH {: k s0 s1 r0 r1 :}
   #CFC @ 31 > IF -1 UNCK ! ELSE
     k #CFC @ cells CFKND + !  s0 #CFC @ cells CFSA + !  s1 #CFC @ cells CFSB + !
     r0 #CFC @ cells CFRA + !  r1 #CFC @ cells CFRB + !
     #CFC @ 1 + #CFC ! THEN ;

: CF@K #CFC @ 1 - cells CFKND + @ ;

: CF@A #CFC @ 1 - cells CFSA + @ ;

: CF@B #CFC @ 1 - cells CFSB + @ ;

: CF@RA #CFC @ 1 - cells CFRA + @ ;

: CF@RB #CFC @ 1 - cells CFRB + @ ;

: CF-DROP #CFC @ 1 - #CFC ! ;

: CF-MT? #CFC @ 0 > 0= ;

: SUNI {: s :}
   DCUR @ s UNIFY
   dup 0=  FAILSET @ 0=  and  OK @ and  IF s DEXP !  DCUR @ DACT !  -1 FAILSET ! THEN
   OK @ and OK ! ;

: RSUNI {: s :}  RCUR @ s UNIFY OK @ and OK ! ;

: ROW-OPEN? ( row -- bool )
   R-RES TAG S-ROW = ;

: CHECK-ROW-NOT-BORROWED ( row -- )
   dup 0= if drop exit then
   ROW-OPEN? 0= if 0 OK ! then ;

: CHECK-NO-BORROW ( -- )
   SGDBASE @ CHECK-ROW-NOT-BORROWED
   SGRBASE @ CHECK-ROW-NOT-BORROWED ;

: SG-SAVE
   SGHASR @ RSHAS !  SGIN @ RSGIN !  SGOUT @ RSGOUT !
   SGRIN @ RSGRIN !  SGROUT @ RSGROUT ! ;
: SG-REST
   RSHAS @ SGHASR !  RSGIN @ SGIN !  RSGOUT @ SGOUT !
   RSGRIN @ SGRIN !  RSGROUT @ SGROUT ! ;
: CF-RECURSE
   VSIG @ SGSEEN @ and IF
      SG-SAVE
      SGA @ SGU @ PARSE-SIG-RAW
      SGHASR @ RHAS !
      RROUT !  RRIN !  RDOUT !  RDIN !
      SG-REST
      RDIN @ SUNI  RDOUT @ DCUR !
      RHAS @ IF RRIN @ RSUNI  RROUT @ RCUR ! THEN
   ELSE -1 UNCK ! THEN ;

: CF-IF  s" bool --" PARSE-SIG  1 DCUR @ 0 RCUR @ 0 CF-PUSH ;   \ IF consumes a flag, not any value

: CF-ELSE
   CF-MT? IF -1 UNCK ! ELSE CF@K 1 <> IF -1 UNCK ! ELSE
     DEADP @ #CFC @ 1 - cells CFDED + !  0 DEADP !       \ save if-branch deadness; else runs live
     DCUR @ CTMP !  CF@A DCUR !
     RCUR @ RTMP !  CF@RA RCUR !
     2 #CFC @ 1 - cells CFKND + !
     CTMP @ #CFC @ 1 - cells CFSB + !
     RTMP @ #CFC @ 1 - cells CFRB + !
   THEN THEN ;

: CF-THEN
   CF-MT? IF -1 UNCK ! ELSE
     CF@K 1 = IF                                          \ IF ... THEN (no else)
        DEADP @ IF CF@A DCUR !  CF@RA RCUR !  0 DEADP !   \ if-branch exited: take fall-through
        ELSE CF@A SUNI  CF@RA RSUNI THEN  CF-DROP
     ELSE CF@K 2 = IF                                     \ IF ... ELSE ... THEN
        DEADP @  CF@DED                                   \ ( else-dead if-dead )
        2dup and IF 2drop -1 DEADP !                      \ both exited -> path stays dead
        ELSE over IF 2drop CF@B DCUR ! CF@RB RCUR ! 0 DEADP !  \ else exited -> take if-branch
        ELSE nip IF 0 DEADP !                             \ if exited -> keep else (in DCUR)
        ELSE CF@B SUNI CF@RB RSUNI 0 DEADP ! THEN THEN THEN
        CF-DROP
     ELSE -1 UNCK ! THEN THEN THEN ;

: CF-EXIT                                                 \ early return: accumulate, kill path
   XSET @ IF  DCUR @ XROW @ UNIFY OK @ and OK !
              RCUR @ XRROW @ UNIFY OK @ and OK !
   ELSE  DCUR @ XROW !  RCUR @ XRROW !  -1 XSET ! THEN
   -1 DEADP ! ;

: CF-UNLOOP ;                                             \ loop control isn't typed -> no-op

: CF-BEGIN  3 DCUR @ 0 RCUR @ 0 CF-PUSH ;

: CF-UNTIL
   s" bool --" PARSE-SIG
   CF-MT? IF -1 UNCK ! ELSE CF@K 3 <> IF -1 UNCK ! ELSE
     CF@A SUNI  CF@A DCUR !  CF@RA RSUNI  CF@RA RCUR !  CF-DROP THEN THEN ;

: CF-AGAIN                              \ unconditional loop: code after AGAIN is unreachable
   CF-MT? IF -1 UNCK ! ELSE CF@K 3 <> IF -1 UNCK ! ELSE
     CF@A SUNI  CF@A DCUR !  CF@RA RSUNI  CF@RA RCUR !  CF-DROP  -1 DEADP ! THEN THEN ;

: CF-WHILE
   s" bool --" PARSE-SIG
   CF-MT? IF -1 UNCK ! ELSE CF@K 3 <> IF -1 UNCK ! ELSE
     4 #CFC @ 1 - cells CFKND + !
     DCUR @ #CFC @ 1 - cells CFSB + !
     RCUR @ #CFC @ 1 - cells CFRB + !
   THEN THEN ;

: CF-REPEAT
   CF-MT? IF -1 UNCK ! ELSE CF@K 4 <> IF -1 UNCK ! ELSE
     CF@A SUNI  CF@B DCUR !  CF@RA RSUNI  CF@RB RCUR !  CF-DROP THEN THEN ;

: CF-DO  s" n n --" PARSE-SIG  5 DCUR @ 0 RCUR @ 0 CF-PUSH ;

\ At LOOP the exit is always live: ?do/do terminates, and a `leave` jumps here.
\ If the body fall-through is dead (unconditional leave/exit), the back-edge is
\ never taken — skip the body-vs-DO-point unify, but the loop-exit row is still
\ the DO-point row (a zero-trip ?do or a leave both leave exactly that). Live
\ fall-through: the back edge requires a stack-neutral body (CF@A SUNI).
: CF-LOOP
   CF-MT? IF -1 UNCK ! ELSE CF@K 5 <> IF -1 UNCK ! ELSE
     DEADP @ IF  0 DEADP !
     ELSE  CF@A SUNI  CF@RA RSUNI  THEN
     CF@A DCUR !  CF@RA RCUR !  CF-DROP THEN THEN ;

: CF-+LOOP
   s" n --" PARSE-SIG
   CF-MT? IF -1 UNCK ! ELSE CF@K 5 <> IF -1 UNCK ! ELSE
     DEADP @ IF  0 DEADP !
     ELSE  CF@A SUNI  CF@RA RSUNI  THEN
     CF@A DCUR !  CF@RA RCUR !  CF-DROP THEN THEN ;

: CF-I
   0 INDO !  0 BEGIN dup #CFC @ < WHILE
     dup cells CFKND + @ 5 = IF -1 INDO ! THEN  1 + REPEAT drop
   INDO @ IF s" -- n" PARSE-SIG ELSE -1 UNCK ! THEN ;

: CF-J                                     \ needs two enclosing DO frames
   0 INDO !  0 BEGIN dup #CFC @ < WHILE
     dup cells CFKND + @ 5 = IF INDO @ 1 + INDO ! THEN  1 + REPEAT drop
   INDO @ 1 > IF s" -- n" PARSE-SIG ELSE -1 UNCK ! THEN ;

variable LVDO  variable LVDN
\ CF-FINDDO ( -- ) : LVDO = index of the nearest enclosing DO frame, or -1.
\ Scans top-down and stops at the first DO (kind 5) or quotation boundary
\ (kind 6) — a `leave` inside [: ;] does not escape to an outer loop.
: CF-FINDDO
   -1 LVDO !  0 LVDN !
   #CFC @ 1 -
   BEGIN dup 0 >= LVDN @ 0= and WHILE
     dup cells CFKND + @ 5 = IF dup LVDO !  -1 LVDN ! THEN
     dup cells CFKND + @ 6 = IF -1 LVDN ! THEN
     1 - REPEAT drop ;

\ CF-LEAVE : early loop exit. The stack at `leave` must match the loop-exit row
\ (= the DO-point row CFSA, since the body is stack-neutral); likewise the return
\ row. Then the path to `loop` is dead (CF-LOOP revives the live loop exit).
: CF-LEAVE
   CF-FINDDO
   LVDO @ 0< IF -1 UNCK ! ELSE
     LVDO @ cells CFSA + @ SUNI
     LVDO @ cells CFRA + @ RSUNI
     -1 DEADP ! THEN ;

: CF-QUOT   \ [: — pause the outer inference (incl. its exit state), open a nested one
   6  DCUR @  BROW @  RCUR @  RBROW @  CF-PUSH
   XROW @ #CFC @ 1 - cells CFXRO + !  XRROW @ #CFC @ 1 - cells CFXRR + !
   XSET @ #CFC @ 1 - cells CFXST + !  DEADP @ #CFC @ 1 - cells CFXDP + !
   THDROW @ #CFC @ 1 - cells CFTXD + !  THRROW @ #CFC @ 1 - cells CFTXR + !
   THSET @ #CFC @ 1 - cells CFTXS + !
   0 XSET !  0 DEADP !  0 THSET !
   QDEPTH @ 1 + QDEPTH !
   FRESH MK-ROW dup BROW ! DCUR !
   FRESH MK-ROW dup RBROW ! RCUR ! ;

variable QTMP

: CF-SEMIQ  \ ;] — quot<nested effect> pushed onto the restored outer row
   CF-MT? IF -1 UNCK ! ELSE CF@K 6 <> IF -1 UNCK ! ELSE
     XSET @ IF                                   \ fold the quote's OWN early returns into its effect
       DEADP @ IF XROW @ DCUR !  XRROW @ RCUR !
       ELSE DCUR @ XROW @ UNIFY OK @ and OK !  RCUR @ XRROW @ UNIFY OK @ and OK ! THEN
     THEN
     BROW @  DCUR @  RBROW @  RCUR @  MK-QUOT QTMP !
     QTMP @ THSET @ DEADP @ XSET @ 0= and THDROW @ THRROW @ QX!
     #CFC @ 1 - cells CFXRO + @ XROW !  #CFC @ 1 - cells CFXRR + @ XRROW !
     #CFC @ 1 - cells CFXST + @ XSET !  #CFC @ 1 - cells CFXDP + @ DEADP !  \ restore outer exit state
     #CFC @ 1 - cells CFTXD + @ THDROW !  #CFC @ 1 - cells CFTXR + @ THRROW !
     #CFC @ 1 - cells CFTXS + @ THSET !
     QDEPTH @ 1 - QDEPTH !
     CF@B BROW !  CF@RB RBROW !
     CF@RA RCUR !
     QTMP @  CF@A  MK-PUSH DCUR !
     CF-DROP THEN THEN ;

: CF-TOK? {: a u :}
   -1 CFH !
   a u s" [:" STR= IF CF-QUOT ELSE
   a u s" ;]" STR= IF CF-SEMIQ ELSE
   a u s" if" STR= IF CF-IF ELSE
   a u s" else" STR= IF CF-ELSE ELSE
   a u s" then" STR= IF CF-THEN ELSE
   a u s" begin" STR= IF CF-BEGIN ELSE
   a u s" until" STR= IF CF-UNTIL ELSE
   a u s" again" STR= IF CF-AGAIN ELSE
   a u s" while" STR= IF CF-WHILE ELSE
   a u s" repeat" STR= IF CF-REPEAT ELSE
   a u s" do" STR= IF CF-DO ELSE
   a u s" ?do" STR= IF CF-DO ELSE
   a u s" loop" STR= IF CF-LOOP ELSE
   a u s" +loop" STR= IF CF-+LOOP ELSE
   a u s" i" STR= IF CF-I ELSE
   a u s" j" STR= IF CF-J ELSE
   a u s" exit" STR= IF CF-EXIT ELSE
   a u s" leave" STR= IF CF-LEAVE ELSE
   a u s" unloop" STR= IF CF-UNLOOP ELSE
   a u s" recurse" STR= IF CF-RECURSE ELSE
   0 CFH ! THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN
   CFH @ ;
\ first token of the checked text is the word's NAME (skipped, kept for the
\ recorder); RECXT (installed by render.f) records certified sigs by name.
variable NMA  variable NMU  variable TOK0  variable RECXT  0 RECXT !
variable DIAGXT  0 DIAGXT !              \ reject-diagnostic hook (render.f installs)
variable CTLNEW
\ the engine folds A-Z in keyword and dict matching — fold every token the same
\ way (into a scratch copy: the source text may live in the read-only image).
variable TKFU
variable SKI  variable SKF

: SGBAD-IN-SOURCE? ( -- bool )
   SGBAD-U @ 0= IF 0 EXIT THEN
   SGBAD-A @ TBASE @ < IF 0 EXIT THEN
   SGBAD-A @ SGBAD-U @ + TBASE @ TBLEN @ + > IF 0 EXIT THEN
   -1 ;

: SGBAD-COPY-TOKEN ( -- )
   SGBAD-U @ TOKBUF-ENSURE
   SGBAD-A @ FAILTK SGBAD-U @ CCOPY
   SGBAD-U @ FAILTU ! ;

: SGBAD-SPAN! ( -- )
   SGBAD-IN-SOURCE? IF
      SGBAD-A @ TBASE @ - FAILB !
      FAILB @ SGBAD-U @ + FAILE !
   ELSE
      TSTART @ FAILB !
      TI @ FAILE !
   THEN ;

: SGBAD-FAIL! ( -- )
   SGBAD @ 0= IF exit THEN
   FAILSET @ IF exit THEN
   SGBAD-COPY-TOKEN
   SGBAD-SPAN!
   0 FAILIX !
   -1 FAILSET ! ;

: STRING-OPENER? {: a u :}
   a u SDQN 2 STR= IF -1 EXIT THEN
   a u CDQN 2 STR= IF -1 EXIT THEN
   a u DOTQN 2 STR= ;

: SKIP-STRING-PAYLOAD
   TI @ SKI !  0 SKF !
   BEGIN SKI @ TBLEN @ <  SKF @ 0=  and WHILE
      TBASE @ SKI @ + c@ 34 = IF -1 SKF ! ELSE SKI @ 1 + SKI ! THEN
   REPEAT
   SKF @ IF SKI @ 1 + TI ! ELSE TBLEN @ TI ! 0 OK ! THEN ;

: DEAD-OWNER! ( ptr u8 n -- )
   DEADTU !  DEADTA ! ;

: DEAD-CLOSE? {: a u :}
   a u s" else"   STR= IF -1 EXIT THEN
   a u s" then"   STR= IF -1 EXIT THEN
   a u s" loop"   STR= IF -1 EXIT THEN
   a u s" +loop"  STR= IF -1 EXIT THEN
   a u s" repeat" STR= IF -1 EXIT THEN
   a u s" again"  STR= IF -1 EXIT THEN
   a u s" ;]"     STR= IF -1 EXIT THEN
   0 ;

: LIVE-TOKEN? {: a u :}
   DEADP @ 0= IF -1 EXIT THEN
   a u DEAD-CLOSE? ;

: TOKFOLD {: a u :}
   u TOKBUF-ENSURE
   0 BEGIN dup u < WHILE
     dup a + c@  dup 64 >  over 91 <  and IF 32 or THEN
     over TKF + c!  1 +
   REPEAT drop
   u TKFU !  -1 ;
: FAIL-SPAN! ( -- )
   TSTART @ TBASE @ - FAILB !
   FAILB @ TKFU @ + FAILE ! ;
: CAP-FAIL ( -- )
   FAILSET @ 0= IF
      TKF FAILTK TKFU @ CCOPY  TKFU @ FAILTU !  TOKIX @ FAILIX !  FAIL-SPAN!
   THEN ;
create DIAGFB 256 allot   variable DIAGFU
variable DIAGL0  variable DIAGC0  variable DIAGB0
: DIAG-FILE! {: a u :}
   u 255 > IF s" diag: file path too long" 76 die THEN
   0 BEGIN dup u < WHILE
      dup a + c@  over DIAGFB + c!
      1 +
   REPEAT drop
   u DIAGFU ! ;
: DIAG-ORIGIN! {: line col byte :}
   line DIAGL0 !  col DIAGC0 !  byte DIAGB0 ! ;
s" <input>" DIAG-FILE!
1 1 0 DIAG-ORIGIN!

\ TRUST: declare a word's effect without checking its body — the native escape
\ hatch (PLAN's TRUSTED:). Callers are checked against the declared sig.
\ Usage:  s" myword" s" n n -- n" trust
: TRUST {: na nu sa su :}
   na nu TOKFOLD drop
   sa su  TKF TKFU @  CHECKER-USIG-ADD ;

: UNSAFE-TOK? {: a u :}
   a u s" evaluate" STR= IF -1 EXIT THEN
   a u s" trust" STR= IF -1 EXIT THEN
   a u s" set-check" STR= IF -1 EXIT THEN
   a u s" postpone" STR= IF -1 EXIT THEN
   a u s" compile," STR= IF -1 EXIT THEN
   a u s" immediate" STR= IF -1 EXIT THEN
   a u s" [" STR= IF -1 EXIT THEN
   a u s" ]" STR= ;

: REJECT-UNSAFE ( -- )
   -1 UNSAFE !  0 OK !  -1 FAILSET ! ;

variable ISQ
variable IS-TA
variable IS-TU

: IS-WS? ( n -- bool )
   32 <= ;

: IS-SKIP-WS ( -- )
   BEGIN TI @ TBLEN @ < WHILE
      TBASE @ TI @ + c@ IS-WS? 0= IF exit THEN
      TI @ 1 + TI !
   REPEAT ;

: IS-NEXT-TOKEN ( -- ptr u8 n bool )
   IS-SKIP-WS
   TI @ TBLEN @ >= IF 0 0 0 0 EXIT THEN
   TBASE @ TI @ + IS-TA !
   0 IS-TU !
   BEGIN TI @ TBLEN @ < WHILE
      TBASE @ TI @ + c@ IS-WS? IF
         IS-TA @ IS-TU @ -1 EXIT
      THEN
      IS-TU @ 1 + IS-TU !
      TI @ 1 + TI !
   REPEAT
   IS-TA @ IS-TU @ -1 ;

: IS-FAIL ( -- )
   0 OK !
   -1 FAILSET ! ;

: IS-QUOT-ROWS ( ptr u8 n -- n )
   PARSE-SIG-RAW
   SGHASR @ 0= IF 2drop FRESH MK-ROW dup THEN
   MK-QUOT ;

: IS-APPLY ( n -- )
   ISQ !
   FRESH MK-ROW {: rest :}
   DCUR @ ISQ @ rest MK-PUSH UNIFY OK @ and OK !
   rest DCUR ! ;

: IS-TOK ( -- )
   IS-NEXT-TOKEN 0= IF IS-FAIL EXIT THEN
   TOKFOLD drop
   TKF TKFU @ CHECKER-FIND-ACTIVE-SIG
   FSU @ 0= IF IS-FAIL EXIT THEN
   FSA @ FSU @ IS-QUOT-ROWS IS-APPLY ;

: DO-TOK1 {: a u :}
   a u TOKFOLD drop
   CAP-FAIL
   TOK0 @ IF TKF NMB TKFU @ CCOPY  NMB NMA !  TKFU @ NMU !  0 TOK0 ! ELSE
   TKF TKFU @ LIVE-TOKEN? 0= IF -1 DEADERR ! 0 OK ! ELSE
   LMODE @ IF TKF TKFU @ LOC-TOK ELSE
   TKF TKFU @ s" {:" STR= IF LOC-BEGIN ELSE
   TKF TKFU @ UNSAFE-TOK? IF REJECT-UNSAFE ELSE
   TKF TKFU @ s" is" STR= IF IS-TOK ELSE
   OK @ IF TKF TKFU @ s" exit" STR= IF a u DEAD-OWNER! THEN THEN
   OK @ IF TKF TKFU @ s" leave" STR= IF a u DEAD-OWNER! THEN THEN
   OK @ IF TKF TKFU @ s" again" STR= IF a u DEAD-OWNER! THEN THEN
   TKF TKFU @ LOC-REF? 0= IF
   TKF TKFU @ CF-TOK? 0= IF
   TKF TKFU @ RS-TOK? 0= IF
   TKF TKFU @ DO-TOK
   OK @ IF TKF TKFU @ THROW-TOK? IF THROW-EDGE THEN THEN
   OK @ IF TKF TKFU @ DEAD-TOK? IF a u DEAD-OWNER! -1 DEADP ! THEN THEN
   TKF TKFU @ STRING-OPENER? IF SKIP-STRING-PAYLOAD THEN
   THEN THEN THEN THEN THEN THEN THEN THEN THEN
   OK @ 0=  FAILSET @ 0=  and IF -1 FAILSET ! THEN
   UNCK @  FAILSET @ 0=  and IF -1 FAILSET ! THEN
   TOKIX @ 1 + TOKIX ! ;

\ CHECK-RESET ( a u -- )
: CHECK-RESET {: a u :}
   u TOKBUF-ENSURE
   a TBASE !  u TBLEN !  NEW
   0 TI !  1 TOK0 !  0 NMU !  0 #LOC !  0 LMODE !  0 #CFC !  0 QDEPTH !
   0 FAILSET !  0 DEXP !  0 DACT !  0 FAILTU !  0 SGSEEN !  0 SGHASR !
   0 SGIN !  0 SGOUT !  0 SGRIN !  0 SGROUT !  0 SGDBASE !  0 SGRBASE !
   0 SGA !  0 SGU !
   0 TOKIX !  0 FAILIX !  0 DVERD !
   0 FAILB !  0 FAILE !  0 XSET !  0 DEADP !  0 DEADERR !  0 DEADTA !  0 DEADTU !
   0 THDROW !  0 THRROW !  0 THSET !
   SGBAD-CLEAR  0 UNSAFE !  0 LOCALBAD ! ;

: CHECK-SCAN ( -- )
   BEGIN TI @ TBLEN @ < WHILE
     BEGIN TI @ TBLEN @ <  TBASE @ TI @ + c@ 32 =  and WHILE TI @ 1 + TI ! REPEAT
     TI @ TBLEN @ < IF
       TBASE @ TI @ + c@ 40 =  TBASE @ TI @ + 1 + c@ 32 =  and IF   \ '( ' (not '(CMP)') -> sig
         TI @ 1 + TI !  TI @ TSTART !             \ sig text starts after '('
         BEGIN TI @ TBLEN @ <  TBASE @ TI @ + c@ 41 <>  and WHILE TI @ 1 + TI ! REPEAT
         VSIG @ IF
           TBASE @ TSTART @ + SGA !  TI @ TSTART @ - SGU !
           TBASE @ TSTART @ +  TI @ TSTART @ -  PARSE-SIG-RAW   \ ( din dout rin rout )
           SGBAD-FAIL!
           PD-BASE @ SGDBASE !
           RR-SHARED @ SGRBASE !
           SGHASR @ IF
             SGROUT !  dup SGRIN !  RCUR !  SGOUT !  dup SGIN !  DCUR !
           ELSE
             2drop  SGOUT !  dup SGIN !  DCUR !
           THEN  -1 SGSEEN !
         THEN
         TI @ TBLEN @ < IF TI @ 1 + TI ! THEN     \ skip ')'
       ELSE
         TBASE @ TI @ + TSTART !
         BEGIN TI @ TBLEN @ <  TBASE @ TI @ + c@ 32 <>  and WHILE TI @ 1 + TI ! REPEAT
         TSTART @  TBASE @ TI @ +  TSTART @ -  DO-TOK1
       THEN
     THEN
   REPEAT ;

: CHECK-FOLD-EXITS ( -- )
   XSET @ IF                                         \ fold early-return states into the output
     DEADP @ IF XROW @ DCUR !  XRROW @ RCUR !         \ every path exited: output = accumulator
     ELSE DCUR @ XROW @ UNIFY OK @ and OK !  RCUR @ XRROW @ UNIFY OK @ and OK ! THEN
   THEN ;

: CHECK-VERDICT ( -- n )
   SGBAD @ UNSAFE @ or  LOCALBAD @ or IF 0 ELSE UNCK @ IF 1 ELSE OK @ THEN THEN ;

: CHECK {: a u :}   \ ( a u -- -1=certified | 0=rejected | 1=uncheckable )
   a u CHECK-RESET
   CHECK-SCAN
   CHECK-FOLD-EXITS
   VSIG @ SGSEEN @ and IF CHECK-NO-BORROW THEN
   VSIG @ SGSEEN @ and IF
      SGOUT @ SUNI
      OK @ IF SGIN @ BROW !  SGOUT @ DCUR ! THEN    \ record the verified declared effect
   THEN                                        \ SUNI captures declared(exp)/inferred(act)
   LMODE @ 0 <>  #CFC @ 0 <>  or IF -1 UNCK ! THEN
   SGHASR @ 0= IF RCUR @ R-RES  RBROW @ R-RES  <> IF 0 OK ! THEN THEN   \ balance (no clause)
   VSIG @ SGSEEN @ SGHASR @ and and IF
      RCUR @ SGROUT @ UNIFY OK @ and OK !
      OK @ IF SGRIN @ RBROW !  SGROUT @ RCUR ! THEN
   THEN
   CHECK-VERDICT                                      \ malformed/unsafe rejects
   dup DVERD !
   dup 0 =  over 1 = JSON-DIAGS @ and  or
   DIAGXT @ 0 <> and IF DIAGXT @ execute THEN
   dup -1 = NMU @ 0 > and IF
      0 CTLNEW !
      DEADP @ XSET @ 0= and IF CTLNEW @ CTL-DEAD or CTLNEW ! THEN
      THSET @ IF CTLNEW @ CTL-THROW or CTLNEW ! THEN
      NMA @ NMU @ CTL-FLAGS CTLNEW @ <> IF
         NMA @ NMU @ CTLNEW @ NORET-ADD
      THEN
      VSIG @ SGSEEN @ and IF
         SGA @ SGU @  NMA @ NMU @  CHECKER-USIG-CERT-ADD
      ELSE
         RECXT @ 0 <> IF NMA @ NMU @ RECXT @ execute THEN
      THEN
   THEN ;

\ CHECK! ( a u -- flag ) : like CHECK but VERIFIES the body against a leading
\ ( in -- out ) declared sig (rejects on mismatch). The standalone REPL hook.
: CHECK! {: a u :}  -1 VSIG !  a u CHECK  0 VSIG ! ;

: DOES-DIN ( row -- row' )
   FRESH MK-VAR MK-PTR swap MK-PUSH ;

: RAW-SIG! ( din dout rin rout -- )
   PD-BASE @ SGDBASE !
   RR-SHARED @ SGRBASE !
   SGHASR @ IF
      SGROUT !  SGRIN !  SGOUT !  SGIN !
   ELSE
      2drop  SGOUT !  SGIN !
   THEN ;

\ CHECK-DOES! ( body-a body-u sig-a sig-u -- verdict ) verifies a DOES> body
\ against a created-word runtime effect.  If the created word is declared
\ `( in -- out )`, the DOES> body must type as `( in ptr a -- out )`: the native
\ CREATE stub pushes the created word's data-field address before branching to
\ the DOES> body.
: CHECK-DOES! {: ba bu sa su :}
   ba bu CHECK-RESET
   0 TOK0 !
   sa su PARSE-SIG-RAW RAW-SIG!
   SGIN @ DOES-DIN dup BROW ! DCUR !
   SGHASR @ IF SGRIN @ dup RBROW ! RCUR ! THEN
   CHECK-SCAN
   CHECK-FOLD-EXITS
   CHECK-NO-BORROW
   SGOUT @ SUNI
   OK @ IF SGOUT @ DCUR ! THEN
   LMODE @ 0 <>  #CFC @ 0 <>  or IF -1 UNCK ! THEN
   SGHASR @ 0= IF RCUR @ R-RES  RBROW @ R-RES  <> IF 0 OK ! THEN THEN
   SGHASR @ IF RCUR @ SGROUT @ UNIFY OK @ and OK ! THEN
   CHECK-VERDICT dup DVERD ! ;
