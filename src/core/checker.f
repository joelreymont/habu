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
create ATOMK MAXATOM cells allot
variable ATOMN
variable RIGID-N
: ATOMA-FIELD ( n -- ptr ptr u8 )
   cells ATOMA + 0 ptr-field ;
: RIGID-RESET ( -- )
   1 RIGID-N ! ;
: RIGID-FRESH ( -- n )
   RIGID-N @ dup 1+ RIGID-N ! ;
: MK-ATOM-K ( ptr u8 n n -- n ) {: a:ptr u:n k:n :}
   ATOMN @ MAXATOM 1 - > IF s" checker: out of atom terms" 76 die THEN
   a ATOMN @ ATOMA-FIELD !
   u ATOMN @ cells ATOMU + !
   k ATOMN @ cells ATOMK + !
   ATOMN @ 3 lshift T-ATOM or
   ATOMN @ 1 + ATOMN ! ;
: MK-ATOM ( ptr u8 n -- n )
   0 MK-ATOM-K ;
: ATOM>A ( n -- ptr u8 ) PAY ATOMA-FIELD @ ;
: ATOM>U ( n -- n ) PAY cells ATOMU + @ ;
: ATOM>K ( n -- n ) PAY cells ATOMK + @ ;

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

1 constant CC-N     2 constant CC-F     3 constant CC-R
4 constant CC-I64   5 constant CC-U8    6 constant CC-U32   7 constant CC-CELL
8 constant CC-CHAR  9 constant CC-STR  10 constant CC-ADDR  11 constant CC-BOOL
12 constant CC-IDX  13 constant CC-LEN  14 constant CC-COUNT 15 constant CC-OFF
16 constant CC-FD   17 constant CC-RC   18 constant CC-PID   19 constant CC-MS
20 constant CC-NS   21 constant CC-TOK  22 constant CC-REG   23 constant CC-LABEL
24 constant CC-VA   25 constant CC-SYMIDX 26 constant CC-ASM
27 constant CC-IMG  28 constant CC-SNAP  29 constant CC-F32
30 constant CC-U16 31 constant CC-MAX
256 constant CT-CAP
4096 constant CT-STR-CAP

0 constant CT-NONE
1 constant CT-INT
2 constant CT-ROLE
3 constant CT-BOOL
4 constant CT-FLOAT
5 constant CT-OBJ

0 constant CS-NONE
1 constant CS-GENERIC
2 constant CS-SIGNED
3 constant CS-UNSIGNED
4 constant CS-ADDR

0 constant UK-EXACT
1 constant UK-INPUT
variable UNIFY-KIND
UK-EXACT UNIFY-KIND !

create CT-NAME-A CT-CAP cells allot
create CT-NAME-U CT-CAP cells allot
create CT-CLASS CT-CAP cells allot
create CT-WIDTH CT-CAP cells allot
create CT-SIGN CT-CAP cells allot
create CT-STR CT-STR-CAP allot
variable CTN
variable CT-STR-U
variable CT-I
variable CT-J
variable CT-DST

1 CTN !
0 CT-STR-U !

: CT-NAME-FIELD ( n -- ptr ptr u8 )
   cells CT-NAME-A + 0 ptr-field ;

: CT-DST-FIELD ( -- ptr ptr u8 )
   CT-DST 0 ptr-field ;

: CT-DST@ ( -- ptr u8 )
   CT-DST-FIELD @ ;

: CT-DST! ( ptr u8 -- )
   CT-DST-FIELD ! ;

: CT-CODE-CHECK ( n -- )
   dup 0 <= IF s" checker: bad signature type code" 76 die THEN
   CT-CAP >= IF s" checker: signature type table full" 76 die THEN ;

: CT-ROOM ( n -- )
   CTN @ CT-CAP >= IF s" checker: signature type table full" 76 die THEN
   CT-STR-U @ + CT-STR-CAP > IF s" checker: signature type strings full" 76 die THEN ;

: CT-COPY ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u CT-ROOM
   CT-STR CT-STR-U @ + CT-DST!
   0 CT-J !
   begin CT-J @ u < while
      a CT-J @ + c@ CT-DST@ CT-J @ + c!
      CT-J @ 1 + CT-J !
   repeat
   CT-STR-U @ u + CT-STR-U !
   CT-DST@ u ;

: CT-ADVANCE ( n -- )
   1 + dup CTN @ > IF CTN ! ELSE drop THEN ;

: CT-SET ( ptr u8 n n n n n -- ) {: a:ptr u:n code:n class:n width:n sign:n :}
   code CT-CODE-CHECK
   a u CT-COPY {: dst:ptr len:n :}
   dst code CT-NAME-FIELD !
   len code cells CT-NAME-U + !
   class code cells CT-CLASS + !
   width code cells CT-WIDTH + !
   sign code cells CT-SIGN + !
   code CT-ADVANCE ;

: CT-INIT ( -- )
   s" n"       CC-N      CT-INT   64 CS-GENERIC CT-SET
   s" f"       CC-F      CT-BOOL   1 CS-NONE    CT-SET
   s" r"       CC-R      CT-FLOAT 64 CS-NONE    CT-SET
   s" i64"     CC-I64    CT-INT   64 CS-SIGNED  CT-SET
   s" u8"      CC-U8     CT-INT    8 CS-UNSIGNED CT-SET
   s" u32"     CC-U32    CT-INT   32 CS-UNSIGNED CT-SET
   s" cell"    CC-CELL   CT-INT   64 CS-GENERIC CT-SET
   s" char"    CC-CHAR   CT-INT    8 CS-UNSIGNED CT-SET
   s" str"     CC-STR    CT-OBJ    0 CS-NONE    CT-SET
   s" addr"    CC-ADDR   CT-INT   64 CS-ADDR    CT-SET
   s" bool"    CC-BOOL   CT-BOOL   1 CS-NONE    CT-SET
   s" idx"     CC-IDX    CT-ROLE  64 CS-NONE    CT-SET
   s" len"     CC-LEN    CT-ROLE  64 CS-NONE    CT-SET
   s" count"   CC-COUNT  CT-ROLE  64 CS-NONE    CT-SET
   s" off"     CC-OFF    CT-ROLE  64 CS-NONE    CT-SET
   s" fd"      CC-FD     CT-ROLE  64 CS-NONE    CT-SET
   s" rc"      CC-RC     CT-ROLE  64 CS-NONE    CT-SET
   s" pid"     CC-PID    CT-ROLE  64 CS-NONE    CT-SET
   s" ms"      CC-MS     CT-ROLE  64 CS-NONE    CT-SET
   s" ns"      CC-NS     CT-ROLE  64 CS-NONE    CT-SET
   s" tok"     CC-TOK    CT-ROLE  64 CS-NONE    CT-SET
   s" reg"     CC-REG    CT-ROLE  64 CS-NONE    CT-SET
   s" label"   CC-LABEL  CT-ROLE  64 CS-NONE    CT-SET
   s" va"      CC-VA     CT-ROLE  64 CS-NONE    CT-SET
   s" symidx"  CC-SYMIDX CT-ROLE  64 CS-NONE    CT-SET
   s" asm"     CC-ASM    CT-ROLE  64 CS-NONE    CT-SET
   s" img"     CC-IMG    CT-ROLE  64 CS-NONE    CT-SET
   s" snap"    CC-SNAP   CT-ROLE  64 CS-NONE    CT-SET
   s" f32"     CC-F32    CT-FLOAT 32 CS-NONE    CT-SET
   s" u16"     CC-U16    CT-INT   16 CS-UNSIGNED CT-SET ;

CT-INIT

: CT-CLASS@ ( n -- n )
   cells CT-CLASS + @ ;

: CT-WIDTH@ ( n -- n )
   cells CT-WIDTH + @ ;

: CT-SIGN@ ( n -- n )
   cells CT-SIGN + @ ;

: CT-INT? ( n -- bool )
   CT-CLASS@ CT-INT = ;

: CT-NAME$ ( n -- ptr u8 n )
   dup CT-NAME-FIELD @
   swap cells CT-NAME-U + @ ;

: CT-NAME= ( ptr u8 n n -- bool ) {: a:ptr u:n code:n :}
   code CT-NAME$ a u CORE-STR= ;

: CT-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   1 CT-I !
   begin CT-I @ CTN @ < while
      a u CT-I @ CT-NAME= IF CT-I @ exit THEN
      CT-I @ 1 + CT-I !
   repeat 0 ;

: INT-FAM? {: code :}
   code CT-INT? ;

: INT-WIDENS? {: got:n want:n :}
   got want = IF -1 EXIT THEN
   got INT-FAM? want INT-FAM? and 0= IF 0 EXIT THEN
   got CC-N = IF -1 EXIT THEN
   want CC-N = IF -1 EXIT THEN
   got CT-WIDTH@ want CT-WIDTH@ <= 0= IF 0 EXIT THEN
   got CT-SIGN@ CS-GENERIC = IF -1 EXIT THEN
   want CT-SIGN@ CS-GENERIC = IF -1 EXIT THEN
   got CT-SIGN@ want CT-SIGN@ = IF -1 EXIT THEN
   got CT-SIGN@ CS-UNSIGNED = want CT-SIGN@ CS-SIGNED = and
   got CT-WIDTH@ want CT-WIDTH@ < and ;

\ CON-OK? ( t1 t2 -- f ) : exact joins require the same concrete code except for
\ generic n/int-family interaction. Input/output checks use the integer lattice:
\ a narrower concrete int can flow into a wider one; widening never applies to
\ nominal roles (pid/fd/rc/idx/len/...), which stay strict.
: CON-OK? {: t1 t2 :}
   t1 PAY t2 PAY = IF -1 EXIT THEN
   UNIFY-KIND @ UK-INPUT = IF t1 PAY t2 PAY INT-WIDENS? EXIT THEN
   t1 PAY CC-N = t2 PAY INT-FAM? and IF -1 EXIT THEN
   t2 PAY CC-N = t1 PAY INT-FAM? and IF -1 EXIT THEN
   0 ;

: ATOM-OK? {: t1 t2 :}
   t1 ATOM>K t2 ATOM>K <> IF 0 EXIT THEN
   t1 ATOM>K 0 < IF 0 EXIT THEN
   t1 ATOM>K 0 = 0= IF -1 EXIT THEN
   t1 ATOM>A t1 ATOM>U t2 ATOM>A t2 ATOM>U CORE-STR= ;

: PARAM-NAME-OK? {: t1 t2 :}
   t1 PARAM>NAME-A t1 PARAM>NAME-U t2 PARAM>NAME-A t2 PARAM>NAME-U CORE-STR= ;

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

: UNIFY-EXACT ( n n -- bool )
   UK-EXACT UNIFY-KIND !
   UNIFY ;

: UNIFY-IN ( n n -- bool )
   UK-INPUT UNIFY-KIND !
   UNIFY
   UK-EXACT UNIFY-KIND ! ;
variable FV

: FRESH FV @ MAXTV 1 - > IF s" checker: out of typevars" 76 die THEN  FV @ dup 1 + FV ! ;
variable OK   variable DCUR   variable UNCK   variable BROW
variable RCUR   variable RBROW
variable THDROW  variable THRROW  variable THSET
variable XROW  variable XRROW  variable XSET  variable DEADP
variable DEADERR  variable DEADTA  variable DEADTU

: NEW -1 OK ! 0 UNCK ! 0 SPN ! 0 USP ! TVINIT 0 FV ! 0 QEN ! 0 PTRN !
   RIGID-RESET
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

: DIAG-JSON! ( bool -- )
   JSON-DIAGS ! ;

: CHECKER-STEP {: din dout :}
   DCUR @ WAS !
   DCUR @ din UNIFY-IN
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
     DCUR @ QTT @ Q>DIN  UNIFY-IN OK @ and OK !
     RCUR @ QTT @ Q>RIN  UNIFY-IN OK @ and OK !
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
     DCUR @ QTT @ Q>DIN   UNIFY-IN OK @ and OK !
     RCUR @ QTT @ Q>RIN   UNIFY-IN OK @ and OK !
     QTT @ Q>XDEAD IF
        QTT @ Q>XHAS 0= IF 0 RSRET !  -1 DEADP ! THEN
     ELSE
        DCUR @ QTT @ Q>DOUT  UNIFY-IN OK @ and OK !
        RCUR @ QTT @ Q>ROUT  UNIFY-IN OK @ and OK !
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
   a u s" >r" CORE-STR= IF RS->R ELSE
   a u s" r>" CORE-STR= IF RSR> ELSE
   a u s" r@" CORE-STR= IF RSR@ ELSE
   a u s" 2>r" CORE-STR= IF RS2->R ELSE
   a u s" 2r>" CORE-STR= IF RS2R> ELSE
   a u s" 2r@" CORE-STR= IF RS2R@ ELSE
   a u s" execute" CORE-STR= IF RSEXEC ELSE
   a u s" catch" CORE-STR= IF RSCATCH ELSE
   0 RSH ! THEN THEN THEN THEN THEN THEN THEN THEN
   RSH @ ;

\ --- generic signature parser: build a step effect from a textual " in -- out "
\ stack effect. A single lowercase letter is a polymorphic type variable (shared
\ across in/out within one signature); `n` = int (con 1), `f` = flag (con 2).
\ Unknown multi-char tokens mark the signature malformed; row variables are
\ shared so the effect is row-polymorphic.
create NMAP 26 cells allot

: NMAP-RESET 0 BEGIN dup cells NMAP + UNBOUND swap ! 1 + dup 25 > UNTIL drop ;

64 constant FAM-CAP
create FAM-A FAM-CAP cells allot
create FAM-U FAM-CAP cells allot
variable FAM-N
variable FAM-I
variable FAM-K

: FAM-RESET ( -- )
   0 FAM-N ! ;

: FAM-A-FIELD ( n -- ptr ptr u8 )
   cells FAM-A + 0 ptr-field ;

: FAM-A@ ( n -- ptr u8 )
   FAM-A-FIELD @ ;

: FAM-IDX>KEY ( n -- n )
   1+ negate ;

: FAM-MATCH? ( ptr u8 n n -- bool ) {: a:ptr u:n idx:n :}
   idx FAM-A@ idx cells FAM-U + @ a u CORE-STR= ;

: FAM-FIND ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   0 FAM-I !
   BEGIN FAM-I @ FAM-N @ < WHILE
      a u FAM-I @ FAM-MATCH? IF FAM-I @ -1 EXIT THEN
      FAM-I @ 1 + FAM-I !
   REPEAT
   0 0 ;

: FAM-ADD ( ptr u8 n -- n ) {: a:ptr u:n :}
   FAM-N @ FAM-CAP >= IF s" checker: fresh atom table full" 76 die THEN
   a FAM-N @ FAM-A-FIELD !
   u FAM-N @ cells FAM-U + !
   FAM-N @ FAM-IDX>KEY
   FAM-N @ 1 + FAM-N ! ;

: FAM-MARK ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u FAM-FIND IF FAM-IDX>KEY EXIT THEN drop
   a u FAM-ADD ;

: DIGIT? {: c :} c 47 > c 58 < and ;

: LOWER? {: c :} c 96 > c 123 < and ;
variable NRES  variable NDI  variable NDH
0 constant SGBAD-SYNTAX-KIND
1 constant SGBAD-UNKNOWN-KIND
2 constant SGBAD-BAREPTR-KIND
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
: CON-OF {: a u :}                      \ multi-char name -> con code, or 0
   a u CT-FIND ;
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
: SGBAD-BAREPTR! ( ptr u8 n -- )
   SGBAD-BAREPTR-KIND SGBAD-SET ;
: SGBAD-BAREPTR? ( -- bool )
   SGBAD @ SGBAD-KIND @ SGBAD-BAREPTR-KIND = and ;

: BAD-SIG-TYPE ( ptr u8 n -- type )
   SGBAD-UNKNOWN!
   1 MK-CON ;
: SIG-PREFIX? {: a u p v :}
   u v < IF 0 EXIT THEN
   a v p v CORE-STR= ;
: ATOM-TOK? {: a u :}
   a u s" space-" SIG-PREFIX? IF -1 EXIT THEN
   a u s" extent-" SIG-PREFIX? IF -1 EXIT THEN
   a u s" mask-" SIG-PREFIX? IF -1 EXIT THEN
   a u s" block-" SIG-PREFIX? IF -1 EXIT THEN
   a u s" align-" SIG-PREFIX? ;
: FRESH-ATOM-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" fresh-extent-" SIG-PREFIX? IF -1 EXIT THEN
   a u s" fresh-mask-" SIG-PREFIX? ;
: FRESH-ATOM>TYPE ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u FAM-MARK FAM-K !
   a 6 + u 6 - FAM-K @ MK-ATOM-K ;
: PARAM-CTOR? {: a u :}
   a u s" ptr" CORE-STR= IF -1 EXIT THEN
   a u s" span" CORE-STR= IF -1 EXIT THEN
   a u s" matrix" CORE-STR= IF -1 EXIT THEN
   a u s" gridctx" CORE-STR= IF -1 EXIT THEN
   a u s" coopctx" CORE-STR= IF -1 EXIT THEN
   a u s" rowctx" CORE-STR= IF -1 EXIT THEN
   a u s" tile" CORE-STR= IF -1 EXIT THEN
   a u s" acc" CORE-STR= IF -1 EXIT THEN
   a u s" mmctx" CORE-STR= IF -1 EXIT THEN
   a u s" mmacc" CORE-STR= IF -1 EXIT THEN
   a u s" uniform" CORE-STR= IF -1 EXIT THEN
   a u s" rowidx" CORE-STR= ;
: TYPE-VAR-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 1 = IF a c@ LOWER? EXIT THEN
   0 ;
: TYPE-BAD-CHAR? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup u < while
      a over + c@ dup 60 = swap dup 62 = swap 44 = or or IF drop -1 EXIT THEN
      1+
   repeat drop 0 ;
: TYPE-RESERVED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= IF -1 EXIT THEN
   a u CT-FIND 0 <> IF -1 EXIT THEN
   a u PARAM-CTOR? IF -1 EXIT THEN
   a u ATOM-TOK? IF -1 EXIT THEN
   a u FRESH-ATOM-TOK? IF -1 EXIT THEN
   a u TYPE-VAR-TOK? IF -1 EXIT THEN
   a u TYPE-BAD-CHAR? ;
: CT-ADD-NOMINAL ( ptr u8 n -- ) {: a:ptr u:n :}
   a u TYPE-RESERVED? IF s" checker: bad or duplicate signature type" 70 die THEN
   a u CTN @ CT-ROLE 64 CS-NONE CT-SET ;
: TOK-TYPE {: a u :}  a c@ {: c :}
   u 1 = c 110 = and IF 1 MK-CON ELSE          \ 'n' -> generic int (con 1)
   u 1 = c 102 = and IF CC-BOOL MK-CON ELSE     \ 'f' -> bool (a comparison result is a flag, not an int)
   u 1 = c 114 = and IF 3 MK-CON ELSE          \ 'r' -> real/float (con 3)
   a u CON-OF dup IF MK-CON ELSE drop          \ i64/u8/u32/cell/char/str/addr/bool
   a u FRESH-ATOM-TOK? IF a u FRESH-ATOM>TYPE ELSE
   a u ATOM-TOK? IF a u MK-ATOM ELSE
   u 1 = c LOWER? and IF c VAR-OF ELSE          \ single letter -> type var
   a u BAD-SIG-TYPE THEN THEN THEN THEN THEN THEN THEN ;

: LOCAL-TYPE {: a u :}
   a u s" ptr" CORE-STR= IF FRESH MK-VAR MK-PTR ELSE a u TOK-TYPE THEN ;

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
   a u s" --" CORE-STR= IF -1 EXIT THEN
   a u s" ]"  CORE-STR= IF -1 EXIT THEN
   a u s" |"  CORE-STR= ;

: SIG-TYPE {: a u :}
   a u PARAM-CTOR? IF
      NEXT-SIG-TOK 2dup s" <" CORE-STR= IF
         2drop PARAM-SCR-RESET
         BEGIN
            NEXT-SIG-TOK 2dup s" >" CORE-STR= IF
               2drop a u MK-PARAM EXIT
            THEN
            2dup DELIM? IF SGBAD-SYNTAX! a u MK-PARAM EXIT THEN
            PARAM-SCR-FULL? IF SGBAD-SYNTAX! a u MK-PARAM EXIT THEN
            RECURSE PARAM-SCR+
            NEXT-SIG-TOK 2dup s" ," CORE-STR= IF 2drop ELSE
            2dup s" >" CORE-STR= IF 2drop a u MK-PARAM EXIT ELSE
               SGBAD-SYNTAX! a u MK-PARAM EXIT
            THEN THEN
         AGAIN
      ELSE
         PK!
      THEN
   THEN
   a u s" ptr" CORE-STR= IF
      NEXT-SIG-TOK 2dup DELIM? IF a u SGBAD-BAREPTR! PK! 1 MK-CON ELSE RECURSE MK-PTR THEN
   ELSE a u TOK-TYPE THEN ;

create ROWMAP 26 cells allot
: ROWMAP-RESET 0 BEGIN dup cells ROWMAP + UNBOUND swap ! 1 + dup 25 > UNTIL drop ;
: RVAR-OF {: c :}  c 65 - cells ROWMAP +  dup @ UNBOUND = IF FRESH over ! THEN  @ MK-ROW ;

\ SGBAD: the declared signature is malformed (a required '--'/']' delimiter was
\ missing or wrong). A malformed contract must REJECT, never silently parse as
\ some other effect. EXPECT-SIG consumes the next sig token and fails closed if
\ it is not the expected delimiter (EOF reads as a 0-length token -> mismatch).
: EXPECT-SIG {: ea eu :}
   NEXT-SIG-TOK 2dup ea eu CORE-STR= IF 2drop ELSE SGBAD-SYNTAX! THEN ;

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
     2dup s" [" CORE-STR= IF
        2drop
        FRESH MK-ROW                                  \ q data row
        FRESH MK-ROW                                  \ q return row
        over RECURSE                                  \ row qd qr qin
        s" --" EXPECT-SIG
        >r >r                                         \ park qin qr
        RECURSE                                       \ row qout
        r>
        NEXT-SIG-TOK 2dup s" |" CORE-STR= IF
           2drop
           dup RECURSE                                \ row qout qr qrin
           s" --" EXPECT-SIG
           >r dup RECURSE                             \ row qout qr qrout
           s" ]" EXPECT-SIG
           swap drop                                  \ row qout qrout
           r> r> 2swap >r rot r>                      \ row qin qout qrin qrout
        ELSE
           2dup s" ]" CORE-STR= IF
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
   NEXT-SIG-TOK 2dup s" |" CORE-STR= IF
      2drop  -1 SGHASR !  RRTAIL PSTACK           \ ( drow rrow ) explicit return
   ELSE PK! RR-SHARED @ THEN ;                    \ no | here -> shared tail (untouched)

\ PSIG ( -- din dout rin rout ) : data + return rows over the cursor.
: PSIG
   PKRESET NMAP-RESET ROWMAP-RESET FAM-RESET  0 SGHASR !  0 RR-SHARED !
   FRESH MK-ROW dup PD-BASE ! {: dr :}
   dr PSIDE  PR-IN ! PD-IN !
   s" --" EXPECT-SIG                              \ require the top-level '--'
   dr PSIDE  PR-OUT ! PD-OUT !
   PD-IN @ PD-OUT @ PR-IN @ PR-OUT @ ;

\ PARSE-SIG-RAW ( a u -- din dout rin rout ) : the declared effect as four rows
\ (no CHECKER-STEP), for verifying a definition's body against its own ( in -- out ).
: PARSE-SIG-RAW {: a u :}  a SB ! u SL ! 0 SI !  PSIG ;

\ Structured internal effects. Textual signatures are source-boundary input
\ only; checker-owned token semantics construct rows directly.
: STEP-TYPE-OUT ( n -- ) {: t:n :}
   FRESH MK-ROW {: rest:n :}
   rest
   t rest MK-PUSH
   CHECKER-STEP ;

: STEP-TYPE-IN ( n -- ) {: t:n :}
   FRESH MK-ROW {: rest:n :}
   t rest MK-PUSH
   rest CHECKER-STEP ;

: STEP-TYPE2-IN ( n n -- ) {: a:n b:n :}
   FRESH MK-ROW {: rest:n :}
   a rest MK-PUSH
   b swap MK-PUSH
   rest CHECKER-STEP ;

: STEP-N-IN ( -- )
   CC-N MK-CON STEP-TYPE-IN ;

: STEP-N-OUT ( -- )
   CC-N MK-CON STEP-TYPE-OUT ;

: STEP-R-OUT ( -- )
   CC-R MK-CON STEP-TYPE-OUT ;

: STEP-BOOL-IN ( -- )
   CC-BOOL MK-CON STEP-TYPE-IN ;

: STEP-NN-IN ( -- )
   CC-N MK-CON CC-N MK-CON STEP-TYPE2-IN ;

: STEP-FETCH ( -- )
   FRESH MK-VAR FRESH MK-ROW {: t:n rest:n :}
   t MK-PTR rest MK-PUSH
   t rest MK-PUSH
   CHECKER-STEP ;

: STEP-STORE ( -- )
   FRESH MK-VAR FRESH MK-ROW {: t:n rest:n :}
   t rest MK-PUSH
   t MK-PTR swap MK-PUSH
   rest CHECKER-STEP ;

variable FP
\ user sigs: certified words recorded as effect records after the structural
\ primitive-effect prefix. The renderer appends user records so later wins.
\ The baked checker image stores canonical typed effect graphs for certified
\ words, not rendered signature strings. The static boot arena must hold that
\ snapshot without relying on process-local mmap state.
$800000 constant USIGS-INIT-CAP
$10000 constant USIGS-GRAIN
$7FFFFFFFFFFFFFFF constant USIGS-MAX-CAP
3 constant USIGS-PROT-RW
$1002 constant USIGS-MAP-ANON
-1 constant USIGS-ANON-FD
0 constant USIGS-OFF-ZERO
variable USIGS-P   variable USIGS-CAP-U   variable UEND
variable USIGS-USER-OFF
variable USIGS-GROW-CAP   variable USIGS-GROW-NEXT
variable CHK-CAND
PTR-VARIABLE USIGS-SNAP-P

: USIGS ( -- ptr u8 ) USIGS-P @ ;

0 USIGS-USER-OFF !
0 CHK-CAND !

: USIGS-COPY {: src:ptr dst:ptr n :}
   n 0 > IF n 0 DO src i + c@ dst i + c! LOOP THEN ;

: USIGS-ROUND-CAP {: need :}
   need 0 <= IF s" checker: bad user sig cap" 76 die THEN
   need USIGS-MAX-CAP USIGS-GRAIN - > IF s" checker: user sigs too large" 76 die THEN
   need 1 - USIGS-GRAIN / 1 + USIGS-GRAIN * ;

: USIGS-ALLOC {: cap :}
   0 cap USIGS-PROT-RW USIGS-MAP-ANON USIGS-ANON-FD USIGS-OFF-ZERO mmap
   dup 0 < IF s" checker: user sigs mmap failed" 76 die THEN ;

: USIGS-CLEAR ( -- )
   0 UEND !
   0 USIGS !
   0 USIGS-GROW-CAP !
   0 USIGS-GROW-NEXT ! ;

: USIGS-ALLOC-INIT ( -- )
   USIGS-INIT-CAP USIGS-ALLOC USIGS-P !
   USIGS-INIT-CAP USIGS-CAP-U ! ;

: USIGS-RUNTIME-INIT ( -- )
   USIGS-ALLOC-INIT
   USIGS-CLEAR ;

USIGS-RUNTIME-INIT

: USIGS-RUNTIME-SIZED? ( -- bool )
   USIGS-P @ 0 = 0=
   USIGS-CAP-U @ USIGS-INIT-CAP >= and ;

: USIGS-RESET ( -- )
   USIGS-RUNTIME-SIZED? 0= IF USIGS-ALLOC-INIT THEN
   USIGS-CLEAR
   0 USIGS-USER-OFF ! ;

: USIGS-SNAP@ ( -- ptr u8 )
   USIGS-SNAP-P @ ;

: USIGS-SNAPSHOT-SIZE ( -- n )
   UEND @ cell+ ;

: USIGS-SNAPSHOT-ALLOC ( n -- ptr u8 ) {: n:n :}
   here USIGS-SNAP-P !
   n allot
   USIGS-SNAP@ ;

: USIGS-SNAPSHOT-PERSIST ( -- )
   USIGS-SNAPSHOT-SIZE {: n:n :}
   n USIGS-SNAPSHOT-ALLOC {: dst:ptr :}
   USIGS dst n USIGS-COPY
   dst USIGS-P !
   n USIGS-CAP-U !
   0 USIGS-GROW-CAP !
   0 USIGS-GROW-NEXT ! ;

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

: USIGS-RESTORE-END ( n -- )
   UEND !
   UTERM! ;

: USIGS-USER ( -- ptr a )
   USIGS USIGS-USER-OFF @ + ;

: SYM-FOLD-C ( n -- n ) {: c:n :}
   c $41 < if c exit then
   c $5A > if c exit then
   c $20 or ;

: SYM-STR=CI ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v <> if 0 exit then
   0 begin dup u < while
      dup a + c@ SYM-FOLD-C
      over b + c@ SYM-FOLD-C <> if drop 0 exit then
      1+
   repeat drop
   0 0= ;

$4000 constant SYM-CAP
$100000 constant SYM-STR-CAP
0 constant SYM-GLOBAL
1 constant SYM-PRIVATE
2 constant SYM-PUBLIC

BEGIN-STRUCTURE SYM-REC
   PTR-FIELD: SYM.PKG-A
   CELL +FIELD SYM.PKG-U
   PTR-FIELD: SYM.NAME-A
   CELL +FIELD SYM.NAME-U
   CELL +FIELD SYM.VIS
END-STRUCTURE

create SYMS SYM-CAP SYM-REC * allot
create SYM-STR SYM-STR-CAP allot
variable SYM-N
variable SYM-STR-U
variable SYM-I
variable SYM-DST
variable SYM-ID

1 SYM-N !
0 SYM-STR-U !

: SYM-ROW ( n -- ptr a )
   SYM-REC * SYMS + ;

: SYM-PKG-A-FIELD ( n -- ptr ptr a )
   SYM-ROW SYM.PKG-A ;

: SYM-NAME-A-FIELD ( n -- ptr ptr a )
   SYM-ROW SYM.NAME-A ;

: SYM-DST-FIELD ( -- ptr ptr u8 )
   SYM-DST 0 ptr-field ;

: SYM-DST@ ( -- ptr u8 )
   SYM-DST-FIELD @ ;

: SYM-DST! ( ptr u8 -- )
   SYM-DST-FIELD ! ;

: SYM-PKG$ ( n -- ptr u8 n )
   dup SYM-PKG-A-FIELD @
   swap SYM-ROW SYM.PKG-U @ ;

: SYM-NAME$ ( n -- ptr u8 n )
   dup SYM-NAME-A-FIELD @
   swap SYM-ROW SYM.NAME-U @ ;

: SYM-STR-NEED ( n -- )
   SYM-STR-U @ + SYM-STR-CAP > IF s" checker: symbol strings full" 76 die THEN ;

: SYM-COPY-FOLD ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u SYM-STR-NEED
   SYM-STR SYM-STR-U @ + SYM-DST!
   0 SYM-I !
   begin SYM-I @ u < while
      a SYM-I @ + c@ SYM-FOLD-C SYM-DST@ SYM-I @ + c!
      SYM-I @ 1 + SYM-I !
   repeat
   SYM-STR-U @ u + SYM-STR-U !
   SYM-DST@ u ;

: SYM-MATCH? ( ptr u8 n n ptr u8 n n -- bool ) {: pkg:ptr pkgu:n vis:n name:ptr nameu:n id:n :}
   id SYM-ROW SYM.VIS @ vis <> IF 0 EXIT THEN
   id SYM-PKG$ pkg pkgu SYM-STR=CI 0= IF 0 EXIT THEN
   id SYM-NAME$ name nameu SYM-STR=CI ;

: SYM-FIND ( ptr u8 n n ptr u8 n -- n bool ) {: pkg:ptr pkgu:n vis:n name:ptr nameu:n :}
   1 SYM-I !
   begin SYM-I @ SYM-N @ < while
      pkg pkgu vis name nameu SYM-I @ SYM-MATCH? IF SYM-I @ -1 EXIT THEN
      SYM-I @ 1 + SYM-I !
   repeat
   0 0 ;

: SYM-PKG! ( ptr u8 n n -- ) {: a:ptr u:n id:n :}
   a u SYM-COPY-FOLD {: dst:ptr len:n :}
   dst id SYM-PKG-A-FIELD !
   len id SYM-ROW SYM.PKG-U ! ;

: SYM-NAME! ( ptr u8 n n -- ) {: a:ptr u:n id:n :}
   a u SYM-COPY-FOLD {: dst:ptr len:n :}
   dst id SYM-NAME-A-FIELD !
   len id SYM-ROW SYM.NAME-U ! ;

: SYM-SET ( ptr u8 n n ptr u8 n n -- ) {: pkg:ptr pkgu:n vis:n name:ptr nameu:n id:n :}
   pkg pkgu id SYM-PKG!
   name nameu id SYM-NAME!
   vis id SYM-ROW SYM.VIS ! ;

: SYM-INTERN ( ptr u8 n n ptr u8 n -- n ) {: pkg:ptr pkgu:n vis:n name:ptr nameu:n :}
   pkg pkgu vis name nameu SYM-FIND IF EXIT THEN drop
   SYM-N @ SYM-CAP >= IF s" checker: symbol table full" 76 die THEN
   SYM-N @ SYM-ID !
   pkg pkgu vis name nameu SYM-ID @ SYM-SET
   SYM-ID @ 1 + SYM-N !
   SYM-ID @ ;

\ checker-registry.f - typed checker effect store.
\
\ Loaded from checker.f after the signature parser and before callers need
\ certified word lookup. Source strings are parsed once at boundary adapters;
\ callers instantiate the stored effect graph.

0 constant EFF-DELETED
1 constant EFF-ACTIVE

0 constant EN-CON
1 constant EN-VAR
2 constant EN-ROW
3 constant EN-PTR
4 constant EN-PUSH
5 constant EN-QUOT
6 constant EN-ATOM
7 constant EN-PARAM

BEGIN-STRUCTURE EFF-REC
   CELL +FIELD ER.NEXT
   CELL +FIELD ER.ACTIVE
   CELL +FIELD ER.DIN
   CELL +FIELD ER.DOUT
   CELL +FIELD ER.RIN
   CELL +FIELD ER.ROUT
   CELL +FIELD ER.HASR
   CELL +FIELD ER.TVN
   CELL +FIELD ER.RVN
   CELL +FIELD ER.SYM
END-STRUCTURE

BEGIN-STRUCTURE EFF-NODE
   CELL +FIELD EN.TAG
   CELL +FIELD EN.A
   CELL +FIELD EN.B
   CELL +FIELD EN.C
   CELL +FIELD EN.D
   CELL +FIELD EN.E
   CELL +FIELD EN.F
   CELL +FIELD EN.G
   CELL +FIELD EN.H
END-STRUCTURE

create EC-TV MAXTV cells allot
create EC-RV MAXTV cells allot
variable EC-TVN
variable EC-RVN

create EI-TV MAXTV cells allot
create EI-RV MAXTV cells allot
64 constant EI-AK-CAP
create EI-AK EI-AK-CAP cells allot

variable FEP
variable CHECKER-REC-SYM
0 CHECKER-REC-SYM !

: E-MAP-RESET-ONE ( ptr a -- ) {: p:ptr :}
   0 begin dup MAXTV < while
      UNBOUND over cells p + !
      1 +
   repeat drop ;

: E-COPY-MAPS-RESET ( -- )
   EC-TV E-MAP-RESET-ONE
   EC-RV E-MAP-RESET-ONE
   0 EC-TVN !
   0 EC-RVN ! ;

: E-I-AK-RESET ( -- )
   0 begin dup EI-AK-CAP < while
      UNBOUND over cells EI-AK + !
      1 +
   repeat drop ;

: E-TV-ID ( n -- n ) {: id:n :}
   id cells EC-TV + dup @ UNBOUND = if
      EC-TVN @ over !
      EC-TVN @ 1+ EC-TVN !
   then @ ;

: E-RV-ID ( n -- n ) {: id:n :}
   id cells EC-RV + dup @ UNBOUND = if
      EC-RVN @ over !
      EC-RVN @ 1+ EC-RVN !
   then @ ;

: E-OFF ( ptr a -- n )
   USIGS - ;

: E-PTR ( n -- ptr a )
   USIGS + ;

: E-ENSURE-NODE ( -- )
   UEND @ EFF-NODE + cell+ USIGS-ENSURE ;

: E-NODE-NEW ( n -- ptr a ) {: tag:n :}
   E-ENSURE-NODE
   USIGS UEND @ + >r
   tag r@ EN.TAG !
   0 r@ EN.A !  0 r@ EN.B !  0 r@ EN.C !  0 r@ EN.D !
   0 r@ EN.E !  0 r@ EN.F !  0 r@ EN.G !  0 r@ EN.H !
   UEND @ EFF-NODE + UEND !
   r> ;

: E-NODE-OFF ( n -- n )
   E-NODE-NEW E-OFF ;

: E-COPY-STR ( ptr u8 n ptr a -- ) {: a:ptr u:n p:ptr :}
   UEND @ p EN.A !
   u p EN.B !
   UEND @ u + UALIGN cell+ USIGS-ENSURE
   a u UBS
   UALIGN! ;

: E-RES ( n -- n ) {: x:n :}
   x TAG S-ROW = x TAG S-PUSH = or if x R-RES else x T-RES then ;

: E-COPY ( n -- n ) {: x:n :}
   x 0= if 0 exit then
   x E-RES TAG case
      T-CON of
         EN-CON E-NODE-NEW E-OFF >r
         x E-RES PAY r@ E-PTR EN.A !
         r>
      endof
      T-VAR of
         EN-VAR E-NODE-NEW E-OFF >r
         x E-RES PAY E-TV-ID r@ E-PTR EN.A !
         r>
      endof
      S-ROW of
         EN-ROW E-NODE-NEW E-OFF >r
         x E-RES PAY E-RV-ID r@ E-PTR EN.A !
         r>
      endof
      T-PTR of
         EN-PTR E-NODE-NEW E-OFF >r
         x E-RES PTR>INNER RECURSE r@ E-PTR EN.A !
         r>
      endof
      S-PUSH of
         EN-PUSH E-NODE-NEW E-OFF >r
         x E-RES P>TYPE RECURSE r@ E-PTR EN.A !
         x E-RES P>REST RECURSE r@ E-PTR EN.B !
         r>
      endof
      T-QUOT of
         EN-QUOT E-NODE-NEW E-OFF >r
         x E-RES Q>DIN RECURSE r@ E-PTR EN.A !
         x E-RES Q>DOUT RECURSE r@ E-PTR EN.B !
         x E-RES Q>RIN RECURSE r@ E-PTR EN.C !
         x E-RES Q>ROUT RECURSE r@ E-PTR EN.D !
         x E-RES Q>XHAS r@ E-PTR EN.E !
         x E-RES Q>XDEAD r@ E-PTR EN.F !
         x E-RES Q>XDOUT r@ E-PTR EN.G !
         x E-RES Q>XROUT r@ E-PTR EN.H !
         r>
      endof
      T-ATOM of
         EN-ATOM E-NODE-NEW E-OFF >r
         x E-RES ATOM>A x E-RES ATOM>U r@ E-PTR E-COPY-STR
         x E-RES ATOM>K r@ E-PTR EN.C !
         r>
      endof
      T-PARAM of
         EN-PARAM E-NODE-NEW E-OFF >r
         x E-RES PARAM>NAME-A x E-RES PARAM>NAME-U r@ E-PTR E-COPY-STR
         x E-RES PARAM>ARGC r@ E-PTR EN.C !
         x E-RES PARAM>ARGC 0 > if x E-RES 0 PARAM>ARG RECURSE r@ E-PTR EN.D ! then
         x E-RES PARAM>ARGC 1 > if x E-RES 1 PARAM>ARG RECURSE r@ E-PTR EN.E ! then
         x E-RES PARAM>ARGC 2 > if x E-RES 2 PARAM>ARG RECURSE r@ E-PTR EN.F ! then
         x E-RES PARAM>ARGC 3 > if x E-RES 3 PARAM>ARG RECURSE r@ E-PTR EN.G ! then
         r>
      endof
      0 swap
   endcase ;

: USIG-NEXT ( ptr a -- ptr a )
   ER.NEXT @ E-PTR ;

: USIG-OFF ( ptr a -- n )
   E-OFF ;

: USIG-END? ( ptr a -- bool )
   @ 0= ;

: E-REC-START ( -- ptr a )
   UEND @ EFF-REC + cell+ USIGS-ENSURE
   USIGS UEND @ + >r
   0 r@ ER.NEXT !  0 r@ ER.ACTIVE !
   0 r@ ER.DIN !   0 r@ ER.DOUT !  0 r@ ER.RIN !  0 r@ ER.ROUT !
   0 r@ ER.HASR !  0 r@ ER.TVN !   0 r@ ER.RVN !
   CHECKER-REC-SYM @ r@ ER.SYM !
   r@ EFF-REC + USIGS - UEND !
   r> ;

: E-REC-FINISH ( ptr a -- )
   UEND @ swap ER.NEXT !
   UTERM! ;

: E-BUILD-EFFECT ( n n n n n -- n ) {: din:n dout:n rin:n rout:n hasr:n :}
   E-REC-START E-OFF >r
   E-COPY-MAPS-RESET
   EFF-ACTIVE r@ E-PTR ER.ACTIVE !
   din E-COPY r@ E-PTR ER.DIN !
   dout E-COPY r@ E-PTR ER.DOUT !
   hasr if
      rin E-COPY r@ E-PTR ER.RIN !
      rout E-COPY r@ E-PTR ER.ROUT !
   then
   hasr r@ E-PTR ER.HASR !
   EC-TVN @ r@ E-PTR ER.TVN !
   EC-RVN @ r@ E-PTR ER.RVN !
   r@ E-PTR E-REC-FINISH
   r> ;

: E-ADD-EFFECT ( n n n n n -- )
   E-BUILD-EFFECT drop ;

: E-ADD-DELETED ( -- )
   E-REC-START E-OFF >r
   EFF-DELETED r@ E-PTR ER.ACTIVE !
   r> E-PTR E-REC-FINISH ;

: E-PARSE-ADD ( ptr u8 n -- ) {: sa:ptr su:n :}
   NEW
   SGBAD-CLEAR
   sa su PARSE-SIG-RAW
   SGBAD @ if s" checker: bad stored signature" 76 die then
   SGHASR @ E-ADD-EFFECT ;

: USIG-ADD ( ptr u8 n ptr u8 n -- )
   2drop E-PARSE-ADD ;

: USIG-DELETE ( ptr u8 n -- )
   2drop E-ADD-DELETED ;

: USIG-SYM@ ( ptr a -- n )
   ER.SYM @ ;

: USIG-MATCH-SYM? ( ptr a n -- bool ) {: rec:ptr sym:n :}
   rec USIG-SYM@ sym = ;

: USIG-FIND-OFF-SYM-REC ( ptr a n -- n bool ) {: rec:ptr sym:n :}
   rec USIG-END? if 0 0 exit then
   rec sym USIG-MATCH-SYM? if rec USIG-OFF -1 exit then
   rec USIG-NEXT sym recurse ;

: USIG-FIND-OFF-SYM ( n -- n bool ) {: sym:n :}
   sym 0= if 0 0 exit then
   USIGS-USER sym USIG-FIND-OFF-SYM-REC ;

: SCAN-USIGS-SYM {: sym:n :}
   0 FEP !
   USIGS-USER FP !
   begin FP @ USIG-END? 0= while
      FP @ sym USIG-MATCH-SYM? if
         FP @ dup ER.ACTIVE @ if FEP ! else drop 0 FEP ! then
      then
      FP @ USIG-NEXT FP !
   repeat ;

: E-INST-RESET ( ptr a -- ) {: h:ptr :}
   E-I-AK-RESET
   0 begin dup h ER.TVN @ < while
      UNBOUND over cells EI-TV + !
      1 +
   repeat drop
   0 begin dup h ER.RVN @ < while
      UNBOUND over cells EI-RV + !
      1 +
   repeat drop ;

: E-I-TV ( n -- n ) {: id:n :}
   id cells EI-TV + dup @ UNBOUND = if
      FRESH MK-VAR over !
   then @ ;

: E-I-RV ( n -- n ) {: id:n :}
   id cells EI-RV + dup @ UNBOUND = if
      FRESH MK-ROW over !
   then @ ;

: E-I-AK-IDX ( n -- n )
   negate 1 - ;

: E-I-AK ( n -- n ) {: k:n :}
   k 0 >= if k exit then
   k E-I-AK-IDX dup EI-AK-CAP >= if s" checker: fresh atom inst table full" 76 die then
   cells EI-AK + dup @ UNBOUND = if
      RIGID-FRESH over !
   then @ ;

: E-I-STR ( ptr a -- ptr u8 n )
   dup EN.A @ E-PTR swap EN.B @ ;

: E-INST ( n -- n ) {: off:n :}
   off 0= if 0 exit then
   off E-PTR >r
   r@ EN.TAG @ case
      EN-CON of r@ EN.A @ MK-CON r> drop endof
      EN-VAR of r@ EN.A @ E-I-TV r> drop endof
      EN-ROW of r@ EN.A @ E-I-RV r> drop endof
      EN-PTR of r@ EN.A @ RECURSE MK-PTR r> drop endof
      EN-PUSH of r@ EN.A @ RECURSE r@ EN.B @ RECURSE MK-PUSH r> drop endof
      EN-QUOT of
         r@ EN.A @ RECURSE
         r@ EN.B @ RECURSE
         r@ EN.C @ RECURSE
         r@ EN.D @ RECURSE
         MK-QUOT
         dup r@ EN.E @ r@ EN.F @ r@ EN.G @ r@ EN.H @ QX!
         r> drop
      endof
      EN-ATOM of r@ E-I-STR r@ EN.C @ E-I-AK MK-ATOM-K r> drop endof
      EN-PARAM of
         PARAM-SCR-RESET
         r@ EN.C @ 0 > if r@ EN.D @ RECURSE PARAM-SCR+ then
         r@ EN.C @ 1 > if r@ EN.E @ RECURSE PARAM-SCR+ then
         r@ EN.C @ 2 > if r@ EN.F @ RECURSE PARAM-SCR+ then
         r@ EN.C @ 3 > if r@ EN.G @ RECURSE PARAM-SCR+ then
         r@ E-I-STR MK-PARAM r> drop
      endof
      r> drop 0 swap
   endcase ;

: EFF-APPLY ( ptr a -- ) {: h:ptr :}
   h E-INST-RESET
   h ER.DIN @ E-INST
   h ER.DOUT @ E-INST
   CHECKER-STEP
   h ER.HASR @ if
      RCUR @ h ER.RIN @ E-INST UNIFY-IN OK @ and OK !
      h ER.ROUT @ E-INST RCUR !
   then ;

: EFF-QUOT ( ptr a -- n ) {: h:ptr :}
   h E-INST-RESET
   h ER.HASR @ if
      h ER.DIN @ E-INST
      h ER.DOUT @ E-INST
      h ER.RIN @ E-INST
      h ER.ROUT @ E-INST
   else
      h ER.DIN @ E-INST
      h ER.DOUT @ E-INST
      FRESH MK-ROW dup
   then
   MK-QUOT ;

256 constant PE-CAP
1 constant PE-ACTIVE

BEGIN-STRUCTURE PE-REC
   CELL +FIELD PE.SYM
   CELL +FIELD PE.EFF
   CELL +FIELD PE.FLAGS
END-STRUCTURE

create PES PE-CAP PE-REC * allot
variable #PE
variable PE-I
create SDQN 2 allot   115 SDQN c!   34 SDQN 1 + c!    \ the two chars of `s"`
create CDQN 2 allot    99 CDQN c!   34 CDQN 1 + c!    \ the two chars of `c"`
create DOTQN 2 allot   46 DOTQN c!  34 DOTQN 1 + c!   \ the two chars of `."`

: PE-ROW ( n -- ptr a )
   PE-REC * PES + ;

: PE-SYM@ ( n -- n )
   PE-ROW PE.SYM @ ;

: PE-EFF@ ( n -- n )
   PE-ROW PE.EFF @ ;

: PE-FLAGS@ ( n -- n )
   PE-ROW PE.FLAGS @ ;

: PE-ACTIVE? ( n -- bool )
   PE-FLAGS@ PE-ACTIVE and 0 <> ;

: PRIM-CHECK-CAP ( -- )
   #PE @ PE-CAP >= IF s" checker: prim table full" 76 die THEN ;

: PRIM-ADD ( n n n -- ) {: sym:n eff:n flags:n :}
   PRIM-CHECK-CAP
   sym #PE @ PE-ROW PE.SYM !
   eff #PE @ PE-ROW PE.EFF !
   flags #PE @ PE-ROW PE.FLAGS !
   #PE @ 1 + #PE ! ;

: PRIM-FIRST-SYM ( n -- n ) {: sym:n :}
   0 PE-I !
   begin PE-I @ #PE @ < while
      PE-I @ PE-ACTIVE? IF
         PE-I @ PE-SYM@ sym = IF PE-I @ PE-EFF@ EXIT THEN
      THEN
      PE-I @ 1 + PE-I !
   repeat
   0 ;

: PE-SYM-OF ( ptr u8 n -- n ) {: a:ptr u:n :}
   s" " SYM-GLOBAL a u SYM-INTERN ;

variable PE-NA
variable PE-NU
variable PE-BASE
variable PE-DIN
variable PE-DOUT
variable PE-RIN
variable PE-ROUT
variable PE-HASR
variable PE-SYM-ID
variable PE-EFF-ID

: PE-NA@ ( -- ptr u8 )
   PE-NA 0 ptr-field @ ;

: PE-NA! ( ptr u8 -- )
   PE-NA 0 ptr-field ! ;

: PE-OPEN ( ptr u8 n -- ) {: a:ptr u:n :}
   a PE-NA!  u PE-NU !
   NEW
   NMAP-RESET
   ROWMAP-RESET
   SGBAD-CLEAR
   FRESH MK-ROW dup PE-BASE ! dup PE-DIN ! PE-DOUT !
   0 PE-RIN !  0 PE-ROUT !  0 PE-HASR ! ;

: PRIM: ( -- )
   parse-name PE-OPEN ;

: PE-CLOSE ( -- )
   PE-NA@ PE-NU @ PE-SYM-OF PE-SYM-ID !
   PE-SYM-ID @ CHECKER-REC-SYM !
   PE-DIN @ PE-DOUT @ PE-RIN @ PE-ROUT @ PE-HASR @
   E-BUILD-EFFECT PE-EFF-ID !
   PE-SYM-ID @ PE-EFF-ID @ PE-ACTIVE PRIM-ADD ;

: PRIM; ( -- )
   PE-CLOSE ;

: PE-IN ( n -- )
   PE-DIN @ MK-PUSH PE-DIN ! ;

: PE-OUT ( n -- )
   PE-DOUT @ MK-PUSH PE-DOUT ! ;

: PE-A ( -- n ) $61 VAR-OF ;
: PE-B ( -- n ) $62 VAR-OF ;
: PE-C ( -- n ) $63 VAR-OF ;
: PE-D ( -- n ) $64 VAR-OF ;
: PE-N ( -- n ) CC-N MK-CON ;
: PE-F ( -- n ) CC-BOOL MK-CON ;
: PE-R ( -- n ) CC-R MK-CON ;
: PE-U8 ( -- n ) CC-U8 MK-CON ;
: PE-PTR ( n -- n ) MK-PTR ;
: PE-PTR-A ( -- n ) PE-A PE-PTR ;
: PE-PTR-B ( -- n ) PE-B PE-PTR ;
: PE-PTR-C ( -- n ) PE-C PE-PTR ;
: PE-PTR-N ( -- n ) PE-N PE-PTR ;
: PE-PTR-U8 ( -- n ) PE-U8 PE-PTR ;
: PE-PTR-PTR-B ( -- n ) PE-B PE-PTR PE-PTR ;

: PTABLE-START ( -- )
   0 #PE !
   0 UEND !
   UTERM! ;

: PTABLE-END ( -- )
   UEND @ USIGS-USER-OFF !
   UTERM! ;

PTABLE-START

PRIM: dup   PE-A PE-IN  PE-A PE-OUT PE-A PE-OUT PRIM;
PRIM: drop  PE-A PE-IN PRIM;
PRIM: swap  PE-A PE-IN PE-B PE-IN  PE-B PE-OUT PE-A PE-OUT PRIM;
PRIM: over  PE-A PE-IN PE-B PE-IN  PE-A PE-OUT PE-B PE-OUT PE-A PE-OUT PRIM;
PRIM: nip   PE-A PE-IN PE-B PE-IN  PE-B PE-OUT PRIM;
PRIM: tuck  PE-A PE-IN PE-B PE-IN  PE-B PE-OUT PE-A PE-OUT PE-B PE-OUT PRIM;
PRIM: rot   PE-A PE-IN PE-B PE-IN PE-C PE-IN  PE-B PE-OUT PE-C PE-OUT PE-A PE-OUT PRIM;
PRIM: -rot  PE-A PE-IN PE-B PE-IN PE-C PE-IN  PE-C PE-OUT PE-A PE-OUT PE-B PE-OUT PRIM;
PRIM: 2dup  PE-A PE-IN PE-B PE-IN  PE-A PE-OUT PE-B PE-OUT PE-A PE-OUT PE-B PE-OUT PRIM;
PRIM: 2drop PE-A PE-IN PE-B PE-IN PRIM;
PRIM: 2swap PE-A PE-IN PE-B PE-IN PE-C PE-IN PE-D PE-IN
            PE-C PE-OUT PE-D PE-OUT PE-A PE-OUT PE-B PE-OUT PRIM;
PRIM: 2over PE-A PE-IN PE-B PE-IN PE-C PE-IN PE-D PE-IN
            PE-A PE-OUT PE-B PE-OUT PE-C PE-OUT PE-D PE-OUT PE-A PE-OUT PE-B PE-OUT PRIM;

PRIM: +      PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: +      PE-PTR-A PE-IN PE-N PE-IN  PE-PTR-A PE-OUT PRIM;
PRIM: +      PE-N PE-IN PE-PTR-A PE-IN  PE-PTR-A PE-OUT PRIM;
PRIM: -      PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: -      PE-PTR-A PE-IN PE-N PE-IN  PE-PTR-A PE-OUT PRIM;
PRIM: -      PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-N PE-OUT PRIM;
PRIM: *      PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: and    PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: and    PE-F PE-IN PE-F PE-IN  PE-F PE-OUT PRIM;
PRIM: or     PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: or     PE-F PE-IN PE-F PE-IN  PE-F PE-OUT PRIM;
PRIM: xor    PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: xor    PE-F PE-IN PE-F PE-IN  PE-F PE-OUT PRIM;
PRIM: 1+     PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: 1+     PE-PTR-A PE-IN  PE-PTR-A PE-OUT PRIM;
PRIM: 1-     PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: 1-     PE-PTR-A PE-IN  PE-PTR-A PE-OUT PRIM;
PRIM: negate PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: invert PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: 0=     PE-A PE-IN  PE-F PE-OUT PRIM;
PRIM: 0<     PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: =      PE-N PE-IN PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: =      PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT PRIM;
PRIM: <      PE-N PE-IN PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: <      PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT PRIM;
PRIM: >      PE-N PE-IN PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: >      PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT PRIM;
PRIM: <>     PE-N PE-IN PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: <>     PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT PRIM;
PRIM: <=     PE-N PE-IN PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: <=     PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT PRIM;
PRIM: >=     PE-N PE-IN PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: >=     PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT PRIM;
PRIM: /      PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: mod    PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: /mod   PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PE-N PE-OUT PRIM;
PRIM: abs    PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: min    PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: max    PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: lshift PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: rshift PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: cells  PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: cell+  PE-PTR-A PE-IN  PE-PTR-A PE-OUT PRIM;
PRIM: cell+  PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: chars  PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: char+  PE-PTR-A PE-IN  PE-PTR-A PE-OUT PRIM;
PRIM: char+  PE-N PE-IN  PE-N PE-OUT PRIM;

PRIM: @          PE-PTR-A PE-IN  PE-A PE-OUT PRIM;
PRIM: !          PE-A PE-IN PE-PTR-A PE-IN PRIM;
PRIM: ptr-field  PE-PTR-A PE-IN PE-N PE-IN  PE-PTR-PTR-B PE-OUT PRIM;
PRIM: +!         PE-N PE-IN PE-PTR-N PE-IN PRIM;
PRIM: c@         PE-PTR-U8 PE-IN  PE-U8 PE-OUT PRIM;
PRIM: c!         PE-U8 PE-IN PE-PTR-U8 PE-IN PRIM;
PRIM: atomic@    PE-PTR-A PE-IN  PE-A PE-OUT PRIM;
PRIM: atomic!    PE-A PE-IN PE-PTR-A PE-IN PRIM;
PRIM: atomic-add PE-N PE-IN PE-PTR-N PE-IN  PE-N PE-OUT PRIM;
PRIM: atomic-cas PE-A PE-IN PE-A PE-IN PE-PTR-A PE-IN  PE-A PE-OUT PRIM;
PRIM: fence      PRIM;
PRIM: run-in-stack PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: count      PE-PTR-U8 PE-IN  PE-PTR-U8 PE-OUT PE-N PE-OUT PRIM;

PRIM: .            PE-N PE-IN PRIM;
PRIM: .s           PRIM;
PRIM: depth        PE-N PE-OUT PRIM;
PRIM: here         PE-PTR-A PE-OUT PRIM;
PRIM: allot        PE-N PE-IN PRIM;
PRIM: ,            PE-N PE-IN PRIM;
PRIM: c,           PE-N PE-IN PRIM;
PRIM: type         PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: script-argc  PE-N PE-OUT PRIM;
PRIM: script-argv$ PE-N PE-IN  PE-PTR-U8 PE-OUT PE-N PE-OUT PRIM;
PRIM: throw        PE-N PE-IN PRIM;
PRIM: die          PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN PRIM;

PRIM: open     PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: read     PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: ioctl    PE-N PE-IN PE-N PE-IN PE-PTR-A PE-IN  PE-N PE-OUT PRIM;
PRIM: mmap     PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: path0    PE-PTR-U8 PE-IN PE-N PE-IN  PE-PTR-U8 PE-OUT PRIM;
PRIM: open-rd  PE-PTR-U8 PE-IN  PE-N PE-OUT PRIM;
PRIM: access   PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: unlink   PE-PTR-U8 PE-IN  PE-N PE-OUT PRIM;
PRIM: rename   PE-PTR-U8 PE-IN PE-PTR-U8 PE-IN  PE-N PE-OUT PRIM;
PRIM: chmod    PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: symlink  PE-PTR-U8 PE-IN PE-PTR-U8 PE-IN  PE-N PE-OUT PRIM;
PRIM: readlink PE-PTR-U8 PE-IN PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: mkdir    PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: rmdir    PE-PTR-U8 PE-IN  PE-N PE-OUT PRIM;
PRIM: stat64   PE-PTR-U8 PE-IN PE-PTR-U8 PE-IN  PE-N PE-OUT PRIM;
PRIM: lstat64  PE-PTR-U8 PE-IN PE-PTR-U8 PE-IN  PE-N PE-OUT PRIM;
PRIM: getdirentries64
   PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-N PE-IN  PE-N PE-OUT PRIM;
PRIM: pipe     PE-N PE-OUT PE-N PE-OUT PE-N PE-OUT PRIM;
PRIM: dup2     PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: fcntl    PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: poll     PE-PTR-A PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: kill     PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;

PRIM: spawn-io  PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: spawn-argv-io
   PE-PTR-U8 PE-IN PE-PTR-A PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: spawn-argv-env-io
   PE-PTR-U8 PE-IN PE-PTR-A PE-IN PE-PTR-A PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN
   PE-N PE-OUT PRIM;
PRIM: spawn-argv-env-cwd-io
   PE-PTR-U8 PE-IN PE-PTR-A PE-IN PE-PTR-A PE-IN PE-PTR-U8 PE-IN
   PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: fork          PE-N PE-OUT PRIM;
PRIM: wait-rc       PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: wait-status   PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: patch32       PE-N PE-IN PE-N PE-IN PRIM;
PRIM: write         PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: close         PE-N PE-IN PRIM;
PRIM: epoch-seconds PE-N PE-OUT PRIM;
PRIM: mono-ns       PE-N PE-OUT PRIM;
PRIM: prof-on       PE-N PE-IN PRIM;
PRIM: prof-report   PRIM;

PRIM: rbase          PE-N PE-OUT PRIM;
PRIM: cp@            PE-N PE-OUT PRIM;
PRIM: cp!            PE-N PE-IN PRIM;
PRIM: dbase@         PE-N PE-OUT PRIM;
PRIM: ndict@         PE-N PE-OUT PRIM;
PRIM: ndict!         PE-N PE-IN PRIM;
PRIM: data-base      PE-PTR-A PE-OUT PRIM;
PRIM: wordlist       PE-N PE-OUT PRIM;
PRIM: get-current    PE-N PE-OUT PRIM;
PRIM: set-current    PE-N PE-IN PRIM;
PRIM: search-wl      PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: parse-name     PE-PTR-U8 PE-OUT PE-N PE-OUT PRIM;
PRIM: CORE-STR=      PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: PATHZ          PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-IN PRIM;
PRIM: PATH0          PE-PTR-U8 PE-IN PE-N PE-IN  PE-PTR-U8 PE-OUT PRIM;
PRIM: RD32           PE-PTR-U8 PE-IN  PE-N PE-OUT PRIM;
PRIM: DIAG-FILE!     PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: DIAG-ORIGIN!   PE-N PE-IN PE-N PE-IN PE-N PE-IN PRIM;
PRIM: DIAG-JSON!     PE-F PE-IN PRIM;
PRIM: DIAG-BUFFER!   PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: DIAG-BUFFER-OFF PRIM;
PRIM: DIAG-BUFFER$   PE-PTR-U8 PE-OUT PE-N PE-OUT PRIM;
PRIM: CHECKER-SCOPE-START PRIM;
PRIM: CHECKER-SCOPE-DONE PRIM;
PRIM: CHECK-CANDIDATE! PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: CHECKER-CANDIDATE-SCOPE-START PRIM;
PRIM: CHECKER-CANDIDATE-SCOPE-DONE PRIM;
PRIM: CHECKER-USIGS-TRUNCATE-FROM PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-UNDEFINE PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-DEFTYPE PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-DEFER PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-PACKAGE PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-PUBLIC PRIM;
PRIM: CHECKER-PRIVATE PRIM;
PRIM: CHECKER-END-PACKAGE PRIM;
PRIM: ffi-call       PE-PTR-A PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: ffi-call-n     PE-PTR-A PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: ffi-call-abi   PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN PE-N PE-IN PE-N PE-IN
                     PE-N PE-OUT PRIM;
PRIM: ffi-call-abi-r PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN PE-N PE-IN PE-N PE-IN
                     PE-R PE-OUT PRIM;

PRIM: f+      PE-R PE-IN PE-R PE-IN  PE-R PE-OUT PRIM;
PRIM: f-      PE-R PE-IN PE-R PE-IN  PE-R PE-OUT PRIM;
PRIM: f*      PE-R PE-IN PE-R PE-IN  PE-R PE-OUT PRIM;
PRIM: f/      PE-R PE-IN PE-R PE-IN  PE-R PE-OUT PRIM;
PRIM: fnegate PE-R PE-IN  PE-R PE-OUT PRIM;
PRIM: fabs    PE-R PE-IN  PE-R PE-OUT PRIM;
PRIM: fsqrt   PE-R PE-IN  PE-R PE-OUT PRIM;
PRIM: f<      PE-R PE-IN PE-R PE-IN  PE-F PE-OUT PRIM;
PRIM: f>      PE-R PE-IN PE-R PE-IN  PE-F PE-OUT PRIM;
PRIM: f=      PE-R PE-IN PE-R PE-IN  PE-F PE-OUT PRIM;
PRIM: f0<     PE-R PE-IN  PE-F PE-OUT PRIM;
PRIM: f0=     PE-R PE-IN  PE-F PE-OUT PRIM;
PRIM: s>f     PE-N PE-IN  PE-R PE-OUT PRIM;
PRIM: f>s     PE-R PE-IN  PE-N PE-OUT PRIM;
PRIM: f.      PE-R PE-IN PRIM;

PRIM: s"     PE-PTR-U8 PE-OUT PE-N PE-OUT PRIM;
PRIM: c"     PE-PTR-U8 PE-OUT PRIM;
PRIM: ."     PRIM;
PRIM: s\"    PE-PTR-U8 PE-OUT PE-N PE-OUT PRIM;
PRIM: c\"    PE-PTR-U8 PE-OUT PRIM;
PRIM: .\"    PRIM;
PRIM: [']    PE-N PE-OUT PRIM;
PRIM: char   PE-N PE-OUT PRIM;
PRIM: [char] PE-N PE-OUT PRIM;
PRIM: emit   PE-N PE-IN PRIM;
PRIM: cr     PRIM;
PRIM: space  PRIM;
PRIM: u.     PE-N PE-IN PRIM;

PRIM: create   PE-PTR-A PE-OUT PRIM;
PRIM: variable PE-PTR-A PE-OUT PRIM;
PRIM: constant PE-A PE-OUT PRIM;

PTABLE-END

0 constant CHECKER-PACKAGE-NONE
1 constant CHECKER-PACKAGE-PRIVATE
2 constant CHECKER-PACKAGE-PUBLIC
$100 constant CHECKER-PACKAGE-CAP
create CHECKER-PACKAGE-NAME CHECKER-PACKAGE-CAP allot
variable CHECKER-PACKAGE-U
variable CHECKER-PACKAGE-MODE
variable CHECKER-COLON-N
variable CHECKER-COLON-I
variable CHECKER-REC-A
variable CHECKER-REC-U
variable CHECKER-QA
variable CHECKER-QU
variable CHECKER-TA
variable CHECKER-TU

$10000 constant DFER-CAP

BEGIN-STRUCTURE DFER-REC
   CELL +FIELD DFER.SYM
   CELL +FIELD DFER.FLAG
END-STRUCTURE

create DFERS DFER-CAP allot
variable DFER-END
0 DFERS !
0 DFER-END !

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

: CHECKER-QA-FIELD ( -- ptr ptr u8 )
   CHECKER-QA 0 ptr-field ;

: CHECKER-TA-FIELD ( -- ptr ptr u8 )
   CHECKER-TA 0 ptr-field ;

: CHECKER-QA@ ( -- ptr u8 )
   CHECKER-QA-FIELD @ ;

: CHECKER-TA@ ( -- ptr u8 )
   CHECKER-TA-FIELD @ ;

: CHECKER-QA! ( ptr u8 -- )
   CHECKER-QA-FIELD ! ;

: CHECKER-TA! ( ptr u8 -- )
   CHECKER-TA-FIELD ! ;

: CHECKER-QUALIFIED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u CHECKER-COLON-SCAN
   CHECKER-COLON-N @ 1 <> IF 0 EXIT THEN
   CHECKER-COLON-I @ 0= IF 0 EXIT THEN
   CHECKER-COLON-I @ u 1 - = IF 0 EXIT THEN
   a CHECKER-QA!
   CHECKER-COLON-I @ CHECKER-QU !
   a CHECKER-COLON-I @ + 1 + CHECKER-TA!
   u CHECKER-COLON-I @ - 1 - CHECKER-TU !
   -1 ;

: CHECKER-QPKG$ ( -- ptr u8 n )
   CHECKER-QA@ CHECKER-QU @ ;

: CHECKER-QTAIL$ ( -- ptr u8 n )
   CHECKER-TA@ CHECKER-TU @ ;

: CHECKER-GLOBAL-SYM ( ptr u8 n -- n ) {: a:ptr u:n :}
   s" " SYM-GLOBAL a u SYM-INTERN ;

: CHECKER-GLOBAL-SYM? ( ptr u8 n -- n ) {: a:ptr u:n :}
   s" " SYM-GLOBAL a u SYM-FIND IF EXIT THEN drop 0 ;

: CHECKER-PUBLIC-SYM ( ptr u8 n ptr u8 n -- n ) {: pkg:ptr pkgu:n a:ptr u:n :}
   pkg pkgu SYM-PUBLIC a u SYM-INTERN ;

: CHECKER-PUBLIC-SYM? ( ptr u8 n ptr u8 n -- n ) {: pkg:ptr pkgu:n a:ptr u:n :}
   pkg pkgu SYM-PUBLIC a u SYM-FIND IF EXIT THEN drop 0 ;

: CHECKER-PKG-SYM ( ptr u8 n n ptr u8 n -- n ) {: pkg:ptr pkgu:n vis:n a:ptr u:n :}
   pkg pkgu vis a u SYM-INTERN ;

: CHECKER-PKG-SYM? ( ptr u8 n n ptr u8 n -- n ) {: pkg:ptr pkgu:n vis:n a:ptr u:n :}
   pkg pkgu vis a u SYM-FIND IF EXIT THEN drop 0 ;

: CHECKER-RECORD-SYM ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u CHECKER-QUALIFIED? IF CHECKER-QPKG$ CHECKER-QTAIL$ CHECKER-PUBLIC-SYM EXIT THEN
   CHECKER-PACKAGE-ACTIVE? IF
      CHECKER-PACKAGE-NAME CHECKER-PACKAGE-U @ CHECKER-PACKAGE-MODE @ a u CHECKER-PKG-SYM EXIT
   THEN
   a u CHECKER-GLOBAL-SYM ;

: CHECKER-FIND-ACTIVE-SYM ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u CHECKER-QUALIFIED? IF CHECKER-QPKG$ CHECKER-QTAIL$ CHECKER-PUBLIC-SYM? EXIT THEN
   CHECKER-PACKAGE-ACTIVE? IF
      CHECKER-PACKAGE-NAME CHECKER-PACKAGE-U @ SYM-PRIVATE a u CHECKER-PKG-SYM? dup 0 <> IF EXIT THEN drop
      CHECKER-PACKAGE-NAME CHECKER-PACKAGE-U @ SYM-PUBLIC a u CHECKER-PKG-SYM? dup 0 <> IF EXIT THEN drop
   THEN
   a u CHECKER-GLOBAL-SYM? ;

: CHECKER-FIND-USIG-SYM ( n -- bool ) {: sym:n :}
   sym 0= IF 0 EXIT THEN
   sym SCAN-USIGS-SYM
   FEP @ 0 <> ;

: CHECKER-FIND-USIG ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u CHECKER-RECORD-SYM CHECKER-FIND-USIG-SYM ;

: CHECKER-USIGS-TRUNCATE-FROM ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CHECKER-FIND-ACTIVE-SYM USIG-FIND-OFF-SYM 0= IF
      s" checker: missing signature truncation mark" 76 die
   THEN
   UEND !
   UTERM! ;

: CHECKER-FIND-ACTIVE-SIG ( ptr u8 n -- ) {: a:ptr u:n :}
   0 FEP !
   a u CHECKER-FIND-ACTIVE-SYM CHECKER-FIND-USIG-SYM drop ;

: FIND-SIG ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u CHECKER-FIND-ACTIVE-SIG
   FEP @ IF -1 EXIT THEN
   a u CHECKER-FIND-ACTIVE-SYM PRIM-FIRST-SYM
   dup IF E-PTR FEP ! -1 ELSE drop 0 THEN ;

: DFER-ENSURE ( n -- )
   DFER-CAP > IF s" checker: defer table full" 76 die THEN ;

: DFER-CUR ( -- ptr a )
   DFERS DFER-END @ + ;

: DFER-NEED ( -- n )
   DFER-END @ DFER-REC + cell+ ;

: DFER-TERM ( -- )
   0 DFERS DFER-END @ + ! ;

: DFER-ADD-FLAG ( ptr u8 n n -- ) {: a:ptr u:n flag:n :}
   DFER-NEED DFER-ENSURE
   a u CHECKER-RECORD-SYM DFER-CUR DFER.SYM !
   flag DFER-CUR DFER.FLAG !
   DFER-END @ DFER-REC + DFER-END !
   DFER-TERM ;

: DFER-ADD ( ptr u8 n -- )
   1 DFER-ADD-FLAG ;

: DFER-DELETE ( ptr u8 n -- )
   0 DFER-ADD-FLAG ;

: DFER-NEXT ( ptr a -- ptr a )
   DFER-REC + ;

: DFER-FLAG@ ( ptr a -- n )
   DFER.FLAG @ ;

: DFER-SYM@ ( ptr a -- n )
   DFER.SYM @ ;

: DFER-END? ( ptr a -- bool )
   @ 0= ;

: DFER-MATCH-SYM? ( ptr a n -- bool ) {: rec:ptr sym:n :}
   rec DFER-SYM@ sym = ;

variable DFER-HIT
variable DFER-VALUE

: DFER-SCAN-SYM ( ptr a n -- ) {: rec:ptr sym:n :}
   rec DFER-END? IF EXIT THEN
   rec sym DFER-MATCH-SYM? IF
      -1 DFER-HIT !
      rec DFER-FLAG@ DFER-VALUE !
   THEN
   rec DFER-NEXT sym RECURSE ;

: DFER-FIND-SYM ( n -- bool ) {: sym:n :}
   sym 0= IF 0 EXIT THEN
   0 DFER-HIT !
   0 DFER-VALUE !
   DFERS sym DFER-SCAN-SYM
   DFER-HIT @ IF DFER-VALUE @ 0 <> ELSE 0 THEN ;

: CHECKER-FIND-ACTIVE-DEFER ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u CHECKER-FIND-ACTIVE-SYM DFER-FIND-SYM ;

: CHECKER-RECORD-NAME ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u CHECKER-RECORD-SYM CHECKER-REC-SYM !
   a u ;

: CHECKER-DEFER ( ptr u8 n -- )
   CHECKER-RECORD-NAME DFER-ADD ;

: CHECKER-USIG-ADD ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n na:ptr nu:n :}
   sa su na nu CHECKER-RECORD-NAME USIG-ADD ;

: CHECKER-REC-NAME! ( ptr u8 n -- )
   CHECKER-RECORD-NAME CHECKER-REC-U ! CHECKER-REC-A ! ;

: CHECKER-REC-A@ ( -- ptr u8 )
   CHECKER-REC-A @ ;

: CHECKER-REC-U@ ( -- n )
   CHECKER-REC-U @ ;

: CHECKER-CERT-DUP? ( -- bool )
   CHK-CAND @ IF 0 EXIT THEN
   CHECKER-REC-A@ CHECKER-REC-U@ CHECKER-FIND-USIG ;

: CHECKER-DUP-DEFINITION ( -- )
   $4E throw ;

: CHECKER-USIG-CERT-ADD ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n na:ptr nu:n :}
   na nu CHECKER-REC-NAME!
   CHECKER-CERT-DUP? IF CHECKER-DUP-DEFINITION THEN
   sa su CHECKER-REC-A@ CHECKER-REC-U@ USIG-ADD ;

: CHECKER-USIG-CERT-CURRENT ( ptr u8 n -- ) {: na:ptr nu:n :}
   na nu CHECKER-REC-NAME!
   CHECKER-CERT-DUP? IF CHECKER-DUP-DEFINITION THEN
   BROW @ DCUR @ 0 0 0 E-ADD-EFFECT ;

\ Control-effect flags are append-only and later-wins so redefinitions can clear
\ stale metadata. CTL-DEAD means a call has no normal continuation. CTL-THROW
\ means a call may reach a catchable throw edge.
1 constant CTL-DEAD
2 constant CTL-THROW
$10000 constant NORET-INIT-CAP

BEGIN-STRUCTURE NORET-ENTRY
   CELL +FIELD NORET.SYM
   CELL +FIELD NORET.FLAG
END-STRUCTURE

create NORET-BOOT NORET-INIT-CAP allot
variable NORET-P   variable NORET-CAP-U   variable NORET-END
NORET-BOOT NORET-P !   NORET-INIT-CAP NORET-CAP-U !   0 NORET-END !   0 NORET-BOOT !
variable NORET-POS   variable NORET-FLAG
variable NORET-GROW-CAP   variable NORET-GROW-NEXT

: NORETS ( -- ptr u8 ) NORET-P @ ;

: NORET-TERM ( -- )
   0 NORETS NORET-END @ + ! ;

: NORET-RESTORE-END ( n -- )
   NORET-END !
   NORET-TERM ;

: NORET-RESET ( -- )
   NORET-BOOT NORET-P !
   NORET-INIT-CAP NORET-CAP-U !
   0 NORET-END !
   0 NORET-BOOT !
   0 NORET-GROW-CAP !
   0 NORET-GROW-NEXT ! ;

: NORET-BOOT? ( -- bool )
   NORETS NORET-BOOT = ;

: NORET-SNAPSHOT-CAP ( -- )
   NORET-END @ cell+ NORET-INIT-CAP > IF s" checker: no-return snapshot too large" 76 die THEN ;

: NORET-SNAPSHOT-PERSIST ( -- )
   NORET-SNAPSHOT-CAP
   NORET-BOOT? 0= IF NORETS NORET-BOOT NORET-END @ cell+ USIGS-COPY THEN
   NORET-BOOT NORET-P !
   NORET-INIT-CAP NORET-CAP-U !
   0 NORET-GROW-CAP !
   0 NORET-GROW-NEXT ! ;

: NORET-GROW {: need :}
   need USIGS-ROUND-CAP NORET-GROW-CAP !
   NORET-GROW-CAP @ USIGS-ALLOC NORET-GROW-NEXT !
   NORETS NORET-GROW-NEXT @ NORET-END @ cell+ USIGS-COPY
   NORET-GROW-NEXT @ NORET-P !
   NORET-GROW-CAP @ NORET-CAP-U ! ;

: NORET-ENSURE {: need :}
   need NORET-CAP-U @ <= IF exit THEN
   need NORET-GROW ;

: CHECKER-SNAPSHOT-PREPARE ( -- )
   TOKBUF-RESET
   USIGS-SNAPSHOT-PERSIST
   NORET-SNAPSHOT-PERSIST ;

: NORET-REC ( -- ptr a )
   NORETS NORET-END @ + ;

: NORET-FLAG@ ( ptr a -- n )
   NORET.FLAG @ ;

: NORET-SYM@ ( ptr a -- n )
   NORET.SYM @ ;

: NORET-NEXT ( ptr a -- ptr a )
   NORET-ENTRY + ;

: NORET-END? ( ptr a -- bool )
   @ 0= ;

: NORET-ADD {: a:ptr u:n flag:n :}
   NORET-END @ NORET-ENTRY + cell+ NORET-ENSURE
   a u CHECKER-RECORD-SYM NORET-REC NORET.SYM !
   flag NORET-REC NORET.FLAG !
   NORET-END @ NORET-ENTRY + NORET-END !
   NORET-TERM ;

: CHECKER-UNDEFINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CHECKER-RECORD-NAME {: name:ptr nameu:n :}
   name nameu USIG-DELETE
   name nameu DFER-DELETE
   name nameu 0 NORET-ADD ;

: CHECKER-DEFTYPE ( ptr u8 n -- )
   CT-ADD-NOMINAL ;

: CTL-FLAGS-SYM {: sym:n :}
   sym 0= IF 0 EXIT THEN
   0 NORET-FLAG !
   0 NORET-POS !
   BEGIN NORETS NORET-POS @ + NORET-END? 0= WHILE
      NORETS NORET-POS @ + NORET-SYM@ sym = IF
         NORETS NORET-POS @ + NORET-FLAG@ NORET-FLAG !
      THEN
      NORETS NORET-POS @ + NORET-NEXT NORETS - NORET-POS !
   REPEAT
   NORET-FLAG @ ;

: CTL-FLAGS {: a:ptr u:n :}
   a u CHECKER-FIND-ACTIVE-SYM CTL-FLAGS-SYM ;

: NORET-USER? {: a:ptr u:n :}
   a u CTL-FLAGS CTL-DEAD and 0 <> ;

: THROW-USER? {: a:ptr u:n :}
   a u CTL-FLAGS CTL-THROW and 0 <> ;

: DEAD-TOK? {: a u :}
   a u s" die" CORE-STR= IF -1 EXIT THEN
   a u s" throw" CORE-STR= IF -1 EXIT THEN
   a u NORET-USER? ;

: THROW-TOK? {: a u :}
   a u s" throw" CORE-STR= IF -1 EXIT THEN
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

variable TSEEN  variable TSOK  variable TFA

: TRY-EFF ( ptr a -- bool ) {: h:ptr :}
   TRIAL-SAVE
   h EFF-APPLY
   OK @ SGBAD @ 0= and IF TRIAL-REST-SG -1 ELSE TRIAL-REST 0 THEN ;

: TRY-PRIMS ( n -- bool ) {: sym:n :}
   0 TSEEN !  0 TSOK !  0 TFA !
   0 PE-I !
   begin PE-I @ #PE @ < while
      PE-I @ PE-ACTIVE? IF
         PE-I @ PE-SYM@ sym = IF
            TSEEN @ 0= IF PE-I @ PE-EFF@ TFA ! THEN
            -1 TSEEN !
            TSOK @ 0= IF PE-I @ PE-EFF@ E-PTR TRY-EFF IF -1 TSOK ! THEN THEN
         THEN
      THEN
      PE-I @ 1 + PE-I !
   repeat
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
   a u s" create" CORE-STR= IF -1 EXIT THEN
   a u s" variable" CORE-STR= IF -1 EXIT THEN
   a u s" constant" CORE-STR= IF STEP-N-IN -1 EXIT THEN
   0 ;

: LITERAL-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u ALLDIG? IF STEP-N-OUT -1 EXIT THEN
   a u FLODIG? IF STEP-R-OUT -1 EXIT THEN
   0 ;

: BYTE-CON? ( t -- bool )
   T-RES dup TAG T-CON = IF PAY CC-U8 = EXIT THEN drop 0 ;

: BYTE-PTR? ( t -- bool )
   T-RES dup TAG T-PTR = IF PTR>INNER BYTE-CON? EXIT THEN drop 0 ;

: ROW-TOP-BYTE-PTR? ( row -- bool )
   R-RES dup TAG S-PUSH = IF P>TYPE BYTE-PTR? EXIT THEN drop 0 ;

: CELL-FETCH-TOK ( -- )
   DCUR @ ROW-TOP-BYTE-PTR? {: bad :}
   STEP-FETCH
   bad IF 0 OK ! THEN ;

: CELL-STORE-TOK ( -- )
   DCUR @ ROW-TOP-BYTE-PTR? {: bad :}
   STEP-STORE
   bad IF 0 OK ! THEN ;

: CELL-MEMORY-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" @" CORE-STR= IF CELL-FETCH-TOK -1 EXIT THEN
   a u s" !" CORE-STR= IF CELL-STORE-TOK -1 EXIT THEN
   0 ;

: DO-TOK {: a u :}
   a u DEFINER-TOK IF EXIT THEN
   a u LITERAL-TOK? IF EXIT THEN
   a u CELL-MEMORY-TOK? IF EXIT THEN
   a u CHECKER-FIND-ACTIVE-SIG
   FEP @ IF FEP @ EFF-APPLY ELSE
   a u CHECKER-FIND-ACTIVE-SYM TRY-PRIMS IF EXIT THEN
   TSEEN @ IF TFA @ E-PTR EFF-APPLY ELSE
   -1 UNCK ! THEN THEN ;

\ --- locals: {: a b :} pops and binds names to type vars; a reference pushes
\ its binding. Groups accumulate (a later group binds only its own names).
: CCOPY {: a d u :}  0 BEGIN dup u < WHILE  dup a + c@  over d + c!  1 + REPEAT drop ;
create LOCNB 1024 allot   create LOCLN 64 cells allot   create LOCTV 64 cells allot
create LOCSHOW 64 cells allot
variable #LOC  variable LMODE  variable LGRP  variable LROW  variable LCH  variable LI  variable LRF
variable LOCSHOWXT  0 LOCSHOWXT !
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
: LOC-SHOW-SUFFIX? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 1 = if a c@ 63 = exit then
   0 ;

: LOC-SUFFIX$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a LCO @ + 1 +  u LCO @ - 1 - ;

: LOC-SHOW-OFF! ( n -- ) {: idx:n :}
   0 idx cells LOCSHOW + ! ;

: LOC-SHOW-ON! ( n -- ) {: idx:n :}
   -1 idx cells LOCSHOW + ! ;

: LOC-ANN ( ptr u8 n n -- ) {: a:ptr u:n idx:n :}
   a u LOC-SUFFIX$ LOC-SHOW-SUFFIX? if
      idx LOC-SHOW-ON!
      exit
   then
   a u LOC-SUFFIX$ LOCAL-TYPE
   idx cells LOCTV + @ UNIFY OK @ and OK ! ;

: LOC-SHOW-ONE ( n -- ) {: idx:n :}
   LOCSHOWXT @ 0= if exit then
   idx cells LOCSHOW + @ 0= if exit then
   LOCNB idx 16 * +  idx cells LOCLN + @  idx cells LOCTV + @
   LOCSHOWXT @ execute ;

: LOC-SHOW-GROUP ( -- )
   OK @ 0= if exit then
   LGRP @ begin dup #LOC @ < while
      dup LOC-SHOW-ONE
      1 +
   repeat drop ;

: LOC-ADD {: a u :}
   a u LCOLON
   #LOC @ 63 >  LCO @ 16 >  or IF -1 UNCK ! ELSE
     #LOC @ LOC-SHOW-OFF!
     a  LOCNB #LOC @ 16 * +  LCO @ CCOPY
     LCO @ #LOC @ cells LOCLN + !
     FRESH MK-VAR #LOC @ cells LOCTV + !
     LCO @ u < IF
      a u #LOC @ LOC-ANN
     THEN
     #LOC @ 1 + #LOC ! THEN ;

: LOC-BIND
   FRESH dup LROW !  MK-ROW LCH !
   LGRP @ BEGIN dup #LOC @ < WHILE
     dup cells LOCTV + @  LCH @ MK-PUSH LCH !
     1 + REPEAT drop
   LCH @  LROW @ MK-ROW  CHECKER-STEP
   LOC-SHOW-GROUP ;

: LOC-TOK {: a u :}
   a u s" :}" CORE-STR= IF 0 LMODE ! LOC-BIND ELSE
   a u s" --" CORE-STR= IF -1 UNCK ! ELSE
   a u LOC-ADD THEN THEN ;

: LOC-REJECT ( -- )
   0 OK !  -1 FAILSET !  -1 LOCALBAD ! ;

: LOC-BEGIN ( -- )
   QDEPTH @ 0 >  DEADP @ or IF LOC-REJECT ELSE
   1 LMODE !  #LOC @ LGRP ! THEN ;

: LOC-REF? {: a u :}
   0 LRF !  #LOC @ LI !
   BEGIN LI @ 0 >  LRF @ 0=  and WHILE
     LI @ 1 - LI !
     a u  LOCNB LI @ 16 * +  LI @ cells LOCLN + @  CORE-STR= IF
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
\ exit-accumulator save fields: a [: ;] quotation is a nested scope, so its
\ early returns must NOT leak into the enclosing word's accumulator.
BEGIN-STRUCTURE CFS-REC
   CELL +FIELD CF.KND
   CELL +FIELD CF.SA
   CELL +FIELD CF.SB
   CELL +FIELD CF.RA
   CELL +FIELD CF.RB
   CELL +FIELD CF.DED
   CELL +FIELD CF.LN
   CELL +FIELD CF.XRO
   CELL +FIELD CF.XRR
   CELL +FIELD CF.XST
   CELL +FIELD CF.XDP
   CELL +FIELD CF.TXD
   CELL +FIELD CF.TXR
   CELL +FIELD CF.TXS
END-STRUCTURE

create CFS 32 CFS-REC * allot
variable CTMP  variable RTMP  variable CFH  variable INDO
\ EXIT: an early return. XROW accumulates the data row at each exit (all returns,
\ incl. the fall-through at ';', must unify). DEADP marks the current linear path
\ terminated by exit, so the enclosing THEN excludes it from the branch join.
\ CF.DED saves the if-branch's deadness across CF-ELSE. (leave targets the
\ enclosing DO frame's loop-exit row; unloop is a typing no-op — loop control
\ isn't on the typed rows.)
variable RSHAS  variable RSGIN  variable RSGOUT  variable RSGRIN  variable RSGROUT
variable RHAS   variable RDIN   variable RDOUT   variable RRIN    variable RROUT

: CF-ROW ( n -- ptr a )
   CFS-REC * CFS + ;

: CF-TOP ( -- ptr a )
   #CFC @ 1 - CF-ROW ;

: CF@DED ( -- n )
   CF-TOP CF.DED @ ;

: CF-BELOW-CASE? ( -- bool )
   #CFC @ 2 < IF 0 EXIT THEN
   #CFC @ 2 - CF-ROW CF.KND @ 7 = ;

: CF-CASE-IDX ( -- n )
   #CFC @ 2 - ;

: CF-CASE-HAS? ( n -- bool ) {: idx:n :}
   idx CF-ROW CF.DED @ ;

: CF-CASE-HAS! ( n -- ) {: idx:n :}
   -1 idx CF-ROW CF.DED ! ;

: CF-CASE-DATA@ ( n -- n ) {: idx:n :}
   idx CF-ROW CF.SB @ ;

: CF-CASE-RET@ ( n -- n ) {: idx:n :}
   idx CF-ROW CF.RB @ ;

: CF-CASE-DATA! ( n n -- ) {: row:n idx:n :}
   row idx CF-ROW CF.SB ! ;

: CF-CASE-RET! ( n n -- ) {: row:n idx:n :}
   row idx CF-ROW CF.RB ! ;

: CF-PUSH {: k s0 s1 r0 r1 :}
   #CFC @ 31 > IF -1 UNCK ! ELSE
     #CFC @ CF-ROW {: rec:ptr :}
     k rec CF.KND !  s0 rec CF.SA !  s1 rec CF.SB !
     r0 rec CF.RA !  r1 rec CF.RB !
     #LOC @ rec CF.LN !
     #CFC @ 1 + #CFC ! THEN ;

: CF@K CF-TOP CF.KND @ ;

: CF@A CF-TOP CF.SA @ ;

: CF@B CF-TOP CF.SB @ ;

: CF@RA CF-TOP CF.RA @ ;

: CF@RB CF-TOP CF.RB @ ;

: CF@LN CF-TOP CF.LN @ ;

: CF-LOC-REST ( -- )
   CF@LN #LOC ! ;

: CF-DROP #CFC @ 1 - #CFC ! ;

: CF-MT? #CFC @ 0 > 0= ;

: CF-FAIL ( -- )
   0 OK !
   -1 FAILSET ! ;

: SUNI {: s :}
   DCUR @ s UNIFY
   dup 0=  FAILSET @ 0=  and  OK @ and  IF s DEXP !  DCUR @ DACT !  -1 FAILSET ! THEN
   OK @ and OK ! ;

: SUNI-IN {: s:n :}
   DCUR @ s UNIFY-IN
   dup 0=  FAILSET @ 0=  and  OK @ and  IF s DEXP !  DCUR @ DACT !  -1 FAILSET ! THEN
   OK @ and OK ! ;

: RSUNI {: s :}  RCUR @ s UNIFY OK @ and OK ! ;

: RSUNI-IN {: s:n :}  RCUR @ s UNIFY-IN OK @ and OK ! ;

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
      RDIN @ SUNI-IN  RDOUT @ DCUR !
      RHAS @ IF RRIN @ RSUNI-IN  RROUT @ RCUR ! THEN
   ELSE -1 UNCK ! THEN ;

: CF-IF  STEP-BOOL-IN  1 DCUR @ 0 RCUR @ 0 CF-PUSH ;   \ IF consumes a flag, not any value

: CF-CASE ( -- )
   7 DCUR @ 0 RCUR @ 0 CF-PUSH
   0 CF-TOP CF.DED ! ;

: CF-CASE-ACCUM ( n -- ) {: idx:n :}
   OK @ 0= IF EXIT THEN
   DEADP @ IF EXIT THEN
   idx CF-CASE-HAS? IF
      idx CF-CASE-DATA@ SUNI
      idx CF-CASE-RET@ RSUNI
   ELSE
      DCUR @ idx CF-CASE-DATA!
      RCUR @ idx CF-CASE-RET!
      idx CF-CASE-HAS!
   THEN ;

: CF-OF ( -- )
   CF-MT? IF CF-FAIL ELSE CF@K 7 <> IF CF-FAIL ELSE
      STEP-N-IN
      CF@A SUNI
      CF@RA RSUNI
      STEP-N-IN
      8 CF@A 0 CF@RA 0 CF-PUSH
   THEN THEN ;

: CF-ENDOF ( -- )
   CF-BELOW-CASE? 0= IF CF-FAIL ELSE CF@K 8 <> IF CF-FAIL ELSE
      CF-CASE-IDX CF-CASE-ACCUM
      CF@A CTMP !  CF@RA RTMP !
      CF-LOC-REST
      0 DEADP !
      CF-DROP
      CTMP @ DCUR !  RTMP @ RCUR !
   THEN THEN ;

: CF-ENDCASE ( -- )
   CF-MT? IF CF-FAIL ELSE CF@K 7 <> IF CF-FAIL ELSE
      DEADP @ 0= IF STEP-N-IN THEN
      #CFC @ 1 - CF-CASE-ACCUM
      CF@DED IF
         CF@B DCUR !  CF@RB RCUR !  0 DEADP !
      ELSE
         -1 DEADP !
      THEN
      CF-LOC-REST
      CF-DROP
   THEN THEN ;

: CF-ELSE
   CF-MT? IF CF-FAIL ELSE CF@K 1 <> IF CF-FAIL ELSE
     DEADP @ CF-TOP CF.DED !  0 DEADP !                  \ save if-branch deadness; else runs live
     DCUR @ CTMP !  CF@A DCUR !
     RCUR @ RTMP !  CF@RA RCUR !
     2 CF-TOP CF.KND !
     CTMP @ CF-TOP CF.SB !
     RTMP @ CF-TOP CF.RB !
     CF-LOC-REST
   THEN THEN ;

: CF-THEN
   CF-MT? IF CF-FAIL ELSE
     CF@K 1 = IF                                          \ IF ... THEN (no else)
        DEADP @ IF CF@A DCUR !  CF@RA RCUR !  0 DEADP !   \ if-branch exited: take fall-through
        ELSE CF@A SUNI  CF@RA RSUNI THEN  CF-LOC-REST  CF-DROP
     ELSE CF@K 2 = IF                                     \ IF ... ELSE ... THEN
        DEADP @  CF@DED                                   \ ( else-dead if-dead )
        2dup and IF 2drop -1 DEADP !                      \ both exited -> path stays dead
        ELSE over IF 2drop CF@B DCUR ! CF@RB RCUR ! 0 DEADP !  \ else exited -> take if-branch
        ELSE nip IF 0 DEADP !                             \ if exited -> keep else (in DCUR)
        ELSE CF@B SUNI CF@RB RSUNI 0 DEADP ! THEN THEN THEN
        CF-LOC-REST  CF-DROP
     ELSE CF-FAIL THEN THEN THEN ;

: CF-EXIT                                                 \ early return: accumulate, kill path
   XSET @ IF  DCUR @ XROW @ UNIFY OK @ and OK !
              RCUR @ XRROW @ UNIFY OK @ and OK !
   ELSE  DCUR @ XROW !  RCUR @ XRROW !  -1 XSET ! THEN
   -1 DEADP ! ;

: CF-UNLOOP ;                                             \ loop control isn't typed -> no-op

: CF-BEGIN  3 DCUR @ 0 RCUR @ 0 CF-PUSH ;

: CF-UNTIL
   STEP-BOOL-IN
   CF-MT? IF CF-FAIL ELSE CF@K 3 <> IF CF-FAIL ELSE
     CF@A SUNI  CF@A DCUR !  CF@RA RSUNI  CF@RA RCUR !
     CF-LOC-REST  CF-DROP THEN THEN ;

: CF-AGAIN                              \ unconditional loop: code after AGAIN is unreachable
   CF-MT? IF CF-FAIL ELSE CF@K 3 <> IF CF-FAIL ELSE
     CF@A SUNI  CF@A DCUR !  CF@RA RSUNI  CF@RA RCUR !
     CF-LOC-REST  CF-DROP  -1 DEADP ! THEN THEN ;

: CF-WHILE
   STEP-BOOL-IN
   CF-MT? IF CF-FAIL ELSE CF@K 3 <> IF CF-FAIL ELSE
     4 CF-TOP CF.KND !
     DCUR @ CF-TOP CF.SB !
     RCUR @ CF-TOP CF.RB !
   THEN THEN ;

: CF-REPEAT
   CF-MT? IF CF-FAIL ELSE CF@K 4 <> IF CF-FAIL ELSE
     CF@A SUNI  CF@B DCUR !  CF@RA RSUNI  CF@RB RCUR !
     CF-LOC-REST  CF-DROP THEN THEN ;

: CF-DO  STEP-NN-IN  5 DCUR @ 0 RCUR @ 0 CF-PUSH ;

\ At LOOP the exit is always live: ?do/do terminates, and a `leave` jumps here.
\ If the body fall-through is dead (unconditional leave/exit), the back-edge is
\ never taken — skip the body-vs-DO-point unify, but the loop-exit row is still
\ the DO-point row (a zero-trip ?do or a leave both leave exactly that). Live
\ fall-through: the back edge requires a stack-neutral body (CF@A SUNI).
: CF-LOOP
   CF-MT? IF CF-FAIL ELSE CF@K 5 <> IF CF-FAIL ELSE
     DEADP @ IF  0 DEADP !
     ELSE  CF@A SUNI  CF@RA RSUNI  THEN
     CF@A DCUR !  CF@RA RCUR !  CF-LOC-REST  CF-DROP THEN THEN ;

: CF-+LOOP
   STEP-N-IN
   CF-MT? IF CF-FAIL ELSE CF@K 5 <> IF CF-FAIL ELSE
     DEADP @ IF  0 DEADP !
     ELSE  CF@A SUNI  CF@RA RSUNI  THEN
     CF@A DCUR !  CF@RA RCUR !  CF-LOC-REST  CF-DROP THEN THEN ;

: CF-I
   0 INDO !  0 BEGIN dup #CFC @ < WHILE
     dup CF-ROW CF.KND @ 5 = IF -1 INDO ! THEN  1 + REPEAT drop
   INDO @ IF STEP-N-OUT ELSE CF-FAIL THEN ;

: CF-J                                     \ needs two enclosing DO frames
   0 INDO !  0 BEGIN dup #CFC @ < WHILE
     dup CF-ROW CF.KND @ 5 = IF INDO @ 1 + INDO ! THEN  1 + REPEAT drop
   INDO @ 1 > IF STEP-N-OUT ELSE CF-FAIL THEN ;

variable LVDO  variable LVDN
\ CF-FINDDO ( -- ) : LVDO = index of the nearest enclosing DO frame, or -1.
\ Scans top-down and stops at the first DO (kind 5) or quotation boundary
\ (kind 6) — a `leave` inside [: ;] does not escape to an outer loop.
: CF-FINDDO
   -1 LVDO !  0 LVDN !
   #CFC @ 1 -
   BEGIN dup 0 >= LVDN @ 0= and WHILE
     dup CF-ROW CF.KND @ 5 = IF dup LVDO !  -1 LVDN ! THEN
     dup CF-ROW CF.KND @ 6 = IF -1 LVDN ! THEN
     1 - REPEAT drop ;

\ CF-LEAVE : early loop exit. The stack at `leave` must match the loop-exit row
\ (= the DO-point row CF.SA, since the body is stack-neutral); likewise the return
\ row. Then the path to `loop` is dead (CF-LOOP revives the live loop exit).
: CF-LEAVE
   CF-FINDDO
   LVDO @ 0< IF CF-FAIL ELSE
     LVDO @ CF-ROW CF.SA @ SUNI
     LVDO @ CF-ROW CF.RA @ RSUNI
     -1 DEADP ! THEN ;

: CF-QUOT   \ [: — pause the outer inference (incl. its exit state), open a nested one
   6  DCUR @  BROW @  RCUR @  RBROW @  CF-PUSH
   XROW @ CF-TOP CF.XRO !  XRROW @ CF-TOP CF.XRR !
   XSET @ CF-TOP CF.XST !  DEADP @ CF-TOP CF.XDP !
   THDROW @ CF-TOP CF.TXD !  THRROW @ CF-TOP CF.TXR !
   THSET @ CF-TOP CF.TXS !
   0 XSET !  0 DEADP !  0 THSET !
   QDEPTH @ 1 + QDEPTH !
   FRESH MK-ROW dup BROW ! DCUR !
   FRESH MK-ROW dup RBROW ! RCUR ! ;

variable QTMP

: CF-SEMIQ  \ ;] — quot<nested effect> pushed onto the restored outer row
   CF-MT? IF CF-FAIL ELSE CF@K 6 <> IF CF-FAIL ELSE
     XSET @ IF                                   \ fold the quote's OWN early returns into its effect
       DEADP @ IF XROW @ DCUR !  XRROW @ RCUR !
       ELSE DCUR @ XROW @ UNIFY OK @ and OK !  RCUR @ XRROW @ UNIFY OK @ and OK ! THEN
     THEN
     BROW @  DCUR @  RBROW @  RCUR @  MK-QUOT QTMP !
     QTMP @ THSET @ DEADP @ XSET @ 0= and THDROW @ THRROW @ QX!
     CF-TOP CF.XRO @ XROW !  CF-TOP CF.XRR @ XRROW !
     CF-TOP CF.XST @ XSET !  CF-TOP CF.XDP @ DEADP !  \ restore outer exit state
     CF-TOP CF.TXD @ THDROW !  CF-TOP CF.TXR @ THRROW !
     CF-TOP CF.TXS @ THSET !
     QDEPTH @ 1 - QDEPTH !
     CF@B BROW !  CF@RB RBROW !
     CF@RA RCUR !
     QTMP @  CF@A  MK-PUSH DCUR !
     CF-LOC-REST
     CF-DROP THEN THEN ;

: CF-TOK? {: a u :}
   -1 CFH !
   a u s" [:" CORE-STR= IF CF-QUOT ELSE
   a u s" ;]" CORE-STR= IF CF-SEMIQ ELSE
   a u s" if" CORE-STR= IF CF-IF ELSE
   a u s" else" CORE-STR= IF CF-ELSE ELSE
   a u s" then" CORE-STR= IF CF-THEN ELSE
   a u s" case" CORE-STR= IF CF-CASE ELSE
   a u s" of" CORE-STR= IF CF-OF ELSE
   a u s" endof" CORE-STR= IF CF-ENDOF ELSE
   a u s" endcase" CORE-STR= IF CF-ENDCASE ELSE
   a u s" begin" CORE-STR= IF CF-BEGIN ELSE
   a u s" until" CORE-STR= IF CF-UNTIL ELSE
   a u s" again" CORE-STR= IF CF-AGAIN ELSE
   a u s" while" CORE-STR= IF CF-WHILE ELSE
   a u s" repeat" CORE-STR= IF CF-REPEAT ELSE
   a u s" do" CORE-STR= IF CF-DO ELSE
   a u s" ?do" CORE-STR= IF CF-DO ELSE
   a u s" loop" CORE-STR= IF CF-LOOP ELSE
   a u s" +loop" CORE-STR= IF CF-+LOOP ELSE
   a u s" i" CORE-STR= IF CF-I ELSE
   a u s" j" CORE-STR= IF CF-J ELSE
   a u s" exit" CORE-STR= IF CF-EXIT ELSE
   a u s" leave" CORE-STR= IF CF-LEAVE ELSE
   a u s" unloop" CORE-STR= IF CF-UNLOOP ELSE
   a u s" recurse" CORE-STR= IF CF-RECURSE ELSE
   0 CFH ! THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN
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

: STRING-LEAD? ( ptr u8 -- bool ) {: a:ptr :}
   a c@ 46 = IF -1 EXIT THEN
   a c@ dup 65 >= over 90 <= and IF 32 + THEN
   dup 115 = swap 99 = or ;

: NORMAL-STRING-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 2 <> IF 0 0= 0= EXIT THEN
   a 1 + c@ 34 <> IF 0 0= 0= EXIT THEN
   a STRING-LEAD? ;

: ESCAPED-STRING-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 3 <> IF 0 0= 0= EXIT THEN
   a 1 + c@ 92 <> IF 0 0= 0= EXIT THEN
   a 2 + c@ 34 <> IF 0 0= 0= EXIT THEN
   a STRING-LEAD? ;

: STRING-OPENER? ( ptr u8 n -- bool )
   2dup NORMAL-STRING-OPENER? IF 2drop -1 EXIT THEN
   ESCAPED-STRING-OPENER? ;

: PARSE-LIT? {: a:ptr u:n :}
   a u s" [char]" CORE-STR= IF -1 EXIT THEN
   a u s" char" CORE-STR= ;

: SKIP-STRING-PAYLOAD
   TI @ SKI !  0 SKF !
   BEGIN SKI @ TBLEN @ <  SKF @ 0=  and WHILE
      TBASE @ SKI @ + c@ 34 = IF -1 SKF ! ELSE SKI @ 1 + SKI ! THEN
   REPEAT
   SKF @ IF SKI @ 1 + TI ! ELSE TBLEN @ TI ! 0 OK ! THEN ;

: SKIP-ESCAPED-STRING-PAYLOAD ( -- )
   TI @ SKI !  0 SKF !
   BEGIN SKI @ TBLEN @ <  SKF @ 0=  and WHILE
      TBASE @ SKI @ + c@ 92 = IF
         SKI @ 1 + SKI !
         SKI @ TBLEN @ < IF SKI @ 1 + SKI ! THEN
      ELSE
         TBASE @ SKI @ + c@ 34 = IF -1 SKF ! ELSE SKI @ 1 + SKI ! THEN
      THEN
   REPEAT
   SKF @ IF SKI @ 1 + TI ! ELSE TBLEN @ TI ! 0 OK ! THEN ;

: SKIP-PARSE-LIT-PAYLOAD ( -- )
   BEGIN TI @ TBLEN @ < IF TBASE @ TI @ + c@ 32 <= ELSE 0 0= 0= THEN WHILE
      TI @ 1 + TI !
   REPEAT
   TI @ TBLEN @ >= IF 0 OK ! exit THEN
   BEGIN TI @ TBLEN @ < IF TBASE @ TI @ + c@ 32 > ELSE 0 0= 0= THEN WHILE
      TI @ 1 + TI !
   REPEAT ;

: DEAD-OWNER! ( ptr u8 n -- )
   DEADTU !  DEADTA ! ;

: DEAD-CLOSE? {: a u :}
   a u s" else"   CORE-STR= IF -1 EXIT THEN
   a u s" then"   CORE-STR= IF -1 EXIT THEN
   a u s" loop"   CORE-STR= IF -1 EXIT THEN
   a u s" +loop"  CORE-STR= IF -1 EXIT THEN
   a u s" endof"  CORE-STR= IF -1 EXIT THEN
   a u s" endcase" CORE-STR= IF -1 EXIT THEN
   a u s" repeat" CORE-STR= IF -1 EXIT THEN
   a u s" again"  CORE-STR= IF -1 EXIT THEN
   a u s" ;]"     CORE-STR= IF -1 EXIT THEN
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
   a u s" evaluate" CORE-STR= IF -1 EXIT THEN
   a u s" trust" CORE-STR= IF -1 EXIT THEN
   a u s" set-check" CORE-STR= IF -1 EXIT THEN
   a u s" postpone" CORE-STR= IF -1 EXIT THEN
   a u s" compile," CORE-STR= IF -1 EXIT THEN
   a u s" immediate" CORE-STR= IF -1 EXIT THEN
   a u s" [" CORE-STR= IF -1 EXIT THEN
   a u s" ]" CORE-STR= ;

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
   TKF TKFU @ CHECKER-FIND-ACTIVE-DEFER 0= IF IS-FAIL EXIT THEN
   TKF TKFU @ CHECKER-FIND-ACTIVE-SIG
   FEP @ 0= IF IS-FAIL EXIT THEN
   FEP @ EFF-QUOT IS-APPLY ;

: DO-TOK1 {: a u :}
   a u TOKFOLD drop
   CAP-FAIL
   TOK0 @ IF TKF NMB TKFU @ CCOPY  NMB NMA !  TKFU @ NMU !  0 TOK0 ! ELSE
   TKF TKFU @ LIVE-TOKEN? 0= IF -1 DEADERR ! 0 OK ! ELSE
   LMODE @ IF TKF TKFU @ LOC-TOK ELSE
   TKF TKFU @ s" {:" CORE-STR= IF LOC-BEGIN ELSE
   TKF TKFU @ UNSAFE-TOK? IF REJECT-UNSAFE ELSE
   TKF TKFU @ s" is" CORE-STR= IF IS-TOK ELSE
   OK @ IF TKF TKFU @ s" exit" CORE-STR= IF a u DEAD-OWNER! THEN THEN
   OK @ IF TKF TKFU @ s" leave" CORE-STR= IF a u DEAD-OWNER! THEN THEN
   OK @ IF TKF TKFU @ s" again" CORE-STR= IF a u DEAD-OWNER! THEN THEN
   TKF TKFU @ LOC-REF? 0= IF
   TKF TKFU @ CF-TOK? 0= IF
   TKF TKFU @ RS-TOK? 0= IF
   TKF TKFU @ DO-TOK
   OK @ IF TKF TKFU @ THROW-TOK? IF THROW-EDGE THEN THEN
   OK @ IF TKF TKFU @ DEAD-TOK? IF a u DEAD-OWNER! -1 DEADP ! THEN THEN
   TKF TKFU @ ESCAPED-STRING-OPENER? IF SKIP-ESCAPED-STRING-PAYLOAD ELSE
   TKF TKFU @ NORMAL-STRING-OPENER? IF SKIP-STRING-PAYLOAD THEN THEN
   TKF TKFU @ PARSE-LIT? IF SKIP-PARSE-LIT-PAYLOAD THEN
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
      SGOUT @ SUNI-IN
      OK @ IF SGIN @ BROW !  SGOUT @ DCUR ! THEN    \ record the verified declared effect
   THEN                                        \ SUNI captures declared(exp)/inferred(act)
   LMODE @ 0 <>  #CFC @ 0 <>  or IF CF-FAIL THEN
   SGHASR @ 0= IF RCUR @ R-RES  RBROW @ R-RES  <> IF 0 OK ! THEN THEN   \ balance (no clause)
   VSIG @ SGSEEN @ SGHASR @ and and IF
      RCUR @ SGROUT @ UNIFY-IN OK @ and OK !
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

variable CAND-UEND
variable CAND-NEND
variable CAND-SYMN
variable CAND-SYMU
variable CAND-CTN
variable CAND-CTU
variable CAND-VERD
variable CSCOPE-UEND
variable CSCOPE-NEND
variable CSCOPE-SYMN
variable CSCOPE-SYMU
variable CSCOPE-CTN
variable CSCOPE-CTU
variable CSCOPE-CAND
variable CSCOPE-VSIG

: CHECKER-SCOPE-START ( -- )
   UEND @ CSCOPE-UEND !
   NORET-END @ CSCOPE-NEND !
   SYM-N @ CSCOPE-SYMN !
   SYM-STR-U @ CSCOPE-SYMU !
   CTN @ CSCOPE-CTN !
   CT-STR-U @ CSCOPE-CTU !
   CHK-CAND @ CSCOPE-CAND !
   VSIG @ CSCOPE-VSIG ! ;

: CHECKER-SCOPE-DONE ( -- )
   CSCOPE-UEND @ USIGS-RESTORE-END
   CSCOPE-NEND @ NORET-RESTORE-END
   CSCOPE-SYMN @ SYM-N !
   CSCOPE-SYMU @ SYM-STR-U !
   CSCOPE-CTN @ CTN !
   CSCOPE-CTU @ CT-STR-U !
   CSCOPE-CAND @ CHK-CAND !
   CSCOPE-VSIG @ VSIG ! ;

: CHECK-CANDIDATE-START ( -- )
   UEND @ CAND-UEND !
   NORET-END @ CAND-NEND !
   SYM-N @ CAND-SYMN !
   SYM-STR-U @ CAND-SYMU !
   CTN @ CAND-CTN !
   CT-STR-U @ CAND-CTU !
   -1 CHK-CAND !
   -1 VSIG ! ;

: CHECK-CANDIDATE-DONE ( n -- n )
   CAND-VERD !
   0 VSIG !
   0 CHK-CAND !
   CAND-UEND @ USIGS-RESTORE-END
   CAND-NEND @ NORET-RESTORE-END
   CAND-SYMN @ SYM-N !
   CAND-SYMU @ SYM-STR-U !
   CAND-CTN @ CTN !
   CAND-CTU @ CT-STR-U !
   CAND-VERD @ ;

: CHECK-CANDIDATE! ( ptr u8 n -- n )
   CHECK-CANDIDATE-START
   CHECK
   CHECK-CANDIDATE-DONE ;

: CHECKER-CANDIDATE-SCOPE-START ( -- )
   CHECK-CANDIDATE-START ;

: CHECKER-CANDIDATE-SCOPE-DONE ( -- )
   0 CHECK-CANDIDATE-DONE drop ;

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
   SGOUT @ SUNI-IN
   OK @ IF SGOUT @ DCUR ! THEN
   LMODE @ 0 <>  #CFC @ 0 <>  or IF CF-FAIL THEN
   SGHASR @ 0= IF RCUR @ R-RES  RBROW @ R-RES  <> IF 0 OK ! THEN THEN
   SGHASR @ IF RCUR @ SGROUT @ UNIFY-IN OK @ and OK ! THEN
   CHECK-VERDICT dup DVERD ! ;
