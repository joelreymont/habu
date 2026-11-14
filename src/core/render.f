\ render.f: renders checker types and rows as text: a residual stack (DCUR), a
\ refusal's diagnostic line and JSON packet. REC-SIG records a certified
\ sig-less word's inferred effect under its name, so its callers certify;
\ rendering only decides whether that effect is declarable
\ (RQM, NLET). Compiles after checker.f and before any src/lib/ file: no emit or
\ +!, so bytes go out through the one-byte ECH buffer.
using TFAM

create ECH 1 allot
variable RDST   0 RDST !                 \ 0 = stdout, 1 = RSBUF (sig recording)
\ RSBUF, mapped on first use, holds the text being recorded, RSN bytes of it;
\ RSBUF-FLUSH zeroes RSN.
16384 constant RSBUF-FIRST                \ bytes of the first mapping
PERSISTED-PTR-VARIABLE RSBUF-P   NULL-PTR RSBUF-P !
variable RSBUF-CAP   0 RSBUF-CAP !        \ bytes mapped at RSBUF-P, 0 for none
variable RSN


: RSBUF-MAP ( -- )
   RSBUF-FIRST ARENA-ALLOC-BYTES RSBUF-P !  RSBUF-FIRST RSBUF-CAP ! ;


: RSBUF ( -- ptr u8 )
   RSBUF-CAP @ 0= IF RSBUF-MAP THEN
   RSBUF-P @ ;


\ Doubles RSBUF, keeping its bytes; a span taken from it before is stale.
: RSBUF-GROW ( -- )
   RSBUF-CAP @ 0= IF RSBUF-MAP EXIT THEN
   RSBUF-CAP @ {: oc:n :}
   RSBUF-P @ oc oc 2 * ARENA-BYTES-GROW RSBUF-P !
   oc 2 * RSBUF-CAP ! ;


\ RQM: the text cannot say the effect, so REC-SIG does not record it; set where the
\ text writes `?` and where it meets a type or row no signature spells.
variable RQM
\ RROW-UNSAID: the unsaid part is a row, for REC-REFUSE-WHY.
variable RROW-UNSAID
\ RBIND-FULL: a forall past BINDER-DEPTH-MAX nested binders, for REC-REFUSE-WHY.
variable RBIND-FULL
variable RJSON   0 RJSON !               \ inside a JSON string: EMIT1 escapes
variable RDIAG-ON
variable RDIAG-FD   2 RDIAG-FD !
PTR-VARIABLE RDIAG-A
variable RDIAG-CAP
variable RDIAG-U
variable RDIAG-I

\ E-DIAG-CAPACITY refuses a record that does not fit the caller's buffer.
\ src/lib/error.f owns it but compiles after this file, so the same (code, name)
\ pair is re-registered here, the one form tools/error-code-lint.f admits.
package RDIAG
public
-2901 constant E-DIAG-CAPACITY
;package

\ RTAILS is set while TAIL-CAPTURE renders: every quotation then shows its row
\ tails.
variable RTAILS   0 RTAILS !
\ RNAMES is 1 while the first pass of REND-EFFECT or DECL-EFFECT records each row's
\ tail (RPOS) and 2 while its second pass names the rows that share one; 0 otherwise.
variable RNAMES   0 RNAMES !
\ RDECL is set while DECL-EFFECT renders a declared effect: its rows are named
\ as RNAMES names them, its atoms and families spelled as a signature writes them.
variable RDECL   0 RDECL !
: RFACTS? ( -- bool ) RNAMES @ 0 <>  RDECL @ 0=  and ;


: EMIT-RAW ( n -- )
   {: c:n :}
   RDST @ IF
     RSN @ RSBUF-CAP @ 2 - > IF RSBUF-GROW THEN
     c RSBUF RSN @ + c!  RSN @ 1 + RSN !
   ELSE c ECH c! ECH 1 type THEN ;


: JHEX ( n -- ) {: d:n :}
   d 10 < IF d 48 + ELSE d 55 + THEN EMIT-RAW ;

\ Escapes per RFC 8259 section 7: \n \r \t \" \\ by letter, every other control
\ byte as \u00XX in uppercase hex.
: JCHAR {: c :}
   c case
      10 of 92 EMIT-RAW 110 EMIT-RAW endof
      13 of 92 EMIT-RAW 114 EMIT-RAW endof
      9 of 92 EMIT-RAW 116 EMIT-RAW endof
      34 of 92 EMIT-RAW c EMIT-RAW endof
      92 of 92 EMIT-RAW c EMIT-RAW endof
      c 32 < IF
         92 EMIT-RAW 117 EMIT-RAW 48 EMIT-RAW 48 EMIT-RAW
         c 4 rshift JHEX  c 15 and JHEX
      ELSE c EMIT-RAW THEN
   endcase ;

\ Between JOPEN and JCLOSE every byte is escaped: a package may be named `\`.
: EMIT1 {: c :}
   RJSON @ IF c JCHAR ELSE c EMIT-RAW THEN ;
: EMIT-UNSAID ( -- ) 1 RQM !  63 EMIT1 ;
: JOPEN ( -- )   34 EMIT-RAW  -1 RJSON ! ;
: JCLOSE ( -- )  0 RJSON !  34 EMIT-RAW ;

: DIAG-BUFFER! ( ptr u8 n -- )
   {: a:ptr cap:n :}
   a RDIAG-A !
   cap RDIAG-CAP !
   0 RDIAG-U !
   -1 RDIAG-ON ! ;

: DIAG-BUFFER-OFF ( -- )
   0 RDIAG-ON !
   0 RDIAG-U ! ;

: DIAG-FD! ( n -- )
   RDIAG-FD ! ;

\ RDIAG-A is a pointer cell, so a plain fetch keeps the ptr u8 view the byte
\ store in RDIAG-COPY needs.
: DIAG-BUFFER$ ( -- ptr u8 n )
   RDIAG-A @ RDIAG-U @ ;

: RDIAG-COPY ( ptr u8 n -- )
   {: a:ptr u:n :}
   0 RDIAG-I !
   BEGIN RDIAG-I @ u < WHILE
      a RDIAG-I @ + c@
      RDIAG-A @ RDIAG-U @ + RDIAG-I @ + c!
      RDIAG-I @ 1 + RDIAG-I !
   REPEAT ;

\ The caller's buffer cannot grow: a record that does not fit is refused whole by
\ a throw, and the buffer keeps every record before it.
: RDIAG-APPEND ( ptr u8 n -- )
   {: a:ptr u:n :}
   RDIAG-ON @ 0= IF RDIAG-FD @ a u write drop EXIT THEN
   RDIAG-U @ u + RDIAG-CAP @ > IF RDIAG:E-DIAG-CAPACITY throw THEN
   a u RDIAG-COPY
   RDIAG-U @ u + RDIAG-U ! ;

\ Rendering returns to stdout before the append, so a refused record leaves the
\ renderer as a delivered one does.
: RSBUF-FLUSH ( -- )
   RSBUF RSN @
   0 RDST !  0 RSN !
   RDIAG-APPEND ;
PERSISTED-PTR-VARIABLE SEEN-P   NULL-PTR SEEN-P !
variable SEEN-CAP   0 SEEN-CAP !
\ Only the assigned prefix needs clearing; allocation initializes every cell
\ above SEEN-HW to UNBOUND, including a grown tail.
variable SEEN-HW   0 SEEN-HW !
variable NLET                                      \ SEEN is indexed by typevar (PAY)
variable RBIND-N                                   \ lexical names while rendering forall
64 constant RATOM-CAP
create RATOM-KEY RATOM-CAP cells allot
variable RATOM-N
variable RATOM-I
$7FFFFFFFFFFFFFFF CELL / constant SEEN-MAX-CAP

: SEEN-UNMAP-RC ( ptr a n -- n ) {: base:ptr cap:n :}
   cap 0= IF 0 EXIT THEN
   base cap cells munmap ;

: SEEN-GROW-CAP ( n -- n ) {: need:n :}
   need MAXTV-INIT max
   SEEN-CAP @ SEEN-MAX-CAP 2 / <= IF SEEN-CAP @ 2 * max THEN ;

: SEEN-GROW ( n -- ) {: need:n :}
   need SEEN-GROW-CAP {: nc:n :}
   SEEN-CAP @ {: oc:n :}
   nc cells ARENA-ALLOC {: next:ptr :}
   oc 0 > IF SEEN-P @ next oc ARENA-CELLS-COPY THEN
   next oc nc ARENA-CELLS-UNBOUND
   SEEN-P @ oc SEEN-UNMAP-RC 0 <> IF
      next nc SEEN-UNMAP-RC 0 <> IF s" render: seen cleanup munmap failed" 76 die THEN
      s" render: seen munmap failed" 76 die
   THEN
   next SEEN-P !   nc SEEN-CAP ! ;

: SEEN-ENSURE ( -- )
   MAXTV MAXTV-INIT max {: need:n :}
   need 0 < need SEEN-MAX-CAP > or IF s" render: seen capacity overflow" 76 die THEN
   need SEEN-CAP @ <= IF EXIT THEN
   need SEEN-GROW ;

: SEEN ( -- ptr n ) SEEN-ENSURE SEEN-P @ ;

: SEEN-RESET ( -- )
   SEEN-ENSURE
   0 BEGIN dup SEEN-HW @ < WHILE
      UNBOUND over cells SEEN + !
      1 +
   REPEAT drop
   0 SEEN-HW ! 0 RBIND-N ! ;


\ Unnames every variable SEEN named at or past index idx.
: SEEN-FORGET ( n -- )
   {: idx:n :}
   0 BEGIN dup SEEN-HW @ < WHILE
      dup cells SEEN + @ idx >= IF UNBOUND over cells SEEN + ! THEN
      1 +
   REPEAT drop ;


: RATOM-RESET ( -- )
   0 RATOM-N ! ;

: SEEN-IDX ( n -- n )
   {: vp:n :}
   SEEN-ENSURE
   vp 1 + SEEN-HW @ < 0= IF vp 1 + SEEN-HW ! THEN
   vp cells SEEN + @ UNBOUND = IF NLET @ vp cells SEEN + ! NLET @ 1 + NLET ! THEN
   vp cells SEEN + @ ;
: RATOM-CHAR ( n -- n ) {: idx:n :}
   idx 26 < IF idx 97 + ELSE 1 RQM ! 63 THEN ;
: RATOM-FIND ( n -- n bool ) {: key:n :}
   0 RATOM-I !
   BEGIN RATOM-I @ RATOM-N @ < WHILE
      RATOM-I @ cells RATOM-KEY + @ key = IF RATOM-I @ 0 0= EXIT THEN
      RATOM-I @ 1 + RATOM-I !
   REPEAT
   0 1 0= ;
: RATOM-ADD ( n -- n ) {: key:n :}
   RATOM-N @ RATOM-CAP >= IF 1 RQM ! 0 EXIT THEN
   key RATOM-N @ cells RATOM-KEY + !
   RATOM-N @
   RATOM-N @ 1 + RATOM-N ! ;
: RATOM-ORD ( n -- n ) {: key:n :}
   key RATOM-FIND IF EXIT THEN drop
   key RATOM-ADD ;

: RSTR ( ptr u8 n -- ) {: a:ptr u:n :}
   0 BEGIN dup u < WHILE dup a + c@ EMIT1 1 + REPEAT drop ;

\ RFOLD emits the case-folded spelling. Diagnostics name identifiers folded; only
\ a declaring-package name reaches the renderer unfolded, because TFAM-DECL
\ interns the spelling the source `package` line typed.
: RFOLD ( ptr u8 n -- ) {: a:ptr u:n :}
   0 BEGIN dup u < WHILE dup a + c@ CORE-FOLD-C EMIT1 1 + REPEAT drop ;

: CON-OUT ( n -- ) {: p:n :}
   p 2 = IF 102 EMIT1 ELSE
   p 0 > p CTN @ < and IF
      p CT-NAME$ dup 0 <> IF RSTR ELSE 2drop EMIT-UNSAID THEN
   ELSE EMIT-UNSAID THEN THEN ;

\ Facts text (RFACTS?) writes a plain atom as `'NAME`, which TOK-TYPE reads back.
\ A fresh atom's ordinal tells two identities of one name apart; a signature
\ keys a fresh name by its spelling, so its declared effect (RDECL) writes none.
: ATOM-REND {: t :}
   t ATOM>K 0 = IF
      RFACTS? IF 39 EMIT1 THEN
      t ATOM>A t ATOM>U RSTR EXIT THEN
   s" fresh-" RSTR
   t ATOM>A t ATOM>U RSTR
   RDECL @ IF EXIT THEN
   45 EMIT1
   t ATOM>K RATOM-ORD RATOM-CHAR EMIT1 ;

: RNUM ( n -- )                 \ small non-negative number (hidden slot index)
   dup 10 >= IF dup 10 / RECURSE THEN
   10 mod 48 + EMIT1 ;


\ Name index idx: a..z without f, n and r, which the parser reads as types,
\ then the ordinal (checker.f SIG-VAR-SLOT reads both back).
: VAR-NAME ( n -- )
   {: idx:n :}
   idx 0 < idx TYPE-VAR-MAX >= or IF 1 RQM !  63 EMIT1 EXIT THEN
   idx 22 > IF idx RNUM EXIT THEN
   idx 97 +
   dup 102 >= IF 1+ THEN
   dup 110 >= IF 1+ THEN
   dup 114 >= IF 1+ THEN EMIT1 ;


\ A term minted before the registry loads holds an id outside it. A slot a
\ partial rollback reused names its new occupant; diagnostics render before it.
: FAM-INTERNED? ( n -- f )
   {: fam:n :}
   fam 0 >=  fam TFAM-N@ <  and ;


\ Exact: both sides read a package's one interned spelling. Facts text (RFACTS?)
\ names every package but "" ("@" included), so it reads back with no scope.
: FAM-FOREIGN? ( n -- f )
   {: fam:n :}
   fam TFAM-PKG$ {: pa:ptr pu:n :}
   pu 0 = IF RES-FALSE EXIT THEN
   RFACTS? IF RES-TRUE EXIT THEN
   pa pu s" @" CORE-STR= IF RES-FALSE EXIT THEN
   pa pu TFAM-ACTIVE-PKG$ CORE-STR= 0= ;


\ The package folds: packages compare case-insensitively (TFAM-PKG-MATCH?).
: FAM-QNAME-REND ( n -- )
   {: fam:n :}
   fam FAM-FOREIGN? IF fam TFAM-PKG$ RFOLD 58 EMIT1 THEN
   fam TFAM-NAME$ RSTR ;


\ A T-PARAM's name span may point into a scan buffer later tokens overwrite
\ (TKF), so an interned family's name comes from the registry.
: FAM-NAME-REND ( n -- )
   {: t:n :}
   t PARAM>FAM {: fam:n :}
   fam FAM-INTERNED? 0= IF t PARAM>NAME-A t PARAM>NAME-U RSTR EXIT THEN
   fam FAM-QNAME-REND ;


\ A hidden physical field renders as the diagnostic-only '@family.slotN<args>'
\ or '@family.tag<args>' form and sets RQM, so REC-SIG never records a lone
\ hidden cell. A full run never reaches here: REND-COLLECT compacts it to the
\ logical family type.
: PARAM-HEAD {: t:n :}
   t HIDDEN-PARAM? IF
      1 RQM !
      64 EMIT1
      t FAM-NAME-REND
      46 EMIT1
      t HIDDEN-SLOT@  t T-WIDTH 1 -  = IF
         s" tag" RSTR
      ELSE
         s" slot" RSTR  t HIDDEN-SLOT@ RNUM
      THEN
   ELSE
      t FAM-NAME-REND
   THEN ;


\ HRS: the run holds a cell `catch` left stale. The whole logical value is
\ unusable, so the row prints `stale<option<pt>>` once, never three cells.
variable HRC  variable HRI  variable HRF  variable HRS
: HID-RUN-CELL? ( n n n -- bool ) {: node:n fam:n slot:n :}
   node TAG S-PUSH <> IF RES-FALSE EXIT THEN
   node P>TYPE T-RES {: t:n :}
   t HIDDEN-PARAM? 0= IF RES-FALSE EXIT THEN
   t CELL>FAM fam <> IF RES-FALSE EXIT THEN
   t HIDDEN-SLOT@ slot <> IF RES-FALSE EXIT THEN
   t TAG T-STALE = IF -1 HRS ! THEN
   RES-TRUE ;


\ True when node heads a full run of its family, tag at T-WIDTH-1 down to slot0;
\ rest is the row below the run, else below node, whose cell renders as '@'.
: HID-RUN-REST ( n -- n bool ) {: node:n :}
   node P>TYPE T-RES {: t:n :}
   t CELL>FAM {: fam:n :}
   t T-WIDTH {: w:n :}
   t HIDDEN-SLOT@ w 1 - <> IF node P>REST RES-FALSE EXIT THEN
   node HRC !  -1 HRF !
   t TAG T-STALE = IF -1 ELSE 0 THEN HRS !
   w 1 - HRI !
   BEGIN HRI @ 0 >  HRF @ 0 <>  and WHILE
      HRC @ P>REST R-RES  fam  HRI @ 1 -  HID-RUN-CELL? IF
         HRC @ P>REST R-RES HRC !
      ELSE
         0 HRF !
      THEN
      HRI @ 1 - HRI !
   REPEAT
   HRF @ 0 <> IF HRC @ P>REST RES-TRUE ELSE node P>REST RES-FALSE THEN ;

6 constant QDEPTH-MAX                        \ a diagnostic's nesting budget
64 constant RBUF-CAP                         \ values a diagnostic shows of one row
create RBIND BINDER-DEPTH-MAX cells allot    \ the name index of each binder in scope
\ A row renders from RBUF without recursion: a nested quotation's row pushes
\ above the rows that hold it, so QREND recurses once per level, never per value.
create RBUF EFFECT-DEPTH-MAX cells allot   variable RBN

\ An effect's text (RNAMES nonzero) says every level a record holds.
: QDEPTH-CAP ( -- n ) RNAMES @ 0 <> IF EFFECT-DEPTH-MAX ELSE QDEPTH-MAX THEN ;


\ A push past RBUF's end sets RQM: an effect's text that cannot hold every value
\ is refused as unsayable, never cut.
: RBUF+ ( n -- )
   RBN @ EFFECT-DEPTH-MAX >= IF drop 1 RQM ! EXIT THEN
   RBN @ cells RBUF + !  RBN @ 1 + RBN ! ;


\ Push row s's values on RBUF from RBN, its bottom value last. A diagnostic
\ (RNAMES 0) pushes a row's top RBUF-CAP values; an effect's text pushes all.
: RBUF-ROW ( n -- )
   {: s:n :}
   RBN @ RBUF-CAP + {: stop:n :}
   s
   BEGIN R-RES dup TAG S-PUSH =  RNAMES @ 0 <>  RBN @ stop <  or  and WHILE
     dup P>TYPE T-RES HIDDEN-PARAM? IF
        dup HID-RUN-REST IF
           swap P>TYPE T-RES MK-LOGICAL  HRS @ IF MK-STALE THEN  RBUF+
        ELSE
           swap P>TYPE RBUF+
        THEN
     ELSE
        dup P>TYPE RBUF+  P>REST
     THEN
   REPEAT drop ;


: QRET? ( n -- bool ) {: q:n :}  q Q>RIN R-RES  q Q>ROUT R-RES  <> ;

\ A refusal for an escaped callable tail (MD-TAIL-ESCAPE, TAIL-CAPTURE) names
\ every quotation's row tails, so the callable that keeps its input stack shows
\ it: `[ C -- C n ]` against `[ A -- B ]`. Rows letter in their own namespace, A
\ to Z, then '?'. Facts text and a declared effect (RNAMES) name rows; every other
\ rendering shows cells only.
26 constant RROW-CAP
create RROW-KEY RROW-CAP cells allot   variable RROW-N   0 RROW-N !


: RROW-LETTER ( n -- n )
   {: t:n :}
   t PAY {: id:n :}
   0 BEGIN dup RROW-N @ < WHILE
      dup cells RROW-KEY + @ id = IF 65 + EXIT THEN
      1 +
   REPEAT drop
   RROW-N @ RROW-CAP >= IF 1 RROW-UNSAID !  1 RQM !  63 EXIT THEN
   id RROW-N @ cells RROW-KEY + !
   RROW-N @ 65 +  RROW-N @ 1 + RROW-N ! ;


: QTAIL ( n -- )
   {: row:n :}
   row ROW-TAIL RROW-LETTER EMIT1  32 EMIT1 ;


\ RPOS holds a (tail, owner) pair per row a first pass (RNAMES 1) visits: owner
\ 0 is the effect's data rows, 1 its return rows, 2v and 2v+1 the v-th quotation's.
PERSISTED-PTR-VARIABLE RPOS-P   NULL-PTR RPOS-P !
variable RPOS-CAP   0 RPOS-CAP !          \ pairs mapped at RPOS-P, 0 for none
variable RPOS-N   variable RVISIT
: RPOS ( -- ptr n ) RPOS-P @ ;

: RPOS+ ( n n -- )
   {: t:n owner:n :}
   RPOS-P RPOS-CAP RPOS-N @ 1 + 2 ARENA-ROWS-ENSURE
   t RPOS-N @ 2 * cells RPOS + !
   owner RPOS-N @ 2 * 1 + cells RPOS + !
   RPOS-N @ 1 + RPOS-N ! ;


\ RQUOT-BLOCK holds, per quotation RVISIT numbered (from 1, as its `[` is
\ written), its resolved block part, for REND-FACTS's block and noblock tags.
create RQUOT-BLOCK SQUOT-CAP cells allot

: RQUOT+ ( n -- ) {: q:n :}
   RVISIT @ SQUOT-CAP > IF 1 RQM ! EXIT THEN
   q Q>BLOCK T-RES  RVISIT @ 1 - cells RQUOT-BLOCK + ! ;

: RQUOT@ ( n -- n ) 1 - cells RQUOT-BLOCK + @ ;


\ A row is named when its tail has no scope, its kind is not its unnamed form's
\ (RVK-QUOT in a quotation, else 0), or the tail is not its owner's two rows' alone.
: RPOS-NAMED? ( n n -- bool )
   {: t:n owner:n :}
   t ISROW IF
      t PAY NSCOPE@ IF RES-TRUE EXIT THEN
      t PAY TVK@  owner 2 >= IF RVK-QUOT ELSE 0 THEN  <> IF RES-TRUE EXIT THEN
   THEN
   0 0
   RPOS-N @ 0 ?DO
      i 2 * cells RPOS + @ t = IF
         swap 1 + swap
         i 2 * 1 + cells RPOS + @ owner <> IF 1 + THEN
      THEN
   LOOP
   {: hits:n others:n :}
   hits 2 <>  others 0 <>  or ;


\ The letter a second pass (RNAMES 2) writes at the bottom of a row, 0 for none.
: RPOS-LETTER ( n n -- n )
   {: row:n owner:n :}
   row ROW-TAIL {: t:n :}
   RNAMES @ 1 = IF t owner RPOS+ 0 EXIT THEN
   t owner RPOS-NAMED? IF t RROW-LETTER ELSE 0 THEN ;


: RROW-NAME ( n n -- )
   {: row:n owner:n :}
   RNAMES @ 0= IF EXIT THEN
   row owner RPOS-LETTER {: l:n :}
   l 0 <> IF l EMIT1 32 EMIT1 THEN ;


\ The return clause of quotation q whose sides resolve to one row. The parser
\ admits only `| U -- U` there, so a row holding cells sets RQM.
: QRET-NAME ( n n -- )
   {: q:n owner:n :}
   q Q>RIN R-RES TAG S-PUSH = IF 1 RQM !  1 RROW-UNSAID ! EXIT THEN
   q Q>RIN owner RPOS-LETTER {: l:n :}
   q Q>ROUT owner RPOS-LETTER drop
   l 0 = IF EXIT THEN
   124 EMIT1 32 EMIT1  l EMIT1 32 EMIT1  45 EMIT1 45 EMIT1 32 EMIT1  l EMIT1 32 EMIT1 ;


\ QREND ( x d mode -- ): mode>0 renders row x bottom value first, each value
\ followed by a space; mode=0 renders a type.
: QREND ( n n n -- )
   {: x:n d:n mode:n :}
   mode 0 > IF
      RBN @ {: base:n :}
      x RBUF-ROW
      RBN @ BEGIN dup base > WHILE 1 - dup cells RBUF + @ d 0 RECURSE 32 EMIT1 REPEAT drop
      base RBN !
      EXIT
   THEN
   x T-RES {: r:n :}
   r TAG case
      T-VAR of r PAY SEEN-IDX VAR-NAME endof
      T-CON of r PAY CON-OUT endof
      T-PTR of s" ptr " RSTR  r PTR>INNER d 0 RECURSE endof
      \ A cell a caught throw may have overwritten names the type it lost.
      \ `stale<...>` is not a declarable type, so it sets RQM: REC-SIG never
      \ records a row carrying one.
      T-STALE of
        1 RQM !
        s" stale<" RSTR  r STALE>INNER d 0 RECURSE  62 EMIT1
      endof
      T-QUOT of
        d QDEPTH-CAP < IF
           RTAILS @ 0 <> {: named:bool :}
           RNAMES @ 0 <> IF RVISIT @ 1 + RVISIT !  r RQUOT+ THEN  RVISIT @ 2 * {: owner:n :}
           91 EMIT1 32 EMIT1  named IF r Q>DIN QTAIL THEN
           r Q>DIN owner RROW-NAME  r Q>DIN d 1+ 1 RECURSE
           45 EMIT1 45 EMIT1 32 EMIT1  named IF r Q>DOUT QTAIL THEN
           r Q>DOUT owner RROW-NAME  r Q>DOUT d 1+ 1 RECURSE
           r QRET? IF
              RNAMES @ 0 <> IF 1 RQM !  1 RROW-UNSAID ! THEN
              124 EMIT1 32 EMIT1  r Q>RIN d 1+ 1 RECURSE
              45 EMIT1 45 EMIT1 32 EMIT1  r Q>ROUT d 1+ 1 RECURSE
           ELSE RNAMES @ 0 <> IF r owner 1 + QRET-NAME THEN THEN
           93 EMIT1
        ELSE EMIT-UNSAID THEN
      endof
      T-FORALL of
        NLET @ TYPE-VAR-MAX >=  RBIND-N @ BINDER-DEPTH-MAX >=  or IF
           RBIND-N @ BINDER-DEPTH-MAX >= IF 1 RBIND-FULL ! THEN
           NLET @ 1 + NLET ! EMIT-UNSAID
        ELSE d QDEPTH-CAP < IF
           NLET @ {: idx:n :}
           idx RBIND-N @ cells RBIND + !
           NLET @ 1 + NLET !
           r F>DOMAIN BIND-REGION = IF s" forall-region<" ELSE s" forall<" THEN RSTR idx VAR-NAME
           r F>PARENT dup 0= IF drop ELSE
              r F>SECOND 0= IF s"  inside " RSTR d 1+ 0 RECURSE ELSE
                 s"  inside [" RSTR d 1+ 0 RECURSE  44 EMIT1
                 r F>SECOND d 1+ 0 RECURSE  93 EMIT1 THEN THEN
           RBIND-N @ 1 + RBIND-N !
           44 EMIT1
           r F>BODY d 1+ 0 RECURSE 62 EMIT1
           RBIND-N @ 1 - RBIND-N !
           \ A body that named no free variable frees the name for a sibling.
           NLET @ idx 1 + = IF idx NLET ! THEN
        ELSE EMIT-UNSAID THEN THEN
      endof
      T-BVAR of
        r B>DEPTH 0 >= r B>DEPTH RBIND-N @ < and IF
           RBIND-N @ r B>DEPTH - 1 - cells RBIND + @ VAR-NAME
        ELSE EMIT-UNSAID THEN
      endof
      T-SCOPE of
        1 RQM !
        s" <live-scope-" RSTR r PAY RNUM 62 EMIT1
      endof
      T-ATOM of r ATOM-REND endof
      \ A family applied to no arguments renders as its bare name, the spelling a
      \ signature uses: a bare family token reads back as `name<>`.
      T-PARAM of
        d QDEPTH-CAP < IF
           r PARAM-HEAD
           r PARAM>ARGC 0 > IF
              60 EMIT1
              0 BEGIN dup r PARAM>ARGC < WHILE
                dup 0 > IF 44 EMIT1 THEN
                r over PARAM>ARG d 1+ 0 RECURSE
                1 +
              REPEAT drop 62 EMIT1
           THEN
        ELSE EMIT-UNSAID THEN
      endof
      EMIT-UNSAID
   endcase ;


: REND-TYPE {: t:n :}  t 0 0 QREND ;
variable RSHOW-DST

: REND-COLLECT ( n -- )  0 RBN !  RBUF-ROW ;

: RENDER  SEEN-RESET RATOM-RESET 0 NLET !  DCUR @ REND-COLLECT
   RBN @ BEGIN dup 0 > WHILE 1 - dup cells RBUF + @ REND-TYPE 32 EMIT1 REPEAT drop ;

: SHOW-LOCAL-TYPE ( ptr u8 n n -- ) {: name:ptr nameu:n t:n :}
   RDST @ RSHOW-DST !
   0 RDST !
   s" inferred " RSTR
   name nameu RSTR
   s" : " RSTR
   SEEN-RESET RATOM-RESET 0 NLET !
   t REND-TYPE
   10 EMIT1
   RSHOW-DST @ RDST ! ;
: LOCSHOW-INSTALL ( -- ) [: SHOW-LOCAL-TYPE ;] is LOCSHOWXT ;
LOCSHOW-INSTALL

\ DIAG-PRINT writes one reject line to stderr, its packet's suggestion last:
\   habu: in NAME: at 'TOK' expected: <row> actual: <row> ; suggestion: <text>
\ A top-level step's line names its place instead (DSITE): habu: PATH:LINE: ...
\ expected and actual appear only when the failing unify was captured.
: DTXT ( ptr u8 n -- ) {: a:ptr u:n :}
   0 BEGIN dup u < WHILE dup a + c@ EMIT1 1 + REPEAT drop ;

: DROW {: s :}  s REND-COLLECT
   RBN @ BEGIN dup 0 > WHILE 1 - dup cells RBUF + @ REND-TYPE 32 EMIT1 REPEAT drop ;


: REND-PASS ( n n n n bool -- )
   {: din:n dout:n rin:n rout:n hasr:bool :}
   0 RQM !  0 RROW-UNSAID !  0 RBIND-FULL !  SEEN-RESET RATOM-RESET 0 NLET !  0 RROW-N !  0 RVISIT !
   din 0 RROW-NAME  din DROW
   hasr IF  s" | " DTXT  rin 1 RROW-NAME  rin DROW  THEN
   45 EMIT1  45 EMIT1
   dout 0 RPOS-LETTER {: lo:n :}
   lo 0 <> IF 32 EMIT1 lo EMIT1 THEN
   dout REND-COLLECT
   RBN @ BEGIN dup 0 > WHILE 1 - 32 EMIT1 dup cells RBUF + @ REND-TYPE REPEAT drop
   hasr IF  s"  | " DTXT  rout 1 RROW-NAME  rout DROW  THEN ;


\ Both passes write from the RSN it is called with, after what the caller wrote:
\ the first records the row tails, the second writes them as RPOS-NAMED? names them.
: REND-EFFECT ( n n n n bool -- ptr u8 n )
   {: din:n dout:n rin:n rout:n hasr:bool :}
   RSN @ {: base:n :}
   1 RDST !  0 RPOS-N !
   1 RNAMES !  base RSN !  din dout rin rout hasr REND-PASS
   2 RNAMES !  base RSN !  din dout rin rout hasr REND-PASS
   0 RNAMES !  0 RDST !  RSBUF RSN @ ;


: FACT-TAG ( ptr u8 n n -- ) {: a:ptr u:n letter:n :}
   a u DTXT  32 EMIT1  letter EMIT1  32 EMIT1 ;


: VAR-FACT-TAG ( ptr u8 n n -- )
   {: a:ptr u:n idx:n :}
   a u DTXT  32 EMIT1  idx VAR-NAME  32 EMIT1 ;

\ The type variable REND-EFFECT gave name index idx, -1 for a forall's name.
: SEEN-VAR ( n -- n ) {: idx:n :}
   0 BEGIN dup SEEN-HW @ < WHILE
      dup cells SEEN + @ idx = IF EXIT THEN
      1 +
   REPEAT drop -1 ;

: VAR-KIND-TAG ( n -- ) {: idx:n :}
   idx SEEN-VAR {: id:n :}
   id 0 < IF EXIT THEN
   id TVK@ KIND>TAG$ {: a:ptr u:n :}
   u 0 = IF EXIT THEN
   a u idx VAR-FACT-TAG ;

: VAR-SCOPE-TAG ( n -- ) {: idx:n :}
   idx SEEN-VAR {: id:n :}
   id 0 < IF EXIT THEN
   id NSCOPE@ IF s" noscope" idx VAR-FACT-TAG THEN
   id NLINEAR@ IF s" nolinear" idx VAR-FACT-TAG THEN ;

: ROW-SCOPE-TAG ( n -- ) {: idx:n :}
   idx cells RROW-KEY + @ NSCOPE@ IF s" noscope" idx 65 + FACT-TAG THEN ;

: ROW-KIND-TAG ( n -- ) {: idx:n :}
   idx cells RROW-KEY + @ TVK@ {: kind:n :}
   kind 0 = IF EXIT THEN
   kind KIND>TAG$ {: a:ptr u:n :}
   u 0 = IF 1 RQM ! EXIT THEN
   a u idx 65 + FACT-TAG ;

\ The first quotation whose block part is quotation q's, q when none before it is.
: RQUOT-FIRST ( n -- n ) {: q:n :}
   1 BEGIN dup q < WHILE
      dup RQUOT@ q RQUOT@ = IF EXIT THEN
      1 +
   REPEAT ;

\ `block N q` when quotation q's block part is a variable an earlier N's is.
: QUOT-BLOCK-TAG ( n -- ) {: q:n :}
   q RQUOT@ ISVAR 0= IF EXIT THEN
   q RQUOT-FIRST {: first:n :}
   first q = IF EXIT THEN
   s" block " DTXT  first RNUM  32 EMIT1  q RNUM  32 EMIT1 ;

\ `noblock q` when quotation q's block part is a flagged variable no earlier one's is.
: QUOT-NOBLOCK-TAG ( n -- ) {: q:n :}
   q RQUOT@ ISVAR 0= IF EXIT THEN
   q RQUOT-FIRST q <> IF EXIT THEN
   q RQUOT@ PAY NSCOPE@ 0= IF EXIT THEN
   s" noblock " DTXT  q RNUM  32 EMIT1 ;

\ Facts text, `( E ) offset a noscope A nolinear a block 1 2 noblock 1 `,
\ written from the RSN it is called with: a caller that wants a fresh text sets
\ `0 RSN !` first.
: REND-FACTS ( n n n n bool -- ptr u8 n )
   {: din:n dout:n rin:n rout:n hasr:bool :}
   1 RDST !  s" ( " DTXT
   din dout rin rout hasr REND-EFFECT 2drop
   1 RDST !  s"  ) " DTXT
   0 BEGIN dup NLET @ < WHILE dup VAR-KIND-TAG 1 + REPEAT drop
   0 BEGIN dup NLET @ < WHILE dup VAR-SCOPE-TAG 1 + REPEAT drop
   0 BEGIN dup RROW-N @ < WHILE dup ROW-KIND-TAG 1 + REPEAT drop
   0 BEGIN dup RROW-N @ < WHILE dup ROW-SCOPE-TAG 1 + REPEAT drop
   RVISIT @ SQUOT-CAP min 1 + {: nq:n :}
   2 BEGIN dup nq < WHILE dup QUOT-BLOCK-TAG 1 + REPEAT drop
   1 BEGIN dup nq < WHILE dup QUOT-NOBLOCK-TAG 1 + REPEAT drop
   0 RDST !  RSBUF RSN @ ;


: CTL-FLAG-TAG ( n n -- )
   {: ctl:n bit:n :}
   ctl 1 bit lshift and 0= IF EXIT THEN
   bit CTL-BIT>TAG$ {: a:ptr u:n :}
   u 0 = IF EXIT THEN
   a u DTXT  32 EMIT1 ;


: CTL-THROWS-TAG ( n -- )
   {: ctl:n :}
   ctl CTL-THROW and 0= IF EXIT THEN
   s" throws " DTXT  ctl XFER-DMASK RNUM  32 EMIT1  ctl XFER-RMASK RNUM  32 EMIT1 ;


: CTL-INTRINSIC-TAG ( n -- )
   {: ctl:n :}
   ctl CTL>INTRINSIC {: id:n :}
   id 0 = IF EXIT THEN
   id INTRINSIC-NAME$ {: a:ptr u:n :}
   u 0 = IF 1 RQM ! EXIT THEN
   s" intrinsic " DTXT  a u DTXT  32 EMIT1 ;


\ Control word n's tags for checker.f SIG-CTL-TAG, in bit order, appended at RSN.
\ RQM: a bit XFER-PACK does not keep, an unnamed intrinsic id, masks without CTL-THROW.
: REND-CTL ( n -- ptr u8 n )
   {: ctl:n :}
   1 RDST !
   ctl XFER-FLAGS  ctl XFER-DMASK  ctl XFER-RMASK  XFER-PACK  ctl <> IF 1 RQM ! THEN
   ctl CTL-THROW and 0=  ctl XFER-DMASK ctl XFER-RMASK or 0 <>  and IF 1 RQM ! THEN
   ctl 0 CTL-FLAG-TAG
   ctl CTL-THROWS-TAG
   19 2 ?do
      i CTL-INTRINSIC-SHIFT = IF ctl CTL-INTRINSIC-TAG THEN
      ctl i CTL-FLAG-TAG
   loop
   0 RDST !  RSBUF RSN @ ;


\ A PE row's flags as facts tags appended at RSN; the PE-ACTIVE, PE-TRUSTED-ONLY
\ and PE-TOP-ONLY bits are facts a caller's check reads. `primitive` (PE-ACTIVE)
\ makes the read a primitive row, whose tick is an opaque xt: without it a text
\ reads as the word's declaration, whose tick is its quotation (`['] die`).
: REND-PE ( n -- ptr u8 n )
   {: flags:n :}
   1 RDST !
   flags PE-ACTIVE and 0 <> IF s" primitive " DTXT THEN
   flags PE-TRUSTED-ONLY and 0 <> IF s" trusted-only " DTXT THEN
   flags PE-TOP-ONLY and 0 <> IF s" top-only " DTXT THEN
   0 RDST !  RSBUF RSN @ ;


\ A parsing immediate's payload count as a facts tag appended at RSN, nothing
\ for -1.
: REND-PIMM ( n -- ptr u8 n )
   {: cnt:n :}
   1 RDST !
   cnt 0 >= IF s" parse-imm " DTXT  cnt RNUM  32 EMIT1 THEN
   0 RDST !  RSBUF RSN @ ;


\ Record off's facts text, appended at RSN.
: REND-RECORD ( n -- ptr u8 n )
   {: off:n :}
   off E-PTR E-INST-RESET
   off E-PTR E-HASR@ 0 <> {: hasr:bool :}
   off E-PTR E-DIN@ E-INST  off E-PTR E-DOUT@ E-INST
   hasr IF off E-PTR E-RIN@ E-INST  off E-PTR E-ROUT@ E-INST ELSE 0 0 THEN
   hasr REND-FACTS ;


\ `creates-effect ` and the facts text of n, a created effect as checker.f
\ NORET-CREATES@ answers it, appended at RSN; 0 appends nothing. RQM stays set.
: REND-CREATES ( n -- ptr u8 n )
   {: n:n :}
   n 0= IF 0 RDST !  RSBUF RSN @ EXIT THEN
   RQM @ {: qm:n :}
   1 RDST !  s" creates-effect " DTXT
   n 1 - REND-RECORD
   qm 0 <> IF 1 RQM ! THEN ;


\ `overload `, primitive row k's effect and its tags, appended at RSN; checker.f
\ SIG-TAGS reads the rest of the text as the primitive's next row, so the row
\ needs no `primitive`.
: REND-OVERLOAD ( n -- ptr u8 n )
   {: k:n :}
   1 RDST !  s" overload " DTXT
   k PE-EFF@ REND-RECORD 2drop
   k PE-FLAGS@ PE-ACTIVE invert and REND-PE ;


\ FACTS-SAME?'s state for the quotations it runs; FACTS-CLEAN zeroes it, so
\ none outlives a call. FACTS-NEW is the parsed record, offset+1, 0 for none.
variable FACTS-REC   variable FACTS-MARK   variable FACTS-WAS   variable FACTS-NEW


\ Writes record off's facts text into RSBUF from its start.
: FACTS-REND ( n -- )
   NEW  0 RSN !  REND-RECORD 2drop ;


: FACTS-PARSE ( -- n n n n )
   RES-TRUE SIG-FACTS!
   [: RSBUF RSN @ PARSE-SIG-RAW ;] [: RES-FALSE SIG-FACTS! ;] finally ;


\ The record is built under symbol 0, as CREATED-RECORD-BUILD builds one, so no
\ lookup sees it; a row no record can hold is refused before E-BUILD-EFFECT dies.
: FACTS-BUILD ( -- )
   NEW
   FACTS-PARSE {: din:n dout:n rin:n rout:n :}
   SGBAD @ 0 <>  SIG-UNRES @ 0 <>  or IF EXIT THEN
   RSBUF RSN @ din dout rin rout SGBAD-UNFIT!
   SGBAD @ 0 <> IF EXIT THEN
   0 CHECKER-REC-SYM !
   din dout rin rout SGHASR @ E-BUILD-EFFECT 1 + FACTS-NEW ! ;


\ A throw from the parse refuses the text as SGBAD does.
: FACTS-ANSWER ( -- bool )
   [: FACTS-BUILD ;] catch {: rc:n :}
   rc 0 <> IF RES-FALSE EXIT THEN
   FACTS-NEW @ 0= IF RES-FALSE EXIT THEN
   FACTS-REC @ E-PTR ER.CONTENT @  FACTS-NEW @ 1 - E-PTR ER.CONTENT @ E-CONTENT-SAME? ;


: FACTS-CLEAN ( -- )
   FACTS-MARK @ USIGS-RESTORE-END
   FACTS-WAS @ CHECKER-REC-SYM !
   SIGSCOPE-OFF  SGBAD-CLEAR  SIG-UNRES-CLEAR
   0 FACTS-REC !  0 FACTS-MARK !  0 FACTS-WAS !  0 FACTS-NEW ! ;


\ Whether record off, sym's, survives its facts text: rendered, parsed in the
\ symbol's package and built again, the same content. RSBUF and RSN keep the text.
: FACTS-SAME-AT? ( n n -- bool )
   {: sym:n off:n :}
   off FACTS-REND
   RQM @ 0 <> IF RES-FALSE EXIT THEN
   off FACTS-REC !  UEND @ FACTS-MARK !  CHECKER-REC-SYM @ FACTS-WAS !  0 FACTS-NEW !
   SGBAD-CLEAR  SIG-UNRES-CLEAR
   sym SYM-PKG$ SIGSCOPE!
   [: FACTS-ANSWER ;] [: FACTS-CLEAN ;] finally ;

\ Whether sym's newest record survives its facts text, as FACTS-SAME-AT?.
: FACTS-SAME? ( n -- bool ) {: sym:n :} sym  sym USIG-NEWEST 1 -  FACTS-SAME-AT? ;


\ Inputs from the base row's instantiation (BROW), outputs from DCUR.
: REND-SIG ( -- ptr u8 n )
   0 RSN !  BROW @ DCUR @ 0 0 RES-FALSE REND-EFFECT ;

\ `JSON-DIAGS ON` emits one JSON object per reject or uncheckable verdict.
create JNBUF 20 allot  variable JNV  variable JNN
variable DSUGE  variable DSUGA
: JNUM
   JNV !  0 JNN !
   JNV @ 0= IF 48 EMIT1 EXIT THEN
   BEGIN JNV @ 0 > WHILE
      JNV @ 10 mod 48 +  JNBUF JNN @ + c!
      JNN @ 1 + JNN !
      JNV @ 10 / JNV !
   REPEAT
   JNN @ BEGIN dup 0 > WHILE
      1 - dup JNBUF + c@ EMIT1
   REPEAT drop ;

\ A signed number in decimal, as JNUM prints one from 0 up.
: SNUM ( n -- ) {: v:n :}
   v 0 >= IF v JNUM EXIT THEN
   45 EMIT1
   v 1 63 lshift = IF s" 9223372036854775808" DTXT EXIT THEN
   v negate JNUM ;
\ A tool's label (checker.f DIAG-FILE!) names a top step's file in place of its
\ path while the load reads that file or none.
: TOP-FILE$ ( -- ptr u8 n )
   CHECK:TOP-SITE-PATH$ {: pa:ptr pu:n :}
   pa pu DIAG-LABELS? IF DIAGFB DIAGFU @ EXIT THEN
   pa pu ;
\ A top step names its file, then the line checker.f CHECK:TOP-SITE
\ gives, if any; a step with no file and no label names no place.
: DSITE ( -- )
   TOP-STEP @ 0= IF s" habu: in " DTXT  NMA @ NMU @ DTXT  s" : " DTXT EXIT THEN
   s" habu: " DTXT
   TOP-FILE$ {: fa:ptr fu:n :}
   fu 0= IF EXIT THEN
   fa fu DTXT
   CHECK:TOP-SITE IF 58 EMIT1 JNUM ELSE drop THEN
   s" : " DTXT ;
: JSTR ( ptr u8 n -- )
   JOPEN DTXT JCLOSE ;
: JKEY ( ptr u8 n -- ) {: a:ptr u:n :}
   a u JSTR  58 EMIT1 ;
: JROW {: s :}  JOPEN  s DROW  JCLOSE ;
: SIG-WS? {: c :}  c 32 <= ;
: SIG-LTRIM ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 BEGIN dup u < WHILE
      dup a + c@ SIG-WS? 0= IF dup a + u rot - EXIT THEN
      1 +
   REPEAT drop a 0 ;
: SIG-RTRIM ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u BEGIN dup 0 > WHILE
      a over 1 - + c@ SIG-WS? IF 1 - ELSE a swap EXIT THEN
   REPEAT drop a 0 ;
: SIG-TRIM ( ptr u8 n -- ptr u8 n )  SIG-LTRIM SIG-RTRIM ;


\ Under RNAMES each row is named as REND-EFFECT names it (RROW-NAME).
: JEFFECT-TEXT ( n n n n bool -- )
   {: din:n dout:n rin:n rout:n hasr:bool :}
   din 0 RROW-NAME  din DROW  s" -- " DTXT  dout 0 RROW-NAME  dout DROW
   hasr IF
      s" | " DTXT  rin 1 RROW-NAME  rin DROW  s" -- " DTXT  rout 1 RROW-NAME  rout DROW
   THEN ;


\ One DECL-EFFECT pass over the declared rows, written from RSN at with the
\ names it started from.
: DECL-PASS ( n n n n bool n n n n -- )
   {: din:n dout:n rin:n rout:n hasr:bool at:n nlet:n rrow:n ratom:n :}
   at RSN !  nlet SEEN-FORGET  nlet NLET !  rrow RROW-N !  ratom RATOM-N !  0 RVISIT !
   din dout rin rout hasr JEFFECT-TEXT ;


\ The record SIG-EFF-CACHE! kept, rendered in two passes as REND-EFFECT is; the
\ live declared rows then take its names, unprinted, for the later fields.
: DECL-EFFECT ( -- )
   RECEFF @ {: off:n :}
   NLET @ RROW-N @ RATOM-N @ {: nlet:n rrow:n ratom:n :}
   off E-PTR E-INST-RESET
   off E-PTR E-HASR@ 0 <> {: hasr:bool :}
   off E-PTR E-DIN@ E-INST  off E-PTR E-DOUT@ E-INST
   hasr IF off E-PTR E-RIN@ E-INST  off E-PTR E-ROUT@ E-INST ELSE 0 0 THEN
   {: din:n dout:n rin:n rout:n :}
   JOPEN  RSN @ {: from:n :}
   -1 RDECL !  0 RPOS-N !
   1 RNAMES !  din dout rin rout hasr from nlet rrow ratom DECL-PASS
   2 RNAMES !  din dout rin rout hasr from nlet rrow ratom DECL-PASS
   0 RNAMES !  0 RDECL !
   JCLOSE
   nlet NLET !  rrow RROW-N !  ratom RATOM-N !
   RSN @ {: at:n :}
   SGIN @ SGOUT @ SGRIN @ SGROUT @ SGHASR @ JEFFECT-TEXT
   at RSN ! ;


: INFERRED-EFFECT ( -- )   \ the body's effect so far: the declared inputs to the current rows
   SGSEEN @ IF SGIN @ ELSE BROW @ THEN
   DCUR @
   SGHASR @ IF SGRIN @ ELSE RBROW @ THEN
   RCUR @
   SGHASR @ JEFFECT-TEXT ;


\ The escape refusal (MD-TAIL-ESCAPE) as checker.f CELL-DIAG! named it: the
\ inferred effect, then the declared callable and the argument compared with it,
\ rendered in DIAG-PRINT's letter order into TCAP before later tokens bind their
\ rows. Naming the callable pair, not the stacks around it, it reads the same
\ whatever the definition holds beneath. The held row names the tail that
\ escaped (checker.f TAIL-SKOLEM) and the row that would have come to equal or
\ hold it (TAIL-ROW), so a join whose two callables read alike still says which
\ side escaped. MDIAG gates every read, so a capture an abandoned trial made is
\ never rendered.
\ TCAP is mapped by the first capture. Its lengths are 0 until a capture, so a
\ reader reads nothing.
4096 constant TCAP-FIRST                  \ bytes of the first mapping
PERSISTED-PTR-VARIABLE TCAP-P   NULL-PTR TCAP-P !
variable TCAP-CAP   0 TCAP-CAP !          \ bytes mapped at TCAP-P, 0 for none
variable TCAP-INF   variable TCAP-EXP   variable TCAP-ACT   variable TCAP-HELD
: TCAP ( -- ptr u8 ) TCAP-P @ ;
: TCAP-INF$ ( -- ptr u8 n ) TCAP TCAP-INF @ ;
: TCAP-EXP$ ( -- ptr u8 n ) TCAP TCAP-INF @ +  TCAP-EXP @ TCAP-INF @ - ;
: TCAP-ACT$ ( -- ptr u8 n ) TCAP TCAP-EXP @ +  TCAP-ACT @ TCAP-EXP @ - ;
: TCAP-HELD$ ( -- ptr u8 n ) TCAP TCAP-ACT @ +  TCAP-HELD @ TCAP-ACT @ -  SIG-RTRIM ;


: TCAP-ROW ( n -- )   \ the escaped-into row: its tail's letter and cells
   {: x:n :}
   x ISVAR IF x REND-TYPE EXIT THEN
   x QTAIL  x DROW ;


\ Each capture rewrites every byte a reader sees, so a grow keeps none.
: TCAP-ROOM ( n -- )
   {: need:n :}
   need TCAP-CAP @ <= IF EXIT THEN
   need TCAP-CAP @ 2 * max TCAP-FIRST max {: nc:n :}
   nc ARENA-ALLOC-BYTES {: next:ptr :}
   TCAP-P @ TCAP-CAP @ ARENA-UNMAP
   next TCAP-P !  nc TCAP-CAP ! ;


: TAIL-CAPTURE ( -- )
   RDST @ {: dst:n :}  RSN @ {: base:n :}  RQM @ {: qm:n :}
   1 RDST !  SEEN-RESET 0 NLET !  0 RROW-N !
   INFERRED-EFFECT  RSN @ base - TCAP-INF !
   -1 RTAILS !
   DF-EXP @ REND-TYPE 32 EMIT1  RSN @ base - TCAP-EXP !
   DF-ACT @ REND-TYPE 32 EMIT1  RSN @ base - TCAP-ACT !
   TAIL-SKOLEM @ ROW-TAIL RROW-LETTER EMIT1  s"  would escape into " DTXT
   TAIL-ROW @ TCAP-ROW  RSN @ base - TCAP-HELD !
   0 RTAILS !
   TCAP-HELD @ TCAP-ROOM
   0 BEGIN dup TCAP-HELD @ < WHILE
      dup base + RSBUF + c@  over TCAP + c!  1 +
   REPEAT drop
   base RSN !  qm RQM !  dst RDST ! ;


: TAIL-CAPTURE-INSTALL ( -- ) [: TAIL-CAPTURE ;] is TAIL-CAPTURE-XT ;
TAIL-CAPTURE-INSTALL

\ Match/construct reasons (docs/forth.md "Structures And Enums"): MDIAG's latched
\ code maps to a JSON code, repair class, suggestion and prose. MD-NONEXH names
\ the missing variants from the latched (family, seen-bitset, count):
\ declaration-order tags index the family's contiguous SUMV rows.
variable MDV-I   variable MDV-F

\ A row pair's repair by depth, counted in the unit its rows were judged in:
\ ROW-VALUES for a mismatch, as ROWS-COVER pairs; ROW-CELLS for a depth reason.
: ROWS-DEPTHS ( [ n -- n ] -- )
   {: measure :}
   AT-PIN
   DEXP @ measure execute DSUGE !
   DACT @ measure execute DSUGA !
   ;AT-PIN ;

: ROWS-CLASS$ ( [ n -- n ] -- ptr u8 n )
   ROWS-DEPTHS
   DSUGA @ DSUGE @ > IF s" remove_producer" EXIT THEN
   DSUGA @ DSUGE @ < IF s" add_producer" EXIT THEN
   s" fix_type" ;

: ROWS-SUGGEST$ ( [ n -- n ] -- ptr u8 n )
   ROWS-DEPTHS
   DSUGA @ DSUGE @ > IF s" Remove an extra producer or drop the surplus value." EXIT THEN
   DSUGA @ DSUGE @ < IF s" Add the missing producer or stop consuming a required value." EXIT THEN
   s" Change the body so produced types match the signature." ;


: MD-CODE$ ( n -- ptr u8 n )
   case
      MD-FAM-UNKNOWN  of s" E-MATCH-UNKNOWN-FAMILY" endof
      MD-FAM-KIND     of s" E-MATCH-FAMILY-KIND" endof
      MD-SCRUT        of s" E-MATCH-SCRUTINEE" endof
      MD-FAM-MISMATCH of s" E-MATCH-FAMILY-MISMATCH" endof
      MD-VAR-UNKNOWN  of s" E-MATCH-UNKNOWN-VARIANT" endof
      MD-VAR-DUP      of s" E-MATCH-DUPLICATE-VARIANT" endof
      MD-MISSING-OF   of s" E-MATCH-MISSING-OF" endof
      MD-NONEXH       of s" E-MATCH-NONEXHAUSTIVE" endof
      MD-STRAY        of s" E-MATCH-STRAY" endof
      MD-TRUNC        of s" E-MATCH-UNTERMINATED" endof
      MD-DEPTH        of s" E-MATCH-DEPTH" endof
      MD-QUOT         of s" E-MATCH-QUOTATION" endof
      MD-OPEN-ARGS    of s" E-MATCH-OPEN-ARGS" endof
      MD-JOIN         of s" E-MATCH-BRANCH-JOIN" endof
      MD-CON-FAM      of s" E-CONSTRUCT-UNKNOWN-FAMILY" endof
      MD-CON-KIND     of s" E-CONSTRUCT-FAMILY-KIND" endof
      MD-CON-VAR      of s" E-CONSTRUCT-UNKNOWN-VARIANT" endof
      MD-CON-TRUNC    of s" E-CONSTRUCT-UNTERMINATED" endof
      MD-DIVBAR       of s" E-DIVERGENT-BARRIER" endof
      MD-EXEC-OPAQUE  of s" E-EXEC-OPAQUE-XT" endof
      MD-CATCH-OPAQUE of s" E-EXEC-OPAQUE-XT" endof
      MD-RAW-PTR      of s" E-RAW-CELL-PTR" endof
      MD-NULL-PTR     of s" E-RAW-CELL-PTR" endof
      MD-DBASE-PTR    of s" E-RAW-CELL-PTR" endof
      MD-RAW-FIELD    of s" E-RAW-CELL-PTR" endof
      MD-RAW-EXEC     of s" E-RAW-CELL-PTR" endof
      MD-STALE-READ   of s" E-STALE-READ" endof
      MD-TOP-UNKNOWN  of s" E-STALE-READ" endof
      MD-TAIL-ESCAPE  of s" E-MISMATCH" endof
      MD-LIT-FIT      of s" E-MISMATCH" endof
      MD-SCOPE-STORE  of s" E-SCOPED-STORAGE" endof
      MD-SCOPE-KIND   of s" E-SCOPE-TYPE" endof
      MD-C2-COPY      of s" E-C2-COPY" endof
      MD-C2-DROP      of s" E-C2-DROP" endof
      MD-C2-ESCAPE    of s" E-C2-SCOPE-ESCAPE" endof
      MD-ARENA-CALL   of s" E-ARENA-CALL" endof
      MD-UNDERFLOW    of s" E-INPUT-UNDERFLOW" endof
      MD-RIGID-REGION of s" E-RIGID-REGION-MISMATCH" endof
      MD-RIGID-EXTENT of s" E-RIGID-EXTENT-MISMATCH" endof
      MD-RIGID-GEN    of s" E-RIGID-STALE-GENERATION" endof
      MD-RIGID-XDOM   of s" E-RIGID-DOMAIN-CONFUSION" endof
      MD-EFFECT-DEPTH of s" E-UNCHECKABLE" endof
      MD-INPUT-WIDTH  of s" E-UNCHECKABLE" endof
      MD-CF-DEPTH     of s" E-UNCHECKABLE" endof
      MD-RECURSE-SIG  of s" E-UNCHECKABLE" endof
      MD-RECURSE-DOES of s" E-UNCHECKABLE" endof
      MD-IS-UNKNOWN   of s" E-UNDEFINED" endof
      MD-IS-NOT-DEFER of s" E-NOT-DEFERRED" endof
      MD-IS-MISSING   of s" E-MISSING-NAME" endof
      MD-TYPE-FORM    of s" E-BAD-STORAGE" endof
      MD-STORED-LINEAR of s" E-CAST-LINEAR" endof
      MD-STORED-CLASS of s" E-CAST-CLASS" endof
      MD-IS-STORED    of s" E-CAST-LINEAR" endof
      MD-DIV-ZERO     of s" E-DIV-ZERO" endof
      MD-THROW-ZERO   of s" E-THROW-ZERO" endof
      MD-LOOP-DEPTH   of s" E-MISMATCH" endof
      MD-IF-DEPTH     of s" E-MISMATCH" endof
      MD-ARMS-DEPTH   of s" E-MISMATCH" endof
      MD-LOCAL-ESCAPE of s" E-LOCAL-ESCAPE" endof
      MD-BINDER-PLACE of s" E-BINDER-PLACE" endof
      s" E-REJECTED" rot
   endcase ;


: MD-CLASS$ ( n -- ptr u8 n )
   case
      MD-NONEXH       of s" add_missing_branches" endof
      MD-VAR-DUP      of s" remove_duplicate_branch" endof
      MD-VAR-UNKNOWN  of s" fix_variant_reference" endof
      MD-CON-VAR      of s" fix_variant_reference" endof
      MD-FAM-UNKNOWN  of s" fix_family_reference" endof
      MD-FAM-KIND     of s" fix_family_reference" endof
      MD-CON-FAM      of s" fix_family_reference" endof
      MD-CON-KIND     of s" fix_family_reference" endof
      MD-SCRUT        of s" fix_match_scrutinee" endof
      MD-FAM-MISMATCH of s" fix_match_scrutinee" endof
      MD-QUOT         of s" fix_match_scrutinee" endof
      MD-OPEN-ARGS    of s" fix_match_scrutinee" endof
      MD-JOIN         of s" fix_branch_outputs" endof
      MD-DEPTH        of s" factor_match_nesting" endof
      MD-DIVBAR       of s" move_collective_to_block_uniform_control" endof
      MD-EXEC-OPAQUE  of s" fix_opaque_execute" endof
      MD-CATCH-OPAQUE of s" fix_opaque_execute" endof
      MD-RAW-PTR      of s" declare_pointer_cell" endof
      MD-NULL-PTR     of s" declare_pointer_cell" endof
      MD-DBASE-PTR    of s" declare_pointer_cell" endof
      MD-RAW-FIELD    of s" declare_pointer_cell" endof
      MD-RAW-EXEC     of s" declare_xt_cell" endof
      MD-STALE-READ   of s" keep_value_before_catch" endof
      MD-TOP-UNKNOWN  of s" drop_unknown_cell" endof
      MD-TAIL-ESCAPE  of s" fix_type" endof
      MD-LIT-FIT      of s" fix_type" endof
      MD-SCOPE-STORE  of s" keep_scoped_value_on_stack" endof
      MD-SCOPE-KIND   of s" fix_scope_parameter" endof
      MD-C2-COPY      of s" move_exclusive_value" endof
      MD-C2-DROP      of s" consume_exclusive_value" endof
      MD-C2-ESCAPE    of s" keep_borrow_within_scope" endof
      MD-ARENA-CALL   of s" use_arena_words" endof
      MD-UNDERFLOW    of s" supply_missing_input" endof
      MD-RIGID-REGION of s" fix_host_region" endof
      MD-RIGID-EXTENT of s" fix_host_extent" endof
      MD-RIGID-GEN    of s" fix_stale_generation" endof
      MD-RIGID-XDOM   of s" fix_rigid_domain" endof
      MD-EFFECT-DEPTH of s" rewrite_uncheckable" endof
      MD-INPUT-WIDTH  of s" rewrite_uncheckable" endof
      MD-CF-DEPTH     of s" rewrite_uncheckable" endof
      MD-RECURSE-SIG  of s" rewrite_uncheckable" endof
      MD-RECURSE-DOES of s" rewrite_uncheckable" endof
      MD-IS-UNKNOWN   of s" fix_is_target" endof
      MD-IS-NOT-DEFER of s" fix_is_target" endof
      MD-IS-MISSING   of s" fix_missing_name" endof
      MD-TYPE-FORM    of s" fix_type_form" endof
      MD-STORED-LINEAR of s" fix_type_form" endof
      MD-STORED-CLASS of s" fix_type_form" endof
      MD-IS-STORED    of s" fix_type_form" endof
      MD-DIV-ZERO     of s" fix_divisor" endof
      MD-THROW-ZERO   of s" fix_throw_code" endof
      MD-LOOP-DEPTH   of ['] ROW-CELLS ROWS-CLASS$ endof
      MD-IF-DEPTH     of ['] ROW-CELLS ROWS-CLASS$ endof
      MD-ARMS-DEPTH   of ['] ROW-CELLS ROWS-CLASS$ endof
      MD-LOCAL-ESCAPE of s" run_quotation_while_locals_live" endof
      MD-BINDER-PLACE of s" call_scope_operation_directly" endof
      MD-DEPTH-WIDE   of s" count_values" endof
      MD-LOOP-RDEPTH  of s" fix_return_stack" endof
      MD-LOOP-CARRIED of s" fix_control_flow" endof
      MD-NO-LOOP      of s" fix_control_flow" endof
      MD-NO-OUTER-LOOP of s" fix_control_flow" endof
      MD-CLOSES-NOTHING of s" fix_control_flow" endof
      MD-NO-CASE      of s" fix_control_flow" endof
      MD-ELSE-TWICE   of s" fix_control_flow" endof
      MD-EXIT-LOOP    of s" fix_control_flow" endof
      MD-UNLOOP-JOIN  of s" fix_control_flow" endof
      MD-CF-OPEN      of s" fix_control_flow" endof
      s" fix_match_syntax" rot
   endcase ;


\ Shared by a definition's refusal and a stored signature's (BADSIG-REASON).
: DEPTH-REASON$ ( -- ptr u8 n )
   s" effect too deep to record" ;

\ Shared by a definition's depth refusal (MD-SUGGEST$) and a signature's
\ (SGBAD-SUGGEST$): a row is deep by its cells or by the types it nests.
: DEPTH-SUGGEST$ ( -- ptr u8 n )
   s" Keep bulk values in a buffer, not on the stack, and name an inner callback's effect in a word of its own." ;

: WIDTH-REASON$ ( -- ptr u8 n )
   s" input row too wide to record" ;

\ More than BINDER-DEPTH-MAX nested binders: a signature's (checker.f
\ SGBAD-BINDERS-KIND) or an inferred effect's (RBIND-FULL).
: BINDERS-REASON ( -- )
   s" more than " DTXT  BINDER-DEPTH-MAX JNUM  s"  nested binders" DTXT ;


\ Shared by a token of unknown effect (SUGGEST-TEXT) and a deferral whose cause
\ no site recorded (DEFER-SUGGEST$).
: UNCK-SUGGEST$ ( -- ptr u8 n )
   s" Rewrite with modeled words or isolate an audited primitive." ;

\ What lifts the deferral whose cause DEFER-WHY holds (checker.f DW-PARSER ..).
: DEFER-SUGGEST$ ( -- ptr u8 n )
   DEFER-WHY @ case
      DW-PARSER   of s" State what this word reads after its definition: `parses: W n` or `parses-through: W n ( END )`; call a primitive through a word that has such a row." endof
      DW-DEFER    of s" Call this deferred word from a definition: a top-level `defer` may read any of the source after it, and no row bounds it." endof
      DW-PARSES   of s" Nothing to write: the check went on after the operands its row states; the run decides what the word reads." endof
      DW-RENDERER of s" If this renderer reads the name of the word it makes (`D NAME`), declare that word's effect with `generates: D ( effect )` after its definition; a foreign function is declared by a `FUNCTION:` group; other rendered text is the run's to check." endof
      DW-RENDERED of s" Make this name with a definer called as `D NAME` and declare it with `generates: D ( effect )` after its definition or with a `does>` clause; otherwise the run defines it or refuses a misspelling." endof
      DW-TYPE     of s" Declare this type in the source before its use (`NEWTYPE`, `STRUCTURE`, `SUMTYPE`); a type only rendered text makes is the run's to check." endof
      DW-TICK     of s" Tick this trusted-only word inside a `TRUSTED:` body; checked code may tick it only if the run's checker allows it." endof
      UNCK-SUGGEST$ rot
   endcase ;

: TAIL-SUGGEST$ ( -- ptr u8 n )
   s" The callee declares this callable with an implicit input row, [ -- S ], so it may run the callable on any stack and expects S back; a callable that keeps or reads the stack it runs on cannot promise that. Declare the callable [ R -- S ] from the word's input row, as in ( R [ R -- S ] -- S ), or pass a callable whose output does not depend on the stack beneath it." ;


: MD-SUGGEST$ ( n -- ptr u8 n )
   case
      MD-NONEXH       of s" Add an OF branch for every listed variant; v1 has no default branch." endof
      MD-VAR-DUP      of s" Remove the repeated variant branch; each variant may appear once." endof
      MD-VAR-UNKNOWN  of s" Use a variant declared by this family, lowercase as declared." endof
      MD-CON-VAR      of s" Use a variant declared by this family, lowercase as declared." endof
      MD-FAM-UNKNOWN  of s" Name a visible sum family: own package, a global, a `using` import, or PKG:tail." endof
      MD-CON-FAM      of s" construct resolves only families declared in the active package." endof
      MD-FAM-KIND     of s" MATCH eliminates sum or enum families only." endof
      MD-CON-KIND     of s" construct builds sum or enum families only." endof
      MD-SCRUT        of s" Put the family value on top of the stack before MATCH." endof
      MD-FAM-MISMATCH of s" The value on top belongs to a different family; match that family." endof
      MD-QUOT         of s" Move the match out of the quotation; quotation rows cannot carry a scrutinee." endof
      MD-OPEN-ARGS    of s" Instantiate the family arguments to concrete types before matching." endof
      MD-JOIN         of s" Make every branch leave the same stack shape; ;MATCH has one continuation." endof
      MD-DEPTH        of s" Factor the inner match into a named word; control frames are capped." endof
      MD-MISSING-OF   of s" Write `variant OF ... ENDOF` for each branch." endof
      MD-DIVBAR       of s" Call the block collective on the straight-line (block-uniform) path; do not place it inside if/loop/case or a quotation." endof
      MD-EXEC-OPAQUE  of s" Execute an xt whose effect is statically known: a quotation parameter, a defer bound with is, or a typed xt cell. Do not execute an xt fetched from untyped memory or ticked from a primitive or a word with no stack effect." endof
      MD-CATCH-OPAQUE of s" Catch an xt whose effect is statically known: a quotation parameter, a defer bound with is, or a typed xt cell. Do not catch an xt fetched from untyped memory or ticked from a primitive or a word with no stack effect." endof
      MD-RAW-PTR      of s" Declare the cell that holds an address: PTR-VARIABLE, PERSISTED-PTR-VARIABLE, TYPED-VARIABLE NAME ptr t, or TYPED-BUFFER. A plain variable, create or constant cell holds scalars, roles and atoms only." endof
      MD-NULL-PTR     of s" NULL-PTR is the address of nothing: it compares, subtracts, tests and stores like any pointer, but no value is ever read through it. Take the value from the cell that really holds it." endof
      MD-DBASE-PTR    of s" Reach the cell through a declared accessor instead: give the word that adds the offset a concrete pointee (ptr n, ptr u8), or take the field of a declared pointer cell with ptr-field. data-base addresses no declared element, so a cell of the DATA region holds a plain value at every depth -- it is not the address of an address either." endof
      MD-RAW-EXEC     of s" Declare the cell that holds the execution token: TYPED-VARIABLE NAME [ in -- out ], a TYPED-BUFFER or DYNAMIC-BUFFER of [ in -- out ], or bind a defer with is. An undeclared variable, create, constant or data-base cell holds scalars, roles and atoms only, so what comes back from it is an integer, not code." endof
      MD-RAW-FIELD    of s" Take the pointer field of a declared cell: PTR-VARIABLE, PERSISTED-PTR-VARIABLE, TYPED-VARIABLE NAME ptr t, or TYPED-BUFFER. 0 ptr-field on a plain variable or create cell laundered a raw cell into a typed pointer." endof
      MD-RIGID-REGION of s" These are different host allocations; a value carrying one region's identity cannot stand in for another. Thread the same allocation through, or re-borrow from the target." endof
      MD-RIGID-EXTENT of s" These host allocations have different extents; a bound proved for one does not carry to another. Use the value whose extent identity the position requires." endof
      MD-RIGID-GEN    of s" This index or borrow is from an earlier mutation generation; the container was mutated since. Re-derive the index after the mutation." endof
      MD-RIGID-XDOM   of s" A host region, an extent, and a mutation generation are distinct identities that never interchange. Supply the identity the position requires." endof
      MD-UNDERFLOW    of s" Push the missing inputs before the call, or declare them in the signature; a definition may not consume below its declared inputs." endof
      MD-STALE-READ   of s" `catch` puts the stacks back to the DEPTH it was entered at and never to their contents, so a cell the caught body may have written on a throw path holds an unknown machine word afterwards. Bind the value to a local BEFORE the catch and use the local, or drop the cell. A stale cell may still be moved, dropped, or bound to an untyped local." endof
      MD-TOP-UNKNOWN  of s" The interpret row knows only the cells pushed while it checks; a cell beneath them was left before checking began or by a `0 set-check` window, so its type is unknown. Push the value with checking on, or drop the cell. An unknown cell may still be moved, dropped, or bound to an untyped local." endof
      MD-TAIL-ESCAPE  of TAIL-SUGGEST$ endof
      MD-LIT-FIT      of s" Write a value in range, or widen the declared type." endof
      MD-SCOPE-STORE  of s" Keep the scoped value on the stack or in a local; ordinary memory has no scope dependency declaration." endof
      MD-SCOPE-KIND   of s" Use a scope parameter only in a declared scope slot; it is neither a value type nor an allocation identity." endof
      MD-C2-COPY      of s" Move the exclusive value once instead of copying it." endof
      MD-C2-DROP      of s" Return or consume the exclusive value instead of discarding it." endof
      MD-C2-ESCAPE    of s" Return only values independent of the owner or loan opened by this operation." endof
      MD-ARENA-CALL   of s" Declare the storage with HEAP NAME TYPE and call its generated NAME-NEW and NAME-RELEASE: only those words call RECORD-NEW and RECORD-RELEASE, because only they tie the record size to the declared type." endof
      MD-EFFECT-DEPTH of DEPTH-SUGGEST$ endof
      MD-INPUT-WIDTH  of s" Take fewer cells: pass bulk values in a buffer, not on the stack." endof
      MD-CF-DEPTH     of s" Move the inner control structures into a named word and call it there; each definition counts its own control frames." endof
      MD-RECURSE-SIG  of s" Declare the definition's stack effect after its name." endof
      MD-RECURSE-DOES of s" Move the recursion into a named word with a declared effect and call it from the clause." endof
      MD-IS-UNKNOWN   of s" Name a word declared with defer; qualify it as PKG:NAME or import its package with using." endof
      MD-IS-NOT-DEFER of s" Name a word declared with defer; `is` rebinds deferred words only." endof
      MD-IS-MISSING   of s" Write the name of a word declared with defer after is." endof
      MD-TYPE-FORM    of s" Name a closed type ARENA stores: one cell or more, or u8, with no type variable, quotation, linear value, atom, xt or width<t> in it, and no cell family with arguments as the type or a pointer's pointee." endof
      MD-STORED-LINEAR of s" Pass a type with no linear value in it: a cast's stored type is copied as plain cells." endof
      MD-STORED-CLASS of s" Pass a type a cast may name as a pointee: no atom and no width<t> in it." endof
      MD-IS-STORED    of s" Declare the cell over the concrete type the callable stores, or install a callable whose type variable takes any type: a cast's stored type holds no linear value, atom or width." endof
      MD-DIV-ZERO     of s" Divide by a value that cannot be zero; a zero divisor's answer is undefined." endof
      MD-THROW-ZERO   of s" Throw a nonzero code: 0 means no error, and a catch that answers 0 cannot tell the unwind from a normal return." endof
      MD-LOOP-DEPTH   of ['] ROW-CELLS ROWS-SUGGEST$ endof
      MD-IF-DEPTH     of ['] ROW-CELLS ROWS-SUGGEST$ endof
      MD-ARMS-DEPTH   of ['] ROW-CELLS ROWS-SUGGEST$ endof
      MD-LOCAL-ESCAPE of s" Run the quotation inside the block of the locals it reads; a quotation that is stored, kept or returned takes its values from the stack." endof
      MD-BINDER-PLACE of s" Call the scope operation directly in the word that supplies its callback; only a scope operation's callback input may hold a callback binder." endof
      MD-DEPTH-WIDE   of s" Count the values another way, or take the wide value apart first: MATCH a sum or an enum whose arguments are concrete, UNMAKE a product; a stale value can only be moved or dropped. `depth` and `.s` count raw cells, and a wide value is one value held in several." endof
      MD-LOOP-RDEPTH  of s" Take back with r> in the loop body every cell it pushes with >r, so each pass leaves the return stack as it found it." endof
      MD-LOOP-CARRIED of s" Make each pass leave the cells it carries as it found them, of the same types and with the same facts known of them; keep a value that changes per pass in a variable." endof
      MD-NO-LOOP      of s" Use i, leave, unloop, loop and +loop only inside a do or ?do this definition opened and has not closed." endof
      MD-NO-OUTER-LOOP of s" Use j only inside a do loop nested in another; the index of a single loop is i." endof
      MD-CLOSES-NOTHING of s" Close each structure with its own word, innermost first: if ... else ... then, begin ... until, begin ... while ... repeat, case ... of ... endof ... endcase, [: ... ;]." endof
      MD-NO-CASE      of s" Write of only inside case ... endcase, after the previous branch's endof." endof
      MD-ELSE-TWICE   of s" Give an if one else at most; choose among more paths with an if ... else ... then nested in the else branch." endof
      MD-EXIT-LOOP    of s" Write unloop before exit once for each loop the exit leaves, or leave the loop and return after it." endof
      MD-UNLOOP-JOIN  of s" Unloop the same loops on every path that meets here, or exit from each path right after its unloop." endof
      MD-CF-OPEN      of s" Close every if, do, begin, case and [: before the definition ends." endof
      s" Complete the form: MATCH family, variant OF ... ENDOF per variant, ;MATCH." rot
   endcase ;


: MD-REASON$ ( n -- ptr u8 n )
   case
      MD-FAM-UNKNOWN  of s" bad match: unknown type family" endof
      MD-FAM-KIND     of s" bad match: family is not a sum or enum" endof
      MD-SCRUT        of s" bad match: expected sum or enum value on stack" endof
      MD-FAM-MISMATCH of s" bad match: family mismatch" endof
      MD-VAR-UNKNOWN  of s" bad match: unknown variant" endof
      MD-VAR-DUP      of s" bad match: duplicate variant" endof
      MD-MISSING-OF   of s" bad match: variant token must be followed by OF" endof
      MD-NONEXH       of s" bad match: missing variants:" endof
      MD-STRAY        of s" bad match: misplaced match token" endof
      MD-TRUNC        of s" bad match: unterminated match" endof
      MD-DEPTH        of s" bad match: nesting exceeds control-frame capacity" endof
      MD-QUOT         of s" bad match: quotation rows cannot carry a scrutinee" endof
      MD-OPEN-ARGS    of s" bad match: scrutinee type arguments are unresolved" endof
      MD-JOIN         of s" bad match: branch output mismatch" endof
      MD-CON-FAM      of s" bad construct: family not declared in the active package" endof
      MD-CON-KIND     of s" bad construct: family is not a sum or enum" endof
      MD-CON-VAR      of s" bad construct: unknown variant" endof
      MD-CON-TRUNC    of s" bad construct: missing family or variant token" endof
      MD-DIVBAR       of s" divergent barrier: block collective requires block-uniform control" endof
      MD-EXEC-OPAQUE  of s" execute: opaque xt of unknown provenance (fetched from untyped memory, or ticked from a primitive or a word with no stack effect)" endof
      MD-CATCH-OPAQUE of s" catch: opaque xt of unknown provenance (fetched from untyped memory, or ticked from a primitive or a word with no stack effect)" endof
      MD-RAW-PTR      of s" raw storage cell: a pointer cannot be stored in or fetched from an undeclared cell" endof
      MD-NULL-PTR     of s" null address: nothing is read through NULL-PTR, so it is never a nominal type and never the address of one" endof
      MD-DBASE-PTR    of s" base address: a cell reached from data-base, or from a pointer computed off NULL-PTR, holds a plain value at every pointee depth, never a nominal type or a pointer" endof
      MD-RAW-FIELD    of s" ptr-field: base is an undeclared raw storage cell, not a declared pointer cell" endof
      MD-RAW-EXEC     of s" raw storage cell: an undeclared cell cannot hold an execution token / a quotation" endof
      MD-STALE-READ   of s" stale cell: this reads a cell `catch` left stale (a throw path of the caught body may have overwritten it)" endof
      MD-TOP-UNKNOWN  of s" unknown cell: this reads a cell beneath the cells the interpret row knows (left before checking began or by a `0 set-check` window)" endof
      MD-TAIL-ESCAPE  of s" callable row escape: a callable declared with an implicit input row cannot keep or name the stack it runs on" endof
      MD-LIT-FIT      of s" literal" endof
      MD-SCOPE-STORE  of s" scoped storage: ordinary memory cannot retain a scope dependency" endof
      MD-SCOPE-KIND   of s" scope parameter: a scope identity cannot stand in for a value type" endof
      MD-C2-COPY      of s" exclusive value copied" endof
      MD-C2-DROP      of s" exclusive value discarded" endof
      MD-C2-ESCAPE    of s" scoped value escapes its owner or loan" endof
      MD-ARENA-CALL   of s" arena call: only HEAP's generated NAME-NEW and NAME-RELEASE call the storage runtime" endof
      MD-RIGID-REGION of s" rigid host: region mismatch (different allocation)" endof
      MD-RIGID-EXTENT of s" rigid host: extent mismatch (different bounds identity)" endof
      MD-RIGID-GEN    of s" rigid host: stale mutation generation" endof
      MD-RIGID-XDOM   of s" rigid host: identity domain confusion" endof
      MD-UNDERFLOW    of s" input underflow: the call takes more cells than the definition's declared inputs leave" endof
      MD-EFFECT-DEPTH of DEPTH-REASON$ endof
      MD-INPUT-WIDTH  of WIDTH-REASON$ endof
      MD-CF-DEPTH     of s" control flow nested too deep to check" endof
      MD-RECURSE-SIG  of s" recursion needs a declared stack effect" endof
      MD-RECURSE-DOES of s" recurse in a does> clause would call the defining word" endof
      MD-IS-UNKNOWN   of s" names no word" endof
      MD-IS-NOT-DEFER of s" is not a deferred word" endof
      MD-IS-MISSING   of s" missing target word after is" endof
      MD-TYPE-FORM    of s" [type] names no type storage holds" endof
      MD-STORED-LINEAR of s" a cast's stored type would hold a linear value" endof
      MD-STORED-CLASS of s" a cast's stored type would hand out an atom or a width" endof
      MD-IS-STORED    of s" the callable takes only a cast's stored types where the cell takes any type" endof
      MD-DIV-ZERO     of s" zero divisor: a literal 0 divisor has no quotient" endof
      MD-THROW-ZERO   of s" zero throw code: `0 throw` unwinds with the code that means no error" endof
      MD-LOOP-DEPTH   of s" the loop body changes the stack's depth" endof
      MD-IF-DEPTH     of s" the if arm changes the stack's depth" endof
      MD-ARMS-DEPTH   of s" the if and else arms leave different stack depths" endof
      MD-LOCAL-ESCAPE of s" a quotation that reads a local would outlive it: stored, kept, returned or held past the local's block" endof
      MD-BINDER-PLACE of s" a callback binder would sit outside a scope operation's callback input in the word's effect" endof
      MD-DEPTH-WIDE   of s" cannot count" endof
      MD-LOOP-RDEPTH  of s" the loop body changes the return stack's depth" endof
      MD-LOOP-CARRIED of s" the loop body leaves a carried cell different from the one it entered with" endof
      MD-NO-LOOP      of s" no loop is open here" endof
      MD-NO-OUTER-LOOP of s" no outer loop is open here" endof
      MD-CLOSES-NOTHING of s" this word closes nothing open here" endof
      MD-NO-CASE      of s" no case is open here" endof
      MD-ELSE-TWICE   of s" this if already has an else" endof
      MD-EXIT-LOOP    of s" exit inside a loop needs unloop first" endof
      MD-UNLOOP-JOIN  of s" the paths that meet here have unlooped different loops" endof
      MD-CF-OPEN      of s" a control structure is still open at the end of the definition" endof
      s" bad match: rejected" rot
   endcase ;


: MDIAG-MISSING-WALK ( bool -- ) {: json:bool :}   \ unseen variant tails, space-led
   MDIAG-FAM @ TFAM-VAR-START@ {: vstart:n :}
   0 MDV-I !  0 MDV-F !
   BEGIN MDV-I @ MDIAG-VCNT @ < WHILE
      MDIAG-SEEN @ MDV-I @ MSEEN-GET 0= IF
         json MDV-F @ 0= and 0= IF s"  " DTXT THEN
         vstart MDV-I @ + SUMV-NAME$ DTXT
         -1 MDV-F !
      THEN
      MDV-I @ 1 + MDV-I !
   REPEAT ;

: MDIAG-MISSING-JSTR ( -- )
   JOPEN  0 0= MDIAG-MISSING-WALK  JCLOSE ;

: MDIAG-MISSING-PROSE ( -- )
   0 0= 0= MDIAG-MISSING-WALK ;

: MDIAG-UF-COUNTS ( -- )
   s"  (needs " DTXT  MDIAG-NEED @ JNUM
   s" , has " DTXT  MDIAG-HAVE @ JNUM  s" )" DTXT ;

: CAP-DEPTH-COUNTS ( n n -- )
   {: depth:n cap:n :}
   s"  (depth " DTXT  depth JNUM
   s" , at most " DTXT  cap JNUM  s" )" DTXT ;

: DEPTH-COUNTS ( n -- )
   EFFECT-DEPTH-MAX CAP-DEPTH-COUNTS ;

: WIDTH-COUNTS ( n -- )
   {: width:n :}
   s"  (" DTXT  width JNUM
   s"  cells, at most " DTXT  EFFECT-MIN-IN-MAX JNUM  s" )" DTXT ;

\ The literal that does not fit and its type's range, after the reason's
\ `literal`: `300` does not fit `u8` (`0`..`255`).
: LIT-FIT-COUNTS ( -- )
   s"  `" DTXT  MDIAG-LITV @ SNUM
   s" ` does not fit `" DTXT  MDIAG-LITT @ CT-NAME$ DTXT
   s" ` (`0`..`" DTXT  MDIAG-LITT @ LIT-HI JNUM  s" `)" DTXT ;

\ The wide value `depth` or `.s` met and its cells, or that they are not known
\ where its width is open, after the reason's `cannot count`.
: DEPTH-WIDE-COUNTS ( -- )
   AT-PIN
   s"  `" DTXT  MDIAG-WIDE @ REND-TYPE
   MDIAG-OPEN @ IF
      s" `: it is one value, and how many cells it holds is not known here" DTXT
   ELSE
      s" `: it is one value held in " DTXT  MDIAG-WIDE @ T-WIDTH JNUM  s"  cells" DTXT
   THEN
   ;AT-PIN ;


\ What a reason carries after its prose, in text and JSON alike.
: MDIAG-COUNTS ( -- )
   MDIAG @ MD-TAIL-ESCAPE = IF s" ; held row " DTXT  TCAP-HELD$ DTXT EXIT THEN
   MDIAG @ MD-LIT-FIT = IF LIT-FIT-COUNTS EXIT THEN
   MDIAG @ MD-DEPTH-WIDE = IF DEPTH-WIDE-COUNTS EXIT THEN
   MDIAG @ MD-UNDERFLOW = IF MDIAG-UF-COUNTS EXIT THEN
   MDIAG @ MD-EFFECT-DEPTH = IF MDIAG-DEPTH @ DEPTH-COUNTS EXIT THEN
   MDIAG @ MD-CF-DEPTH = IF CF-FRAMES-MAX 1 + CF-FRAMES-MAX CAP-DEPTH-COUNTS EXIT THEN
   MDIAG @ MD-INPUT-WIDTH = IF MDIAG-WIDTH @ WIDTH-COUNTS THEN ;


: IMM-CODE$ ( -- ptr u8 n )
   s" E-UNMODELED-IMMEDIATE" ;

: IMM-CLASS$ ( -- ptr u8 n )
   s" model_compile_immediate" ;

: IMM-SUGGEST$ ( -- ptr u8 n )
   s" Declare a stack-neutral parsing immediate with parse-imm, or remove it from the compiled body." ;

\ The LOCALBAD kinds come from checker.f LOC-REJECT and LOC-GROUP-REJECT.
: LOCALBAD-CODE$ ( -- ptr u8 n )
   LOCALBAD-KIND @ 2 = IF s" E-LOCAL-NAME-TOO-LONG" EXIT THEN
   LOCALBAD-KIND @ 1 = IF s" E-TOO-MANY-LOCALS" EXIT THEN
   s" E-BAD-LOCAL-SHAPE" ;

: LOCALBAD-CLASS$ ( -- ptr u8 n )
   LOCALBAD-KIND @ 2 = IF s" shorten_local_name" EXIT THEN
   LOCALBAD-KIND @ 1 = IF s" reduce_local_count" EXIT THEN
   s" factor_local_shape" ;

: LOCALBAD-SUGGEST$ ( -- ptr u8 n )
   LOCALBAD-KIND @ 2 = IF s" Shorten the local name to at most 16 bytes." EXIT THEN
   LOCALBAD-KIND @ 1 = IF s" Bind at most 64 locals in one definition, or factor a helper." EXIT THEN
   LOCALBAD-KIND @ 3 >= IF s" Remove the token: a locals group lists only the names to bind." EXIT THEN
   s" Move locals to a live top-level path or factor a helper." ;

\ Kinds 3 and 4, a locals group token that names no local, state why; the
\ prose prints it after the token and the packet as its reason.
: LOCALBAD-GROUP? ( -- bool )
   LOCALBAD-KIND @ 3 >= ;

: LOCALBAD-REASON$ ( -- ptr u8 n )
   LOCALBAD-KIND @ 3 = IF s" a locals group names only locals; the signature declares the outputs" EXIT THEN
   s" a locals group names only locals; Habu has no uninitialised locals" ;

\ FAILTK is the declaration token pinned by the first failure, so a later body
\ reference to the rejected name cannot relabel the reject as an undefined word.
: LOCALBAD-PROSE ( -- )
   LOCALBAD-CODE$ DTXT  s"  " DTXT  DSITE
   LOCALBAD-GROUP? IF
     s" at '" DTXT  FAILTK FAILTU @ DTXT  s" ': " DTXT  LOCALBAD-REASON$ DTXT EXIT
   THEN
   LOCALBAD-KIND @ 2 = IF
     s" local '" DTXT  FAILTK FAILTU @ DTXT  s" ' has a " DTXT  LOCALBAD-LEN @ JNUM
     s" -byte name; the limit is " DTXT  LOC-NAME-CAP JNUM  s"  bytes" DTXT EXIT
   THEN
   LOCALBAD-KIND @ 1 = IF
     s" local '" DTXT  FAILTK FAILTU @ DTXT  s" ' is one over the " DTXT  LOC-RECS JNUM
     s"  locals a definition may bind" DTXT EXIT
   THEN
   s" at '" DTXT  FAILTK FAILTU @ DTXT
   s" ': a local cannot be bound inside a quotation, or bound on a dead path" DTXT ;

\ A definition's signature, a local's type or a stored signature that does not
\ parse; SGBAD's state from the failed parse names the kind.
: SGBAD-CODE$ ( -- ptr u8 n )
   SGBAD-UNKNOWN? IF s" E-UNKNOWN-SIGNATURE-TYPE" EXIT THEN
   SGBAD-BAREPTR? IF s" E-BARE-PTR-SIGNATURE" EXIT THEN
   SGBAD-ARITY? IF s" E-WRONG-ARITY" EXIT THEN
   s" E-BAD-SIGNATURE" ;
: SGBAD-CLASS$ ( -- ptr u8 n )
   SGBAD-RETURN? IF s" fix_return_stack" EXIT THEN
   SGBAD-UNKNOWN? IF s" fix_signature_type" EXIT THEN
   SGBAD-BAREPTR? IF s" fix_bare_ptr_element" EXIT THEN
   SGBAD-ARITY? IF s" fix_signature_arity" EXIT THEN
   SGBAD-SIZE? IF s" fix_signature_size" EXIT THEN
   s" fix_signature_syntax" ;
: RCLAUSE-FIX$ ( -- ptr u8 n )
   s" after '|' write one row variable, the same on both sides, or no '|'" ;
\ A syntax fault has no line of its own: it reads as a refusal at its token.
: SGBAD-PROSE ( -- )
   DSITE
   SGBAD-UNKNOWN? IF
     s" unknown type '" DTXT  FAILTK FAILTU @ DTXT  s" ' in signature" DTXT EXIT
   THEN
   SGBAD-BAREPTR? IF
     s" 'ptr' needs an element type" DTXT EXIT
   THEN
   SGBAD-ARITY? IF
     s" wrong arity for type family '" DTXT  FAILTK FAILTU @ DTXT  s" '" DTXT EXIT
   THEN
   SGBAD-RETURN? IF
     s" return-stack effect in signature '" DTXT  FAILTK FAILTU @ DTXT
     s" '" DTXT EXIT
   THEN
   SGBAD-BINDERS? IF BINDERS-REASON s"  at '" DTXT  FAILTK FAILTU @ DTXT  s" '" DTXT EXIT THEN
   SGBAD-DEPTH? IF DEPTH-REASON$ DTXT  SGBAD-SIZE @ DEPTH-COUNTS  s"  at '" DTXT  FAILTK FAILTU @ DTXT  s" '" DTXT EXIT THEN
   s" at '" DTXT  FAILTK FAILTU @ DTXT  s" '" DTXT ;

\ The code, repair class, suggestion, prose and per-code JSON fields all name
\ the one refusal checker.f REFUSAL@ chose.
: DCODE ( -- ptr u8 n )
   REFUSAL@ {: k:n :}
   k RF-IMM = IF IMM-CODE$ EXIT THEN
   k RF-UNDEF = IF s" E-UNDEFINED" EXIT THEN
   k RF-QUAL = IF s" E-BAD-QUALIFIED" EXIT THEN
   k RF-LOCAL = IF LOCALBAD-CODE$ EXIT THEN
   k RF-SIG = IF SGBAD-CODE$ EXIT THEN
   k RF-DEAD = IF s" E-DEAD-CODE" EXIT THEN
   k RF-NP = IF s" E-NONPARAMETRIC-EFFECT" EXIT THEN
   k RF-CAP = IF s" E-CAP-TRUSTED" EXIT THEN
   k RF-UNSAFE = IF s" E-UNSAFE" EXIT THEN
   k RF-LINLOCAL = IF s" E-LINEAR-LOCAL" EXIT THEN
   k RF-REASON = IF MDIAG @ MD-CODE$ EXIT THEN
   k RF-UNCHECKABLE = IF s" E-UNCHECKABLE" EXIT THEN
   k RF-MISMATCH = IF s" E-MISMATCH" EXIT THEN
   s" E-REJECTED" ;
\ The verdict the load gave the definition or step,
\ which each DIAG-PRINT caller stores in DVERD before it calls it.
: DVERDICT ( -- ptr u8 n )
   DVERD @ VERDICT-UNCHECKABLE = IF s" uncheckable" EXIT THEN s" rejected" ;
\ repair_class is the stable machine name a consumer keys its repair on. This
\ word picks it for checker refusals; tools/check-verify-core.f and tools/diag-code.f
\ pick it for the refusals the check tool finds itself. A class means what its
\ suggestion says (SUGGEST-TEXT, the *-SUGGEST$ words): short prose for choosing
\ the action; the raw evidence stays in the expected, actual and return_stack
\ fields.
: REPAIR-CLASS ( -- ptr u8 n )
   REFUSAL@ {: k:n :}
   k RF-IMM = IF IMM-CLASS$ EXIT THEN
   k RF-UNDEF = IF s" unknown_rejection" EXIT THEN
   k RF-QUAL = IF s" fix_qualified_name" EXIT THEN
   k RF-LOCAL = IF LOCALBAD-CLASS$ EXIT THEN
   k RF-SIG = IF SGBAD-CLASS$ EXIT THEN
   k RF-DEAD = IF s" remove_dead_code" EXIT THEN
   k RF-NP = IF s" fix_parametric_effect" EXIT THEN
   k RF-CAP = IF s" trusted_boundary_required" EXIT THEN
   k RF-UNSAFE = IF s" remove_unsafe_word" EXIT THEN
   k RF-LINLOCAL = IF s" factor_linear_local" EXIT THEN
   k RF-REASON = IF MDIAG @ MD-CLASS$ EXIT THEN
   k RF-UNCHECKABLE = IF s" rewrite_uncheckable" EXIT THEN
   k RF-RETURN = IF s" fix_return_stack" EXIT THEN
   k RF-MISMATCH <> IF s" unknown_rejection" EXIT THEN
   ['] ROW-VALUES ROWS-CLASS$ ;
\ Shared by SUGGEST-TEXT, a bad stored signature's diagnostic and a refused
\ `generates:` row's.
: SGBAD-SUGGEST$ ( -- ptr u8 n )
   SGBAD-RETURN? IF s" Declare no return-stack effect: after '|' write one row variable, the same on both sides, or no '|'; park a value with >r and take it back with r> in one definition." EXIT THEN
   SGBAD-UNKNOWN? IF s" Use a known stack-signature type or a single-letter type variable." EXIT THEN
   SGBAD-BAREPTR? IF s" Give 'ptr' an element type, e.g. 'ptr u8' or 'ptr a'." EXIT THEN
   SGBAD-ARITY? IF s" Give the type family its exact declared number of arguments." EXIT THEN
   SGBAD-BINDERS? IF s" Nest fewer binders: name an inner callback's binders in a word of its own." EXIT THEN
   SGBAD-DEPTH? IF DEPTH-SUGGEST$ EXIT THEN
   SGBAD-SIZE? IF s" Declare fewer cells: keep bulk values in a buffer, not on the stack." EXIT THEN
   s" Repair the stack-effect comment syntax, including --." ;
: NP-SUGGEST$ ( -- ptr u8 n )
   NPBAD-KIND @ 4 = IF
      s" Declare the pointee this base address really reaches (ptr n, ptr u8, ...); a pointer derived from data-base cannot be published under a type variable, nor handed to one a caller instantiates." EXIT
   THEN
   NPBAD-KIND @ 5 = IF
      s" Declare the pointee this null stands in for (ptr n, ptr u8, ...); NULL-PTR may be stored through a declared parameter, but not published under a type variable." EXIT
   THEN
   NPBAD-KIND @ 3 = IF
      s" Declare the concrete storage type, or keep the body polymorphic over the type variable." EXIT
   THEN
   NPBAD-KIND @ 0= IF
      s" Declare the concrete family in the signature, or keep the body polymorphic over the type variable." EXIT
   THEN
   NPBAD-KIND @ 1 = IF
      s" Keep each declared type variable distinct; do not unify two quantifiers in the body." EXIT
   THEN
   NPBAD-KIND @ 6 = IF
      s" Keep each declared row distinct: declare the row the body really leaves, or reach the declared output row on every path, as by running the quotation that yields it." EXIT
   THEN
   NPBAD-KIND @ 7 = IF
      s" Declare the cells the body leaves on the row, or keep the body polymorphic over the row variable." EXIT
   THEN
   s" Bind the minted phantom's type arguments to the inputs, or mint it behind an audited TRUSTED: boundary." ;
: SUGGEST-TEXT ( -- ptr u8 n )
   REFUSAL@ {: k:n :}
   k RF-IMM = IF IMM-SUGGEST$ EXIT THEN
   k RF-UNDEF = IF s" Inspect the token, signature, and raw stack evidence." EXIT THEN
   k RF-QUAL = IF s" Use one ':' qualifier, e.g. PKG:WORD." EXIT THEN
   k RF-LOCAL = IF LOCALBAD-SUGGEST$ EXIT THEN
   k RF-SIG = IF SGBAD-SUGGEST$ EXIT THEN
   k RF-DEAD = IF s" Nothing after a word that never returns runs (throw, die, exit, or a call whose body always ends in one): remove the tokens after it, or move the work before it." EXIT THEN
   k RF-NP = IF NP-SUGGEST$ EXIT THEN
   k RF-CAP = IF s" Move this compiler or runtime boundary behind audited TRUST." EXIT THEN
   k RF-UNSAFE = IF s" Remove it from the checked body; to evaluate text there, use evaluate-closed." EXIT THEN
   k RF-LINLOCAL = IF s" Keep the linear value on the stack; do not bind it to a local." EXIT THEN
   k RF-REASON = IF MDIAG @ MD-SUGGEST$ EXIT THEN
   k RF-UNCHECKABLE = IF UNCK-SUGGEST$ EXIT THEN
   k RF-RETURN = IF
      RSBAD @ 2 = IF s" Read or pop only return-row cells this definition pushed with >r; below them lies the caller's frame." EXIT THEN
      RSBAD @ 3 = IF s" Balance return-stack transfers before the quotation returns." EXIT THEN
      RSBAD @ 4 = IF s" Read or pop only return-row cells this quotation pushed with >r; below them lies the caller's frame." EXIT THEN
      s" Balance return-stack transfers before the definition exits." EXIT
   THEN
   k RF-MISMATCH <> IF s" Inspect the token, signature, and raw stack evidence." EXIT THEN
   ['] ROW-VALUES ROWS-SUGGEST$ ;
\ A packet's position: the file line, column and byte of its token's first byte,
\ and the byte after its last.
variable JLOC-L  variable JLOC-C  variable JLOC-B  variable JLOC-E
\ False when the token's text has no map (src/core/checker.f DIAG-LOCATE). The
\ end is the source's first file byte plus the end's offset in the source.
: JLOCATE ( ptr u8 ptr u8 -- bool )
   {: a:ptr e:ptr :}
   e DIAG>SRC 0= IF drop 0 0= 0= EXIT THEN
   DSRC-B @ + JLOC-E !
   a DIAG-LOCATE 0= IF drop drop drop 0 0= 0= EXIT THEN
   JLOC-B !  JLOC-C !  JLOC-L !
   0 0= ;
\ With no map: the definition's origin plus the token's offset in the checked text.
: JLOC-ORIGIN ( -- )
   TBASE@  TBASE@ FAILB @ +  DIAGL0 @ DIAGC0 @ DIAGB0 @  DIAG-POS
   JLOC-B !  JLOC-C !  JLOC-L !
   DIAGB0 @ FAILE @ + JLOC-E ! ;
: JLOC-FIELDS ( -- )
   s" line" JKEY  JLOC-L @ JNUM  44 EMIT1
   s" column" JKEY  JLOC-C @ JNUM  44 EMIT1
   s" byte_start" JKEY  JLOC-B @ JNUM  44 EMIT1
   s" byte_end" JKEY  JLOC-E @ JNUM  44 EMIT1 ;
: JTOKEN-FIELDS ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 > IF
      a  a u +  JLOCATE IF JLOC-FIELDS THEN
   THEN ;
: NM-SPAN ( -- ptr u8 n )
   NMOFF @ TADDR NMU @ ;
\ A packet opens with JSTART (schema version and code), or JHEAD for a refusal
\ (adds repair class and verdict); JTAIL closes it with the file, the span's
\ place when it locates, and the suggestion. A code's own fields go between.
\ DIAG-JSON, USING-JSON and STGR-JSON close themselves: fields or a differently
\ sourced place follow the file.
: JSTART ( ptr u8 n -- )
   {: ca:ptr cu:n :}
   123 EMIT1                                              \ {
   s" schema_version" JKEY 1 JNUM 44 EMIT1
   s" code" JKEY ca cu JSTR 44 EMIT1 ;
: JHEAD ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: ca:ptr cu:n ka:ptr ku:n va:ptr vu:n :}
   ca cu JSTART
   s" repair_class" JKEY ka ku JSTR 44 EMIT1
   s" verdict" JKEY va vu JSTR 44 EMIT1 ;
: JTAIL ( ptr u8 n ptr u8 n -- )
   {: aa:ptr au:n sa:ptr su:n :}
   s" file" JKEY DIAGFB DIAGFU @ JSTR 44 EMIT1
   aa au JTOKEN-FIELDS
   s" suggestion" JKEY sa su JSTR
   125 EMIT1 ;                                            \ }
: NP-FAM-REND ( -- )
   NPBAD-TERM @ NP-FAM {: fam:n :}
   fam 0 >= IF s" family '" DTXT  fam FAM-QNAME-REND  s" '" DTXT
   ELSE s" a concrete type" DTXT THEN ;
: NP-PROSE ( -- )
   s" E-NONPARAMETRIC-EFFECT " DTXT  DSITE
   NPBAD-KIND @ 4 = IF
     s" declared type variable '" DTXT  NPBAD-Q1 @ EMIT1
     s" ' is restricted to a base address -- the DATA region, or a pointer computed off NULL-PTR; its declared kind must stay unchanged" DTXT EXIT
   THEN
   NPBAD-KIND @ 5 = IF
     s" declared type variable '" DTXT  NPBAD-Q1 @ EMIT1
     s" ' is restricted to the null address; its declared kind must stay unchanged" DTXT EXIT
   THEN
   NPBAD-KIND @ 3 = IF
     s" declared type variable '" DTXT  NPBAD-Q1 @ EMIT1
     s" ' is restricted by raw storage; its declared kind must stay unchanged" DTXT EXIT
   THEN
   NPBAD-KIND @ 0= IF
     s" declared type variable '" DTXT  NPBAD-Q1 @ EMIT1
     s" ' is specialized to " DTXT  NP-FAM-REND
     s" ; a declared effect must stay parametric over its quantifier" DTXT EXIT
   THEN
   NPBAD-KIND @ 1 = IF
     s" declared type variables '" DTXT  NPBAD-Q1 @ EMIT1
     s" ' and '" DTXT  NPBAD-Q2 @ EMIT1
     s" ' are unified; each declared quantifier must stay a distinct variable" DTXT EXIT
   THEN
   NPBAD-KIND @ 6 = IF
     s" declared row variable '" DTXT  NPBAD-Q1 @ EMIT1
     NPBAD-Q2 @ IF s" ' and row '" DTXT  NPBAD-Q2 @ EMIT1  s" '" DTXT
     ELSE s" ' and the unnamed row" DTXT THEN
     s"  are unified; each declared row must stay a distinct row variable" DTXT EXIT
   THEN
   NPBAD-KIND @ 7 = IF
     NPBAD-Q1 @ IF s" declared row variable '" DTXT  NPBAD-Q1 @ EMIT1  s" '" DTXT
     ELSE s" the unnamed row" DTXT THEN
     s"  is bound to fixed cells; a declared effect must stay polymorphic over its row" DTXT EXIT
   THEN
   s" declared type variable '" DTXT  NPBAD-Q1 @ EMIT1
   s" ' is minted into " DTXT  NP-FAM-REND
   s"  but is unbound in the inputs; a checked definition cannot mint a phantom of input-unrelated type" DTXT ;
\ The return rows `return_stack` names, expected then actual: a quotation
\ literal's own when its seal refused it (RSBAD 3, 4), else the definition's.
: RS-ROWS ( -- n n )
   RSBAD @ 3 >= IF QRS-EXP @ QRS-ACT @ EXIT THEN
   SGHASR @ IF SGROUT @ ELSE RBROW @ THEN  RCUR @ ;

: RS-REASON$ ( -- ptr u8 n )
   RSBAD @ 2 = IF s" return stack: this reads below the return-stack cells pushed here" EXIT THEN
   RSBAD @ 3 = IF s" return stack: the quotation leaves its return row unbalanced" EXIT THEN
   RSBAD @ 4 = IF s" return stack: the quotation reads below the return-stack cells it pushed" EXIT THEN
   s" return stack: the return row does not balance at exit" ;

: ROWS-REFUSAL? ( n -- bool )
   {: k:n :}
   k RF-MISMATCH =  k RF-REASON = or ;


: DIAG-PROSE
   REFUSAL@ {: k:n :}
   k RF-IMM = IF
     IMM-CODE$ DTXT  s"  " DTXT  DSITE
     s" compile-time immediate '" DTXT  FAILTK FAILTU @ DTXT
     s" ' is not a stack-neutral parsing immediate" DTXT EXIT
   THEN
   k RF-SIG = IF SGBAD-PROSE EXIT THEN
   k RF-NP = IF NP-PROSE EXIT THEN
   k RF-CAP = IF
     DCODE DTXT  s"  " DTXT  DSITE
     s" '" DTXT  FAILTK FAILTU @ DTXT
     s" ' is a trust-boundary primitive" DTXT EXIT
   THEN
   k RF-UNSAFE = IF
     DCODE DTXT  s"  " DTXT  DSITE
     s" '" DTXT  FAILTK FAILTU @ DTXT
     s" ' reads source or changes the compiler's or checker's state, which no stack" DTXT
     s"  effect describes, so a checked body cannot use it" DTXT EXIT
   THEN
   k RF-LOCAL = IF LOCALBAD-PROSE EXIT THEN
   k RF-QUAL = IF
     s" E-BAD-QUALIFIED " DTXT  DSITE
     s" malformed qualified name '" DTXT  FAILTK FAILTU @ DTXT
     s" ' (more than one ':')" DTXT EXIT
   THEN
   k RF-UNDEF = IF
     s" E-UNDEFINED " DTXT  DSITE
     s" undefined word '" DTXT  FAILTK FAILTU @ DTXT  s" '" DTXT EXIT
   THEN
   k RF-LINLOCAL = IF
     s" E-LINEAR-LOCAL " DTXT  DSITE
     s" linear value cannot be bound to a local" DTXT EXIT
   THEN
   DSITE  s" at '" DTXT  FAILTK FAILTU @ DTXT
   s" '" DTXT
   k RF-REASON = IF
     s"  " DTXT  MDIAG @ MD-REASON$ DTXT
     MDIAG @ MD-NONEXH = IF MDIAG-MISSING-PROSE THEN
     MDIAG-COUNTS
   THEN
   k RF-DEAD = IF s"  after '" DTXT DEADTA @ DEADTU @ DTXT s" '" DTXT THEN
   k RF-RETURN = IF
     s"  " DTXT  RS-REASON$ DTXT
     RS-ROWS {: exp:n act:n :}
     s"  expected: " DTXT  exp DROW
     s" actual: " DTXT  act DROW EXIT
   THEN
   k ROWS-REFUSAL? 0= IF EXIT THEN
   MDIAG @ MD-TAIL-ESCAPE = IF
     s"  expected: " DTXT  TCAP-EXP$ DTXT
     s" actual: " DTXT  TCAP-ACT$ DTXT EXIT THEN
   AT-PIN
   DEXP @ 0 <> IF
     s"  expected: " DTXT  DEXP @ DROW
     s" actual: " DTXT  DACT @ DROW THEN
   DF-ACT @ 0 <>  DEXP @ 0= and IF
     s"  actual: " DTXT  DF-ACT @ REND-TYPE THEN
   ;AT-PIN ;


\ Expected takes precedence over actual, so unrelated matched row cells cannot
\ leak into the diagnostic; a scalar pair emits no family. The hint spells the
\ family as FAM-QNAME-REND does, matching the rows and resolving from the
\ failing definition's scope.
: TERM-FAM ( n -- n )                    \ layout-family id for one type term, else -1
   T-RES dup LAYOUT-PARAM? IF PARAM>FAM EXIT THEN
   drop -1 ;
\ DVAR is the SUMV id a `construct family variant` payload mismatch latched, -1
\ for none: a scalar or non-construct mismatch emits no variant or tag.
: DIAG-FAM-ID ( n n -- n )
   {: efam:n afam:n :}
   efam 0 >= IF efam EXIT THEN
   afam 0 >= IF afam EXIT THEN
   DVAR @ dup 0 >= IF SUMV-FAM@ THEN ;
\ DPOS is the slot-from-top of the failed expected-row element; the packet
\ reports the 0-based declaration-order payload index, absent when the position
\ is unknown or outside the variant's payload cells.
: DIAG-PAYLOAD-POS ( -- )                \ "payload_pos":<decl-order slot> when captured
   DPOS @ 0 < IF EXIT THEN
   DVAR @ SUMV-PAY-N {: cnt:n :}
   DPOS @ cnt < 0= IF EXIT THEN
   cnt 1 - DPOS @ - {: pos:n :}
   44 EMIT1 s" payload_pos" JKEY  pos JNUM
   DVAR @ pos SUMV-PAY-FIELD IF
      44 EMIT1 s" field" JKEY  PF-NAME$ JSTR
   ELSE drop THEN ;
: DIAG-VARIANT ( -- )                    \ "variant":"<name>","tag":<n> for the captured arm
   DVAR @ 0 < IF EXIT THEN
   44 EMIT1 s" variant" JKEY  DVAR @ SUMV-NAME$ JSTR
   44 EMIT1 s" tag" JKEY  DVAR @ SUMV-TAG@ JNUM
   DIAG-PAYLOAD-POS ;
: DIAG-FAMILY ( -- )
   DF-EXP @ TERM-FAM  DF-ACT @ TERM-FAM  DIAG-FAM-ID {: fam:n :}
   fam 0 >= IF
      44 EMIT1 s" family" JKEY  JOPEN fam FAM-QNAME-REND JCLOSE
   THEN
   DIAG-VARIANT ;
\ A top step's file as its prose names it (DSITE), its line the one checker.f
\ CHECK:TOP-SITE gives (DLOC-FIELDS); else the checked token in its
\ mapped or origin source.
: DFILE$ ( -- ptr u8 n )
   TOP-STEP @ 0= IF DIAGFB DIAGFU @ EXIT THEN
   TOP-FILE$ dup 0 > IF EXIT THEN drop drop
   DIAG-NO-FILE$ ;
: DLOC-FIELDS ( -- )
   TOP-STEP @ IF
      CHECK:TOP-SITE IF s" line" JKEY  JNUM  44 EMIT1 ELSE drop THEN
      EXIT
   THEN
   TBASE@ FAILB @ +  TBASE@ FAILE @ +  JLOCATE IF JLOC-FIELDS EXIT THEN
   DIAG-MAPPED? IF EXIT THEN   \ mapped text no source holds
   JLOC-ORIGIN JLOC-FIELDS ;

\ The reason a signature too large to record names its bound, count and limit
\ by: a definition's packet (DIAG-JSON) and a stored row's (BADSIG-JSON).
: BADSIG-REASON ( -- )
   SGBAD-BINDERS? IF BINDERS-REASON EXIT THEN
   SGBAD-DEPTH? IF DEPTH-REASON$ DTXT  SGBAD-SIZE @ DEPTH-COUNTS EXIT THEN
   WIDTH-REASON$ DTXT  SGBAD-SIZE @ WIDTH-COUNTS ;


\ A field that belongs to one refusal goes only on that refusal's packet: the
\ flags of a failure the refusal outranks leave their fields behind. A top-level
\ step has no definition, and its packet says so with a null word and null
\ definition fields.
: DIAG-JSON
   REFUSAL@ {: k:n :}
   DCODE REPAIR-CLASS DVERDICT JHEAD
   s" word" JKEY
   TOP-STEP @ IF s" null" DTXT ELSE NMA @ NMU @ JSTR THEN  44 EMIT1
   s" token" JKEY  FAILTK FAILTU @ JSTR  44 EMIT1
   k RF-DEAD = IF s" dead_owner" JKEY DEADTA @ DEADTU @ JSTR 44 EMIT1 THEN
   k RF-REASON = IF
     s" reason" JKEY
     \ A reason's counts go inside its string, after its text.
     JOPEN  MDIAG @ MD-REASON$ DTXT  MDIAG-COUNTS  JCLOSE
     44 EMIT1
     MDIAG @ MD-NONEXH = IF s" missing_variants" JKEY MDIAG-MISSING-JSTR 44 EMIT1 THEN
   THEN
   k RF-SIG = SGBAD-SIZE? and IF s" reason" JKEY JOPEN BADSIG-REASON JCLOSE 44 EMIT1 THEN
   k RF-LOCAL = LOCALBAD-GROUP? and IF s" reason" JKEY LOCALBAD-REASON$ JSTR 44 EMIT1 THEN
   k RF-RETURN = IF s" reason" JKEY RS-REASON$ JSTR 44 EMIT1 THEN   \ the sentence the load's prose prints
   s" token_index" JKEY
   TOP-STEP @ IF s" null" DTXT ELSE FAILIX @ JNUM THEN  44 EMIT1
   s" file" JKEY  DFILE$ JSTR  44 EMIT1
   DLOC-FIELDS
   s" definition_source" JKEY
   TOP-STEP @ IF s" null" DTXT ELSE TBASE @ TBLEN @ JSTR THEN  44 EMIT1
   s" declared_effect" JKEY
   RECEFF-ON? TOP-STEP @ 0= and IF AT-PIN DECL-EFFECT ;AT-PIN ELSE s" null" DTXT THEN
   44 EMIT1
   s" declared_effect_source" JKEY
   RECEFF-ON? TOP-STEP @ 0= and IF SGA @ SGU @ SIG-TRIM JSTR ELSE s" null" DTXT THEN
   44 EMIT1
   s" inferred_effect" JKEY
   TOP-STEP @ IF s" null" DTXT ELSE
      MDIAG @ MD-TAIL-ESCAPE = IF TCAP-INF$ JSTR ELSE JOPEN INFERRED-EFFECT JCLOSE THEN
   THEN
   44 EMIT1
   s" return_stack" JKEY
   123 EMIT1
   RS-ROWS swap
   s" expected" JKEY  JROW  44 EMIT1
   s" actual" JKEY    JROW
   125 EMIT1
   k ROWS-REFUSAL? IF
     MDIAG @ MD-TAIL-ESCAPE = IF
       44 EMIT1 s" expected" JKEY TCAP-EXP$ JSTR
       44 EMIT1 s" actual"   JKEY TCAP-ACT$ JSTR
     ELSE
       AT-PIN
       DEXP @ 0 <> IF
         44 EMIT1 s" expected" JKEY DEXP @ JROW
         44 EMIT1 s" actual"   JKEY DACT @ JROW
         DIAG-FAMILY THEN
       DF-ACT @ 0 <>  DEXP @ 0= and IF
          44 EMIT1 s" actual_type" JKEY 34 EMIT1 DF-ACT @ REND-TYPE 34 EMIT1 THEN
       ;AT-PIN
     THEN
   THEN
   k RF-NP = IF                                           \ non-parametric declared effect
     NPBAD-Q1 @ IF 44 EMIT1 s" quantifier" JKEY JOPEN NPBAD-Q1 @ EMIT1 JCLOSE THEN
     NPBAD-Q2 @ IF
       44 EMIT1 s" quantifier2" JKEY JOPEN NPBAD-Q2 @ EMIT1 JCLOSE
     ELSE
       NPBAD-TERM @ NP-FAM dup 0 >= IF
         44 EMIT1 s" family" JKEY JOPEN FAM-QNAME-REND JCLOSE
       ELSE drop THEN
     THEN
   THEN
   k RF-SIG =  SGBAD-ARITY? and IF                        \ E-WRONG-ARITY counts
     44 EMIT1 s" arity_expected" JKEY SGBAD-AR-DECL @ JNUM
     44 EMIT1 s" arity_actual" JKEY SGBAD-AR-GOT @ JNUM THEN
   44 EMIT1 s" suggestion" JKEY SUGGEST-TEXT JSTR
   125 EMIT1 ;                                            \ }


: DIAG-PRINT
   1 RDST !  0 RSN !  0 RQM !  SEEN-RESET 0 NLET !  0 RROW-N !
   JSON-DIAGS @ IF DIAG-JSON ELSE
     DIAG-PROSE  s" ; suggestion: " DTXT  SUGGEST-TEXT DTXT
   THEN
   10 EMIT1
   RSBUF-FLUSH
   JSON-DIAGS @ IF CHECKER-REPORT-MARK THEN ;



\ Bad stored-signature diagnostics (checker.f USIG-ADD-BAD, every mode). SGBAD's
\ state from the failed parse is still live, so class and suggestion are a
\ signature's; a row that parsed but is too deep or too wide to record has its
\ own class and a reason naming the bound, count and limit. The JSON is a refused
\ record (its tools/diag-code.f row), placed at the declaring name AT in the
\ checked text.
: BADSIG-JSON ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: sa:ptr su:n na:ptr nu:n at:ptr atu:n :}
   s" E-BAD-STORED-SIGNATURE" SGBAD-CLASS$ s" rejected" JHEAD
   s" token" JKEY na nu JSTR 44 EMIT1
   s" signature" JKEY sa su SIG-TRIM JSTR 44 EMIT1
   SGBAD-SIZE? IF s" reason" JKEY JOPEN BADSIG-REASON JCLOSE 44 EMIT1 THEN
   at atu SGBAD-SUGGEST$ JTAIL ;
: BADSIG-PROSE ( ptr u8 n ptr u8 n -- )
   {: sa:ptr su:n na:ptr nu:n :}
   s" habu: in " DTXT  na nu DTXT  s" : bad stored signature '" DTXT
   sa su SIG-TRIM DTXT  s" '" DTXT
   SGBAD-SIZE? IF s"  " DTXT BADSIG-REASON THEN
   SGBAD-RETURN? IF s" ; return-stack effect: " DTXT RCLAUSE-FIX$ DTXT THEN ;
: BADSIG-DIAG ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: sa:ptr su:n na:ptr nu:n at:ptr atu:n :}
   1 RDST !  0 RSN !
   JSON-DIAGS @ IF sa su na nu at atu BADSIG-JSON ELSE sa su na nu BADSIG-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;
: BADSIG-DIAG-INSTALL ( -- ) [: BADSIG-DIAG ;] is BADSIG-XT ;
BADSIG-DIAG-INSTALL

\ A bad NEWTYPE or SUMTYPE declaration reports a declaration-shaped packet (kind,
\ family, token, reason) with no definition fields.
: TDECL-SUGGEST$ ( -- ptr u8 n )
   s" Repair the family declaration: unique lowercase names, exact arity, closed VARIANT blocks." ;
: TDECL-DIAG-JSON ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: ka:ptr ku:n fa:ptr fu:n ta:ptr tu:n wa:ptr wu:n :}
   s" E-BAD-DECLARATION" s" fix_family_declaration" s" rejected" JHEAD
   s" decl" JKEY ka ku JSTR 44 EMIT1
   s" family" JKEY fa fu JSTR 44 EMIT1
   s" token" JKEY ta tu JSTR 44 EMIT1
   s" reason" JKEY wa wu JSTR 44 EMIT1
   ta tu TDECL-SUGGEST$ JTAIL ;
: TDECL-DIAG-PROSE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: ka:ptr ku:n fa:ptr fu:n ta:ptr tu:n wa:ptr wu:n :}
   s" habu: bad " DTXT  ka ku DTXT  s"  declaration '" DTXT  fa fu DTXT
   s" ': " DTXT  wa wu DTXT
   tu 0 > IF s"  at '" DTXT  ta tu DTXT  s" '" DTXT THEN ;
: TDECL-DIAG ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: ka:ptr ku:n fa:ptr fu:n ta:ptr tu:n wa:ptr wu:n :}
   1 RDST !  0 RSN !
   ka ku fa fu ta tu wa wu
   JSON-DIAGS @ IF TDECL-DIAG-JSON ELSE TDECL-DIAG-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;

\ Why REC-SIG left a certified sig-less word unrecorded; the last arm answers
\ every other RQM cause, a stale or scoped type among them, as an unmodeled tag.
: REC-REFUSE-WHY ( -- )
   NLET @ TYPE-VAR-MAX > IF
      s" more than " DTXT  TYPE-VAR-MAX JNUM
      s"  type variables or binders in inferred effect" DTXT EXIT
   THEN
   RBIND-FULL @ IF BINDERS-REASON s"  in inferred effect" DTXT EXIT THEN
   RROW-UNSAID @ IF
      s" quotation return-stack clause or more than " DTXT  RROW-CAP JNUM
      s"  named rows the effect text cannot say" DTXT EXIT
   THEN
   s" unmodeled type tag in inferred effect" DTXT ;

: REC-REFUSE-PROSE ( ptr u8 n -- )
   {: na:ptr nu:n :}
   s" habu: in " DTXT  na nu DTXT
   s" : effect not recorded: " DTXT  REC-REFUSE-WHY ;

\ W-EFFECT-NOT-RECORDED is a warning outside the repair contract: the word
\ certified and loaded, but a later call refuses it as undefined. The JSON
\ carries no verdict, repair class or suggestion, and the run's status stands.
: REC-REFUSE-JSON ( ptr u8 n -- )
   {: na:ptr nu:n :}
   s" W-EFFECT-NOT-RECORDED" JSTART
   s" word" JKEY na nu JSTR 44 EMIT1
   s" file" JKEY DIAGFB DIAGFU @ JSTR 44 EMIT1
   NM-SPAN JTOKEN-FIELDS
   s" reason" JKEY JOPEN REC-REFUSE-WHY JCLOSE
   125 EMIT1 ;

: REC-REFUSE-EMIT ( ptr u8 n -- )
   {: na:ptr nu:n :}
   1 RDST !  0 RSN !
   na nu JSON-DIAGS @ IF REC-REFUSE-JSON ELSE REC-REFUSE-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;

\ Check's run turns this off, as the check before it wrote the placed warnings;
\ a session the run's program starts turns it on (CHECK-ALL-ERRORS:SESSION).
variable WARN-DIAGS   -1 WARN-DIAGS !

: REC-REFUSE-DIAG ( ptr u8 n -- )
   WARN-DIAGS @ 0= IF 2drop EXIT THEN
   REC-REFUSE-EMIT ;

\ A sig-less word's inferred effect is recorded (CHECKER-USIG-CERT-CURRENT) unless
\ its text sets RQM or has more than TYPE-VAR-MAX names.
: REC-SIG ( ptr u8 n -- )
   {: na:ptr nu:n :}
   REND-SIG 2drop
   RQM @ 0 =  NLET @ TYPE-VAR-MAX 1 + <  and IF
      na nu CHECKER-USIG-CERT-CURRENT
      EXIT
   THEN
   BROW @ DCUR @ CHECK-UNRECORDED!
   na nu REC-REFUSE-DIAG ;

\ Global-vs-used-public shadow (CK-SHADOW-CAPTURE): the reject names both
\ candidates with their arity. Effects read through the read-only USIGS accessors
\ (CHECKER-FIND-USIG-SYM, E-DIN@, E-DOUT@, EFF-ROW-N), never the bare-name
\ resolver, so rendering cannot re-trigger the shadow throw.
: USH-EFF ( n -- )                        \ append " (Din -- Dout)" for a sym's effect, " (?)" if none
   {: sym:n :}
   sym CHECKER-FIND-USIG-SYM 0= IF s"  (?)" DTXT EXIT THEN
   s"  (" DTXT
   FEP @ E-DIN@ EFF-ROW-N RNUM
   s"  -- " DTXT
   FEP @ E-DOUT@ EFF-ROW-N RNUM
   41 EMIT1 ;
: USHADOW-PROSE ( -- )
   s" E-USING-SHADOW-GLOBAL habu: bare '" DTXT  USH-TOK-A @ USH-TOK-U @ DTXT
   s" ' is ambiguous under using: the global '" DTXT  USH-TOK-A @ USH-TOK-U @ DTXT  39 EMIT1
   USH-GSYM @ USH-EFF
   s"  and used public '" DTXT  USH-PKG-A @ USH-PKG-U @ DTXT  58 EMIT1  USH-TOK-A @ USH-TOK-U @ DTXT  39 EMIT1
   USH-USYM @ USH-EFF
   s"  export the same name; qualify " DTXT  USH-PKG-A @ USH-PKG-U @ DTXT  58 EMIT1  USH-TOK-A @ USH-TOK-U @ DTXT
   s"  for the package word, or rename the collision to reach the global" DTXT ;
\ The used packages a bare token resolves in: the slots checker.f CK-USED-MARK
\ set in CK-USED-MASK, in using-scan order, each package once.
: UPKG-MATCHED? ( n -- bool )             \ used-scan slot n exports the token
   1 swap lshift CK-USED-MASK @ and 0 <> ;
: UPKG-NAME$ ( n -- ptr u8 n )            \ slot n's folded package name
   dup CK-USE-SLOT swap CK-USE-LEN@ ;
: UPKG-FIRST? ( n -- bool )               \ slot n matched and no earlier slot names its package
   {: slot:n :}
   slot UPKG-MATCHED?
   slot 0 ?DO
      i UPKG-MATCHED? IF  i UPKG-NAME$ slot UPKG-NAME$ CORE-STR= IF drop RES-FALSE THEN  THEN
   LOOP ;
: UPKG-EACH ( [ n bool -- ] -- )          \ XT on each package's slot, true the first time
   {: xt :}
   RES-TRUE
   USE-MAX 0 ?DO
      i UPKG-FIRST? IF  i over xt execute  drop RES-FALSE  THEN
   LOOP drop ;
\ `used_packages` lists each used package the token resolves in, between the
\ token's place and the suggestion, so this packet closes itself. For the shadow
\ that is one package: every matched slot holds its one used public.
: USING-JSON ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: code:ptr codeu:n class:ptr classu:n sug:ptr sugu:n :}
   code codeu class classu s" rejected" JHEAD
   s" token" JKEY USH-TOK-A @ USH-TOK-U @ JSTR 44 EMIT1
   s" file" JKEY DIAGFB DIAGFU @ JSTR 44 EMIT1
   USH-TOK-A @ USH-TOK-U @ JTOKEN-FIELDS
   s" used_packages" JKEY 91 EMIT1
   [: 0= IF 44 EMIT1 THEN  UPKG-NAME$ JSTR ;] UPKG-EACH
   93 EMIT1 44 EMIT1
   s" suggestion" JKEY sug sugu JSTR
   125 EMIT1 ;
: USHADOW-JSON ( -- )
   s" E-USING-SHADOW-GLOBAL" s" disambiguate_using_shadow"
   s" A global word and a used package public share this name. Qualify the package word as PKG:WORD, or rename the collision; the global has no bare qualifier."
   USING-JSON ;
: USHADOW-DIAG ( -- )
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF USHADOW-JSON ELSE USHADOW-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;

\ A package public its own private tail shadows (checker.f SHADOW-ARITY-CK). The
\ widths come from the check, not the store: the rule runs before the append, so
\ the public has no row to read. Both sides are in cells, what the compiler reads
\ a contract in. Package and tail come from the private twin's symbol, already
\ folded.
: SBA-CELLS-TXT ( n n -- )                \ append " (in -- out)"
   {: in:n out:n :}
   s"  (" DTXT  in RNUM  s"  -- " DTXT  out RNUM  41 EMIT1 ;
: SBARITY-PROSE ( -- )
   s" E-SHADOWED-ARITY habu: public '" DTXT
   SBA-TWIN @ SYM-PKG$ DTXT  58 EMIT1  SBA-TWIN @ SYM-NAME$ DTXT  39 EMIT1
   SBA-NIN @ SBA-NOUT @ SBA-CELLS-TXT
   s"  does not bind its own name: the same package's private '" DTXT
   SBA-TWIN @ SYM-NAME$ DTXT  39 EMIT1
   SBA-PIN @ SBA-POUT @ SBA-CELLS-TXT
   s"  owns that bare tail, a definition's contract is read from the binding its" DTXT
   s"  own name has, and the two do not move the same cells - so the public word" DTXT
   s"  would be compiled against the private word's arity. Declare the same" DTXT
   s"  effect on both (a public forwarder repeats its private word's signature)" DTXT
   s"  or rename one of the two" DTXT ;
: SBARITY-JSON ( -- )
   s" E-SHADOWED-ARITY" s" match_shadowed_private_effect" s" rejected" JHEAD
   s" token" JKEY SBA-TWIN @ SYM-NAME$ JSTR 44 EMIT1
   s" package" JKEY SBA-TWIN @ SYM-PKG$ JSTR 44 EMIT1
   NM-SPAN s" A private word of this package owns the same tail, and a bare tail binds the private word first, so the native compiler reads this definition's arity from it. Give the public definition the private word's effect, or rename one of the two." JTAIL ;
: SBARITY-DIAG ( -- )
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF SBARITY-JSON ELSE SBARITY-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;
\ A bare token two used publics export: the
\ reject names each package it resolves in.
: UAMB-PROSE ( -- )
   s" E-USING-AMBIGUOUS habu: bare '" DTXT  USH-TOK-A @ USH-TOK-U @ DTXT
   s" ' is ambiguous under using: used publics " DTXT
   [: 0= IF s" , " DTXT THEN
      39 EMIT1  UPKG-NAME$ DTXT  58 EMIT1  USH-TOK-A @ USH-TOK-U @ DTXT  39 EMIT1 ;] UPKG-EACH
   s"  export the same name; qualify the one meant as PKG:" DTXT  USH-TOK-A @ USH-TOK-U @ DTXT
   s" , or rename the collision" DTXT ;
: UAMB-JSON ( -- )
   s" E-USING-AMBIGUOUS" s" disambiguate_using_ambiguous"
   s" Used publics of more than one package share this name. Qualify the one meant as PKG:WORD, or rename the collision."
   USING-JSON ;
: UAMB-DIAG ( -- )
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF UAMB-JSON ELSE UAMB-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;
\ A top-level token the load refuses, a stretch the pre-pass deferred, or a
\ deferred definition: records at the token, no definition fields. A
\ refusal has its own code (tools/diag-code.f) with the class and suggestion a
\ body's reference gets; a deferral is no refusal, under W-CHECK-DEFERRED.
: TOP-SPAN-JSON ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: c:ptr cu:n k:ptr ku:n v:ptr vu:n g:ptr gu:n :}
   c cu k ku v vu JHEAD
   s" token" JKEY USH-TOK-A @ USH-TOK-U @ JSTR 44 EMIT1
   USH-TOK-A @ USH-TOK-U @ g gu JTAIL ;
\ `CODE habu: FILE:LINE:COLUMN: TEXT 'TOKEN'`, the place when the token locates.
: TOP-SPAN-PROSE ( ptr u8 n ptr u8 n -- )
   {: c:ptr cu:n t:ptr tu:n :}
   c cu DTXT  s"  habu: " DTXT
   USH-TOK-A @  USH-TOK-A @ USH-TOK-U @ +  JLOCATE IF
      DIAGFB DIAGFU @ DTXT  58 EMIT1  JLOC-L @ JNUM  58 EMIT1  JLOC-C @ JNUM  s" : " DTXT
   THEN
   t tu DTXT  s"  '" DTXT  USH-TOK-A @ USH-TOK-U @ DTXT  39 EMIT1 ;
: TOP-UNDEF-DIAG ( -- )
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF
      s" E-UNDEFINED-TOP-LEVEL" s" unknown_rejection" s" rejected"
      s" Inspect the token, signature, and raw stack evidence." TOP-SPAN-JSON
   ELSE s" E-UNDEFINED-TOP-LEVEL" s" undefined word" TOP-SPAN-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;
: TOP-BADQUAL-DIAG ( -- )
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF
      s" E-BAD-QUALIFIED-TOP-LEVEL" s" fix_qualified_name" s" rejected"
      s" Use one ':' qualifier, e.g. PKG:WORD." TOP-SPAN-JSON
   ELSE s" E-BAD-QUALIFIED-TOP-LEVEL" s" malformed qualified name" TOP-SPAN-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;
: TOP-ARENA-DIAG ( -- )
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF
      s" E-ARENA-CALL-TOP-LEVEL"  MD-ARENA-CALL MD-CLASS$  s" rejected"
      MD-ARENA-CALL MD-SUGGEST$ TOP-SPAN-JSON
   ELSE s" E-ARENA-CALL-TOP-LEVEL"  MD-ARENA-CALL MD-REASON$ TOP-SPAN-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;
\ Only the verifier's child reports a deferral, and it writes packets. Its
\ cause is cleared once written, so a report that records none prints none.
: TOP-DEFERRED-DIAG ( -- )
   1 RDST !  0 RSN !  0 RQM !
   s" W-CHECK-DEFERRED" s" rewrite_uncheckable" s" deferred"
   DEFER-SUGGEST$ TOP-SPAN-JSON
   10 EMIT1
   RSBUF-FLUSH
   0 DEFER-WHY ! ;
\ checker.f SHADOW-DIAG-XT selects: 0 the using-shadow reference site, 1 the
\ arity-shadow definition site, 2 a using ambiguity, 3 and 4 a top-level token
\ undefined or malformed, 5 a deferred top-level stretch or definition, 6 a
\ top-level token naming the storage runtime. One hook
\ because every defer before `: TRUST` takes a slot of the engine's pre-trust
\ pending table (src/habu/layout.f PD-CAP).
: SHADOW-DIAG ( n -- )
   {: sel:n :}
   sel 1 = IF SBARITY-DIAG EXIT THEN
   sel 2 = IF UAMB-DIAG EXIT THEN
   sel 3 = IF TOP-UNDEF-DIAG EXIT THEN
   sel 4 = IF TOP-BADQUAL-DIAG EXIT THEN
   sel 5 = IF TOP-DEFERRED-DIAG EXIT THEN
   sel 6 = IF TOP-ARENA-DIAG EXIT THEN
   USHADOW-DIAG ;
: SHADOW-DIAG-INSTALL ( -- ) [: SHADOW-DIAG ;] is SHADOW-DIAG-XT ;
SHADOW-DIAG-INSTALL

\ A `trust` row naming a word the engine resolves to nothing. It sits beside the
\ shadow diagnostic because it stops a stale row from becoming one. Placed at the
\ row's name (checker.f TRUST) when that lies in checked text.
: TSTALE-PROSE ( ptr u8 n -- )
   {: na:ptr nu:n :}
   s" E-TRUST-UNRESOLVED habu: trust row for '" DTXT  na nu DTXT
   s" ' names no word where its record lands: nothing in the open" DTXT
   s"  section's wordlist, the global wordlist outside a package, or PKG's" DTXT
   s"  public wordlist for PKG:TAIL is" DTXT
   s"  spelled that way, so the effect would be recorded against a symbol" DTXT
   s"  the engine never defined. Delete the row, correct the name to the" DTXT
   s"  word it was meant to describe, or write it in the section that" DTXT
   s"  defines that word" DTXT ;
: TSTALE-JSON ( ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n pa:ptr pu:n :}
   s" E-TRUST-UNRESOLVED" s" fix_stale_trust_row" s" rejected" JHEAD
   s" token" JKEY na nu JSTR 44 EMIT1
   pa pu s" This trust row names no word in the wordlist its record lands in: the open section's, the global wordlist outside a package, or PKG's public wordlist for PKG:TAIL. Delete the row if the word is gone, correct the spelling, or write the row in the section that defines the word." JTAIL ;
: TSTALE-DIAG ( ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n pa:ptr pu:n :}
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF na nu pa pu TSTALE-JSON ELSE na nu TSTALE-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;

\ A storage registrar called outside the verifier window (checker.f
\ CHECKER-REPLAY-NAME-OK?): names the word and the definer that makes it soundly.
\ The name is the run's, so the packet carries no place.
: REPLAY-ONLY-PROSE ( ptr u8 n -- )
   {: na:ptr nu:n :}
   s" E-PKG-CONTEXT habu: storage record for '" DTXT  na nu DTXT
   s" ' refused outside the engine's verifier window: a checker storage" DTXT
   s"  registrar records a definer's accessor only while the source pre-pass" DTXT
   s"  replays the definer that defines the word, so from source the record" DTXT
   s"  would certify callers against an effect the engine never binds to that" DTXT
   s"  name. Define the storage with its definer: TYPED-VARIABLE, TYPED-BUFFER," DTXT
   s"  LAYOUT-BUFFER or DYNAMIC-BUFFER" DTXT ;
: REPLAY-ONLY-JSON ( ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n pa:ptr pu:n :}
   s" E-PKG-CONTEXT" s" use_storage_definer" s" rejected" JHEAD
   s" token" JKEY na nu JSTR 44 EMIT1
   pa pu s" A checker storage registrar records a definer's accessor only inside the engine's verifier window. Define the storage with its definer (TYPED-VARIABLE, TYPED-BUFFER, LAYOUT-BUFFER, DYNAMIC-BUFFER) instead of calling the registrar." JTAIL ;
: REPLAY-ONLY-DIAG ( ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n pa:ptr pu:n :}
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF na nu pa pu REPLAY-ONLY-JSON ELSE na nu REPLAY-ONLY-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;

\ A malformed qualified name's record (checker.f CHECKER-RECORD-NAME) has the
\ class and suggestion of E-BAD-QUALIFIED, and a code of its own in a refused
\ record's shape (tools/diag-code.f); the refusal still throws E-BAD-QUALIFIED.
\ Placed at the declaring name (checker.f CHECKER-RECORD-NAME-AT) when in
\ checked text.
: BADQUAL-PROSE ( ptr u8 n -- )
   {: na:ptr nu:n :}
   s" E-BAD-QUALIFIED-RECORD habu: record for '" DTXT  na nu DTXT
   s" ' refused: malformed qualified name, where one non-edge ':' selects a" DTXT
   s"  package and a second ':' names no word. Use one ':' qualifier, e.g." DTXT
   s"  PKG:WORD" DTXT ;
: BADQUAL-JSON ( ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n pa:ptr pu:n :}
   s" E-BAD-QUALIFIED-RECORD" s" fix_qualified_name" s" rejected" JHEAD
   s" token" JKEY na nu JSTR 44 EMIT1
   pa pu s" Use one ':' qualifier, e.g. PKG:WORD." JTAIL ;
: BADQUAL-DIAG ( ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n pa:ptr pu:n :}
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF na nu pa pu BADQUAL-JSON ELSE na nu BADQUAL-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;
\ checker.f RECORD-DIAG-XT selects: 0 the stale trust row, 1 the storage
\ record, 2 the malformed name.
: RECORD-DIAG ( ptr u8 n ptr u8 n n -- )
   {: na:ptr nu:n pa:ptr pu:n which:n :}
   which 1 = IF na nu pa pu REPLAY-ONLY-DIAG EXIT THEN
   which 2 = IF na nu pa pu BADQUAL-DIAG EXIT THEN
   na nu pa pu TSTALE-DIAG ;
: RECORD-DIAG-INSTALL ( -- ) [: RECORD-DIAG ;] is RECORD-DIAG-XT ;
RECORD-DIAG-INSTALL

\ Storage declaration refusals (checker.f CHECKER-STORAGE-REFUSE): the declared
\ word, refused token and reason, the definer standing in for both when the line
\ has no name; no definition fields. A run-time refusal carries no place.
: STGR-NAME-WHY? ( n -- f ) {: why:n :}
   why STG-MALFORMED-NAME =
   why STG-NO-NAME = or
   why STG-QUALIFIED-NAME = or ;
: STGR-COUNT-WHY? ( n -- f ) {: why:n :}
   why STG-BAD-COUNT =  why STG-NO-COUNT = or
   why STG-COUNT-WORD = or ;
: STGR-REASON$ ( n -- ptr u8 n ) {: why:n :}
   why STG-UNKNOWN-TYPE = IF s" unknown type" EXIT THEN
   why STG-MALFORMED-TYPE = IF s" malformed type" EXIT THEN
   why STG-UNSTORABLE-TYPE = IF s" type this definer cannot store" EXIT THEN
   why STG-SCHEME-TYPE = IF s" scheme in a stored type" EXIT THEN
   why STG-XT-TYPE = IF s" xt in a stored type" EXIT THEN
   why STG-WIDTH-TYPE = IF s" a width is not stored: keep its bytes (width>n)" EXIT THEN
   why STG-LINEAR-TYPE = IF s" stored type may hold a linear value" EXIT THEN
   why STG-MALFORMED-NAME = IF s" more than one ':' in name" EXIT THEN
   why STG-BAD-COUNT = IF s" count outside the buffer's extent" EXIT THEN
   why STG-COUNT-WORD = IF s" count resolves to no ( -- n ) word" EXIT THEN
   why STG-NO-TYPE = IF s" no type for" EXIT THEN
   why STG-NO-NAME = IF s" no name for" EXIT THEN
   why STG-NO-PACKAGE = IF s" declared outside any package" EXIT THEN
   why STG-QUALIFIED-NAME = IF s" qualified name" EXIT THEN
   s" no count for" ;
: STGR-CLASS$ ( n -- ptr u8 n ) {: why:n :}
   why STGR-NAME-WHY? IF s" fix_storage_name" EXIT THEN
   why STGR-COUNT-WHY? IF s" fix_storage_count" EXIT THEN
   why STG-NO-PACKAGE = IF s" fix_storage_package" EXIT THEN
   s" fix_storage_type" ;
: STGR-SUGGEST$ ( n -- ptr u8 n ) {: why:n :}
   why STG-QUALIFIED-NAME = IF s" Name the storage without a package prefix." EXIT THEN
   why STGR-NAME-WHY? IF s" Name the storage with at most one inner ':'." EXIT THEN
   why STGR-COUNT-WHY? IF s" Put a positive count before the definer whose cells fit in memory: a literal, a constant or an expression." EXIT THEN
   why STG-NO-PACKAGE = IF s" Declare the storage inside a package." EXIT THEN
   why STG-LINEAR-TYPE = IF s" Keep a linear value on the stack; name a callable over one, or over an open type, with defer." EXIT THEN
   s" Declare the type before the storage, or store a closed, copyable type this definer admits." ;
: STGR-LOCATE ( ptr u8 -- )   \ the token's place, counted from the start of the verifier's buffer
   {: ta:ptr :}
   STGR-SRC-A @ ta 1 1 0 DIAG-ORIGIN-SPAN! ;
: STGR-JSON ( ptr u8 n ptr u8 n n bool -- )
   {: na:ptr nu:n ta:ptr tu:n why:n at:bool :}
   s" E-BAD-STORAGE" why STGR-CLASS$ s" rejected" JHEAD
   s" word" JKEY na nu JSTR 44 EMIT1
   s" token" JKEY ta tu JSTR 44 EMIT1
   s" reason" JKEY why STGR-REASON$ JSTR 44 EMIT1
   s" file" JKEY DIAGFB DIAGFU @ JSTR 44 EMIT1
   at IF
      DIAGL0 @ JLOC-L !  DIAGC0 @ JLOC-C !  DIAGB0 @ JLOC-B !
      DIAGB0 @ tu + JLOC-E !
      JLOC-FIELDS
   THEN
   s" suggestion" JKEY why STGR-SUGGEST$ JSTR
   125 EMIT1 ;
: STGR-PROSE ( ptr u8 n ptr u8 n n bool -- )
   {: na:ptr nu:n ta:ptr tu:n why:n at:bool :}
   at IF
      DIAGFB DIAGFU @ DTXT  58 EMIT1  DIAGL0 @ JNUM  58 EMIT1  DIAGC0 @ JNUM
      s" : " DTXT
   THEN
   s" habu: in " DTXT  na nu DTXT  s" : " DTXT  why STGR-REASON$ DTXT
   s"  '" DTXT  ta tu DTXT  s" '" DTXT ;
: STGR-DIAG ( ptr u8 n ptr u8 n n -- )
   {: na:ptr nu:n ta:ptr tu:n why:n :}
   ta tu STGR-IN-SOURCE? {: at:bool :}
   at IF ta STGR-LOCATE THEN
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF
      na nu ta tu why at STGR-JSON
   ELSE
      na nu ta tu why at STGR-PROSE
   THEN
   10 EMIT1
   RSBUF-FLUSH ;
: STGR-DIAG-INSTALL ( -- ) [: STGR-DIAG ;] is STORAGE-DIAG-XT ;
STGR-DIAG-INSTALL

\ A `generates:` row the checker cannot keep, on
\ TSTALE-JSON's template: the kind says which of four claims failed, each with
\ its own repair (correct the name, delete the row, make D read its name, or
\ the signature refusal's class and text). Placed at the name where the pre-pass scanner read it.
: GENR-BADSIG-PROSE ( -- )
   SGBAD-UNKNOWN? IF
      s" unknown type '" DTXT  SGBAD-A @ SGBAD-U @ DTXT  s" ' in its effect" DTXT EXIT
   THEN
   SGBAD-BAREPTR? IF s" 'ptr' needs an element type, e.g. 'ptr u8' or 'ptr a'" DTXT EXIT THEN
   SGBAD-ARITY? IF
      s" wrong arity for type family '" DTXT  SGBAD-A @ SGBAD-U @ DTXT  s" '" DTXT EXIT
   THEN
   SGBAD-RETURN? IF s" return-stack effect: " DTXT  RCLAUSE-FIX$ DTXT EXIT THEN
   SGBAD-SIZE? IF BADSIG-REASON EXIT THEN
   s" its effect is not a stack-effect comment" DTXT ;
: GENR-PROSE ( ptr u8 n n -- )
   {: ta:ptr tu:n kind:n :}
   s" E-GENERATES-ROW habu: generates: row for '" DTXT  ta tu DTXT
   kind GENR-UNRESOLVED = IF
      s" ' names no word here: the row follows its definer's definition and" DTXT
      s"  names it as the code there spells it. Define the definer first or" DTXT
      s"  correct the name" DTXT EXIT
   THEN
   kind GENR-CREATES = IF
      s" ' already creates: its does> clause, an earlier generates: row or the" DTXT
      s"  definer it wraps already states what it makes. Delete the row" DTXT EXIT
   THEN
   kind GENR-READS-NONE = IF
      s" ' is deferred, or no parse-name reaches it through the words it calls" DTXT
      s"  (a call through a defer or an executed xt shows none), so the check" DTXT
      s"  cannot take the next token as the word a call makes. Delete the row," DTXT
      s"  or make the definer read its name with parse-name, called directly or" DTXT
      s"  through words it calls by name" DTXT EXIT
   THEN
   s" ': " DTXT  GENR-BADSIG-PROSE ;
: GENR-SUGGEST$ ( n -- ptr u8 n )
   {: kind:n :}
   kind GENR-UNRESOLVED = IF
      s" This generates: row names no word here. Write it after the definer's definition, spelled as the definition spells it." EXIT
   THEN
   kind GENR-CREATES = IF
      s" This definer already states what it makes: its does> clause, an earlier generates: row or the definer it wraps. Delete the row." EXIT
   THEN
   kind GENR-READS-NONE = IF
      s" This word is deferred, or no parse-name reaches it through the words it calls (a call through a defer or an executed xt shows none), so the check cannot take the next token as the word a call makes. Delete the row, or make the definer read its name with parse-name, called directly or through words it calls by name, and make the word from that name." EXIT
   THEN
   SGBAD-SUGGEST$ ;
: GENR-CLASS$ ( n -- ptr u8 n )
   {: kind:n :}
   kind GENR-UNRESOLVED = IF s" fix_generates_row" EXIT THEN
   kind GENR-CREATES = IF s" delete_generates_row" EXIT THEN
   kind GENR-READS-NONE = IF s" fix_generates_row" EXIT THEN
   SGBAD-CLASS$ ;
: GENR-JSON ( ptr u8 n n -- )
   {: ta:ptr tu:n kind:n :}
   s" E-GENERATES-ROW" kind GENR-CLASS$ s" rejected" JHEAD
   s" token" JKEY ta tu JSTR 44 EMIT1
   ta tu kind GENR-SUGGEST$ JTAIL ;
: GENR-DIAG ( ptr u8 n n -- )
   {: ta:ptr tu:n kind:n :}
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF ta tu kind GENR-JSON ELSE ta tu kind GENR-PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;

\ A refused `cast:` or `linear:` declaration.
package CAST-ERROR
: CODE$ ( -- ptr u8 n )
   CODE @ E-CAST-ARITY = IF s" E-CAST-ARITY" EXIT THEN
   CODE @ E-CAST-CLASS = IF s" E-CAST-CLASS" EXIT THEN
   CODE @ E-CAST-FAM = IF s" E-CAST-FAM" EXIT THEN
   CODE @ E-CAST-LINEAR = IF s" E-CAST-LINEAR" EXIT THEN
   CODE @ E-CAST-MINT = IF s" E-CAST-MINT" EXIT THEN
   CODE @ E-CAST-SCOPE = IF s" E-CAST-SCOPE" EXIT THEN
   CODE @ E-LINEAR-PAYLOAD = IF s" E-LINEAR-PAYLOAD" EXIT THEN
   CODE @ E-LINEAR-OWNER = IF s" E-LINEAR-OWNER" EXIT THEN
   CODE @ E-LINEAR-SCOPE = IF s" E-LINEAR-SCOPE" EXIT THEN
   CODE @ throw ;
: SUGGEST$ ( -- ptr u8 n )
   CODE @ E-CAST-ARITY = IF s" Declare one term on each side over the same stack below, with no return-stack effect." EXIT THEN
   CODE @ E-CAST-CLASS = IF s" Retype one machine cell: a con, a width-1 family, a pointer chain ending at storage or a quotation, and hand out no atom." EXIT THEN
   CODE @ E-CAST-FAM = IF s" Declare the type before the cast and give each family its declared number of arguments." EXIT THEN
   CODE @ E-CAST-LINEAR = IF s" Remove the linear type or type variable from the cast; a linear value changes hands only through its owner's words." EXIT THEN
   CODE @ E-CAST-MINT = IF s" Declare a cast that mints a pointer or a quotation in its package's private section." EXIT THEN
   CODE @ E-CAST-SCOPE = IF s" Write a scoped cast only as a view pack or unpack, unqualified, in a private section." EXIT THEN
   CODE @ E-LINEAR-PAYLOAD = IF s" Pair one linear type with one non-linear payload: a con or a pointer chain ending at one." EXIT THEN
   CODE @ E-LINEAR-OWNER = IF s" Declare the linear: row in the package that declared the linear type." EXIT THEN
   CODE @ E-LINEAR-SCOPE = IF s" Declare the linear: row unqualified, in its owner's private section." EXIT THEN
   CODE @ throw ;
: WORD$ ( -- ptr u8 n ) WORD-A @ WORD-U @ ;
: REASON ( -- )
   WHY-A @ WHY-U @ DTXT
   TOK-U @ 0 > IF s"  '" DTXT  TOK-A @ TOK-U @ DTXT  s" '" DTXT THEN ;
\ Mapped: the word lies in the text the pre-pass checks, its place in JLOC-*;
\ else the place is the signature's line in the file the load reads, as DSITE's.
: SITE ( -- ptr u8 n n bool bool )
   WORD-A @  WORD-A @ WORD-U @ +  JLOCATE IF
      DIAGFB DIAGFU @ JLOC-L @ RES-TRUE RES-TRUE EXIT
   THEN
   TOP-FILE$  SIG-A @ CHECK:LINE-AT  RES-FALSE ;
: PLACE ( -- )
   SITE {: fa:ptr fu:n line:n known:bool mapped:bool :}
   fu 0= IF EXIT THEN
   fa fu DTXT
   known IF 58 EMIT1 line JNUM THEN
   mapped IF 58 EMIT1 JLOC-C @ JNUM THEN
   s" : " DTXT ;
: PROSE ( -- )
   CODE$ DTXT  s"  habu: " DTXT  PLACE
   KEY-A @ KEY-U @ DTXT  s"  '" DTXT  WORD$ DTXT  s" ': " DTXT  REASON ;
: PACKET ( -- )
   CODE$ s" fix_cast_declaration" s" rejected" JHEAD
   s" token" JKEY WORD$ JSTR 44 EMIT1
   s" reason" JKEY JOPEN REASON JCLOSE 44 EMIT1
   SITE {: fa:ptr fu:n line:n known:bool mapped:bool :}
   s" file" JKEY  fu 0= IF DIAG-NO-FILE$ ELSE fa fu THEN JSTR  44 EMIT1
   mapped IF JLOC-FIELDS ELSE
      known IF s" line" JKEY line JNUM 44 EMIT1 THEN
   THEN
   s" suggestion" JKEY SUGGEST$ JSTR  125 EMIT1 ;
: RENDER ( -- )
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF PACKET ELSE PROSE THEN
   10 EMIT1
   RSBUF-FLUSH ;
;package

\ A `parses:` or `parses-through:` row the checker refused (checker.f
\ CHECKER-PARSES-ROW), named at its target or else its keyword: a target fault
\ takes fix_parses_row, a syntax fault fix_parses_syntax.
: PRS-SYNTAX? ( n -- bool )
   PARSES-NO-TARGET >= ;
: PRS-REASON ( n -- )
   {: kind:n :}
   kind PARSES-UNDEFINED = IF
      s" ' names no word here: the row follows the word's definition and names" DTXT
      s"  it as the code there spells it" DTXT EXIT
   THEN
   kind PARSES-QUALIFIED = IF
      s" ' is a malformed qualified name: one colon between a package and a" DTXT
      s"  public tail" DTXT EXIT
   THEN
   kind PARSES-SHADOW = IF
      s" ' is refused here: a used package's public shadows the global word of" DTXT
      s"  that name" DTXT EXIT
   THEN
   kind PARSES-AMBIGUOUS = IF
      s" ' is refused here: two used packages export that name" DTXT EXIT
   THEN
   kind PARSES-READS-NONE = IF
      s" ' names a word that reads no source after it: only a word that parses" DTXT
      s"  takes a row" DTXT EXIT
   THEN
   kind PARSES-NO-TARGET = IF s" ' has no target after it" DTXT EXIT THEN
   kind PARSES-COUNT = IF
      s" ': its count is not a number of tokens, 0 or more" DTXT EXIT
   THEN
   s" ': its terminators are not a standalone ( , at least one terminator and a" DTXT
   s"  standalone )" DTXT ;
: PRS-PROSE ( ptr u8 n n -- )
   {: ta:ptr tu:n kind:n :}
   s" E-PARSES-ROW habu: parses: row '" DTXT  ta tu DTXT
   kind PRS-REASON ;
: PRS-SUGGEST$ ( n -- ptr u8 n )
   PRS-SYNTAX? IF
      s" Write the row as parses: W n or parses-through: W n ( E1 E2 ): a count of 0 or more, then at least one terminator between a standalone ( and )." EXIT
   THEN
   s" This row's target is no word here that reads the source after it. Write the row after the definition of a word that parses, spelled as the definition spells it." ;
: PRS-CLASS$ ( n -- ptr u8 n )
   PRS-SYNTAX? IF s" fix_parses_syntax" EXIT THEN
   s" fix_parses_row" ;
: ROW-JSON ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: ta:ptr tu:n ca:ptr cu:n la:ptr lu:n sa:ptr su:n :}
   ca cu la lu s" rejected" JHEAD
   s" token" JKEY ta tu JSTR 44 EMIT1
   ta tu sa su JTAIL ;
: PRS-DIAG ( ptr u8 n n -- )
   {: ta:ptr tu:n kind:n :}
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF
      ta tu s" E-PARSES-ROW" kind PRS-CLASS$ kind PRS-SUGGEST$ ROW-JSON
   ELSE
      ta tu kind PRS-PROSE
   THEN
   10 EMIT1
   RSBUF-FLUSH ;

\ A `names:` row the checker refused: with no
\ target fix_names_syntax, else fix_names_row.
: NMS-SYNTAX? ( n -- bool )
   PARSES-NO-TARGET = ;
: NMS-PROSE ( ptr u8 n n -- )
   {: ta:ptr tu:n kind:n :}
   s" E-NAMES-ROW habu: names: row '" DTXT  ta tu DTXT
   kind NAMES-NO-STRING = IF
      s" ' names a word that takes no string: only a word whose input row ends in" DTXT
      s"  ptr u8 n takes a row" DTXT EXIT
   THEN
   kind PRS-REASON ;
: NMS-SUGGEST$ ( n -- ptr u8 n )
   NMS-SYNTAX? IF
      s" Write the row as names: W, after the definition of a word W that looks its string operand up as a name." EXIT
   THEN
   s" This row's target is no word here that takes a string ( ptr u8 n ). Write the row after the definition of a word that looks its string operand up as a name, spelled as the definition spells it." ;
: NMS-CLASS$ ( n -- ptr u8 n )
   NMS-SYNTAX? IF s" fix_names_syntax" EXIT THEN
   s" fix_names_row" ;
: NMS-DIAG ( ptr u8 n n -- )
   {: ta:ptr tu:n kind:n :}
   1 RDST !  0 RSN !  0 RQM !
   JSON-DIAGS @ IF
      ta tu s" E-NAMES-ROW" kind NMS-CLASS$ kind NMS-SUGGEST$ ROW-JSON
   ELSE
      ta tu kind NMS-PROSE
   THEN
   10 EMIT1
   RSBUF-FLUSH ;

;using

\ `+` and `cell+` refuse a layout pointee; bounds stay the caller's.
: record-at ( ptr a n -- ptr a ) cells + ;  INTRINSIC-RECORD-AT INTRINSIC
