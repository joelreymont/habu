\ render.f: renders checker types and rows as text: a residual stack (DCUR), a
\ rejection's diagnostic. REC-SIG records a certified
\ sig-less word's inferred effect under its name, so its callers certify;
\ rendering only decides whether that effect is declarable
\ (RQM, NLET). Compiles after checker.f and before any src/lib/ file: no emit or
\ +!; bytes go out through REJECTION.
using TFAM
using REJECTION

\ RQM: the text cannot say the effect, so REC-SIG does not record it; set where the
\ text writes `?` and where it meets a type or row no signature spells.
variable RQM
\ RROW-UNSAID: the unsaid part is a row, for REC-REFUSE-WHY.
variable RROW-UNSAID
\ RBIND-FULL: a forall past BINDER-DEPTH-MAX nested binders, for REC-REFUSE-WHY.
variable RBIND-FULL

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


: EMIT-UNSAID ( -- ) 1 RQM !  63 EMIT1 ;
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
   DEST @ RSHOW-DST !
   0 DEST !
   s" inferred " RSTR
   name nameu RSTR
   s" : " RSTR
   SEEN-RESET RATOM-RESET 0 NLET !
   t REND-TYPE
   10 EMIT1
   RSHOW-DST @ DEST ! ;
: LOCSHOW-INSTALL ( -- ) [: SHOW-LOCAL-TYPE ;] is LOCSHOWXT ;
LOCSHOW-INSTALL

: DROW {: s :}  s REND-COLLECT
   RBN @ BEGIN dup 0 > WHILE 1 - dup cells RBUF + @ REND-TYPE 32 EMIT1 REPEAT drop ;


: REND-PASS ( n n n n bool -- )
   {: din:n dout:n rin:n rout:n hasr:bool :}
   0 RQM !  0 RROW-UNSAID !  0 RBIND-FULL !  SEEN-RESET RATOM-RESET 0 NLET !  0 RROW-N !  0 RVISIT !
   din 0 RROW-NAME  din DROW
   hasr IF  s" | " EMIT-STR  rin 1 RROW-NAME  rin DROW  THEN
   45 EMIT1  45 EMIT1
   dout 0 RPOS-LETTER {: lo:n :}
   lo 0 <> IF 32 EMIT1 lo EMIT1 THEN
   dout REND-COLLECT
   RBN @ BEGIN dup 0 > WHILE 1 - 32 EMIT1 dup cells RBUF + @ REND-TYPE REPEAT drop
   hasr IF  s"  | " EMIT-STR  rout 1 RROW-NAME  rout DROW  THEN ;


\ Both passes write from the TEXT-LEN it is called with, after what the caller
\ wrote: the first records the row tails, the second writes them as RPOS-NAMED?
\ names them.
: REND-EFFECT ( n n n n bool -- ptr u8 n )
   {: din:n dout:n rin:n rout:n hasr:bool :}
   TEXT-LEN @ {: base:n :}
   1 DEST !  0 RPOS-N !
   1 RNAMES !  base TEXT-LEN !  din dout rin rout hasr REND-PASS
   2 RNAMES !  base TEXT-LEN !  din dout rin rout hasr REND-PASS
   0 RNAMES !  0 DEST !  TEXT TEXT-LEN @ ;


: FACT-TAG ( ptr u8 n n -- ) {: a:ptr u:n letter:n :}
   a u EMIT-STR  32 EMIT1  letter EMIT1  32 EMIT1 ;


: VAR-FACT-TAG ( ptr u8 n n -- )
   {: a:ptr u:n idx:n :}
   a u EMIT-STR  32 EMIT1  idx VAR-NAME  32 EMIT1 ;

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
   s" block " EMIT-STR  first RNUM  32 EMIT1  q RNUM  32 EMIT1 ;

\ `noblock q` when quotation q's block part is a flagged variable no earlier one's is.
: QUOT-NOBLOCK-TAG ( n -- ) {: q:n :}
   q RQUOT@ ISVAR 0= IF EXIT THEN
   q RQUOT-FIRST q <> IF EXIT THEN
   q RQUOT@ PAY NSCOPE@ 0= IF EXIT THEN
   s" noblock " EMIT-STR  q RNUM  32 EMIT1 ;

\ Facts text, `( E ) offset a noscope A nolinear a block 1 2 noblock 1 `,
\ written from the TEXT-LEN it is called with: a caller that wants a fresh text
\ sets `0 TEXT-LEN !` first.
: REND-FACTS ( n n n n bool -- ptr u8 n )
   {: din:n dout:n rin:n rout:n hasr:bool :}
   1 DEST !  s" ( " EMIT-STR
   din dout rin rout hasr REND-EFFECT 2drop
   1 DEST !  s"  ) " EMIT-STR
   0 BEGIN dup NLET @ < WHILE dup VAR-KIND-TAG 1 + REPEAT drop
   0 BEGIN dup NLET @ < WHILE dup VAR-SCOPE-TAG 1 + REPEAT drop
   0 BEGIN dup RROW-N @ < WHILE dup ROW-KIND-TAG 1 + REPEAT drop
   0 BEGIN dup RROW-N @ < WHILE dup ROW-SCOPE-TAG 1 + REPEAT drop
   RVISIT @ SQUOT-CAP min 1 + {: nq:n :}
   2 BEGIN dup nq < WHILE dup QUOT-BLOCK-TAG 1 + REPEAT drop
   1 BEGIN dup nq < WHILE dup QUOT-NOBLOCK-TAG 1 + REPEAT drop
   0 DEST !  TEXT TEXT-LEN @ ;


: CTL-FLAG-TAG ( n n -- )
   {: ctl:n bit:n :}
   ctl 1 bit lshift and 0= IF EXIT THEN
   bit CTL-BIT>TAG$ {: a:ptr u:n :}
   u 0 = IF EXIT THEN
   a u EMIT-STR  32 EMIT1 ;


: CTL-THROWS-TAG ( n -- )
   {: ctl:n :}
   ctl CTL-THROW and 0= IF EXIT THEN
   s" throws " EMIT-STR  ctl XFER-DMASK RNUM  32 EMIT1  ctl XFER-RMASK RNUM  32 EMIT1 ;


: CTL-INTRINSIC-TAG ( n -- )
   {: ctl:n :}
   ctl CTL>INTRINSIC {: id:n :}
   id 0 = IF EXIT THEN
   id INTRINSIC-NAME$ {: a:ptr u:n :}
   u 0 = IF 1 RQM ! EXIT THEN
   s" intrinsic " EMIT-STR  a u EMIT-STR  32 EMIT1 ;


\ Control word n's tags for checker.f SIG-CTL-TAG, in bit order, appended at TEXT-LEN.
\ RQM: a bit XFER-PACK does not keep, an unnamed intrinsic id, masks without CTL-THROW.
: REND-CTL ( n -- ptr u8 n )
   {: ctl:n :}
   1 DEST !
   ctl XFER-FLAGS  ctl XFER-DMASK  ctl XFER-RMASK  XFER-PACK  ctl <> IF 1 RQM ! THEN
   ctl CTL-THROW and 0=  ctl XFER-DMASK ctl XFER-RMASK or 0 <>  and IF 1 RQM ! THEN
   ctl 0 CTL-FLAG-TAG
   ctl CTL-THROWS-TAG
   19 2 ?do
      i CTL-INTRINSIC-SHIFT = IF ctl CTL-INTRINSIC-TAG THEN
      ctl i CTL-FLAG-TAG
   loop
   0 DEST !  TEXT TEXT-LEN @ ;


\ A PE row's flags as facts tags appended at TEXT-LEN; the PE-ACTIVE,
\ PE-TRUSTED-ONLY and PE-TOP-ONLY bits are facts a caller's check reads.
\ `primitive` (PE-ACTIVE) makes the read a primitive row, whose tick is an
\ opaque xt: without it a text reads as the word's declaration, whose tick is
\ its quotation (`['] die`).
: REND-PE ( n -- ptr u8 n )
   {: flags:n :}
   1 DEST !
   flags PE-ACTIVE and 0 <> IF s" primitive " EMIT-STR THEN
   flags PE-TRUSTED-ONLY and 0 <> IF s" trusted-only " EMIT-STR THEN
   flags PE-TOP-ONLY and 0 <> IF s" top-only " EMIT-STR THEN
   0 DEST !  TEXT TEXT-LEN @ ;


\ A parsing immediate's payload count as a facts tag appended at TEXT-LEN,
\ nothing for -1.
: REND-PIMM ( n -- ptr u8 n )
   {: cnt:n :}
   1 DEST !
   cnt 0 >= IF s" parse-imm " EMIT-STR  cnt RNUM  32 EMIT1 THEN
   0 DEST !  TEXT TEXT-LEN @ ;


\ Record off's facts text, appended at TEXT-LEN.
: REND-RECORD ( n -- ptr u8 n )
   {: off:n :}
   off E-PTR E-INST-RESET
   off E-PTR E-HASR@ 0 <> {: hasr:bool :}
   off E-PTR E-DIN@ E-INST  off E-PTR E-DOUT@ E-INST
   hasr IF off E-PTR E-RIN@ E-INST  off E-PTR E-ROUT@ E-INST ELSE 0 0 THEN
   hasr REND-FACTS ;


\ `creates-effect ` and the facts text of n, a created effect as checker.f
\ NORET-CREATES@ answers it, appended at TEXT-LEN; 0 appends nothing. RQM stays
\ set.
: REND-CREATES ( n -- ptr u8 n )
   {: n:n :}
   n 0= IF 0 DEST !  TEXT TEXT-LEN @ EXIT THEN
   RQM @ {: qm:n :}
   1 DEST !  s" creates-effect " EMIT-STR
   n 1 - REND-RECORD
   qm 0 <> IF 1 RQM ! THEN ;


\ `overload `, primitive row k's effect and its tags, appended at TEXT-LEN;
\ checker.f SIG-TAGS reads the rest of the text as the primitive's next row, so
\ the row needs no `primitive`.
: REND-OVERLOAD ( n -- ptr u8 n )
   {: k:n :}
   1 DEST !  s" overload " EMIT-STR
   k PE-EFF@ REND-RECORD 2drop
   k PE-FLAGS@ PE-ACTIVE invert and REND-PE ;


\ FACTS-SAME?'s state for the quotations it runs; FACTS-CLEAN zeroes it, so
\ none outlives a call. FACTS-NEW is the parsed record, offset+1, 0 for none.
variable FACTS-REC   variable FACTS-MARK   variable FACTS-WAS   variable FACTS-NEW


\ Writes record off's facts text into TEXT from its start.
: FACTS-REND ( n -- )
   NEW  0 TEXT-LEN !  REND-RECORD 2drop ;


: FACTS-PARSE ( -- n n n n )
   RES-TRUE SIG-FACTS!
   [: TEXT TEXT-LEN @ PARSE-SIG-RAW ;] [: RES-FALSE SIG-FACTS! ;] finally ;


\ The record is built under symbol 0, as CREATED-RECORD-BUILD builds one, so no
\ lookup sees it; a row no record can hold is refused before E-BUILD-EFFECT dies.
: FACTS-BUILD ( -- )
   NEW
   FACTS-PARSE {: din:n dout:n rin:n rout:n :}
   SGBAD @ 0 <>  SIG-UNRES @ 0 <>  or IF EXIT THEN
   TEXT TEXT-LEN @ din dout rin rout SGBAD-UNFIT!
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
\ symbol's package and built again, the same content. TEXT and TEXT-LEN keep the text.
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
   0 TEXT-LEN !  BROW @ DCUR @ 0 0 RES-FALSE REND-EFFECT ;

variable DSUGE  variable DSUGA
\ A top step names its file, then the line checker.f CHECK:TOP-SITE
\ gives, if any; a step with no file and no label names no place.
: DSITE ( -- )
   TOP-STEP @ 0= IF s" habu: in " EMIT-STR  NMA @ NMU @ EMIT-STR
      s" : " EMIT-STR EXIT THEN
   s" habu: " EMIT-STR
   LOAD-FILE$ {: fa:ptr fu:n :}
   fu 0= IF EXIT THEN
   fa fu EMIT-STR
   CHECK:TOP-SITE IF 58 EMIT1 EMIT-U ELSE drop THEN
   s" : " EMIT-STR ;
: JROW {: s :}  JSON-OPEN  s DROW  JSON-CLOSE ;


\ Under RNAMES each row is named as REND-EFFECT names it (RROW-NAME).
: JEFFECT-TEXT ( n n n n bool -- )
   {: din:n dout:n rin:n rout:n hasr:bool :}
   din 0 RROW-NAME  din DROW  s" -- " EMIT-STR  dout 0 RROW-NAME  dout DROW
   hasr IF
      s" | " EMIT-STR  rin 1 RROW-NAME  rin DROW  s" -- " EMIT-STR  rout 1 RROW-NAME  rout DROW
   THEN ;


\ One DECL-EFFECT pass over the declared rows, written from TEXT-LEN at with the
\ names it started from.
: DECL-PASS ( n n n n bool n n n n -- )
   {: din:n dout:n rin:n rout:n hasr:bool at:n nlet:n rrow:n ratom:n :}
   at TEXT-LEN !  nlet SEEN-FORGET  nlet NLET !  rrow RROW-N !  ratom RATOM-N !  0 RVISIT !
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
   JSON-OPEN  TEXT-LEN @ {: from:n :}
   -1 RDECL !  0 RPOS-N !
   1 RNAMES !  din dout rin rout hasr from nlet rrow ratom DECL-PASS
   2 RNAMES !  din dout rin rout hasr from nlet rrow ratom DECL-PASS
   0 RNAMES !  0 RDECL !
   JSON-CLOSE
   nlet NLET !  rrow RROW-N !  ratom RATOM-N !
   TEXT-LEN @ {: at:n :}
   SGIN @ SGOUT @ SGRIN @ SGROUT @ SGHASR @ JEFFECT-TEXT
   at TEXT-LEN ! ;


: INFERRED-EFFECT ( -- )   \ the body's effect so far: the declared inputs to the current rows
   SGSEEN @ IF SGIN @ ELSE BROW @ THEN
   DCUR @
   SGHASR @ IF SGRIN @ ELSE RBROW @ THEN
   RCUR @
   SGHASR @ JEFFECT-TEXT ;


\ The escape rejection (MD-TAIL-ESCAPE): the inferred effect, then the declared
\ callable and the argument compared with it, rendered with fresh letters into
\ TCAP before later tokens bind their rows. Naming the callable pair, not the stacks around it, it reads the same
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
   DEST @ {: dst:n :}  TEXT-LEN @ {: base:n :}  RQM @ {: qm:n :}
   1 DEST !  SEEN-RESET 0 NLET !  0 RROW-N !
   INFERRED-EFFECT  TEXT-LEN @ base - TCAP-INF !
   -1 RTAILS !
   DF-EXP @ REND-TYPE 32 EMIT1  TEXT-LEN @ base - TCAP-EXP !
   DF-ACT @ REND-TYPE 32 EMIT1  TEXT-LEN @ base - TCAP-ACT !
   TAIL-SKOLEM @ ROW-TAIL RROW-LETTER EMIT1  s"  would escape into " EMIT-STR
   TAIL-ROW @ TCAP-ROW  TEXT-LEN @ base - TCAP-HELD !
   0 RTAILS !
   TCAP-HELD @ TCAP-ROOM
   0 BEGIN dup TCAP-HELD @ < WHILE
      dup base + TEXT + c@  over TCAP + c!  1 +
   REPEAT drop
   base TEXT-LEN !  qm RQM !  dst DEST ! ;


: TAIL-CAPTURE-INSTALL ( -- ) [: TAIL-CAPTURE ;] is TAIL-CAPTURE-XT ;
TAIL-CAPTURE-INSTALL

\ Match/construct reasons (docs/forth.md "Structures And Enums"): MDIAG's latched
\ code maps to a code, suggestion and prose. MD-NONEXH names
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




\ Shared by a definition's depth refusal (MD-SUGGEST$) and a signature's
\ (SGBAD-SUGGEST$): a row is deep by its cells or by the types it nests.
: DEPTH-SUGGEST$ ( -- ptr u8 n )
   s" Keep bulk values in a buffer, not on the stack, and name an inner callback's effect in a word of its own." ;


\ A token of unknown effect (SUGGEST-TEXT).
: UNCK-SUGGEST$ ( -- ptr u8 n )
   s" Rewrite with modeled words or isolate an audited primitive." ;

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
         json MDV-F @ 0= and 0= IF s"  " EMIT-STR THEN
         vstart MDV-I @ + SUMV-NAME$ EMIT-STR
         -1 MDV-F !
      THEN
      MDV-I @ 1 + MDV-I !
   REPEAT ;


: MDIAG-MISSING-PROSE ( -- )
   0 0= 0= MDIAG-MISSING-WALK ;

: MDIAG-UF-COUNTS ( -- )
   s"  (needs " EMIT-STR  MDIAG-NEED @ EMIT-U
   s" , has " EMIT-STR  MDIAG-HAVE @ EMIT-U  s" )" EMIT-STR ;

\ The literal that does not fit and its type's range, after the reason's
\ `literal`: `300` does not fit `u8` (`0`..`255`).
: LIT-FIT-COUNTS ( -- )
   s"  `" EMIT-STR  MDIAG-LITV @ EMIT-N
   s" ` does not fit `" EMIT-STR  MDIAG-LITT @ CT-NAME$ EMIT-STR
   s" ` (`0`..`" EMIT-STR  MDIAG-LITT @ LIT-HI EMIT-U  s" `)" EMIT-STR ;

\ The wide value `depth` or `.s` met and its cells, or that they are not known
\ where its width is open, after the reason's `cannot count`.
: DEPTH-WIDE-COUNTS ( -- )
   AT-PIN
   s"  `" EMIT-STR  MDIAG-WIDE @ REND-TYPE
   MDIAG-OPEN @ IF
      s" `: it is one value, and how many cells it holds is not known here" EMIT-STR
   ELSE
      s" `: it is one value held in " EMIT-STR  MDIAG-WIDE @ T-WIDTH EMIT-U  s"  cells" EMIT-STR
   THEN
   ;AT-PIN ;


\ What a reason carries after its prose, in text and JSON alike.
: MDIAG-COUNTS ( -- )
   MDIAG @ MD-TAIL-ESCAPE = IF s" ; held row " EMIT-STR
      TCAP-HELD$ EMIT-STR EXIT THEN
   MDIAG @ MD-LIT-FIT = IF LIT-FIT-COUNTS EXIT THEN
   MDIAG @ MD-DEPTH-WIDE = IF DEPTH-WIDE-COUNTS EXIT THEN
   MDIAG @ MD-UNDERFLOW = IF MDIAG-UF-COUNTS EXIT THEN
   MDIAG @ MD-EFFECT-DEPTH = IF MDIAG-DEPTH @ DEPTH-COUNTS EXIT THEN
   MDIAG @ MD-CF-DEPTH = IF CF-FRAMES-MAX 1 + CF-FRAMES-MAX CAP-DEPTH-COUNTS EXIT THEN
   MDIAG @ MD-INPUT-WIDTH = IF MDIAG-WIDTH @ WIDTH-COUNTS THEN ;


: IMM-CODE$ ( -- ptr u8 n )
   s" E-UNMODELED-IMMEDIATE" ;


: IMM-SUGGEST$ ( -- ptr u8 n )
   s" Declare a stack-neutral parsing immediate with parse-imm, or remove it from the compiled body." ;

\ The LOCALBAD kinds come from checker.f LOC-REJECT and LOC-GROUP-REJECT.
: LOCALBAD-CODE$ ( -- ptr u8 n )
   LOCALBAD-KIND @ 2 = IF s" E-LOCAL-NAME-TOO-LONG" EXIT THEN
   LOCALBAD-KIND @ 1 = IF s" E-TOO-MANY-LOCALS" EXIT THEN
   s" E-BAD-LOCAL-SHAPE" ;


: LOCALBAD-SUGGEST$ ( -- ptr u8 n )
   LOCALBAD-KIND @ 2 = IF s" Shorten the local name to at most 16 bytes." EXIT THEN
   LOCALBAD-KIND @ 1 = IF s" Bind at most 64 locals in one definition, or factor a helper." EXIT THEN
   LOCALBAD-KIND @ 3 >= IF s" Remove the token: a locals group lists only the names to bind." EXIT THEN
   s" Move locals to a live top-level path or factor a helper." ;

\ Kinds 3 and 4, a locals group token that names no local, state why in the
\ message.
: LOCALBAD-GROUP? ( -- bool )
   LOCALBAD-KIND @ 3 >= ;

: LOCALBAD-REASON$ ( -- ptr u8 n )
   LOCALBAD-KIND @ 3 = IF s" a locals group names only locals; the signature declares the outputs" EXIT THEN
   s" a locals group names only locals; Habu has no uninitialised locals" ;

\ A definition's signature, a local's type or a stored signature that does not
\ parse; SGBAD's state from the failed parse names the kind.
: SGBAD-CODE$ ( -- ptr u8 n )
   SGBAD-UNKNOWN? IF s" E-UNKNOWN-SIGNATURE-TYPE" EXIT THEN
   SGBAD-BAREPTR? IF s" E-BARE-PTR-SIGNATURE" EXIT THEN
   SGBAD-ARITY? IF s" E-WRONG-ARITY" EXIT THEN
   s" E-BAD-SIGNATURE" ;

\ The code, message, label, notes and help all name the one rejection REFUSAL@
\ chose.
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
: NM-SPAN ( -- ptr u8 n )
   NMOFF @ TADDR NMU @ ;
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

\ DIAG-PRINT prints the rejection REFUSAL@ names through REJECTION's parts, the
\ same calls giving prose and JSON. Rows in its text are rendered as they stood
\ at the pin (AT-PIN), without DROW's trailing space.

\ The kind the message, label, notes and help follow: a mismatch with no
\ captured pair has no rows to name.
: DKIND ( -- n )
   REFUSAL@ {: k:n :}
   k RF-MISMATCH =  DEXP @ 0=  and IF RF-REJECTED EXIT THEN
   k ;

\ A pinned token where its source holds it: a top step's as the step read it,
\ a body's in the checked text. A closer past the text's end that no source
\ holds, as a checked string's, is FAILTK, the copy of its spelling.
: DIAG-TOKEN ( -- ptr u8 n )
   TOP-STEP @ IF TOK-SPELL-A @ TOK-SPELL-U @ EXIT THEN
   TBASE@ FAILB @ + {: p:ptr :}
   FAILE @ TBLEN @ >  p REJECTION:HELD? 0=  and IF FAILTK FAILTU @ EXIT THEN
   p FAILE @ FAILB @ - ;

: PIN-TOK$ ( -- ptr u8 n ) FAILTK FAILTU @ ;
: CODE-SPAN ( ptr u8 n -- ) 96 EMIT1 EMIT-STR 96 EMIT1 ;
: TOK-SPAN ( -- ) PIN-TOK$ CODE-SPAN ;
\ The pin is the closer of a body that walked no token.
: CLOSER-PINNED? ( -- bool )
   PIN-TOK$ s" ;" CORE-STR=  PIN-TOK$ s" does>" CORE-STR=  or ;

: TRIM-SPACE ( -- )
   TEXT-LEN @ 0= IF EXIT THEN
   TEXT TEXT-LEN @ 1 - + c@ 32 = IF TEXT-LEN @ 1 - TEXT-LEN ! THEN ;

\ Row s as DROW writes it, or the word e for an empty row.
: ROW-OR ( n ptr u8 n -- ) {: s:n ea:ptr eu:n :}
   TEXT-LEN @ {: at:n :}
   s DROW
   TEXT-LEN @ at = IF ea eu EMIT-STR EXIT THEN
   TRIM-SPACE ;
: ROW-WORD ( n -- ) s" nothing" ROW-OR ;
: ROW-NOTE ( n -- ) s" (nothing)" ROW-OR ;

\ A signature as written, without its parentheses.
: SIG-BARE ( ptr u8 n -- ptr u8 n )
   SIG-TRIM {: a:ptr u:n :}
   u 2 < IF a u EXIT THEN
   a c@ 40 <>  a u + 1 - c@ 41 <>  or IF a u EXIT THEN
   a 1 + u 2 - SIG-TRIM ;

: INFERRED ( -- ) INFERRED-EFFECT TRIM-SPACE ;

: END-MESSAGE ( -- )
   s" the body ends with " EMIT-STR  DACT @ ROW-WORD
   s"  on the stack; the signature promises " EMIT-STR  DEXP @ ROW-WORD ;
: MISMATCH-MESSAGE ( -- )
   DEND @ 0 <> IF END-MESSAGE EXIT THEN
   TOK-SPAN  s"  takes " EMIT-STR  DEXP @ ROW-WORD
   s" ; the stack holds " EMIT-STR  DACT @ ROW-WORD ;

: LOCAL-MESSAGE ( -- )
   LOCALBAD-GROUP? IF LOCALBAD-REASON$ EMIT-STR EXIT THEN
   LOCALBAD-KIND @ 2 = IF
      s" local " EMIT-STR  TOK-SPAN  s"  has a " EMIT-STR  LOCALBAD-LEN @ EMIT-U
      s" -byte name; the limit is " EMIT-STR  LOC-NAME-CAP EMIT-U
      s"  bytes" EMIT-STR EXIT
   THEN
   LOCALBAD-KIND @ 1 = IF
      s" local " EMIT-STR  TOK-SPAN  s"  is one over the " EMIT-STR  LOC-RECS EMIT-U
      s"  locals a definition may bind" EMIT-STR EXIT
   THEN
   s" a local cannot be bound inside a quotation, or bound on a dead path" EMIT-STR ;

: ARITY-MESSAGE ( -- )
   s" type family " EMIT-STR  TOK-SPAN
   s"  takes " EMIT-STR  SGBAD-AR-DECL @ EMIT-U
   s"  type arguments, not " EMIT-STR  SGBAD-AR-GOT @ EMIT-U ;
: SIG-MESSAGE ( -- )
   SGBAD-UNKNOWN? IF
      s" unknown type " EMIT-STR  TOK-SPAN  s"  in signature" EMIT-STR EXIT
   THEN
   SGBAD-BAREPTR? IF s" `ptr` needs an element type" EMIT-STR EXIT THEN
   SGBAD-ARITY? IF ARITY-MESSAGE EXIT THEN
   SGBAD-RETURN? IF s" return-stack effect in signature " EMIT-STR  TOK-SPAN EXIT THEN
   SGBAD-SIZE? IF BADSIG-REASON  s"  at " EMIT-STR  TOK-SPAN EXIT THEN
   s" bad signature at " EMIT-STR  TOK-SPAN ;

: NP-FAM-REND ( -- )
   NPBAD-TERM @ NP-FAM {: fam:n :}
   fam 0 >= IF s" family `" EMIT-STR  fam FAM-QNAME-REND  96 EMIT1
   ELSE s" a concrete type" EMIT-STR THEN ;
: NP-VAR ( n -- ) {: q:n :}
   s" declared type variable `" EMIT-STR  q EMIT1  96 EMIT1 ;
: NP-KEEP ( -- ) s" ; its declared kind must stay unchanged" EMIT-STR ;
: NP-ROWS ( -- )
   s" declared row variable `" EMIT-STR  NPBAD-Q1 @ EMIT1
   NPBAD-Q2 @ IF s" ` and row `" EMIT-STR  NPBAD-Q2 @ EMIT1  96 EMIT1
   ELSE s" ` and the unnamed row" EMIT-STR THEN
   s"  are unified; each declared row must stay a distinct row variable" EMIT-STR ;
: NP-ROW-FIXED ( -- )
   NPBAD-Q1 @ IF s" declared row variable `" EMIT-STR  NPBAD-Q1 @ EMIT1  96 EMIT1
   ELSE s" the unnamed row" EMIT-STR THEN
   s"  is bound to fixed cells; a declared effect must stay polymorphic over its row" EMIT-STR ;
: NP-MESSAGE ( -- )
   NPBAD-KIND @ case
      4 of NPBAD-Q1 @ NP-VAR
         s"  is restricted to a base address -- the DATA region, or a pointer computed off NULL-PTR" EMIT-STR
         NP-KEEP endof
      5 of NPBAD-Q1 @ NP-VAR  s"  is restricted to the null address" EMIT-STR  NP-KEEP endof
      3 of NPBAD-Q1 @ NP-VAR  s"  is restricted by raw storage" EMIT-STR  NP-KEEP endof
      0 of NPBAD-Q1 @ NP-VAR  s"  is specialized to " EMIT-STR  NP-FAM-REND
         s" ; a declared effect must stay parametric over its quantifier" EMIT-STR endof
      1 of s" declared type variables `" EMIT-STR  NPBAD-Q1 @ EMIT1
         s" ` and `" EMIT-STR  NPBAD-Q2 @ EMIT1
         s" ` are unified; each declared quantifier must stay a distinct variable" EMIT-STR endof
      6 of NP-ROWS endof
      7 of NP-ROW-FIXED endof
      NPBAD-Q1 @ NP-VAR  s"  is minted into " EMIT-STR  NP-FAM-REND
      s"  but is unbound in the inputs; a checked definition cannot mint a phantom of input-unrelated type" EMIT-STR
   endcase ;

: REASON-MESSAGE ( -- )
   MDIAG @ MD-REASON$ EMIT-STR
   MDIAG @ MD-NONEXH = IF MDIAG-MISSING-PROSE THEN
   MDIAG-COUNTS ;

: DEAD-MESSAGE ( -- )
   TOK-SPAN  s"  never runs: " EMIT-STR  DEADTA @ DEADTU @ CODE-SPAN
   s"  before it never returns" EMIT-STR ;

: TOK-MESSAGE ( ptr u8 n -- ) {: a:ptr u:n :}
   TOK-SPAN  a u EMIT-STR ;

: DIAG-MESSAGE ( -- )
   DKIND case
      RF-MISMATCH of AT-PIN MISMATCH-MESSAGE ;AT-PIN endof
      RF-UNDEF of s" cannot find word " EMIT-STR  TOK-SPAN endof
      RF-QUAL of s" malformed qualified name " EMIT-STR  TOK-SPAN endof
      RF-IMM of s"  is a compile-time immediate that is not a stack-neutral parsing immediate" TOK-MESSAGE endof
      RF-CAP of s"  is a trust-boundary primitive" TOK-MESSAGE endof
      RF-UNSAFE of s"  reads source or changes the compiler's or checker's state, which no stack effect describes" TOK-MESSAGE endof
      RF-LINLOCAL of s" a linear value cannot be bound to a local" EMIT-STR endof
      RF-LOCAL of LOCAL-MESSAGE endof
      RF-SIG of SIG-MESSAGE endof
      RF-NP of NP-MESSAGE endof
      RF-DEAD of DEAD-MESSAGE endof
      RF-RETURN of RS-REASON$ EMIT-STR endof
      RF-REASON of REASON-MESSAGE endof
      RF-UNCHECKABLE of s"  cannot be checked here" TOK-MESSAGE endof
      s" rejected at " EMIT-STR  TOK-SPAN
   endcase ;

: END-LABEL ( -- )
   CLOSER-PINNED? IF s" the body ends here" EMIT-STR EXIT THEN
   DACT @ ROW-VALUES 0= IF s" the stack is empty after this" EMIT-STR EXIT THEN
   s" the stack holds " EMIT-STR  DACT @ ROW-WORD  s"  after this" EMIT-STR ;
: MISMATCH-LABEL ( -- )
   DEND @ 0 <> IF END-LABEL EXIT THEN
   s" takes " EMIT-STR  DEXP @ ROW-WORD ;

: LOCAL-LABEL ( -- )
   LOCALBAD-KIND @ 2 = IF s" name too long" EMIT-STR EXIT THEN
   LOCALBAD-KIND @ 1 = IF s" one local too many" EMIT-STR EXIT THEN
   LOCALBAD-GROUP? IF s" not a local name" EMIT-STR EXIT THEN
   s" bound here" EMIT-STR ;

: SIG-LABEL ( -- )
   SGBAD-UNKNOWN? IF s" unknown type" EMIT-STR EXIT THEN
   SGBAD-BAREPTR? IF s" no element type" EMIT-STR EXIT THEN
   SGBAD-ARITY? IF s" wrong number of type arguments" EMIT-STR EXIT THEN
   SGBAD-RETURN? IF s" return-stack effect" EMIT-STR EXIT THEN
   s" here" EMIT-STR ;

: DIAG-LABEL ( -- )
   DKIND case
      RF-MISMATCH of AT-PIN MISMATCH-LABEL ;AT-PIN endof
      RF-UNDEF of s" not found" EMIT-STR endof
      RF-QUAL of s" more than one `:`" EMIT-STR endof
      RF-IMM of s" unmodeled immediate" EMIT-STR endof
      RF-CAP of s" not callable from a checked body" EMIT-STR endof
      RF-UNSAFE of s" not allowed in a checked body" EMIT-STR endof
      RF-LINLOCAL of s" bound here" EMIT-STR endof
      RF-LOCAL of LOCAL-LABEL endof
      RF-SIG of SIG-LABEL endof
      RF-DEAD of s" never runs" EMIT-STR endof
      RF-UNCHECKABLE of s" checking stops here" EMIT-STR endof
      s" here" EMIT-STR
   endcase ;

: TEXT-NOTE ( ptr u8 n ptr u8 n -- ) {: ka:ptr ku:n va:ptr vu:n :}
   ka ku REJECTION:NOTE  va vu SIG-RTRIM EMIT-STR ;
: PAIR-NOTES ( n n -- ) {: e:n a:n :}
   s" expected" REJECTION:NOTE  e ROW-NOTE
   s" found" REJECTION:NOTE  a ROW-NOTE ;

\ Expected takes precedence over actual, so unrelated matched row cells cannot
\ leak into the family; a scalar pair names none.
: TERM-FAM ( n -- n )                    \ layout-family id for one type term, else -1
   T-RES dup LAYOUT-PARAM? IF PARAM>FAM EXIT THEN
   drop -1 ;
\ DVAR is the SUMV id a `construct family variant` payload mismatch latched, -1
\ for none.
: DIAG-FAM-ID ( n n -- n )
   {: efam:n afam:n :}
   efam 0 >= IF efam EXIT THEN
   afam 0 >= IF afam EXIT THEN
   DVAR @ dup 0 >= IF SUMV-FAM@ THEN ;
\ DPOS is the slot from the top of the failed expected element; the field is
\ the variant's payload field in declaration order, none outside its cells.
: FIELD-NOTE ( -- )
   DPOS @ 0 < IF EXIT THEN
   DVAR @ SUMV-PAY-N {: cnt:n :}
   DPOS @ cnt < 0= IF EXIT THEN
   DVAR @  cnt 1 - DPOS @ -  SUMV-PAY-FIELD IF
      s" field" REJECTION:NOTE  PF-NAME$ EMIT-STR
   ELSE drop THEN ;
: FAMILY-NOTES ( -- )
   DF-EXP @ TERM-FAM  DF-ACT @ TERM-FAM  DIAG-FAM-ID {: fam:n :}
   fam 0 >= IF s" family" REJECTION:NOTE  fam FAM-QNAME-REND THEN
   DVAR @ 0 < IF EXIT THEN
   s" variant" REJECTION:NOTE  DVAR @ SUMV-NAME$ EMIT-STR
   FIELD-NOTE ;

: ESCAPE-NOTES ( -- )
   s" expected" TCAP-EXP$ TEXT-NOTE
   s" found" TCAP-ACT$ TEXT-NOTE
   s" held" TCAP-HELD$ TEXT-NOTE ;
: ROWS-NOTES ( -- )
   MDIAG @ MD-TAIL-ESCAPE = IF ESCAPE-NOTES EXIT THEN
   AT-PIN
   DEXP @ 0 <> IF DEXP @ DACT @ PAIR-NOTES  FAMILY-NOTES THEN
   DF-ACT @ 0 <>  DEXP @ 0= and IF
      s" found" REJECTION:NOTE  DF-ACT @ REND-TYPE
   THEN
   ;AT-PIN ;

: EFFECT-NOTES? ( n -- bool ) {: k:n :}
   RECEFF-ON? TOP-STEP @ 0= and 0= IF RES-FALSE EXIT THEN
   k RF-MISMATCH =  k RF-RETURN = or  k RF-NP = or
   k RF-REASON =  DEXP @ 0 <> and  or ;
: EFFECT-NOTES ( -- )
   s" declared" REJECTION:NOTE  SGA @ SGU @ SIG-BARE EMIT-STR
   s" inferred" REJECTION:NOTE
   MDIAG @ MD-TAIL-ESCAPE = IF TCAP-INF$ SIG-RTRIM EMIT-STR EXIT THEN
   INFERRED ;

: DIAG-NOTES ( -- )
   DKIND {: k:n :}
   k ROWS-REFUSAL? IF ROWS-NOTES THEN
   k RF-RETURN = IF RS-ROWS PAIR-NOTES THEN
   k RF-REASON =  MDIAG @ MD-NONEXH = and IF
      s" missing" REJECTION:NOTE  0 0= MDIAG-MISSING-WALK
   THEN
   k EFFECT-NOTES? IF EFFECT-NOTES THEN ;

\ The help for a body whose end does not fit its signature, by its depths.
: END-WHERE ( -- )
   CLOSER-PINNED? IF s"  in the body" EMIT-STR EXIT THEN
   s"  after " EMIT-STR  TOK-SPAN ;
: END-HELP ( -- )
   ['] ROW-VALUES ROWS-DEPTHS
   DSUGA @ DSUGE @ = IF s" make the body end with " EMIT-STR ELSE
      DSUGA @ DSUGE @ < IF s" add the missing values" ELSE s" drop the extra values" THEN
      EMIT-STR  END-WHERE  s"  so the body ends with " EMIT-STR
   THEN
   AT-PIN DEXP @ ROW-WORD ;AT-PIN
   s" , or declare ( " EMIT-STR  INFERRED  s"  )" EMIT-STR ;
: TAKES-HELP ( -- )
   ['] ROW-VALUES ROWS-DEPTHS
   DSUGA @ DSUGE @ < IF
      s" put what " EMIT-STR  TOK-SPAN
      s"  takes on the stack before it, or declare it as an input" EMIT-STR EXIT
   THEN
   s" give " EMIT-STR  TOK-SPAN  32 EMIT1  AT-PIN DEXP @ ROW-WORD ;AT-PIN
   s" : convert or replace the value before it" EMIT-STR ;

\ The package record at dictionary index i whose public wordlist holds the
\ token t tu, else NULL-PTR. A part, named A:B, is left out: no qualifier
\ outside A reaches it.
: PUBLIC-IN ( ptr u8 n n -- ptr n ) {: t:ptr tu:n i:n :}
   i CHECKER-RESOLVE:REC-AT {: rec:ptr :}
   rec CK-REC-WID BOOT:NAMESPACE-WL <> IF NULL-PTR EXIT THEN
   rec @ {: wid:n :}
   rec CHECKER-RESOLVE:CK-REC-NAME$ CHECKER-COLON-SCAN drop {: colons:n :}
   wid 0=  colons 0 <>  or IF NULL-PTR EXIT THEN
   t tu wid CHECKER-RESOLVE:WL-PROBE NULL-PTR = IF NULL-PTR EXIT THEN
   rec ;
variable HOLDER-N
PTR-VARIABLE HOLDER-FIRST
: HOLDER-NAME ( ptr n -- ) CHECKER-RESOLVE:CK-REC-NAME$ CODE-SPAN ;
\ Counts the packages whose public wordlist holds t tu, writes their names when
\ list is set.
: HOLDER-SCAN ( ptr u8 n bool -- ) {: t:ptr tu:n list:bool :}
   0 HOLDER-N !
   ndict@ 0 ?do
      t tu i PUBLIC-IN {: rec:ptr :}
      rec NULL-PTR <> IF
         HOLDER-N @ 0= IF rec HOLDER-FIRST ! THEN
         list IF HOLDER-N @ 0 > IF s" , " EMIT-STR THEN  rec HOLDER-NAME THEN
         HOLDER-N @ 1 + HOLDER-N !
      THEN
   loop ;
: QUALIFIED ( ptr u8 n -- ) {: t:ptr tu:n :}
   96 EMIT1  HOLDER-FIRST @ CHECKER-RESOLVE:CK-REC-NAME$ EMIT-STR
   58 EMIT1  t tu EMIT-STR  96 EMIT1 ;
: UNDEF-HELP ( ptr u8 n -- ) {: t:ptr tu:n :}
   t tu RES-FALSE HOLDER-SCAN
   HOLDER-N @ 0= IF
      s" define " EMIT-STR  t tu CODE-SPAN
      s"  before this point, or check its spelling" EMIT-STR EXIT
   THEN
   t tu CODE-SPAN  s"  is public in " EMIT-STR
   HOLDER-N @ 1 = IF
      HOLDER-FIRST @ HOLDER-NAME  s" : write " EMIT-STR  t tu QUALIFIED
      s" , or call it inside `using " EMIT-STR
      HOLDER-FIRST @ CHECKER-RESOLVE:CK-REC-NAME$ EMIT-STR  s" ` … `;using`" EMIT-STR EXIT
   THEN
   t tu RES-TRUE HOLDER-SCAN  s" : write it qualified, as " EMIT-STR  t tu QUALIFIED ;

: DIAG-HELP ( -- )
   DKIND {: k:n :}
   k RF-MISMATCH = IF DEND @ 0 <> IF END-HELP ELSE TAKES-HELP THEN EXIT THEN
   k RF-UNDEF = IF PIN-TOK$ UNDEF-HELP EXIT THEN
   SUGGEST-TEXT EMIT-STR ;

: DIAG-PRINT
   0 RQM !  SEEN-RESET 0 NLET !  0 RROW-N !
   DCODE REJECTION:OPEN  DIAG-MESSAGE
   DIAG-TOKEN REJECTION:AT  DIAG-LABEL
   DIAG-NOTES
   REJECTION:HELP  DIAG-HELP
   REJECTION:KEEP ;



: TDECL-TITLE ( ptr u8 n ptr u8 n -- ) {: ka:ptr ku:n fa:ptr fu:n :}
   s" E-BAD-DECLARATION" REJECTION:OPEN
   s" bad " EMIT-STR  ka ku EMIT-STR  s"  declaration" EMIT-STR
   fu 0 > IF s"  `" EMIT-STR  fa fu EMIT-STR  96 EMIT1 THEN ;
\ A rejected declaration of a kind and family: its token labeled with the
\ reason, the fix as help. The caller ends the record.
: TDECL-DIAG ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: ka:ptr ku:n fa:ptr fu:n ta:ptr tu:n wa:ptr wu:n xa:ptr xu:n :}
   ka ku fa fu TDECL-TITLE
   ta tu REJECTION:AT  wa wu EMIT-STR
   REJECTION:HELP  xa xu EMIT-STR ;

\ Why REC-SIG left a certified sig-less word unrecorded; the last arm answers
\ every other RQM cause, a stale or scoped type among them, as an unmodeled tag.
: REC-REFUSE-WHY ( -- )
   NLET @ TYPE-VAR-MAX > IF
      s" more than " EMIT-STR  TYPE-VAR-MAX EMIT-U
      s"  type variables or binders in inferred effect" EMIT-STR EXIT
   THEN
   RBIND-FULL @ IF BINDERS-REASON s"  in inferred effect" EMIT-STR EXIT THEN
   RROW-UNSAID @ IF
      s" quotation return-stack clause or more than " EMIT-STR  RROW-CAP EMIT-U
      s"  named rows the effect text cannot say" EMIT-STR EXIT
   THEN
   s" unmodeled type tag in inferred effect" EMIT-STR ;

\ The fix for REC-REFUSE-WHY's reason, for the word na nu.
: REC-REFUSE-HELP ( ptr u8 n -- ) {: na:ptr nu:n :}
   NLET @ TYPE-VAR-MAX > IF
      s" split `" EMIT-STR  na nu EMIT-STR
      s" ` so each word's effect names at most " EMIT-STR  TYPE-VAR-MAX EMIT-U
      s"  type variables" EMIT-STR EXIT
   THEN
   RBIND-FULL @ IF
      s" name an inner callback's effect in a word of its own, so at most "
      EMIT-STR  BINDER-DEPTH-MAX EMIT-U  s"  binders nest" EMIT-STR EXIT
   THEN
   RROW-UNSAID @ IF
      s" balance `>r` and `r>` inside each quotation `" EMIT-STR  na nu EMIT-STR
      s" ` returns, and split it if its effect names more than " EMIT-STR
      RROW-CAP EMIT-U  s"  rows" EMIT-STR EXIT
   THEN
   s" declare the stack effect after the name, as `: " EMIT-STR  na nu EMIT-STR
   s"  ( inputs -- outputs )`, so a call is checked against it" EMIT-STR ;

\ Check's run turns this off, as the check before it wrote the placed warnings;
\ a session the run's program starts turns it on (CHECK-ALL-ERRORS:SESSION).
variable WARN-DIAGS   -1 WARN-DIAGS !

\ W-EFFECT-NOT-RECORDED warns that the word na nu certified and loaded, but a
\ later call rejects it as undefined; the run's status stands.
: REC-REFUSE-DIAG ( ptr u8 n -- ) {: na:ptr nu:n :}
   WARN-DIAGS @ 0= IF EXIT THEN
   s" W-EFFECT-NOT-RECORDED" REJECTION:WARN
   s" the effect of `" EMIT-STR  na nu EMIT-STR
   s" ` cannot be recorded, so a later call of it is rejected as undefined"
   EMIT-STR
   NM-SPAN REJECTION:AT  s" effect not recorded" EMIT-STR
   s" reason" REJECTION:NOTE  REC-REFUSE-WHY
   REJECTION:HELP  na nu REC-REFUSE-HELP
   REJECTION:KEEP ;

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

\ The text of a rejected `cast:` or `linear:` declaration.
package CAST-ERROR
: CODE$ ( n -- ptr u8 n ) {: code:n :}
   code E-CAST-ARITY = IF s" E-CAST-ARITY" EXIT THEN
   code E-CAST-CLASS = IF s" E-CAST-CLASS" EXIT THEN
   code E-CAST-FAM = IF s" E-CAST-FAM" EXIT THEN
   code E-CAST-LINEAR = IF s" E-CAST-LINEAR" EXIT THEN
   code E-CAST-MINT = IF s" E-CAST-MINT" EXIT THEN
   code E-CAST-SCOPE = IF s" E-CAST-SCOPE" EXIT THEN
   code E-LINEAR-PAYLOAD = IF s" E-LINEAR-PAYLOAD" EXIT THEN
   code E-LINEAR-OWNER = IF s" E-LINEAR-OWNER" EXIT THEN
   code E-LINEAR-SCOPE = IF s" E-LINEAR-SCOPE" EXIT THEN
   s" checker: a cast rejection with no code name" 76 die ;
\ E-CAST-FAM's label is the parse's: only PARSE raises it, with SGBAD set.
: LABEL$ ( n -- ptr u8 n ) {: code:n :}
   code E-CAST-ARITY = IF s" not one term on each side" EXIT THEN
   code E-CAST-CLASS = IF s" a retype no cast may make" EXIT THEN
   code E-CAST-FAM = IF SGBAD-LABEL$ EXIT THEN
   code E-CAST-LINEAR = IF s" a linear value would change hands" EXIT THEN
   code E-CAST-MINT = IF s" mints outside a private section" EXIT THEN
   code E-CAST-SCOPE = IF s" not a view cast this place may declare" EXIT THEN
   code E-LINEAR-PAYLOAD = IF s" not one token and one payload" EXIT THEN
   code E-LINEAR-OWNER = IF s" declared outside the type's package" EXIT THEN
   s" outside its owner's private section" ;
: WORD$ ( -- ptr u8 n ) WORD-A @ WORD-U @ ;
: SIG$ ( -- ptr u8 n ) SIG-A @ SIG-U @ ;
\ The message: the declaration in backticks, why wa wu, then the token TOKEN!
\ named in backticks.
: TITLE ( ptr u8 n -- ) {: wa:ptr wu:n :}
   s" in `" EMIT-STR  KEY-A @ KEY-U @ EMIT-STR  32 EMIT1  WORD$ EMIT-STR
   s" `, " EMIT-STR  wa wu EMIT-STR
   TOK-U @ 0 > IF
      s"  `" EMIT-STR  TOK-A @ TOK-U @ EMIT-STR  96 EMIT1
   THEN ;
\ The token the record places: TOKEN!'s when the signature holds it (a package
\ name it named is no source), else the declared name.
: SPOT$ ( -- ptr u8 n )
   TOK-A @ SIG-A @ - {: d:n :}
   TOK-U @ 0 >  d 0 >= and  d SIG-U @ < and IF TOK-A @ TOK-U @ EXIT THEN
   WORD$ ;
;package

;using

\ `+` and `cell+` refuse a layout pointee; bounds stay the caller's.
: record-at ( ptr a n -- ptr a ) cells + ;  INTRINSIC-RECORD-AT INTRINSIC
