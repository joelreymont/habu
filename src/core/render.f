split-str
\ render.f: renders checker types and rows as text: a residual stack (DCUR), a
\ rejection's diagnostic. REC-SIG records a certified
\ sig-less word's inferred effect under its name, so its callers certify;
\ rendering only decides whether that effect is declarable
\ (RQM). Compiles after checker.f and before any src/lib/ file: no emit or
\ +!; bytes go out through REJECTION.
using REJECTION
package CHECK
EXPORT TFAM:SUMV-FAM@
EXPORT TFAM:SUMV-NAME$
EXPORT TFAM:TFAM-ACTIVE-PKG$
EXPORT TFAM:TFAM-N@
EXPORT TFAM:TFAM-NAME$
EXPORT TFAM:TFAM-PKG$
EXPORT TFAM:TFAM-VAR-START@

\ RQM: the text cannot say the effect, so REC-SIG does not record it; set where the
\ text writes `?` and where it meets a type or row no signature spells.
variable RQM
\ RORDINAL: the text names a type variable by its ordinal, which no source
\ signature spells.
variable RORDINAL
\ RROW-UNSAID: the unsaid part is a row, for REC-REFUSE-WHY.
variable RROW-UNSAID
\ RBIND-FULL: a forall past BINDER-DEPTH-MAX nested binders, for REC-REFUSE-WHY.
variable RBIND-FULL

\ RTAILS is set while TAIL-CAPTURE, CYCLE-NOTES or NARROWED-NOTES renders: every
\ quotation then shows its row tails.
variable RTAILS   0 RTAILS !
\ RNAMES is 1 while the first pass of REND-EFFECT or INFERRED-FROM records each
\ row's tail (RPOS) and 2 while its second pass names the rows that share one;
\ 0 otherwise. RFACTS is set while REND-EFFECT writes facts text.
variable RNAMES   0 RNAMES !
variable RFACTS   0 RFACTS !
: RFACTS? ( -- bool ) RFACTS @ 0 <> ;


: EMIT-UNSAID ( -- ) 1 RQM !  63 EMIT1 ;
PERSISTED-PTR-VARIABLE SEEN-P   NULL-PTR SEEN-P !
variable SEEN-CAP   0 SEEN-CAP !
\ Only the assigned prefix needs clearing; allocation initializes every cell
\ above SEEN-HW to UNBOUND, including a grown tail.
variable SEEN-HW   0 SEEN-HW !
variable NLET                                      \ one past the highest name index given
variable RBIND-N                                   \ lexical names while rendering forall
variable RLETS                                     \ the letters given, bit i for name index i
variable RNAMED                                    \ how many names were given
variable RRESV                                     \ the letters REND-EFFECT's first pass met stored
variable RFRESH                                    \ nonzero while PE-INPUTS writes a row
64 constant RATOM-CAP
create RATOM-KEY RATOM-CAP cells allot
variable RATOM-N
variable RATOM-I
$7FFFFFFFFFFFFFFF CELL / constant SEEN-MAX-CAP

: SEEN-UNMAP-RC ( ^a n -- n ) {: base:^ cap:n :}
   cap 0= IF 0 EXIT THEN
   base cap cells munmap ;

: SEEN-GROW-CAP ( n -- n ) {: need:n :}
   need MAXTV-INIT max
   SEEN-CAP @ SEEN-MAX-CAP 2 / <= IF SEEN-CAP @ 2 * max THEN ;

: SEEN-GROW ( n -- ) {: need:n :}
   need SEEN-GROW-CAP {: nc:n :}
   SEEN-CAP @ {: oc:n :}
   nc cells ARENA-ALLOC {: next:^ :}
   oc 0 > IF SEEN-P @ next oc ARENA-CELLS-COPY THEN
   next oc nc ARENA-CELLS-UNBOUND
   SEEN-P @ oc SEEN-UNMAP-RC 0 <> IF
      next nc SEEN-UNMAP-RC 0 <> IF
         s" render: seen cleanup munmap failed" EXIT-STATUS:SYSTEM die
      THEN
      s" render: seen munmap failed" EXIT-STATUS:SYSTEM die
   THEN
   next SEEN-P !   nc SEEN-CAP ! ;

: SEEN-ENSURE ( -- )
   MAXTV MAXTV-INIT max {: need:n :}
   need SEEN-CAP @ <= IF EXIT THEN
   need SEEN-GROW ;

: SEEN ( -- ^n ) SEEN-ENSURE SEEN-P @ ;

: SEEN-RESET ( -- )
   SEEN-ENSURE
   0 BEGIN dup SEEN-HW @ < WHILE
      UNBOUND over cells SEEN + !
      1 +
   REPEAT drop
   0 SEEN-HW ! 0 RBIND-N !  0 RLETS !  0 RNAMED ! ;


: RATOM-RESET ( -- )
   0 RATOM-N ! ;

: NAME-TAKE ( n -- n ) {: idx:n :}
   idx VAR-LETTERS < IF RLETS @ 1 idx lshift or RLETS ! THEN
   idx 1 + NLET @ max NLET !
   RNAMED @ 1 + RNAMED !
   idx ;

\ The first name index not given; in REND-EFFECT's second pass, no letter a
\ variable of the effect stores either.
: NAME-FREE ( -- n )
   RLETS @  RNAMES @ 2 = IF RRESV @ or THEN {: used:n :}
   VAR-LETTERS 0 ?do used 1 i lshift and 0= IF i NAME-TAKE unloop EXIT THEN loop
   NLET @ VAR-LETTERS max NAME-TAKE ;

\ A variable is named by the letter it stores unless a name took it; that one,
\ an unlettered one and one in a row PE-INPUTS writes take NAME-FREE's.
: VAR-NAME-NEW ( n -- n ) {: vp:n :}
   vp TVS@ {: c:n :}
   c 0=  RFRESH @ 0 <>  or IF NAME-FREE EXIT THEN
   c LETTER>IDX {: own:n :}
   RNAMES @ 1 = IF RRESV @ 1 own lshift or RRESV ! THEN
   RLETS @ 1 own lshift and 0= IF own NAME-TAKE EXIT THEN
   NAME-FREE ;

: SEEN-IDX ( n -- n )
   {: vp:n :}
   SEEN-ENSURE
   vp 1 + SEEN-HW @ < 0= IF vp 1 + SEEN-HW ! THEN
   vp cells SEEN + @ UNBOUND = IF vp VAR-NAME-NEW vp cells SEEN + ! THEN
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

: RSTR ( str -- ) {: a:^ u:n :}
   0 BEGIN dup u < WHILE dup a + c@ EMIT1 1 + REPEAT drop ;

\ RFOLD emits the case-folded spelling. Diagnostics name identifiers folded; only
\ a declaring-package name reaches the renderer unfolded, because TFAM-DECL
\ interns the spelling the source `package` line typed.
: RFOLD ( str -- ) {: a:^ u:n :}
   0 BEGIN dup u < WHILE dup a + c@ CORE-FOLD-C EMIT1 1 + REPEAT drop ;

: CON-OUT ( n -- ) {: p:n :}
   p 2 = IF 102 EMIT1 ELSE
   p 0 > p CTN @ < and IF
      p CT-NAME$ dup 0 <> IF RSTR ELSE 2drop EMIT-UNSAID THEN
   ELSE EMIT-UNSAID THEN THEN ;

\ Facts text (RFACTS?) writes a plain atom but `xt` as `'NAME`, which TOK-TYPE
\ reads back; a fresh atom's ordinal tells two identities of one name apart.
: ATOM-REND {: t :}
   t ATOM>K 0 = IF
      RFACTS? t OPAQUE-XT? 0= and IF 39 EMIT1 THEN
      t ATOM>A t ATOM>U RSTR EXIT THEN
   s" fresh-" RSTR
   t ATOM>A t ATOM>U RSTR
   45 EMIT1
   t ATOM>K RATOM-ORD RATOM-CHAR EMIT1 ;

: RNUM ( n -- )                 \ small non-negative number (hidden slot index)
   dup 10 >= IF dup 10 / RECURSE THEN
   10 mod 48 + EMIT1 ;


\ Name index idx is a letter, then the ordinal; the signature parser reads both back.
: VAR-NAME ( n -- )
   {: idx:n :}
   idx VAR-LETTERS < 0= IF 1 RORDINAL !  idx RNUM EXIT THEN
   idx IDX>LETTER EMIT1 ;


\ A term minted before the registry loads holds an id outside it. A slot a
\ partial rollback reused names its new occupant; diagnostics render before it.
: FAM-INTERNED? ( n -- f )
   {: fam:n :}
   fam 0 >=  fam TFAM-N@ <  and ;


\ Exact: both sides read a package's one interned spelling. Facts text (RFACTS?)
\ names every package but "" ("@" included), so it reads back with no scope.
: FAM-FOREIGN? ( n -- f )
   {: fam:n :}
   fam TFAM-PKG$ {: pa:^ pu:n :}
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
create RBIND BINDER-DEPTH-MAX cells allot    \ the name index of each binder in scope
\ A row renders from RBUF without recursion: a nested quotation's row pushes
\ above the rows that hold it, so QREND recurses once per level, never per value.
create RBUF EFFECT-DEPTH-MAX cells allot   variable RBN

\ Facts text says every level a record holds.
: QDEPTH-CAP ( -- n ) RFACTS? IF EFFECT-DEPTH-MAX ELSE QDEPTH-MAX THEN ;


variable RLOST   \ how many bottom values of the row pushed last RBUF could not hold
variable RBUF-TAIL   \ the tail of the row pushed last
\ A push past RBUF's end sets RQM: an effect's text that cannot hold every value
\ is rejected as unsayable, never cut.
: RBUF+ ( n -- )
   RBN @ EFFECT-DEPTH-MAX >= IF drop 1 RQM !  RLOST @ 1 + RLOST ! EXIT THEN
   RBN @ cells RBUF + !  RBN @ 1 + RBN ! ;


\ Push all of row s's values on RBUF from RBN, its bottom value last.
: RBUF-ROW ( n -- )
   {: s:n :}
   0 RLOST !
   s
   BEGIN R-RES dup TAG S-PUSH = WHILE
     dup P>TYPE T-RES HIDDEN-PARAM? IF
        dup HID-RUN-REST IF
           swap P>TYPE T-RES MK-LOGICAL  HRS @ IF MK-STALE THEN  RBUF+
        ELSE
           swap P>TYPE RBUF+
        THEN
     ELSE
        dup P>TYPE RBUF+  P>REST
     THEN
   REPEAT
   dup RBUF-TAIL !
   \ a stale tail prints as its bottom value, `stale<A>`
   dup STALE-ROW? IF RBUF+ ELSE drop THEN ;

\ A diagnostic's row RBUF cut prints "(N more) " first, where its bottom
\ values go; call it before the values print: a quotation among them resets RLOST.
: CUT-MARK ( -- )
   RLOST @ 0=  RFACTS?  or IF EXIT THEN
   40 EMIT1  RLOST @ RNUM  s"  more) " RSTR ;


: QRET? ( n -- bool ) {: q:n :}  q Q>RIN R-RES  q Q>ROUT R-RES  <> ;

\ Rows letter in their own namespace, A to Z, then '?'. REND-EFFECT, INFERRED-FROM
\ (RNAMES), RTAILS renderings, `stale<R>` and a diagnostic's pair holding a stale
\ tail name rows; every other rendering shows cells only.
26 constant RROW-CAP
create RROW-KEY RROW-CAP cells allot   variable RROW-N   0 RROW-N !
create RROW-LTR RROW-CAP cells allot   \ the letter each RROW-KEY row prints by

\ A diagnostic names its declared rows by the letters ROWMAP holds.
: RROW-MAP? ( -- bool ) RFACTS? 0= ;

\ Whether ROWMAP slot i's row is row id, or bound to it alone.
: RROW-IS? ( n n -- bool ) {: i:n id:n :}
   i cells ROWMAP + @ {: v:n :}
   v UNBOUND = IF RES-FALSE EXIT THEN
   v MK-ROW R-RES {: r:n :}
   r ISROW IF r PAY id = EXIT THEN
   RES-FALSE ;

: RROW-DECLARED ( n -- n )   \ row id's declared letter, 0 for none
   {: id:n :}
   RROW-MAP? 0= IF 0 EXIT THEN
   26 0 ?do i id RROW-IS? IF i 65 + unloop EXIT THEN loop 0 ;

: RROW-FREE? ( n -- bool )   \ letter c names no declared or lettered row
   {: c:n :}
   RROW-MAP? IF c 65 - cells ROWMAP + @ UNBOUND <> IF RES-FALSE EXIT THEN THEN
   RROW-N @ 0 ?do i cells RROW-LTR + @ c = IF RES-FALSE unloop EXIT THEN loop
   RES-TRUE ;

: RROW-NEXT ( -- n ) 91 65 ?do i RROW-FREE? IF i unloop EXIT THEN loop 63 ;


\ A stale row wraps a var a later unify may bind to a row ending in another
\ stale row; the occurs check looks through stale rows, so the chain ends.
: STALE-TAIL ( n -- n )
   ROW-TAIL BEGIN dup STALE-ROW? WHILE STALE>INNER ROW-TAIL REPEAT ;


: RROW-LETTER ( n -- n )
   STALE-TAIL PAY {: id:n :}
   id RROW-DECLARED dup 0 <> IF EXIT THEN drop
   0 BEGIN dup RROW-N @ < WHILE
      dup cells RROW-KEY + @ id = IF cells RROW-LTR + @ EXIT THEN
      1 +
   REPEAT drop
   RROW-NEXT {: c:n :}
   c 63 = IF 1 RROW-UNSAID !  1 RQM !  63 EXIT THEN
   id RROW-N @ cells RROW-KEY + !  c RROW-N @ cells RROW-LTR + !
   RROW-N @ 1 + RROW-N !  c ;


: QTAIL ( n -- )
   {: row:n :}
   row ROW-TAIL RROW-LETTER EMIT1  32 EMIT1 ;


\ RPOS holds a (tail, owner) pair per row a first pass (RNAMES 1) visits: owner
\ 0 is the effect's data rows, 1 its return rows, 2v and 2v+1 the v-th quotation's.
PERSISTED-PTR-VARIABLE RPOS-P   NULL-PTR RPOS-P !
variable RPOS-CAP   0 RPOS-CAP !          \ pairs mapped at RPOS-P, 0 for none
variable RPOS-N   variable RVISIT
: RPOS ( -- ^n ) RPOS-P @ ;

: RPOS+ ( n n -- )
   {: t:n owner:n :}
   RPOS-P RPOS-CAP RPOS-N @ 1 + 2 ARENA-ROWS-ENSURE
   t RPOS-N @ 2 * cells RPOS + !
   owner RPOS-N @ 2 * 1 + cells RPOS + !
   RPOS-N @ 1 + RPOS-N ! ;


\ RQUOT-BLOCK holds, per quotation RVISIT numbered (from 1, as its `[` is
\ written), its resolved block part, for REND-FACTS's block and noblock tags.
SCRATCH{
PTR-VARIABLE RQUOT-BLOCK-P   variable RQUOT-BLOCK-CAP
}SCRATCH

: RQUOT+ ( n -- ) {: q:n :}
   RQUOT-BLOCK-P RQUOT-BLOCK-CAP RVISIT @ 1 ARENA-ROWS-ENSURE
   q Q>BLOCK T-RES  RVISIT @ 1 - cells RQUOT-BLOCK-P @ + ! ;

: RQUOT@ ( n -- n ) 1 - cells RQUOT-BLOCK-P @ + @ ;


\ A row is named when its tail is not its owner's two rows' alone, and in facts
\ text, which tags them, when its tail has no scope or its kind is not its
\ unnamed form's (RVK-QUOT in a quotation, else 0).
: RPOS-NAMED? ( n n -- bool )
   {: t:n owner:n :}
   t ISROW RFACTS? and IF
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
   \ RBUF-ROW prints a stale tail among the values, named by the tail it wraps
   row ROW-TAIL STALE-ROW? IF
      RNAMES @ 1 = IF row STALE-TAIL owner RPOS+ THEN
      0 EXIT
   THEN
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
      x RBUF-ROW  CUT-MARK
      RBN @ BEGIN dup base > WHILE 1 - dup cells RBUF + @ d 0 RECURSE 32 EMIT1 REPEAT drop
      base RBN !
      EXIT
   THEN
   x T-RES {: r:n :}
   r TAG case
      T-VAR of r PAY SEEN-IDX VAR-NAME endof
      T-CON of r PAY CON-OUT endof
      T-PTR of s" ^" RSTR  r PTR>INNER d 0 RECURSE endof
      \ A cell a caught throw may have overwritten names the type it lost.
      \ `stale<...>` is not a declarable type, so it sets RQM: REC-SIG never
      \ records a row carrying one.
      T-STALE of
        1 RQM !
        s" stale<" RSTR
        r STALE-ROW? IF r RROW-LETTER EMIT1
        ELSE r STALE>INNER d 0 RECURSE THEN
        62 EMIT1
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
           r Q>FACTS CTL-STATABLE and {: facts:n :}
           63 0 ?do
              facts 1 i lshift and 0 <> IF i CTL-BIT>TAG$ RSTR 32 EMIT1 THEN
           loop
           93 EMIT1
        ELSE EMIT-UNSAID THEN
      endof
      T-FORALL of
        RBIND-N @ BINDER-DEPTH-MAX >= IF
           1 RBIND-FULL !
           NAME-FREE drop EMIT-UNSAID
        ELSE d QDEPTH-CAP < IF
           NLET @ RNAMED @ {: high:n given:n :}
           NAME-FREE {: idx:n :}
           idx RBIND-N @ cells RBIND + !
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
           RNAMED @ given 1 + = IF
              idx VAR-LETTERS < IF RLETS @ 1 idx lshift invert and RLETS ! THEN
              high NLET !  given RNAMED !
           THEN
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

\ Type T as a diagnostic spells it, in TEXT until TEXT is next written.
: TYPE-TEXT ( n -- str ) {: t:n :}
   1 DEST !  0 TEXT-LEN !  SEEN-RESET RATOM-RESET 0 NLET !
   t REND-TYPE
   0 DEST !  TEXT TEXT-LEN @ ;
variable RSHOW-DST

: REND-COLLECT ( n -- )  0 RBN !  RBUF-ROW ;

: SHOW-LOCAL-TYPE ( str n -- ) {: name:^ nameu:n t:n :}
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

\ Whether row s is a side of the diagnostic's pair (DEXP, DACT) and either
\ side ends in a stale tail: both sides then show their tails, `R n` against
\ `stale<R> n`.
: PAIR-STALE? ( n -- bool ) {: s:n :}
   DEXP @ 0=  DACT @ 0=  or IF RES-FALSE EXIT THEN
   s DEXP @ =  s DACT @ =  or 0= IF RES-FALSE EXIT THEN
   DEXP @ ROW-TAIL STALE-ROW?  DACT @ ROW-TAIL STALE-ROW?  or ;

: DROW {: s :}  s REND-COLLECT  RBUF-TAIL @ {: t:n :}
   s PAIR-STALE?  t ISROW  and IF t RROW-LETTER EMIT1 32 EMIT1 THEN
   CUT-MARK
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
: REND-EFFECT ( n n n n bool -- str )
   {: din:n dout:n rin:n rout:n hasr:bool :}
   TEXT-LEN @ {: base:n :}
   1 DEST !  0 RPOS-N !  0 RRESV !  1 RFACTS !
   1 RNAMES !  base TEXT-LEN !  din dout rin rout hasr REND-PASS
   2 RNAMES !  base TEXT-LEN !  din dout rin rout hasr REND-PASS
   0 RNAMES !  0 RFACTS !  0 DEST !  TEXT TEXT-LEN @ ;


: FACT-TAG ( str n -- ) {: a:^ u:n letter:n :}
   a u EMIT-STR  32 EMIT1  letter EMIT1  32 EMIT1 ;


: VAR-FACT-TAG ( str n -- )
   {: a:^ u:n idx:n :}
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
   id TVK@ KIND>TAG$ {: a:^ u:n :}
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
   kind KIND>TAG$ {: a:^ u:n :}
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
: REND-FACTS ( n n n n bool -- str )
   {: din:n dout:n rin:n rout:n hasr:bool :}
   1 DEST !  s" ( " EMIT-STR
   din dout rin rout hasr REND-EFFECT 2drop
   1 DEST !  s"  ) " EMIT-STR
   0 BEGIN dup NLET @ < WHILE dup VAR-KIND-TAG 1 + REPEAT drop
   0 BEGIN dup NLET @ < WHILE dup VAR-SCOPE-TAG 1 + REPEAT drop
   0 BEGIN dup RROW-N @ < WHILE dup ROW-KIND-TAG 1 + REPEAT drop
   0 BEGIN dup RROW-N @ < WHILE dup ROW-SCOPE-TAG 1 + REPEAT drop
   RVISIT @ 1 + {: nq:n :}
   2 BEGIN dup nq < WHILE dup QUOT-BLOCK-TAG 1 + REPEAT drop
   1 BEGIN dup nq < WHILE dup QUOT-NOBLOCK-TAG 1 + REPEAT drop
   0 DEST !  TEXT TEXT-LEN @ ;


: CTL-FLAG-TAG ( n n -- )
   {: ctl:n bit:n :}
   ctl 1 bit lshift and 0= IF EXIT THEN
   bit CTL-BIT>TAG$ {: a:^ u:n :}
   u 0 = IF EXIT THEN
   a u EMIT-STR  32 EMIT1 ;


: CTL-THROWS-TAG ( n -- )
   {: ctl:n :}
   ctl CTL-THROW and 0= IF EXIT THEN
   s" throws " EMIT-STR  ctl XFER-DMASK RNUM  32 EMIT1  ctl XFER-RMASK RNUM  32 EMIT1 ;


\ checker.f NORET.NONZERO n as the tag `nonzero D T` appended at TEXT-LEN, D
\ the divisor and T the thrown inputs' mask; 0 appends nothing.
: REND-NONZERO ( n -- str )
   {: n:n :}
   1 DEST !
   n 0 <> IF
      n NONZERO-UNPACK {: div:n thr:n :}
      s" nonzero " EMIT-STR  div RNUM  32 EMIT1  thr RNUM  32 EMIT1
   THEN
   0 DEST !  TEXT TEXT-LEN @ ;


: CTL-INTRINSIC-TAG ( n -- )
   {: ctl:n :}
   ctl CTL>INTRINSIC {: id:n :}
   id 0 = IF EXIT THEN
   id INTRINSIC-NAME$ {: a:^ u:n :}
   u 0 = IF 1 RQM ! EXIT THEN
   s" intrinsic " EMIT-STR  a u EMIT-STR  32 EMIT1 ;


\ Control word n's tags for checker.f SIG-CTL-TAG, in bit order, appended at TEXT-LEN.
\ RQM: a bit XFER-PACK does not keep, an unnamed intrinsic id, masks without CTL-THROW.
: REND-CTL ( n -- str )
   {: ctl:n :}
   1 DEST !
   ctl XFER-FLAGS  ctl XFER-DMASK  ctl XFER-RMASK  XFER-PACK  ctl <> IF 1 RQM ! THEN
   ctl CTL-THROW and 0=  ctl XFER-DMASK ctl XFER-RMASK or 0 <>  and IF 1 RQM ! THEN
   ctl 0 CTL-FLAG-TAG
   ctl CTL-THROWS-TAG
   63 2 ?do
      i CTL-INTRINSIC-SHIFT = IF ctl CTL-INTRINSIC-TAG THEN
      ctl i CTL-FLAG-TAG
   loop
   0 DEST !  TEXT TEXT-LEN @ ;


\ A PE row's flags as facts tags appended at TEXT-LEN; the PE-ACTIVE,
\ PE-TRUSTED-ONLY and PE-TOP-ONLY bits are facts a caller's check reads.
\ `primitive` (PE-ACTIVE) makes the read a primitive row, whose tick is an
\ opaque xt: without it a text reads as the word's declaration, whose tick is
\ its quotation (`['] die`).
: REND-PE ( n -- str )
   {: flags:n :}
   1 DEST !
   flags PE-ACTIVE and 0 <> IF s" primitive " EMIT-STR THEN
   flags PE-TRUSTED-ONLY and 0 <> IF s" trusted-only " EMIT-STR THEN
   flags PE-TOP-ONLY and 0 <> IF s" top-only " EMIT-STR THEN
   0 DEST !  TEXT TEXT-LEN @ ;


\ A `parses:` row as a facts tag appended at TEXT-LEN, spelled as the row:
\ `parses N`, or `parses-through N ( E1 E2 )` for terminators T TU, each
\ followed by a blank; nothing for a count of -1.
: REND-PARSES ( n str -- str )
   {: cnt:n t:^ tu:n :}
   1 DEST !
   cnt 0 >= IF
      tu 0= IF s" parses " ELSE s" parses-through " THEN EMIT-STR
      cnt RNUM  32 EMIT1
      tu 0 <> IF s" ( " EMIT-STR  t tu EMIT-STR  s" ) " EMIT-STR THEN
   THEN
   0 DEST !  TEXT TEXT-LEN @ ;


\ Record off instantiated: its data rows, its return rows (0 0 when it has
\ none) and whether it has them.
: RECORD-ROWS ( n -- n n n n bool )
   {: off:n :}
   off E-PTR E-INST-RESET
   off E-PTR E-HASR@ 0 <> {: hasr:bool :}
   off E-PTR E-DIN@ E-INST  off E-PTR E-DOUT@ E-INST
   hasr IF off E-PTR E-RIN@ E-INST  off E-PTR E-ROUT@ E-INST ELSE 0 0 THEN
   hasr ;
\ Record off's facts text, appended at TEXT-LEN.
: REND-RECORD ( n -- str ) RECORD-ROWS REND-FACTS ;


\ Record off's effect as REND-EFFECT writes it, appended at TEXT-LEN.
: REND-RECORD-EFFECT ( n -- str ) RECORD-ROWS REND-EFFECT ;


\ `creates-effect ` and the facts text of n, a created effect as checker.f
\ NORET-CREATES@ answers it, appended at TEXT-LEN; 0 appends nothing. RQM stays
\ set.
: REND-CREATES ( n -- str )
   {: n:n :}
   n 0= IF 0 DEST !  TEXT TEXT-LEN @ EXIT THEN
   RQM @ {: qm:n :}
   1 DEST !  s" creates-effect " EMIT-STR
   n 1 - REND-RECORD
   qm 0 <> IF 1 RQM ! THEN ;


\ `overload `, primitive row k's effect and its tags, appended at TEXT-LEN;
\ checker.f SIG-TAGS reads the rest of the text as the primitive's next row, so
\ the row needs no `primitive`.
: REND-OVERLOAD ( n -- str )
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
\ lookup sees it; a row too deep to record is refused before E-BUILD-EFFECT dies.
: FACTS-BUILD ( -- )
   NEW
   FACTS-PARSE {: din:n dout:n rin:n rout:n :}
   SGBAD @ 0 <>  SIG-UNRES @ 0 <>  or IF EXIT THEN
   TEXT TEXT-LEN @ din dout rin rout SGBAD-UNFIT!
   SGBAD @ 0 <> IF EXIT THEN
   0 CHECKER-REC-SYM !
   din dout rin rout SGHASR @ E-BUILD-EFFECT 1 + FACTS-NEW ! ;


\ The parse refuses a text by SGBAD or SIG-UNRES, never by a throw.
: FACTS-ANSWER ( -- bool )
   FACTS-BUILD
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
: REND-SIG ( -- str )
   0 TEXT-LEN !  BROW @ DCUR @ 0 0 RES-FALSE REND-EFFECT ;

variable DSUGE  variable DSUGA


\ One pass of INFERRED-FROM: RROW-NAME names each row as in REND-EFFECT.
: JEFFECT-TEXT ( n n n n bool -- )
   {: din:n dout:n rin:n rout:n hasr:bool :}
   din 0 RROW-NAME  din DROW  s" -- " EMIT-STR  dout 0 RROW-NAME  dout DROW
   hasr IF
      s" | " EMIT-STR  rin 1 RROW-NAME  rin DROW  s" -- " EMIT-STR  rout 1 RROW-NAME  rout DROW
   THEN ;

: INFERRED-PASS ( n -- )   \ input row n to the current rows
   0 RVISIT !
   DCUR @
   SGHASR @ IF SGRIN @ ELSE RBROW @ THEN
   RCUR @
   SGHASR @ JEFFECT-TEXT ;


\ RSEEN holds SEEN's names while INFERRED-FROM's first pass gives more.
SCRATCH{
PTR-VARIABLE RSEEN-P   variable RSEEN-CAP
}SCRATCH

: SEEN-SAVE ( -- )
   RSEEN-P RSEEN-CAP SEEN-HW @ 1 ARENA-ROWS-ENSURE
   SEEN RSEEN-P @ SEEN-HW @ ARENA-CELLS-COPY ;

\ Puts back the names SEEN-SAVE copied when SEEN-HW was hw.
: SEEN-BACK ( n -- ) {: hw:n :}
   RSEEN-P @ SEEN hw ARENA-CELLS-COPY
   SEEN hw SEEN-HW @ ARENA-CELLS-UNBOUND
   hw SEEN-HW ! ;

\ The names given so far, which NAMES-BACK puts back.
: NAMES-MARK ( -- n n n n n )
   SEEN-SAVE  SEEN-HW @ RLETS @ RNAMED @ NLET @ RROW-N @ ;

: NAMES-BACK ( n n n n n -- )
   RROW-N !  NLET !  RNAMED !  RLETS !  SEEN-BACK ;


\ The body's effect so far, input row n to the current rows, in REND-EFFECT's
\ two passes from the TEXT-LEN it is called with. Its names follow those the
\ record gave; the second pass starts from the names the first started from.
: INFERRED-FROM ( n -- )
   {: din:n :}
   TEXT-LEN @ {: base:n :}  NAMES-MARK
   0 RPOS-N !  0 RRESV !  1 RNAMES !  din INFERRED-PASS
   NAMES-BACK  base TEXT-LEN !  2 RNAMES !  din INFERRED-PASS
   0 RNAMES ! ;
: INFERRED-EFFECT ( -- )   \ from the declared inputs
   SGSEEN @ IF SGIN @ ELSE BROW @ THEN INFERRED-FROM ;


\ The escape rejection (MD-TAIL-ESCAPE): the declared callable, the argument
\ compared with it, the held row, then the inferred effect, the order the record
\ reads them, rendered with fresh letters into TCAP before later tokens bind
\ their rows. Naming the callable pair, not the stacks around it, it reads the same
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
: TCAP ( -- ^u8 ) TCAP-P @ ;
: TCAP-EXP$ ( -- str ) TCAP TCAP-EXP @ ;
: TCAP-ACT$ ( -- str ) TCAP TCAP-EXP @ +  TCAP-ACT @ TCAP-EXP @ - ;
: TCAP-HELD$ ( -- str ) TCAP TCAP-ACT @ +  TCAP-HELD @ TCAP-ACT @ -  SIG-RTRIM ;
: TCAP-INF$ ( -- str ) TCAP TCAP-HELD @ +  TCAP-INF @ TCAP-HELD @ - ;


: TCAP-ROW ( n -- )   \ a row: its tail's letter and cells
   {: x:n :}
   x ISVAR IF x REND-TYPE EXIT THEN
   x QTAIL  x DROW ;


\ Each capture rewrites every byte a reader sees, so a grow keeps none.
: TCAP-ROOM ( n -- )
   {: need:n :}
   need TCAP-CAP @ <= IF EXIT THEN
   need TCAP-CAP @ 2 * max TCAP-FIRST max {: nc:n :}
   nc ARENA-ALLOC-BYTES {: next:^ :}
   TCAP-P @ TCAP-CAP @ ARENA-UNMAP
   next TCAP-P !  nc TCAP-CAP ! ;


: TAIL-CAPTURE ( -- )
   DEST @ {: dst:n :}  TEXT-LEN @ {: base:n :}  RQM @ {: qm:n :}
   1 DEST !  SEEN-RESET 0 NLET !  0 RROW-N !
   -1 RTAILS !
   DF-EXP @ REND-TYPE 32 EMIT1  TEXT-LEN @ base - TCAP-EXP !
   DF-ACT @ REND-TYPE 32 EMIT1  TEXT-LEN @ base - TCAP-ACT !
   TAIL-SKOLEM @ ROW-TAIL RROW-LETTER EMIT1  s"  would escape into " EMIT-STR
   TAIL-ROW @ TCAP-ROW  TEXT-LEN @ base - TCAP-HELD !
   0 RTAILS !
   INFERRED-EFFECT  TEXT-LEN @ base - TCAP-INF !
   TCAP-INF @ TCAP-ROOM
   0 BEGIN dup TCAP-INF @ < WHILE
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

\ A row pair's repair by depth, counted in cells.
: ROWS-DEPTHS ( -- )
   AT-PIN
   DEXP @ ROW-CELLS DSUGE !
   DACT @ ROW-CELLS DSUGA !
   ;AT-PIN ;


: ROWS-SUGGEST$ ( -- str )
   ROWS-DEPTHS
   DSUGA @ DSUGE @ > IF s" drop the extra value, or remove the word that pushes it" EXIT THEN
   DSUGA @ DSUGE @ < IF s" push the missing value before it is taken, or stop taking it" EXIT THEN
   s" change the body so the values it leaves have the types the signature declares" ;


: MD-CODE$ ( n -- str )
   case
      MD-FAM-UNKNOWN  of s" E-MATCH-UNKNOWN-FAMILY" endof
      MD-FAM-AMBIG    of s" E-USING-AMBIGUOUS" endof
      MD-FAM-KIND     of s" E-MATCH-FAMILY-KIND" endof
      MD-SCRUT        of s" E-MATCH-SCRUTINEE" endof
      MD-FAM-MISMATCH of s" E-MATCH-FAMILY-MISMATCH" endof
      MD-VAR-UNKNOWN  of s" E-MATCH-UNKNOWN-VARIANT" endof
      MD-VAR-DUP      of s" E-MATCH-DUPLICATE-VARIANT" endof
      MD-MISSING-OF   of s" E-MATCH-MISSING-OF" endof
      MD-NONEXH       of s" E-MATCH-NONEXHAUSTIVE" endof
      MD-STRAY        of s" E-MATCH-STRAY" endof
      MD-TRUNC        of s" E-MATCH-UNTERMINATED" endof
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
      MD-RAW-FAMILY   of s" E-MISMATCH" endof
      MD-NULL-PTR     of s" E-RAW-CELL-PTR" endof
      MD-DBASE-PTR    of s" E-RAW-CELL-PTR" endof
      MD-RAW-FIELD    of s" E-RAW-CELL-PTR" endof
      MD-RAW-EXEC     of s" E-RAW-CELL-PTR" endof
      MD-STALE-READ   of s" E-STALE-READ" endof
      MD-TOP-UNKNOWN  of s" E-STALE-READ" endof
      MD-TAIL-ESCAPE  of s" E-MISMATCH" endof
      MD-LIT-FIT      of s" E-MISMATCH" endof
      MD-ROW-CYCLE    of s" E-MISMATCH" endof
      MD-SCOPE-STORE  of s" E-SCOPED-STORAGE" endof
      MD-SCOPE-KIND   of s" E-SCOPE-TYPE" endof
      MD-C2-COPY      of s" E-C2-COPY" endof
      MD-C2-DROP      of s" E-C2-DROP" endof
      MD-C2-ESCAPE    of s" E-C2-SCOPE-ESCAPE" endof
      MD-C2-INIT-RECORD of s" E-C2-INIT-RECORD" endof
      MD-UNDERFLOW    of s" E-INPUT-UNDERFLOW" endof
      MD-RIGID-REGION of s" E-RIGID-REGION-MISMATCH" endof
      MD-RIGID-EXTENT of s" E-RIGID-EXTENT-MISMATCH" endof
      MD-RIGID-GEN    of s" E-RIGID-STALE-GENERATION" endof
      MD-RIGID-XDOM   of s" E-RIGID-DOMAIN-CONFUSION" endof
      MD-EFFECT-DEPTH of s" E-UNCHECKABLE" endof
      MD-RECURSE-SIG  of s" E-UNCHECKABLE" endof
      MD-RECURSE-DOES of s" E-UNCHECKABLE" endof
      MD-IS-UNKNOWN   of s" E-UNDEFINED" endof
      MD-LOCAL-ENDED  of s" E-UNDEFINED" endof
      MD-LOCAL-STRAY  of s" E-UNDEFINED" endof
      MD-LOCAL-UNBOUND of s" E-UNDEFINED" endof
      MD-IS-NOT-DEFER of s" E-NOT-DEFERRED" endof
      MD-IS-MISSING   of s" E-MISSING-NAME" endof
      MD-TYPE-FORM    of s" E-BAD-STORAGE" endof
      MD-STORED-LINEAR of s" E-CAST-LINEAR" endof
      MD-STORED-CLASS of s" E-CAST-CLASS" endof
      MD-IS-STORED    of s" E-CAST-LINEAR" endof
      MD-DIV-ZERO     of s" E-DIV-ZERO" endof
      MD-THROW-ZERO   of s" E-THROW-ZERO" endof
      MD-LINEAR-DROP  of s" E-LINEAR-DROP" endof
      MD-LINEAR-COPY  of s" E-LINEAR-COPY" endof
      MD-LINEAR-TAINT of s" E-LINEAR-COPY" endof
      MD-LINEAR-EFFECT of s" E-LINEAR-EFFECT" endof
      MD-LINEAR-MEMORY of s" E-LINEAR-MEMORY" endof
      MD-MEMORY-ARG   of s" E-LINEAR-MEMORY" endof
      MD-MEMORY-EMPTY of s" E-ZERO-WIDTH" endof
      MD-LOOP-DEPTH   of s" E-MISMATCH" endof
      MD-IF-DEPTH     of s" E-MISMATCH" endof
      MD-ARMS-DEPTH   of s" E-MISMATCH" endof
      MD-LOCAL-ESCAPE of s" E-LOCAL-ESCAPE" endof
      MD-BINDER-PLACE of s" E-BINDER-PLACE" endof
      MD-FINALLY-CLEANUP of s" E-FINALLY-CLEANUP" endof
      MD-CALLABLE-NARROW of s" E-CALLABLE-NARROW" endof
      MD-RECORD-AT-WIDE of s" E-RECORD-AT-WIDE" endof
      MD-MEMBER-TOP of s" E-LAYOUT-VALUE" endof
      MD-CATCH-TOP  of s" E-LAYOUT-VALUE" endof
      MD-CATCH-LINEAR of s" E-LINEAR-DROP" endof
      MD-ONE-CELL   of s" E-MISMATCH" endof
      MD-OFF-STEP   of s" E-MISMATCH" endof
      MD-MOVED-LINEAR of s" E-LINEAR-COPY" endof
      MD-FACT-UNSTATED of s" E-MISMATCH" endof
      MD-FACT-INPUT of s" E-MISMATCH" endof
      MD-FACT-PASSED of s" E-MISMATCH" endof
      MD-CONSTRUCT-WIDE of s" E-CONSTRUCT-WIDE" endof
      MD-C2-BINDER    of s" E-C2-BINDER" endof
      MD-C2-RECORDS-BINDER of s" E-C2-BINDER" endof
      MD-C2-CHILD-BINDER of s" E-C2-BINDER" endof
      MD-C2-FIELD-BINDER of s" E-C2-BINDER" endof
      MD-C2-ROOT-BINDER of s" E-C2-BINDER" endof
      MD-C2-ROOT-INIT of s" E-MISMATCH" endof
      MD-FIELD-NAME   of s" E-MISSING-NAME" endof
      MD-C2-FIELD-UNKNOWN of s" E-UNKNOWN-FIELD" endof
      MD-CALLBACK-NOT-GENERIC of s" E-CALLBACK-NOT-GENERIC" endof
      MD-SCOPED-TICK  of s" E-UNSAFE" endof
      MD-DOES-UNMADE  of s" E-DOES-UNCREATED" endof
      MD-NO-LOOP      of s" E-NO-LOOP" endof
      MD-LOOP-CARRIED of s" E-LOOP-CARRIED" endof
      MD-LOOP-NARROWED of s" E-LOOP-CARRIED" endof
      MD-INPUT-NARROWED of s" E-MISMATCH" endof
      MD-CLOSES-NOTHING of s" E-CONTROL-ORPHAN" endof
      MD-NO-CASE      of s" E-CONTROL-ORPHAN" endof
      MD-NO-OUTER-LOOP of s" E-NO-OUTER-LOOP" endof
      MD-LOOP-RDEPTH  of s" E-LOOP-RETURN-DEPTH" endof
      MD-ELSE-TWICE   of s" E-ELSE-TWICE" endof
      MD-EXIT-LOOP    of s" E-EXIT-IN-LOOP" endof
      MD-UNLOOP-JOIN  of s" E-UNLOOP-JOIN" endof
      MD-CF-OPEN      of s" E-CONTROL-OPEN" endof
      MD-DEPTH-WIDE   of s" E-DEPTH-WIDE" endof
      MD-LITERAL-RANGE of s" E-LITERAL-RANGE" endof
      s" render: a reason with no code" EXIT-STATUS:INTERNAL die
   endcase ;


: TAIL-SUGGEST$ ( -- str )
   s" declare the callable `[ R -- S ]` from the word's input row, as in `( R [ R -- S ] -- S )`, or pass a callable whose output does not depend on the stack beneath it" ;


: MD-SUGGEST$ ( n -- str )
   case
      MD-NONEXH       of s" add a `variant OF ... ENDOF` arm for each missing variant" endof
      MD-VAR-DUP      of s" keep one `OF ... ENDOF` arm for this variant and delete the other" endof
      MD-VAR-UNKNOWN  of s" write a variant the family declares, spelled as its declaration spells it" endof
      MD-CON-VAR      of s" write a variant the family declares, spelled as its declaration spells it" endof
      MD-FAM-UNKNOWN  of s" name a family visible here: one of this package, a global one, one a `using` imports, or `PKG:family`" endof
      MD-FAM-AMBIG    of s" qualify the family as `PKG:family` with the package you mean" endof
      MD-CON-FAM      of s" name a family of this package after `construct`, or call another package's generated constructor, as `PKG-FAMILY:VARIANT`" endof
      MD-FAM-KIND     of s" name a `SUMTYPE` or `ENUM` family after `MATCH`" endof
      MD-CON-KIND     of s" name a `SUMTYPE` or `ENUM` family after `construct`" endof
      MD-SCRUT        of s" push the family's value so it is on top of the stack at `MATCH`" endof
      MD-FAM-MISMATCH of s" name the family of the value on top of the stack after `MATCH`" endof
      MD-QUOT         of s" move the `MATCH` into a named word with a declared effect and call that word in the quotation" endof
      MD-OPEN-ARGS    of s" give the value concrete family arguments in the signature, as `option<n>` instead of `option<a>`" endof
      MD-JOIN         of s" make every `OF` arm that continues leave the same stack types at its `ENDOF`" endof
      MD-MISSING-OF   of s" write `OF` after the variant, as `variant OF ... ENDOF`" endof
      MD-DIVBAR       of s" call the block collective where no control structure or quotation is open, so every thread of the block reaches it" endof
      MD-EXEC-OPAQUE  of s" `execute` a quotation parameter, a `defer` bound with `is`, or an xt fetched from a `TYPED-VARIABLE NAME [ in -- out ]` cell; call a declaration by name at top level" endof
      MD-CATCH-OPAQUE of s" `catch` a quotation parameter, a `defer` bound with `is`, or an xt fetched from a `TYPED-VARIABLE NAME [ in -- out ]` cell; call a declaration by name at top level" endof
      MD-RAW-PTR      of s" declare the cell that holds the address with `PTR-VARIABLE`, `PERSISTED-PTR-VARIABLE`, `TYPED-VARIABLE NAME ^t` or `TYPED-BUFFER NAME ^t`" endof
      MD-RAW-FAMILY   of s" keep the value in storage that names its type, `TYPED-VARIABLE NAME t` or `TYPED-BUFFER NAME t`; for `constant`, define a word that leaves it, `: NAME ( -- t ) ... ;`" endof
      MD-NULL-PTR     of s" read the value from the cell that holds it, not through `NULL-PTR`" endof
      MD-DBASE-PTR    of s" reach the cell through an accessor that names its pointee, as `( -- ^n )`, or take a pointer it holds with `ptr-field`" endof
      MD-RAW-EXEC     of s" declare the cell that holds the code as `TYPED-VARIABLE NAME [ in -- out ]`, a `TYPED-BUFFER` or `DYNAMIC-BUFFER` of `[ in -- out ]`, or a `defer` bound with `is`" endof
      MD-RAW-FIELD    of s" take `ptr-field` of a cell declared with `PTR-VARIABLE`, `PERSISTED-PTR-VARIABLE`, `TYPED-VARIABLE NAME ^t` or `TYPED-BUFFER NAME ^t`" endof
      MD-RIGID-REGION of s" pass a value of the allocation this position takes, or borrow again from that allocation" endof
      MD-RIGID-EXTENT of s" pass the value whose extent this position takes" endof
      MD-RIGID-GEN    of s" derive the index again after the mutation that outdated it" endof
      MD-RIGID-XDOM   of s" pass a region where a region is taken, an extent where an extent is, and a mutation generation where a generation is" endof
      MD-UNDERFLOW    of TOP-STEP @ IF s" push the cells the statement takes before it" ELSE s" push the missing inputs before the call, or declare them in the signature" THEN endof
      MD-STALE-READ   of s" bind the value to a local before the `catch` and read the local after it, or drop the cell; read a caught result where its code is 0: after `code 0<> if code throw then`" endof
      MD-TOP-UNKNOWN  of s" push the value while checking is on, or drop the cell" endof
      MD-TAIL-ESCAPE  of TAIL-SUGGEST$ endof
      MD-LIT-FIT      of s" write a literal in the type's range, or declare a wider type" endof
      MD-ROW-CYCLE    of s" keep callables that run on this stack off it: move them aside with `>r` … `r>` around the call, or drop them first" endof
      MD-SCOPE-STORE  of s" keep the scoped value on the stack or in a local instead of storing it in memory" endof
      MD-SCOPE-KIND   of s" write the scope parameter only in a view type's scope slot, never as a value type" endof
      MD-C2-COPY      of s" move the exclusive value once, or lend it with `WITH-MUT-LOAN`, instead of copying it" endof
      MD-C2-DROP      of s" return the exclusive value or pass it to a word that consumes it instead of dropping it" endof
      MD-C2-ESCAPE    of s" return only values that do not depend on the owner or loan this operation opened" endof
      MD-C2-INIT-RECORD of s" pass a `STRUCTURE … DERIVE init` value made by its `MAKE`, with no linear, unique or open field" endof
      MD-EFFECT-DEPTH of DEPTH-SUGGEST$ endof
      MD-RECURSE-SIG  of s" declare the definition's stack effect after its name, as `: NAME ( n -- n )`" endof
      MD-RECURSE-DOES of s" move the recursion into a named word with a declared effect and call that word from the `does>` clause" endof
      MD-IS-UNKNOWN   of s" name a word declared with `defer` after `is`, as `PKG:NAME` or with its package imported by `using`" endof
      MD-LOCAL-ENDED  of s" read the local inside the `{:` block that binds it, and keep a value needed after the block on the stack" endof
      MD-LOCAL-STRAY  of s" append a built local between the `{:` and `:}` keywords of its group" endof
      MD-LOCAL-UNBOUND of s" read a built local by the index `EFFECT:LOCAL` answered, after the `:}` that binds it" endof
      MD-IS-NOT-DEFER of s" declare the target with `defer NAME ( in -- out )`, or name a deferred word after `is`" endof
      MD-IS-MISSING   of s" write the name of a deferred word after `is`" endof
      MD-TYPE-FORM    of s" write after `[type]` a closed type storage holds, as `n`, `u8` or a declared record, with no type variable, quotation, linear value, atom, xt or width in it" endof
      MD-STORED-LINEAR of s" pass a type that holds no linear value where a cast stores this type variable" endof
      MD-STORED-CLASS of s" pass a type that holds no atom and no width where a cast stores this type variable" endof
      MD-IS-STORED    of s" declare the cell over the concrete type the callable stores, or install a callable whose type variable takes any type" endof
      MD-DIV-ZERO     of s" divide by a value that cannot be zero" endof
      MD-THROW-ZERO   of s" throw a nonzero code, as an error constant the package declares" endof
      MD-LINEAR-DROP  of s" pass the value to the word its package gives for consuming it instead of dropping it" endof
      MD-LINEAR-COPY  of s" use the value once and take another from its producer instead of copying it" endof
      MD-LINEAR-TAINT of s" use the value once and take another from its producer instead of copying it" endof
      MD-LINEAR-EFFECT of s" call a word whose effect takes and returns that value equally, or pass a value that is not linear" endof
      MD-LINEAR-MEMORY of s" keep the value on the stack and pass it on instead of `@` or `!` through a pointer" endof
      MD-MEMORY-ARG   of s" give the family arguments that hold no linear value and no `mut-view`, as `n`, or keep the value on the stack" endof
      MD-MEMORY-EMPTY of s" keep the zero-width value on the stack instead of `@` or `!` through a pointer" endof
      MD-LOOP-DEPTH   of ROWS-SUGGEST$ endof
      MD-IF-DEPTH     of ROWS-SUGGEST$ endof
      MD-ARMS-DEPTH   of ROWS-SUGGEST$ endof
      MD-LOCAL-ESCAPE of s" run the quotation within the block of the locals it reads, or have it take their values from the stack" endof
      MD-BINDER-PLACE of s" call the scope operation directly in the word that gives it its callback" endof
      MD-DEPTH-WIDE   of s" take the wide value apart first, with `MATCH` on concrete arguments or `UNMAKE`, or count the values another way" endof
      MD-LOOP-RDEPTH  of s" pop with `r>` in the loop body every cell it pushes with `>r`" endof
      MD-LOOP-CARRIED of s" make each pass leave the cells it carries with the types and facts it entered with, and keep a value that changes per pass in a variable" endof
      MD-LOOP-NARROWED of s" carry the quotation with the type it entered with, or give it the narrower type before the loop" endof
      MD-INPUT-NARROWED of s" narrow the input before the body runs it, or state the word's effect in a signature" endof
      MD-NO-LOOP      of s" write this word only inside a `do` or `?do` loop this definition opened and has not closed" endof
      MD-NO-OUTER-LOOP of s" write `i` for a single loop's index, and `j` only inside a `do` loop nested in another" endof
      MD-CLOSES-NOTHING of s" close each structure with its own word, innermost first: `then` for `if`, `until` or `repeat` for `begin`, `loop` for `do`, `endcase` for `case`, `;]` for `[:`" endof
      MD-NO-CASE      of s" write `of` only inside `case ... endcase`, after the previous arm's `endof`" endof
      MD-ELSE-TWICE   of s" nest another `if ... else ... then` inside the `else` arm for each further path" endof
      MD-EXIT-LOOP    of s" write `unloop` before `exit` once for each loop the `exit` leaves, or `leave` the loop and return after it" endof
      MD-UNLOOP-JOIN  of s" write `unloop` for the same loops on every path that meets here, or `exit` from each path right after its `unloop`" endof
      MD-CF-OPEN      of s" close every `if`, `do`, `begin`, `case` and `[:` before the `;`" endof
      MD-DOES-UNMADE  of s" call `create NAME` before `does>`, outside any IF, loop or quotation" endof
      MD-FINALLY-CLEANUP of s" put a cleanup of effect `[ -- ]` on top, as `[: body ;] [: cleanup ;] finally`" endof
      MD-CALLABLE-NARROW of s" store a callable whose effect works for every type the cell's effect names, or declare the cell's effect with the types this one takes" endof
      MD-RECORD-AT-WIDE of s" index a `LAYOUT-BUFFER` or `TYPED-BUFFER` of the instantiated type, which steps by its width" endof
      MD-MEMBER-TOP of s" move the call inside a definition, which uses the receiver's layout" endof
      MD-CATCH-TOP  of s" move the catch inside a definition" endof
      MD-CATCH-LINEAR of s" open the handle inside the caught body" endof
      MD-ONE-CELL   of s" call it with a plain one-cell type (`n`, a pointer), or do the work in a word whose effect names" endof
      MD-FACT-UNSTATED of s" state" endof
      MD-FACT-INPUT of s" state" endof
      MD-FACT-PASSED of s" state" endof
      MD-CONSTRUCT-WIDE of s" declare the widest variant over the type argument, or hold the argument through a one-cell `^`" endof
      MD-C2-BINDER    of s" declare the callback `forall<i inside [l,T],[ mut-view<p,i,a,init<i,T>> -- … ]>`, with `l` the view's scope and `T` the record" endof
      MD-C2-RECORDS-BINDER of s" declare the callback `forall<i inside [l,T],[ records<p,i,a,T> -- … ]>`, with `l` the view's scope and `T` the record" endof
      MD-C2-CHILD-BINDER of s" declare the callback `forall<j inside l,[ … ]>`, with `l` the parent view's scope" endof
      MD-C2-FIELD-BINDER of s" declare the callback `forall<j inside i,forall-region<c,[ mut-view<p,j,c,init<i,F>> -- … ]>>`, with `i` the view's scope and `F` the field's type" endof
      MD-C2-ROOT-BINDER of s" declare the callback `forall<p,forall-region<a,[ mut-view<p,p,C2-MEM:root<a>,u8> -- … ]>>`" endof
      MD-C2-ROOT-INIT of s" bind an owner only in a `WITH-INIT` over a `C2-MEM:root<a>` view, which `C2-MEM:WITH-MUT` lends and `WITH-MUT-LOAN` and `PREFIX` keep; seed any other view with a callback that takes its own region" endof
      MD-FIELD-NAME   of s" write the field after it, as `C2-MEM:WITH-FIELD first`" endof
      MD-C2-FIELD-UNKNOWN of s" name a field the record's `STRUCTURE` declares" endof
      MD-CALLBACK-NOT-GENERIC of s" pass a quotation written at the call whose types name no outer scope, or declare the parameter `forall<q,…>` too" endof
      s" close each `MATCH family` with `;MATCH` after its `variant OF ... ENDOF` arms, and write `construct family variant` with both operands" rot
   endcase ;


: MD-REASON$ ( n -- str )
   case
      MD-FAM-UNKNOWN  of s" bad match: unknown type family" endof
      MD-FAM-AMBIG    of s" bad match: ambiguous type family" endof
      MD-FAM-KIND     of s" bad match: family is not a sum or enum" endof
      MD-SCRUT        of s" bad match: expected sum or enum value on stack" endof
      MD-FAM-MISMATCH of s" bad match: family mismatch" endof
      MD-VAR-UNKNOWN  of s" bad match: unknown variant" endof
      MD-VAR-DUP      of s" bad match: duplicate variant" endof
      MD-MISSING-OF   of s" bad match: variant token must be followed by OF" endof
      MD-NONEXH       of s" bad match: missing variants:" endof
      MD-STRAY        of s" bad match: misplaced match token" endof
      MD-TRUNC        of s" bad match: unterminated match" endof
      MD-QUOT         of s" bad match: quotation rows cannot carry a scrutinee" endof
      MD-OPEN-ARGS    of s" bad match: scrutinee type arguments are unresolved" endof
      MD-JOIN         of s" bad match: branch output mismatch" endof
      MD-CON-FAM      of s" bad construct: family not declared in the active package" endof
      MD-CON-KIND     of s" bad construct: family is not a sum or enum" endof
      MD-CON-VAR      of s" bad construct: unknown variant" endof
      MD-CON-TRUNC    of s" bad construct: missing family or variant token" endof
      MD-DIVBAR       of s" divergent barrier: block collective requires block-uniform control" endof
      MD-EXEC-OPAQUE  of s" execute: opaque xt of unknown provenance (fetched from untyped memory, or ticked from a primitive, a declaration or a word with no stack effect)" endof
      MD-CATCH-OPAQUE of s" catch: opaque xt of unknown provenance (fetched from untyped memory, or ticked from a primitive, a declaration or a word with no stack effect)" endof
      MD-RAW-PTR      of s" raw storage cell: a pointer cannot be stored in or fetched from an undeclared cell" endof
      MD-RAW-FAMILY   of s" raw storage cell: an undeclared cell holds only a plain number, flag or character, never a nominal value" endof
      MD-NULL-PTR     of s" null address: nothing is read through NULL-PTR, so it is never a nominal type and never the address of one" endof
      MD-DBASE-PTR    of s" base address: a cell reached from a pointer computed off NULL-PTR, or from the engine's DATA base, holds a plain value at every pointee depth, never a nominal type or a pointer" endof
      MD-RAW-FIELD    of s" ptr-field: base is an undeclared raw storage cell, not a declared pointer cell" endof
      MD-RAW-EXEC     of s" raw storage cell: an undeclared cell cannot hold an execution token / a quotation" endof
      MD-STALE-READ   of s" stale cell: this reads a cell `catch` left stale (a throw path of the caught body may have overwritten it)" endof
      MD-TOP-UNKNOWN  of s" unknown cell: this reads a cell beneath the cells the interpret row knows (left before checking began)" endof
      MD-TAIL-ESCAPE  of s" callable row escape: a callable declared with an implicit input row cannot keep or name the stack it runs on" endof
      MD-LIT-FIT      of s" literal" endof
      MD-ROW-CYCLE    of s"  would make a stack row contain itself" endof
      MD-SCOPE-STORE  of s" scoped storage: ordinary memory cannot retain a scope dependency" endof
      MD-SCOPE-KIND   of s" scope parameter: a scope identity cannot stand in for a value type" endof
      MD-C2-COPY      of s" exclusive value copied" endof
      MD-C2-DROP      of s" exclusive value discarded" endof
      MD-C2-ESCAPE    of s" scoped value escapes its owner or loan" endof
      MD-C2-INIT-RECORD of s" the entry initializes a fixed-cell record, and this value is not one" endof
      MD-RIGID-REGION of s" rigid host: region mismatch (different allocation)" endof
      MD-RIGID-EXTENT of s" rigid host: extent mismatch (different bounds identity)" endof
      MD-RIGID-GEN    of s" rigid host: stale mutation generation" endof
      MD-RIGID-XDOM   of s" rigid host: identity domain confusion" endof
      MD-UNDERFLOW    of TOP-STEP @ IF s" input underflow: the statement takes more cells than the stack holds" ELSE s" input underflow: the call takes more cells than the definition's declared inputs leave" THEN endof
      MD-EFFECT-DEPTH of DEPTH-REASON$ endof
      MD-RECURSE-SIG  of s" recursion needs a declared stack effect" endof
      MD-RECURSE-DOES of s" recurse in a does> clause would call the defining word" endof
      MD-IS-UNKNOWN   of s"  names no word" endof
      MD-LOCAL-ENDED  of s" reads built local" endof
      MD-LOCAL-STRAY  of s"  is a built local outside a `{:` group" endof
      MD-LOCAL-UNBOUND of s" reads built local" endof
      MD-IS-NOT-DEFER of s"  is not a deferred word" endof
      MD-IS-MISSING   of s" missing target word after is" endof
      MD-TYPE-FORM    of s" [type] names no type storage holds" endof
      MD-STORED-LINEAR of s" a cast's stored type would hold a linear value" endof
      MD-STORED-CLASS of s" a cast's stored type would hand out an atom or a width" endof
      MD-IS-STORED    of s" the callable takes only a cast's stored types where the cell takes any type" endof
      MD-DIV-ZERO     of s" zero divisor: a literal 0 divisor has no quotient" endof
      MD-THROW-ZERO   of s" zero throw code: 0 means no error, so `0 throw` always throws E-THROW-ZERO" endof
      MD-LINEAR-DROP  of s" a linear value is discarded here" endof
      MD-LINEAR-COPY  of s" a linear value is copied here" endof
      MD-LINEAR-TAINT of s" a value copied while its type was open turned out linear" endof
      MD-LINEAR-EFFECT of s" the called word copies or drops a value of a type that is linear here" endof
      MD-LINEAR-MEMORY of s" a value that may be linear cannot go through memory" endof
      MD-MEMORY-ARG   of s" a type argument of this value may be linear or exclusive, and memory holds neither" endof
      MD-MEMORY-EMPTY of s" a value of width 0 has nothing memory could hold" endof
      MD-LOOP-DEPTH   of s" the loop body changes the stack's depth" endof
      MD-IF-DEPTH     of s" the if arm changes the stack's depth" endof
      MD-ARMS-DEPTH   of s" the if and else arms leave different stack depths" endof
      MD-LOCAL-ESCAPE of s" a quotation that reads a local would outlive it: stored, kept, returned or held past the local's block" endof
      MD-BINDER-PLACE of s" a callback binder would sit outside a scope operation's callback input in the word's effect" endof
      MD-DEPTH-WIDE   of s" cannot count" endof
      MD-LOOP-RDEPTH  of s" the loop body changes the return stack's depth" endof
      MD-LOOP-CARRIED of s" the loop body leaves a carried cell different from the one it entered with" endof
      MD-LOOP-NARROWED of s" this pass narrows a quotation the body ran on any stack" endof
      MD-INPUT-NARROWED of s" the body narrows an input quotation after running it on any stack" endof
      MD-NO-LOOP      of s" no loop is open here" endof
      MD-NO-OUTER-LOOP of s" no outer loop is open here" endof
      MD-CLOSES-NOTHING of s" this word closes nothing open here" endof
      MD-NO-CASE      of s" no case is open here" endof
      MD-ELSE-TWICE   of s" this if already has an else" endof
      MD-EXIT-LOOP    of s" exit inside a loop needs unloop first" endof
      MD-UNLOOP-JOIN  of s" the paths that meet here have unlooped different loops" endof
      MD-CF-OPEN      of s" a control structure is still open at the end of the definition" endof
      MD-DOES-UNMADE  of s" this definition reaches `does>` without creating a word" endof
      MD-FINALLY-CLEANUP of s" the cleanup `finally` runs must take and leave nothing" endof
      MD-CALLABLE-NARROW of s" the callable works on fewer types than the cell or defer it goes into holds" endof
      MD-RECORD-AT-WIDE of s" `record-at` steps by the record's declared width, and this instantiation is wider" endof
      MD-MEMBER-TOP of s" at top level the word runs with its declared layout, and the receiver's instance moves this member" endof
      MD-CATCH-TOP  of s" at top level a throw gives back the stack the caught body took, and this body leaves another" endof
      MD-CATCH-LINEAR of s" a throw out of the caught body loses the cells it took, and this body changes the stack" endof
      MD-ONE-CELL   of s"  is one cell" endof
      MD-OFF-STEP   of s"  is stepped by an offset" endof
      MD-MOVED-LINEAR of s"  is moved by stack words that may copy or drop it" endof
      MD-FACT-UNSTATED of s" this quotation does more than the type that keeps it states" endof
      MD-FACT-INPUT of s" the type that keeps this quotation hands it a quotation its input type does not allow" endof
      MD-FACT-PASSED of s" the type this quotation is passed as hands it a quotation its input type does not allow" endof
      MD-CONSTRUCT-WIDE of s" the variant is wider than the declared widest variant once its type argument is filled in" endof
      MD-C2-BINDER    of s" the callback's binder does not fit this entry's scope and record" endof
      MD-C2-RECORDS-BINDER of s" the callback's binder does not fit this entry's scope and record" endof
      MD-C2-CHILD-BINDER of s" the callback's binder does not fit the scope this entry lends inside" endof
      MD-C2-FIELD-BINDER of s" the callback's binder does not fit the scope and field region this entry lends" endof
      MD-C2-ROOT-BINDER of s" the callback's binder does not fit the scope and region this entry opens" endof
      MD-C2-ROOT-INIT of s" the callback takes a `C2-MEM:root<a>` view, and the view given has another region" endof
      MD-FIELD-NAME   of s" `C2-MEM:WITH-FIELD` names the field it lends, and the definition ends first" endof
      MD-C2-FIELD-UNKNOWN of s" the record has no field of this name" endof
      MD-CALLBACK-NOT-GENERIC of s" the callback must work for every scope, and this one names an outer scope" endof
      MD-SCOPED-TICK  of s"  is a scoped entry, which only a direct call may reach" endof
      s" render: a reason with no message" EXIT-STATUS:INTERNAL die
   endcase ;

\ A reason's label: a few words under its token.
: MD-LABEL$ ( n -- str )
   case
      MD-SCOPE-STORE  of s" scoped value in memory" endof
      MD-LINEAR-MEMORY of s" linear value in memory" endof
      MD-MEMORY-ARG   of s" type argument may be linear" endof
      MD-MEMORY-EMPTY of s" zero-width value" endof
      MD-FAM-UNKNOWN  of s" names no visible family" endof
      MD-FAM-AMBIG    of s" names a family two used packages make public" endof
      MD-FAM-KIND     of s" family of another kind" endof
      MD-SCRUT        of s" no matchable value on top" endof
      MD-FAM-MISMATCH of s" top value of another family" endof
      MD-VAR-UNKNOWN  of s" not a variant of the family" endof
      MD-VAR-DUP      of s" variant already has an arm" endof
      MD-MISSING-OF   of s" in place of `OF`" endof
      MD-NONEXH       of s" some variants have no arm" endof
      MD-STRAY        of s" innermost is no `MATCH`" endof
      MD-TRUNC        of s" ends with a `MATCH` open" endof
      MD-QUOT         of s" inside a quotation" endof
      MD-OPEN-ARGS    of s" arguments not concrete" endof
      MD-JOIN         of s" arms leave different types" endof
      MD-CON-FAM      of s" no family of this package" endof
      MD-CON-KIND     of s" family of another kind" endof
      MD-CON-VAR      of s" not a variant of the family" endof
      MD-CON-TRUNC    of s" ends before both operands" endof
      MD-DIVBAR       of s" not reached by every thread" endof
      MD-EXEC-OPAQUE  of s" runs an xt of unknown type" endof
      MD-CATCH-OPAQUE of s" catches an xt of unknown type" endof
      MD-RIGID-REGION of s" value from another allocation" endof
      MD-RIGID-EXTENT of s" value of another extent" endof
      MD-RIGID-GEN    of s" generation from before a mutation" endof
      MD-RIGID-XDOM   of s" identity of another domain" endof
      MD-RAW-PTR      of s" pointer through an untyped cell" endof
      MD-RAW-FAMILY   of s" nominal value through an untyped cell" endof
      MD-RAW-FIELD    of s" field of an undeclared cell" endof
      MD-UNDERFLOW    of TOP-STEP @ IF s" reaches below the stack" ELSE s" reaches below the inputs" THEN endof
      MD-NULL-PTR     of s" reaches through `NULL-PTR`" endof
      MD-DBASE-PTR    of s" reaches through a base address" endof
      MD-RAW-EXEC     of s" code through an untyped cell" endof
      MD-STALE-READ   of s" cell stale after `catch`" endof
      MD-SCOPE-KIND   of s" scope used as a value type" endof
      MD-C2-COPY      of s" second reference made" endof
      MD-C2-DROP      of s" only reference dropped" endof
      MD-C2-ESCAPE    of s" result outlives its scope" endof
      MD-EFFECT-DEPTH of s" too many nested levels" endof
      MD-TOP-UNKNOWN  of s" cell from before checking" endof
      MD-TAIL-ESCAPE  of s" row escapes the callable" endof
      MD-LIT-FIT      of s" out of the type's range" endof
      MD-ROW-CYCLE    of s" row would contain itself" endof
      MD-IS-UNKNOWN   of s" not found" endof
      MD-LOCAL-ENDED  of s" local past its block" endof
      MD-LOCAL-STRAY  of s" outside a `{:` group" endof
      MD-LOCAL-UNBOUND of s" no group bound it before" endof
      MD-IS-NOT-DEFER of s" declared without `defer`" endof
      MD-IS-MISSING   of s" no name after it" endof
      MD-TYPE-FORM    of s" needs a closed storable type" endof
      MD-STORED-LINEAR of s" linear type for a stored variable" endof
      MD-STORED-CLASS of s" atom or width for a stored variable" endof
      MD-IS-STORED    of s" takes only storable types" endof
      MD-DIV-ZERO     of s" divides by literal 0" endof
      MD-THROW-ZERO   of s" throws code 0" endof
      MD-FINALLY-CLEANUP of s" cleanup not stack-neutral" endof
      MD-CALLABLE-NARROW of s" callable narrower than the cell" endof
      MD-RECORD-AT-WIDE of s" wide instantiation" endof
      MD-MEMBER-TOP of s" runs with its declared layout" endof
      MD-CATCH-TOP  of s" changes the stack at top level" endof
      MD-CATCH-LINEAR of s" may lose a linear value" endof
      MD-ONE-CELL   of s" takes one cell" endof
      MD-OFF-STEP   of s" steps by an offset" endof
      MD-MOVED-LINEAR of s" may copy a linear value" endof
      MD-FACT-UNSTATED of s" kept here" endof
      MD-FACT-INPUT of s" kept here" endof
      MD-FACT-PASSED of s" passed here" endof
      MD-CONSTRUCT-WIDE of s" variant wider than declared" endof
      MD-LINEAR-DROP  of s" linear value dropped" endof
      MD-LINEAR-COPY  of s" linear value copied" endof
      MD-LINEAR-TAINT of s" copied before its type was known" endof
      MD-LINEAR-EFFECT of s" binds a linear type" endof
      MD-C2-BINDER    of s" binder does not fit this entry" endof
      MD-C2-RECORDS-BINDER of s" binder does not fit this entry" endof
      MD-C2-CHILD-BINDER of s" binder does not fit this entry" endof
      MD-C2-FIELD-BINDER of s" binder does not fit this entry" endof
      MD-C2-ROOT-BINDER of s" binder does not fit this entry" endof
      MD-C2-ROOT-INIT of s" view not lent by `WITH-MUT`" endof
      MD-FIELD-NAME   of s" no field name" endof
      MD-CALLBACK-NOT-GENERIC of s" callback bound to an outer scope" endof
      MD-SCOPED-TICK  of s" scoped entry" endof
      MD-C2-INIT-RECORD of s" not an initializable record" endof
      MD-C2-FIELD-UNKNOWN of s" no such field" endof
      MD-LOOP-DEPTH   of s" depth unlike the loop's start" endof
      MD-LOOP-RDEPTH  of s" return depth unlike the start" endof
      MD-LOOP-CARRIED of s" carried cell changed" endof
      MD-LOOP-NARROWED of s" the next pass runs the narrower quotation" endof
      MD-INPUT-NARROWED of s" callers would pass the narrower quotation" endof
      MD-NO-LOOP      of s" no open `do` in this body" endof
      MD-NO-OUTER-LOOP of s" fewer than two loops open" endof
      MD-CLOSES-NOTHING of s" does not close the innermost" endof
      MD-NO-CASE      of s" innermost is no `case`" endof
      MD-ELSE-TWICE   of s" second `else`" endof
      MD-EXIT-LOOP    of s" loop still open" endof
      MD-UNLOOP-JOIN  of s" paths disagree on `unloop`" endof
      MD-CF-OPEN      of s" ends with a structure open" endof
      MD-DOES-UNMADE  of s" `does>` needs a word this definition creates" endof
      MD-RECURSE-SIG  of s" no effect to call" endof
      MD-RECURSE-DOES of s" inside a `does>` clause" endof
      MD-IF-DEPTH     of s" arm ends at another depth" endof
      MD-ARMS-DEPTH   of s" arms end at other depths" endof
      MD-LOCAL-ESCAPE of s" quotation outlives its locals" endof
      MD-BINDER-PLACE of s" binder outside a callback" endof
      MD-DEPTH-WIDE   of s" row with a wide or open value" endof
      s" render: a reason with no label" EXIT-STATUS:INTERNAL die
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

\ The ended local's index, after the reason's `reads built local`.
: LOCAL-ENDED-COUNTS ( -- )
   s"  " EMIT-STR  MDIAG-LOCAL @ EMIT-U
   s" , whose `{:` block has ended" EMIT-STR ;

\ The unbound local's index, after the reason's `reads built local`.
: LOCAL-UNBOUND-COUNTS ( -- )
   s"  " EMIT-STR  MDIAG-LOCAL @ EMIT-N
   s" , which no `{:` group bound before it" EMIT-STR ;

\ The call a literal 0 reached, as written, and the input it reached, after the
\ reason's prose: `NAME` divides by its top input, or its input J below the top.
: ZERO-COUNTS ( -- )
   s" ; `" EMIT-STR  FAILTK FAILTU @ EMIT-STR
   MDIAG @ MD-DIV-ZERO = IF s" ` divides by its " ELSE s" ` throws its " THEN EMIT-STR
   MDIAG-ZIN @ 0= IF s" top input" EMIT-STR EXIT THEN
   s" input " EMIT-STR  MDIAG-ZIN @ EMIT-U  s"  below the top" EMIT-STR ;

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

\ The declared type a one-cell variable met, a hidden cell named by its whole
\ type.
: CELL-TYPE ( -- n )
   MDIAG-CELL @ dup HIDDEN-PARAM? IF MK-LOGICAL THEN ;

\ The word whose effect made the one-cell or stepped variable, 0 for none known.
\ A deferred verdict prints after SIG-EFF-DROP dropped the checked definition's
\ own records, so there it knows none.
: CELL-VAR-SYM ( -- n )
   MDIAG-CELL-VAR @ TVO@ {: org:n :}
   org 0=  DVERD @ VERDICT-DEFERRED =  or IF 0 EXIT THEN
   org 1 - E-PTR ER.SYM @ ;

\ The variable and that word, before the reason's prose.
: ONE-CELL-LEAD ( -- )
   AT-PIN  s" `" EMIT-STR  MDIAG-CELL-VAR @ MK-VAR REND-TYPE  s" `" EMIT-STR  ;AT-PIN
   CELL-VAR-SYM {: sym:n :}
   sym 0= IF EXIT THEN
   s"  in `" EMIT-STR  sym SYM-NAME$ EMIT-STR  s" `'s effect" EMIT-STR ;

\ The declared type a one-cell variable met, after the reason's prose: its
\ cells, or that it is a declared type where it fits one cell.
: ONE-CELL-COUNTS ( -- )
   AT-PIN
   CELL-TYPE {: t:n :}
   s" ; `" EMIT-STR  t REND-TYPE
   t T-WIDTH 1 > IF
      s" ` is " EMIT-STR  t T-WIDTH EMIT-U  s"  cells" EMIT-STR
   ELSE
      s" ` is a declared type, not a plain cell" EMIT-STR
   THEN
   ;AT-PIN ;

: OFF-STEP-COUNTS ( -- )
   AT-PIN  s" ; `" EMIT-STR  CELL-TYPE REND-TYPE  s" ` is not a plain cell type" EMIT-STR  ;AT-PIN ;

: MOVED-LIN-COUNTS ( -- )
   AT-PIN  s" ; `" EMIT-STR  CELL-TYPE REND-TYPE  s" ` may hold a linear value" EMIT-STR  ;AT-PIN ;


\ What a reason carries after its prose, in text and JSON alike.
: MDIAG-COUNTS ( -- )
   MDIAG @ MD-TAIL-ESCAPE = IF s" ; held row " EMIT-STR
      TCAP-HELD$ EMIT-STR EXIT THEN
   MDIAG @ MD-LIT-FIT = IF LIT-FIT-COUNTS EXIT THEN
   MDIAG @ MD-LOCAL-ENDED = IF LOCAL-ENDED-COUNTS EXIT THEN
   MDIAG @ MD-LOCAL-UNBOUND = IF LOCAL-UNBOUND-COUNTS EXIT THEN
   MDIAG @ MD-DIV-ZERO =  MDIAG @ MD-THROW-ZERO = or IF ZERO-COUNTS EXIT THEN
   MDIAG @ MD-DEPTH-WIDE = IF DEPTH-WIDE-COUNTS EXIT THEN
   MDIAG @ MD-ONE-CELL = IF ONE-CELL-COUNTS EXIT THEN
   MDIAG @ MD-OFF-STEP = IF OFF-STEP-COUNTS EXIT THEN
   MDIAG @ MD-MOVED-LINEAR =  MDIAG @ MD-CATCH-LINEAR = or IF MOVED-LIN-COUNTS EXIT THEN
   MDIAG @ MD-UNDERFLOW = IF MDIAG-UF-COUNTS EXIT THEN
   MDIAG @ MD-EFFECT-DEPTH = IF DEPTH-COUNTS THEN ;


: IMM-CODE$ ( -- str )
   s" E-UNMODELED-IMMEDIATE" ;



: LOCALBAD-CODE$ ( -- str )
   s" E-BAD-LOCAL-SHAPE" ;


\ A locals group token that names no local states why in the message.
: LOCALBAD-GROUP? ( -- bool )
   LOCALBAD-KIND @ LOCALBAD-SCOPE <> ;

: LOCALBAD-REASON$ ( -- str )
   LOCALBAD-KIND @ LOCALBAD-DASH = IF s" a locals group names only locals; the signature declares the outputs" EXIT THEN
   s" a locals group names only locals; Habu has no uninitialised locals" ;

\ A definition's signature, a local's type or a stored signature that does not
\ parse; SGBAD's state from the failed parse names the kind.
: SGBAD-CODE$ ( -- str )
   SGBAD-AMBIG? IF s" E-USING-AMBIGUOUS" EXIT THEN
   SGBAD-UNKNOWN? IF s" E-UNKNOWN-SIGNATURE-TYPE" EXIT THEN
   SGBAD-BAREPTR? IF s" E-BARE-PTR-SIGNATURE" EXIT THEN
   SGBAD-ARITY? IF s" E-WRONG-ARITY" EXIT THEN
   s" E-BAD-SIGNATURE" ;

\ The code, message, label, notes and help all name the one rejection REFUSAL@
\ chose.
: DCODE ( -- str )
   REFUSAL@ {: k:n :}
   k RF-IMM = IF IMM-CODE$ EXIT THEN
   k RF-UNDEF = IF s" E-UNDEFINED" EXIT THEN
   k RF-KEYWORD = IF s" E-UNDEFINED" EXIT THEN
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
   k RF-RETURN = IF s" E-RETURN-STACK" EXIT THEN
   s" render: a rejection with no code" EXIT-STATUS:INTERNAL die ;
: NM-SPAN ( -- str )
   NMOFF @ TADDR NMU @ ;
\ The return rows `return_stack` names, expected then actual: a rejected read's
\ and the row it met (RSR-EXP), a quotation literal's own when its seal refused
\ it (RSBAD 3, 4), else the definition's.
: RS-ROWS ( -- n n )
   RSR-EXP @ 0 <> IF RSR-EXP @ RSR-ACT @ EXIT THEN
   RSBAD @ 3 >= IF QRS-EXP @ QRS-ACT @ EXIT THEN
   SGHASR @ IF SGROUT @ ELSE RBROW @ THEN  RCUR @ ;

: RS-REASON$ ( -- str )
   RSBAD @ 2 = IF s" return stack: this reads below the return-stack cells pushed here" EXIT THEN
   RSBAD @ 3 = IF s" return stack: the quotation leaves its return row unbalanced" EXIT THEN
   RSBAD @ 4 = IF s" return stack: the quotation reads below the return-stack cells it pushed" EXIT THEN
   s" return stack: the return row does not balance at exit" ;

: RS-LABEL$ ( -- str )
   RSBAD @ 2 = IF s" reads below the definition's return cells" EXIT THEN
   RSBAD @ 3 = IF s" quotation's return row unbalanced" EXIT THEN
   RSBAD @ 4 = IF s" reads below the quotation's return cells" EXIT THEN
   s" return row unbalanced" ;

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
\ holds, as a checked string's, is the empty token at the text's end where that
\ is held, else FAILTK, the copy of its spelling.
: DIAG-TOKEN ( -- str )
   TOP-STEP @ IF TOK-SPELL-A @ TOK-SPELL-U @ EXIT THEN
   TBASE@ FAILB @ + {: p:^ :}
   FAILE @ FAILB @ - {: u:n :}
   FAILE @ TBLEN @ <=  p u REJECTION:HELD?  or IF p u EXIT THEN
   TBASE@ TBLEN @ + {: e:^ :}
   e 0 REJECTION:HELD? IF e 0 EXIT THEN
   FAILTK FAILTU @ ;
\ The token a local's annotation verdict rejects, in the local's token a u.
: LOC-REJECTED$ ( str -- str ) {: a:^ u:n :}
   a  a u LCOLON +  SGBAD-LOCAL @ +  SGBAD-U @ ;
\ The span the record places: a local's annotation verdict pins the local's
\ token, `name:type`, and places the token it rejects.
: DIAG-PLACE ( -- str )
   DIAG-TOKEN
   DKIND RF-SIG =  SGBAD-LOCAL @ 0 <>  and IF LOC-REJECTED$ THEN ;

: PIN-TOK$ ( -- str ) FAILTK FAILTU @ ;
: TOK-SPAN ( -- ) PIN-TOK$ CODE-SPAN ;
\ The pin is the closer of a body that walked no token.
: CLOSER-PINNED? ( -- bool )
   PIN-TOK$ s" ;" CORE-STR=  PIN-TOK$ s" does>" CORE-STR=  or ;

EXPORT CHECKER-RESOLVE:TOP-KEYWORD-ID
EXPORT CHECKER-RESOLVE:TOP-KEYWORD?
EXPORT CHECKER-RESOLVE:TOP-PARSER?
s" package" TOP-KEYWORD-ID constant KW-PACKAGE
s" public" TOP-KEYWORD-ID constant KW-PUBLIC
s" private" TOP-KEYWORD-ID constant KW-PRIVATE
s" global" TOP-KEYWORD-ID constant KW-GLOBAL
s" ;package" TOP-KEYWORD-ID constant KW-END-PACKAGE
s" host" TOP-KEYWORD-ID constant KW-HOST
s" target" TOP-KEYWORD-ID constant KW-TARGET
s" seal-package" TOP-KEYWORD-ID constant KW-SEAL
s" using" TOP-KEYWORD-ID constant KW-USING
s" ;using" TOP-KEYWORD-ID constant KW-END-USING
s" export" TOP-KEYWORD-ID constant KW-EXPORT
s" split-str" TOP-KEYWORD-ID constant KW-SPLIT-STR
s" :" TOP-KEYWORD-ID constant KW-COLON
s" kernel:" TOP-KEYWORD-ID constant KW-KERNEL
s" trusted:" TOP-KEYWORD-ID constant KW-TRUSTED
s" create" TOP-KEYWORD-ID constant KW-CREATE
s" variable" TOP-KEYWORD-ID constant KW-VARIABLE
s" constant" TOP-KEYWORD-ID constant KW-CONSTANT
s" defer" TOP-KEYWORD-ID constant KW-DEFER
s" cast:" TOP-KEYWORD-ID constant KW-CAST
s" linear:" TOP-KEYWORD-ID constant KW-LINEAR
s" immediate" TOP-KEYWORD-ID constant KW-IMMEDIATE

: STATE-SWITCH? ( str -- bool ) {: t:^ tu:n :}
   t tu s" [" CORE-STR=CI  t tu s" ]" CORE-STR=CI or ;

: LOCALS-END? ( str -- bool ) s" :}" CORE-STR= ;

: LOCALS-END-HELP ( -- )
   s" a locals list begins with `{:` and this `:}` closes none: delete it, or write `{:` where the list begins"
   EMIT-STR ;

: TRIM-SPACE ( -- )
   TEXT-LEN @ 0= IF EXIT THEN
   TEXT TEXT-LEN @ 1 - + c@ 32 = IF TEXT-LEN @ 1 - TEXT-LEN ! THEN ;

\ Row s as DROW writes it, or the word e for an empty row.
: ROW-OR ( n str -- ) {: s:n ea:^ eu:n :}
   TEXT-LEN @ {: at:n :}
   s DROW
   TEXT-LEN @ at = IF ea eu EMIT-STR EXIT THEN
   TRIM-SPACE ;
: ROW-WORD ( n -- ) s" nothing" ROW-OR ;
: ROW-NOTE ( n -- ) s" (nothing)" ROW-OR ;

\ Primitive row i's inputs, its variables named after the record's; the names
\ are given back, so every row starts from the same ones.
: PE-INPUTS ( n -- ) {: i:n :}
   NLET @ RLETS @ RNAMED @ {: nlet:n lets:n named:n :}
   i PE-EFF@ E-PTR {: h:^ :}
   1 RFRESH !
   h E-INST-RESET  h E-DIN@ E-INST ROW-WORD
   0 RFRESH !
   nlet NLET !  lets RLETS !  named RNAMED ! ;

: SYM-ROW? ( n n -- bool ) {: i:n sym:n :}
   i PE-TAKES?  i PE-SYM@ sym =  and ;
: SYM-ROWS ( n -- n ) {: sym:n :}
   sym PRIM-FIRST-IDX dup 0= IF EXIT THEN
   0 swap 1 - BEGIN dup #PE @ < WHILE
      dup sym SYM-ROW? IF swap 1 + swap THEN
      1 +
   REPEAT drop ;

\ The start and length in TEXT of each row text TAKES-ROW kept.
SCRATCH{
PTR-VARIABLE TAKEN-P   variable TAKEN-CAP   variable TAKEN-N
}SCRATCH
: TAKEN-AT ( n -- ^n ) 2 * cells TAKEN-P @ + ;
: TAKEN$ ( n -- str ) TAKEN-AT {: row:^ :}  TEXT row @ +  row cell+ @ ;
: TAKEN? ( n n -- bool ) {: at:n len:n :}
   0 BEGIN dup TAKEN-N @ < WHILE
      dup TAKEN$  TEXT at + len  CORE-STR= IF drop RES-TRUE EXIT THEN
      1 +
   REPEAT drop RES-FALSE ;
: TAKEN+ ( n n -- ) {: at:n len:n :}
   TAKEN-P TAKEN-CAP TAKEN-N @ 1 + 2 ARENA-ROWS-ENSURE
   TAKEN-N @ TAKEN-AT {: row:^ :}
   at row !  len row cell+ !
   TAKEN-N @ 1 + TAKEN-N ! ;

\ Row i's inputs after ` or ` unless no row was written, taken back when an
\ earlier row wrote the same text.
: TAKES-ROW ( n -- ) {: i:n :}
   TEXT-LEN @ {: at:n :}
   TAKEN-N @ 0 <> IF s"  or " EMIT-STR THEN
   TEXT-LEN @ {: from:n :}
   i PE-INPUTS
   TEXT-LEN @ from - {: len:n :}
   from len TAKEN? IF at TEXT-LEN ! EXIT THEN
   from len TAKEN+ ;

\ Names the variables of the expected row, then of the found one, writing
\ nothing, so a form list takes none of their letters.
: RECORD-NAMES ( -- )
   TEXT-LEN @ RQM @ {: at:n qm:n :}
   DEXP @ 0 <> IF DEXP @ DROW THEN
   DACT @ 0 <> IF DACT @ DROW THEN
   at TEXT-LEN !  qm RQM ! ;

\ What the pinned token takes: the expected row, or, when the token's word has
\ more than one row, each row's inputs in table order.
: TAKES-WORD ( -- )
   PIN-TOK$ CHECKER-FIND-ACTIVE-SYM {: sym:n :}
   sym SYM-ROWS 2 < IF DEXP @ ROW-WORD EXIT THEN
   RECORD-NAMES
   0 TAKEN-N !
   sym PRIM-FIRST-IDX 1 - BEGIN dup #PE @ < WHILE
      dup sym SYM-ROW? IF dup TAKES-ROW THEN
      1 +
   REPEAT drop ;

\ A signature as written, without its parentheses.
: SIG-BARE ( str -- str )
   SIG-TRIM {: a:^ u:n :}
   u 2 < IF a u EXIT THEN
   a c@ 40 <>  a u + 1 - c@ 41 <>  or IF a u EXIT THEN
   a 1 + u 2 - SIG-TRIM ;

: INFERRED ( -- ) INFERRED-EFFECT TRIM-SPACE ;

\ The definer of the does> clause being reported, as written: DOES-NAME! sets it.
SCRATCH{
PTR-VARIABLE DOES-NA   variable DOES-NU
}SCRATCH

\ The body whose end is checked: a colon definition's name, or a does>
\ clause's definer, as written.
: END-SUBJECT ( -- )
   CLAUSE-WALK @ 0= IF NM-SPAN EMIT-STR EXIT THEN
   s" the `does>` clause of " EMIT-STR
   DOES-NA @ DOES-NU @ EMIT-STR ;
: END-MESSAGE ( -- )
   END-SUBJECT  s"  ends with " EMIT-STR  DACT @ ROW-WORD
   s"  on the stack; its signature promises " EMIT-STR  DEXP @ ROW-WORD ;
: MISMATCH-MESSAGE ( -- )
   DEND @ 0 <> IF END-MESSAGE EXIT THEN
   TOK-SPAN  s"  takes " EMIT-STR  TAKES-WORD
   s" ; the stack holds " EMIT-STR  DACT @ ROW-WORD ;

: LOCAL-MESSAGE ( -- )
   LOCALBAD-GROUP? IF LOCALBAD-REASON$ EMIT-STR EXIT THEN
   s" a local cannot be bound inside a quotation, or bound on a dead path" EMIT-STR ;

\ Names the local l lu whose annotation the verdict rejects.
: LOCAL-PHRASE ( str -- ) {: l:^ lu:n :}
   lu 0= IF EXIT THEN
   s"  in the annotation of local " EMIT-STR  l lu CODE-SPAN ;
\ The rejected token t tu, then the local whose annotation holds it.
: SUBJECT-SPAN ( str str -- )   2swap CODE-SPAN LOCAL-PHRASE ;
: ARITY-MESSAGE ( str str -- )
   s" type family " EMIT-STR  SUBJECT-SPAN
   s"  takes " EMIT-STR  SGBAD-AR-DECL @ EMIT-U
   SGBAD-AR-DECL @ 1 = IF s"  type argument" ELSE s"  type arguments" THEN EMIT-STR
   s" , not " EMIT-STR  SGBAD-AR-GOT @ EMIT-U ;
\ What ended: the signature, or the annotation of the local l lu.
: SIG-WHOLE ( str -- ) {: l:^ lu:n :}
   lu 0= IF s" the signature" EMIT-STR EXIT THEN
   s" the annotation of local " EMIT-STR  l lu CODE-SPAN ;
\ A signature, or the annotation of the local l lu, that ended where the parser
\ expected a word.
: SIG-END-MESSAGE ( str -- ) {: l:^ lu:n :}
   SGBAD-EXP-U @ 0= IF l lu SIG-WHOLE  s"  ends before it is complete" EMIT-STR EXIT THEN
   s" expected " EMIT-STR  SGBAD-EXP-A @ SGBAD-EXP-U @ CODE-SPAN
   s" , found the end of " EMIT-STR  l lu SIG-WHOLE ;
\ A local's annotation is pinned as the local's token, `name:type`: the token
\ its verdict rejects and its name.
: PIN-LOCAL$ ( -- str str ) PIN-TOK$ {: a:^ u:n :}
   a u LOC-REJECTED$  a  a u LCOLON ;
\ The token a signature verdict rejects, and the local whose annotation that
\ is, empty in a signature.
: SIG-SUBJECT$ ( -- str str )
   SGBAD-LOCAL @ IF PIN-LOCAL$ EXIT THEN
   PIN-TOK$ over 0 ;
: SIG-MESSAGE ( -- )
   SIG-SUBJECT$ {: t:^ tu:n l:^ lu:n :}
   SGBAD-AMBIG? IF
      s" type family " EMIT-STR  t tu l lu SUBJECT-SPAN
      s"  is public in more than one used package" EMIT-STR EXIT
   THEN
   SGBAD-UNKNOWN? IF
      s" unknown type " EMIT-STR  t tu l lu SUBJECT-SPAN
      lu 0= IF s"  in signature" EMIT-STR THEN EXIT
   THEN
   SGBAD-SCHEME? IF s" polymorphic type " EMIT-STR  t tu l lu SUBJECT-SPAN EXIT THEN
   SGBAD-BAREPTR? IF s" `^` needs an element type" EMIT-STR  l lu LOCAL-PHRASE EXIT THEN
   SGBAD-ARITY? IF t tu l lu ARITY-MESSAGE EXIT THEN
   SGBAD-RETURN? IF
      s" return-stack effect " EMIT-STR  lu 0= IF s" in signature " EMIT-STR THEN
      t tu l lu SUBJECT-SPAN EXIT
   THEN
   SGBAD-SIZE? IF BADSIG-REASON  s"  at " EMIT-STR  t tu l lu SUBJECT-SPAN EXIT THEN
   tu 0= IF l lu SIG-END-MESSAGE EXIT THEN
   lu 0= IF s" bad signature at " ELSE s" unexpected " THEN EMIT-STR
   t tu l lu SUBJECT-SPAN ;

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
: VAR-SPAN ( n -- ) {: q:n :}
   96 EMIT1  q EMIT1  96 EMIT1 ;
: NP-ROW ( -- )
   NPBAD-Q1 @ IF s" row " EMIT-STR  NPBAD-Q1 @ VAR-SPAN EXIT THEN
   s" the unnamed row" EMIT-STR ;
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
      8 of NP-ROWS endof
      7 of NP-ROW-FIXED endof
      NPBAD-Q1 @ NP-VAR  s"  is minted into " EMIT-STR  NP-FAM-REND
      s"  but is unbound in the inputs; a checked definition cannot mint a phantom of input-unrelated type" EMIT-STR
   endcase ;
: NP-ROWS-LABEL ( -- )
   s" unifies " EMIT-STR  NP-ROW  s"  and " EMIT-STR
   NPBAD-Q2 @ IF s" row " EMIT-STR  NPBAD-Q2 @ VAR-SPAN ELSE s" the unnamed row" EMIT-STR THEN
   s"  here" EMIT-STR ;
: NP-LABEL ( -- )
   NPBAD-KIND @ case
      0 of s" makes " EMIT-STR  NPBAD-Q1 @ VAR-SPAN  32 EMIT1  NP-FAM-REND
         s"  here" EMIT-STR endof
      1 of s" unifies " EMIT-STR  NPBAD-Q1 @ VAR-SPAN  s"  and " EMIT-STR
         NPBAD-Q2 @ VAR-SPAN  s"  here" EMIT-STR endof
      6 of NP-ROWS-LABEL endof
      8 of NP-ROWS-LABEL endof
      7 of s" gives " EMIT-STR  NP-ROW  s"  fixed cells here" EMIT-STR endof
      2 of s" mints " EMIT-STR  NPBAD-Q1 @ VAR-SPAN  s"  here" EMIT-STR endof
      s" restricts " EMIT-STR  NPBAD-Q1 @ VAR-SPAN  s"  here" EMIT-STR
   endcase ;

\ Whether the end check read a stale row: the body returns a tail `catch` left
\ stale. A step that takes a cell from a stale row reads a cell.
: STALE-ROW-READ? ( -- bool )
   MDIAG @ MD-STALE-READ =  DEND @ 0 <>  and  MDIAG-CELL @ STALE-ROW?  and ;

: REASON-MESSAGE ( -- )
   STALE-ROW-READ? IF
      s" stale row: " EMIT-STR  END-SUBJECT
      s"  returns cells `catch` left stale: a throw path of the caught body may have overwritten them" EMIT-STR
      EXIT
   THEN
   MDIAG @ MD-SCOPED-TICK =  MDIAG @ MD-IS-UNKNOWN = or  MDIAG @ MD-IS-NOT-DEFER = or
   MDIAG @ MD-LOCAL-STRAY = or  MDIAG @ MD-ROW-CYCLE = or
   IF TOK-SPAN THEN
   \ A built local's read leads with the type its `{:` group gave it, which a
   \ read of an index no group declared lacks.
   MDIAG @ MD-LOCAL-ENDED =  MDIAG @ MD-LOCAL-UNBOUND = or
   PIN-TOK$ nip 0 <>  and IF TOK-SPAN 32 EMIT1 THEN
   MDIAG @ MD-ONE-CELL =  MDIAG @ MD-OFF-STEP = or  MDIAG @ MD-MOVED-LINEAR = or
   IF ONE-CELL-LEAD THEN
   MDIAG @ MD-REASON$ EMIT-STR
   MDIAG @ MD-NONEXH = IF MDIAG-MISSING-PROSE THEN
   MDIAG-COUNTS ;

: DEAD-MESSAGE ( -- )
   TOK-SPAN  s"  never runs: " EMIT-STR  DEADTA @ DEADTU @ CODE-SPAN
   s"  before it never returns" EMIT-STR ;

: TOK-MESSAGE ( str -- ) {: a:^ u:n :}
   TOK-SPAN  a u EMIT-STR ;

: UNDEF-MESSAGE ( str -- ) {: t:^ tu:n :}
   t tu LOCALS-END? IF
      t tu CODE-SPAN  s"  closes no locals list" EMIT-STR EXIT
   THEN
   t tu TOP-KEYWORD?  t tu TOP-PARSER?  or IF
      t tu CODE-SPAN  s"  is a top-level keyword, not a word" EMIT-STR EXIT
   THEN
   t tu STATE-SWITCH? IF
      t tu CODE-SPAN
      s"  switches the compiler's state, which a checked body cannot do" EMIT-STR EXIT
   THEN
   s" cannot find word " EMIT-STR  t tu CODE-SPAN ;

\ The message for keyword t tu named where a word is needed.
: KEYWORD-MESSAGE ( str -- )
   {: t:^ tu:n :}
   t tu CODE-SPAN  s"  is a keyword, not a word" EMIT-STR ;

\ The help for keyword t tu named where a word is needed.
: KEYWORD-HELP ( str -- )
   {: t:^ tu:n :}
   s" name a word defined with `:` … `;` that uses " EMIT-STR
   t tu CODE-SPAN ;

\ Why a body does not admit immediate t tu: IMM-READS of its binding.
: IMM-WHY ( str -- n ) {: t:^ tu:n :}
   t tu  t tu CHECKER-FIND-ACTIVE-SYM IMM-READS ;

: IMM-MESSAGE ( str -- ) {: t:^ tu:n :}
   t tu CODE-SPAN
   t tu IMM-WHY {: why:n :}
   why IMM-EFFECT = IF
      s"  runs while the definition compiles, so its effect must be `( -- )`" EMIT-STR EXIT
   THEN
   why IMM-WRITES = IF
      s"  runs while the definition compiles and may define words or add to a definition"
      EMIT-STR EXIT
   THEN
   why IMM-ROOTS = IF
      s"  runs while the definition compiles and may move the source root" EMIT-STR EXIT
   THEN
   s"  reads the source after it and no `parses:` row says how many tokens" EMIT-STR ;

: DIAG-MESSAGE ( -- )
   DKIND case
      RF-MISMATCH of AT-PIN MISMATCH-MESSAGE ;AT-PIN endof
      RF-UNDEF of PIN-TOK$ UNDEF-MESSAGE endof
      RF-KEYWORD of PIN-TOK$ KEYWORD-MESSAGE endof
      RF-QUAL of s" malformed qualified name " EMIT-STR  TOK-SPAN endof
      RF-IMM of PIN-TOK$ IMM-MESSAGE endof
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
      s" render: a rejection with no message" EXIT-STATUS:INTERNAL die
   endcase ;

: END-LABEL ( -- )
   CLOSER-PINNED? IF s" the body ends here" EMIT-STR EXIT THEN
   DACT @ ROW-VALUES 0=  DACT @ ROW-TAIL STALE-ROW? 0=  and IF
      s" the stack is empty after this" EMIT-STR EXIT
   THEN
   s" the stack holds " EMIT-STR  DACT @ ROW-WORD  s"  after this" EMIT-STR ;
: MISMATCH-LABEL ( -- )
   DEND @ 0 <> IF END-LABEL EXIT THEN
   s" takes " EMIT-STR  TAKES-WORD ;

: LOCAL-LABEL ( -- )
   LOCALBAD-GROUP? IF s" not a local name" EMIT-STR EXIT THEN
   s" bound here" EMIT-STR ;

: SIG-LABEL ( -- )
   SGBAD-AMBIG? IF s" ambiguous here" EMIT-STR EXIT THEN
   SGBAD-UNKNOWN? IF s" unknown type" EMIT-STR EXIT THEN
   SGBAD-SCHEME? IF s" a local holds one type" EMIT-STR EXIT THEN
   SGBAD-BAREPTR? IF s" no element type" EMIT-STR EXIT THEN
   SGBAD-ARITY? IF s" wrong number of type arguments" EMIT-STR EXIT THEN
   SGBAD-RETURN? IF s" return-stack effect" EMIT-STR EXIT THEN
   s" here" EMIT-STR ;

: UNDEF-LABEL ( str -- ) {: t:^ tu:n :}
   t tu LOCALS-END? IF s" no `{:` open" EMIT-STR EXIT THEN
   t tu TOP-KEYWORD?  t tu TOP-PARSER?  or IF s" keyword, not a word" EMIT-STR EXIT THEN
   t tu STATE-SWITCH? IF s" state switch" EMIT-STR EXIT THEN
   s" not found" EMIT-STR ;

: DIAG-LABEL ( -- )
   DKIND case
      RF-MISMATCH of AT-PIN MISMATCH-LABEL ;AT-PIN endof
      RF-UNDEF of PIN-TOK$ UNDEF-LABEL endof
      RF-KEYWORD of s" not a word" EMIT-STR endof
      RF-QUAL of s" more than one `:`" EMIT-STR endof
      RF-IMM of s" unmodeled immediate" EMIT-STR endof
      RF-CAP of TOP-STEP @ IF s" not callable from checked code"
         ELSE s" not callable from a checked body" THEN EMIT-STR endof
      RF-UNSAFE of s" not allowed in a checked body" EMIT-STR endof
      RF-LINLOCAL of s" bound here" EMIT-STR endof
      RF-LOCAL of LOCAL-LABEL endof
      RF-SIG of SIG-LABEL endof
      RF-NP of NP-LABEL endof
      RF-DEAD of s" never runs" EMIT-STR endof
      RF-UNCHECKABLE of s" checking stops here" EMIT-STR endof
      RF-REASON of
         STALE-ROW-READ? IF s" stale after `catch`" ELSE MDIAG @ MD-LABEL$ THEN EMIT-STR
      endof
      RF-RETURN of RS-LABEL$ EMIT-STR endof
      s" render: a rejection with no label" EXIT-STATUS:INTERNAL die
   endcase ;

: TEXT-NOTE ( str str -- ) {: ka:^ ku:n va:^ vu:n :}
   ka ku REJECTION:NOTE  va vu SIG-RTRIM EMIT-STR ;
: PAIR-NOTES ( n n -- ) {: e:n a:n :}
   s" expected" REJECTION:NOTE  e ROW-NOTE
   s" found" REJECTION:NOTE  a ROW-NOTE ;

\ Expected takes precedence over actual, so unrelated matched row cells cannot
\ leak into the family; a scalar pair names none.
: TERM-FAM ( n -- n )                    \ layout-family id for one type term, else -1
   T-UNSTALE dup LAYOUT-PARAM? IF PARAM>FAM EXIT THEN
   drop -1 ;
\ DVAR is the SUMV id a `construct family variant` payload mismatch latched, -1
\ for none.
: DIAG-FAM-ID ( n n -- n )
   {: efam:n afam:n :}
   efam 0 >= IF efam EXIT THEN
   afam 0 >= IF afam EXIT THEN
   DVAR @ dup 0 >= IF SUMV-FAM@ THEN ;
EXPORT TFAM:PF-NAME$
EXPORT TFAM:SUMV-PAY-FIELD
EXPORT TFAM:SUMV-PAY-N

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
\ A row that would contain itself (MD-ROW-CYCLE): the row variable and the row
\ holding it, each with its tail's letter, so both name the one row.
: CYCLE-NOTES ( -- )
   AT-PIN  0 RROW-N !  -1 RTAILS !
   s" expected" REJECTION:NOTE  DEXP @ TCAP-ROW  TRIM-SPACE
   s" found" REJECTION:NOTE  DACT @ TCAP-ROW  TRIM-SPACE
   0 RTAILS !  ;AT-PIN ;
\ A narrowed quotation (MD-LOOP-NARROWED, MD-INPUT-NARROWED) differs from
\ what it entered with only in its tails, so both are shown with their letters.
: NARROWED-NOTES ( -- )
   AT-PIN  0 RROW-N !  -1 RTAILS !
   DEXP @ DACT @ PAIR-NOTES
   0 RTAILS !  ;AT-PIN ;
: ROWS-NOTES ( -- )
   MDIAG @ MD-TAIL-ESCAPE = IF ESCAPE-NOTES EXIT THEN
   MDIAG @ MD-ROW-CYCLE = IF CYCLE-NOTES EXIT THEN
   MDIAG @ MD-LOOP-NARROWED =  MDIAG @ MD-INPUT-NARROWED = or IF NARROWED-NOTES EXIT THEN
   AT-PIN
   DEXP @ 0 <> IF DEXP @ DACT @ PAIR-NOTES  FAMILY-NOTES THEN
   DF-ACT @ 0 <>  DEXP @ 0= and IF
      s" found" REJECTION:NOTE  DF-ACT @ REND-TYPE
   THEN
   ;AT-PIN ;

\ The inferred effect as the body's declaration writes it: a does> clause
\ declares its created words' effect, without the data-field cell the CREATE
\ stub pushes on its inputs.
: DECLARABLE ( -- )
   CLAUSE-WALK @ 0= IF INFERRED EXIT THEN
   SGIN @ INFERRED-FROM TRIM-SPACE ;
: EFFECT-NOTES? ( n -- bool ) {: k:n :}
   RECEFF-ON? TOP-STEP @ 0= and 0= IF RES-FALSE EXIT THEN
   k RF-MISMATCH =  k RF-RETURN = or  k RF-NP = or  REFUSAL-UNCHECKABLE? or
   k RF-REASON =  DEXP @ 0 <>  MDIAG @ MD-UNDERFLOW = or  and  or ;
: EFFECT-NOTES ( -- )
   s" declared" REJECTION:NOTE  SGA @ SGU @ SIG-BARE EMIT-STR
   s" inferred" REJECTION:NOTE
   MDIAG @ MD-TAIL-ESCAPE = IF TCAP-INF$ SIG-RTRIM EMIT-STR EXIT THEN
   DECLARABLE ;

\ A rejected read's rows stand at its pin, before the read bound them.
: RS-NOTES ( -- )
   RSR-EXP @ 0= IF RS-ROWS PAIR-NOTES EXIT THEN
   AT-PIN RS-ROWS PAIR-NOTES ;AT-PIN ;

\ The effect an immediate in a body must have, and immediate t tu's own, written
\ in place into the record.
: IMM-NOTES ( str -- ) {: t:^ tu:n :}
   t tu IMM-WHY IMM-EFFECT <> IF EXIT THEN
   s" expected" REJECTION:NOTE  s" ( -- )" EMIT-STR
   t tu CHECKER-FIND-ACTIVE-SYM USIG-NEWEST-VISIBLE {: rec1:n :}
   rec1 0= IF EXIT THEN
   s" found" REJECTION:NOTE  s" ( " EMIT-STR
   RQM @ REJECTION:DEST @ {: qm:n dst:n :}
   rec1 1 - REND-RECORD-EFFECT 2drop
   qm RQM !  dst REJECTION:DEST !
   s"  )" EMIT-STR ;

\ A reason one row of a many-row word gave still names every form the word takes.
: FORMS-NOTE ( -- )
   PIN-TOK$ CHECKER-FIND-ACTIVE-SYM SYM-ROWS 2 < IF EXIT THEN
   s" note" REJECTION:NOTE  TOK-SPAN  s"  takes " EMIT-STR  AT-PIN TAKES-WORD ;AT-PIN ;

: DIAG-NOTES ( -- )
   DKIND {: k:n :}
   k RF-IMM = IF PIN-TOK$ IMM-NOTES THEN
   k ROWS-REFUSAL? IF ROWS-NOTES THEN
   k RF-RETURN = IF RS-NOTES THEN
   k RF-REASON =  MDIAG @ MD-NONEXH = and IF
      s" missing" REJECTION:NOTE  0 0= MDIAG-MISSING-WALK
   THEN
   k RF-REASON =  MDIAG @ MD-FACT-UNSTATED = and  MDIAG-CELL-VAR @ 0 <> and IF
      s" note" REJECTION:NOTE  s" it may run the `" EMIT-STR
      AT-PIN  MDIAG-CELL-VAR @ REND-TYPE  ;AT-PIN
      s" ` the type that keeps it hands it" EMIT-STR
   THEN
   k RF-REASON = IF FORMS-NOTE THEN
   k EFFECT-NOTES? IF EFFECT-NOTES THEN ;

\ The help for a body whose end does not fit its signature: the row it should
\ leave after its last word, or the effect it has when a signature can spell it.
: END-WHERE ( -- )
   CLOSER-PINNED? IF s"  in the body" EMIT-STR EXIT THEN
   s"  after " EMIT-STR  TOK-SPAN ;
: END-HELP ( -- )
   s" leave " EMIT-STR  AT-PIN DEXP @ ROW-WORD ;AT-PIN  END-WHERE
   TEXT-LEN @ {: at:n :}  RQM @ {: qm:n :}  0 RORDINAL !  0 RQM !
   s" , or declare ( " EMIT-STR  DECLARABLE  s"  )" EMIT-STR
   RQM @ RORDINAL @ or 0 <> IF at TEXT-LEN ! THEN
   qm RQM @ or RQM ! ;
\ The stack misses values only where the found row holds fewer values and fewer
\ cells than expected: a narrower value is a wrong one, a wider one fills cells.
: TAKES-HELP ( -- )
   ROWS-DEPTHS
   AT-PIN  DACT @ ROW-VALUES  DEXP @ ROW-VALUES  <  ;AT-PIN
   DSUGA @ DSUGE @ <  and IF
      s" put what " EMIT-STR  TOK-SPAN
      s"  takes on the stack before it, or declare it as an input" EMIT-STR EXIT
   THEN
   s" give " EMIT-STR  TOK-SPAN  32 EMIT1  AT-PIN DEXP @ ROW-WORD ;AT-PIN
   s" : convert or replace the value before it" EMIT-STR ;

\ The package record at dictionary index i whose public wordlist holds the
\ token t tu, else NULL-PTR. A part, named A:B, is left out: no qualifier
\ outside A reaches it.
: PUBLIC-IN ( str n -- ^n ) {: t:^ tu:n i:n :}
   i REC-AT {: rec:^ :}
   rec CK-REC-WID BOOT:NAMESPACE-WL <> IF NULL-PTR EXIT THEN
   rec @ {: wid:n :}
   rec CK-REC-NAME$ CHECKER-COLON-SCAN drop {: colons:n :}
   wid 0=  colons 0 <>  or IF NULL-PTR EXIT THEN
   t tu wid CHECKER-RESOLVE:WL-PROBE NULL-PTR = IF NULL-PTR EXIT THEN
   rec ;
variable HOLDER-N
PTR-VARIABLE HOLDER-FIRST
: HOLDER-NAME ( ^n -- ) CK-REC-NAME$ CODE-SPAN ;
\ Counts the packages whose public wordlist holds t tu, writes their names when
\ list is set.
: HOLDER-SCAN ( str bool -- ) {: t:^ tu:n list:bool :}
   0 HOLDER-N !
   ndict@ 0 ?do
      t tu i PUBLIC-IN {: rec:^ :}
      rec NULL-PTR <> IF
         HOLDER-N @ 0= IF rec HOLDER-FIRST ! THEN
         list IF HOLDER-N @ 0 > IF s" , " EMIT-STR THEN  rec HOLDER-NAME THEN
         HOLDER-N @ 1 + HOLDER-N !
      THEN
   loop ;
: QUALIFIED ( str -- ) {: t:^ tu:n :}
   96 EMIT1  HOLDER-FIRST @ CK-REC-NAME$ EMIT-STR
   58 EMIT1  t tu EMIT-STR  96 EMIT1 ;
\ The help for t tu, which nothing defines and which closes no locals list,
\ used in the body of d du, empty outside one: a body's own name is found only
\ after its `;`. Names fold case.
: UNDEF-HELP ( str str -- )
   {: t:^ tu:n d:^ du:n :}
   t tu TOP-PARSER? IF
      s" write `[" EMIT-STR  t tu EMIT-STR  s" ]` in a body" EMIT-STR EXIT
   THEN
   t tu TOP-KEYWORD? IF
      s" write " EMIT-STR  t tu CODE-SPAN  s"  at top level, outside any definition"
      EMIT-STR EXIT
   THEN
   t tu STATE-SWITCH? IF
      s" compute the value at top level before the definition and name it with `constant`"
      EMIT-STR EXIT
   THEN
   t tu d du CORE-STR=CI IF
      96 EMIT1  t tu EMIT-STR
      s" ` is the word this definition defines: " EMIT-STR
      s" call it from its own body with `recurse`" EMIT-STR EXIT
   THEN
   t tu RES-FALSE HOLDER-SCAN
   HOLDER-N @ 0= IF
      s" define `" EMIT-STR  t tu EMIT-STR
      s" ` before this point, or check its spelling" EMIT-STR EXIT
   THEN
   t tu CODE-SPAN  s"  is public in " EMIT-STR
   HOLDER-N @ 1 = IF
      HOLDER-FIRST @ HOLDER-NAME  s" : write " EMIT-STR  t tu QUALIFIED
      s" , or call it inside `using " EMIT-STR
      HOLDER-FIRST @ CK-REC-NAME$ EMIT-STR  s" ` … `;using`" EMIT-STR EXIT
   THEN
   t tu RES-TRUE HOLDER-SCAN  s" : write it qualified, as " EMIT-STR  t tu QUALIFIED ;

\ The record of t tu, which nothing defines, written through UNDEF-HELP for
\ d du: the caller stops or keeps it.
: UNDEFINED-RECORD ( str str -- )
   {: t:^ tu:n d:^ du:n :}
   s" E-UNDEFINED" REJECTION:OPEN
   t tu UNDEF-MESSAGE
   t tu REJECTION:AT  t tu UNDEF-LABEL
   REJECTION:HELP
   t tu LOCALS-END? IF
      du 0= IF
         s" delete it: a locals list is written only inside a definition" EMIT-STR
      ELSE LOCALS-END-HELP THEN EXIT
   THEN
   t tu d du UNDEF-HELP ;

\ Body keyword t tu run outside a definition; the caller stops or keeps it.
: BODY-ONLY-RECORD ( str -- )
   {: t:^ tu:n :}
   s" E-UNDEFINED" REJECTION:OPEN
   t tu CODE-SPAN  s"  works only inside a definition" EMIT-STR
   t tu REJECTION:AT  s" outside a definition" EMIT-STR
   REJECTION:HELP  s" move " EMIT-STR  t tu CODE-SPAN
   s"  into a `:` … `;` body" EMIT-STR ;

\ `include` and `require` take their path as the definition compiles, which
\ the engine rejects; their string form w wu takes it when the word runs.
: LOADER-HELP ( str str -- ) {: t:^ tu:n w:^ wu:n :}
   s" move " EMIT-STR  t tu CODE-SPAN
   s"  and its path out of the definition to top level, or load the file when the word runs with `s" EMIT-STR
   34 EMIT1  s"  path" EMIT-STR  34 EMIT1  32 EMIT1
   w wu EMIT-STR  96 EMIT1 ;

\ Keyword t tu named where a word is wanted; the caller stops or keeps it.
: KEYWORD-RECORD ( str -- )
   {: t:^ tu:n :}
   s" E-UNDEFINED" REJECTION:OPEN
   t tu KEYWORD-MESSAGE
   t tu REJECTION:AT  s" not a word" EMIT-STR
   REJECTION:HELP  t tu KEYWORD-HELP ;

: IMM-HELP ( str -- ) {: t:^ tu:n :}
   \ The token's binding is read afresh: the walk's last bound token may be another.
   t tu CHECKER-FIND-ACTIVE-SYM t tu TOK-IDENTITY CTL>INTRINSIC {: id:n :}
   id INTRINSIC-INCLUDE = IF t tu s" included" LOADER-HELP EXIT THEN
   id INTRINSIC-REQUIRE = IF t tu s" required" LOADER-HELP EXIT THEN
   t tu IMM-WHY {: why:n :}
   why IMM-EFFECT = IF
      s" give " EMIT-STR  t tu CODE-SPAN
      s"  the effect `( -- )`, or drop `immediate`" EMIT-STR EXIT
   THEN
   why IMM-WRITES = IF
      s" write those tokens in the definition, or build the word with `define-word`" EMIT-STR EXIT
   THEN
   why IMM-ROOTS = IF
      s" move the root at top level, before the definition" EMIT-STR EXIT
   THEN
   s" state the count with `parses: " EMIT-STR  t tu EMIT-STR
   s"  n` after " EMIT-STR  t tu CODE-SPAN ;

: QUAL-HELP ( -- )
   s" write " EMIT-STR  TOK-SPAN
   s"  with one `:` between the package and the word, as `PKG:WORD`" EMIT-STR ;

: LOCAL-HELP ( -- )
   LOCALBAD-GROUP? IF
      s" remove " EMIT-STR  TOK-SPAN  s"  from the locals group" EMIT-STR EXIT
   THEN
   s" bind the local outside the quotation, on a path that runs, or move this code into a word of its own"
   EMIT-STR ;

: SIG-HELP ( -- )   SIG-SUBJECT$ SGBAD-FIX ;

: DEAD-HELP ( -- )
   s" remove " EMIT-STR  TOK-SPAN
   s"  and what follows it in this branch, or move that work before " EMIT-STR
   DEADTA @ DEADTU @ CODE-SPAN ;

\ A declared type or row variable's letter in backticks.
: NP-GENERIC ( -- )
   s" , or keep the body generic over " EMIT-STR  NPBAD-Q1 @ VAR-SPAN ;
: NP-POINTEE ( str -- ) {: a:^ u:n :}
   s" declare the pointee the " EMIT-STR  a u EMIT-STR
   s" , as `^n` or `^u8`, in place of " EMIT-STR  NPBAD-Q1 @ VAR-SPAN ;
: NP-HELP ( -- )
   NPBAD-KIND @ case
      4 of s" base address reaches" NP-POINTEE endof
      5 of s" null stands in for" NP-POINTEE endof
      3 of s" declare the type the raw storage holds in place of " EMIT-STR
         NPBAD-Q1 @ VAR-SPAN  NP-GENERIC endof
      0 of s" declare " EMIT-STR  NP-FAM-REND  s"  in place of " EMIT-STR
         NPBAD-Q1 @ VAR-SPAN  NP-GENERIC endof
      1 of s" write one type variable for " EMIT-STR  NPBAD-Q1 @ VAR-SPAN
         s"  and " EMIT-STR  NPBAD-Q2 @ VAR-SPAN
         s" , or keep the body from making them one type" EMIT-STR endof
      6 of s" run the quotation that yields the output row on every path, or declare the row the body leaves"
         EMIT-STR endof
      8 of s" declare " EMIT-STR  NP-ROW
         s"  as the row the body leaves and read it only where the code is 0: after `code 0<> if code throw then`"
         EMIT-STR endof
      7 of s" declare the cells the body leaves in place of " EMIT-STR  NP-ROW
         s" , or keep the body generic over it" EMIT-STR endof
      s" declare an input that carries " EMIT-STR  NPBAD-Q1 @ VAR-SPAN
      s" , or write a concrete type in its place" EMIT-STR
   endcase ;

: CAP-HELP ( -- )
   s" call a checked word that wraps " EMIT-STR  TOK-SPAN
   TOP-STEP @ IF s"  in place of calling it here"
   ELSE s"  in place of calling it from this body" THEN EMIT-STR ;

: UNSAFE-HELP ( -- )
   PIN-TOK$ s" evaluate" CORE-STR=CI IF
      s" call `evaluate-closed` in place of " EMIT-STR  TOK-SPAN EXIT
   THEN
   s" move " EMIT-STR  TOK-SPAN  s"  out of the body to top level" EMIT-STR ;

: SCOPED-TICK-HELP ( -- )
   s" call " EMIT-STR  TOK-SPAN
   s"  directly where the checker proves its scope instead of ticking it" EMIT-STR ;

: ONE-CELL-HELP ( -- )
   MD-ONE-CELL MD-SUGGEST$ EMIT-STR
   AT-PIN  s"  `" EMIT-STR  CELL-TYPE REND-TYPE  s" `" EMIT-STR  ;AT-PIN ;

: OFF-STEP-HELP ( -- )
   CELL-VAR-SYM {: sym:n :}
   sym 0 <> IF
      s" declare `" EMIT-STR  sym SYM-NAME$ EMIT-STR  s" `" EMIT-STR
   ELSE s" declare the word" EMIT-STR THEN
   s"  for the plain cell type it steps, or step `" EMIT-STR
   AT-PIN  CELL-TYPE REND-TYPE  ;AT-PIN  s" ` by its size" EMIT-STR ;

\ A word whose effect names the type is checked as linear; a mover's var is not.
: MOVED-LIN-HELP ( -- )
   s" call a word whose effect names `" EMIT-STR  AT-PIN  CELL-TYPE REND-TYPE  ;AT-PIN
   s" ` in place of " EMIT-STR
   CELL-VAR-SYM {: sym:n :}
   sym 0= IF s" the stack words that move it" EMIT-STR EXIT THEN
   s" `" EMIT-STR  sym SYM-NAME$ EMIT-STR  s" `, which moves any type" EMIT-STR ;

\ The latched FACT-HIT's facts in bit order, joined by one space.
: FACT-NAMES ( -- )
   MDIAG-CELL @ dup negate and {: low:n :}
   63 0 ?do
      1 i lshift {: f:n :}
      MDIAG-CELL @ f and 0 <> IF
         f low <> IF s"  " EMIT-STR THEN  i CTL-BIT>TAG$ EMIT-STR
      THEN
   loop ;

\ The type that keeps it may be a primitive's row (`compile,`), which no program
\ changes, so the help names the other fix too.
: FACT-HELP ( -- )
   MDIAG @ MD-SUGGEST$ EMIT-STR  s"  `" EMIT-STR  FACT-NAMES
   MDIAG @ MD-FACT-UNSTATED = IF
      s" ` in the type that keeps it, or keep a word without `" EMIT-STR
   ELSE
      s" ` in the input `" EMIT-STR  AT-PIN  MDIAG-CELL-VAR @ REND-TYPE  ;AT-PIN
      s" ` of the quotation's type, or " EMIT-STR
      MDIAG @ MD-FACT-INPUT = IF s" keep it in" ELSE s" pass it as" THEN EMIT-STR
      s"  a type whose input states no `" EMIT-STR
   THEN
   FACT-NAMES  s" `" EMIT-STR ;

: LINLOCAL-HELP ( -- )
   s" keep the linear value on the stack and pass it on with stack words, in place of binding it to a local"
   EMIT-STR ;

: UNCK-HELP ( -- )
   s" call a word whose stack effect the checker knows in place of " EMIT-STR
   TOK-SPAN ;

: RETURN-HELP ( -- )
   RSBAD @ 2 = IF
      s" read or pop with `r@` and `r>` only cells this definition pushed with `>r`"
      EMIT-STR EXIT
   THEN
   RSBAD @ 3 = IF
      s" take back with `r>` every cell the quotation pushes with `>r` before its `;]`"
      EMIT-STR EXIT
   THEN
   RSBAD @ 4 = IF
      s" read or pop with `r@` and `r>` only cells this quotation pushed with `>r`"
      EMIT-STR EXIT
   THEN
   s" take back with `r>` every cell the definition pushes with `>r` before it exits"
   EMIT-STR ;

: DIAG-HELP ( -- )
   DKIND {: k:n :}
   k RF-MISMATCH = IF DEND @ 0 <> IF END-HELP ELSE TAKES-HELP THEN EXIT THEN
   k RF-UNDEF = IF
      PIN-TOK$ LOCALS-END? IF LOCALS-END-HELP EXIT THEN
      PIN-TOK$
      \ Only a call in the open body may become `recurse`: not a top-level step,
      \ a tick (no xt) or a call in a quotation (`recurse` calls the quotation).
      TOP-STEP @ 0=  PIN-BODY-CALL @ 0 <>  and
      IF NMA @ NMU @ ELSE NMA @ 0 THEN
      UNDEF-HELP EXIT
   THEN
   k RF-KEYWORD = IF PIN-TOK$ KEYWORD-HELP EXIT THEN
   k RF-IMM = IF PIN-TOK$ IMM-HELP EXIT THEN
   k RF-QUAL = IF QUAL-HELP EXIT THEN
   k RF-LOCAL = IF LOCAL-HELP EXIT THEN
   k RF-SIG = IF SIG-HELP EXIT THEN
   k RF-DEAD = IF DEAD-HELP EXIT THEN
   k RF-NP = IF NP-HELP EXIT THEN
   k RF-CAP = IF CAP-HELP EXIT THEN
   k RF-UNSAFE = IF UNSAFE-HELP EXIT THEN
   k RF-LINLOCAL = IF LINLOCAL-HELP EXIT THEN
   k RF-REASON = IF
      MDIAG @ MD-SCOPED-TICK = IF SCOPED-TICK-HELP EXIT THEN
      MDIAG @ MD-ONE-CELL = IF ONE-CELL-HELP EXIT THEN
      MDIAG @ MD-OFF-STEP = IF OFF-STEP-HELP EXIT THEN
      MDIAG @ MD-MOVED-LINEAR = IF MOVED-LIN-HELP EXIT THEN
      MDIAG @ MD-FACT-UNSTATED =  MDIAG @ MD-FACT-INPUT = or  MDIAG @ MD-FACT-PASSED = or IF
         FACT-HELP EXIT
      THEN
      STALE-ROW-READ? IF
         s" return them where the code is 0: after `code 0<> if code throw then`" EMIT-STR EXIT
      THEN
      MDIAG @ MD-SUGGEST$ EMIT-STR EXIT
   THEN
   k RF-UNCHECKABLE = IF UNCK-HELP EXIT THEN
   k RF-RETURN = IF RETURN-HELP EXIT THEN
   s" render: a rejection with no help" EXIT-STATUS:INTERNAL die ;

\ The record of a does> head that creates no word, placed at its `does>` token
\ t tu, for a reader that judges the head without CHECK; the caller ends it.
: DOES-UNMADE-OPEN ( str -- ) {: t:^ tu:n :}
   MD-DOES-UNMADE MD-CODE$ REJECTION:OPEN
   MD-DOES-UNMADE MD-REASON$ EMIT-STR
   t tu REJECTION:AT  MD-DOES-UNMADE MD-LABEL$ EMIT-STR
   REJECTION:HELP  MD-DOES-UNMADE MD-SUGGEST$ EMIT-STR ;

\ The notes and the help; an ambiguous family's packages note and help are
\ one part.
: DIAG-ADVICE ( -- )
   DKIND RF-SIG =  SGBAD-AMBIG?  and IF SIG-SUBJECT$ 2drop SGBAD-AMBIG-HELP EXIT THEN
   DIAG-NOTES
   REJECTION:HELP  DIAG-HELP ;

: DIAG-PRINT
   0 RQM !  SEEN-RESET 0 NLET !  0 RROW-N !
   DKIND RF-REASON =  MDIAG @ MD-LITERAL-RANGE =  and IF
      DIAG-TOKEN REJECTION:LITERAL-RANGE  REJECTION:KEEP EXIT
   THEN
   DCODE REJECTION:OPEN  DIAG-MESSAGE
   DIAG-PLACE REJECTION:AT
   \ The message names a built local by its index, never by its listing token.
   DKIND RF-REASON =  MDIAG @ MD-LOCAL-ENDED =  MDIAG @ MD-LOCAL-UNBOUND = or  and
   IF REJECTION:NO-WORD THEN
   DIAG-LABEL
   DIAG-ADVICE
   REJECTION:KEEP ;



: TDECL-TITLE ( str str str -- ) {: c:^ cu:n ka:^ ku:n fa:^ fu:n :}
   c cu REJECTION:OPEN
   s" bad " EMIT-STR  ka ku EMIT-STR  s"  declaration" EMIT-STR
   fu 0 > IF s"  `" EMIT-STR  fa fu EMIT-STR  96 EMIT1 THEN ;
\ A rejected declaration of a kind and family: its token labeled with the
\ reason, the word it names as a `word` note unless empty, the fix as help.
\ The caller ends the record.
: TDECL-DIAG ( str str str str str str -- )
   {: ka:^ ku:n fa:^ fu:n ta:^ tu:n wa:^ wu:n xa:^ xu:n na:^ nu:n :}
   s" E-BAD-DECLARATION" ka ku fa fu TDECL-TITLE
   ta tu REJECTION:AT  wa wu EMIT-STR
   nu 0 > IF s" word" REJECTION:NOTE  na nu EMIT-STR THEN
   REJECTION:HELP  xa xu EMIT-STR ;

\ Why REC-SIG left a certified sig-less word unrecorded; the last arm answers
\ every other RQM cause, a stale or scoped type among them, as an unmodeled tag.
: REC-REFUSE-WHY ( -- )
   RBIND-FULL @ IF BINDERS-REASON s"  in inferred effect" EMIT-STR EXIT THEN
   RROW-UNSAID @ IF
      s" quotation return-stack clause or more than " EMIT-STR  RROW-CAP EMIT-U
      s"  named rows the effect text cannot say" EMIT-STR EXIT
   THEN
   s" unmodeled type tag in inferred effect" EMIT-STR ;

\ The fix for REC-REFUSE-WHY's reason, for the word na nu.
: REC-REJECT-HELP ( str -- ) {: na:^ nu:n :}
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

\ W-EFFECT-NOT-RECORDED warns that the word certified and loaded, but a later
\ call rejects it as undefined; the run's status stands. It names the word as
\ its source token is spelled.
: REC-REFUSE-DIAG ( -- )
   NM-SPAN {: na:^ nu:n :}
   s" W-EFFECT-NOT-RECORDED" REJECTION:WARN
   s" the effect of `" EMIT-STR  na nu EMIT-STR
   s" ` cannot be recorded, so a later call of it is rejected as undefined"
   EMIT-STR
   NM-SPAN REJECTION:AT  s" effect not recorded" EMIT-STR
   s" reason" REJECTION:NOTE  REC-REFUSE-WHY
   REJECTION:HELP  na nu REC-REJECT-HELP
   REJECTION:KEEP ;

\ A sig-less word's inferred effect is recorded (CHECKER-USIG-CERT-CURRENT) unless
\ its text sets RQM.
: REC-SIG ( str -- )
   {: na:^ nu:n :}
   REND-SIG 2drop
   RQM @ 0 = IF
      BROW @ KEEP-INNER  DCUR @ KEEP-INNER
      na nu CHECKER-USIG-CERT-CURRENT
      EXIT
   THEN
   BROW @ DCUR @ CHECK-UNRECORDED!
   REC-REFUSE-DIAG ;

\ The text of a rejected `cast:` or `linear:` declaration.
package CAST-ERROR
: CODE$ ( n -- str ) {: code:n :}
   code E-CAST-ARITY = IF s" E-CAST-ARITY" EXIT THEN
   code E-CAST-CLASS = IF s" E-CAST-CLASS" EXIT THEN
   code E-CAST-FAM = IF s" E-CAST-FAM" EXIT THEN
   code E-CAST-LINEAR = IF s" E-CAST-LINEAR" EXIT THEN
   code E-CAST-MINT = IF s" E-CAST-MINT" EXIT THEN
   code E-CAST-SCOPE = IF s" E-CAST-SCOPE" EXIT THEN
   code E-LINEAR-PAYLOAD = IF s" E-LINEAR-PAYLOAD" EXIT THEN
   code E-LINEAR-OWNER = IF s" E-LINEAR-OWNER" EXIT THEN
   code E-LINEAR-SCOPE = IF s" E-LINEAR-SCOPE" EXIT THEN
   s" checker: a cast rejection with no code name" EXIT-STATUS:INTERNAL die ;
\ E-CAST-FAM's label is the parse's: only PARSE raises it, with SGBAD set. Each
\ E-CAST-ARITY case gives its own label.
: LABEL$ ( n -- str ) {: code:n :}
   code E-CAST-CLASS = IF s" a retype no cast may make" EXIT THEN
   code E-CAST-FAM = IF s" the signature does not parse here" SGBAD-LABEL$ EXIT THEN
   code E-CAST-LINEAR = IF s" a linear value would change hands" EXIT THEN
   code E-CAST-MINT = IF s" mints outside a private section" EXIT THEN
   code E-CAST-SCOPE = IF s" not a view cast this place may declare" EXIT THEN
   code E-LINEAR-PAYLOAD = IF s" not one token and a payload it may carry" EXIT THEN
   code E-LINEAR-OWNER = IF s" declared outside the type's package" EXIT THEN
   code E-LINEAR-SCOPE = IF s" outside its owner's private section" EXIT THEN
   s" checker: a cast rejection with no label" EXIT-STATUS:INTERNAL die ;
: WORD$ ( -- str ) WORD-A @ WORD-U @ ;
: SIG$ ( -- str ) SIG-A @ SIG-U @ ;
\ The message: the declaration in backticks, why wa wu, then the token TOKEN!
\ named in backticks.
: TITLE ( str -- ) {: wa:^ wu:n :}
   s" in `" EMIT-STR  KEY-A @ KEY-U @ EMIT-STR  32 EMIT1  WORD$ EMIT-STR
   s" `, " EMIT-STR  wa wu EMIT-STR
   TOK-U @ 0 > IF
      s"  `" EMIT-STR  TOK-A @ TOK-U @ EMIT-STR  96 EMIT1
   THEN ;
\ The token the record places: TOKEN!'s when the signature holds it (a package
\ name it named is no source), else the declared name.
: SPOT$ ( -- str )
   TOK-A @ SIG-A @ - {: d:n :}
   TOK-U @ 0 >  d 0 >= and  d SIG-U @ < and IF TOK-A @ TOK-U @ EXIT THEN
   WORD$ ;
;package

global
\ `+` and `cell+` refuse a layout pointee; bounds stay the caller's.
: record-at ( ^a n -- ^a ) cells + ;  INTRINSIC-RECORD-AT INTRINSIC

;package
