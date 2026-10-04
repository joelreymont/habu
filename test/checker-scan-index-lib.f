\ checker-scan-index-lib.f — the shared fixture of the two checker-scan-index
\ rows, test/checker-scan-index-suite.f and
\ test/checker-scan-index-rollback-suite.f.
\
\ It holds definitions only and runs no case: the assertion words, the checked
\ words that reach the checker's stores, their indexes and the walks
\ that specify them, and the differential that compares every index against
\ its walk. The suite's header says what is under test. Each row requires this
\ file and reopens `package SCANIDX-TEST` to run its cases.

\ Every definition here is a fixture helper, so they live in the rows' own
\ package. The cases still run as top-level interpret lines inside it, and the
\ open package is where the definitions those cases make land — which is why
\ symbols are resolved with CHECKER-FIND-ACTIVE-SYM (the current scope) rather
\ than as globals.
require lib/fmt.f                        \ FMT:.INT - one-line number text

using TFAM

package SCANIDX-TEST

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;

: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want FMT:.INT s"  got " type got FMT:.INT cr
   then ;

: TTRUE ( bool -- )
   if -1 else 0 then -1 T= ;

: TFALSE ( bool -- )
   if -1 else 0 then 0 T= ;

\ ---------------------------------------------------------------------------
\ whitebox reach. The stores, their indexes, and the walks that specify them
\ are checker-internal colon words, named directly: on the unsealed engine a
\ checked body binds their recorded rows. The SCX- words read or combine them.
\ ---------------------------------------------------------------------------
: SCX-SYM-N ( -- n ) SYM-N @ ;
\ The din cell count of the newest record the store keeps under the name's own
\ symbol, -1 for none. The suites are about the symbol-keyed index, so this asks
\ the store's key rather than a binding: a name only CHECKER-USIG-ADD recorded
\ has no engine record, and compiled code binds nothing to it.
: SCX-SIG-MIN-IN ( ptr u8 n -- n )
   CHECKER-RECORD-SYM? CHECKER-FIND-USIG-SYM IF FEP @ E-MINI@ ELSE -1 THEN ;

: SCX-SVX@ ( n -- n ) SVX-ENSURE SVX@ ;

: SCX-TFAM-N ( -- n ) TFAM-N@ ;
: SCX-TFAM-NAME$ ( n -- ptr u8 n ) TFAM-NAME$ ;
: SCX-TFX-SLOTS-INIT ( -- n ) TFX-SLOTS-INIT ;

\ The span cells are pinned to what they hold, so the store is checked rather
\ than asserted (test/typed-storage-structural-test.f §1).
TYPED-VARIABLE SCX-NA ptr u8
TYPED-VARIABLE SCX-NU n
: SCX-NAME! ( ptr u8 n -- ) SCX-NU ! SCX-NA ! ;
: SCX-SYM-INTERN ( -- n ) s" " SYM-GLOBAL SCX-NA @ SCX-NU @ SYM-INTERN ;

\ the two scans the dot names, read directly: SCAN-USIGS-SYM leaves its answer
\ in FEP/FMEND and NORET-SCAN-SYM in NORET-CTL, and the memoizing entry points
\ above them can hide a wrong answer behind a cache hit.
: SCX-FEP-MINI ( -- n ) FEP @ E-MINI@ ;
: SCX-FMEND ( -- n ) FMEND @ ;
create SCX-WIRE EFF-WIRE allot
: SCX-WIRE-NEXT ( n -- n )
   USIG-NEWEST 1- E-PTR SCX-WIRE E-WIRE-COPY
   SCX-WIRE EW.NEXT @ ;
: SCX-NORET-FLAGS ( -- n ) NORET-CTL @ XFER-FLAGS ;   \ the control word's flag bits

\ Each index carries the store end it was last made exact at. A rollback that
\ repaired the index in place leaves that mark at or below the store's new end;
\ a rollback that did NOT leaves it above, and the next lookup is forced to
\ rebuild the whole index from the store. Both answer correctly, so the mark is
\ the only thing that tells them apart — and it is the whole point of the seam.
\ An index no lookup has built yet carries mark 0, which passes vacuously, so a
\ row runs the differential below — it builds all five — before any rollback
\ case reads the marks.
: SCX-UEND ( -- n ) UEND @ ;
: SCX-USX-HI ( -- n ) USX-HI @ ;
: SCX-NORET-END ( -- n ) NORET-END @ ;
: SCX-NRX-HI ( -- n ) NRX-HI @ ;
: SCX-SUMV-N ( -- n ) SUMV-N@ ;
: SCX-SVX-HI ( -- n ) SVX-HI @ ;
: SCX-TFX-HI ( -- n ) TFX-HI @ ;
: SCX-VNX-HI ( -- n ) VNX-HI @ ;

: SCX-MARKS-EXACT ( -- )
   SCX-USX-HI SCX-UEND > TFALSE
   SCX-NRX-HI SCX-NORET-END > TFALSE
   SCX-SVX-HI SCX-SUMV-N > TFALSE
   SCX-TFX-HI SCX-TFAM-N > TFALSE
   SCX-VNX-HI SCX-SUMV-N > TFALSE ;

variable TC                    \ last caught throw code
variable NMIS                  \ differential mismatches in the current section
variable IX
variable REC-END

\ ---------------------------------------------------------------------------
\ the reference side of the effect and control differentials. The
\ specification of each store, USIG-NEWEST-LINEAR and NORET-NEWEST-LINEAR,
\ answers ONE symbol with a walk of the whole store, so asking it for every
\ symbol walks the store once per symbol. The walks below are those same
\ walks, made once: each record's position lands in SCX-REF under the symbol
\ it carries, and a later record of the same symbol overwrites an earlier one,
\ so what is left is the newest record of every symbol at once. They read the
\ records and the store's own bounds and nothing an index maintains — no head,
\ no back-link, no mark — and they fill their own table, so the index under
\ test never answers for them.
\ ---------------------------------------------------------------------------
DYNAMIC-BUFFER SCX-REF n       \ per symbol: newest record's offset+1, 0 = none
variable REF-POS               \ the walk's record cursor
variable REF-NEXT              \ ... and the link it reads

: SCX-USER-OFF ( -- n ) USIGS-USER-OFF @ ;
: SCX-EFF-REC ( -- n ) EFF-REC ;
: SCX-REC-SYM ( n -- n ) E-PTR ER.SYM @ ;
: SCX-REC-NEXT ( n -- n ) E-PTR E-NEXT@ ;
: SCX-NORET-ENTRY ( -- n ) NORET-ENTRY ;
: SCX-NORET-SYM ( n -- n ) NORET-CELL NORET.SYM @ ;

: SCX-REF-CLEAR ( -- )
   SCX-SYM-N SCX-REF-RESERVE
   SCX-SYM-N 0 ?do 0 i SCX-REF ! loop ;

\ The differential reads the interned ids, 1 up to SYM-N, so a record keyed to
\ any other id answers nothing it compares and is passed over.
: SCX-REF-NOTE ( n n -- ) {: pos:n sym:n :}
   sym 1 >=  sym SCX-SYM-N <  and IF pos 1 + sym SCX-REF ! THEN ;

\ USIG-NEWEST-LINEAR's walk: from the first user record while a whole record
\ fits below UEND, following each record's link until one does not advance or
\ leaves the live store.
: SCX-USIG-REF ( -- )
   SCX-REF-CLEAR
   SCX-USER-OFF REF-POS !
   BEGIN REF-POS @ SCX-EFF-REC + SCX-UEND <= WHILE
      REF-POS @  REF-POS @ SCX-REC-SYM  SCX-REF-NOTE
      REF-POS @ SCX-REC-NEXT REF-NEXT !
      REF-NEXT @ REF-POS @ <=  REF-NEXT @ SCX-UEND >  or IF
         SCX-UEND 1 + REF-POS !
      ELSE
         REF-NEXT @ REF-POS !
      THEN
   REPEAT ;

\ NORET-NEWEST-LINEAR's walk: entry by entry from the first, up to the entry
\ keyed 0 that terminates the store.
: SCX-NORET-REF ( -- )
   SCX-REF-CLEAR
   0 REF-POS !
   BEGIN REF-POS @ SCX-NORET-SYM 0 <> WHILE
      REF-POS @  REF-POS @ SCX-NORET-SYM  SCX-REF-NOTE
      REF-POS @ SCX-NORET-ENTRY + REF-POS !
   REPEAT ;

\ ---------------------------------------------------------------------------
\ the differential. For every symbol the image has interned and every family
\ it has declared, the index and the walk that specifies it agree. One
\ assertion per store: the number of disagreements is zero. The variant and
\ family stores are small, so their differentials still ask the specification
\ word itself once per symbol or family.
\ ---------------------------------------------------------------------------
: SCX-DIFF-USIG ( -- n )
   SCX-USIG-REF
   0 NMIS !
   1 IX !
   BEGIN IX @ SCX-SYM-N < WHILE
      IX @ USIG-NEWEST  IX @ SCX-REF @ <> IF 1 NMIS +! THEN
      IX @ 1 + IX !
   REPEAT
   NMIS @ ;

: SCX-DIFF-NORET ( -- n )
   SCX-NORET-REF
   0 NMIS !
   1 IX !
   BEGIN IX @ SCX-SYM-N < WHILE
      IX @ NORET-NEWEST  IX @ SCX-REF @ <> IF 1 NMIS +! THEN
      IX @ 1 + IX !
   REPEAT
   NMIS @ ;

: SCX-DIFF-SUMV ( -- n )
   0 NMIS !
   1 IX !
   BEGIN IX @ SCX-SYM-N < WHILE
      IX @ SCX-SVX@  IX @ SUMV-CTOR-FIRST-LINEAR <> IF 1 NMIS +! THEN
      IX @ 1 + IX !
   REPEAT
   NMIS @ ;

\ Each family is looked up by its own (package, tail), which is the pair the
\ registry guarantees unique, so the indexed and walked answers must be the
\ same row id and the same found flag.
: SCX-DIFF-TFAM ( -- n )
   0 NMIS !
   0 IX !
   BEGIN IX @ SCX-TFAM-N < WHILE
      IX @ TFAM-PKG$ IX @ SCX-TFAM-NAME$ TFAM-FIND-IN {: gid:n gf:bool :}
      IX @ TFAM-PKG$ IX @ SCX-TFAM-NAME$ TFAM-FIND-IN-LINEAR {: wid:n wf:bool :}
      gid wid <> IF 1 NMIS +! THEN
      gf IF wf 0= IF 1 NMIS +! THEN ELSE wf IF 1 NMIS +! THEN THEN
      IX @ 1 + IX !
   REPEAT
   NMIS @ ;

\ Each variant is looked up by its own (family, tail), so the indexed and walked
\ answers must be the same row id and the same found flag.
: SCX-DIFF-VNX ( -- n )
   0 NMIS !
   0 IX !
   BEGIN IX @ SCX-SUMV-N < WHILE
      IX @ SUMV-FAM@ IX @ SUMV-NAME$ SUMV-FIND {: gid:n gf:bool :}
      IX @ SUMV-FAM@ IX @ SUMV-NAME$ SUMV-FIND-LINEAR {: wid:n wf:bool :}
      gid wid <> IF 1 NMIS +! THEN
      gf IF wf 0= IF 1 NMIS +! THEN ELSE wf IF 1 NMIS +! THEN THEN
      IX @ 1 + IX !
   REPEAT
   NMIS @ ;

: SCX-DIFF-ALL ( -- )
   SCX-DIFF-USIG 0 T=
   SCX-DIFF-NORET 0 T=
   SCX-DIFF-SUMV 0 T=
   SCX-DIFF-TFAM 0 T=
   SCX-DIFF-VNX 0 T= ;

\ ---------------------------------------------------------------------------
\ report: "ok" on success; on any failure, the failure count and the row's own
\ message, then a nonzero exit.
\ ---------------------------------------------------------------------------
: REPORT ( ptr u8 n -- ) {: msg:ptr msgu:n :}
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . msg msgu 1 die ;

;package

;using
