\ checker-scan-index-lib.f — the shared fixture of the two checker-scan-index
\ rows, test/checker-scan-index-suite.f and
\ test/checker-scan-index-rollback-suite.f.
\
\ It holds definitions only and runs no case: the assertion words, the named
\ TRUSTED: shims that reach the checker's stores, their indexes and the walks
\ that specify them, and the differential that compares every index against
\ its walk. The suite's header says what is under test. Each row requires this
\ file and reopens `package SCANIDX-TEST` to run its cases.

\ Every definition here is a fixture helper, so they live in the rows' own
\ package. The cases still run as top-level interpret lines inside it: the
\ checker's rollback and registry operations resolve only at top level, and the
\ open package is where the definitions those cases make land — which is why
\ symbols are resolved with CHECKER-FIND-ACTIVE-SYM (the current scope) rather
\ than as globals.
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
      T-FAIL s" assert: expected " type want . s" got " type got . cr
   then ;

: TTRUE ( bool -- )
   if -1 else 0 then -1 T= ;

: TFALSE ( bool -- )
   if -1 else 0 then 0 T= ;

\ ---------------------------------------------------------------------------
\ whitebox shims. The stores, their indexes, and the walks that specify them
\ are checker-internal colon words; each is reached through one named boundary.
\ ---------------------------------------------------------------------------
: SCX-SYM-N ( -- n ) SYM-N @ ;
TRUSTED: SCX-SYM-CAP ( -- n ) SYM-CAP ;
TRUSTED: SCX-ACTIVE-SYM ( ptr u8 n -- n ) CHECKER-FIND-ACTIVE-SYM ;
TRUSTED: SCX-SIG-MIN-IN ( ptr u8 n -- n ) SIG-MIN-IN ;
TRUSTED: SCX-CTL-FLAGS ( ptr u8 n -- n ) CTL-FLAGS ;

TRUSTED: SCX-USIG-NEWEST ( n -- n ) USIG-NEWEST ;
TRUSTED: SCX-USIG-NEWEST-LINEAR ( n -- n ) USIG-NEWEST-LINEAR ;
TRUSTED: SCX-NORET-NEWEST ( n -- n ) NORET-NEWEST ;
TRUSTED: SCX-NORET-NEWEST-LINEAR ( n -- n ) NORET-NEWEST-LINEAR ;

TRUSTED: SCX-SVX@ ( n -- n ) SVX-ENSURE SVX@ ;
TRUSTED: SCX-SUMV-CTOR-FIRST-LINEAR ( n -- n ) SUMV-CTOR-FIRST-LINEAR ;
TRUSTED: SCX-SUMV-FROM-CTOR ( n -- n bool ) SUMV-FROM-CTOR-SYM ;

: SCX-TFAM-N ( -- n ) TFAM-N@ ;
TRUSTED: SCX-TFAM-PKG$ ( n -- ptr u8 n ) TFAM-PKG$ ;
: SCX-TFAM-NAME$ ( n -- ptr u8 n ) TFAM-NAME$ ;
TRUSTED: SCX-TFAM-FIND-IN ( ptr u8 n ptr u8 n -- n bool ) TFAM-FIND-IN ;
TRUSTED: SCX-TFAM-FIND-IN-LINEAR ( ptr u8 n ptr u8 n -- n bool ) TFAM-FIND-IN-LINEAR ;
TRUSTED: SCX-TFAM-FIND-PUBLIC ( ptr u8 n -- n bool ) TFAM-FIND-PUBLIC ;
TRUSTED: SCX-TFAM-DECL ( ptr u8 n n ptr u8 n n n -- n ) TFAM-DECL ;
TRUSTED: SCX-TFX-SLOTS ( -- n ) TFX-SLOTS ;
: SCX-TFX-SLOTS-INIT ( -- n ) TFX-SLOTS-INIT ;

\ The span cells are pinned to what they hold, so the store is checked rather
\ than asserted (test/typed-storage-structural-test.f §2). SCX-SYM-INTERN stays
\ trusted for SYM-INTERN, not for these cells.
TYPED-VARIABLE SCX-NA ptr u8
TYPED-VARIABLE SCX-NU n
: SCX-NAME! ( ptr u8 n -- ) SCX-NU ! SCX-NA ! ;
TRUSTED: SCX-SYM-INTERN ( -- n ) s" " SYM-GLOBAL SCX-NA @ SCX-NU @ SYM-INTERN ;

\ the two scans the dot names, read directly: SCAN-USIGS-SYM leaves its answer
\ in FEP/FMEND and NORET-SCAN-SYM in NORET-CTL, and the memoizing entry points
\ above them can hide a wrong answer behind a cache hit.
TRUSTED: SCX-SCAN-USIG ( n -- ) SCAN-USIGS-SYM ;
TRUSTED: SCX-FEP-HIT? ( -- bool ) FEP-HIT? ;
TRUSTED: SCX-FEP-MINI ( -- n ) FEP @ E-MINI@ ;
: SCX-FMEND ( -- n ) FMEND @ ;
create SCX-WIRE EFF-WIRE allot
TRUSTED: SCX-WIRE-NEXT ( n -- n )
   USIG-NEWEST 1- E-PTR SCX-WIRE E-WIRE-COPY
   SCX-WIRE EW.NEXT @ ;
TRUSTED: SCX-SCAN-NORET ( n -- ) NORET-SCAN-SYM ;
TRUSTED: SCX-NORET-FLAGS ( -- n ) NORET-CTL @ XFER-FLAGS ;   \ the control word's flag bits

\ Each index carries the store end it was last made exact at. A rollback that
\ repaired the index in place leaves that mark at or below the store's new end;
\ a rollback that did NOT leaves it above, and the next lookup is forced to
\ rebuild the whole index from the store. Both answer correctly, so the mark is
\ the only thing that tells them apart — and it is the whole point of the seam.
\ An index no lookup has built yet carries mark 0, which passes vacuously, so a
\ row runs the differential below — it builds all four — before any rollback
\ case reads the marks.
: SCX-UEND ( -- n ) UEND @ ;
: SCX-USX-HI ( -- n ) USX-HI @ ;
: SCX-NORET-END ( -- n ) NORET-END @ ;
: SCX-NRX-HI ( -- n ) NRX-HI @ ;
: SCX-SUMV-N ( -- n ) SUMV-N@ ;
TRUSTED: SCX-SVX-HI ( -- n ) SVX-HI @ ;
TRUSTED: SCX-TFX-HI ( -- n ) TFX-HI @ ;

: SCX-MARKS-EXACT ( -- )
   SCX-USX-HI SCX-UEND > TFALSE
   SCX-NRX-HI SCX-NORET-END > TFALSE
   SCX-SVX-HI SCX-SUMV-N > TFALSE
   SCX-TFX-HI SCX-TFAM-N > TFALSE ;

TRUSTED: SCX-CAND-START ( -- ) CHECK-CANDIDATE-START ;
TRUSTED: SCX-CAND-DONE ( n -- n ) CHECK-CANDIDATE-DONE ;
TRUSTED: SCX-USIG-ADD ( ptr u8 n ptr u8 n -- ) CHECKER-USIG-ADD ;
TRUSTED: SCX-SUMV-ADD ( n ptr u8 n n n n n -- n ) SUMV-ADD ;
TRUSTED: SCX-SUMV-CTOR-SYM! ( n n -- ) SUMV-CTOR-SYM! ;

variable TC                    \ last caught throw code
variable NMIS                  \ differential mismatches in the current section
variable IX
variable REC-END

\ ---------------------------------------------------------------------------
\ the differential. For every symbol the image has interned and every family
\ it has declared, the index and the walk that specifies it agree. One
\ assertion per store: the number of disagreements is zero.
\ ---------------------------------------------------------------------------
: SCX-DIFF-USIG ( -- n )
   0 NMIS !
   1 IX !
   BEGIN IX @ SCX-SYM-N < WHILE
      IX @ SCX-USIG-NEWEST  IX @ SCX-USIG-NEWEST-LINEAR <> IF 1 NMIS +! THEN
      IX @ 1 + IX !
   REPEAT
   NMIS @ ;

: SCX-DIFF-NORET ( -- n )
   0 NMIS !
   1 IX !
   BEGIN IX @ SCX-SYM-N < WHILE
      IX @ SCX-NORET-NEWEST  IX @ SCX-NORET-NEWEST-LINEAR <> IF 1 NMIS +! THEN
      IX @ 1 + IX !
   REPEAT
   NMIS @ ;

: SCX-DIFF-SUMV ( -- n )
   0 NMIS !
   1 IX !
   BEGIN IX @ SCX-SYM-N < WHILE
      IX @ SCX-SVX@  IX @ SCX-SUMV-CTOR-FIRST-LINEAR <> IF 1 NMIS +! THEN
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
      IX @ SCX-TFAM-PKG$ IX @ SCX-TFAM-NAME$ SCX-TFAM-FIND-IN {: gid:n gf:bool :}
      IX @ SCX-TFAM-PKG$ IX @ SCX-TFAM-NAME$ SCX-TFAM-FIND-IN-LINEAR {: wid:n wf:bool :}
      gid wid <> IF 1 NMIS +! THEN
      gf IF wf 0= IF 1 NMIS +! THEN ELSE wf IF 1 NMIS +! THEN THEN
      IX @ 1 + IX !
   REPEAT
   NMIS @ ;

: SCX-DIFF-ALL ( -- )
   SCX-DIFF-USIG 0 T=
   SCX-DIFF-NORET 0 T=
   SCX-DIFF-SUMV 0 T=
   SCX-DIFF-TFAM 0 T= ;

\ ---------------------------------------------------------------------------
\ report: "ok" on success; on any failure, the failure count and the row's own
\ message, then a nonzero exit.
\ ---------------------------------------------------------------------------
: REPORT ( ptr u8 n -- ) {: msg:ptr msgu:n :}
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . msg msgu 1 die ;

;package

;using
