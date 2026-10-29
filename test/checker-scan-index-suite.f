\ checker-scan-index-suite.f — the checker's symbol-keyed store indexes against
\ the walks that define them. A WHITEBOX-SUITE row: the gate runs it in its
\ unsealed whitebox engine; under bin/hb it exits 70 (docs/gate.md "How a suite
\ runs, and what that demands of its files").

require src/lib/string.f
\ Index per lookup: SCAN-USIGS-SYM HT-USX, NORET-SCAN-SYM HT-NRX,
\ SUMV-FROM-CTOR-SYM HT-SVX, SUMV-FIND VNX, TFAM-FIND-IN TFX; each store keeps
\ its *-LINEAR walk. Shims and the differential: test/checker-scan-index-lib.f.
require test/checker-scan-index-lib.f

\ Reopened, not imported: the definitions the cases make land in the fixture's
\ package.
using TFAM

package SCANIDX-TEST

\ A scan and the reads of what it left, in one body: checking the next
\ top-level line runs the checker, which reuses FEP and NORET-CTL.
: SCX-SCAN-USIGS ( n -- n bool n ) SCAN-USIGS-SYM FEP-HIT? SCX-FEP-MINI ;
: SCX-SCAN-NORET ( n -- n ) NORET-SCAN-SYM SCX-NORET-FLAGS ;

\ The cases here share state in order (section 1 defines SCXA, SCXT and scxfam;
\ 3d, 3e and 5 read them); test/checker-scan-index-rollback-suite.f holds 3a-3c,
\ which touch none of it.
\ 1. ORDER PINNING: which record of a symbol wins, read through the checker's
\    own entry points.
\ Section 1 pins the order (redefinition, deletion, shadowing) before any index
\ word is named: it catches a specification and an index wrong together.

\ 1a. effect records: newest wins; a deletion is an absence, not a fall-back to
\     the record it shadows.
: SCX-DEF1 ( -- ) s" : SCXA ( n -- n ) ;" evaluate-closed ;
: SCX-UNDEF ( -- ) s" undefine SCXA" evaluate-closed ;
: SCX-DEF2 ( -- ) s" : SCXA ( n n -- n ) drop ;" evaluate-closed ;
: SCX-DEF3 ( -- ) s" undefine SCXA : SCXA ( n n n -- n ) drop drop ;" evaluate-closed ;
: SCX-DEF4 ( -- ) s" : SCXA ( n n n -- n ) drop drop ;" evaluate-closed ;

s" SCXA" SCX-SIG-MIN-IN -1 T=
' SCX-DEF1 catch TC !   TC @ 0 T=
s" SCXA" SCX-SIG-MIN-IN 1 T=
' SCX-UNDEF catch TC !  TC @ 0 T=
s" SCXA" SCX-SIG-MIN-IN -1 T=
' SCX-DEF2 catch TC !   TC @ 0 T=
s" SCXA" SCX-SIG-MIN-IN 2 T=
' SCX-DEF3 catch TC !   TC @ 0 T=
SCX-UEND REC-END !
s" SCXA" SCX-SIG-MIN-IN 3 T=

\ SCX-SIG-MIN-IN caches, so a wrong scan can hide behind a hit: read the index,
\ the walk and the scan directly.
s" SCXA" CHECKER-FIND-ACTIVE-SYM IX !
IX @ 0 <> TTRUE
IX @ USIG-NEWEST 1- 0 > TTRUE
IX @ USIG-NEWEST  IX @ USIG-NEWEST-LINEAR T=
IX @ SCX-SCAN-USIGS 3 T= TTRUE REC-END @ T=   \ arity, hit, absolute completed end
IX @ SCX-WIRE-NEXT REC-END @ T=               \ owner wire preserves that same end

' SCX-UNDEF catch TC !  TC @ 0 T=
SCX-UEND REC-END !
IX @ SCX-SCAN-USIGS drop TFALSE REC-END @ T=  \ deletion still depends on its completed end
IX @ SCX-WIRE-NEXT REC-END @ T=
s" SCXA" SCX-SIG-MIN-IN -1 T=
' SCX-DEF4 catch TC !   TC @ 0 T=
IX @ SCX-SCAN-USIGS 3 T= TTRUE drop

\ 1b. control flags: later wins, and a redefinition clears the stale metadata.
: SCX-CTLDEF ( -- ) s" : SCXT ( n -- n ) 7101 throw ;" evaluate-closed ;
: SCX-CTLREDEF ( -- ) s" undefine SCXT : SCXT ( n -- n ) ;" evaluate-closed ;

s" SCXT" CTL-FLAGS 0 T=
' SCX-CTLDEF catch TC !   TC @ 0 T=
s" SCXT" CTL-FLAGS CTL-THROW and CTL-THROW T=
' SCX-CTLREDEF catch TC !  TC @ 0 T=
s" SCXT" CTL-FLAGS CTL-THROW and 0 T=

s" SCXT" CHECKER-FIND-ACTIVE-SYM IX !
IX @ NORET-NEWEST 0 <> TTRUE                   \ the differential below is not vacuous
IX @ NORET-NEWEST  IX @ NORET-NEWEST-LINEAR T=
IX @ SCX-SCAN-NORET CTL-THROW and 0 T=

\ 1e. CHECKER-RECORD-SYM answers 0 for a token it cannot resolve; a NORETS entry
\     keyed 0 reads as the store's terminator and would hide every later entry,
\     so nothing is recorded for it.
: SCX-CTLADD-BAD ( -- ) s" a:b:c" CTL-DEAD NORET-ADD ;
: SCX-CTLADD-DEAD ( -- ) s" SCXT" CTL-DEAD NORET-ADD ;
: SCX-CTLADD-CLEAR ( -- ) s" SCXT" 0 NORET-ADD ;

s" a:b:c" CHECKER-RECORD-SYM 0 T=
' SCX-CTLADD-BAD catch TC !  TC @ 0 T=
' SCX-CTLADD-DEAD catch TC !  TC @ 0 T=              \ an entry appended AFTER it
s" SCXT" CTL-FLAGS CTL-DEAD and CTL-DEAD T=          \ ... is still visible
s" SCXT" CHECKER-FIND-ACTIVE-SYM IX !
IX @ NORET-NEWEST  IX @ NORET-NEWEST-LINEAR T=
' SCX-CTLADD-CLEAR catch TC !  TC @ 0 T=
s" SCXT" CTL-FLAGS CTL-DEAD and 0 T=

\ 1c. family rows: a package row and a global row may share a tail, and each
\     exact (package, tail) resolves to its own row.
s" " CHECKER-PACKAGE-PUBLIC s" scxfam" 0 TK-CELL TFAM-DECL IX !
s" scxpk" CHECKER-PACKAGE-PUBLIC s" scxfam" 0 TK-CELL TFAM-DECL
IX @ <> TTRUE
s" " s" scxfam" TFAM-FIND-IN TTRUE IX @ T=
s" scxpk" s" scxfam" TFAM-FIND-IN TTRUE IX @ <> TTRUE
s" scxpk" s" nosuchtail" TFAM-FIND-IN TFALSE drop
\ the global row is lexical and never enters the public fallback set, so the
\ package row is the sole public answer for this tail.
s" scxfam" TFAM-FIND-PUBLIC TTRUE IX @ <> TTRUE

\ a second package exporting the same tail makes the unqualified answer
\ ambiguous: the index reproduces the refusal.
s" scxpk2" CHECKER-PACKAGE-PUBLIC s" scxfam" 0 TK-CELL TFAM-DECL drop
: SCX-FIND-AMBIG ( -- ) s" scxfam" TFAM-FIND-PUBLIC 2drop ;
' SCX-FIND-AMBIG catch E-TFAM-AMBIG T=

\ 1d. constructor symbols: a generated constructor resolves to its variant, and
\     an ordinary word symbol resolves to nothing.
: SCX-SUMDECL ( -- )
   s" SUMTYPE scxsum VARIANT scxva n ;VARIANT VARIANT scxvb ;VARIANT ;SUMTYPE" evaluate-closed ;
' SCX-SUMDECL catch TC !  TC @ 0 T=
s" SCXA" CHECKER-FIND-ACTIVE-SYM SUMV-FROM-CTOR-SYM TFALSE drop

\ 2. DIFFERENTIAL (SCX-DIFF-ALL): every interned symbol, family and variant. It
\    also builds all five indexes, so the rollback marks below mean something.
SCX-TFAM-N 0 > TTRUE                           \ the family differential is not vacuous
SCX-SYM-N 1 > TTRUE                            \ nor the symbol ones
SCX-DIFF-ALL

\ 3. ROLLBACK: the index retires the records a rejected frame retires. 3d and 3e
\    add records for section 1's symbols; 3a-3c, in the rollback row, create
\    theirs inside the frame.

\ 3d. the symbol survives the rollback, so its head reverts to the record the
\     frame did not touch.
s" SCXA" SCX-SIG-MIN-IN 3 T=
CHECK-CANDIDATE-START
   s" n n n n -- n" s" SCXA" CHECKER-USIG-ADD        \ TWO records for the one symbol, so
   s" n n n n n -- n" s" SCXA" CHECKER-USIG-ADD      \ the repair has to reach past both
   HIDX-EPOCH+                                       \ force a miss that records the frame end
   s" SCXA" SCX-SIG-MIN-IN 5 T=
0 CHECK-CANDIDATE-DONE drop
SCX-MARKS-EXACT                                      \ read FIRST: a lookup would rebuild
s" SCXA" CHECKER-FIND-ACTIVE-SYM IX !
IX @ USIG-NEWEST  IX @ USIG-NEWEST-LINEAR T=
IX @ SCX-SCAN-USIGS 3 T= TTRUE drop
s" SCXA" SCX-SIG-MIN-IN 3 T=
SCX-DIFF-ALL

\ 3e. the same for the control store: a flag entry added for an existing symbol
\     inside a rejected frame reverts to the entry below it, in place.
s" SCXT" CTL-FLAGS CTL-DEAD and 0 T=
CHECK-CANDIDATE-START
   ' SCX-CTLADD-DEAD catch TC !  TC @ 0 T=
   ' SCX-CTLADD-DEAD catch TC !  TC @ 0 T=
   s" SCXT" CTL-FLAGS CTL-DEAD and CTL-DEAD T=
0 CHECK-CANDIDATE-DONE drop
SCX-MARKS-EXACT                                      \ read FIRST: a lookup would rebuild
s" SCXT" CHECKER-FIND-ACTIVE-SYM IX !
IX @ SCX-SCAN-NORET CTL-DEAD and 0 T=
s" SCXT" CTL-FLAGS CTL-DEAD and 0 T=
SCX-DIFF-ALL

\ 5. GROWTH: past SYM-CAP the symbol mapping is re-laid-out and every table in
\    it rebuilds; the family tail buckets resize and rehash with the record
\    arena (TFX-RESIZE).

\ the boot image already declares far more than TFX-SLOTS-INIT families, so the
\ bucket array has already been resized and rehashed at least once
TFX-SLOTS SCX-TFX-SLOTS-INIT > TTRUE
SCX-DIFF-TFAM 0 T=

10 constant SCX-RADIX
48 constant SCX-ZERO

: SCX-NAME$ ( n -- ptr u8 n ) {: n:n :}
   SB-RESET
   s" scxsym" SB-APPEND
   1000000
   BEGIN dup 0 > WHILE
      n over / SCX-RADIX mod SCX-ZERO + SB-APPEND-C
      SCX-RADIX /
   REPEAT drop
   SB$ ;

: SCX-FILL-SYMS ( n -- ) {: target:n :}
   0 IX !
   BEGIN SCX-SYM-N target < WHILE
      IX @ SCX-NAME$ SCX-NAME! SCX-SYM-INTERN drop
      IX @ 1 + IX !
   REPEAT ;

PTR-VARIABLE RETIRED-SYMS
variable RETIRED-SYMS-U
: SCX-SYM-STORAGE ( -- ptr u8 n ) SYMS-P @ SYM-CAP SYM-REC * ;
: SCX-ZERO? ( ptr u8 n -- bool ) {: base:ptr size:n :}
   size 0 ?do base i + c@ 0<> if false unloop exit then loop true ;

SCX-SYM-STORAGE RETIRED-SYMS-U ! RETIRED-SYMS !
RETIRED-SYMS @ RETIRED-SYMS-U @ SCX-ZERO? TFALSE
SYM-CAP IX !
IX @ 1 + SCX-FILL-SYMS
SYM-CAP IX @ > TTRUE
RETIRED-SYMS @ RETIRED-SYMS-U @ SCX-ZERO? TTRUE  \ no dead pointers enter a capture
SCX-DIFF-ALL

s" SCXA" SCX-SIG-MIN-IN 3 T=
s" SCXT" CTL-FLAGS CTL-THROW and 0 T=
s" scxpk" s" scxfam" TFAM-FIND-IN TTRUE drop

T-REPORT

;package

;using
