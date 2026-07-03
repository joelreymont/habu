\ snap.f - shared SNAP= stack-snapshot comparator.
\ Loaded by lib/test.f after lib/test/assert.f; failures aggregate into the
\ assert.f counters so T-REPORT covers snapshot cases.

require lib/errors.f
require lib/string.f
require lib/test/record.f
require lib/test/assert.f

32 constant TS-CAP

variable TS-START
variable TS-EXPECT#
variable TS-ACTUAL#

create TS-EXPECT TS-CAP cells allot
create TS-ACTUAL TS-CAP cells allot

: TS-CELL ( ptr a n -- ptr a ) {: buf:ptr idx:n :}
   idx 0 < if E-TBL-BOUNDS throw then
   idx TS-CAP >= if E-TBL-BOUNDS throw then
   buf idx cells + ;

: TS-N-CHECK ( n -- n )
   dup 0 < if E-TBL-BOUNDS throw then
   dup TS-CAP > if E-TBL-BOUNDS throw then ;

: TS-ACTUAL-N! ( n -- )
   TS-N-CHECK TS-ACTUAL# ! ;

: TS-EXPECT-N! ( n -- )
   TS-N-CHECK TS-EXPECT# ! ;

: TS-ACTUAL! ( a n -- )
   TS-ACTUAL swap TS-CELL ! ;

: TS-EXPECT! ( a n -- )
   TS-EXPECT swap TS-CELL ! ;

: TS-CELL= ( n -- bool ) {: idx:n :}
   TS-EXPECT idx TS-CELL @
   TS-ACTUAL idx TS-CELL @ = ;

: TS-CELLS= ( -- bool )
   0 begin dup TS-EXPECT# @ < while
      dup TS-CELL= 0= if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;

: TS-MATCH? ( -- bool )
   TS-EXPECT# @ TS-ACTUAL# @ <> if 0 0= 0= exit then
   TS-CELLS= ;

: TS-CELLS. ( ptr a n -- ) {: buf:ptr cnt:n :}
   cnt begin dup 0 > while
      1-
      STR-SPACE emit
      buf over TS-CELL @ TREC-N.
   repeat drop ;

: TS-EXPECT. ( -- )
   s" snap: expected " type TS-EXPECT# @ TREC-N.
   s"  cell(s):" type
   TS-EXPECT TS-EXPECT# @ TS-CELLS. cr ;

: TS-ACTUAL. ( -- )
   s" snap: actual " type TS-ACTUAL# @ TREC-N.
   s"  cell(s):" type
   TS-ACTUAL TS-ACTUAL# @ TS-CELLS. cr ;

: TS-JUDGE ( -- )
   T-NEXT
   TS-MATCH? 0= if
      s" snap" T-FAIL-AS
      TS-EXPECT.
      TS-ACTUAL.
   then
   T-LABEL-CLEAR ;

\ Typed depth-introspection comparator (dot habu-typed-depth-introspection-18f0efda),
\ the sole snapshot boundary now that the untyped `T{ code -> expected }T` DSL is
\ retired. `SNAP=` takes the actual and expected computations as two quotations
\ that must produce the SAME row shape S. The checker types the signature
\ ( [ R -- S ] [ R -- S ] -- ) and verifies both quotations leave an identical
\ stack shape at CHECK time, so a shape mismatch such as `[: 1 2 ;] [: 3 ;] SNAP=`
\ is rejected before it ever runs — a guarantee the old `T{ ... }T` never had.
\ The values are compared at runtime through the checked TS-* judge path. Only the
\ depth-marked drain of each quotation's arbitrary output row stays trusted: live
\ `depth` introspection and the capture of an unnamed stack tail cannot yet be
\ typed, so both drains are inlined here into this single trusted word rather than
\ split across extra trusted helpers. Callers must be checked (a quotation is
\ compile-only), so assertions live inside a checked test word:
\   : CASES ( -- ) [: 1 2 + ;] [: 3 ;] SNAP= ... ;
TRUSTED: SNAP= ( [ R -- S ] [ R -- S ] -- )
   swap
   depth 1 - TS-START ! execute
   depth TS-START @ - TS-ACTUAL-N!
   TS-ACTUAL# @ 0 ?do i TS-ACTUAL! loop
   depth 1 - TS-START ! execute
   depth TS-START @ - TS-EXPECT-N!
   TS-EXPECT# @ 0 ?do i TS-EXPECT! loop
   TS-JUDGE ;
