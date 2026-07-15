\ typed-storage-test.f - TYPED-VARIABLE / TYPED-BUFFER convenience definers
\ (dot habu-nominal-storage-typed-c5f44d66).
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\   src/habu/verify-source.f test/checker-assert.f test/typed-storage-test.f
\
\ Pins the convenience typed-storage surface: single typed cell (TYPED-VARIABLE)
\ and typed fixed-capacity buffer (TYPED-BUFFER) for nominal scalars, closed
\ layout families, and closed typed pointers. Same-family store/fetch round-trips
\ certify and execute; cross-family / bounds / overflow / rollback reject with a
\ named E-* code. Two capabilities stay DISTINCT: a live LAYOUT-BUFFER over a
\ nominal-parameterized result keeps working, and a raw `variable` still cannot
\ mint a nominal family (the definers are the sound alternative, not a bypass).

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require src/habu/verify-source.f
require test/checker-assert.f

\ named storage-boundary codes (mirrors src/core/layout-buffer.f)
7121 constant E-STORAGE
7122 constant E-STORAGE-BOUNDS
78 constant E-STORAGE-DUP

\ ---- families under test ----------------------------------------------------
\ tsk / tsk2 are arity-0 nominal scalars; tsres a closed non-linear layout
\ family; tsowned a linear layout (inadmissible for storage).
TYPEFAMILY tsk 0
TYPEFAMILY tsk2 0
SUMTYPE tsres 2
   VARIANT ok a ;VARIANT
   VARIANT err b ;VARIANT
;SUMTYPE
DEFLINEAR tslin
SUMTYPE tsowned 0
   VARIANT hold tslin ;VARIANT
;SUMTYPE

\ trusted boundary bridges for live round-trips: every raw cell is a valid
\ nominal id, exactly like test/layout-buffer.f's N>LBTK etc. Layout values are
\ built with the real TSRES:OK constructor (valid tag) and read back with a
\ trusted whole-bundle unmaker (payload deep, tag on top), like LB-UN there.
TRUSTED: N>TSK ( n -- tsk ) ;
TRUSTED: TSK>N ( tsk -- n ) ;
TRUSTED: TSRES-UN ( tsres<n,n> -- n n ) ;
TRUSTED: RES-K-UN ( tsres<tsk,n> -- tsk n ) ;

\ ---- eval helper (define-time reject verdicts) ------------------------------
variable TS-EVAL-A
variable TS-EVAL-U
: TS-EVAL-RUN ( -- ) TS-EVAL-A @ TS-EVAL-U @ INCLUDE-EVALUATE ;
: TS-EVAL ( ptr u8 n -- n ) TS-EVAL-U ! TS-EVAL-A ! [: TS-EVAL-RUN ;] catch ;

\ =============================================================================
\ 1. TYPED-VARIABLE: single nominal cell — checker seam + live round-trip
\ =============================================================================
TYPED-VARIABLE TSV tsk

: TSV-PUT ( n -- ) N>TSK TSV ! ;
: TSV-GET ( -- n ) TSV @ TSK>N ;

: TS-SECTION-VAR-NOMINAL ( -- )
   \ checker seam: same-family store/fetch certify; cross-family + raw reject
   s" A1 ( tsk -- ) TSV !" CHECK-QUIET-CANDIDATE! -1 T=
   s" A2 ( -- tsk ) TSV @" CHECK-QUIET-CANDIDATE! -1 T=
   s" A3 ( tsk2 -- ) TSV !" CHECK-QUIET-CANDIDATE! 0 T=
   s" A4 ( -- tsk2 ) TSV @" CHECK-QUIET-CANDIDATE! 0 T=
   s" A5 ( n -- ) TSV !" CHECK-QUIET-CANDIDATE! 0 T=
   \ raw pointer to the cell can never re-acquire the family (introduction seal)
   s" A6 ( -- ptr tsk ) here" CHECK-QUIET-CANDIDATE! 0 T=
   \ live round-trip: zero image reads as id 0, stored value survives
   TSV-GET 0 T=
   42 TSV-PUT
   TSV-GET 42 T= ;

\ =============================================================================
\ 2. TYPED-BUFFER: nominal, indexed — round-trip + bounds
\ =============================================================================
3 TYPED-BUFFER TSB tsk

: TSB-PUT ( n n -- ) {: v:n i:n :} v N>TSK i TSB ! ;
: TSB-GET ( n -- n ) TSB @ TSK>N ;
: TSB-LOW ( -- ) -1 TSB drop ;
: TSB-HIGH ( -- ) 3 TSB drop ;
: TSB-LOW-RC ( -- n ) [: TSB-LOW ;] catch ;
: TSB-HIGH-RC ( -- n ) [: TSB-HIGH ;] catch ;

: TS-SECTION-BUFFER-NOMINAL ( -- )
   s" B1 ( tsk n -- ) TSB !" CHECK-QUIET-CANDIDATE! -1 T=
   s" B2 ( n -- tsk ) TSB @" CHECK-QUIET-CANDIDATE! -1 T=
   s" B3 ( tsk2 n -- ) TSB !" CHECK-QUIET-CANDIDATE! 0 T=
   \ distinct indices stay independent
   7 0 TSB-PUT  8 1 TSB-PUT  9 2 TSB-PUT
   0 TSB-GET 7 T=
   1 TSB-GET 8 T=
   2 TSB-GET 9 T=
   \ either signed bound throws the named bounds code
   TSB-LOW-RC E-STORAGE-BOUNDS T=
   TSB-HIGH-RC E-STORAGE-BOUNDS T= ;

\ =============================================================================
\ 3. TYPED-VARIABLE: closed typed pointer — checker seam + live deref round-trip
\ =============================================================================
TYPED-VARIABLE TSP ptr tsk

: TSP-STORE ( -- ) TSV TSP ! ;         \ store the address of the TSV nominal cell
: TSP-VIA ( -- n ) TSP @ @ TSK>N ;     \ fetch ptr tsk, deref the tsk, to n

: TS-SECTION-POINTER ( -- )
   s" C1 ( ptr tsk -- ) TSP !" CHECK-QUIET-CANDIDATE! -1 T=
   s" C2 ( -- ptr tsk ) TSP @" CHECK-QUIET-CANDIDATE! -1 T=
   s" C3 ( ptr tsk2 -- ) TSP !" CHECK-QUIET-CANDIDATE! 0 T=
   s" C4 ( -- ptr tsk2 ) TSP @" CHECK-QUIET-CANDIDATE! 0 T=
   \ live: read TSV's value THROUGH the pointer stored in TSP
   99 TSV-PUT
   TSP-STORE
   TSP-VIA 99 T= ;

\ =============================================================================
\ 4. TYPED-VARIABLE / TYPED-BUFFER: closed layout family round-trip
\ =============================================================================
TYPED-VARIABLE TSL tsres<n,n>
2 TYPED-BUFFER TLB tsres<n,n>

: TSL-PUT ( n -- ) TSRES:OK TSL ! ;
: TSL-GET ( -- n ) TSL @ TSRES-UN drop ;
: TLB-PUT ( n n -- ) {: v:n i:n :} v TSRES:OK i TLB ! ;
: TLB-GET ( n -- n ) TLB @ TSRES-UN drop ;

: TS-SECTION-LAYOUT ( -- )
   s" D1 ( tsres<n,n> -- ) TSL !" CHECK-QUIET-CANDIDATE! -1 T=
   s" D2 ( -- tsres<n,n> ) TSL @" CHECK-QUIET-CANDIDATE! -1 T=
   \ scalar-into-layout and layout-family mismatch reject
   s" D3 ( n -- ) TSL !" CHECK-QUIET-CANDIDATE! 0 T=
   s" D4 ( tsk -- ) TSL !" CHECK-QUIET-CANDIDATE! 0 T=
   \ live round-trips
   55 TSL-PUT  TSL-GET 55 T=
   11 0 TLB-PUT  22 1 TLB-PUT
   0 TLB-GET 11 T=
   1 TLB-GET 22 T= ;

\ =============================================================================
\ 5. Define-time admissibility rejects — each throws the named storage code
\ =============================================================================
: TS-SECTION-ADMISSIBILITY ( -- )
   s" TYPED-VARIABLE BAD-OPEN ptr a" TS-EVAL E-STORAGE T=
   s" TYPED-VARIABLE BAD-VAR a" TS-EVAL E-STORAGE T=
   s" TYPED-VARIABLE BAD-QUOT [ n -- n ]" TS-EVAL E-STORAGE T=
   s" TYPED-VARIABLE BAD-LIN tsowned" TS-EVAL E-STORAGE T=
   s" TYPED-VARIABLE BAD-PTR-N ptr n" TS-EVAL E-STORAGE T=
   s" TYPED-VARIABLE BAD-N n" TS-EVAL E-STORAGE T=
   \ overflow: non-positive counts reject
   s" 0 TYPED-BUFFER BAD-ZERO tsk" TS-EVAL E-STORAGE T=
   s" -1 TYPED-BUFFER BAD-NEG tsk" TS-EVAL E-STORAGE T=
   \ none of the rejected names ever reached the dictionary
   s" BAD-OPEN" 0 search-wl 0= TTRUE
   s" BAD-QUOT" 0 search-wl 0= TTRUE
   s" BAD-ZERO" 0 search-wl 0= TTRUE ;

\ =============================================================================
\ 6. Duplicate + transactional rollback
\ =============================================================================
PTR-VARIABLE TS-DP

: TS-DUP-DECL ( -- ) s" TYPED-VARIABLE TSV tsk" INCLUDE-EVALUATE ;
: TS-DUP-RC ( -- n ) [: TS-DUP-DECL ;] catch ;

: TS-SECTION-DUP-ROLLBACK ( -- )
   \ redefining an existing name rejects and rolls the allocation back
   here TS-DP 0 ptr-field !
   TS-DUP-RC E-STORAGE-DUP T=
   here TS-DP 0 ptr-field @ = TTRUE
   \ a rejected admissibility decl also defines nothing and rolls back
   here TS-DP 0 ptr-field !
   s" TYPED-VARIABLE BAD-RB ptr a" TS-EVAL E-STORAGE T=
   here TS-DP 0 ptr-field @ = TTRUE
   s" BAD-RB" 0 search-wl 0= TTRUE ;

\ =============================================================================
\ 7. Distinct capability pin #1: numeric-result over a nominal keeps working
\    through the LANDED LAYOUT-BUFFER (the existing capability is untouched)
\ =============================================================================
3 LAYOUT-BUFFER RES-K-BUF tsres<tsk,n>

: RES-K-PUT ( n n -- ) {: v:n i:n :} v N>TSK TSRES:OK i RES-K-BUF ! ;
: RES-K-GET ( n -- n ) RES-K-BUF @ RES-K-UN drop TSK>N ;

: TS-SECTION-PIN-LAYOUT-BUFFER ( -- )
   \ the landed LAYOUT-BUFFER accessor certifies typed store/fetch ...
   s" E1 ( tsres<tsk,n> n -- ) RES-K-BUF !" CHECK-QUIET-CANDIDATE! -1 T=
   s" E2 ( n -- tsres<tsk,n> ) RES-K-BUF @" CHECK-QUIET-CANDIDATE! -1 T=
   \ ... and executes a live nominal-parameterized round-trip.
   77 0 RES-K-PUT  88 2 RES-K-PUT
   0 RES-K-GET 77 T=
   2 RES-K-GET 88 T= ;

\ =============================================================================
\ 8. Distinct capability pin #2: a raw variable/create/constant still cannot
\    mint a nominal family (TVK-RAW), while TYPED-VARIABLE is the sound path.
\    The raw definers are registered through the verify-source RAW gate — the
\    enforcing path — then a laundering word is checked with the quiet checker.
\ =============================================================================
: TS-REG-RAW ( -- )
   s\" TYPEFAMILY rawk 0\nvariable RAWV\ncreate RAWC 8 allot\n7 constant RAWK"
   VERIFY:SOURCE-BUF-IN-SCOPE ;

: TS-SECTION-PIN-RAW-REJECT ( -- )
   TS-REG-RAW
   \ raw variable/create/constant laundering a nominal family rejects (verdict 0)
   s" R1 ( n -- rawk ) RAWV ! RAWV @" CHECK-QUIET-CANDIDATE! 0 T=
   s" R2 ( n -- rawk ) RAWC ! RAWC @" CHECK-QUIET-CANDIDATE! 0 T=
   s" R3 ( -- rawk ) RAWK" CHECK-QUIET-CANDIDATE! 0 T=
   \ the same raw cell still certifies a plain-scalar round-trip
   s" R4 ( n -- n ) RAWV ! RAWV @" CHECK-QUIET-CANDIDATE! -1 T=
   \ the SOUND alternative — a TYPED-VARIABLE of the same family — certifies
   s" R5 ( tsk -- ) TSV !" CHECK-QUIET-CANDIDATE! -1 T=
   s" R6 ( -- tsk ) TSV @" CHECK-QUIET-CANDIDATE! -1 T= ;

: RUN ( -- )
   T-RESET
   TS-SECTION-VAR-NOMINAL
   TS-SECTION-BUFFER-NOMINAL
   TS-SECTION-POINTER
   TS-SECTION-LAYOUT
   TS-SECTION-ADMISSIBILITY
   TS-SECTION-DUP-ROLLBACK
   TS-SECTION-PIN-LAYOUT-BUFFER
   TS-SECTION-PIN-RAW-REJECT
   T-REPORT ;

RUN
