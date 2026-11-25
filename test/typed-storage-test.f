\ typed-storage-test.f - TYPED-VARIABLE and TYPED-BUFFER for nominal scalars,
\ closed layout families, closed typed pointers and quotations: same-family
\ round-trips certify and execute, other uses reject with a named E-* code, a
\ live LAYOUT-BUFFER keeps working and a raw `variable` still cannot mint a
\ nominal family. Closed structural cells are typed-storage-structural-test.f's.

\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/test.f src/lib/fs.f
\   src/habu/verify-source.f test/typed-storage-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/span.f
require src/habu/verify-source.f
require src/habu/layout.f
require src/habu/address-cells.f

E-LAYOUT-BOUNDS constant E-STORAGE-BOUNDS

\ ---- families under test ----------------------------------------------------
\ tsk / tsk2 are arity-0 nominal scalars; tsres a closed non-linear layout
\ family; tsowned a linear layout (inadmissible for storage); tsscr a nominal
\ scalar valid only in the process that made it.
NEWTYPE tsk
NEWTYPE tsk2
NEWTYPE tsscr
SUMTYPE tsres<a,b>
   VARIANT ok a ;VARIANT
   VARIANT err b ;VARIANT
;SUMTYPE
DEFLINEAR tslin
SUMTYPE tsowned
   VARIANT hold tslin ;VARIANT
;SUMTYPE

\ Boundary bridges for live round-trips: every raw cell is a valid nominal id,
\ exactly like test/layout-buffer.f's N>LBTK etc. Layout values are built with
\ the real TSRES:OK constructor (valid tag) and read back by a MATCH that leaves
\ the payload deep and the variant tag on top, like LB-UN there.
\ A test id is any integer; the casts give it the nominal it is stored as.
CAST: N>TSK ( n -- tsk )
CAST: TSK>N ( tsk -- n )
: TSRES-UN ( tsres<n,n> -- n n )
   MATCH tsres ok OF 0 ENDOF err OF 1 ENDOF ;MATCH ;
: RES-K-UN ( tsres<tsk,n> -- tsk n )
   MATCH tsres ok OF 0 ENDOF err OF N>TSK 1 ENDOF ;MATCH ;

\ ---- eval helper (define-time reject verdicts) ------------------------------
TYPED-VARIABLE TS-EVAL-A ^u8
variable TS-EVAL-U
: TS-EVAL-RUN ( -- ) TS-EVAL-A @ TS-EVAL-U @ >str evaluate-closed ;
: TS-EVAL ( str -- n ) dup STR:LEN TS-EVAL-U ! STR:DATA TS-EVAL-A ! [: TS-EVAL-RUN ;] catch ;

\ 1. TYPED-VARIABLE: single nominal cell — checker seam + live round-trip
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
   s" A6 ( -- ^tsk ) here" CHECK-QUIET-CANDIDATE! 0 T=
   \ live round-trip: zero image reads as id 0, stored value survives
   TSV-GET 0 T=
   42 TSV-PUT
   TSV-GET 42 T= ;

\ 2. TYPED-BUFFER: nominal, indexed — round-trip
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
   2 TSB-GET 9 T= ;

\ 3. TYPED-VARIABLE: closed typed pointer — checker seam + live deref round-trip
TYPED-VARIABLE TSP ^tsk
TYPED-VARIABLE TSP-LAYOUT ^SPAN:span<u8>
1 TYPED-BUFFER TSP-SPANS SPAN:span<u8>
7 SPAN-BUFFER: TSP-BYTES

: TSP-STORE ( -- ) TSV TSP ! ;         \ store the address of the TSV nominal cell
: TSP-VIA ( -- n ) TSP @ @ TSK>N ;     \ fetch ^tsk, deref the tsk, to n
: TSP-CLEAR ( -- ) NULL-PTR TSP-LAYOUT ! ;
: TSP-NULL ( -- ^SPAN:span<u8> ) NULL-PTR ;

: TS-SECTION-POINTER ( -- )
   s" C1 ( ^tsk -- ) TSP !" CHECK-QUIET-CANDIDATE! -1 T=
   s" C2 ( -- ^tsk ) TSP @" CHECK-QUIET-CANDIDATE! -1 T=
   s" C3 ( ^tsk2 -- ) TSP !" CHECK-QUIET-CANDIDATE! 0 T=
   s" C4 ( -- ^tsk2 ) TSP @" CHECK-QUIET-CANDIDATE! 0 T=
   s" C5 ( ^SPAN:span<n> -- ) TSP-LAYOUT !" CHECK-QUIET-CANDIDATE! 0 T=
   s" C6 ( -- ^SPAN:span<n> ) TSP-LAYOUT @" CHECK-QUIET-CANDIDATE! 0 T=
   s" C7 ( ^u8 n -- ^^SPAN:span<u8> ) ptr-field" CHECK-QUIET-CANDIDATE! 0 T=
   s" C8 ( n -- ^SPAN:span<u8> ) NULL-PTR +" CHECK-QUIET-CANDIDATE! 0 T=
   \ live: read TSV's value THROUGH the pointer stored in TSP
   99 TSV-PUT
   TSP-STORE
   TSP-VIA 99 T=
   \ Read through a borrowed layout-array pointer, then clear it after use.
   TSP-BYTES 0 TSP-SPANS !
   0 TSP-SPANS TSP-LAYOUT !
   TSP-LAYOUT @ @ SPAN:LEN 7 T=
   TSP-CLEAR
   TSP-LAYOUT @ NULL-PTR = TTRUE
   TSP-NULL NULL-PTR = TTRUE ;

\ 4. TYPED-VARIABLE / TYPED-BUFFER: closed layout family round-trip
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

\ 5. Define-time admissibility rejects — a stored type is the checker's refusal,
\    and a count outside the buffer's extent is rejected at its token
: TS-SECTION-ADMISSIBILITY ( -- )
   s" TYPED-VARIABLE BAD-OPEN ^a" TS-EVAL 70 T=
   s" TYPED-VARIABLE BAD-VAR a" TS-EVAL 70 T=
   \ a well-formed xt<effect> quotation cell is admissible
   \ (test/xt-cell-test.f); a malformed one (missing --) rejects
   s" TYPED-VARIABLE BAD-QUOT [ n n ]" TS-EVAL 70 T=
   s" TYPED-VARIABLE BAD-LIN tsowned" TS-EVAL 70 T=
   \ overflow: non-positive counts reject
   s" 0 TYPED-BUFFER BAD-ZERO tsk" TS-EVAL 70 T=
   s" -1 TYPED-BUFFER BAD-NEG tsk" TS-EVAL 70 T=
   \ none of the rejected names ever reached the dictionary
   s" BAD-OPEN" 0 search-wl 0= TTRUE
   s" BAD-QUOT" 0 search-wl 0= TTRUE
   s" BAD-ZERO" 0 search-wl 0= TTRUE ;

\ 6. TYPED-VARIABLE and TYPED-BUFFER declared SCRATCH-ONLY, storage a build
\    leaves out, store and fetch their type
TYPED-VARIABLE TSS tsscr SCRATCH-ONLY
2 TYPED-BUFFER TSSB tsscr SCRATCH-ONLY

CAST: N>TSSCR ( n -- tsscr )
CAST: TSSCR>N ( tsscr -- n )

: TS-SECTION-SCRATCH ( -- )
   5 N>TSSCR TSS !  TSS @ TSSCR>N 5 T=
   6 N>TSSCR 1 TSSB !  1 TSSB @ TSSCR>N 6 T= ;

\ 7. Duplicate + transactional rollback
PTR-VARIABLE TS-DP

: TS-DUP-DECL ( -- ) s" TYPED-VARIABLE TSV tsk" evaluate-closed ;
: TS-DUP-RC ( -- n ) [: TS-DUP-DECL ;] catch ;

: TS-SECTION-DUP-ROLLBACK ( -- )
   \ redefining an existing name rejects and rolls the allocation back
   here TS-DP 0 ptr-field !
   REJECTION:CAPTURE  TS-DUP-RC 70 T=
   REJECTION:BUFFER$ s" error[E-DUPLICATE-DEFINITION]" STR:CONTAINS? TTRUE
   REJECTION:BUFFER$ s" `TSV`" STR:CONTAINS? TTRUE
   REJECTION:BUFFER-OFF
   here TS-DP 0 ptr-field @ = TTRUE
   \ a rejected admissibility decl also defines nothing and rolls back
   here TS-DP 0 ptr-field !
   s" TYPED-VARIABLE BAD-RB ^a" TS-EVAL 70 T=
   here TS-DP 0 ptr-field @ = TTRUE
   s" BAD-RB" 0 search-wl 0= TTRUE ;

\ 8. A result family over a nominal is stored and fetched through LAYOUT-BUFFER
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

\ The verifier classes an element at its type's own width, so a field past
\ the first cell that mixes kinds is rejected as a load rejects it.
: TS-VERIFY-WIDE ( -- )
   s\" SUMTYPE tsamix VARIANT ref ^n ;VARIANT VARIANT num n ;VARIANT ;SUMTYPE\nSTRUCTURE tswide FIELD f1 n FIELD z tsamix ;STRUCTURE\n1 TYPED-BUFFER TSW-ROW tswide"
   VERIFY:SOURCE-BUF-IN-SCOPE ;

: TS-SECTION-VERIFY-MIXED ( -- )
   REJECTION:CAPTURE  [: TS-VERIFY-WIDE ;] catch 70 T=
   REJECTION:BUFFER$ s" error[E-BAD-STORAGE]" STR:CONTAINS? TTRUE
   REJECTION:BUFFER$ s" in cell 1 of an element" STR:CONTAINS? TTRUE
   REJECTION:BUFFER-OFF ;

\ 9. Distinct capability pin #2: a raw variable/create/constant still cannot
\    mint a nominal family (TVK-RAW), while TYPED-VARIABLE is the sound path.
\    The raw definers are registered through the verify-source RAW gate — the
\    enforcing path. That scan compiles nothing: RAWV, RAWC and RAWK bind
\    through the checker overlay only while the neutral scope holding the scan
\    and its probes is open, so a laundering word naming them is asked there,
\    on the certify path (VERIFY:CANDIDATE-IN-SCOPE).
: TS-REG-RAW ( -- )
   s\" NEWTYPE rawk\nvariable RAWV\ncreate RAWC 8 allot\n7 constant RAWK"
   VERIFY:SOURCE-BUF-IN-SCOPE ;

: TS-SECTION-PIN-RAW-REJECT ( -- )
   [: TS-REG-RAW
   \ raw variable/create/constant laundering a nominal family rejects (verdict 0)
   s" R1 ( n -- rawk ) RAWV ! RAWV @" VERIFY:CANDIDATE-IN-SCOPE 0 T=
   s" R2 ( n -- rawk ) RAWC ! RAWC @" VERIFY:CANDIDATE-IN-SCOPE 0 T=
   s" R3 ( -- rawk ) RAWK" VERIFY:CANDIDATE-IN-SCOPE 0 T=
   \ the same raw cell and constant still certify a plain scalar
   s" R4 ( n -- n ) RAWV ! RAWV @" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" R7 ( -- n ) RAWK" VERIFY:CANDIDATE-IN-SCOPE -1 T= ;] CHECK:SCOPED
   \ the SOUND alternative — a TYPED-VARIABLE of the same family, a live word —
   \ certifies where compiled code asks
   s" R5 ( tsk -- ) TSV !" CHECK-QUIET-CANDIDATE! -1 T=
   s" R6 ( -- tsk ) TSV @" CHECK-QUIET-CANDIDATE! -1 T= ;

\ Declaration, including a null cell, owns the relocation row. Ordinary cells
\ must not gain the code kind, and a later store must reuse the declared row.
: TS-ADDRESS-ROWS ( -- n ) ADDRESS-CELLS:LIVE-SPAN nip ;

TS-ADDRESS-ROWS constant TS-BEFORE-QUOT
TYPED-VARIABLE TS-CALLBACK [ n -- n ]
TS-ADDRESS-ROWS TS-BEFORE-QUOT - constant TS-QUOT-ROWS
TS-ADDRESS-ROWS constant TS-BEFORE-QUOTS
3 TYPED-BUFFER TS-CALLBACKS [ n -- n ]
TS-ADDRESS-ROWS TS-BEFORE-QUOTS - constant TS-QUOTS-ROWS
TS-ADDRESS-ROWS constant TS-BEFORE-NUMBER
TYPED-VARIABLE TS-NUMBER n
TS-ADDRESS-ROWS TS-BEFORE-NUMBER - constant TS-NUMBER-ROWS

: TS-STORE-CALLBACK ( -- ) [: 1 + ;] TS-CALLBACK ! ;

: TS-SECTION-QUOT-DECLARATION ( -- )
   s" typed quotation storage declares every zero code cell" T-LABEL
   TS-QUOT-ROWS 1 T= TS-QUOTS-ROWS 3 T= TS-NUMBER-ROWS 0 T=
   TS-CALLBACK BYTE-VIEW CELL-VIEW @ 0 T=
   TS-ADDRESS-ROWS {: before:n :}
   TS-STORE-CALLBACK
   TS-ADDRESS-ROWS before T=
   41 TS-CALLBACK @ execute 42 T= ;

: RUN ( -- )
   T-RESET
   TS-SECTION-VAR-NOMINAL
   TS-SECTION-BUFFER-NOMINAL
   TS-SECTION-POINTER
   TS-SECTION-LAYOUT
   TS-SECTION-ADMISSIBILITY
   TS-SECTION-SCRATCH
   TS-SECTION-DUP-ROLLBACK
   TS-SECTION-PIN-LAYOUT-BUFFER
   TS-SECTION-VERIFY-MIXED
   TS-SECTION-PIN-RAW-REJECT
   TS-SECTION-QUOT-DECLARATION
   T-REPORT ;

RUN

false [if] \ off until dot habu-add-the-checks-a49d054d
\ either signed bound throws the named bounds code
TSB-LOW-RC E-STORAGE-BOUNDS T=
TSB-HIGH-RC E-STORAGE-BOUNDS T=
T-REPORT
[then]
