\ pointer-storage-test.f - focused pointer-slot ownership and effect regression.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/habu/verify-source.f

package POINTER-STORAGE-TEST

PTR-VARIABLE ZERO-SLOT                   \ a declared slot nothing ever writes
create TARGET 0 ,
PTR-VARIABLE SLOT
PERSISTED-PTR-VARIABLE PERSISTED-SLOT
4 PTR-U8-TABLE TABLE-SLOT
PERSISTED-PTR-U8-TABLE-VARIABLE TABLE-HEAD

\ Two reserved engine-layout cells, named by DATA offset; neither is written and
\ nothing depends on what lives at either offset. That they load at all is the
\ first half of the two-row rule: a prefix definer the seal marked DNAME-INT
\ answers `internal engine word` here, not a definition.
0 RESERVED-PTR-U8-CELL RSV-CELL-A
$18 RESERVED-PTR-U8-CELL RSV-CELL-B

: ZERO-PTR ( -- ptr n )
   ZERO-SLOT @ ;

: ADDRESS ( -- ptr ptr n )
   SLOT ;

\ An address is an integer through its distance from NULL-PTR; the offset is the
\ cell's distance less the DATA base's, so no view of either pointer is taken.
: DATA-OFF ( ptr ptr u8 -- n ) NULL-PTR - data-base NULL-PTR - - ;
: RSV-OFF-A ( -- n ) RSV-CELL-A DATA-OFF ;
: RSV-OFF-B ( -- n ) RSV-CELL-B DATA-OFF ;

\ The definer promises only `data-base + the offset it was given`; the
\ difference pins that the offset is the created body's, not a constant baked
\ into the clause.
: RESERVED-RUNTIME ( -- )
   RSV-OFF-A 0 T=
   RSV-OFF-B $18 T=
   RSV-OFF-B RSV-OFF-A - $18 T= ;

: RUNTIME ( -- )
   ADDRESS @ ZERO-PTR = TTRUE
   TARGET ADDRESS !
   ADDRESS @ TARGET = TTRUE ;

: SAMPLE ( -- ptr u8 )
   s" pointer-storage" drop ;

: TABLE-FIELD ( n -- ptr ptr u8 )
   cells TABLE-SLOT + 0 ptr-field ;

: TABLE-RUNTIME ( -- )
   TABLE-SLOT TABLE-HEAD !
   TABLE-HEAD @ TABLE-SLOT = TTRUE
   SAMPLE 0 TABLE-FIELD !
   0 TABLE-FIELD @ SAMPLE = TTRUE ;

: VERIFY-EFFECT ( -- )
   [: s" PTR-VARIABLE VS-PTR : VS-ADDR ( -- ptr ptr n ) VS-PTR ;" VERIFY:SOURCE-BUF ;]
   catch 0 T=
   [: s" PERSISTED-PTR-VARIABLE VPS-PTR : VPS-ADDR ( -- ptr ptr n ) VPS-PTR ;" VERIFY:SOURCE-BUF ;]
   catch 0 T= ;

\ PTR-U8-TABLE and PERSISTED-PTR-U8-TABLE-VARIABLE spell the pointee in the
\ does> clause, so the published effect carries no type variable, `trust-raw`
\ has nothing to seal, and the cell admits exactly the type named, both
\ directions. The raw form is the control: it absorbs a scalar pointee, all
\ DYNAMIC-BUFFER's control head needs; a raw cell refusing a pointer pointee is
\ test/compiler/raw-cell-pointer-refusals.f's case. The pre-scan compiles
\ nothing: DPT, DPH and DPR bind through the checker overlay only while the
\ neutral scope holding the scan is open, so the probes ask
\ VERIFY:CANDIDATE-IN-SCOPE there.
: REG-DECLARED-CELLS ( -- )
   s\" 4 PTR-U8-TABLE DPT\nPERSISTED-PTR-U8-TABLE-VARIABLE DPH\nPTR-VARIABLE DPR"
   VERIFY:SOURCE-BUF-IN-SCOPE ;

: VERIFY-DECLARED-POINTEE ( -- )
   CHECKER-SCOPE-START-NEUTRAL
   REG-DECLARED-CELLS
   s" DP-BASE ( -- ptr ptr u8 ) DPT" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" DP-READ ( -- ptr ptr u8 ) DPH @" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" DP-STORE ( ptr u8 -- ) 0 cells DPT + 0 ptr-field !" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" DP-RAW-SCALAR ( -- ptr u8 ) DPR @" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" DP-BASE-WRONG ( -- ptr ptr n ) DPT" VERIFY:CANDIDATE-IN-SCOPE 0 T=
   s" DP-READ-WRONG ( -- ptr u8 ) DPH @" VERIFY:CANDIDATE-IN-SCOPE 0 T=
   s" DP-STORE-WRONG ( n -- ) 0 cells DPT + 0 ptr-field !" VERIFY:CANDIDATE-IN-SCOPE 0 T=
   s" DP-HEAD-WRONG ( ptr ptr n -- ) DPH !" VERIFY:CANDIDATE-IN-SCOPE 0 T=
   CHECKER-SCOPE-DONE ;

\ `RESERVED-PTR-U8-CELL` is written in src/core/pointer-storage.f before the
\ checker exists, so the seal would mark it DNAME-INT on its own; the two rows
\ are src/core/cell-effects.f (the definer's effect) and
\ src/habu/verify-source.f (what it publishes for the created word).
\ POINTER-STORAGE:DATA-CELL is the control: same file and phase, neither row. It
\ is the real sibling because the seal runs when the engine is built, so a clone
\ here could never be sealed; test/internal-word-gate.f owns what outside source
\ sees of a sealed name. The control candidate declares the created word's
\ effect, so only the name fails it: unresolvable outside the engine (1), not
\ merely refused.
: VERIFY-RESERVED-EFFECT ( -- )
   s" RSV-BASE ( -- ptr ptr u8 ) RSV-CELL-A" CHECK-QUIET-CANDIDATE! -1 T=
   s" RSV-READ ( -- ptr u8 ) RSV-CELL-A @" CHECK-QUIET-CANDIDATE! -1 T=
   s" RSV-BASE-WRONG ( -- ptr ptr n ) RSV-CELL-A" CHECK-QUIET-CANDIDATE! 0 T=
   s" RSV-READ-WRONG ( -- ptr n ) RSV-CELL-A @" CHECK-QUIET-CANDIDATE! 0 T=
   s" RSV-SEALED ( -- ptr ptr u8 ) 0 POINTER-STORAGE:DATA-CELL" CHECK-QUIET-CANDIDATE! 1 T= ;

\ The pre-scan publishes the same effect for the word the definer creates, so a
\ source declaring a reserved cell verifies without being run. The scan compiles
\ nothing: a candidate binds VSRSV only inside the neutral scope holding the
\ scan, and once it closes no VSRSV record exists, so a live candidate cannot
\ bind it (1).
: REG-RESERVED-CELL ( -- )
   s" $30 RESERVED-PTR-U8-CELL VSRSV" VERIFY:SOURCE-BUF-IN-SCOPE ;
: VERIFY-RESERVED-SCAN ( -- )
   CHECKER-SCOPE-START-NEUTRAL
   REG-RESERVED-CELL
   s" VSRSV-BASE ( -- ptr ptr u8 ) VSRSV" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" VSRSV-WRONG ( -- ptr ptr n ) VSRSV" VERIFY:CANDIDATE-IN-SCOPE 0 T=
   CHECKER-SCOPE-DONE
   s" VSRSV-BASE ( -- ptr ptr u8 ) VSRSV" CHECK-QUIET-CANDIDATE! 1 T= ;

: RUN ( -- )
   T-RESET
   RUNTIME
   TABLE-RUNTIME
   VERIFY-EFFECT
   VERIFY-DECLARED-POINTEE
   RESERVED-RUNTIME
   VERIFY-RESERVED-EFFECT
   VERIFY-RESERVED-SCAN
   T-REPORT ;

RUN

;package
