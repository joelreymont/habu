\ pointer-storage-test.f - focused pointer-slot ownership and effect regression.
\ Run: bin/hb --load test/pointer-storage-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require src/habu/verify-source.f
require test/checker-assert.f

package POINTER-STORAGE-TEST

PTR-VARIABLE ZERO-SLOT                   \ a declared slot nothing ever writes
create TARGET 0 ,
PTR-VARIABLE SLOT
PERSISTED-PTR-VARIABLE PERSISTED-SLOT
4 PTR-U8-TABLE TABLE-SLOT
PERSISTED-PTR-U8-TABLE-VARIABLE TABLE-HEAD

\ Reserved engine-layout cells, named by their DATA offset. Two of them, so the
\ runtime case below can read the offset arithmetic itself rather than a layout
\ meaning: nothing here depends on what lives at either offset, and neither is
\ ever written. That these two lines LOAD AT ALL is the first half of the
\ two-row rule — a prefix definer the seal marked DNAME-INT answers `internal
\ engine word` here, not a definition.
0 RESERVED-PTR-U8-CELL RSV-CELL-A
$18 RESERVED-PTR-U8-CELL RSV-CELL-B

: ZERO-PTR ( -- ptr n )
   ZERO-SLOT @ ;

: ADDRESS ( -- ptr ptr n )
   SLOT ;

TRUSTED: RSV-OFF-A ( -- n ) RSV-CELL-A data-base - ;
TRUSTED: RSV-OFF-B ( -- n ) RSV-CELL-B data-base - ;

\ The definer answers `data-base + the offset it was given`, which is the whole
\ of what it promises; the difference pins that the offset is the created
\ body's and not a constant baked into the clause.
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

\ A declared table is ordinary storage at runtime: the base goes into the head
\ and a byte pointer round-trips through a cell of it.
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

\ Declared-pointee storage (dot habu-refuse-a-ptr-5ad2734e). PTR-U8-TABLE and
\ PERSISTED-PTR-U8-TABLE-VARIABLE spell the pointee in the does> clause, so the
\ published effect carries no type variable, `trust-raw` has nothing to seal, and
\ the cell holds EXACTLY the type named: it admits that type and refuses every
\ other one, in both directions. The raw form is the control - it keeps absorbing
\ a scalar pointee, which is all DYNAMIC-BUFFER's control head ever needs. That a
\ raw cell REFUSES a pointer pointee is the rule's own fixture
\ (test/compiler/raw-cell-pointer-refusals.f), not this file's: the rule lands
\ after these conversions, and this file must read the same before and after it.
\ The pre-scan registers DPT, DPH and DPR for the certify path alone - the
\ engine holds no record of them - so the probes ask that path
\ (VERIFY:CANDIDATE-IN-SCOPE); VERIFY-RESERVED-SCAN below shows both answers.
: REG-DECLARED-CELLS ( -- )
   s\" 4 PTR-U8-TABLE DPT\nPERSISTED-PTR-U8-TABLE-VARIABLE DPH\nPTR-VARIABLE DPR"
   VERIFY:SOURCE-BUF-IN-SCOPE ;

: VERIFY-DECLARED-POINTEE ( -- )
   REG-DECLARED-CELLS
   s" DP-BASE ( -- ptr ptr u8 ) DPT" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" DP-READ ( -- ptr ptr u8 ) DPH @" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" DP-STORE ( ptr u8 -- ) 0 cells DPT + 0 ptr-field !" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" DP-RAW-SCALAR ( -- ptr u8 ) DPR @" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" DP-BASE-WRONG ( -- ptr ptr n ) DPT" VERIFY:CANDIDATE-IN-SCOPE 0 T=
   s" DP-READ-WRONG ( -- ptr u8 ) DPH @" VERIFY:CANDIDATE-IN-SCOPE 0 T=
   s" DP-STORE-WRONG ( n -- ) 0 cells DPT + 0 ptr-field !" VERIFY:CANDIDATE-IN-SCOPE 0 T=
   s" DP-HEAD-WRONG ( ptr ptr n -- ) DPH !" VERIFY:CANDIDATE-IN-SCOPE 0 T= ;

\ THE TWO ROWS, AND THE PAIR THAT SHOWS WHAT THEY DO. `RESERVED-PTR-U8-CELL`
\ is written in src/core/pointer-storage.f, before the checker exists, so the
\ seal would mark it DNAME-INT on its own; src/core/cell-effects.f states the
\ definer's effect and src/habu/verify-source.f states what it publishes for the
\ created word. NULL-PTR-CELL is the control: same file, same phase, neither
\ row, and checked source cannot name it. (The control is the real sibling
\ rather than a clone of the definer because the seal runs when the engine is
\ built - a clone written in this file is ordinary checked source and could
\ never be sealed. test/internal-word-gate.f owns the `internal engine word`
\ diagnostic itself.) The control candidate is spelled with NULL-PTR-CELL's OWN
\ effect, so a type disagreement cannot be what fails it: the name is. The
\ product image keeps no NULL-PTR-CELL record (`: X NULL-PTR-CELL ;` is
\ E-UNDEFINED, in a trusted body too), and a candidate binds the record the
\ compiler binds, so the control is unresolvable (1), not refused.
: VERIFY-RESERVED-EFFECT ( -- )
   s" RSV-BASE ( -- ptr ptr u8 ) RSV-CELL-A" CHECK-QUIET-CANDIDATE! -1 T=
   s" RSV-READ ( -- ptr u8 ) RSV-CELL-A @" CHECK-QUIET-CANDIDATE! -1 T=
   s" RSV-BASE-WRONG ( -- ptr ptr n ) RSV-CELL-A" CHECK-QUIET-CANDIDATE! 0 T=
   s" RSV-READ-WRONG ( -- ptr n ) RSV-CELL-A @" CHECK-QUIET-CANDIDATE! 0 T=
   s" RSV-SEALED ( -- ptr n ) NULL-PTR-CELL" CHECK-QUIET-CANDIDATE! 1 T= ;

\ The scanner's own row: the pre-scan sees the same definer and publishes the
\ same effect for the word it creates, so a source file that declares a
\ reserved cell verifies without being run. That effect is a fact of the
\ certify path: the scan compiles nothing, so the engine holds no VSRSV record
\ and a live candidate cannot bind it (1) even with VSRSV the scan's last
\ registration - a replay leaves no pending definition behind - while the same
\ candidate on the certify path binds the scanned row.
: REG-RESERVED-CELL ( -- )
   s" $30 RESERVED-PTR-U8-CELL VSRSV" VERIFY:SOURCE-BUF-IN-SCOPE ;
: VERIFY-RESERVED-SCAN ( -- )
   REG-RESERVED-CELL
   s" VSRSV-BASE ( -- ptr ptr u8 ) VSRSV" CHECK-QUIET-CANDIDATE! 1 T=
   s" VSRSV-BASE ( -- ptr ptr u8 ) VSRSV" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" VSRSV-WRONG ( -- ptr ptr n ) VSRSV" VERIFY:CANDIDATE-IN-SCOPE 0 T= ;

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
