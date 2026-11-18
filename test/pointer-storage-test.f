split-str
\ pointer-storage-test.f - focused pointer-slot ownership and effect regression.

require src/lib/error.f
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
SCRATCH-VARIABLE SCR-CELL
16 SCRATCH-BUFFER: SCR-ROW

\ Two reserved engine-layout cells, named by DATA offset; neither is written and
\ nothing depends on what lives at either offset.
0 RESERVED-PTR-U8-CELL RSV-CELL-A
$18 RESERVED-PTR-U8-CELL RSV-CELL-B

: ZERO-PTR ( -- ^n )
   ZERO-SLOT @ ;

: ADDRESS ( -- ^^n )
   SLOT ;

\ An address is an integer through its distance from NULL-PTR; the offset is the
\ cell's distance less the DATA base's, so no view of either pointer is taken.
: DATA-OFF ( ^^u8 -- n ) NULL-PTR - data-base NULL-PTR - - ;
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

: SCRATCH-RUNTIME ( -- )
   42 SCR-CELL !  SCR-CELL @ 42 T=
   65 SCR-ROW 15 + c!  SCR-ROW 15 + c@ 65 T= ;

: SAMPLE ( -- ^u8 )
   s" pointer-storage" drop ;

: TABLE-FIELD ( n -- ^^u8 )
   cells TABLE-SLOT + 0 ptr-field ;

: TABLE-RUNTIME ( -- )
   TABLE-SLOT TABLE-HEAD !
   TABLE-HEAD @ TABLE-SLOT = TTRUE
   SAMPLE 0 TABLE-FIELD !
   0 TABLE-FIELD @ SAMPLE = TTRUE ;

: VERIFY-EFFECT ( -- )
   [: s" PTR-VARIABLE VS-PTR : VS-ADDR ( -- ^^n ) VS-PTR ;" VERIFY:SOURCE-BUF ;]
   catch 0 T=
   [: s" PERSISTED-PTR-VARIABLE VPS-PTR : VPS-ADDR ( -- ^^n ) VPS-PTR ;" VERIFY:SOURCE-BUF ;]
   catch 0 T=
   [: s" SCRATCH-VARIABLE VS-SCR : VS-SCR-A ( -- ^n ) VS-SCR ;" VERIFY:SOURCE-BUF ;]
   catch 0 T=
   [: s" SCRATCH-VARIABLE VS-SCR2 : VS-SCR2-A ( -- ^^u8 ) VS-SCR2 ;" VERIFY:SOURCE-BUF ;]
   catch 0<> TTRUE ;

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

: VERIFY-DECLARED-POINTEE ( -- ) [:
   REG-DECLARED-CELLS
   s" DP-BASE ( -- ^^u8 ) DPT" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" DP-READ ( -- ^^u8 ) DPH @" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" DP-STORE ( ^u8 -- ) 0 cells DPT + 0 ptr-field !" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" DP-RAW-SCALAR ( -- ^u8 ) DPR @" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" DP-BASE-WRONG ( -- ^^n ) DPT" VERIFY:CANDIDATE-IN-SCOPE 0 T=
   s" DP-READ-WRONG ( -- ^u8 ) DPH @" VERIFY:CANDIDATE-IN-SCOPE 0 T=
   s" DP-STORE-WRONG ( n -- ) 0 cells DPT + 0 ptr-field !" VERIFY:CANDIDATE-IN-SCOPE 0 T=
   s" DP-HEAD-WRONG ( ^^n -- ) DPH !" VERIFY:CANDIDATE-IN-SCOPE 0 T=
   ;] CHECK:SCOPED ;

\ RESERVED-PTR-U8-CELL's created word has the effect its does> states.
: VERIFY-RESERVED-EFFECT ( -- )
   s" RSV-BASE ( -- ^^u8 ) RSV-CELL-A" CHECK-QUIET-CANDIDATE! -1 T=
   s" RSV-READ ( -- ^u8 ) RSV-CELL-A @" CHECK-QUIET-CANDIDATE! -1 T=
   s" RSV-BASE-WRONG ( -- ^^n ) RSV-CELL-A" CHECK-QUIET-CANDIDATE! 0 T=
   s" RSV-READ-WRONG ( -- ^n ) RSV-CELL-A @" CHECK-QUIET-CANDIDATE! 0 T= ;

\ The pre-scan publishes the same effect for the word the definer creates, so a
\ source declaring a reserved cell verifies without being run. The scan compiles
\ nothing: a candidate binds VSRSV only inside the neutral scope holding the
\ scan, and once it closes no VSRSV record exists, so a live candidate cannot
\ bind it (0).
: REG-RESERVED-CELL ( -- )
   s" $30 RESERVED-PTR-U8-CELL VSRSV" VERIFY:SOURCE-BUF-IN-SCOPE ;
: VERIFY-RESERVED-SCAN ( -- ) [:
   REG-RESERVED-CELL
   s" VSRSV-BASE ( -- ^^u8 ) VSRSV" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" VSRSV-WRONG ( -- ^^n ) VSRSV" VERIFY:CANDIDATE-IN-SCOPE 0 T=
   ;] CHECK:SCOPED
   s" VSRSV-BASE ( -- ^^u8 ) VSRSV" CHECK-QUIET-CANDIDATE! 0 T= ;

: RUN ( -- )
   T-RESET
   RUNTIME
   SCRATCH-RUNTIME
   TABLE-RUNTIME
   VERIFY-EFFECT
   VERIFY-DECLARED-POINTEE
   RESERVED-RUNTIME
   VERIFY-RESERVED-EFFECT
   VERIFY-RESERVED-SCAN
   T-REPORT ;

RUN

;package
