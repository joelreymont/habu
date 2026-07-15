\ cad-num-arithmetic-test.f - the CAD-NUM B5.2 arithmetic boundary + static
\ rejection matrix (dot habu-implement-cad-num-cb413b2a). Run:
\   bin/hb --load lib/cad-num-arithmetic-test.f
\
\ Direct-loaded gate home, like lib/cad-num-types-test.f: CAD-NUM is unsealed and
\ no production entry loads it, so this test is intentionally not in
\ test/gate-stdlib-cases.f and suite-coverage-lint has nothing to schedule.
\
\ Every B5.2 row is exercised at the exact boundary cases named in its table row
\ (zero / safe-max / first-overflow / underflow / misalignment). `numeric-result`
\ is a layout value, so it is only built and MATCHed inside compiled words; each
\ classifier below returns the ok payload's RAW value (nonnegative) or the named
\ E-CADNUM-* refusal code (negative), so one `T=` proves BOTH the exact result
\ value on success and the exact refusal variant on failure. The `*>RAW`
\ projections are test-only white-box readers of a role's cell for value
\ assertions; production never gets a public role->n projection.

require lib/errors.f
require lib/string.f
require lib/test.f
require test/checker-assert.f
require lib/cad-num-types.f
require lib/cad-num-arithmetic.f

\ ---- boundary constants (independently mirror the library bounds) -------------
$7FFFFFFFFFFFFFFF constant T-MAX-N                 \ largest nonnegative cell
T-MAX-N 1 cells / constant T-MAX-CELLS             \ safe-max cell count -> whole bytes
T-MAX-CELLS 1 cells * constant T-MAX-WHOLE-BYTES   \ largest whole-cell byte extent
$4000000000000000 constant T-LARGEST-POW2          \ largest positive power-of-two alignment

\ ---- test-only white-box role readers (reopened package; not production API) --
package CAD-NUM
public
: BL>RAW ( byte-len -- n ) BYTE-LEN>N ;
: IC>RAW ( item-count -- n ) ITEM-COUNT>N ;
: CC>RAW ( cell-count -- n ) CELL-COUNT>N ;
: BO>RAW ( byte-off -- n ) BYTE-OFF>N ;
: CO>RAW ( cell-off -- n ) CELL-OFF>N ;
: IDX>RAW ( index -- n ) INDEX>N ;
;package

\ ---- per-role classifiers: numeric-result<role> -> raw value (ok) / code -------
: BL-CODE ( CAD-NUM:numeric-result<CAD-NUM:byte-len> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF CAD-NUM:BL>RAW ENDOF            negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: IC-CODE ( CAD-NUM:numeric-result<CAD-NUM:item-count> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF CAD-NUM:IC>RAW ENDOF            negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: CC-CODE ( CAD-NUM:numeric-result<CAD-NUM:cell-count> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF CAD-NUM:CC>RAW ENDOF            negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: BO-CODE ( CAD-NUM:numeric-result<CAD-NUM:byte-off> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF CAD-NUM:BO>RAW ENDOF            negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: CO-CODE ( CAD-NUM:numeric-result<CAD-NUM:cell-off> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF CAD-NUM:CO>RAW ENDOF            negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: IDX-CODE ( CAD-NUM:numeric-result<CAD-NUM:index> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF CAD-NUM:IDX>RAW ENDOF           negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;

\ ---- builders: raw n -> validated role (a bad seed aborts the run loudly) ------
: OKBL ( n -- CAD-NUM:byte-len ) CAD-NUM:BYTE-LEN MATCH CAD-NUM:numeric-result
   ok OF ENDOF negative OF 90 throw ENDOF zero OF 90 throw ENDOF overflow OF 90 throw ENDOF
   underflow OF 90 throw ENDOF bad-alignment OF 90 throw ENDOF misaligned OF 90 throw ENDOF ;MATCH ;
: OKIC ( n -- CAD-NUM:item-count ) CAD-NUM:ITEM-COUNT MATCH CAD-NUM:numeric-result
   ok OF ENDOF negative OF 90 throw ENDOF zero OF 90 throw ENDOF overflow OF 90 throw ENDOF
   underflow OF 90 throw ENDOF bad-alignment OF 90 throw ENDOF misaligned OF 90 throw ENDOF ;MATCH ;
: OKCC ( n -- CAD-NUM:cell-count ) CAD-NUM:CELL-COUNT MATCH CAD-NUM:numeric-result
   ok OF ENDOF negative OF 90 throw ENDOF zero OF 90 throw ENDOF overflow OF 90 throw ENDOF
   underflow OF 90 throw ENDOF bad-alignment OF 90 throw ENDOF misaligned OF 90 throw ENDOF ;MATCH ;
: OKBO ( n -- CAD-NUM:byte-off ) CAD-NUM:BYTE-OFF MATCH CAD-NUM:numeric-result
   ok OF ENDOF negative OF 90 throw ENDOF zero OF 90 throw ENDOF overflow OF 90 throw ENDOF
   underflow OF 90 throw ENDOF bad-alignment OF 90 throw ENDOF misaligned OF 90 throw ENDOF ;MATCH ;
: OKCO ( n -- CAD-NUM:cell-off ) CAD-NUM:CELL-OFF MATCH CAD-NUM:numeric-result
   ok OF ENDOF negative OF 90 throw ENDOF zero OF 90 throw ENDOF overflow OF 90 throw ENDOF
   underflow OF 90 throw ENDOF bad-alignment OF 90 throw ENDOF misaligned OF 90 throw ENDOF ;MATCH ;
: OKIDX ( n -- CAD-NUM:index ) CAD-NUM:INDEX MATCH CAD-NUM:numeric-result
   ok OF ENDOF negative OF 90 throw ENDOF zero OF 90 throw ENDOF overflow OF 90 throw ENDOF
   underflow OF 90 throw ENDOF bad-alignment OF 90 throw ENDOF misaligned OF 90 throw ENDOF ;MATCH ;
: OKAL ( n -- CAD-NUM:alignment ) CAD-NUM:ALIGNMENT MATCH CAD-NUM:numeric-result
   ok OF ENDOF negative OF 90 throw ENDOF zero OF 90 throw ENDOF overflow OF 90 throw ENDOF
   underflow OF 90 throw ENDOF bad-alignment OF 90 throw ENDOF misaligned OF 90 throw ENDOF ;MATCH ;
: OKPD ( n -- CAD-NUM:positive-divisor ) CAD-NUM:POSITIVE-DIVISOR MATCH CAD-NUM:numeric-result
   ok OF ENDOF negative OF 90 throw ENDOF zero OF 90 throw ENDOF overflow OF 90 throw ENDOF
   underflow OF 90 throw ENDOF bad-alignment OF 90 throw ENDOF misaligned OF 90 throw ENDOF ;MATCH ;

\ ---- extent + extent: zero identity, maximum-safe sum, first overflow ----------
: RT-ADD ( -- )
   0 OKBL 0 OKBL CAD-NUM:ADD-BYTES BL-CODE 0 T=
   5 OKBL 7 OKBL CAD-NUM:ADD-BYTES BL-CODE 12 T=
   T-MAX-N OKBL 0 OKBL CAD-NUM:ADD-BYTES BL-CODE T-MAX-N T=
   T-MAX-N OKBL 1 OKBL CAD-NUM:ADD-BYTES BL-CODE E-CADNUM-OVERFLOW T=
   0 OKIC 0 OKIC CAD-NUM:ADD-ITEMS IC-CODE 0 T=
   T-MAX-N OKIC 0 OKIC CAD-NUM:ADD-ITEMS IC-CODE T-MAX-N T=
   T-MAX-N OKIC 1 OKIC CAD-NUM:ADD-ITEMS IC-CODE E-CADNUM-OVERFLOW T=
   0 OKCC 0 OKCC CAD-NUM:ADD-CELLS CC-CODE 0 T=
   T-MAX-N OKCC 0 OKCC CAD-NUM:ADD-CELLS CC-CODE T-MAX-N T=
   T-MAX-N OKCC 1 OKCC CAD-NUM:ADD-CELLS CC-CODE E-CADNUM-OVERFLOW T= ;

\ ---- offset + extent advance: zero distance, maximum-safe advance, overflow ----
: RT-ADVANCE ( -- )
   5 OKBO 0 OKBL CAD-NUM:ADVANCE-BYTE-OFF BO-CODE 5 T=
   5 OKBO 7 OKBL CAD-NUM:ADVANCE-BYTE-OFF BO-CODE 12 T=
   T-MAX-N OKBO 0 OKBL CAD-NUM:ADVANCE-BYTE-OFF BO-CODE T-MAX-N T=
   T-MAX-N OKBO 1 OKBL CAD-NUM:ADVANCE-BYTE-OFF BO-CODE E-CADNUM-OVERFLOW T=
   5 OKCO 0 OKCC CAD-NUM:ADVANCE-CELL-OFF CO-CODE 5 T=
   T-MAX-N OKCO 0 OKCC CAD-NUM:ADVANCE-CELL-OFF CO-CODE T-MAX-N T=
   T-MAX-N OKCO 1 OKCC CAD-NUM:ADVANCE-CELL-OFF CO-CODE E-CADNUM-OVERFLOW T=
   5 OKIDX 0 OKIC CAD-NUM:ADVANCE-INDEX IDX-CODE 5 T=
   T-MAX-N OKIDX 0 OKIC CAD-NUM:ADVANCE-INDEX IDX-CODE T-MAX-N T=
   T-MAX-N OKIDX 1 OKIC CAD-NUM:ADVANCE-INDEX IDX-CODE E-CADNUM-OVERFLOW T= ;

\ ---- extent - extent: equal gives zero, positive difference, first underflow ---
: RT-SUB ( -- )
   5 OKBL 5 OKBL CAD-NUM:SUB-BYTES BL-CODE 0 T=
   9 OKBL 4 OKBL CAD-NUM:SUB-BYTES BL-CODE 5 T=
   3 OKBL 4 OKBL CAD-NUM:SUB-BYTES BL-CODE E-CADNUM-UNDERFLOW T=
   5 OKIC 5 OKIC CAD-NUM:SUB-ITEMS IC-CODE 0 T=
   9 OKIC 4 OKIC CAD-NUM:SUB-ITEMS IC-CODE 5 T=
   3 OKIC 4 OKIC CAD-NUM:SUB-ITEMS IC-CODE E-CADNUM-UNDERFLOW T=
   5 OKCC 5 OKCC CAD-NUM:SUB-CELLS CC-CODE 0 T=
   9 OKCC 4 OKCC CAD-NUM:SUB-CELLS CC-CODE 5 T=
   3 OKCC 4 OKCC CAD-NUM:SUB-CELLS CC-CODE E-CADNUM-UNDERFLOW T= ;

\ ---- offset - extent retreat: zero distance, exact-to-zero, first underflow ----
: RT-RETREAT ( -- )
   5 OKBO 0 OKBL CAD-NUM:RETREAT-BYTE-OFF BO-CODE 5 T=
   5 OKBO 5 OKBL CAD-NUM:RETREAT-BYTE-OFF BO-CODE 0 T=
   5 OKBO 6 OKBL CAD-NUM:RETREAT-BYTE-OFF BO-CODE E-CADNUM-UNDERFLOW T=
   5 OKCO 0 OKCC CAD-NUM:RETREAT-CELL-OFF CO-CODE 5 T=
   5 OKCO 5 OKCC CAD-NUM:RETREAT-CELL-OFF CO-CODE 0 T=
   5 OKCO 6 OKCC CAD-NUM:RETREAT-CELL-OFF CO-CODE E-CADNUM-UNDERFLOW T=
   5 OKIDX 0 OKIC CAD-NUM:RETREAT-INDEX IDX-CODE 5 T=
   5 OKIDX 5 OKIC CAD-NUM:RETREAT-INDEX IDX-CODE 0 T=
   5 OKIDX 6 OKIC CAD-NUM:RETREAT-INDEX IDX-CODE E-CADNUM-UNDERFLOW T= ;

\ ---- offset - offset distance: equal zero, positive, reversed underflow --------
: RT-DISTANCE ( -- )
   5 OKBO 5 OKBO CAD-NUM:BYTE-DISTANCE BL-CODE 0 T=
   9 OKBO 4 OKBO CAD-NUM:BYTE-DISTANCE BL-CODE 5 T=
   4 OKBO 9 OKBO CAD-NUM:BYTE-DISTANCE BL-CODE E-CADNUM-UNDERFLOW T=
   5 OKCO 5 OKCO CAD-NUM:CELL-DISTANCE CC-CODE 0 T=
   9 OKCO 4 OKCO CAD-NUM:CELL-DISTANCE CC-CODE 5 T=
   4 OKCO 9 OKCO CAD-NUM:CELL-DISTANCE CC-CODE E-CADNUM-UNDERFLOW T=
   5 OKIDX 5 OKIDX CAD-NUM:INDEX-DISTANCE IC-CODE 0 T=
   9 OKIDX 4 OKIDX CAD-NUM:INDEX-DISTANCE IC-CODE 5 T=
   4 OKIDX 9 OKIDX CAD-NUM:INDEX-DISTANCE IC-CODE E-CADNUM-UNDERFLOW T= ;

\ ---- count x size: zero in either position, maximum-safe product, overflow -----
: RT-MUL ( -- )
   0 OKIC 9 OKIC CAD-NUM:MUL-ITEMS IC-CODE 0 T=
   9 OKIC 0 OKIC CAD-NUM:MUL-ITEMS IC-CODE 0 T=
   6 OKIC 7 OKIC CAD-NUM:MUL-ITEMS IC-CODE 42 T=
   T-MAX-N OKIC 1 OKIC CAD-NUM:MUL-ITEMS IC-CODE T-MAX-N T=
   T-MAX-N OKIC 2 OKIC CAD-NUM:MUL-ITEMS IC-CODE E-CADNUM-OVERFLOW T=
   0 OKBL 9 OKIC CAD-NUM:SCALE-BYTES BL-CODE 0 T=
   9 OKBL 0 OKIC CAD-NUM:SCALE-BYTES BL-CODE 0 T=
   T-MAX-N OKBL 1 OKIC CAD-NUM:SCALE-BYTES BL-CODE T-MAX-N T=
   T-MAX-N OKBL 2 OKIC CAD-NUM:SCALE-BYTES BL-CODE E-CADNUM-OVERFLOW T=
   0 OKCC 9 OKIC CAD-NUM:SCALE-CELLS CC-CODE 0 T=
   9 OKCC 0 OKIC CAD-NUM:SCALE-CELLS CC-CODE 0 T=
   T-MAX-N OKCC 1 OKIC CAD-NUM:SCALE-CELLS CC-CODE T-MAX-N T=
   T-MAX-N OKCC 2 OKIC CAD-NUM:SCALE-CELLS CC-CODE E-CADNUM-OVERFLOW T= ;

\ ---- total division: zero dividend, divisor one, exact, maximum dividend -------
: RT-DIV ( -- )
   0 OKBL 5 OKPD CAD-NUM:DIV-BYTES CAD-NUM:BL>RAW 0 T=
   40 OKBL 1 OKPD CAD-NUM:DIV-BYTES CAD-NUM:BL>RAW 40 T=
   40 OKBL 8 OKPD CAD-NUM:DIV-BYTES CAD-NUM:BL>RAW 5 T=
   T-MAX-N OKBL 1 OKPD CAD-NUM:DIV-BYTES CAD-NUM:BL>RAW T-MAX-N T=
   0 OKIC 5 OKPD CAD-NUM:DIV-ITEMS CAD-NUM:IC>RAW 0 T=
   40 OKIC 1 OKPD CAD-NUM:DIV-ITEMS CAD-NUM:IC>RAW 40 T=
   40 OKIC 8 OKPD CAD-NUM:DIV-ITEMS CAD-NUM:IC>RAW 5 T=
   0 OKCC 5 OKPD CAD-NUM:DIV-CELLS CAD-NUM:CC>RAW 0 T=
   40 OKCC 1 OKPD CAD-NUM:DIV-CELLS CAD-NUM:CC>RAW 40 T=
   40 OKCC 8 OKPD CAD-NUM:DIV-CELLS CAD-NUM:CC>RAW 5 T= ;

\ ---- total remainder: zero dividend, exact zero remainder, nonzero remainder ---
: RT-REM ( -- )
   0 OKBL 5 OKPD CAD-NUM:REM-BYTES CAD-NUM:BL>RAW 0 T=
   40 OKBL 8 OKPD CAD-NUM:REM-BYTES CAD-NUM:BL>RAW 0 T=
   43 OKBL 8 OKPD CAD-NUM:REM-BYTES CAD-NUM:BL>RAW 3 T=
   0 OKIC 5 OKPD CAD-NUM:REM-ITEMS CAD-NUM:IC>RAW 0 T=
   40 OKIC 8 OKPD CAD-NUM:REM-ITEMS CAD-NUM:IC>RAW 0 T=
   43 OKIC 8 OKPD CAD-NUM:REM-ITEMS CAD-NUM:IC>RAW 3 T=
   0 OKCC 5 OKPD CAD-NUM:REM-CELLS CAD-NUM:CC>RAW 0 T=
   40 OKCC 8 OKPD CAD-NUM:REM-CELLS CAD-NUM:CC>RAW 0 T=
   43 OKCC 8 OKPD CAD-NUM:REM-CELLS CAD-NUM:CC>RAW 3 T= ;

\ ---- widen cell -> byte: zero, maximum-safe count, first overflow --------------
: RT-WIDEN ( -- )
   0 OKCC CAD-NUM:CELLS>BYTES BL-CODE 0 T=
   4 OKCC CAD-NUM:CELLS>BYTES BL-CODE 32 T=
   T-MAX-CELLS OKCC CAD-NUM:CELLS>BYTES BL-CODE T-MAX-WHOLE-BYTES T=
   T-MAX-CELLS 1 + OKCC CAD-NUM:CELLS>BYTES BL-CODE E-CADNUM-OVERFLOW T=
   0 OKCO CAD-NUM:CELL-OFF>BYTE-OFF BO-CODE 0 T=
   4 OKCO CAD-NUM:CELL-OFF>BYTE-OFF BO-CODE 32 T=
   T-MAX-CELLS OKCO CAD-NUM:CELL-OFF>BYTE-OFF BO-CODE T-MAX-WHOLE-BYTES T=
   T-MAX-CELLS 1 + OKCO CAD-NUM:CELL-OFF>BYTE-OFF BO-CODE E-CADNUM-OVERFLOW T= ;

\ ---- narrow byte -> cell: zero, exact maximum, first misaligned ----------------
: RT-NARROW ( -- )
   0 OKBL CAD-NUM:BYTES>CELLS CC-CODE 0 T=
   32 OKBL CAD-NUM:BYTES>CELLS CC-CODE 4 T=
   T-MAX-WHOLE-BYTES OKBL CAD-NUM:BYTES>CELLS CC-CODE T-MAX-CELLS T=
   4 OKBL CAD-NUM:BYTES>CELLS CC-CODE E-CADNUM-MISALIGNED T=
   0 OKBO CAD-NUM:BYTE-OFF>CELL-OFF CO-CODE 0 T=
   32 OKBO CAD-NUM:BYTE-OFF>CELL-OFF CO-CODE 4 T=
   T-MAX-WHOLE-BYTES OKBO CAD-NUM:BYTE-OFF>CELL-OFF CO-CODE T-MAX-CELLS T=
   4 OKBO CAD-NUM:BYTE-OFF>CELL-OFF CO-CODE E-CADNUM-MISALIGNED T= ;

\ ---- align-up: zero, alignment one, already-aligned maximum, overflow ----------
: RT-ALIGN ( -- )
   0 OKBL 8 OKAL CAD-NUM:ALIGN-UP-BYTES BL-CODE 0 T=
   5 OKBL 1 OKAL CAD-NUM:ALIGN-UP-BYTES BL-CODE 5 T=
   5 OKBL 8 OKAL CAD-NUM:ALIGN-UP-BYTES BL-CODE 8 T=
   T-MAX-WHOLE-BYTES OKBL 8 OKAL CAD-NUM:ALIGN-UP-BYTES BL-CODE T-MAX-WHOLE-BYTES T=
   T-MAX-WHOLE-BYTES 1 + OKBL 8 OKAL CAD-NUM:ALIGN-UP-BYTES BL-CODE E-CADNUM-OVERFLOW T=
   0 OKBO 8 OKAL CAD-NUM:ALIGN-UP-BYTE-OFF BO-CODE 0 T=
   5 OKBO 1 OKAL CAD-NUM:ALIGN-UP-BYTE-OFF BO-CODE 5 T=
   5 OKBO 8 OKAL CAD-NUM:ALIGN-UP-BYTE-OFF BO-CODE 8 T=
   T-MAX-WHOLE-BYTES OKBO 8 OKAL CAD-NUM:ALIGN-UP-BYTE-OFF BO-CODE T-MAX-WHOLE-BYTES T=
   T-MAX-WHOLE-BYTES 1 + OKBO 8 OKAL CAD-NUM:ALIGN-UP-BYTE-OFF BO-CODE E-CADNUM-OVERFLOW T= ;

\ ---- alignment predicates: zero aligned, exact true, non-exact false -----------
: RT-PRED ( -- )
   0 OKBL 8 OKAL CAD-NUM:BYTES-ALIGNED? TTRUE
   16 OKBL 8 OKAL CAD-NUM:BYTES-ALIGNED? TTRUE
   17 OKBL 8 OKAL CAD-NUM:BYTES-ALIGNED? TFALSE
   5 OKBL 1 OKAL CAD-NUM:BYTES-ALIGNED? TTRUE
   T-MAX-WHOLE-BYTES OKBL T-LARGEST-POW2 OKAL CAD-NUM:BYTES-ALIGNED? TFALSE
   0 OKBO 8 OKAL CAD-NUM:BYTE-OFF-ALIGNED? TTRUE
   16 OKBO 8 OKAL CAD-NUM:BYTE-OFF-ALIGNED? TTRUE
   17 OKBO 8 OKAL CAD-NUM:BYTE-OFF-ALIGNED? TFALSE
   5 OKBO 1 OKAL CAD-NUM:BYTE-OFF-ALIGNED? TTRUE ;

: RT ( -- )
   T-RESET
   RT-ADD  RT-ADVANCE  RT-SUB  RT-RETREAT  RT-DISTANCE  RT-MUL
   RT-DIV  RT-REM  RT-WIDEN  RT-NARROW  RT-ALIGN  RT-PRED
   T-REPORT ;
RT

\ ---- static rejection matrix: correct signatures accept, role misuse rejects ---
\ CHECK-QUIET-CANDIDATE!: -1 accepted, 0 rejected (type error), 1 uncheckable.
: STAT ( -- )
   T-RESET
   \ positives: exact admitted signatures resolve.
   s" GOOD-ADD ( CAD-NUM:byte-len CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD-NUM:ADD-BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" GOOD-DIV-COUNT ( CAD-NUM:item-count CAD-NUM:positive-divisor -- CAD-NUM:item-count ) CAD-NUM:DIV-ITEMS"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" GOOD-ADVANCE ( CAD-NUM:byte-off CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:byte-off> ) CAD-NUM:ADVANCE-BYTE-OFF"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" GOOD-DISTANCE ( CAD-NUM:byte-off CAD-NUM:byte-off -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD-NUM:BYTE-DISTANCE"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ negatives: cross-role divisor, cross-unit factor, index+index, reversed advance.
   s" BAD-DIV-ROLE ( CAD-NUM:item-count CAD-NUM:alignment -- CAD-NUM:item-count ) CAD-NUM:DIV-ITEMS"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-MUL-ROLE ( CAD-NUM:item-count CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:item-count> ) CAD-NUM:MUL-ITEMS"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-INDEX-PLUS-INDEX ( CAD-NUM:index CAD-NUM:index -- CAD-NUM:numeric-result<CAD-NUM:index> ) CAD-NUM:ADVANCE-INDEX"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-ADVANCE-REV ( CAD-NUM:byte-len CAD-NUM:byte-off -- CAD-NUM:numeric-result<CAD-NUM:byte-off> ) CAD-NUM:ADVANCE-BYTE-OFF"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ negatives: raw n where a role is required, cross-unit add.
   s" BAD-RAW-ADD ( n n -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD-NUM:ADD-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-CROSS-ADD ( CAD-NUM:byte-len CAD-NUM:cell-count -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD-NUM:ADD-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   T-REPORT ;
STAT
