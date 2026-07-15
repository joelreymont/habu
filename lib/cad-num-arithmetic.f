\ cad-num-arithmetic.f - CAD-NUM B5.2 closed, dimensionally-valid arithmetic
\ (MODEL-CAD-V2-PLAN.md B5.2; dot habu-implement-cad-num-cb413b2a, epic
\ habu-epic-model-cad-70b629a9). Reopens package CAD-NUM (slice 2); depends on
\ lib/cad-num-types.f (slice 1) and adds no roles or overloads.
\
\ This file is the COMPLETE admitted role algebra. The B5.2 table is closed: an
\ omitted, reversed, or cross-unit pair is statically unavailable, because the
\ only inputs each word accepts are its exact nominal roles. There is no generic
\ raw-`n` arithmetic and no extra overload; the type system IS the table. New
\ combinations require a consumer, a unit proof, and a plan amendment - they are
\ never inferred from the shared one-cell representation.
\
\ Result discipline: a role-changing operation that can fail numerically returns
\ `numeric-result<role>` - `ok role` on success, or a payloadless `overflow`
\ (extent/offset/product/round-up past the machine max), `underflow` (a
\ subtraction/retreat/reversed-distance below zero), `misaligned` (a byte
\ extent/offset that is not a whole cell count), or `zero` (an extent-by-extent
\ division whose unit-size divisor is a zero-length extent). Failure is a VALUE,
\ never a throw; consumers throw the matching `E-CADNUM-*` code at their own
\ boundary. The total positive-divisor DIV/REM operations take the
\ `positive-divisor` role, so a zero divisor is STATICALLY impossible: they are
\ total and return their role directly with no error variant. The extent-by-extent
\ DIV-BYTES-CEIL/-FLOOR unit counters instead take a `byte-len` unit size - an
\ extent, which admits zero - so a zero-size unit cannot be excluded by the type
\ and is returned as the `zero` value; the quotient is at most the dividend
\ extent, so it never overflows.
\
\ Minting policy (dot audit): arithmetic mints NO role itself. Every success
\ value is routed through slice 1's public validator (`BYTE-LEN`, `ITEM-COUNT`,
\ `CELL-COUNT`, `INDEX`, `BYTE-OFF`, `CELL-OFF`), which owns the audited
\ `MINT-*`; the raw kernels only decide overflow/underflow/misalignment BEFORE
\ the validated value is built. The only new audited boundaries here are the
\ six proof-erasure projections that read a role's raw cell for the arithmetic
\ (slice 1 shipped `BYTE-LEN>N` and `CELL-COUNT>N`; the other six families need
\ the same read). They have no public inverse and are retired with TVK-RAW
\ (dot habu-nominal-storage-raw-a3430ef2), exactly as the slice 1 projections.
\
\ No `require lib/memory.f`: MEM:ALLOC-* consumes CAD-NUM alloc roles, so a
\ dependency would be a cycle; CELL-BYTES-N mirrors the machine cell size that
\ lib/memory.f also names as MEM-CELL-BYTES, and MAX-CELL-N is slice 1's bound.

require lib/cad-num-types.f

package CAD-NUM
private

\ ---- internal invariant code (never reachable; see OK-* extractors) -----------
-5406 constant E-CADNUM-TOTALITY   \ a total op's validator saw a non-ok result

\ ---- machine cell size (mirrors lib/memory.f MEM-CELL-BYTES; cycle-free) -------
1 cells constant CELL-BYTES-N

\ ---- proof-erasure projections: role -> raw cell (no public inverse) ----------
\ Read a validated role's cell so the raw kernels can do the arithmetic. Slice 1
\ already ships BYTE-LEN>N / CELL-COUNT>N; these six complete the set. Private,
\ audited, and confined by tools/refine-lint.f like the slice 1 projections.
TRUSTED: ITEM-COUNT>N ( item-count -- n ) ;
TRUSTED: INDEX>N ( index -- n ) ;
TRUSTED: BYTE-OFF>N ( byte-off -- n ) ;
TRUSTED: CELL-OFF>N ( cell-off -- n ) ;
TRUSTED: ALIGNMENT>N ( alignment -- n ) ;
TRUSTED: POSITIVE-DIVISOR>N ( positive-divisor -- n ) ;

\ ---- payloadless failure constructors (readable over the escaped ctor names) --
: A-OVER     ( -- numeric-result<a> ) CAD--NUM-NUMERIC--RESULT:OVERFLOW ;
: A-UNDER    ( -- numeric-result<a> ) CAD--NUM-NUMERIC--RESULT:UNDERFLOW ;
: A-MISALIGN ( -- numeric-result<a> ) CAD--NUM-NUMERIC--RESULT:MISALIGNED ;
: A-ZERO     ( -- numeric-result<a> ) CAD--NUM-NUMERIC--RESULT:ZERO ;

\ ---- raw checked scalar kernels (nonnegative cells; role-free) -----------------
\ Each returns the failure predicate as a real bool from a comparison; the caller
\ recomputes the value only on the success path (through a validator).
: SUM-OVF? ( n n -- bool )              \ does a+b exceed the machine max?
   {: a:n b:n :} a MAX-CELL-N b - > ;
: DIFF-UNF? ( n n -- bool )             \ does a-b drop below zero?
   {: a:n b:n :} a b < ;
: PROD-OVF? ( n n -- bool )             \ does a*b exceed the machine max?
   {: a:n b:n :} b 0= if 0 0 > exit then a MAX-CELL-N b / > ;
: WHOLE? ( n n -- bool )                \ is v a whole multiple of unit?
   {: v:n u:n :} v u mod 0= ;
: ALIGNED? ( n n -- bool )              \ is v aligned to power-of-two a?
   {: v:n a:n :} v a 1- and 0= ;
: ROUND-UP ( n n -- n bool )            \ round v up to power-of-two a; value + overflow
   {: v:n a:n :}
   v a 1- and 0= if v  0 0 >  exit then          \ already aligned: no round-up, no overflow
   a  v a 1- and  -  {: delta:n :}                \ distance to the next multiple (1..a-1)
   v delta +  v MAX-CELL-N delta - >  ;           \ rounded value, overflow when v+delta > max
: CEIL-QUOT ( n n -- n )                \ ceil(v / u) for u>0, v>=0 (0 when v=0)
   {: v:n u:n :} v 0= if 0 exit then v 1- u / 1+ ;

\ ---- ok extractors for the total ops (the non-ok arms are proven unreachable) --
: OK-BYTE-LEN ( numeric-result<byte-len> -- byte-len )
   MATCH numeric-result
      ok OF ENDOF                            negative OF E-CADNUM-TOTALITY throw ENDOF
      zero OF E-CADNUM-TOTALITY throw ENDOF  overflow OF E-CADNUM-TOTALITY throw ENDOF
      underflow OF E-CADNUM-TOTALITY throw ENDOF
      bad-alignment OF E-CADNUM-TOTALITY throw ENDOF
      misaligned OF E-CADNUM-TOTALITY throw ENDOF
   ;MATCH ;
: OK-ITEM-COUNT ( numeric-result<item-count> -- item-count )
   MATCH numeric-result
      ok OF ENDOF                            negative OF E-CADNUM-TOTALITY throw ENDOF
      zero OF E-CADNUM-TOTALITY throw ENDOF  overflow OF E-CADNUM-TOTALITY throw ENDOF
      underflow OF E-CADNUM-TOTALITY throw ENDOF
      bad-alignment OF E-CADNUM-TOTALITY throw ENDOF
      misaligned OF E-CADNUM-TOTALITY throw ENDOF
   ;MATCH ;
: OK-CELL-COUNT ( numeric-result<cell-count> -- cell-count )
   MATCH numeric-result
      ok OF ENDOF                            negative OF E-CADNUM-TOTALITY throw ENDOF
      zero OF E-CADNUM-TOTALITY throw ENDOF  overflow OF E-CADNUM-TOTALITY throw ENDOF
      underflow OF E-CADNUM-TOTALITY throw ENDOF
      bad-alignment OF E-CADNUM-TOTALITY throw ENDOF
      misaligned OF E-CADNUM-TOTALITY throw ENDOF
   ;MATCH ;

public

\ ---- extent + extent (overflow-checked) ---------------------------------------
: ADD-BYTES ( byte-len byte-len -- numeric-result<byte-len> )
   {: a:byte-len b:byte-len :}
   a BYTE-LEN>N b BYTE-LEN>N {: x:n y:n :}
   x y SUM-OVF? if A-OVER else x y + BYTE-LEN then ;
: ADD-ITEMS ( item-count item-count -- numeric-result<item-count> )
   {: a:item-count b:item-count :}
   a ITEM-COUNT>N b ITEM-COUNT>N {: x:n y:n :}
   x y SUM-OVF? if A-OVER else x y + ITEM-COUNT then ;
: ADD-CELLS ( cell-count cell-count -- numeric-result<cell-count> )
   {: a:cell-count b:cell-count :}
   a CELL-COUNT>N b CELL-COUNT>N {: x:n y:n :}
   x y SUM-OVF? if A-OVER else x y + CELL-COUNT then ;

\ ---- offset + extent advance (overflow-checked) -------------------------------
: ADVANCE-BYTE-OFF ( byte-off byte-len -- numeric-result<byte-off> )
   {: o:byte-off d:byte-len :}
   o BYTE-OFF>N d BYTE-LEN>N {: x:n y:n :}
   x y SUM-OVF? if A-OVER else x y + BYTE-OFF then ;
: ADVANCE-CELL-OFF ( cell-off cell-count -- numeric-result<cell-off> )
   {: o:cell-off d:cell-count :}
   o CELL-OFF>N d CELL-COUNT>N {: x:n y:n :}
   x y SUM-OVF? if A-OVER else x y + CELL-OFF then ;
: ADVANCE-INDEX ( index item-count -- numeric-result<index> )
   {: o:index d:item-count :}
   o INDEX>N d ITEM-COUNT>N {: x:n y:n :}
   x y SUM-OVF? if A-OVER else x y + INDEX then ;

\ ---- extent - extent (underflow-checked) --------------------------------------
: SUB-BYTES ( byte-len byte-len -- numeric-result<byte-len> )
   {: a:byte-len b:byte-len :}
   a BYTE-LEN>N b BYTE-LEN>N {: x:n y:n :}
   x y DIFF-UNF? if A-UNDER else x y - BYTE-LEN then ;
: SUB-ITEMS ( item-count item-count -- numeric-result<item-count> )
   {: a:item-count b:item-count :}
   a ITEM-COUNT>N b ITEM-COUNT>N {: x:n y:n :}
   x y DIFF-UNF? if A-UNDER else x y - ITEM-COUNT then ;
: SUB-CELLS ( cell-count cell-count -- numeric-result<cell-count> )
   {: a:cell-count b:cell-count :}
   a CELL-COUNT>N b CELL-COUNT>N {: x:n y:n :}
   x y DIFF-UNF? if A-UNDER else x y - CELL-COUNT then ;

\ ---- offset - extent retreat (underflow-checked) ------------------------------
: RETREAT-BYTE-OFF ( byte-off byte-len -- numeric-result<byte-off> )
   {: o:byte-off d:byte-len :}
   o BYTE-OFF>N d BYTE-LEN>N {: x:n y:n :}
   x y DIFF-UNF? if A-UNDER else x y - BYTE-OFF then ;
: RETREAT-CELL-OFF ( cell-off cell-count -- numeric-result<cell-off> )
   {: o:cell-off d:cell-count :}
   o CELL-OFF>N d CELL-COUNT>N {: x:n y:n :}
   x y DIFF-UNF? if A-UNDER else x y - CELL-OFF then ;
: RETREAT-INDEX ( index item-count -- numeric-result<index> )
   {: o:index d:item-count :}
   o INDEX>N d ITEM-COUNT>N {: x:n y:n :}
   x y DIFF-UNF? if A-UNDER else x y - INDEX then ;

\ ---- offset - offset distance -> extent (reversed-operand underflow) -----------
: BYTE-DISTANCE ( byte-off byte-off -- numeric-result<byte-len> )
   {: a:byte-off b:byte-off :}
   a BYTE-OFF>N b BYTE-OFF>N {: x:n y:n :}
   x y DIFF-UNF? if A-UNDER else x y - BYTE-LEN then ;
: CELL-DISTANCE ( cell-off cell-off -- numeric-result<cell-count> )
   {: a:cell-off b:cell-off :}
   a CELL-OFF>N b CELL-OFF>N {: x:n y:n :}
   x y DIFF-UNF? if A-UNDER else x y - CELL-COUNT then ;
: INDEX-DISTANCE ( index index -- numeric-result<item-count> )
   {: a:index b:index :}
   a INDEX>N b INDEX>N {: x:n y:n :}
   x y DIFF-UNF? if A-UNDER else x y - ITEM-COUNT then ;

\ ---- count x size multiplication (overflow-checked) ---------------------------
: MUL-ITEMS ( item-count item-count -- numeric-result<item-count> )
   {: a:item-count b:item-count :}
   a ITEM-COUNT>N b ITEM-COUNT>N {: x:n y:n :}
   x y PROD-OVF? if A-OVER else x y * ITEM-COUNT then ;
: SCALE-BYTES ( byte-len item-count -- numeric-result<byte-len> )
   {: s:byte-len c:item-count :}
   s BYTE-LEN>N c ITEM-COUNT>N {: x:n y:n :}
   x y PROD-OVF? if A-OVER else x y * BYTE-LEN then ;
: SCALE-CELLS ( cell-count item-count -- numeric-result<cell-count> )
   {: s:cell-count c:item-count :}
   s CELL-COUNT>N c ITEM-COUNT>N {: x:n y:n :}
   x y PROD-OVF? if A-OVER else x y * CELL-COUNT then ;

\ ---- total positive-divisor division / remainder (no error variant) -----------
: DIV-BYTES ( byte-len positive-divisor -- byte-len )
   {: a:byte-len d:positive-divisor :}
   a BYTE-LEN>N d POSITIVE-DIVISOR>N / BYTE-LEN OK-BYTE-LEN ;
: REM-BYTES ( byte-len positive-divisor -- byte-len )
   {: a:byte-len d:positive-divisor :}
   a BYTE-LEN>N d POSITIVE-DIVISOR>N mod BYTE-LEN OK-BYTE-LEN ;
: DIV-ITEMS ( item-count positive-divisor -- item-count )
   {: a:item-count d:positive-divisor :}
   a ITEM-COUNT>N d POSITIVE-DIVISOR>N / ITEM-COUNT OK-ITEM-COUNT ;
: REM-ITEMS ( item-count positive-divisor -- item-count )
   {: a:item-count d:positive-divisor :}
   a ITEM-COUNT>N d POSITIVE-DIVISOR>N mod ITEM-COUNT OK-ITEM-COUNT ;
: DIV-CELLS ( cell-count positive-divisor -- cell-count )
   {: a:cell-count d:positive-divisor :}
   a CELL-COUNT>N d POSITIVE-DIVISOR>N / CELL-COUNT OK-CELL-COUNT ;
: REM-CELLS ( cell-count positive-divisor -- cell-count )
   {: a:cell-count d:positive-divisor :}
   a CELL-COUNT>N d POSITIVE-DIVISOR>N mod CELL-COUNT OK-CELL-COUNT ;

\ ---- extent / unit-extent -> unit count (zero-length unit refuses) -------------
\ "How many units of size S fit in extent E": dimensionally bytes / bytes = a
\ dimensionless item-count, so both operands are `byte-len` and the result is an
\ `item-count`. The unit size S is itself an extent and `byte-len` admits zero, so
\ a zero-size unit is representable and cannot be excluded by the type; it is
\ returned as the `zero` value (division-by-zero as a value, per the table style),
\ never a throw. The quotient is at most E, so it never overflows. These are
\ distinct from the unit-PRESERVING DIV-BYTES ( byte-len positive-divisor --
\ byte-len ), which keeps the byte unit and forbids a zero divisor statically.
: DIV-BYTES-CEIL ( byte-len byte-len -- numeric-result<item-count> )
   {: e:byte-len s:byte-len :}
   e BYTE-LEN>N s BYTE-LEN>N {: x:n u:n :}
   u 0= if A-ZERO else x u CEIL-QUOT ITEM-COUNT then ;
: DIV-BYTES-FLOOR ( byte-len byte-len -- numeric-result<item-count> )
   {: e:byte-len s:byte-len :}
   e BYTE-LEN>N s BYTE-LEN>N {: x:n u:n :}
   u 0= if A-ZERO else x u / ITEM-COUNT then ;

\ ---- cell <-> byte conversions ------------------------------------------------
: CELLS>BYTES ( cell-count -- numeric-result<byte-len> )
   CELL-COUNT>N {: v:n :}
   v CELL-BYTES-N PROD-OVF? if A-OVER else v CELL-BYTES-N * BYTE-LEN then ;
: CELL-OFF>BYTE-OFF ( cell-off -- numeric-result<byte-off> )
   CELL-OFF>N {: v:n :}
   v CELL-BYTES-N PROD-OVF? if A-OVER else v CELL-BYTES-N * BYTE-OFF then ;
: BYTES>CELLS ( byte-len -- numeric-result<cell-count> )
   BYTE-LEN>N {: v:n :}
   v CELL-BYTES-N WHOLE? if v CELL-BYTES-N / CELL-COUNT else A-MISALIGN then ;
: BYTE-OFF>CELL-OFF ( byte-off -- numeric-result<cell-off> )
   BYTE-OFF>N {: v:n :}
   v CELL-BYTES-N WHOLE? if v CELL-BYTES-N / CELL-OFF else A-MISALIGN then ;

\ ---- align-up (overflow-checked) ----------------------------------------------
: ALIGN-UP-BYTES ( byte-len alignment -- numeric-result<byte-len> )
   {: v:byte-len a:alignment :}
   v BYTE-LEN>N a ALIGNMENT>N ROUND-UP if drop A-OVER else BYTE-LEN then ;
: ALIGN-UP-BYTE-OFF ( byte-off alignment -- numeric-result<byte-off> )
   {: v:byte-off a:alignment :}
   v BYTE-OFF>N a ALIGNMENT>N ROUND-UP if drop A-OVER else BYTE-OFF then ;

\ ---- alignment predicates (no overflow/underflow class) -----------------------
: BYTES-ALIGNED? ( byte-len alignment -- bool )
   {: v:byte-len a:alignment :}
   v BYTE-LEN>N a ALIGNMENT>N ALIGNED? ;
: BYTE-OFF-ALIGNED? ( byte-off alignment -- bool )
   {: v:byte-off a:alignment :}
   v BYTE-OFF>N a ALIGNMENT>N ALIGNED? ;

;package
