\ num-arithmetic.f - closed, dimensionally valid arithmetic: reopens package NUM
\ after src/lib/num-types.f and adds no roles or overloads. These words are the
\ whole admitted role algebra; an omitted, reversed or cross-unit pair is
\ statically unavailable. A fallible role-changing operation answers
\ numeric-result<role>, never a throw. Every success value goes through
\ num-types.f's public validator, which owns the audited MINT-*.

require src/lib/num-types.f

package NUM
private

-5406 constant E-NUM-TOTALITY   \ a total op's validator saw a non-ok result

\ No require src/lib/memory.f: MEM:ALLOC-* consumes NUM alloc roles, so it would
\ be a cycle. CELL-BYTES-N mirrors its MEM-CELL-BYTES.
1 cells constant CELL-BYTES-N

\ Proof-erasure projections for the raw kernels: private checked casts with no
\ public inverse, like num-types.f's BYTE-LEN>N and CELL-COUNT>N.
CAST: ITEM-COUNT>N ( item-count -- n )
CAST: INDEX>N ( index -- n )
CAST: BYTE-OFF>N ( byte-off -- n )
CAST: CELL-OFF>N ( cell-off -- n )
CAST: ALIGNMENT>N ( alignment -- n )
CAST: POSITIVE-DIVISOR>N ( positive-divisor -- n )

\ Failure arms: overflow past the machine max, underflow below zero, misaligned
\ (a byte extent or offset not a whole cell count), zero (a zero-length unit
\ extent as divisor).
: A-OVER     ( -- numeric-result<a> ) NUM-NUMERIC--RESULT:OVERFLOW ;
: A-UNDER    ( -- numeric-result<a> ) NUM-NUMERIC--RESULT:UNDERFLOW ;
: A-MISALIGN ( -- numeric-result<a> ) NUM-NUMERIC--RESULT:MISALIGNED ;
: A-ZERO     ( -- numeric-result<a> ) NUM-NUMERIC--RESULT:ZERO ;

\ Each returns the failure predicate as a real bool from a comparison; the caller
\ recomputes the value only on the success path (through a validator).
: SUM-OVF? ( n n -- bool )
   {: a:n b:n :} a MAX-CELL-N b - > ;
: DIFF-UNF? ( n n -- bool )
   {: a:n b:n :} a b < ;
: PROD-OVF? ( n n -- bool )
   {: a:n b:n :} b 0= if 0 0 > exit then a MAX-CELL-N b / > ;
: WHOLE? ( n n -- bool )
   {: v:n u:n :} v u mod 0= ;
: ALIGNED? ( n n -- bool )
   {: v:n a:n :} v a 1- and 0= ;
: ROUND-UP ( n n -- n bool )            \ round v up to power-of-two a; value + overflow
   {: v:n a:n :}
   v a 1- and 0= if v  0 0 >  exit then
   a  v a 1- and  -  {: delta:n :}
   v delta +  v MAX-CELL-N delta - >  ;
: CEIL-QUOT ( n n -- n )                \ ceil(v / u) for u>0, v>=0 (0 when v=0)
   {: v:n u:n :} v 0= if 0 exit then v 1- u / 1+ ;

\ The total ops' non-ok arms are unreachable.
: OK-BYTE-LEN ( numeric-result<byte-len> -- byte-len )
   MATCH numeric-result
      ok OF ENDOF                            negative OF E-NUM-TOTALITY throw ENDOF
      zero OF E-NUM-TOTALITY throw ENDOF  overflow OF E-NUM-TOTALITY throw ENDOF
      underflow OF E-NUM-TOTALITY throw ENDOF
      bad-alignment OF E-NUM-TOTALITY throw ENDOF
      misaligned OF E-NUM-TOTALITY throw ENDOF
   ;MATCH ;
: OK-ITEM-COUNT ( numeric-result<item-count> -- item-count )
   MATCH numeric-result
      ok OF ENDOF                            negative OF E-NUM-TOTALITY throw ENDOF
      zero OF E-NUM-TOTALITY throw ENDOF  overflow OF E-NUM-TOTALITY throw ENDOF
      underflow OF E-NUM-TOTALITY throw ENDOF
      bad-alignment OF E-NUM-TOTALITY throw ENDOF
      misaligned OF E-NUM-TOTALITY throw ENDOF
   ;MATCH ;
: OK-CELL-COUNT ( numeric-result<cell-count> -- cell-count )
   MATCH numeric-result
      ok OF ENDOF                            negative OF E-NUM-TOTALITY throw ENDOF
      zero OF E-NUM-TOTALITY throw ENDOF  overflow OF E-NUM-TOTALITY throw ENDOF
      underflow OF E-NUM-TOTALITY throw ENDOF
      bad-alignment OF E-NUM-TOTALITY throw ENDOF
      misaligned OF E-NUM-TOTALITY throw ENDOF
   ;MATCH ;

public

\ ORDINAL is the package's only public projection. Nothing consumes an index
\ directly: a found position drives pointer and length arithmetic, a bound test
\ against a raw extent, or a mint into a foreign index role such as the
\ engine's `idx`. A projection per consumer would put foreign words in NUM's
\ public surface and leave it unsealable. It erases a proof and cannot create
\ one, because the mints have no public counterpart.
: ORDINAL ( index -- n )
   INDEX>N ;

: INDEX= ( index index -- bool )
   {: a:index b:index :}
   a INDEX>N b INDEX>N = ;

: INDEX-IN-COUNT? ( index item-count -- bool )
   {: idx:index count:item-count :}
   idx INDEX>N count ITEM-COUNT>N < ;
: INDEX-BYTE-OFF ( index byte-len -- numeric-result<byte-off> )
   {: idx:index width:byte-len :}
   idx INDEX>N width BYTE-LEN>N {: i:n w:n :}
   i w PROD-OVF? if A-OVER else i w * BYTE-OFF then ;
: BYTE-OFF-IN-LEN? ( byte-off byte-len -- bool )
   {: off:byte-off len:byte-len :}
   off BYTE-OFF>N len BYTE-LEN>N < ;

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

\ DIV and REM take a positive-divisor, so a zero divisor is statically
\ impossible and they answer their role with no error arm.
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

\ Units of size S in extent E: bytes / bytes is a dimensionless item-count.
\ byte-len admits zero, so a zero-size unit answers the `zero` value, never a
\ throw; the quotient is at most E, so it never overflows. DIV-BYTES keeps the
\ byte unit and forbids a zero divisor statically.
: DIV-BYTES-CEIL ( byte-len byte-len -- numeric-result<item-count> )
   {: e:byte-len s:byte-len :}
   e BYTE-LEN>N s BYTE-LEN>N {: x:n u:n :}
   u 0= if A-ZERO else x u CEIL-QUOT ITEM-COUNT then ;
: DIV-BYTES-FLOOR ( byte-len byte-len -- numeric-result<item-count> )
   {: e:byte-len s:byte-len :}
   e BYTE-LEN>N s BYTE-LEN>N {: x:n u:n :}
   u 0= if A-ZERO else x u / ITEM-COUNT then ;

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

: ALIGN-UP-BYTES ( byte-len alignment -- numeric-result<byte-len> )
   {: v:byte-len a:alignment :}
   v BYTE-LEN>N a ALIGNMENT>N ROUND-UP if drop A-OVER else BYTE-LEN then ;
: ALIGN-UP-BYTE-OFF ( byte-off alignment -- numeric-result<byte-off> )
   {: v:byte-off a:alignment :}
   v BYTE-OFF>N a ALIGNMENT>N ROUND-UP if drop A-OVER else BYTE-OFF then ;

\ The one public NUM word that touches a pointer: the byte primitives take a
\ bare n, so without it every caller would erase a validated offset through a
\ projection of its own. It does not prove bounds: the offset role carries no
\ extent, so the caller settles that first (ADVANCE-BYTE-OFF, then
\ BYTE-OFF-IN-LEN? against the owning extent).
: BYTE+ ( ptr u8 byte-off -- ptr u8 )
   BYTE-OFF>N + ;

: BYTES-ALIGNED? ( byte-len alignment -- bool )
   {: v:byte-len a:alignment :}
   v BYTE-LEN>N a ALIGNMENT>N ALIGNED? ;
: BYTE-OFF-ALIGNED? ( byte-off alignment -- bool )
   {: v:byte-off a:alignment :}
   v BYTE-OFF>N a ALIGNMENT>N ALIGNED? ;

;package
