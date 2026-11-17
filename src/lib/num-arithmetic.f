\ num-arithmetic.f - closed, dimensionally valid arithmetic: reopens package NUM
\ after src/lib/num-types.f and adds no roles or overloads. These words are the
\ whole admitted role algebra; an omitted, reversed or cross-unit pair is
\ statically unavailable. A fallible role-changing operation answers
\ numeric-result<role>, never a throw. Every success value goes through
\ num-types.f's public validator, which owns the audited MINT-*.

require src/lib/num-types.f

package NUM
private

\ Proof-erasure projections for the raw kernels: private checked casts with no
\ public inverse.
CAST: ITEM-COUNT>N ( item-count -- n )
CAST: INDEX>N ( index -- n )
CAST: BYTE-OFF>N ( byte-off -- n )

\ Failure arms: overflow past the machine max, underflow below zero.
: A-OVER     ( -- numeric-result<a> ) NUM-NUMERIC--RESULT:OVERFLOW ;
: A-UNDER    ( -- numeric-result<a> ) NUM-NUMERIC--RESULT:UNDERFLOW ;

\ Each returns the failure predicate as a real bool from a comparison; the caller
\ recomputes the value only on the success path (through a validator).
: SUM-OVF? ( n n -- bool )
   {: a:n b:n :} a MAX-CELL-N b - > ;
: DIFF-UNF? ( n n -- bool )
   {: a:n b:n :} a b < ;

public

\ ORDINAL is the package's only public projection. Nothing consumes an index
\ directly: a found position drives pointer and length arithmetic, a bound test
\ against a raw extent, or a mint into a foreign index role such as the
\ engine's `idx`. A projection per consumer would put foreign words in NUM's
\ public surface and leave it unsealable. It erases a proof and cannot create
\ one, because the mints have no public counterpart.
: ORDINAL ( index -- n )
   INDEX>N ;

: BYTE-OFF-IN-LEN? ( byte-off byte-len -- bool )
   {: off:byte-off len:byte-len :}
   off BYTE-OFF>N len BYTE-LEN>N < ;

: ADVANCE-BYTE-OFF ( byte-off byte-len -- numeric-result<byte-off> )
   {: o:byte-off d:byte-len :}
   o BYTE-OFF>N d BYTE-LEN>N {: x:n y:n :}
   x y SUM-OVF? if A-OVER else x y + BYTE-OFF then ;

: SUB-BYTES ( byte-len byte-len -- numeric-result<byte-len> )
   {: a:byte-len b:byte-len :}
   a BYTE-LEN>N b BYTE-LEN>N {: x:n y:n :}
   x y DIFF-UNF? if A-UNDER else x y - BYTE-LEN then ;
: SUB-ITEMS ( item-count item-count -- numeric-result<item-count> )
   {: a:item-count b:item-count :}
   a ITEM-COUNT>N b ITEM-COUNT>N {: x:n y:n :}
   x y DIFF-UNF? if A-UNDER else x y - ITEM-COUNT then ;

\ The one public NUM word that touches a pointer: the byte primitives take a
\ bare n, so without it every caller would erase a validated offset through a
\ projection of its own. It does not prove bounds: the offset role carries no
\ extent, so the caller settles that first (ADVANCE-BYTE-OFF, then
\ BYTE-OFF-IN-LEN? against the owning extent).
: BYTE+ ( ^u8 byte-off -- ^u8 )
   BYTE-OFF>N + ;

;package
