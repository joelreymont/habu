split-str
\ num-arithmetic.f - closed, dimensionally valid arithmetic: reopens package NUM
\ after src/lib/num-types.f and adds no roles or overloads. These words are the
\ whole admitted role algebra; an omitted, reversed or cross-unit pair is
\ statically unavailable. A fallible role-changing operation answers
\ option<role>, never a throw: `none` is its one failure. A success value has
\ passed its failure predicate, so it is minted through num-types.f's MINT-*.

require src/lib/adt/option.f
require src/lib/num-types.f

package NUM
private

\ Proof-erasure projections for the raw kernels: private checked casts with no
\ public inverse.
CAST: ITEM-COUNT>N ( item-count -- n )
CAST: INDEX>N ( index -- n )
CAST: BYTE-OFF>N ( byte-off -- n )

\ Each returns the failure predicate as a real bool from a comparison; the caller
\ recomputes the value only on the success path.
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

: ADVANCE-BYTE-OFF ( byte-off byte-len -- option<byte-off> )
   {: o:byte-off d:byte-len :}
   o BYTE-OFF>N d BYTE-LEN>N {: x:n y:n :}
   x y SUM-OVF? if OPTION:NONE else x y + MINT-BYTE-OFF OPTION:SOME then ;

: SUB-BYTES ( byte-len byte-len -- option<byte-len> )
   {: a:byte-len b:byte-len :}
   a BYTE-LEN>N b BYTE-LEN>N {: x:n y:n :}
   x y DIFF-UNF? if OPTION:NONE else x y - MINT-BYTE-LEN OPTION:SOME then ;
: SUB-ITEMS ( item-count item-count -- option<item-count> )
   {: a:item-count b:item-count :}
   a ITEM-COUNT>N b ITEM-COUNT>N {: x:n y:n :}
   x y DIFF-UNF? if OPTION:NONE else x y - MINT-ITEM-COUNT OPTION:SOME then ;

\ The one public NUM word that touches a pointer: the byte primitives take a
\ bare n, so without it every caller would erase a validated offset through a
\ projection of its own. It does not prove bounds: the offset role carries no
\ extent, so the caller settles that first (ADVANCE-BYTE-OFF, then
\ BYTE-OFF-IN-LEN? against the owning extent).
: BYTE+ ( ^u8 byte-off -- ^u8 )
   BYTE-OFF>N + ;

;package
