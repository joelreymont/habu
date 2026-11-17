\ num-types.f - package NUM: one-cell nominal numeric roles and their checked
\ validators. A role value comes only from its public validator, which calls a
\ private CAST: mint on the success path. The arithmetic is
\ src/lib/num-arithmetic.f.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

\ Validation failure is a value: numeric-result<a> has a payloadless arm per
\ reason and never throws; these codes let a consumer throw at its own boundary.
-5400 constant E-NUM-NEGATIVE       \ a role rejected a negative raw cell
-5401 constant E-NUM-ZERO           \ a positive/allocation role rejected zero
-5402 constant E-NUM-OVERFLOW       \ a sum would exceed MAX-CELL-N
-5403 constant E-NUM-UNDERFLOW      \ a subtraction went below zero
-5404 constant E-NUM-BAD-ALIGNMENT  \ alignment raw cell is not a positive power of two
-5405 constant E-NUM-MISALIGNED     \ a byte extent/offset is not a whole cell count

package NUM
public

\ Raw storage cannot forge a role: a variable, create or constant cell is
\ TVK-RAW (src/core/checker.f), which refuses an arity-zero family both ways; a
\ role is stored only through a typed definer (TYPED-VARIABLE, a TYPED-BUFFER
\ element, a STRUCTURE field).
NEWTYPE byte-len               \ nonnegative extent measured in bytes
NEWTYPE item-count            \ nonnegative logical element count
NEWTYPE index                 \ nonnegative ordinal, not yet bounded
NEWTYPE byte-off              \ nonnegative byte offset
NEWTYPE alloc-byte-len        \ positive byte extent accepted by an allocator
NEWTYPE alloc-cell-count      \ positive cell count accepted by a cell allocator

\ A layout value: constructed and MATCHed only inside compiled words. A
\ polymorphic eliminator is not expressible (whole-bundle MATCH), so a consumer
\ MATCHes the concrete instantiation it holds (src/lib/num-types-test.f).
ENUM numeric-result<a>
   VARIANT ok FIELD role a ;VARIANT
   VARIANT negative ;VARIANT
   VARIANT zero ;VARIANT
   VARIANT overflow ;VARIANT
   VARIANT underflow ;VARIANT
   VARIANT bad-alignment ;VARIANT
   VARIANT misaligned ;VARIANT
;ENUM

private

\ No require: the type grammar is in the checker prefix, and NUM must not
\ depend on src/lib/memory.f (MEM:ALLOC-* consumes NUM roles, a cycle).
\ MAX-CELL-N mirrors its MEM-MAX-N.
$7FFFFFFFFFFFFFFF constant MAX-CELL-N              \ largest nonnegative machine cell

\ The checker cannot infer that a predicate-checked n has become the family, so
\ the crossing is declared.
CAST: MINT-BYTE-LEN ( n -- byte-len )
CAST: MINT-ITEM-COUNT ( n -- item-count )
CAST: MINT-INDEX ( n -- index )
CAST: MINT-BYTE-OFF ( n -- byte-off )
CAST: MINT-ALLOC-BYTE-LEN ( n -- alloc-byte-len )

\ Private proof-erasure projection, for the allocator zero check and the
\ arithmetic kernels. The one public projection is ORDINAL; a consumer of another
\ role declares its own erasure cast at its scope with its reason.
CAST: BYTE-LEN>N ( byte-len -- n )

: NR-OK       ( a -- numeric-result<a> ) NUM-NUMERIC--RESULT:OK ;
: NR-NEG      ( -- numeric-result<a> ) NUM-NUMERIC--RESULT:NEGATIVE ;
: NR-ZERO     ( -- numeric-result<a> ) NUM-NUMERIC--RESULT:ZERO ;

public

\ Zero is valid for ordinary extents, counts and offsets: empty strings, vectors
\ and zero-distance ranges are ordinary values.
: BYTE-LEN ( n -- numeric-result<byte-len> )
   dup 0 < if drop NR-NEG else MINT-BYTE-LEN NR-OK then ;
: ITEM-COUNT ( n -- numeric-result<item-count> )
   dup 0 < if drop NR-NEG else MINT-ITEM-COUNT NR-OK then ;
: INDEX ( n -- numeric-result<index> )
   dup 0 < if drop NR-NEG else MINT-INDEX NR-OK then ;
: BYTE-OFF ( n -- numeric-result<byte-off> )
   dup 0 < if drop NR-NEG else MINT-BYTE-OFF NR-OK then ;

\ An allocation caller narrows a zero-admitting role here, which answers `zero`:
\ no allocator accepts a zero-admitting role.
: AS-ALLOC-BYTE-LEN ( byte-len -- numeric-result<alloc-byte-len> )
   BYTE-LEN>N dup 0= if drop NR-ZERO else MINT-ALLOC-BYTE-LEN NR-OK then ;

;package
