\ num-types.f - package NUM: one-cell nominal numeric roles and their checked
\ validators. A role value comes only from its public validator, which calls a
\ private CAST: mint on the success path. The native build seals NUM
\ (src/core/internal-mark.f SEAL-PACKAGES): a later `package NUM` exits 84 and
\ the mints are unforgeable. The arithmetic is src/lib/num-arithmetic.f.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

\ Validation failure is a value: numeric-result<a> has a payloadless arm per
\ reason and never throws; these codes let a consumer throw at its own boundary.
-5400 constant E-NUM-NEGATIVE       \ a role rejected a negative raw cell
-5401 constant E-NUM-ZERO           \ a positive/allocation role rejected zero
-5402 constant E-NUM-OVERFLOW       \ an allocation count would exceed MAX-CELL-N
-5403 constant E-NUM-UNDERFLOW      \ a subtraction/retreat went below zero
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
NEWTYPE cell-count            \ nonnegative machine-cell count
NEWTYPE index                 \ nonnegative ordinal, not yet bounded
NEWTYPE byte-off              \ nonnegative byte offset
NEWTYPE cell-off              \ nonnegative cell offset
NEWTYPE alignment             \ positive power-of-two alignment
NEWTYPE positive-divisor      \ positive divisor for unit-preserving extent arithmetic
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
MAX-CELL-N 1 cells / constant MAX-ALLOC-CELLS      \ a count above this overflows MAX-CELL-N bytes

\ The checker cannot infer that a predicate-checked n has become the family, so
\ the crossing is declared. CAST-OWNER? refuses these names outside NUM, and
\ src/core/checker.f CAST-CERTIFY refuses a declaration that changes cell count,
\ crosses the pointer/quotation class, widens, carries linear ownership or mints
\ a family NUM does not declare: a mint renames a cell and cannot reshape one.
CAST: MINT-BYTE-LEN ( n -- byte-len )
CAST: MINT-ITEM-COUNT ( n -- item-count )
CAST: MINT-CELL-COUNT ( n -- cell-count )
CAST: MINT-INDEX ( n -- index )
CAST: MINT-BYTE-OFF ( n -- byte-off )
CAST: MINT-CELL-OFF ( n -- cell-off )
CAST: MINT-ALIGNMENT ( n -- alignment )
CAST: MINT-POSITIVE-DIVISOR ( n -- positive-divisor )
CAST: MINT-ALLOC-BYTE-LEN ( n -- alloc-byte-len )
CAST: MINT-ALLOC-CELL-COUNT ( n -- alloc-cell-count )

\ Private proof-erasure projections, for the allocator bound checks. The one
\ public projection is ORDINAL (src/lib/num-arithmetic.f); a consumer of another
\ role declares its own erasure cast at its scope with its reason
\ (src/lib/memory.f, src/lib/string-roles.f, src/lib/elf32.f).
CAST: BYTE-LEN>N ( byte-len -- n )
CAST: CELL-COUNT>N ( cell-count -- n )

: NR-OK       ( a -- numeric-result<a> ) NUM-NUMERIC--RESULT:OK ;
: NR-NEG      ( -- numeric-result<a> ) NUM-NUMERIC--RESULT:NEGATIVE ;
: NR-ZERO     ( -- numeric-result<a> ) NUM-NUMERIC--RESULT:ZERO ;
: NR-OVER     ( -- numeric-result<a> ) NUM-NUMERIC--RESULT:OVERFLOW ;
: NR-BADALIGN ( -- numeric-result<a> ) NUM-NUMERIC--RESULT:BAD-ALIGNMENT ;

public

\ Zero is valid for ordinary extents, counts and offsets: empty strings, vectors
\ and zero-distance ranges are ordinary values.
: BYTE-LEN ( n -- numeric-result<byte-len> )
   dup 0 < if drop NR-NEG else MINT-BYTE-LEN NR-OK then ;
: ITEM-COUNT ( n -- numeric-result<item-count> )
   dup 0 < if drop NR-NEG else MINT-ITEM-COUNT NR-OK then ;
: CELL-COUNT ( n -- numeric-result<cell-count> )
   dup 0 < if drop NR-NEG else MINT-CELL-COUNT NR-OK then ;
: INDEX ( n -- numeric-result<index> )
   dup 0 < if drop NR-NEG else MINT-INDEX NR-OK then ;
: BYTE-OFF ( n -- numeric-result<byte-off> )
   dup 0 < if drop NR-NEG else MINT-BYTE-OFF NR-OK then ;
: CELL-OFF ( n -- numeric-result<cell-off> )
   dup 0 < if drop NR-NEG else MINT-CELL-OFF NR-OK then ;

: ALIGNMENT ( n -- numeric-result<alignment> ) {: v:n :}
   v 0 >  v v 1- and 0=  and if v MINT-ALIGNMENT NR-OK else NR-BADALIGN then ;
: POSITIVE-DIVISOR ( n -- numeric-result<positive-divisor> )
   dup 0 < if drop NR-NEG exit then
   dup 0= if drop NR-ZERO exit then
   MINT-POSITIVE-DIVISOR NR-OK ;

\ An allocation caller narrows a zero-admitting role here, which answers `zero`:
\ no allocator accepts a zero-admitting role. AS-ALLOC-CELL-COUNT answers
\ `overflow` above MAX-ALLOC-CELLS, before any allocation primitive is reachable.
: AS-ALLOC-BYTE-LEN ( byte-len -- numeric-result<alloc-byte-len> )
   BYTE-LEN>N dup 0= if drop NR-ZERO else MINT-ALLOC-BYTE-LEN NR-OK then ;
: AS-ALLOC-CELL-COUNT ( cell-count -- numeric-result<alloc-cell-count> )
   CELL-COUNT>N
   dup 0= if drop NR-ZERO exit then
   dup MAX-ALLOC-CELLS > if drop NR-OVER exit then
   MINT-ALLOC-CELL-COUNT NR-OK ;

;package
