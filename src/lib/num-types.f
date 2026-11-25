\ num-types.f - package NUM: one-cell nominal numeric roles and their checked
\ validators. A role value comes from its validator, or from a cast declared by a
\ package that has shown the cell is the role. The arithmetic is
\ src/lib/num-arithmetic.f.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require src/lib/adt/option.f

package NUM
public

\ Raw storage cannot forge a role: a variable, create or constant cell is
\ TVK-RAW (src/core/checker.f), which rejects an arity-zero family both ways; a
\ role is stored only through a typed definer (TYPED-VARIABLE, a TYPED-BUFFER
\ element, a STRUCTURE field).
NEWTYPE byte-len               \ nonnegative extent measured in bytes
NEWTYPE item-count            \ nonnegative logical element count
NEWTYPE index                 \ nonnegative ordinal, not yet bounded
NEWTYPE byte-off              \ nonnegative byte offset
NEWTYPE alloc-byte-len        \ positive byte extent accepted by an allocator
NEWTYPE alloc-cell-count      \ positive cell count accepted by a cell allocator

private

\ NUM cannot require src/lib/memory.f, whose allocators take NUM roles, so the
\ largest cell is stated here.
$7FFFFFFFFFFFFFFF constant MAX-CELL-N              \ largest nonnegative machine cell

\ The checker cannot infer that a predicate-checked n has become the family, so
\ the crossing is declared.
CAST: MINT-BYTE-LEN ( n -- byte-len )
CAST: MINT-ITEM-COUNT ( n -- item-count )
CAST: MINT-INDEX ( n -- index )
CAST: MINT-BYTE-OFF ( n -- byte-off )
CAST: MINT-ALLOC-BYTE-LEN ( n -- alloc-byte-len )

\ Proof erasure for the allocator zero check and the arithmetic kernels; a
\ consumer of another role declares its own erasure cast.
CAST: BYTE-LEN>N ( byte-len -- n )

public

\ Zero is valid for ordinary extents, counts and offsets: empty strings, vectors
\ and zero-distance ranges are ordinary values. `none` means a negative cell.
: BYTE-LEN ( n -- option<byte-len> )
   dup 0 < if drop OPTION:NONE else MINT-BYTE-LEN OPTION:SOME then ;
: ITEM-COUNT ( n -- option<item-count> )
   dup 0 < if drop OPTION:NONE else MINT-ITEM-COUNT OPTION:SOME then ;
: INDEX ( n -- option<index> )
   dup 0 < if drop OPTION:NONE else MINT-INDEX OPTION:SOME then ;
: BYTE-OFF ( n -- option<byte-off> )
   dup 0 < if drop OPTION:NONE else MINT-BYTE-OFF OPTION:SOME then ;

\ An allocation caller narrows a zero-admitting role here, which answers `none`
\ for zero: no allocator accepts a zero-admitting role.
: AS-ALLOC-BYTE-LEN ( byte-len -- option<alloc-byte-len> )
   BYTE-LEN>N dup 0= if drop OPTION:NONE else MINT-ALLOC-BYTE-LEN OPTION:SOME then ;

;package
