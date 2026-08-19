\ cad-num-types.f - CAD-NUM scalar nominal numeric roles + validators
\ (MODEL-CAD-V2-PLAN.md B5.1; dot habu-implement-cad-num-962bf5d9, epic
\ habu-epic-model-cad-70b629a9).
\
\ All of these values occupy one cell, but they do not have interchangeable
\ meaning: a byte length is not a cell count, an index is not an offset, and an
\ alignment is not a divisor merely because all can currently pass through `n`.
\ Package CAD-NUM gives each fact a distinct arity-zero nominal family so the
\ checker rejects a cross-role swap or a raw `n` where a role is required.
\
\ Authority model: the ONLY way to obtain a role value is through its public
\ CHECKED validator. Each validator inspects the raw cell first and calls a
\ PRIVATE `CAST:` mint (`MINT-*`, a no-op representation cast) only on the
\ success path. There is no public raw mint and no public role->n projection.
\ The two private `*>N` projections are explicit proof erasure used only where an
\ allocator bound must read a role's raw cell; they are never paired with a
\ public inverse.
\ The mints are declarations the CHECKER certifies, not trusted assertions: a
\ `CAST:` body is certified under the identity row, and the declaration itself is
\ refused if it changes cell count, crosses the pointer/quotation class, widens,
\ carries linear ownership, or mints a family this package does not declare
\ (src/core/checker.f CAST-CERTIFY). So a mint can rename a cell; it cannot
\ reshape one.
\
\ Failure is a VALUE, not a throw. Expected validation failures return the
\ on-stack sum `numeric-result<a>` (success `ok`, carrying the validated role in
\ its `role` field, or a payloadless `negative` / `zero` / `overflow` /
\ `underflow` / `bad-alignment` /
\ `misaligned`); they never collapse different failures into one flag and never
\ throw. The `E-CADNUM-*` constants below name each refusal reason so a consumer
\ can throw the named code at its own boundary. `numeric-result<a>` is a layout
\ value, so it is constructed and MATCHed only inside compiled words. A
\ polymorphic eliminator (`numeric-result<a> -- n`) is not yet expressible
\ (whole-bundle MATCH, dot habu-typestate-result-drop-5ae048a7), so a consumer
\ MATCHes the concrete instantiation it holds; lib/cad-num-types-test.f does
\ exactly that per role.
\
\ Zero semantics: zero is VALID for the ordinary extents/counts/offsets
\ (byte-len, item-count, cell-count, index, byte-off, cell-off) - empty tensors,
\ strings, vectors, and zero-distance ranges are ordinary values. Zero is
\ INVALID for the positive roles (alignment, positive-divisor) and for the
\ allocation sinks: an allocation caller first builds a zero-admitting
\ extent/count, then passes it through the matching AS-ALLOC-* validator, which
\ returns `zero` (never throws, no allocator accepts a zero-admitting role).
\ AS-ALLOC-CELL-COUNT additionally returns `overflow` for a count above
\ MAX-CELL-N / CELL-BYTES, before any allocation primitive is reachable.
\
\ UNSEALED (B5.1, B5.5 slice 1): these canonical roles and validators already
\ reach production through lib/cad-num-arithmetic.f, including lib/memory.f and
\ inference consumers. CAD-NUM remains reopenable while its constituent files
\ are assembled, so its private mints are not yet unforgeable. lib/cad-num.f
\ (dot habu-seal-cad-num-36dbeec6) will close that namespace authority; the
\ closed B5.2 arithmetic is lib/cad-num-arithmetic.f (dot
\ habu-implement-cad-num-cb413b2a).
\ Independently of how the mints are declared, a raw cell read back out of
\ storage can still stand in for the `n` a validator accepts until the TVK-RAW
\ checker capability (dot habu-nominal-storage-raw-a3430ef2) lands; the mints
\ themselves no longer assert anything the checker has not certified.
\
\ No `require`: the type-declaration grammar (package/NEWTYPE/ENUM/
\ CAST:/MATCH) is in the checker prefix (cf. maki/cad-kinds.f). CAD-NUM must
\ not depend on lib/memory.f - MEM:ALLOC-* consumes CAD-NUM:alloc-* roles, so a
\ dependency would be a cycle; MAX-CELL-N mirrors the machine max cell that
\ lib/memory.f also names as MEM-MAX-N.

\ ---- named refusal codes (numeric-result error variants; consumers throw) -----
-5400 constant E-CADNUM-NEGATIVE       \ a role rejected a negative raw cell
-5401 constant E-CADNUM-ZERO           \ a positive/allocation role rejected zero
-5402 constant E-CADNUM-OVERFLOW       \ an allocation count would exceed MAX-CELL-N
-5403 constant E-CADNUM-UNDERFLOW      \ a subtraction/retreat went below zero (B5.2)
-5404 constant E-CADNUM-BAD-ALIGNMENT  \ alignment raw cell is not a positive power of two
-5405 constant E-CADNUM-MISALIGNED     \ a byte extent/offset is not a whole cell count (B5.2)

package CAD-NUM
public

\ ---- scalar nominal role families (one cell each, no widening to/from n) ------
NEWTYPE byte-len 0               \ nonnegative extent measured in bytes
NEWTYPE item-count 0            \ nonnegative logical element count
NEWTYPE cell-count 0            \ nonnegative machine-cell count
NEWTYPE index 0                 \ nonnegative ordinal, not yet bounded
NEWTYPE byte-off 0              \ nonnegative byte offset
NEWTYPE cell-off 0              \ nonnegative cell offset
NEWTYPE alignment 0             \ positive power-of-two alignment
NEWTYPE positive-divisor 0      \ positive divisor for unit-preserving extent arithmetic
NEWTYPE alloc-byte-len 0        \ positive byte extent accepted by an allocator
NEWTYPE alloc-cell-count 0      \ positive cell count accepted by a cell allocator

\ ---- the on-stack validation result (success carries one cell-kinded a) -------
\ Declared through the unified ENUM front end in full mode: the arity token after
\ the family name is what selects that mode, so the success payload is a named
\ FIELD instead of a positional type token. The field is called `role` because
\ that is this file's own word for the thing the ok arm carries: the families just
\ above are "scalar nominal role families", the mints below turn a validated raw
\ cell into a "nominal role", and the checker's job here is to refuse a
\ "cross-role" swap. So `role` names the payload after what it is rather than
\ after the generic slot it sits in.
\
\ Nothing else moves. The generated CAD--NUM-NUMERIC--RESULT:OK, :NEGATIVE,
\ :ZERO, :OVERFLOW, :UNDERFLOW, :BAD-ALIGNMENT and :MISALIGNED constructors keep
\ their exact spellings and their exact checked effects, and every MATCH site in
\ this package and in its consumers is untouched, because the spellings come from
\ the package name and the family tail and the payload binding order comes from
\ the declaration order - none of which the declaration mode changes.
ENUM numeric-result 1
   VARIANT ok FIELD role a ;VARIANT
   VARIANT negative ;VARIANT
   VARIANT zero ;VARIANT
   VARIANT overflow ;VARIANT
   VARIANT underflow ;VARIANT
   VARIANT bad-alignment ;VARIANT
   VARIANT misaligned ;VARIANT
;ENUM

private

\ ---- allocation bound (mirrors lib/memory.f MEM-MAX-N / MEM-CELL-BYTES) -------
$7FFFFFFFFFFFFFFF constant MAX-CELL-N              \ largest nonnegative machine cell
MAX-CELL-N 1 cells / constant MAX-ALLOC-CELLS      \ a count above this overflows MAX-CELL-N bytes

\ ---- checked representation mints: validated raw cell -> nominal role ---------
\ No-op identity casts; the checker cannot infer that a predicate-checked n has
\ become the arity-zero family, so the crossing is declared. Private to the
\ owning package - CAST-OWNER? refuses these names anywhere else - and directly
\ tested.
CAST: MINT-BYTE-LEN ( n -- byte-len ) ;
CAST: MINT-ITEM-COUNT ( n -- item-count ) ;
CAST: MINT-CELL-COUNT ( n -- cell-count ) ;
CAST: MINT-INDEX ( n -- index ) ;
CAST: MINT-BYTE-OFF ( n -- byte-off ) ;
CAST: MINT-CELL-OFF ( n -- cell-off ) ;
CAST: MINT-ALIGNMENT ( n -- alignment ) ;
CAST: MINT-POSITIVE-DIVISOR ( n -- positive-divisor ) ;
CAST: MINT-ALLOC-BYTE-LEN ( n -- alloc-byte-len ) ;
CAST: MINT-ALLOC-CELL-COUNT ( n -- alloc-cell-count ) ;

\ ---- private proof-erasure projections (allocator bound checks only) ----------
\ Explicit representation reads with no public inverse; a role has no primitive
\ that consumes it directly for the zero/overflow bound test.
CAST: BYTE-LEN>N ( byte-len -- n ) ;
CAST: CELL-COUNT>N ( cell-count -- n ) ;

\ ---- result constructors (readable names over the escaped ctor spelling) ------
: NR-OK       ( a -- numeric-result<a> ) CAD--NUM-NUMERIC--RESULT:OK ;
: NR-NEG      ( -- numeric-result<a> ) CAD--NUM-NUMERIC--RESULT:NEGATIVE ;
: NR-ZERO     ( -- numeric-result<a> ) CAD--NUM-NUMERIC--RESULT:ZERO ;
: NR-OVER     ( -- numeric-result<a> ) CAD--NUM-NUMERIC--RESULT:OVERFLOW ;
: NR-BADALIGN ( -- numeric-result<a> ) CAD--NUM-NUMERIC--RESULT:BAD-ALIGNMENT ;

public

\ ---- ordinary extents/counts/offsets: reject negative, admit zero + positive --
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

\ ---- positive roles: reject zero (and negative) -------------------------------
: ALIGNMENT ( n -- numeric-result<alignment> ) {: v:n :}
   v 0 >  v v 1- and 0=  and if v MINT-ALIGNMENT NR-OK else NR-BADALIGN then ;
: POSITIVE-DIVISOR ( n -- numeric-result<positive-divisor> )
   dup 0 < if drop NR-NEG exit then
   dup 0= if drop NR-ZERO exit then
   MINT-POSITIVE-DIVISOR NR-OK ;

\ ---- allocation sinks: a zero-admitting role is explicitly narrowed ------------
: AS-ALLOC-BYTE-LEN ( byte-len -- numeric-result<alloc-byte-len> )
   BYTE-LEN>N dup 0= if drop NR-ZERO else MINT-ALLOC-BYTE-LEN NR-OK then ;
: AS-ALLOC-CELL-COUNT ( cell-count -- numeric-result<alloc-cell-count> )
   CELL-COUNT>N
   dup 0= if drop NR-ZERO exit then
   dup MAX-ALLOC-CELLS > if drop NR-OVER exit then
   MINT-ALLOC-CELL-COUNT NR-OK ;

;package
