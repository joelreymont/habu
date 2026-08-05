\ memory.f - checked OS-backed byte buffers.
\
\ Load after lib/errors.f.

s" lib/errors.f" required
require lib/cad-num-arithmetic.f
require lib/adt/result.f

$10000 constant MEM-64K
$7FFFFFFFFFFFFFFF constant MEM-MAX-N
MEM-MAX-N MEM-64K / constant MEM-MAX-64K-BUFFERS
1 cells constant MEM-CELL-BYTES
MEM-MAX-N MEM-CELL-BYTES / constant MEM-MAX-CELLS

0 constant MEM-ADDR-ANY
3 constant MEM-PROT-RW
1 constant MEM-MAP-SHARED
$1002 constant MEM-MAP-PRIVATE-ANON
-1 constant MEM-ANON-FD
0 constant MEM-OFF-ZERO

: MEM-CHECK-SIZE ( n -- )
   dup 0 <= if E-MEM-SIZE throw then
   drop ;

: MEM-CHECK-64K-COUNT ( n -- )
   dup 0 <= if E-MEM-SIZE throw then
   dup MEM-MAX-64K-BUFFERS > if E-MEM-SIZE throw then
   drop ;

: MEM-CHECK-CELL-COUNT ( count -- )
   dup COUNT>N 0 <= if E-MEM-SIZE throw then
   dup COUNT>N MEM-MAX-CELLS > if E-MEM-SIZE throw then
   drop ;

: MEM-64K-BYTES ( n -- n ) {: cnt :}
   cnt MEM-CHECK-64K-COUNT
   cnt MEM-64K * ;

: MEM-CELLS>BYTES ( count -- n )
   dup MEM-CHECK-CELL-COUNT
   COUNT>N cells ;

: MEM-64K-COUNT-FOR ( n -- n ) {: bytes :}
   bytes MEM-CHECK-SIZE
   bytes 1 - MEM-64K / 1 + dup MEM-CHECK-64K-COUNT ;

: MEM-64K-SPAN-BYTES ( n -- n )
   MEM-64K-COUNT-FOR MEM-64K-BYTES ;

: MEM-MMAP-RC ( n -- n ) {: bytes :}
   bytes MEM-CHECK-SIZE
   MEM-ADDR-ANY bytes MEM-PROT-RW MEM-MAP-PRIVATE-ANON MEM-ANON-FD MEM-OFF-ZERO mmap ;

\ Refines a validated successful mmap result to a byte pointer; syscall-result
\ refinement is not expressible. Retirement owner: habu-typed-defining-words-aa224eb5.
TRUSTED: MEM-ALLOC-PTR ( n -- ptr u8 )
   MEM-MMAP-RC dup 0 < if E-MEM-MAP throw then ;

: MEM-ALLOC-BYTES ( n -- ptr u8 n ) {: bytes :}
   bytes MEM-CHECK-SIZE
   bytes MEM-ALLOC-PTR bytes ;

: MEM-ALLOC-CELLS ( count -- ptr a )
   MEM-CELLS>BYTES MEM-ALLOC-PTR ;

: MEM-ALLOC-64K-BUFFERS ( n -- ptr u8 n )
   MEM-64K-BYTES MEM-ALLOC-BYTES ;

: MEM-ALLOC-64K-SPAN ( n -- ptr u8 n )
   MEM-64K-SPAN-BYTES MEM-ALLOC-BYTES ;

: MEM-ALLOC-64K ( -- ptr u8 n )
   1 MEM-ALLOC-64K-BUFFERS ;

\ ---- B5 package-first typed allocation surface (MODEL-CAD-V2-PLAN.md B5.5) -----
\
\ The raw MEM-ALLOC-* words above enforce positivity at RUNTIME on interchangeable
\ `n`. Package MEM re-states the same sizing as CHECKED words over CAD-NUM roles:
\ the scalar words (CELLS>BYTES, 64K-BYTES, 64K-COUNT-FOR, 64K-SPAN-BYTES) are
\ typed compositions of the closed B5.2 algebra that return `numeric-result<a>`
\ (zero is a valid scalar answer), while the allocation sinks (ALLOC-BYTES,
\ ALLOC-CELLS, ALLOC-64K) accept only the `alloc-*` roles, which reject zero and
\ over-allocation at VALIDATION - so a byte/cell role swap or a zero/overflow
\ allocation cannot reach `mmap`. MEM owns three audited private projections:
\ one allocation extent for `mmap` and whole-map `munmap`, one cell count for
\ `cells`, and one mapped byte extent for range `munmap`. Retire each when its
\ primitive accepts the nominal role directly.
\
\ The legacy MEM-ALLOC-BYTES surface stays untouched for its four caller waves;
\ MEM-ALLOC-CELLS and the multi-64K conveniences are out of this B5 wave.

package MEM

public

DEFLINEAR MEM:block

private

\ Internal invariant code (never reachable): a validator/narrowing arm proven
\ impossible by the input still needs an exhaustive MATCH arm. Mirrors the
\ CAD-NUM E-CADNUM-TOTALITY discipline; lives in-file, not lib/errors.f.

\ ---- ok extractors for compile-time-valid role constants (arms unreachable) ----
: OK-BYTE-LEN ( CAD-NUM:numeric-result<CAD-NUM:byte-len> -- CAD-NUM:byte-len )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-TOTALITY throw ENDOF
      zero OF E-MEM-TOTALITY throw ENDOF        overflow OF E-MEM-TOTALITY throw ENDOF
      underflow OF E-MEM-TOTALITY throw ENDOF   bad-alignment OF E-MEM-TOTALITY throw ENDOF
      misaligned OF E-MEM-TOTALITY throw ENDOF
   ;MATCH ;
: OK-ALIGNMENT ( CAD-NUM:numeric-result<CAD-NUM:alignment> -- CAD-NUM:alignment )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-TOTALITY throw ENDOF
      zero OF E-MEM-TOTALITY throw ENDOF        overflow OF E-MEM-TOTALITY throw ENDOF
      underflow OF E-MEM-TOTALITY throw ENDOF   bad-alignment OF E-MEM-TOTALITY throw ENDOF
      misaligned OF E-MEM-TOTALITY throw ENDOF
   ;MATCH ;
: OK-ALLOC-BYTE-LEN ( CAD-NUM:numeric-result<CAD-NUM:alloc-byte-len> -- CAD-NUM:alloc-byte-len )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-TOTALITY throw ENDOF
      zero OF E-MEM-TOTALITY throw ENDOF        overflow OF E-MEM-TOTALITY throw ENDOF
      underflow OF E-MEM-TOTALITY throw ENDOF   bad-alignment OF E-MEM-TOTALITY throw ENDOF
      misaligned OF E-MEM-TOTALITY throw ENDOF
   ;MATCH ;

\ ---- size-refusal extractors for caller-supplied n (arms REACHABLE) ------------
\ Unlike the OK-* extractors above (compile-time constants; refusal arms are
\ unreachable invariants -> E-MEM-TOTALITY), these narrow an arbitrary runtime
\ `n`, so a refusal is the real memory-sizing outcome and throws E-MEM-SIZE.
: SIZE-BYTE-LEN ( CAD-NUM:numeric-result<CAD-NUM:byte-len> -- CAD-NUM:byte-len )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-SIZE throw ENDOF
      zero OF E-MEM-SIZE throw ENDOF            overflow OF E-MEM-SIZE throw ENDOF
      underflow OF E-MEM-SIZE throw ENDOF       bad-alignment OF E-MEM-SIZE throw ENDOF
      misaligned OF E-MEM-SIZE throw ENDOF
   ;MATCH ;
: SIZE-ALLOC-BYTE-LEN ( CAD-NUM:numeric-result<CAD-NUM:alloc-byte-len> -- CAD-NUM:alloc-byte-len )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-SIZE throw ENDOF
      zero OF E-MEM-SIZE throw ENDOF            overflow OF E-MEM-SIZE throw ENDOF
      underflow OF E-MEM-SIZE throw ENDOF       bad-alignment OF E-MEM-SIZE throw ENDOF
      misaligned OF E-MEM-SIZE throw ENDOF
   ;MATCH ;
: SIZE-CELL-COUNT ( CAD-NUM:numeric-result<CAD-NUM:cell-count> -- CAD-NUM:cell-count )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-SIZE throw ENDOF
      zero OF E-MEM-SIZE throw ENDOF            overflow OF E-MEM-SIZE throw ENDOF
      underflow OF E-MEM-SIZE throw ENDOF       bad-alignment OF E-MEM-SIZE throw ENDOF
      misaligned OF E-MEM-SIZE throw ENDOF
   ;MATCH ;
: SIZE-ALLOC-CELL-COUNT ( CAD-NUM:numeric-result<CAD-NUM:alloc-cell-count> -- CAD-NUM:alloc-cell-count )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-SIZE throw ENDOF
      zero OF E-MEM-SIZE throw ENDOF            overflow OF E-MEM-SIZE throw ENDOF
      underflow OF E-MEM-SIZE throw ENDOF       bad-alignment OF E-MEM-SIZE throw ENDOF
      misaligned OF E-MEM-SIZE throw ENDOF
   ;MATCH ;

\ ---- the 64K granularity as validated CAD-NUM roles ---------------------------
\ MEM-64K is a compile-time positive power of two, so BYTE-LEN / ALIGNMENT /
\ AS-ALLOC-BYTE-LEN all succeed; the extractors' failure arms are unreachable.
: 64K-LEN ( -- CAD-NUM:byte-len )
   MEM-64K CAD-NUM:BYTE-LEN OK-BYTE-LEN ;
: 64K-ALIGN ( -- CAD-NUM:alignment )
   MEM-64K CAD-NUM:ALIGNMENT OK-ALIGNMENT ;
: 64K-ALLOC-LEN ( -- CAD-NUM:alloc-byte-len )
   64K-LEN CAD-NUM:AS-ALLOC-BYTE-LEN OK-ALLOC-BYTE-LEN ;

\ Private allocation-role erasure for mmap/cells/munmap; no raw value escapes.
\ Retire when those primitives accept the nominal roles directly.
\ Retirement owner: habu-epic-model-cad-70b629a9.
TRUSTED: ALLOC-BYTES>N ( CAD-NUM:alloc-byte-len -- n ) ;
TRUSTED: ALLOC-CELLS>N ( CAD-NUM:alloc-cell-count -- n ) ;
TRUSTED: BYTE-LEN>N ( CAD-NUM:byte-len -- n ) ;

$47 constant UNMAP-EXIT

: RELEASE-RANGE ( ptr u8 n -- )
   munmap 0 < if
      s" memory: unmap failed" UNMAP-EXIT die
   then ;

: B-ALLOC ( ptr u8 n -- ptr u8 n ) {: bytes:n :}
   drop bytes MEM-ALLOC-PTR bytes ;

: B-BYTE-LEN ( CAD-NUM:alloc-byte-len -- CAD-NUM:byte-len )
   ALLOC-BYTES>N CAD-NUM:BYTE-LEN OK-BYTE-LEN ;

MEM-MAX-N MEM-CELL-BYTES - MEM-CELL-BYTES / MEM-CELL-BYTES * constant B-MAX-LEN

: B-ALIGN ( n -- n )
   MEM-CELL-BYTES 1 - + MEM-CELL-BYTES / MEM-CELL-BYTES * ;

: B-SPAN ( n -- n )
   B-ALIGN MEM-CELL-BYTES + ;

\ The checker cannot bind the payload pointer and hidden validated extent to one
\ linear owner. The handle is the cell-aligned tail header, leaving the borrowed
\ payload pointer at the mmap base. Retirement owner:
\ habu-checker-ptr-lifetime-f59d1e9d.
TRUSTED: B-MINT ( ptr u8 CAD-NUM:alloc-byte-len -- MEM:block )
   dup ALLOC-BYTES>N B-ALIGN rot + tuck ! ;

TRUSTED: B-TAKE ( MEM:block -- ptr u8 CAD-NUM:alloc-byte-len )
   dup @ dup ALLOC-BYTES>N B-ALIGN rot swap - swap ;

public

: OPEN ( CAD-NUM:alloc-byte-len -- result<MEM:block,n> )
   dup ALLOC-BYTES>N
   dup B-MAX-LEN > if
      2drop E-MEM-SIZE RESULT:ERR
      exit
   then
   B-SPAN
   NULL$ drop swap [: B-ALLOC ;] catch {: code:n :}
   code 0 <> if
      2drop drop code RESULT:ERR
      exit
   then
   drop swap B-MINT RESULT:OK ;

\ The byte span is borrowed only until the returned owner is consumed; the
\ checker cannot yet bind pointer lifetime to that owner.
\ Retirement owner: habu-checker-ptr-lifetime-f59d1e9d.
: BORROW ( MEM:block -- MEM:block ptr u8 CAD-NUM:byte-len )
   B-TAKE 2dup B-MINT -rot B-BYTE-LEN ;

: RELEASE ( MEM:block -- )
   B-TAKE
   ALLOC-BYTES>N B-SPAN RELEASE-RANGE ;

\ ---- scalar sizing: typed compositions of the closed B5.2 algebra --------------
: CELLS>BYTES ( CAD-NUM:cell-count -- CAD-NUM:numeric-result<CAD-NUM:byte-len> )
   CAD-NUM:CELLS>BYTES ;
: 64K-BYTES ( CAD-NUM:item-count -- CAD-NUM:numeric-result<CAD-NUM:byte-len> )
   64K-LEN swap CAD-NUM:SCALE-BYTES ;
: 64K-SPAN-BYTES ( CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:byte-len> )
   64K-ALIGN CAD-NUM:ALIGN-UP-BYTES ;
: 64K-COUNT-FOR ( CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:item-count> )
   \ ceil(bytes / 64K) as a logical buffer count, purely over the typed
   \ extent-division op: the byte need is the extent, 64K the unit size. A zero
   \ need is 0 buffers (0 / 64K); 64K is a positive extent, so DIV-BYTES-CEIL's
   \ zero-size-unit refusal is unreachable. No raw cell is read here.
   64K-LEN CAD-NUM:DIV-BYTES-CEIL ;

\ ---- allocation sinks: only the alloc-* roles reach the mmap primitive ---------
: ALLOC-BYTES ( CAD-NUM:alloc-byte-len -- ptr u8 CAD-NUM:alloc-byte-len )
   dup ALLOC-BYTES>N MEM-ALLOC-PTR
   swap ;
: ALLOC-CELLS ( CAD-NUM:alloc-cell-count -- ptr a )
   ALLOC-CELLS>N cells MEM-ALLOC-PTR ;
: ALLOC-64K ( -- ptr u8 CAD-NUM:alloc-byte-len )
   64K-ALLOC-LEN ALLOC-BYTES ;

\ ---- release: return an ALLOC-BYTES mapping to the OS ---------------------------
: RELEASE-BYTES ( ptr u8 CAD-NUM:alloc-byte-len -- )
   ALLOC-BYTES>N RELEASE-RANGE ;

: UNMAP ( ptr u8 CAD-NUM:byte-len -- )
   BYTE-LEN>N RELEASE-RANGE ;

\ ---- caller-facing size narrowing: raw n -> validated alloc role --------------
\ The fixed-capacity buffer callers (source, codesign, content-key, object-cache,
\ process-argv, process-env) narrow a raw size to the positive alloc role BEFORE
\ the allocation sink; any refusal (zero/negative/overflow) throws E-MEM-SIZE.
\ Composes the public CAD-NUM validators only, so no new unchecked boundary.
: BYTES-ALLOC-LEN ( n -- CAD-NUM:alloc-byte-len )
   CAD-NUM:BYTE-LEN SIZE-BYTE-LEN
   CAD-NUM:AS-ALLOC-BYTE-LEN SIZE-ALLOC-BYTE-LEN ;
: CELLS-ALLOC-COUNT ( n -- CAD-NUM:alloc-cell-count )
   CAD-NUM:CELL-COUNT SIZE-CELL-COUNT
   CAD-NUM:AS-ALLOC-CELL-COUNT SIZE-ALLOC-CELL-COUNT ;

\ ---- quotation-scoped owned mapping --------------------------------------------
\ WITH-BYTES is a CHECKED composition of MEM:block ownership: OPEN mints the
\ owner (and refuses an oversize length as E-MEM-SIZE before any syscall, while
\ an ordinary mapping failure keeps its E-MEM-MAP code), BORROW lends the span
\ for the body, and RELEASE returns the mapping on the normal path. The scope
\ therefore holds exactly one owner from mint to release, and no raw pointer or
\ raw length is carried across the body.
\
\ One window stays trusted, and only one: running a body whose row is arbitrary.
\ Checked catch cannot carry an arbitrary result row through a fetched execution
\ token, so WB-GUARD parks the borrowed span and the body off-stack, runs the
\ body under catch, and - on a throw - releases the owner and rethrows the
\ body's code. The owner is named IN and OUT of WB-GUARD's own effect, so the
\ block never sits under the body's row and cannot be absorbed by it.
\ Retirement owner: habu-checker-ptr-lifetime-f59d1e9d.
private
PTR-VARIABLE WB-CUR-BUF
variable WB-CUR-LEN
variable WB-CUR-BODY

TRUSTED: WB-RUN-CUR ( -- )
   WB-CUR-BUF @ WB-CUR-LEN @ WB-CUR-BODY @ execute ;

\ typed-local-lint: allow-bare-local - `body` carries the row-polymorphic quotation
\ effect [ R ptr u8 CAD-NUM:alloc-byte-len -- S ], which a local annotation cannot express.
TRUSTED: WB-GUARD ( R MEM:block ptr u8 CAD-NUM:alloc-byte-len [ R ptr u8 CAD-NUM:alloc-byte-len -- S ] -- S MEM:block )
   {: body :}
   WB-CUR-BUF @ {: sb:ptr :} WB-CUR-LEN @ {: sl:n :} WB-CUR-BODY @ {: sbody:n :}
   {: blk:MEM:block fbuf:ptr flen:CAD-NUM:alloc-byte-len :}
   fbuf WB-CUR-BUF ! flen WB-CUR-LEN ! body WB-CUR-BODY !
   [: WB-RUN-CUR ;] catch {: code:n :}
   sb WB-CUR-BUF ! sl WB-CUR-LEN ! sbody WB-CUR-BODY !
   code 0 <> if blk RELEASE code throw then
   blk ;

public

\ typed-local-lint: allow-bare-local - `body` carries the row-polymorphic quotation
\ effect [ R ptr u8 CAD-NUM:alloc-byte-len -- S ], which a local annotation cannot express.
: WITH-BYTES ( R CAD-NUM:alloc-byte-len [ R ptr u8 CAD-NUM:alloc-byte-len -- S ] -- S )
   {: body :}
   OPEN
   MATCH result
      ok OF ENDOF
      err OF throw ENDOF
   ;MATCH
   BORROW
   \ The borrowed extent is the owner's own allocation length, so the alloc
   \ narrowing cannot refuse here; OK-ALLOC-BYTE-LEN reports a refusal as the
   \ internal invariant failure it would be.
   CAD-NUM:AS-ALLOC-BYTE-LEN OK-ALLOC-BYTE-LEN
   body WB-GUARD
   RELEASE ;

private
get-current prot-wid-add
public
get-current prot-wid-add
;package
