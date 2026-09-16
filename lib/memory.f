\ memory.f - checked OS-backed byte buffers.
\
\ Load after lib/errors.f.
\
\ STORAGE CLASS. CALLER-OWNED: every mapping ALLOC-BYTES, ALLOC-CELLS,
\ ALLOC-64K and ALLOC-GUARDED hand back belongs to the caller, who releases it,
\ and the module keeps nothing about it. The one exception is WITH-BYTES: its
\ quotation-scoped mapping stack (WB-DEPTH and the two dynamic buffers beside
\ it) is PROCESS-WIDE, so WITH-BYTES scopes nest per image and not per task.
\ See docs/threads.md.

s" lib/errors.f" required
require lib/num-arithmetic.f

$10000 constant MEM-64K
$7FFFFFFFFFFFFFFF constant MEM-MAX-N
MEM-MAX-N MEM-64K / constant MEM-MAX-64K-BUFFERS
1 cells constant MEM-CELL-BYTES
MEM-MAX-N MEM-CELL-BYTES / constant MEM-MAX-CELLS

0 constant MEM-ADDR-ANY
0 constant MEM-PROT-NONE
3 constant MEM-PROT-RW
1 constant MEM-MAP-SHARED
$1002 constant MEM-MAP-PRIVATE-ANON
$1012 constant MEM-MAP-PRIVATE-ANON-FIXED   \ the engine's mmap treats $10 as MAP_FIXED on every target
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

\ Fresh storage gets its element type from the caller. The engine owns only
\ the OS mapping and its pointer/error result; sizing and failure policy are checked.
: MEM-ALLOC-PTR ( n -- ptr a )
   dup MEM-CHECK-SIZE
   map-anon 0<> if drop E-MEM-MAP throw then ;

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

\ The address a raw `mmap` returned, as the byte pointer of the mapping this
\ word just made; the only place in this file a bare address becomes a pointer.
TRUSTED: MEM-MAPPED>PTR ( n -- ptr u8 ) ;

\ ---- guarded VM stacks -------------------------------------------------------
\ A stack the engine may run on (run-in-stack, a task's data, return and loop
\ stacks) is a mapping with an inaccessible page on each side: a push past its
\ capacity or a read below its base faults, and the engine's crash handler
\ names the stack (src/habu/crash.f). This is the only way to make such a
\ stack: run-in-stack refuses any other extent (E-STACK-UNGUARDED), because a
\ heap buffer has nothing beyond it to stop an overflow. The layout is the one
\ src/habu/rt.f STACK-GUARD:EMIT-MAP gives the boot stacks: the whole span is
\ mapped inaccessible first, then the capacity is remapped read/write at a
\ STACK-ABI:PAGE-BYTES boundary inside it, so the pages on both sides stay
\ inaccessible whatever granule the kernel returned.
: MEM-GUARDED-SPAN-BYTES ( n -- n ) {: cap:n :}
   cap STACK-ABI:PAGE-BYTES 3 * + ;

: MEM-GUARDED-BASE ( n n -- n ) {: span:n cap:n :}
   span STACK-ABI:PAGE-BYTES 2 * + 1 - STACK-ABI:PAGE-BYTES negate and ;

: MEM-UNMAP-SLIVER ( n n -- ) {: at:n bytes:n :}
   bytes 0 = if exit then
   at MEM-MAPPED>PTR bytes munmap 0 <> if E-MEM-UNMAP throw then ;

\ The kernel hands back a span on its own granule, so the page-aligned base
\ lands up to one page into it. The head and tail slivers outside
\ [base - PAGE, base + cap + PAGE) are unmapped right away, so the extent that
\ stays mapped is exactly the two guard pages around the capacity and the
\ release below can unmap exactly that: a release computed from the span
\ instead reached past the span's end into whatever the kernel mapped next
\ (a task's DATA region, found by test/address-cell-tasks.f, 2026-09-16).
: MEM-ALLOC-GUARDED ( n -- ptr u8 n ) {: cap:n :}
   cap STACK-ABI:PAGE-BYTES mod 0 <> if E-MEM-SIZE throw then
   cap MEM-GUARDED-SPAN-BYTES {: span-bytes:n :}
   MEM-ADDR-ANY span-bytes MEM-PROT-NONE MEM-MAP-PRIVATE-ANON MEM-ANON-FD MEM-OFF-ZERO mmap {: span:n :}
   span 0 < if E-MEM-MAP throw then
   span cap MEM-GUARDED-BASE {: base:n :}
   base STACK-ABI:PAGE-BYTES - {: lo:n :}
   base cap + STACK-ABI:PAGE-BYTES + {: hi:n :}
   span lo span - MEM-UNMAP-SLIVER
   hi span span-bytes + hi - MEM-UNMAP-SLIVER
   base cap MEM-PROT-RW MEM-MAP-PRIVATE-ANON-FIXED MEM-ANON-FD MEM-OFF-ZERO mmap base <> if
      E-MEM-MAP throw
   then
   base MEM-MAPPED>PTR cap ;

\ Release a guarded stack: exactly the extent MEM-ALLOC-GUARDED kept, the
\ capacity and its two guard pages.
: MEM-RELEASE-GUARDED ( ptr u8 n -- ) {: base:ptr cap:n :}
   base STACK-ABI:PAGE-BYTES - cap STACK-ABI:PAGE-BYTES 2 * + munmap 0 <> if E-MEM-UNMAP throw then ;

\ ---- package-first typed allocation surface -----------------------------------
\
\ The raw MEM-ALLOC-* words above enforce positivity at RUNTIME on interchangeable
\ `n`. Package MEM re-states the same sizing as CHECKED words over NUM roles:
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
private

\ Internal invariant code (never reachable): a validator/narrowing arm proven
\ impossible by the input still needs an exhaustive MATCH arm. Mirrors the
\ NUM E-NUM-TOTALITY discipline; lives in-file, not lib/errors.f.

\ ---- ok extractors for compile-time-valid role constants (arms unreachable) ----
: OK-BYTE-LEN ( NUM:numeric-result<NUM:byte-len> -- NUM:byte-len )
   MATCH NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-TOTALITY throw ENDOF
      zero OF E-MEM-TOTALITY throw ENDOF        overflow OF E-MEM-TOTALITY throw ENDOF
      underflow OF E-MEM-TOTALITY throw ENDOF   bad-alignment OF E-MEM-TOTALITY throw ENDOF
      misaligned OF E-MEM-TOTALITY throw ENDOF
   ;MATCH ;
: OK-ALIGNMENT ( NUM:numeric-result<NUM:alignment> -- NUM:alignment )
   MATCH NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-TOTALITY throw ENDOF
      zero OF E-MEM-TOTALITY throw ENDOF        overflow OF E-MEM-TOTALITY throw ENDOF
      underflow OF E-MEM-TOTALITY throw ENDOF   bad-alignment OF E-MEM-TOTALITY throw ENDOF
      misaligned OF E-MEM-TOTALITY throw ENDOF
   ;MATCH ;
: OK-ALLOC-BYTE-LEN ( NUM:numeric-result<NUM:alloc-byte-len> -- NUM:alloc-byte-len )
   MATCH NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-TOTALITY throw ENDOF
      zero OF E-MEM-TOTALITY throw ENDOF        overflow OF E-MEM-TOTALITY throw ENDOF
      underflow OF E-MEM-TOTALITY throw ENDOF   bad-alignment OF E-MEM-TOTALITY throw ENDOF
      misaligned OF E-MEM-TOTALITY throw ENDOF
   ;MATCH ;

\ ---- size-refusal extractors for caller-supplied n (arms REACHABLE) ------------
\ Unlike the OK-* extractors above (compile-time constants; refusal arms are
\ unreachable invariants -> E-MEM-TOTALITY), these narrow an arbitrary runtime
\ `n`, so a refusal is the real memory-sizing outcome and throws E-MEM-SIZE.
: SIZE-BYTE-LEN ( NUM:numeric-result<NUM:byte-len> -- NUM:byte-len )
   MATCH NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-SIZE throw ENDOF
      zero OF E-MEM-SIZE throw ENDOF            overflow OF E-MEM-SIZE throw ENDOF
      underflow OF E-MEM-SIZE throw ENDOF       bad-alignment OF E-MEM-SIZE throw ENDOF
      misaligned OF E-MEM-SIZE throw ENDOF
   ;MATCH ;
: SIZE-ALLOC-BYTE-LEN ( NUM:numeric-result<NUM:alloc-byte-len> -- NUM:alloc-byte-len )
   MATCH NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-SIZE throw ENDOF
      zero OF E-MEM-SIZE throw ENDOF            overflow OF E-MEM-SIZE throw ENDOF
      underflow OF E-MEM-SIZE throw ENDOF       bad-alignment OF E-MEM-SIZE throw ENDOF
      misaligned OF E-MEM-SIZE throw ENDOF
   ;MATCH ;
: SIZE-CELL-COUNT ( NUM:numeric-result<NUM:cell-count> -- NUM:cell-count )
   MATCH NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-SIZE throw ENDOF
      zero OF E-MEM-SIZE throw ENDOF            overflow OF E-MEM-SIZE throw ENDOF
      underflow OF E-MEM-SIZE throw ENDOF       bad-alignment OF E-MEM-SIZE throw ENDOF
      misaligned OF E-MEM-SIZE throw ENDOF
   ;MATCH ;
: SIZE-ALLOC-CELL-COUNT ( NUM:numeric-result<NUM:alloc-cell-count> -- NUM:alloc-cell-count )
   MATCH NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-SIZE throw ENDOF
      zero OF E-MEM-SIZE throw ENDOF            overflow OF E-MEM-SIZE throw ENDOF
      underflow OF E-MEM-SIZE throw ENDOF       bad-alignment OF E-MEM-SIZE throw ENDOF
      misaligned OF E-MEM-SIZE throw ENDOF
   ;MATCH ;

\ ---- the 64K granularity as validated NUM roles ---------------------------
\ MEM-64K is a compile-time positive power of two, so BYTE-LEN / ALIGNMENT /
\ AS-ALLOC-BYTE-LEN all succeed; the extractors' failure arms are unreachable.
: 64K-LEN ( -- NUM:byte-len )
   MEM-64K NUM:BYTE-LEN OK-BYTE-LEN ;
: 64K-ALIGN ( -- NUM:alignment )
   MEM-64K NUM:ALIGNMENT OK-ALIGNMENT ;
: 64K-ALLOC-LEN ( -- NUM:alloc-byte-len )
   64K-LEN NUM:AS-ALLOC-BYTE-LEN OK-ALLOC-BYTE-LEN ;

\ Private allocation-role erasure for mmap/cells/munmap; no raw value escapes.
\ Checked casts, so none of the three can misdeclare its shape. They go away
\ entirely when those primitives accept the nominal roles directly:
\ habu-epic-model-cad-70b629a9.
CAST: ALLOC-BYTES>N ( NUM:alloc-byte-len -- n )
CAST: ALLOC-CELLS>N ( NUM:alloc-cell-count -- n )
CAST: BYTE-LEN>N ( NUM:byte-len -- n )

$47 constant UNMAP-EXIT

: RELEASE-RANGE ( ptr u8 n -- )
   munmap 0 < if
      s" memory: unmap failed" UNMAP-EXIT die
   then ;

public

\ ---- scalar sizing: typed compositions of the closed B5.2 algebra --------------
: CELLS>BYTES ( NUM:cell-count -- NUM:numeric-result<NUM:byte-len> )
   NUM:CELLS>BYTES ;
: 64K-BYTES ( NUM:item-count -- NUM:numeric-result<NUM:byte-len> )
   64K-LEN swap NUM:SCALE-BYTES ;
: 64K-SPAN-BYTES ( NUM:byte-len -- NUM:numeric-result<NUM:byte-len> )
   64K-ALIGN NUM:ALIGN-UP-BYTES ;
: 64K-COUNT-FOR ( NUM:byte-len -- NUM:numeric-result<NUM:item-count> )
   \ ceil(bytes / 64K) as a logical buffer count, purely over the typed
   \ extent-division op: the byte need is the extent, 64K the unit size. A zero
   \ need is 0 buffers (0 / 64K); 64K is a positive extent, so DIV-BYTES-CEIL's
   \ zero-size-unit refusal is unreachable. No raw cell is read here.
   64K-LEN NUM:DIV-BYTES-CEIL ;

\ ---- allocation sinks: only the alloc-* roles reach the mmap primitive ---------
: ALLOC-BYTES ( NUM:alloc-byte-len -- ptr u8 NUM:alloc-byte-len )
   dup ALLOC-BYTES>N MEM-ALLOC-PTR
   swap ;
: ALLOC-CELLS ( NUM:alloc-cell-count -- ptr a )
   ALLOC-CELLS>N cells MEM-ALLOC-PTR ;
: ALLOC-64K ( -- ptr u8 NUM:alloc-byte-len )
   64K-ALLOC-LEN ALLOC-BYTES ;

\ ---- release: return an ALLOC-BYTES mapping to the OS ---------------------------
: RELEASE-BYTES ( ptr u8 NUM:alloc-byte-len -- )
   ALLOC-BYTES>N RELEASE-RANGE ;

: UNMAP ( ptr u8 NUM:byte-len -- )
   BYTE-LEN>N RELEASE-RANGE ;

\ ---- caller-facing size narrowing: raw n -> validated alloc role --------------
\ The fixed-capacity buffer callers (source, codesign, content-key, object-cache,
\ process-argv, process-env) narrow a raw size to the positive alloc role BEFORE
\ the allocation sink; any refusal (zero/negative/overflow) throws E-MEM-SIZE.
\ Composes the public NUM validators only, so no new unchecked boundary.
: BYTES-ALLOC-LEN ( n -- NUM:alloc-byte-len )
   NUM:BYTE-LEN SIZE-BYTE-LEN
   NUM:AS-ALLOC-BYTE-LEN SIZE-ALLOC-BYTE-LEN ;
: CELLS-ALLOC-COUNT ( n -- NUM:alloc-cell-count )
   NUM:CELL-COUNT SIZE-CELL-COUNT
   NUM:AS-ALLOC-CELL-COUNT SIZE-ALLOC-CELL-COUNT ;

\ ---- quotation-scoped owned mapping --------------------------------------------
\ Each scope records a typed pointer and validated extent. The null pointer is
\ its pending state until allocation succeeds. Grow before entering the scope;
\ the outer cleanup also releases storage after a partial reserve failure.
private

DYNAMIC-BUFFER WB-BUFFERS ptr u8
DYNAMIC-BUFFER WB-LENGTHS NUM:alloc-byte-len
variable WB-DEPTH


: WB-RESERVE ( -- )
   WB-DEPTH @ 1+ dup WB-BUFFERS-RESERVE WB-LENGTHS-RESERVE ;


: WB-CACHE-RELEASE ( -- )
   WB-DEPTH @ 0= if WB-BUFFERS-RELEASE WB-LENGTHS-RELEASE then ;


: WB-LEAVE ( -- )
   WB-DEPTH @ 1- {: at:n :}
   at WB-BUFFERS @ dup 0= if drop else
      at WB-LENGTHS @ RELEASE-BYTES
   then
   at WB-DEPTH ! ;


: WB-ALLOC-RUN ( R NUM:alloc-byte-len [ R ptr u8 NUM:alloc-byte-len -- S ] -- S )
   {: body :}
   ALLOC-BYTES
   over WB-DEPTH @ 1- WB-BUFFERS !
   body execute ;


: WB-RUN ( R NUM:alloc-byte-len [ R ptr u8 NUM:alloc-byte-len -- S ] -- S )
   {: body :}
   dup WB-DEPTH @ WB-LENGTHS !
   NULL-PTR WB-DEPTH @ WB-BUFFERS !
   1 WB-DEPTH +!
   body [: WB-ALLOC-RUN ;] [: WB-LEAVE ;] finally ;


: WB-SCOPE ( R NUM:alloc-byte-len [ R ptr u8 NUM:alloc-byte-len -- S ] -- S )
   [: WB-RESERVE WB-RUN ;] [: WB-CACHE-RELEASE ;] finally ;

public

: WITH-BYTES ( R NUM:alloc-byte-len [ R ptr u8 NUM:alloc-byte-len -- S ] -- S )
   WB-SCOPE ;
;package
