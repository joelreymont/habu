\ arena.f - an arena a program holds by pointer: zeroed allocations that never
\ move, release actions and child arenas, all released together. The arena's
\ header sits at the start of its first chunk; each later chunk is the larger
\ of a request with its chunk header and twice the chunk before it. No lock:
\ one task uses an arena at a time. A handle used after RELEASE is undefined,
\ as a HEAP record's is.
\ STORAGE CLASS. CALLER-OWNED: the caller releases each arena NEW makes.

require src/lib/memory.f
require src/lib/span.f

package ARENA
private

\ A release action and the allocation it receives, newest first.
STRUCTURE action DERIVE addr
   FIELD next ptr action
   FIELD alloc ptr u8
   FIELD run [ ptr u8 -- ]
;STRUCTURE

public

\ `self`, `chunks` and `prev` are a chunk's bytes: munmap and NEWEST take bytes,
\ and munmap's `ptr a` refuses a record pointer.
STRUCTURE arena OPAQUE DERIVE addr
   FIELD self ptr u8
   FIELD top n
   FIELD first-top n
   FIELD chunks ptr u8
   FIELD actions ptr action
   FIELD parent ptr arena
   FIELD kids ptr arena
   FIELD older ptr arena
   FIELD newer ptr arena
;STRUCTURE

private

$4000 constant FIRST-BYTES         \ the first chunk, header included
\ A larger request throws E-MEM-SIZE, so a request and its headers never overflow.
MEM-MAX-N FIRST-BYTES - constant MAX-REQUEST

\ A chunk after the first: the chunk before it, newest first, and its bytes.
STRUCTURE chunk DERIVE addr FIELD prev ptr u8 FIELD size n ;STRUCTURE

\ The records laid in chunk bytes, and the chunk a `ptr u8` link names.
CAST: >ARENA ( ptr u8 -- ptr arena )
CAST: >CHUNK ( ptr u8 -- ptr chunk )
CAST: >ACTION ( ptr u8 -- ptr action )

: FIRST-ERROR ( n n -- n ) over 0<> if drop else nip then ;
: RETHROW ( n -- ) dup 0= if drop exit then throw ;


: EXTENT ( n -- n )
   {: len:n :}
   len 0 <= len MAX-REQUEST > or if E-MEM-SIZE throw then
   len CELL 1- + CELL negate and ;


\ The newest chunk's base and bytes.
: NEWEST ( ptr arena -- ptr u8 n )
   {: a:ptr :}
   a ARENA-CHUNKS @ {: c:ptr :}
   c NULL-PTR = if a ARENA-SELF @ FIRST-BYTES exit then
   c c >CHUNK CHUNK-SIZE @ ;


\ `first-top` keeps the first chunk's top while a later chunk is newest. A
\ refused mapping throws E-MEM-MAP before the arena changes.
: GROW ( ptr arena n -- )
   {: a:ptr need:n :}
   a NEWEST nip {: last:n :}
   last MEM-MAX-N 2 / > if need else need last 2 * max then {: size:n :}
   size MEM-ALLOC-PTR byte-view {: base:ptr :}
   base >CHUNK {: c:ptr :}
   a ARENA-CHUNKS @ c CHUNK-PREV !
   size c CHUNK-SIZE !
   a ARENA-CHUNKS @ NULL-PTR = if a ARENA-TOP @ a ARENA-FIRST-TOP ! then
   CHUNK-BYTES a ARENA-TOP !
   base a ARENA-CHUNKS ! ;


\ `bytes`, a positive cell multiple, from the newest chunk, or from a later one
\ when they do not fit. Unused bytes of a chunk are zero.
: BUMP ( ptr arena n -- ptr u8 )
   {: a:ptr bytes:n :}
   a NEWEST nip a ARENA-TOP @ - bytes < if a bytes CHUNK-BYTES + GROW then
   a NEWEST drop a ARENA-TOP @ +
   bytes a ARENA-TOP +! ;


\ Answers the code the action threw, 0 when none.
: RUN-ACTION ( ptr action -- n )
   {: act:ptr :}
   [: act ACTION-ALLOC @ act ACTION-RUN @ execute ;] catch ;


\ Each action leaves the list before it runs, so a run cut short and repeated
\ runs none twice. Answers the first code an action threw, 0 when none.
: RUN-ACTIONS ( ptr arena -- n )
   {: a:ptr :}
   0 begin a ARENA-ACTIONS @ dup NULL-PTR <> while
      dup ACTION-NEXT @ a ARENA-ACTIONS !
      RUN-ACTION FIRST-ERROR
   repeat drop ;


: UNLINK ( ptr arena -- )
   {: a:ptr :}
   a ARENA-PARENT @ {: parent:ptr :}
   a ARENA-OLDER @ {: older:ptr :}
   a ARENA-NEWER @ {: newer:ptr :}
   parent NULL-PTR = if exit then
   newer NULL-PTR = if
      older parent ARENA-KIDS !
   else
      older newer ARENA-OLDER !
   then
   older NULL-PTR <> if newer older ARENA-NEWER ! then
   NULL-PTR a ARENA-PARENT ! ;


\ Answers the chunk before `base`, and E-MEM-UNMAP when the unmap was refused.
: UNMAP-CHUNK ( ptr u8 -- ptr u8 n )
   {: base:ptr :}
   base >CHUNK {: c:ptr :}
   c CHUNK-PREV @
   base c CHUNK-SIZE @ munmap 0< if E-MEM-UNMAP else 0 then ;


\ The arena forgets its later chunks before any is unmapped; every one is tried.
: DROP-CHUNKS ( ptr arena -- n )
   {: a:ptr :}
   a ARENA-CHUNKS @ {: newest:ptr :}
   newest NULL-PTR = if 0 exit then
   a ARENA-FIRST-TOP @ a ARENA-TOP !
   NULL-PTR a ARENA-CHUNKS !
   0 newest begin dup NULL-PTR <> while
      UNMAP-CHUNK rot swap FIRST-ERROR swap
   repeat drop ;


: ZERO-FIRST ( ptr arena -- )
   {: a:ptr :}
   0 a ARENA-SELF @ ARENA-BYTES + a ARENA-TOP @ ARENA-BYTES - SPAN:MAKE SPAN:FILL
   ARENA-BYTES a ARENA-TOP ! ;


\ Children end before this arena's actions run, so a child's action may use
\ what a parent's action frees. Answers the first code an action or a refused
\ unmap raised, 0 when none.
: SETTLE ( ptr arena bool -- n )
   {: a:ptr keep:bool :}
   0 begin a ARENA-KIDS @ dup NULL-PTR <> while
      false RECURSE FIRST-ERROR
   repeat drop
   a RUN-ACTIONS FIRST-ERROR
   a DROP-CHUNKS FIRST-ERROR
   keep if a ZERO-FIRST exit then
   a UNLINK
   a ARENA-SELF @ FIRST-BYTES munmap 0< if E-MEM-UNMAP FIRST-ERROR then ;


: WITH-BODY ( R ptr arena [ R ptr arena -- S ] -- S ) execute ;

public

: NEW ( -- ptr arena )
   FIRST-BYTES MEM-ALLOC-PTR byte-view {: self:ptr :}
   self >ARENA {: a:ptr :}
   self a ARENA-SELF !
   ARENA-BYTES a ARENA-TOP !
   a ;


: NEW-IN ( ptr arena -- ptr arena )
   {: parent:ptr :}
   NEW {: a:ptr :}
   parent ARENA-KIDS @ {: older:ptr :}
   parent a ARENA-PARENT !
   older a ARENA-OLDER !
   older NULL-PTR <> if a older ARENA-NEWER ! then
   a parent ARENA-KIDS !
   a ;


\ Answers `len` as asked; the bytes are rounded up to a cell.
: ALLOC ( ptr arena n -- ptr u8 n )
   {: a:ptr len:n :}
   a len EXTENT BUMP len ;


\ The allocation and its action share one bump, so a refused mapping leaves
\ neither.
: ALLOC-DISPOSE ( ptr arena n [ ptr u8 -- ] -- ptr u8 n )
   {: a:ptr len:n run :}
   len EXTENT {: bytes:n :}
   a bytes ACTION-BYTES + BUMP {: p:ptr :}
   p bytes + >ACTION {: act:ptr :}
   p act ACTION-ALLOC !
   run act ACTION-RUN !
   a ARENA-ACTIONS @ act ACTION-NEXT !
   act a ARENA-ACTIONS !
   p len ;


\ Each action caught; the first code one threw is rethrown after the rest.
: RESET ( ptr arena -- ) true SETTLE RETHROW ;
: RELEASE ( ptr arena -- ) dup NULL-PTR = if drop exit then false SETTLE RETHROW ;


: WITH ( R [ R ptr arena -- S ] -- S )
   {: body :}
   NEW {: a:ptr :}
   a body [: WITH-BODY ;] [: a RELEASE ;] finally ;

;package
