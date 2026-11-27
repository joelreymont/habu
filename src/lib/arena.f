\ arena.f - an arena a program holds by pointer: zeroed allocations that never
\ move, release actions and child arenas, all released together, or those
\ since a MARK by CUT. The arena's header sits at the start of its first
\ chunk; each later chunk is the larger of a request with its chunk header
\ and twice the chunk before it. No lock:
\ one task uses an arena at a time. A handle used after RELEASE is undefined,
\ as a HEAP record's is. An arena provides an allocator whose free does nothing
\ and whose resize leaves a moved block's old bytes in place. RELEASE keeps up
\ to SPARE-N first chunks, zeroed, for the next NEW, at most 64 KiB in all, and
\ unmaps the rest, so a NEW paired with RELEASE maps nothing in steady state.
\ TRIM unmaps the spares, for a program about to wait.
\ STORAGE CLASS. CALLER-OWNED: the caller releases each arena NEW makes.
\ PROCESS-WIDE: the spare first chunks, a CAS per move; a build leaves them out.

require src/lib/memory.f
require src/lib/span.f
require src/lib/allocator.f

package ARENA
private

\ A release action and the allocation it receives, newest first; `born` is the
\ arena's position before its bump.
STRUCTURE action DERIVE addr
   FIELD next ^action
   FIELD born n
   FIELD alloc ^u8
   FIELD run [ ^u8 -- ]
;STRUCTURE

\ A chunk after the first: the chunk before it, newest first, its bytes, and
\ the arena's position when it was mapped, where the chunk before it ends.
STRUCTURE chunk DERIVE addr
   FIELD prev ^chunk
   FIELD size n
   FIELD start n
;STRUCTURE

public

STRUCTURE arena OPAQUE DERIVE addr
   FIELD top n
   FIELD chunks ^chunk
   FIELD actions ^action
   FIELD parent ^arena
   FIELD kids ^arena
   FIELD older ^arena
   FIELD newer ^arena
   FIELD born n        \ the parent's position before NEW-IN
   FIELD floor n       \ the last MARK's answer or CUT's mark
   FIELD mapped n      \ chunk bytes it and its live children hold
   FIELD peak n        \ the most `mapped` has been
   FIELD allocator ALLOC:allocator
;STRUCTURE

private

$4000 constant FIRST-BYTES         \ the first chunk, header included
ARENA-BYTES constant EMPTY         \ an empty arena's position

\ Each cell is 0 or a zeroed first chunk no arena holds. A task takes or gives
\ one by a compare-and-swap of its cell, so each kept chunk goes to one NEW.
4 constant SPARE-N
create SPARES SPARE-N cells SCRATCH-ALLOT

\ The records laid in chunk bytes.
CAST: >ARENA ( ^u8 -- ^arena )
CAST: SPARE>ARENA ( n -- ^arena )
CAST: >ACTION ( ^u8 -- ^action )

: FIRST-ERROR ( n n -- n ) over 0<> if drop else nip then ;
: RETHROW ( n -- ) dup 0= if drop exit then throw ;


: EXTENT ( n -- n )
   CELL 1- + CELL negate and ;


\ The newest chunk's base and bytes.
: NEWEST ( ^arena -- ^u8 n )
   {: a:^ :}
   a ARENA-CHUNKS @ {: c:^ :}
   c NULL-PTR = if a BYTE-VIEW FIRST-BYTES exit then
   c BYTE-VIEW c CHUNK-SIZE @ ;


\ The position at the newest chunk's first byte.
: START ( ^arena -- n )
   {: a:^ :}
   a ARENA-CHUNKS @ {: c:^ :}
   c NULL-PTR = if 0 exit then
   c CHUNK-START @ ;

\ The arena's position: the bytes below its chunks' tops, headers included.
: POSITION ( ^arena -- n )
   {: a:^ :}
   a START a ARENA-TOP @ + ;


\ Adds `n`, below zero to take bytes away, to `mapped` of the arena and of
\ each arena above it, raising each one's `peak` to its `mapped`.
: ADD-MAPPED ( ^arena n -- )
   {: a:^ n:n :}
   a NULL-PTR = if exit then
   n a ARENA-MAPPED +!
   a ARENA-MAPPED @ a ARENA-PEAK @ max a ARENA-PEAK !
   a ARENA-PARENT @ n RECURSE ;

public

\ Answers the position; until the next MARK or CUT, a block shrunk in place
\ leaves the top at or above it.
: MARK ( ^arena -- n )
   {: a:^ :}
   a POSITION dup a ARENA-FLOOR ! ;


\ The most chunk bytes the arena and those made in it have held at once since
\ NEW, each chunk counted whole whether its pages were touched or not.
: PEAK ( ^arena -- n ) ARENA-PEAK @ ;

private


\ A failed mapping throws E-MEM-MAP before the arena changes.
: GROW ( ^arena n -- )
   {: a:^ need:n :}
   a NEWEST nip {: last:n :}
   last MEM-MAX-N 2 / > if need else need last 2 * max then {: size:n :}
   size MEM-ALLOC-PTR {: c:^chunk :}
   a ARENA-CHUNKS @ c CHUNK-PREV !
   size c CHUNK-SIZE !
   a POSITION c CHUNK-START !
   CHUNK-BYTES a ARENA-TOP !
   c a ARENA-CHUNKS !
   a size ADD-MAPPED ;


\ `bytes`, a positive cell multiple, from the newest chunk, or from a later one
\ when they do not fit. Unused bytes of a chunk are zero.
: BUMP ( ^arena n -- ^u8 )
   {: a:^ bytes:n :}
   a NEWEST nip a ARENA-TOP @ - bytes < if a bytes CHUNK-BYTES + GROW then
   a NEWEST drop a ARENA-TOP @ +
   bytes a ARENA-TOP +! ;


\ Answers the code the action threw, 0 when none.
: RUN-ACTION ( ^action -- n )
   {: act:^ :}
   [: act ACTION-ALLOC @ act ACTION-RUN @ execute ;] catch ;


\ The newest action registered at or after `mark`, else NULL-PTR.
: ACTION-SINCE ( ^arena n -- ^action )
   {: a:^ mark:n :}
   a ARENA-ACTIONS @ {: act:^ :}
   act NULL-PTR = if act exit then
   act ACTION-BORN @ mark >= if act exit then
   NULL-PTR ;


\ The newest child made at or after `mark`, else NULL-PTR.
: KID-SINCE ( ^arena n -- ^arena )
   {: a:^ mark:n :}
   a ARENA-KIDS @ {: kid:^ :}
   kid NULL-PTR = if kid exit then
   kid ARENA-BORN @ mark >= if kid exit then
   NULL-PTR ;


\ The actions registered since `mark`. Each leaves the list before it runs,
\ so a run cut short and repeated runs none twice. Answers the first code an
\ action threw, 0 when none.
: RUN-ACTIONS ( ^arena n -- n )
   {: a:^ mark:n :}
   0 begin a mark ACTION-SINCE dup NULL-PTR <> while
      dup ACTION-NEXT @ a ARENA-ACTIONS !
      RUN-ACTION FIRST-ERROR
   repeat drop ;


: UNLINK ( ^arena -- )
   {: a:^ :}
   a ARENA-PARENT @ {: parent:^ :}
   a ARENA-OLDER @ {: older:^ :}
   a ARENA-NEWER @ {: newer:^ :}
   parent NULL-PTR = if exit then
   newer NULL-PTR = if
      older parent ARENA-KIDS !
   else
      older newer ARENA-OLDER !
   then
   older NULL-PTR <> if newer older ARENA-NEWER ! then
   NULL-PTR a ARENA-PARENT ! ;


\ Answers the chunk before c, and E-MEM-UNMAP when the unmap was rejected.
: UNMAP-CHUNK ( ^chunk -- ^chunk n )
   {: c:^ :}
   c CHUNK-PREV @
   c BYTE-VIEW c CHUNK-SIZE @ munmap 0< if E-MEM-UNMAP else 0 then ;


\ The arena forgets its newest chunk, then unmaps it.
: POP-CHUNK ( ^arena -- n )
   {: a:^ :}
   a ARENA-CHUNKS @ {: c:^ :}
   c CHUNK-PREV @ a ARENA-CHUNKS !
   c CHUNK-START @ a START - a ARENA-TOP !
   a c CHUNK-SIZE @ negate ADD-MAPPED
   c UNMAP-CHUNK nip ;


\ The chunks mapped since `mark` are unmapped, every one tried, and the
\ bytes above it zeroed.
: CHOP ( ^arena n -- n )
   {: a:^ mark:n :}
   0 begin mark a START <= while a POP-CHUNK FIRST-ERROR repeat
   mark a START - {: off:n :}
   a NEWEST drop off + {: above:^ :}
   0 above a ARENA-TOP @ off - SPAN:MAKE SPAN:FILL
   off a ARENA-TOP ! ;


\ Spare `i`'s cell.
: SPARE ( n -- ^n ) cells SPARES + ;


\ A spare first chunk's address, 0 when none is spare.
: TAKE-SPARE ( -- n )
   SPARE-N 0 do
      i SPARE atomic@ dup 0<> if
         dup 0 i SPARE atomic-cas over = if unloop exit then
      then drop
   loop 0 ;


\ True when a free spare cell took the zeroed chunk at `c`.
: KEEP-SPARE ( n -- bool )
   {: c:n :}
   SPARE-N 0 do
      0 c i SPARE atomic-cas 0= if true unloop exit then
   loop false ;


\ Answers E-MEM-UNMAP when the unmap of a's first chunk was rejected, else 0.
: UNMAP-FIRST ( ^arena -- n )
   BYTE-VIEW FIRST-BYTES munmap 0< if E-MEM-UNMAP else 0 then ;


\ The released arena's first chunk is zeroed and kept, or unmapped when every
\ spare cell is full. Answers E-MEM-UNMAP when the unmap was rejected, else 0.
: GIVE-BACK ( ^arena -- n )
   {: a:^ :}
   0 a BYTE-VIEW a ARENA-TOP @ SPAN:MAKE SPAN:FILL
   a NULL-PTR - KEEP-SPARE if 0 exit then
   a UNMAP-FIRST ;


\ The arena back to `mark`, released unless kept. Children made since end
\ before the actions registered since run, so a child's action may use what
\ a parent's action frees. Answers the first action or unmap code, else 0.
: SETTLE ( ^arena n bool -- n )
   {: a:^ mark:n keep:bool :}
   0 begin a mark KID-SINCE dup NULL-PTR <> while
      EMPTY false RECURSE FIRST-ERROR
   repeat drop
   a mark RUN-ACTIONS FIRST-ERROR
   a mark CHOP FIRST-ERROR
   keep if exit then
   a ARENA-PARENT @ FIRST-BYTES negate ADD-MAPPED
   a UNLINK
   a GIVE-BACK FIRST-ERROR ;


: WITH-BODY ( R ^arena [ R ^arena -- S ] -- S ) execute ;


\ The arena's allocator: its state is the arena's own first-chunk bytes.
: SELF-ALLOC ( ^u8 n -- ^u8 ) swap >ARENA swap EXTENT BUMP ;


\ A shrink zeroes the cut bytes before a later growth reads them or the
\ newest block gives them back to its chunk.
: ZERO-TAIL ( ^u8 n n -- )
   {: block:^ old:n new:n :}
   new old < if 0 block new + old new - SPAN:MAKE SPAN:FILL then ;


\ True when the block is the newest allocation and its chunk has `more` bytes
\ after it; `more` is below zero when the block shrinks.
: IN-PLACE? ( ^arena ^u8 n n -- bool )
   {: a:^ block:^ extent:n more:n :}
   a NEWEST {: base:^ size:n :}
   block extent + base a ARENA-TOP @ + =
   size a ARENA-TOP @ - more >= and ;


\ Moves the top by `more`, never below the floor.
: MOVE-TOP ( ^arena n -- )
   {: a:^ more:n :}
   a ARENA-TOP @ more + a ARENA-FLOOR @ a START - max
   a ARENA-TOP ! ;


: SELF-RESIZE ( ^u8 ^u8 n n -- ^u8 )
   {: st:^ block:^ old:n new:n :}
   st >ARENA {: a:^ :}
   old EXTENT {: oe:n :}
   new EXTENT oe - {: more:n :}
   block old new ZERO-TAIL
   a block oe more IN-PLACE? if a more MOVE-TOP block exit then
   more 0 <= if block exit then
   a oe more + BUMP {: moved:^ :}
   block moved old BYTE-COPY moved ;
\ An arena frees its bytes together, at CUT, RESET or RELEASE.
: SELF-FREE ( ^u8 ^u8 -- ) drop drop ;

\ A spare first chunk, else a new mapping.
: FIRST-CHUNK ( -- ^arena )
   TAKE-SPARE {: c:n :}
   c 0<> if c SPARE>ARENA exit then
   FIRST-BYTES MEM-ALLOC-PTR {: a:^arena :} a ;

public

: NEW ( -- ^arena )
   FIRST-CHUNK {: a:^arena :}
   ARENA-BYTES a ARENA-TOP !
   EMPTY a ARENA-FLOOR !
   FIRST-BYTES a ARENA-MAPPED !
   FIRST-BYTES a ARENA-PEAK !
   a BYTE-VIEW [: SELF-ALLOC ;] [: SELF-RESIZE ;] [: SELF-FREE ;]
   a ARENA-ALLOCATOR ALLOC:INIT
   a ;


\ The cell bumped from the parent puts a MARK taken after NEW-IN above the
\ child's `born`, so a CUT to it keeps the child.
: NEW-IN ( ^arena -- ^arena )
   {: parent:^ :}
   parent POSITION {: at:n :}
   parent CELL BUMP drop
   NEW {: a:^ :}
   at a ARENA-BORN !
   parent ARENA-KIDS @ {: older:^ :}
   parent a ARENA-PARENT !
   older a ARENA-OLDER !
   older NULL-PTR <> if a older ARENA-NEWER ! then
   a parent ARENA-KIDS !
   parent FIRST-BYTES ADD-MAPPED
   a ;


: ALLOCATOR ( ^arena -- ^ALLOC:allocator ) ARENA-ALLOCATOR ;


\ Answers `len` as asked; the bytes are rounded up to a cell.
: ALLOC ( ^arena n -- ^u8 n )
   {: a:^ len:n :}
   a len EXTENT BUMP len ;


\ The bytes copied into the arena; an empty string allocates nothing.
: COPY ( str ^arena -- str )
   {: s:str a:^ :}
   s STR:LEN 0= if s exit then
   a s STR:LEN ALLOC SPAN:MAKE {: d :}
   s d SPAN:COPY
   d SPAN:$ ;


\ The allocation and its action share one bump, so a failed mapping leaves
\ neither. The action runs on this address: the block is never resized.
: ALLOC-DISPOSE ( ^arena n [ ^u8 -- ] -- ^u8 n )
   {: a:^ len:n run :}
   len EXTENT {: bytes:n :}
   a POSITION {: at:n :}
   a bytes ACTION-BYTES + BUMP {: p:^ :}
   p bytes + >ACTION {: act:^ :}
   p act ACTION-ALLOC !
   at act ACTION-BORN !
   run act ACTION-RUN !
   a ARENA-ACTIONS @ act ACTION-NEXT !
   act a ARENA-ACTIONS !
   p len ;


\ The children made and actions registered since `mark` end as RELEASE ends
\ them, the chunks mapped since are unmapped and the bytes above it zeroed.
\ Bytes above the mark are undefined after CUT, a block grown across it
\ included, as after RESET. Each action caught; the first code one threw is
\ rethrown after the rest.
: CUT ( ^arena n -- )
   {: a:^ mark:n :}
   mark a ARENA-FLOOR !
   a mark true SETTLE RETHROW ;
: RESET ( ^arena -- ) EMPTY CUT ;
: RELEASE ( ^arena -- )
   dup NULL-PTR = if drop exit then EMPTY false SETTLE RETHROW ;


\ Each spare a task has not taken is unmapped; the first rejection is rethrown
\ as E-MEM-UNMAP after the rest.
: TRIM ( -- )
   0 SPARE-N 0 do
      TAKE-SPARE dup 0<> if SPARE>ARENA UNMAP-FIRST FIRST-ERROR else drop then
   loop RETHROW ;


: WITH ( R [ R ^arena -- S ] -- S )
   {: body :}
   NEW {: a:^ :}
   a body [: WITH-BODY ;] [: a RELEASE ;] finally ;

;package
