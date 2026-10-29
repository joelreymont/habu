\ process-chunks.f - row bytes that stay put while a list of rows grows. Chunks
\ never move until freed; chunks past the one being filled hold no live row, so
\ a rewind reuses them. ROOM and FREE reach the chunk table through a quotation:
\ a static list keeps it in a DYNAMIC-BUFFER, an owned one beside its control.
\ OWNED-CELLS adds the table's address and capacity; zeroed cells are empty.
\ STORAGE CLASS. CALLER-OWNED: the caller frees every chunk and table.

s" src/lib/errors.f" required
s" src/lib/memory.f" required

package PROC-CHUNKS
private

0 cells constant HELD-OFF
1 cells constant CUR-OFF
2 cells constant USED-OFF
4 cells constant CAP-OFF
$1000 constant CHUNK-BYTES

: ROOM? ( n ptr n [ n ptr n -- ptr SPAN:span<u8> ] -- bool )
   {: need state chunks :}
   state CUR-OFF + @ state HELD-OFF + @ >= if false exit then
   state CUR-OFF + @ state chunks execute @ SPAN:LEN state USED-OFF + @ - need >= ;


\ A fresh chunk takes the current slot; the one it displaces, too small for
\ this row, moves to the end.
: GROW ( n ptr n [ n ptr n -- ptr SPAN:span<u8> ] [ n ptr n -- ] -- )
   {: need state chunks reserve :}
   state HELD-OFF + @ {: held:n :}
   state CUR-OFF + @ {: cur:n :}
   held 1+ state reserve execute
   cur state chunks execute @ held state chunks execute !
   need CHUNK-BYTES max MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN cur state chunks execute !
   held 1+ state HELD-OFF + ! ;

public

3 constant CONTROL-CELLS

: ROOM ( n ptr n [ n ptr n -- ptr SPAN:span<u8> ] [ n ptr n -- ] -- ptr u8 )
   {: need state chunks reserve :}
   need state chunks ROOM? 0= if
      state CUR-OFF + @ state HELD-OFF + @ < if 1 state CUR-OFF + +! then
      0 state USED-OFF + !
      need state chunks ROOM? 0= if need state chunks reserve GROW then
   then
   state CUR-OFF + @ state chunks execute @ state USED-OFF + @ SPAN:AT
   need state USED-OFF + +! ;


: REWIND ( ptr n -- )
   {: state :}
   0 state CUR-OFF + !
   0 state USED-OFF + ! ;


\ Unmaps every chunk; the caller then releases its chunk buffer.
: FREE ( ptr n [ n ptr n -- ptr SPAN:span<u8> ] -- )
   {: state chunks :}
   state HELD-OFF + @ 0 ?do i state chunks execute @ MEM:FREE-SPAN loop
   0 state HELD-OFF + !
   state REWIND ;

private

2 constant SPAN-CELLS

CAST: >SPANS ( ptr n -- ptr SPAN:span<u8> )

: TABLE ( ptr n -- ptr ptr a ) 3 ptr-field ;

: OWNED-CHUNK ( n ptr n -- ptr SPAN:span<u8> )
   TABLE @ >SPANS swap SPAN-CELLS * record-at ;


: RELEASE-TABLE ( ptr n -- )
   {: state :}
   state CAP-OFF + @ {: cap:n :}
   cap 0= if exit then
   state TABLE @ cap SPAN-CELLS * MEM:CELLS-ALLOC-COUNT MEM:RELEASE-CELLS
   NULL-PTR state TABLE !
   0 state CAP-OFF + ! ;


: OWNED-RESERVE ( n ptr n -- )
   {: need state :}
   need state CAP-OFF + @ <= if exit then
   need state CAP-OFF + @ 2 * max {: cap:n :}
   cap SPAN-CELLS * MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS {: fresh:ptr :}
   state HELD-OFF + @ 0 ?do
      i state OWNED-CHUNK @ fresh >SPANS i SPAN-CELLS * record-at !
   loop
   state RELEASE-TABLE
   fresh state TABLE !
   cap state CAP-OFF + ! ;

public

5 constant OWNED-CELLS

: OWNED-ROOM ( n ptr n -- ptr u8 )
   [: OWNED-CHUNK ;] [: OWNED-RESERVE ;] ROOM ;


: OWNED-FREE ( ptr n -- )
   {: state :}
   state [: OWNED-CHUNK ;] FREE
   state RELEASE-TABLE ;

;package
