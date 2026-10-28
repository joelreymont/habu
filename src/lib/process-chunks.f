\ process-chunks.f - row bytes that stay put while a list of rows grows.
\
\ Row bytes live in chunks that never move until they are freed, so a row's
\ address stays valid while later rows are added. A list's control is
\ CONTROL-CELLS cells: chunks held, the chunk being filled, and its bytes used.
\ Chunks past the one being filled hold no live row, so a rewind reuses them.
\ ROOM and FREE reach the chunk table through quotations that take the index
\ and the control, so a static list keeps its table in a DYNAMIC-BUFFER
\ (`[: drop CHUNKS ;]`) and an owned one keeps it beside its control.
\
\ OWNED LISTS. OWNED-CELLS cells of control hold the list's own chunk table as
\ well: the three control cells, then the table's address and its capacity in
\ chunks. Zeroed cells are an empty list; OWNED-ROOM grows the table by
\ allocate, copy, release, and OWNED-FREE unmaps every chunk and the table and
\ leaves the cells empty again.
\
\ STORAGE CLASS. CALLER-OWNED: the control cells and every chunk and table they
\ name belong to the caller, who frees them.

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

\ Answers need bytes that stay put until FREE.
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

\ The table is mapped as cells; this names them as the spans they hold.
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


\ The new table holds the held chunks; the old one is released after the copy.
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

\ Answers need bytes from an owned list; they stay put until OWNED-FREE.
: OWNED-ROOM ( n ptr n -- ptr u8 )
   [: OWNED-CHUNK ;] [: OWNED-RESERVE ;] ROOM ;


: OWNED-FREE ( ptr n -- )
   {: state :}
   state [: OWNED-CHUNK ;] FREE
   state RELEASE-TABLE ;

;package
