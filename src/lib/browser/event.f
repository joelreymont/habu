\ event.f - BROWSER-EVENT: a browser module's side of the turn protocol's
\ events (src/lib/browser/turn.js). A module built by tools/wasm-build.f answers
\ hello with EVENT's address; the host writes each event's seven cells there
\ before the run that reads it.
\
\ The module owns every delivered byte. Bytes, answered, typed, key, load,
\ focus and highlight events name, in fields a and b, bytes the host copied into
\ pages it reserved through the module's reserve export, at least one page each.
\ A module frees each non-empty delivery it does not keep, in its run or later;
\ a delivery it never frees keeps its pages for the instance. FREE writes the
\ allocator's free-list entry into the pages it gives back, so a record that
\ must show delivered bytes names a copy in the module's own storage.
\
\ STORAGE CLASS. MODULE-OWNED: EVENT, written by the host between runs.

require src/lib/errors.f

package BROWSER-EVENT
private

CAST: >BYTES ( n -- ptr u8 )
CAST: >CELLS ( ptr u8 -- ptr n )

\ Seven cells: kind, a, b, c, d, buttons, mods; zero, which is start, until the
\ host writes one.
56 BUFFER: EVENT-BUF

\ Kinds that deliver bytes: bytes 1, answered 3, typed 4, key 8, load 11,
\ focus 12 and highlight 13.
: DELIVERS? ( n -- bool )
   {: kind:n :}
   kind 1 =  kind 3 = or  kind 4 = or  kind 8 = or
   kind 11 = or  kind 12 = or  kind 13 = or ;

public

\ The event block.
: EVENT ( -- ptr u8 )  EVENT-BUF ;

\ Field n of the event, 0 its kind.
: EVENT@ ( n -- n )
   8 * EVENT-BUF + >CELLS @ ;

\ The event's delivered bytes; none for a kind that delivers none.
: DELIVERY ( -- ptr u8 n )
   0 EVENT@ DELIVERS? if  1 EVENT@ >BYTES 2 EVENT@  else  0 >BYTES 0  then ;

\ Gives delivered bytes back: munmap of their pages, nothing at length 0. A
\ refused unmap, of bytes no reserve handed out, is E-MEM-UNMAP.
: FREE ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0= if exit then
   a u munmap 0<> if E-MEM-UNMAP throw then ;

\ Runs a turn's dispatch, then frees the event's delivery on every exit; a
\ throw from the dispatch goes on after the free. catch, since a wasm32 link
\ has no finally.
: HANDLE ( [ -- ] -- )
   catch {: code:n :}
   DELIVERY FREE  code 0<> if code throw then ;

;package
