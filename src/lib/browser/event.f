\ event.f - BROWSER-EVENT: a browser module's side of the turn protocol's
\ events (src/lib/browser/turn.js). A module built by tools/wasm-build.f answers
\ hello with EVENT's address; the host writes each event's seven cells there
\ before the run that reads it.
\
\ The module owns every delivered byte. Bytes, answered, typed, key, load,
\ focus and highlight events name, in fields a and b, bytes the host copied into
\ pages it reserved through the module's reserve export, at least one page each.
\ HANDLE frees each non-empty delivery its turn does not TAKE; the module frees
\ a taken one with FREE in a later run, and one it never frees keeps its pages
\ for the instance. FREE writes the allocator's free-list entry into the pages
\ it gives back, so a record that must show delivered bytes names a copy in the
\ module's own storage, or a taken delivery.
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

\ This turn's delivery is the module's: HANDLE clears it before the dispatch and
\ frees nothing while it is set.
TYPED-VARIABLE TAKEN bool

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

\ This turn's delivery, as DELIVERY gives it, which HANDLE then leaves to the
\ caller to FREE: on every exit of the turn, a throw after TAKE included. The
\ event block is unchanged.
: TAKE ( -- ptr u8 n )
   true TAKEN !  DELIVERY ;

\ Gives delivered bytes back: munmap of their pages, nothing at length 0. A
\ refused unmap, of bytes no reserve handed out, is E-MEM-UNMAP.
: FREE ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0= if exit then
   a u munmap 0<> if E-MEM-UNMAP throw then ;

\ Runs a turn's dispatch, then frees the event's delivery on every exit unless
\ the dispatch took it; a throw from the dispatch goes on after the free. catch,
\ since a wasm32 link has no finally.
: HANDLE ( [ -- ] -- )
   false TAKEN !
   catch {: code:n :}
   TAKEN @ 0= if DELIVERY FREE then
   code 0<> if code throw then ;

;package
