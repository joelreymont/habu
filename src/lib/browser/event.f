\ event.f - BROWSER-EVENT: a browser module's side of the turn protocol's events
\ (src/lib/browser/turn.js). A module built by tools/wasm-build.f answers hello
\ with EVENT's address; the host writes each event's seven cells there before
\ the run that reads it, and copies delivered bytes into pages it reserved
\ through the module's reserve export. HANDLE frees each delivery its turn does
\ not TAKE. STORAGE CLASS. MODULE-OWNED: EVENT, written by the host between runs.

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

: DELIVERS? ( n -- bool )
   {: kind:n :}
   kind 1 =  kind 3 = or  kind 4 = or  kind 8 = or
   kind 11 = or  kind 12 = or  kind 13 = or ;

public

: EVENT ( -- ptr u8 )  EVENT-BUF ;

: EVENT@ ( n -- n )
   8 * EVENT-BUF + >CELLS @ ;

: DELIVERY ( -- ptr u8 n )
   0 EVENT@ DELIVERS? if  1 EVENT@ >BYTES 2 EVENT@  else  0 >BYTES 0  then ;

\ HANDLE leaves this turn's delivery to the caller to FREE on every exit of the
\ turn, a throw after TAKE included; one never freed keeps its pages.
: TAKE ( -- ptr u8 n )
   true TAKEN !  DELIVERY ;

\ A refused unmap, of bytes no reserve handed out, is E-MEM-UNMAP. FREE writes
\ the allocator's free-list entry into the pages, so a record that must show
\ delivered bytes names a copy in module storage, or a taken delivery.
: FREE ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0= if exit then
   a u munmap 0<> if E-MEM-UNMAP throw then ;

\ A throw from the dispatch goes on after the free: catch, since a wasm32 link
\ has no finally.
: HANDLE ( [ -- ] -- )
   false TAKEN !
   catch {: code:n :}
   TAKEN @ 0= if DELIVERY FREE then
   code 0<> if code throw then ;

;package
