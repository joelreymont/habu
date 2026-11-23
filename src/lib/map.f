split-str
\ map.f - package MAP's INIT, and maps made and grown in an arena. A target
\ with no OS generator (wasm) requires src/lib/map-core.f, the rest of MAP,
\ and makes its maps with INIT-KEYED.
\ STORAGE CLASS. CALLER-OWNED: the caller's cells; no module state.

require src/lib/map-core.f
require src/lib/entropy.f
require src/lib/arena.f

package MAP
public

\ Draws the SipHash-2-4 key into the header from the OS generator
\ (src/lib/entropy.f), so crafted keys cannot aim at one probe chain: chains are
\ expected short over the secret, not guaranteed. The key lives in the map's
\ cells, so a map INITed while building an image carries it into every run; a
\ map for untrusted keys is INITed at run time.
: INIT ( ^a count -- )
   {: m:^ cap :}
   m HASH-K0-OFF cells + BYTE-VIEW 2 cells ENTROPY:FILL
   m cap
   m HASH-K0-OFF HEADER-FIELD @
   m HASH-K1-OFF HEADER-FIELD @
   INIT-KEYED ;

: IN ( ^ARENA:arena count -- ^n )
   {: a:^ cap:count :}
   cap CELL-COUNT COUNT>N cells {: bytes:n :}
   a bytes ARENA:ALLOC drop CELL-VIEW {: m:^ :}
   m cap INIT
   m ;

\ Room for one more entry at mp: a null map is made, and a map that entry
\ would fill past half is doubled. Keys are not copied: they must live as long
\ as arena a.
: ROOM ( ^^n ^ARENA:arena -- )
   {: mp:^ a:^ :}
   mp @ {: old:^ :}
   old NULL-PTR = if a 8 >COUNT IN mp ! exit then
   old CAP@ COUNT>N {: cap:n :}
   old COUNT@ COUNT>N 1+ 2 * cap <= if exit then
   a cap 2 * >COUNT IN {: new:^ :}
   old [: -rot new -rot SET ;] EACH
   new mp ! ;

;package
