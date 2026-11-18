split-str
\ map.f - package MAP's INIT. A target with no OS generator (wasm) requires
\ src/lib/map-core.f, the rest of MAP, and makes its maps with INIT-KEYED.
\ STORAGE CLASS. CALLER-OWNED: the caller's cells; no module state.

require src/lib/map-core.f
require src/lib/entropy.f

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

;package
