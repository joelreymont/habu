\ regalloc-abi.f - the register allocator's DATA cells, package REGALLOC-ABI.
\ Build-only, as prof-abi.f: runtime images need only layout.f. Not rows in
\ layout.f: it is baked into every image and a building host's `require` of it
\ is a no-op, so a cell moved there is undefined to the host that builds the
\ first engine carrying it. habu2.f resets the free masks, interpreter.f saves
\ and restores them, data-claims.f refuses an overlap at build.
package REGALLOC-ABI
public

$208  constant VRFREE-CELL      \ free-register bitmask, bit = pool index
$36B0 constant FRFREE-CELL      \ FLOAT pool free bits: bit i = d(8+i) free

;package
