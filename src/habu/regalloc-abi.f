\ regalloc-abi.f - the register allocator's DATA cells, package REGALLOC-ABI.
\ Build-only, as prof-abi.f is: runtime images need only layout.f.
\
\ src/habu/regalloc.f fills and reads these cells, src/habu/jit.f and habu2.f
\ reset the free masks, and src/habu/data-claims.f claims each one, so an
\ overlap with another DATA claim is refused when the engine is built. It is its
\ own file, not rows in src/habu/layout.f, because layout.f is baked into every
\ runtime image and a building host's `require` of it is a no-op: a cell moved
\ there is undefined to the host that builds the first engine carrying it.
package REGALLOC-ABI
public

$208  constant VRFREE-CELL      \ free-register bitmask, bit = pool index
$36B0 constant FRFREE-CELL      \ FLOAT pool free bits: bit i = d(8+i) free
32    constant VRTAB-BYTES      \ each table's bytes: VRITAB holds one per register x0..x31
$3600 constant VRTAB-OFF        \ idx -> register number   (after LOCNAMES)
VRTAB-OFF VRTAB-BYTES + constant VRITAB-OFF   \ register number -> idx ($FF = not pooled)

;package
