\ Fixed DATA CODE cells shared by the compiler and code reclamation. This
\ module can load against a baked host whose layout.f predates these offsets.
package NATIVE-OBS-CELLS
public
\ $2CE8 is reserved for the literal store's DATA-FLOOR-CELL. The callback
\ run $2CF0..$2D08 follows it, after CODE-END-CELL and JIT-QUOT:END;
\ src/habu/data-claims.f proves each cell disjoint at native build time.
$2CF0 constant OBSERVE
$2CF8 constant PUBLISHED
$2D00 constant INVALIDATE
\ ( first end -- ): retire implementation facts overlapping changed code.
$2D10 constant HOST-INVALIDATE
;package
