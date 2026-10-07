\ Fixed DATA CODE cells shared by publication and code reclamation. This
\ module can load against a baked host whose layout.f predates these offsets.
package NATIVE-OBS-CELLS
public
\ $2CE8 is reserved for the literal store's DATA-FLOOR-CELL. The callback
\ cells follow it, after CODE-END-CELL and JIT-QUOT:END;
\ src/habu/data-claims.f proves each cell disjoint at native build time.
$2CF8 constant PUBLISHED
$2D00 constant INVALIDATE
\ ( first end -- ): retire implementation facts overlapping changed code.
$2D10 constant HOST-INVALIDATE
;package
