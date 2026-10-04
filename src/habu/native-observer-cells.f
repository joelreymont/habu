\ Fixed DATA CODE cells shared by the compiler and code reclamation. This
\ module can load against a baked host whose layout.f predates these offsets.
package NATIVE-OBS-CELLS
public
\ Census: $2CE8..$3000 is unused after CODE-END-CELL and JIT-QUOT:END;
\ src/habu/data-claims.f proves each cell disjoint at native build time.
$2CE8 constant OBSERVE
$2CF0 constant PUBLISHED
$2CF8 constant INVALIDATE
;package
