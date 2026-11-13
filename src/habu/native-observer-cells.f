\ A fixed DATA CODE cell of code publication. This module can load against a
\ baked host whose layout.f predates its offset.
package NATIVE-OBS-CELLS
public
\ $2CE8 is reserved for the literal store's DATA-FLOOR-CELL. The callback
\ cell follows it, after CODE-END-CELL;
\ src/habu/data-claims.f proves each cell disjoint at native build time.
$2CF8 constant PUBLISHED
;package
