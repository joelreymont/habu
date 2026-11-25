require test/fixtures/file-objects/order-ha.f
require test/fixtures/file-objects/order-hb.f

package FILE-OBJECTS-ORDER-HC
public : GETC ( -- n ) FILE-OBJECTS-ORDER-HA:VA @ FILE-OBJECTS-ORDER-HB:VB @ + ;
;package
