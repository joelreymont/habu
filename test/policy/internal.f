\ internal.f - an internal word of an admitted package shadows a foreign one.
\ Without the visibility clause tier 1 would bind PFOREIGN:STEP and print 2.
package PDEP
public
using PFOREIGN
: INNER ( -- n ) STEP ;
;using
;package
PDEP:INNER PDEP:SHOW
