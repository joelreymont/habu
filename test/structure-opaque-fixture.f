\ structure-opaque-fixture.f - the OPAQUE family test/structure-opaque-e2e.f
\ loads: `box` is public, so every package can name OP:box, and its generated
\ BOX-MAKE / BOX-UNMAKE are OP's private words. WRAP and PEEK are the only
\ construction surface another package gets.
package OP
public
STRUCTURE box 0 OPAQUE FIELD x n ;STRUCTURE
: WRAP ( n -- box ) BOX-MAKE ;
: PEEK ( box -- n ) BOX-UNMAKE ;
;package
