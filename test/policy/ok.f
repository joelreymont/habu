\ ok.f - a design inside its vocabulary: its own package, locals, if/else/then,
\ s" names, integer and real literals, a using block and top-level calls.
package PDESIGN
private
: PICK-BIG ( n -- n ) {: dv:n :} dv PDEP:BIG? if dv else PDEP:ZERO then ;
public
: MAIN ( -- ) PDEP:ANSWER PICK-BIG PDEP:SHOW ;  ( a comment )
: NAMED ( -- ) s" front" PDEP:NAME-LEN PDEP:SHOW ;
: SMALL ( -- ) 3 PICK-BIG PDEP:SHOW ;
: RATIO ( -- r ) 1.5 ;
;package
PDESIGN:MAIN
PDESIGN:NAMED
s" front" PDEP:NAME-LEN PDEP:SHOW
using PDEP
: VIA ( -- n ) ANSWER ;
VIA SHOW
;using
PDESIGN:SMALL
