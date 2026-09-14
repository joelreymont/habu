\ Seeded DATA field words must retain the address provenance of fresh fields.
1 set-tier
require lib/test.f
require src/habu/layout.f

package AOT-SEEDED-ADDRESS-TEST
private

variable FRESH-FIELD
get-current constant FIELD-WL

: MARKED? ( n -- bool ) {: off:n :}
   data-base SNAP-RELOC:ADDRMAP-OFF + off 5 rshift + BYTE-VIEW c@
   off 2 rshift 7 and rshift 1 and 0<> ;

: FIELD ( ptr n -- ) {: rec:ptr :}
   rec XREF-FOUND? dup TTRUE 0= if exit then
   rec XREF-START dbase@ - {: off:n :}
   off DICT-SIZE >= off REGION < and dup TTRUE 0= if exit then
   off MARKED? TTRUE ;

: CHECK ( -- )
   \ ENV-DATA-PTR comes from the engine's captured prefix, before this load.
   s" ENV-DATA-PTR" 0 XREF-FIND-WL FIELD
   ENV-DATA-PTR @ data-base = TTRUE
   s" FRESH-FIELD" FIELD-WL XREF-FIND-WL FIELD ;

public
: RUN ( -- )
   T-RESET CHECK
   T-REPORT
   s" aot-seeded-address-sites: ok" type cr ;

;package
AOT-SEEDED-ADDRESS-TEST:RUN
