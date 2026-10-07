\ Seeded DATA field words must retain the address provenance of fresh fields.
1 set-tier
require lib/test.f
require src/habu/layout.f
require src/habu/sites.f

package AOT-SEEDED-ADDRESS-TEST
private

variable FRESH-FIELD
get-current constant FIELD-WL

variable ADDR-SITES

: COUNT-ADDR ( n n -- )
   SNAP-RELOC:SITE-ADDR = if 1 ADDR-SITES +! then drop ;

\ The address chain need not be the word's first instruction - what the field
\ word emits ahead of it is the emitter's business - so the site is looked for
\ anywhere in the word's own code.
: SITE-MARKED? ( n n -- bool )
   0 ADDR-SITES !
   [: COUNT-ADDR ;] SITES:EACH-IN-SPAN
   ADDR-SITES @ 0<> ;

: CHECK-FIELD ( ptr n -- ) {: rec:ptr :}
   rec XREF-FOUND? dup TTRUE 0= if exit then
   rec XREF-START dbase@ - {: off:n :}
   off DICT-SIZE >= off REGION < and dup TTRUE 0= if exit then
   off rec XREF-CODE-BYTES SITE-MARKED? TTRUE ;

: CHECK ( -- )
   \ ENV-DATA-PTR comes from the engine's captured prefix, before this load.
   s" ENV-DATA-PTR" 0 XREF-FIND-WL CHECK-FIELD
   ENV-DATA-PTR @ data-base = TTRUE
   s" FRESH-FIELD" FIELD-WL XREF-FIND-WL CHECK-FIELD ;

public
: RUN ( -- )
   T-RESET CHECK
   T-REPORT
   s" aot-seeded-address-sites: ok" type cr ;

;package
AOT-SEEDED-ADDRESS-TEST:RUN
