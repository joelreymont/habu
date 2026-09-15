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

\ The address chain is not the word's first instruction: the engine's stack
\ guard for the push comes ahead of it (src/habu/habu2.f C-DATA-ADDR), so the
\ site is looked for anywhere in the word's own code.
: SITE-MARKED? ( n n -- bool ) {: off:n len:n :}
   off begin dup off len + < while
      dup MARKED? if drop true exit then
      CODE-SPAN:INSN-BYTES +
   repeat drop false ;

: FIELD ( ptr n -- ) {: rec:ptr :}
   rec XREF-FOUND? dup TTRUE 0= if exit then
   rec XREF-START dbase@ - {: off:n :}
   off DICT-SIZE >= off REGION < and dup TTRUE 0= if exit then
   off rec XREF-CODE-BYTES SITE-MARKED? TTRUE ;

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
