\ An ordinary image of the same capture carries no field-loan authority.
require lib/test.f
require lib/c2-memory.f
require src/habu/xref.f

package C2-FIELD-LOAN-ORDINARY
private
: FIELD-ENTRY ( -- n )
   s" C2-MEM:WITH-FIELD" XREF-FIND dup XREF-FOUND? 0= if
      drop s" field-loan entry missing" 71 die then XREF-START ;
public
: RUN ( -- )
   T-RESET
   s" the ordinary image has no field-loan scope kind" T-LABEL
   FIELD-ENTRY scope-kind? 0 T=
   T-REPORT
   s" c2-field-loan-ordinary: ok" type cr ;
;package

C2-FIELD-LOAN-ORDINARY:RUN
