\ The complete C2 image authenticates the exact field-loan code entry.
require lib/test.f
require lib/c2-memory.f
require src/habu/xref.f

package C2-FIELD-LOAN-KIND
private
: FIELD-ENTRY ( -- n )
   s" C2-MEM:WITH-FIELD" XREF-FIND dup XREF-FOUND? 0= if
      drop s" field-loan entry missing" 71 die then XREF-START ;
public
: RUN ( -- )
   T-RESET
   s" the real field loan has scope kind 8" T-LABEL
   FIELD-ENTRY scope-kind? 8 T=
   T-REPORT
   s" c2-field-loan-kind: ok" type cr ;
;package

C2-FIELD-LOAN-KIND:RUN
