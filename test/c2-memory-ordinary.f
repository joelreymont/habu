\ The same source in the ordinary image has no C2 owner or loan authority.
require src/lib/test.f
require src/lib/test/subject.f
require src/lib/c2-memory.f
require src/habu/xref.f

package C2-MEMORY-ALIAS
public
EXPORT C2-MEM:WITH-MUT
;package

package C2-MEMORY-ORDINARY
private

: ENTRY ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if
      drop s" c2-memory-ordinary: missing entry" 71 die
   then XREF-START ;

public

: RUN ( -- )
   T-RESET
   s" source loading grants no unique owner root" T-LABEL
   s" C2-MEM:WITH-MUT" ENTRY scope-kind? 0 T=
   s" source loading grants no shared or exclusive loan" T-LABEL
   s" C2-MEM:WITH-READ" ENTRY scope-kind? 0 T=
   s" C2-MEM:WITH-MUT-LOAN" ENTRY scope-kind? 0 T=
   s" source export cannot grant an owner root" T-LABEL
   s" C2-MEMORY-ALIAS:WITH-MUT" ENTRY scope-kind? 0 T=
   s" raw root call cannot accept a unique-view callback" T-LABEL
   s" : C2-ORD-DIRECT ( -- n ) 1 MEM:BYTES-ALLOC-LEN [: 0 C2-MEM:MUT-BYTE@ ;] C2-MEM:WITH-MUT ;" 70 SUBJECT:EXITED? TTRUE
   s" a ticked raw root cannot introduce unique authority" T-LABEL
   s" : C2-ORD-TICK ( -- n ) 1 MEM:BYTES-ALLOC-LEN [: 0 C2-MEM:MUT-BYTE@ ;] ['] C2-MEM:WITH-MUT execute ;" 70 SUBJECT:EXITED? TTRUE
   s" an exported raw root cannot introduce unique authority" T-LABEL
   s" : C2-ORD-EXPORT ( -- n ) 1 MEM:BYTES-ALLOC-LEN [: 0 C2-MEM:MUT-BYTE@ ;] C2-MEMORY-ALIAS:WITH-MUT ;" 70 SUBJECT:EXITED? TTRUE
   s" raw shared-loan source cannot acquire a scoped child" T-LABEL
   s" : C2-ORD-READ ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) [: 0 C2-MEM:BYTE@ swap drop ;] C2-MEM:WITH-READ ;" 70 SUBJECT:EXITED? TTRUE
   s" raw exclusive-loan source cannot acquire a scoped child" T-LABEL
   s" : C2-ORD-MUT-LOAN ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) [: 0 C2-MEM:MUT-BYTE@ swap drop ;] C2-MEM:WITH-MUT-LOAN ;" 70 SUBJECT:EXITED? TTRUE
   T-REPORT
   s" c2-memory-ordinary: ok" type cr ;

;package

C2-MEMORY-ORDINARY:RUN
