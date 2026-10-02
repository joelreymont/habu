\ Higher-rank unique effects compiled into the saved C2 image.
require lib/memory.f
require lib/c2-memory.f

package C2-MEMORY-WRAPPER
public

: TWICE
   ( n forall<p,forall-region<a,[ n mut-view<p,p,a,u8> -- n mut-view<p,p,a,u8> | U -- U ]>> | U -- n | U )
   {: cb :}
   1 MEM:BYTES-ALLOC-LEN cb C2-MEM:WITH-MUT
   1 MEM:BYTES-ALLOC-LEN cb C2-MEM:WITH-MUT ;

: CHANGE
   ( n forall<p,forall-region<a,[ n mut-view<p,p,a,u8> -- bool mut-view<p,p,a,u8> | U -- U ]>> | U -- bool | U )
   {: cb :} 1 MEM:BYTES-ALLOC-LEN cb C2-MEM:WITH-MUT ;

: ONCE-MUT-LOAN
   ( R mut-view<p,q,a,u8> forall<l inside q,[ R mut-view<p,l,a,u8> -- S mut-view<p,l,a,u8> | U -- U ]> | U -- S mut-view<p,q,a,u8> | U )
   C2-MEM:WITH-MUT-LOAN ;

: ONCE-READ
   ( R mut-view<p,q,a,u8> forall<l inside q,[ R read-view<p,l,u8> -- S read-view<p,l,u8> | U -- U ]> | U -- S mut-view<p,q,a,u8> | U )
   C2-MEM:WITH-READ ;

;package
