\ A checked owner appends and publishes bytes through the real MEM load path.
require lib/test.f
require lib/c2-owner.f
require lib/memory.f

package C2-OWNER-PRODUCER-PROGRAM
private

: FIRST ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> read-view<p,p,u8> )
   65488 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC
   0 65 C2-MEM:MUT-BYTE!
   C2-MEM:PUBLISH ;

: SECOND ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> read-view<p,p,u8> )
   16 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC
   0 66 C2-MEM:MUT-BYTE!
   C2-MEM:PUBLISH ;

: TWO ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> mut-view<p,p,fresh-region-b,u8> mut-view<p,p,fresh-region-c,u8> )
   16 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC swap
   16 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC rot swap ;

: TWO-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- bool mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND TWO
   0 67 C2-MEM:MUT-BYTE! C2-MEM:PUBLISH swap
   0 68 C2-MEM:MUT-BYTE! C2-MEM:PUBLISH
   0 C2-MEM:BYTE@ swap 68 = swap drop swap
   0 C2-MEM:BYTE@ swap 67 = swap drop
   and swap C2-MEM:UNBIND ;

: TWO-ROOT ( mut-view<p,p,a,u8> -- bool mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: TWO-INIT ;] C2-MEM:WITH-INIT ;

: TWO-RESULT ( -- bool )
   C2-MEM:OWNER-SIZE [: TWO-ROOT ;] C2-MEM:WITH-MUT ;

: ODD-ALLOC ( C2-MEM:owner<p,i,a> n -- C2-MEM:owner<p,i,a> )
   {: bytes:n :}
   bytes MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC
   bytes 1- C2-MEM:MUT-BYTE@ swap 0 T=
   bytes 1- 99 C2-MEM:MUT-BYTE!
   bytes 1- C2-MEM:MUT-BYTE@ swap 99 T=
   C2-MEM:PUBLISH drop ;

: ODD-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND
   1 ODD-ALLOC 7 ODD-ALLOC 9 ODD-ALLOC 17 ODD-ALLOC
   17 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC
   C2-MEM:SEED-OWNER [: ;] C2-MEM:WITH-INIT
   C2-MEM:PUBLISH drop
   C2-MEM:UNBIND ;

: ODD-ROOT ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: ODD-INIT ;] C2-MEM:WITH-INIT ;

: INNER-INIT ( C2-MEM:owner<p,i,a> mut-view<q,j,b,init<j,C2-MEM:owner-state>> -- C2-MEM:owner<p,i,a> read-view<p,p,u8> mut-view<q,j,b,init<j,C2-MEM:owner-state>> )
   C2-MEM:BIND swap FIRST rot C2-MEM:UNBIND ;

: INNER-ROOT ( C2-MEM:owner<p,i,a> mut-view<q,q,b,u8> -- C2-MEM:owner<p,i,a> read-view<p,p,u8> mut-view<q,q,b,u8> )
   C2-MEM:SEED-OWNER [: INNER-INIT ;] C2-MEM:WITH-INIT ;

: INNER-OWNER ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> read-view<p,p,u8> )
   C2-MEM:OWNER-SIZE [: INNER-ROOT ;] C2-MEM:WITH-MUT ;

: OUTER-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- bool mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND
   FIRST swap
   INNER-OWNER
   0 C2-MEM:BYTE@ swap 65 = swap drop >r
   SECOND 0 C2-MEM:BYTE@ swap 66 = swap drop
   r> and >r
   swap
   0 C2-MEM:BYTE@ swap 65 = swap drop
   r> and swap C2-MEM:UNBIND ;

: OUTER-ROOT ( mut-view<p,p,a,u8> -- bool mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: OUTER-INIT ;] C2-MEM:WITH-INIT ;

public

: RUN ( -- )
   T-RESET
   s" an outer append outlives the inner owner and a second append" T-LABEL
   C2-MEM:OWNER-SIZE [: OUTER-ROOT ;] C2-MEM:WITH-MUT TTRUE
   s" two fresh regions remain distinct while both allocations are live" T-LABEL
   TWO-RESULT TTRUE
   s" odd appends are zeroed and a later append admits a cell record" T-LABEL
   C2-MEM:OWNER-SIZE [: ODD-ROOT ;] C2-MEM:WITH-MUT
   T-REPORT
   s" c2-owner-producer-program: ok" type cr ;

;package

C2-OWNER-PRODUCER-PROGRAM:RUN
