\ autograd-neg-test.f - the gradient extent contract is STATIC, fail-closed.
\
\ The ad-reverse capstone claim (habu-ad-softmax-rows): len(dx) = len(y) is
\ proven by the extent TOKEN, never re-asserted at a trusted boundary. Positive
\ controls certify the closed-form softmax backward with shared extents and the
\ MK-SPAN= minted gradient-span pair; the negatives prove a mismatched gradient
\ buffer is a CHECKER error - dx typed with extent-c2 rejects at ROW-STORE, and
\ a gradient span minted SEPARATELY (fresh extent not shared with the primal)
\ rejects at STORE.

require lib/ptx/neg-test-lib.f

: AGN-CERTIFIES ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n label:ptr labelu:n :}
   src srcu PTXN-CHECK {: diag:n verdict:n rc:n :}
   label labelu T-LABEL
   rc 0 T=
   label labelu T-LABEL
   verdict -1 T= ;

: AGN-BWD-OK$ ( -- ptr u8 n )
   s" AGN-BWD-OK ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- ) {: y dy dx :} ROW {: r :} y r ROW-SPAN {: ys :} dy r ROW-SPAN {: dys :} ys ROW-CTX {: c :} ys c ROW-LOAD {: yt :} dys c ROW-LOAD {: dyt :} dyt yt *. BLOCK-SUM {: s :} dyt s PTX:B- yt *. dx r ROW-SPAN c ROW-STORE" ;

: AGN-BWD-BAD$ ( -- ptr u8 n )
   s" AGN-BWD-BAD ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c2> -- ) {: y dy dx :} ROW {: r :} y r ROW-SPAN {: ys :} dy r ROW-SPAN {: dys :} ys ROW-CTX {: c :} ys c ROW-LOAD {: yt :} dys c ROW-LOAD {: dyt :} dyt yt *. BLOCK-SUM {: s :} dyt s PTX:B- yt *. dx r ROW-SPAN c ROW-STORE" ;

: AGN-SPAN-OK$ ( -- ptr u8 n )
   s" AGN-SPAN-OK ( ptr<space-global,f32> ptr<space-global,f32> u32 -- ) MK-SPAN= {: ys dxs :} ys GRID-CTX {: g :} ys g LOAD dxs g STORE" ;

: AGN-SPAN-BAD$ ( -- ptr u8 n )
   s" AGN-SPAN-BAD ( ptr<space-global,f32> u32 ptr<space-global,f32> u32 -- ) MK-SPAN {: dxs :} MK-SPAN {: ys :} ys GRID-CTX {: g :} ys g LOAD dxs g STORE" ;

: AGN-MAIN ( -- )
   T-RESET
   256 %BLOCK
   AGN-BWD-OK$ s" closed-form backward, shared extents" AGN-CERTIFIES
   AGN-BWD-BAD$ s" extent-c2" s" gradient extent mismatch" PTXN-REJECTS
   AGN-SPAN-OK$ s" MK-SPAN= minted gradient pair" AGN-CERTIFIES
   AGN-SPAN-BAD$ s" fresh-extent" s" separately minted gradient span" PTXN-REJECTS
   s" NEG: mismatched gradient extents are checker errors (token-proven lengths)" type cr
   T-REPORT ;

AGN-MAIN
