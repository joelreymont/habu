\ ptx-autograd-test.f - checked verified-gradient kernel (the AD thesis).
\
\ SOFTMAX-ROWS-BWD is the closed-form softmax backward dx = y (dy - sum(dy y)),
\ derived in docs/autograd.md by reversing SOFTMAX-ROWS. The point: the backward
\ is an ordinary kernel, CHECKED by the same parametric type system as the forward
\ - so a mask / extent / address-space mistake in a GRADIENT is a compile error.
\ y, dy, dx share extent-r/extent-c by token, so one row context serves all three
\ spans; the mask threads from ROW-LOAD through *./BLOCK-SUM/PTX:B-/*. to ROW-STORE.
\
\ NB this is TYPE-verified, not NUMERICALLY verified: the checker proves the
\ gradient's types, not that it is the correct derivative. Numeric correctness is
\ the device finite-difference gradcheck (dot habu-ptx-ad-device), a separate gate.

T-RESET

256 %BLOCK

KERNEL: SOFTMAX-ROWS-BWD ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: y dy dx :}
   ROW            {: r :}
   y  r ROW-SPAN  {: ys :}
   dy r ROW-SPAN  {: dys :}
   ys ROW-CTX     {: c :}
   ys  c ROW-LOAD {: yt :}
   dys c ROW-LOAD {: dyt :}
   dyt yt *. BLOCK-SUM {: s :}
   dyt s PTX:B-  yt *.
   dx r ROW-SPAN c ROW-STORE ;

\ Exercises BROADCAST: the reduce-then-fill (BLOCK-SUM then BROADCAST) is exactly
\ the adjoint shape reverse-mode AD substitutes for a reduce's pullback.
KERNEL: SUM-BROADCAST ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: in out :}
   ROW           {: r :}
   in r ROW-SPAN {: xs :}
   xs ROW-CTX    {: c :}
   xs c ROW-LOAD BLOCK-SUM BROADCAST
   out r ROW-SPAN c ROW-STORE ;

\ Clean load past this point is the positive proof: the verified gradient certified.

T-REPORT
