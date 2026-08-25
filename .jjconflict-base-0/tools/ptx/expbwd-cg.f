\ expbwd-cg.f - emit the CHECKED EXP backward kernel to PTX via the codegen.
\
\ The EXP. adjoint AD-REVERSE derives is `LOAD SAVED-Y *.` (dx = dz * y, y = saved output).
\ This is its RESOLVED form (habu-ad-thread-saved): SAVED-Y becomes a real load of the saved
\ value threaded in as the second input span - dx = dz * savedy. A normal checked kernel
\ (just the *. tile op). tools/ptx/gradcheck.f runs it (dz=1, savedy=exp(x)) and confirms the
\ BACKWARD KERNEL's output matches the forward's central difference - the derived backward,
\ device-proven correct. The unused uniform `a` keeps the shared param layout. Load after
\ lib/ptx/cg.f and lib/ptx/tile.f; emits to stdout.

256 %BLOCK

KERNEL: EXPBWD ( span<space-global,f32,extent-n>  span<space-global,f32,extent-n>  uniform<f32> -- )  GRID: ceil-n-256
   {: dz savedy a :}
   dz GRID-CTX {: g :}
   dz g LOAD  savedy g LOAD  *.        \ dx = dz * savedy   (SAVED-Y resolved to a real load)
   dz g STORE ;

: EMIT-EXPBWD ( -- )
   CG-RESET  CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS
   1 SPAN-REG  2 SPAN-REG  1 UNIFORM-REG  EXPBWD
   CG-RET CG-CLOSE ;

EMIT-EXPBWD
