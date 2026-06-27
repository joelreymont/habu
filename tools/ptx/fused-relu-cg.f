\ fused-relu-cg.f - one CHECKED kernel for relu(a*x+y): SCALE,ADD,RELU fused
\ register-resident (no intermediate global writes), v4. The checker proves the
\ fused effect; benchmark target is hand-fused Triton (parity at the memory
\ ceiling), never unfused Habu. Load after cg.f+cg-vec.f+tile.f+tile-v4.f.
256 %BLOCK
KERNEL: SAXPY ( span<space-global,f32,extent-n>  span<space-global,f32,extent-n>  uniform<f32> -- )  GRID: ceil-n-1024
   {: x y a :}
   x GRID-CTX-V4 {: g :}
   x g LOAD-V4  a SCALE-V4
   y g LOAD-V4  ADD-V4
   RELU-V4
   y g STORE-V4 ;
: EMIT-FUSED ( -- )
   CG-RESET  CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS
   1 SPAN-REG  2 SPAN-REG  1 UNIFORM-REG  SAXPY
   CG-RET CG-CLOSE ;
EMIT-FUSED
