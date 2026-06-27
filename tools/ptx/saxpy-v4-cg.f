\ saxpy-v4-cg.f - emit the CHECKED v4 (vectorized) SAXPY kernel to PTX.
\
\ Same checked SAXPY body as saxpy-cg.f, but composed from the v4 tile ops
\ (tile-v4.f): each thread owns 4 consecutive elements and the body lowers to
\ ld.global.v4.f32 / st.global.v4.f32 (cg-vec.f). Entry name stays SAXPY so the
\ existing launchers/graders work. PRECONDITION: launch with grid=ceil(n/(BLOCK*4))
\ and n%4==0. Load after lib/ptx/cg.f + cg-vec.f + lib/ptx/tile.f + tile-v4.f.

256 %BLOCK
KERNEL: SAXPY ( span<space-global,f32,extent-n>  span<space-global,f32,extent-n>  uniform<f32> -- )  GRID: ceil-n-1024
   {: x y a :}
   x GRID-CTX-V4 {: g :}
   x g LOAD-V4  a SCALE-V4
   y g LOAD-V4  ADD-V4
   y g STORE-V4 ;

: EMIT-SAXPY-V4 ( -- )
   CG-RESET  CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS
   1 SPAN-REG  2 SPAN-REG  1 UNIFORM-REG  SAXPY
   CG-RET CG-CLOSE ;

EMIT-SAXPY-V4
