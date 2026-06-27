\ saxpy-wrong-cg.f - a SAXPY candidate that CERTIFIES but is SEMANTICALLY WRONG.
\
\ The body forgets to SCALE x by a, so it computes y' = x + y instead of a*x + y.
\ It is fully TYPE/STACK correct (a is just an unused local), so the checker
\ certifies it - exactly the class of error the checker CANNOT catch. Emitting and
\ running it on the Orin (output x+y, not a*x+y) is what a device-golden gate needs
\ to reject. Same load path as tools/ptx/saxpy-cg.f. Emits to stdout.

256 %BLOCK
KERNEL: SAXPY ( span<space-global,f32,extent-n>  span<space-global,f32,extent-n>  uniform<f32> -- )  GRID: ceil-n-256
   {: x y a :}
   x GRID-CTX {: g :}
   x g LOAD  y g LOAD  +.          \ BUG: no  a SCALE  -> computes x + y
   y g STORE ;

: EMIT-SAXPY ( -- )
   CG-RESET  CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS
   1 SPAN-REG  2 SPAN-REG  1 UNIFORM-REG  SAXPY
   CG-RET CG-CLOSE ;

EMIT-SAXPY
