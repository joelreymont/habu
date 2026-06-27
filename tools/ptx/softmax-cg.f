\ softmax-cg.f - emit the CHECKED SOFTMAX-ROWS kernel to PTX via the codegen.
\
\ The same numerically-stable softmax body that the ptx-stdlib gate CERTIFIES
\ (lib/ptx/collective-test.f), now run in EMIT mode: each row/collective op
\ (lib/ptx/collective.f) calls its EMIT-* helper (lib/ptx/cg-collective.f), so
\ the checked kernel produces its own PTX - block-reduction max/sum via shared
\ memory + bar.sync, identity-seeded inactive lanes. Load after lib/ptx/cg.f,
\ lib/ptx/cg-collective.f, lib/ptx/header.f, lib/ptx/collective.f. Emits to
\ stdout; ptxas -arch=sm_87 assembles it. block = SM-BLK (256) threads per row.

256 %BLOCK
KERNEL: SOFTMAX-ROWS ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: in out :}
   ROW            {: r :}
   in r ROW-SPAN  {: xs :}
   xs ROW-CTX     {: c :}
   xs c ROW-LOAD  {: x :}
   x BLOCK-MAX    {: mx :}
   x mx PTX:B- EXP.   {: e :}
   e BLOCK-SUM    {: s :}
   e s PTX:B/  out r ROW-SPAN c ROW-STORE ;

\ scaffold the entry, run the checked body in emit mode (in=%rd1 out=%rd2 k=%r1), close
: EMIT-SOFTMAX ( -- )
   CG-SM-RESET  CG-HEADER CG-SM-ENTRY CG-SM-OPEN CG-SM-PARAMS
   1 MATRIX-REG  2 MATRIX-REG  SOFTMAX-ROWS
   CG-SM-RET CG-SM-CLOSE ;

EMIT-SOFTMAX
