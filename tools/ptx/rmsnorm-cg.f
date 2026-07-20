\ rmsnorm-cg.f - emit ONE PTX module holding BOTH the checked RMSNORM_ROWS forward
\ and the checked closed-form RMSNORM_BWD_ROWS backward, so a consumer loads a
\ SINGLE cubin and pulls both function handles from it.
\
\ Both bodies are the CERTIFIED kernels (lib/ptx/collective-test.f): running them
\ in emit mode (each row/collective op calls its EMIT-* helper) produces their PTX
\ - block-reduction sum-of-squares via shared memory + bar.sync, then rsqrt + scale
\ (forward), and the maki/rmsnorm.f RMS-BWD closed form dx = (dy - x*coef)/r
\ (backward). A PTX MODULE is exactly ONE header then N `.visible .entry` kernels,
\ so CG-HEADER is emitted once by the forward and the backward appends its entry
\ with NO second header. Emits to stdout; ptxas assembles the whole stream for the
\ probed device arch (sm_87 Orin / sm_121a GB10). Load after lib/ptx/cg.f,
\ lib/ptx/header.f, lib/ptx/cg-collective.f, lib/ptx/tile.f, lib/ptx/collective.f.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/test.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/cg-collective.f
require lib/ptx/tile.f
require lib/ptx/collective.f

256 %BLOCK

KERNEL: RMSNORM-ROWS ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: in out :}
   ROW            {: r :}
   in r ROW-SPAN  {: xs :}
   xs ROW-CTX     {: c :}
   xs c ROW-LOAD  {: x :}
   x x *. BLOCK-SUM             {: ss :}
   ss UN PTX:U/ RMS-EPS+ USQRT {: rr :}
   x rr PTX:B/  out r ROW-SPAN c ROW-STORE ;

KERNEL: RMSNORM-ROWS-BWD ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: x dy dx :}
   ROW            {: r :}
   x  r ROW-SPAN  {: xs :}
   dy r ROW-SPAN  {: dys :}
   xs ROW-CTX     {: c :}
   xs  c ROW-LOAD {: xt :}
   dys c ROW-LOAD {: dyt :}
   xt xt *. BLOCK-SUM UN PTX:U/  {: u :}
   u RMS-EPS+                    {: r2 :}
   dyt xt *. BLOCK-SUM UN PTX:U/ {: sm :}
   sm r2 PTX:U/                  {: coef :}
   dyt  xt coef SCALE  -.  r2 USQRT PTX:B/
   dx r ROW-SPAN c ROW-STORE ;

: CG-RMS-ENTRY ( -- )
   s" .visible .entry RMSNORM_ROWS(.param .u64 p_in, .param .u64 p_out, .param .u32 p_k)" PTX-L ;

: CG-RMSBW-ENTRY ( -- )
   s" .visible .entry RMSNORM_BWD_ROWS(.param .u64 p_x, .param .u64 p_dy, .param .u64 p_out, .param .u32 p_k)" PTX-L ;

: CG-RMSBW-PARAMS ( -- )
   s" ld.param.u64 %rd1, [p_x];" PTX-L
   s" ld.param.u64 %rd2, [p_dy];" PTX-L
   s" ld.param.u64 %rd3, [p_out];" PTX-L
   s" ld.param.u32 %r1, [p_k];" PTX-L ;

\ entry 1: forward (in=%rd1 out=%rd2 k=%r1) WITH the module header
: EMIT-RMSNORM ( -- )
   CG-SM-RESET  CG-HEADER CG-RMS-ENTRY CG-SM-OPEN CG-SM-PARAMS
   1 MATRIX-REG  2 MATRIX-REG  RMSNORM-ROWS
   CG-SM-RET CG-SM-CLOSE ;

\ entry 2: backward (x=%rd1 dy=%rd2 out=%rd3 k=%r1) with NO second header
: EMIT-RMSNORM-BWD ( -- )
   CG-BW-RESET  CG-RMSBW-ENTRY CG-SM-OPEN CG-RMSBW-PARAMS
   1 MATRIX-REG  2 MATRIX-REG  3 MATRIX-REG  RMSNORM-ROWS-BWD
   CG-SM-RET CG-SM-CLOSE ;

EMIT-RMSNORM
EMIT-RMSNORM-BWD
