\ rope-cg.f - emit ONE PTX module holding BOTH the checked ROPE_ROWS forward and
\ the checked ROPE_BWD_ROWS backward (its VJP), so a consumer loads a SINGLE cubin
\ and pulls both handles from it.
\
\ Both bodies are the CERTIFIED kernels (lib/ptx/collective-test.f): running them
\ in emit mode produces their PTX - a within-warp shfl.bfly pair rotation, no
\ block reduction. Forward is the maki/rope.f ROPE-PAIR per lane; backward is the
\ rotation by the negative angle (ROPE-BWD). ONE header (forward), then the
\ backward appends its entry with NO second header. Emits to stdout; ptxas
\ assembles for the probed device arch (sm_87 Orin / sm_121a GB10). Load after
\ lib/ptx/cg.f, lib/ptx/header.f, lib/ptx/cg-collective.f, lib/ptx/tile.f,
\ lib/ptx/collective.f.

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

KERNEL: ROPE-ROWS ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: x cs sn out :}
   ROW              {: r :}
   x  r ROW-SPAN    {: xs :}
   cs r ROW-SPAN    {: cspan :}
   sn r ROW-SPAN    {: sspan :}
   xs ROW-CTX       {: c :}
   xs    c ROW-LOAD {: xt :}
   cspan c ROW-LOAD {: ctile :}
   sspan c ROW-LOAD {: stile :}
   xt ctile stile ROPE-ROT  out r ROW-SPAN c ROW-STORE ;

KERNEL: ROPE-ROWS-BWD ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: dy cs sn dx :}
   ROW              {: r :}
   dy r ROW-SPAN    {: dys :}
   cs r ROW-SPAN    {: cspan :}
   sn r ROW-SPAN    {: sspan :}
   dys ROW-CTX      {: c :}
   dys   c ROW-LOAD {: dyt :}
   cspan c ROW-LOAD {: ctile :}
   sspan c ROW-LOAD {: stile :}
   dyt ctile stile ROPE-ROT-BWD  dx r ROW-SPAN c ROW-STORE ;

\ x/dy=%rd1 cos=%rd2 sin=%rd3 out=%rd4 k=%r1 -> register seeds start past %rd4.
: CG-ROPE-RESET ( -- )  1 CG-NF !  5 CG-NRD !  2 CG-NR !  1 CG-NP !  0 CG-NL ! ;

: CG-ROPE-ENTRY ( -- )
   s" .visible .entry ROPE_ROWS(.param .u64 p_x, .param .u64 p_cos, .param .u64 p_sin, .param .u64 p_out, .param .u32 p_k)" PTX-L ;

: CG-ROPEBW-ENTRY ( -- )
   s" .visible .entry ROPE_BWD_ROWS(.param .u64 p_dy, .param .u64 p_cos, .param .u64 p_sin, .param .u64 p_out, .param .u32 p_k)" PTX-L ;

: CG-ROPE-PARAMS ( -- )
   s" ld.param.u64 %rd1, [p_x];" PTX-L
   s" ld.param.u64 %rd2, [p_cos];" PTX-L
   s" ld.param.u64 %rd3, [p_sin];" PTX-L
   s" ld.param.u64 %rd4, [p_out];" PTX-L
   s" ld.param.u32 %r1, [p_k];" PTX-L ;

: CG-ROPEBW-PARAMS ( -- )
   s" ld.param.u64 %rd1, [p_dy];" PTX-L
   s" ld.param.u64 %rd2, [p_cos];" PTX-L
   s" ld.param.u64 %rd3, [p_sin];" PTX-L
   s" ld.param.u64 %rd4, [p_out];" PTX-L
   s" ld.param.u32 %r1, [p_k];" PTX-L ;

\ entry 1: forward (x=%rd1 cos=%rd2 sin=%rd3 out=%rd4 k=%r1) WITH the module header
: EMIT-ROPE ( -- )
   CG-ROPE-RESET  CG-HEADER CG-ROPE-ENTRY CG-SM-OPEN CG-ROPE-PARAMS
   1 MATRIX-REG  2 MATRIX-REG  3 MATRIX-REG  4 MATRIX-REG  ROPE-ROWS
   CG-SM-RET CG-SM-CLOSE ;

\ entry 2: backward (dy=%rd1 cos=%rd2 sin=%rd3 out=%rd4 k=%r1) with NO second header
: EMIT-ROPE-BWD ( -- )
   CG-ROPE-RESET  CG-ROPEBW-ENTRY CG-SM-OPEN CG-ROPEBW-PARAMS
   1 MATRIX-REG  2 MATRIX-REG  3 MATRIX-REG  4 MATRIX-REG  ROPE-ROWS-BWD
   CG-SM-RET CG-SM-CLOSE ;

EMIT-ROPE
EMIT-ROPE-BWD
