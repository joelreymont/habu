\ layernorm-cg.f - emit ONE PTX module holding BOTH the checked LAYERNORM_ROWS
\ forward and the checked closed-form LAYERNORM_BWD_ROWS backward, so a consumer
\ loads a SINGLE cubin and pulls both function handles from it.
\
\ PLAIN (no-affine) LayerNorm, mirroring maki/layernorm.f LN-FWD / LN-BWD op-for-op:
\   fwd  y   = (x-mu)/sqrt(var+eps)         mu=mean(x), var=mean((x-mu)^2)
\   bwd  dx  = (dy - mdy - xhat*mdyx)/std    xhat=(x-mu)/std, mdy=mean(dy), mdyx=mean(dy*xhat)
\ Both bodies are CHECKED KERNEL: definitions over the M6 row/collective vocabulary
\ (lib/ptx/collective.f): running them in emit mode produces their PTX - two-level
\ warp block reductions via shared memory + bar.sync, then the elementwise scale.
\ A PTX MODULE is exactly ONE header then N `.visible .entry` kernels, so CG-HEADER
\ is emitted once by the forward and the backward appends its entry with NO second
\ header. Emits to stdout; ptxas assembles the whole stream for the probed device
\ arch (sm_87 Orin / sm_121a GB10). Load after lib/ptx/cg.f, lib/ptx/header.f,
\ lib/ptx/cg-collective.f, lib/ptx/tile.f, lib/ptx/collective.f.
\
\ EPS: LayerNorm's eps and RMSNorm's eps are the SAME normalization constant -
\ maki/layernorm.f LN-EPS = maki/rmsnorm.f RMS-EPS = 1e-5 - so the device eps-add is
\ the shared RMS-EPS+ collective op (bit-identical F32:NARROW(1e-5) = 0f3727C5AC). No
\ separate LN-EPS+ op is minted: that would duplicate one instruction and couple a
\ shared lib to this kernel. (A neutral rename of RMS-EPS+ is an owed clarity-only
\ cleanup, its own dot - it changes no bits.)
\
\ AFFINE (gamma/beta) BOUNDARY: this kernel pair covers PLAIN LayerNorm only. The
\ affine forward y = gamma*xhat + beta is already device-lowered in the LRED path
\ (maki/lower/red.f LRED-EMIT-LN-AFFINE, a row-parallel mul+add epilogue); the affine
\ parameter gradients dgamma = sum_rows dy*xhat and dbeta = sum_rows dy are CROSS-ROW
\ COLUMN reductions - a different reduction axis than this block-per-row template - so
\ they are NOT lowered here, matching maki/lower/red.f which fail-closes layernorm-bwd
\ (E-LRED-OP). The entries below take only (in,out,k)/(x,dy,out,k): no gamma/beta param
\ exists, so an affine caller cannot silently reach the plain kernel.

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

KERNEL: LAYERNORM-ROWS ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: in out :}
   ROW            {: r :}
   in r ROW-SPAN  {: xs :}
   xs ROW-CTX     {: c :}
   xs c ROW-LOAD  {: x :}
   x BLOCK-SUM UN PTX:U/         {: mu :}      \ mean(x)
   x mu PTX:B-                   {: d :}       \ x - mu
   d d *. BLOCK-SUM UN PTX:U/    {: var :}     \ mean((x-mu)^2)
   var RMS-EPS+ USQRT           {: std :}      \ sqrt(var+eps)   (shared 1e-5)
   d std PTX:B/  out r ROW-SPAN c ROW-STORE ;

KERNEL: LAYERNORM-ROWS-BWD ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: x dy dx :}
   ROW            {: r :}
   x  r ROW-SPAN  {: xs :}
   dy r ROW-SPAN  {: dys :}
   xs ROW-CTX     {: c :}
   xs  c ROW-LOAD {: xt :}
   dys c ROW-LOAD {: dyt :}
   xt BLOCK-SUM UN PTX:U/        {: mu :}      \ mean(x)
   xt mu PTX:B-                  {: d :}       \ x - mu
   d d *. BLOCK-SUM UN PTX:U/    {: var :}     \ mean((x-mu)^2)
   var RMS-EPS+ USQRT           {: std :}      \ sqrt(var+eps)
   d std PTX:B/                  {: xh :}      \ xhat = (x-mu)/std
   dyt BLOCK-SUM UN PTX:U/       {: mdy :}     \ mean(dy)
   dyt xh *. BLOCK-SUM UN PTX:U/ {: mdyx :}    \ mean(dy*xhat)
   dyt mdy PTX:B-                              \ dy - mdy
   xh mdyx SCALE                               \ xhat*mdyx
   -.                                          \ (dy-mdy) - xhat*mdyx
   std PTX:B/                                  \ / std
   dx r ROW-SPAN c ROW-STORE ;

: CG-LN-ENTRY ( -- )
   s" .visible .entry LAYERNORM_ROWS(.param .u64 p_in, .param .u64 p_out, .param .u32 p_k)" PTX-L ;

: CG-LNBW-ENTRY ( -- )
   s" .visible .entry LAYERNORM_BWD_ROWS(.param .u64 p_x, .param .u64 p_dy, .param .u64 p_out, .param .u32 p_k)" PTX-L ;

: CG-LNBW-PARAMS ( -- )
   s" ld.param.u64 %rd1, [p_x];" PTX-L
   s" ld.param.u64 %rd2, [p_dy];" PTX-L
   s" ld.param.u64 %rd3, [p_out];" PTX-L
   s" ld.param.u32 %r1, [p_k];" PTX-L ;

\ entry 1: forward (in=%rd1 out=%rd2 k=%r1) WITH the module header
: EMIT-LAYERNORM ( -- )
   CG-SM-RESET  CG-HEADER CG-LN-ENTRY CG-SM-OPEN CG-SM-PARAMS
   1 MATRIX-REG  2 MATRIX-REG  LAYERNORM-ROWS
   CG-SM-RET CG-SM-CLOSE ;

\ entry 2: backward (x=%rd1 dy=%rd2 out=%rd3 k=%r1) with NO second header
: EMIT-LAYERNORM-BWD ( -- )
   CG-BW-RESET  CG-LNBW-ENTRY CG-SM-OPEN CG-LNBW-PARAMS
   1 MATRIX-REG  2 MATRIX-REG  3 MATRIX-REG  LAYERNORM-ROWS-BWD
   CG-SM-RET CG-SM-CLOSE ;

EMIT-LAYERNORM
EMIT-LAYERNORM-BWD
