\ softmax-fb-cg.f - emit ONE PTX module holding BOTH the forward SOFTMAX_ROWS and
\ the AD-derived SOFTMAX_BWD kernels, so a consumer loads a SINGLE cubin and pulls
\ both function handles from it (retiring the /tmp/softmax.cubin+softmax-bwd.cubin pair).
\
\ Legal PTX is exactly ONE .version/.target/.address_size header followed by N
\ `.visible .entry` kernels (ptxas rejects a second header in one stream). We reuse
\ tools/ptx/softmax-cg.f VERBATIM: its inline CG-HEADER is the module header and its
\ EMIT-SOFTMAX emits entry 1 (SOFTMAX_ROWS). We then APPEND the AD-derived backward
\ as entry 2 (SOFTMAX_BWD) with NO second header - EMIT-SM-BWD-BODY is exactly the
\ softmax-bwd-cg.f binding minus CG-HEADER. The forward kernel stays single-sourced
\ in softmax-cg.f; only the thin backward binding around the shared reverse-AD engine
\ (lib/ptx/ad-dag.f AD-EMIT-BWD) lives here. Emits to stdout; ptxas -arch=sm_87
\ assembles the whole stream.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/test.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/cg-collective.f
require lib/ptx/collective.f
require lib/ptx/ad-dag.f

create SBW-OPS 7 cells allot
: SBW-INIT ( -- )
   OP-DUP  SBW-OPS 0 cells + !  OP-BMAX SBW-OPS 1 cells + !  OP-BSUB SBW-OPS 2 cells + !
   OP-EXP  SBW-OPS 3 cells + !  OP-DUP  SBW-OPS 4 cells + !  OP-BSUM SBW-OPS 5 cells + !
   OP-BDIV SBW-OPS 6 cells + ! ;

\ backward entry+body WITHOUT the module header (the header is supplied once by the
\ forward file's CG-HEADER); appends SOFTMAX_BWD as the module's second .entry.
: EMIT-SM-BWD-BODY ( -- )
   SBW-INIT
   CG-BW-RESET  CG-BW-ENTRY CG-SM-OPEN CG-BW-PARAMS
   EMIT-ROW              {: r:n :}                 \ blockIdx.x = row
   1 r EMIT-ROW-SPAN     {: xsp:n :}               \ x row base   (p_x = %rd1)
   xsp EMIT-ROW-CTX      {: c:n :}                 \ per-thread column offset
   xsp c EMIT-ROW-LOAD   {: x:n :}                 \ load x[row][col]
   2 r EMIT-ROW-SPAN     {: dysp:n :}              \ dy row base  (p_dy = %rd2)
   dysp c EMIT-ROW-LOAD  {: dy:n :}
   SBW-OPS 7 x dy AD-EMIT-BWD  {: dx:n :}          \ AUTO-DERIVE + emit the backward
   3 r EMIT-ROW-SPAN     {: osp:n :}               \ out row base (p_out = %rd3)
   dx osp c EMIT-ROW-STORE                         \ store dx
   CG-SM-RET CG-SM-CLOSE ;

s" tools/ptx/softmax-cg.f" included                \ module header + SOFTMAX_ROWS (entry 1)
EMIT-SM-BWD-BODY                                   \ SOFTMAX_BWD (entry 2, no 2nd header)
