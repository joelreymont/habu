\ softmax-bwd-cg.f - emit the AUTO-DERIVED SOFTMAX-ROWS backward via the AD engine.
\
\ The forward softmax compute is the op sequence  DUP BLOCK-MAX B- EXP DUP BLOCK-SUM B/
\ (x fans out to BLOCK-MAX & B-; e fans out to BLOCK-SUM & B/). lib/ptx/ad-dag.f
\ symbolic-executes it into a DAG and reverse-mode-AD-emits the backward: recompute
\ the forward, then the cotangent pass (BLOCK-MAX-SELECT, B/ / B- adjoints, fan-out
\ sums). The kernel takes x (input) and dy (output cotangent) and writes dx. Load
\ after lib/ptx/cg.f, lib/ptx/header.f, lib/ptx/cg-collective.f, lib/ptx/ad-dag.f.
\ Emits to stdout.

create SBW-OPS 7 cells allot
: SBW-INIT ( -- )
   OP-DUP  SBW-OPS 0 cells + !  OP-BMAX SBW-OPS 1 cells + !  OP-BSUB SBW-OPS 2 cells + !
   OP-EXP  SBW-OPS 3 cells + !  OP-DUP  SBW-OPS 4 cells + !  OP-BSUM SBW-OPS 5 cells + !
   OP-BDIV SBW-OPS 6 cells + ! ;

256 %BLOCK

: EMIT-SOFTMAX-BWD ( -- )
   SBW-INIT
   CG-BW-RESET  CG-HEADER CG-BW-ENTRY CG-SM-OPEN CG-BW-PARAMS
   EMIT-ROW              {: r :}                 \ blockIdx.x = row
   1 r EMIT-ROW-SPAN     {: xsp :}               \ x row base   (p_x = %rd1)
   xsp EMIT-ROW-CTX      {: c :}                 \ per-thread column offset
   xsp c EMIT-ROW-LOAD   {: x :}                 \ load x[row][col]
   2 r EMIT-ROW-SPAN     {: dysp :}              \ dy row base  (p_dy = %rd2)
   dysp c EMIT-ROW-LOAD   {: dy:n :}
   SBW-OPS 7 x dy AD-EMIT-BWD  {: dx :}          \ AUTO-DERIVE + emit the backward
   3 r EMIT-ROW-SPAN     {: osp :}               \ out row base (p_out = %rd3)
   dx osp c EMIT-ROW-STORE                       \ store dx
   CG-SM-RET CG-SM-CLOSE ;

EMIT-SOFTMAX-BWD
