\ ad-entry-lib.f - per-VJP-entry forward/backward kernel emitters for the
\ device gradcheck gate (habu-ptx-ad-device).
\
\ Each VJP entry in lib/ptx/ad-dag.f (BLOCK-MAX select, B- both branches, EXP,
\ BLOCK-SUM broadcast, B/ both branches, DUP fan-out accumulation) is exercised
\ by a minimal op-list whose OUTPUT is per-lane, so the dy seed semantics are
\ unambiguous:
\   EXP     [EXP]                exp(x)          - the EXP entry alone
\   XMSUB   [DUP BMAX BSUB]      x - max(x)      - BMAX select + BSUB + fan-out
\   XDIVSUM [DUP BSUM BDIV]      x / sum(x)      - BSUM broadcast + BDIV + fan-out
\   SOFTMAX [DUP BMAX BSUB EXP DUP BSUM BDIV]    - the full generated backward
\ ADE-FWD emits the DAG's OWN forward recompute as a kernel (AD_FWD), so the
\ central difference differentiates exactly the function whose backward AD_BWD
\ emits. ADE-XDIVSUM-BWD-WRONG is the deliberate wrong-VJP fixture: the DUP
\ fan-out cotangent is DROPPED (dx = dy/s only), the review-feared class - it
\ type-checks but must FAIL device gradcheck. Load after lib/ptx/cg.f,
\ lib/ptx/header.f, lib/ptx/cg-collective.f, lib/ptx/collective.f,
\ lib/ptx/ad-dag.f. Emitters write PTX to stdout.

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

256 %BLOCK

create ADE-EXP-OPS 1 cells allot
create ADE-XMSUB-OPS 3 cells allot
create ADE-XDIVSUM-OPS 3 cells allot
create ADE-SOFTMAX-OPS 7 cells allot

: ADE-INIT ( -- )
   OP-EXP  ADE-EXP-OPS 0 cells + !
   OP-DUP  ADE-XMSUB-OPS 0 cells + !
   OP-BMAX ADE-XMSUB-OPS 1 cells + !
   OP-BSUB ADE-XMSUB-OPS 2 cells + !
   OP-DUP  ADE-XDIVSUM-OPS 0 cells + !
   OP-BSUM ADE-XDIVSUM-OPS 1 cells + !
   OP-BDIV ADE-XDIVSUM-OPS 2 cells + !
   OP-DUP  ADE-SOFTMAX-OPS 0 cells + !
   OP-BMAX ADE-SOFTMAX-OPS 1 cells + !
   OP-BSUB ADE-SOFTMAX-OPS 2 cells + !
   OP-EXP  ADE-SOFTMAX-OPS 3 cells + !
   OP-DUP  ADE-SOFTMAX-OPS 4 cells + !
   OP-BSUM ADE-SOFTMAX-OPS 5 cells + !
   OP-BDIV ADE-SOFTMAX-OPS 6 cells + ! ;

ADE-INIT

: ADE-FWD-ENTRY ( -- )
   s" .visible .entry AD_FWD(.param .u64 p_in, .param .u64 p_out, .param .u32 p_k)" PTX-L ;

: ADE-BWD-ENTRY ( -- )
   s" .visible .entry AD_BWD(.param .u64 p_x, .param .u64 p_dy, .param .u64 p_out, .param .u32 p_k)" PTX-L ;

\ AD_FWD(in, out, k): out[row][col] = f(x)[col], f = the op-list forward
: ADE-FWD ( ptr a n -- ) {: ops:ptr len:n :}
   CG-SM-RESET  CG-HEADER ADE-FWD-ENTRY CG-SM-OPEN CG-SM-PARAMS
   EMIT-ROW              {: r:n :}
   1 r EMIT-ROW-SPAN     {: xsp:n :}
   xsp EMIT-ROW-CTX      {: c:n :}
   xsp c EMIT-ROW-LOAD   {: x:n :}
   ops len AD-BUILD
   x AD-EMIT-FWD
   AD-OUT @ AD-REG@      {: y:n :}
   2 r EMIT-ROW-SPAN     {: osp:n :}
   y osp c EMIT-ROW-STORE
   CG-SM-RET CG-SM-CLOSE ;

\ AD_BWD(x, dy, out, k): out = the auto-derived backward dx
: ADE-BWD ( ptr a n -- ) {: ops:ptr len:n :}
   CG-BW-RESET  CG-HEADER ADE-BWD-ENTRY CG-SM-OPEN CG-BW-PARAMS
   EMIT-ROW              {: r:n :}
   1 r EMIT-ROW-SPAN     {: xsp:n :}
   xsp EMIT-ROW-CTX      {: c:n :}
   xsp c EMIT-ROW-LOAD   {: x:n :}
   2 r EMIT-ROW-SPAN     {: dysp:n :}
   dysp c EMIT-ROW-LOAD  {: dy:n :}
   ops len x dy AD-EMIT-BWD {: dx:n :}
   3 r EMIT-ROW-SPAN     {: osp:n :}
   dx osp c EMIT-ROW-STORE
   CG-SM-RET CG-SM-CLOSE ;

: ADE-EXP-FWD ( -- )      ADE-EXP-OPS 1 ADE-FWD ;
: ADE-EXP-BWD ( -- )      ADE-EXP-OPS 1 ADE-BWD ;
: ADE-XMSUB-FWD ( -- )    ADE-XMSUB-OPS 3 ADE-FWD ;
: ADE-XMSUB-BWD ( -- )    ADE-XMSUB-OPS 3 ADE-BWD ;
: ADE-XDIVSUM-FWD ( -- )  ADE-XDIVSUM-OPS 3 ADE-FWD ;
: ADE-XDIVSUM-BWD ( -- )  ADE-XDIVSUM-OPS 3 ADE-BWD ;
: ADE-SOFTMAX-FWD ( -- )  ADE-SOFTMAX-OPS 7 ADE-FWD ;
: ADE-SOFTMAX-BWD ( -- )  ADE-SOFTMAX-OPS 7 ADE-BWD ;

\ Deliberate wrong VJP for y = x/sum(x): keeps ONLY the direct dy/s branch and
\ DROPS the DUP fan-out cotangent (-Sum(dy*y)/s broadcast). This is the fan-out
\ mishandling class (cotangent overwrite/drop instead of +. accumulation): the
\ kernel type-checks and assembles - only device gradcheck can reject it.
: ADE-XDIVSUM-BWD-WRONG ( -- )
   CG-BW-RESET  CG-HEADER ADE-BWD-ENTRY CG-SM-OPEN CG-BW-PARAMS
   EMIT-ROW              {: r:n :}
   1 r EMIT-ROW-SPAN     {: xsp:n :}
   xsp EMIT-ROW-CTX      {: c:n :}
   xsp c EMIT-ROW-LOAD   {: x:n :}
   2 r EMIT-ROW-SPAN     {: dysp:n :}
   dysp c EMIT-ROW-LOAD  {: dy:n :}
   x EMIT-BLOCK-SUM      {: s:n :}
   dy s EMIT-B/          {: dx:n :}
   3 r EMIT-ROW-SPAN     {: osp:n :}
   dx osp c EMIT-ROW-STORE
   CG-SM-RET CG-SM-CLOSE ;
