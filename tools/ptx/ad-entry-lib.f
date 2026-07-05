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
require lib/ptx/ad.f
require lib/ptx/ad-gen.f

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

\ ==== two-input elementwise entries: +. -. *. /. (vjp.f table, device leg) =====
\ AD2_FWD(x, y, out, k): z = x <op> y elementwise. AD2_BWD(x, y, dz, dxo, dyo, k):
\ both cotangents per the table. OVERK models the OVER fan-out (z = x*y + x);
\ its WRONG variant is the OVER-as-permutation bug (the copied value's second
\ cotangent dropped). DROPK models DROP (z = x*x, y consumed and dropped); its
\ correct backward writes a TYPED ZERO into dy and its WRONG variant leaks the
\ incoming cotangent (dy = dz).

variable ADE2-R
variable ADE2-C
variable ADE2-X
variable ADE2-Y
variable ADE2-DZ

: ADE2-RESET ( -- )
   1 CG-NF !  4 CG-NRD !  2 CG-NR !  1 CG-NP !  0 CG-NL ! ;

: ADE2-BW-RESET ( -- )
   1 CG-NF !  6 CG-NRD !  2 CG-NR !  1 CG-NP !  0 CG-NL ! ;

: ADE2-FWD-ENTRY ( -- )
   s" .visible .entry AD2_FWD(.param .u64 p_x, .param .u64 p_y, .param .u64 p_out, .param .u32 p_k)" PTX-L ;

: ADE2-BWD-ENTRY ( -- )
   s" .visible .entry AD2_BWD(.param .u64 p_x, .param .u64 p_y, .param .u64 p_dz, .param .u64 p_dxo, .param .u64 p_dyo, .param .u32 p_k)" PTX-L ;

: ADE2-FWD-PARAMS ( -- )
   s" ld.param.u64 %rd1, [p_x];" PTX-L
   s" ld.param.u64 %rd2, [p_y];" PTX-L
   s" ld.param.u64 %rd3, [p_out];" PTX-L
   s" ld.param.u32 %r1, [p_k];" PTX-L ;

: ADE2-BWD-PARAMS ( -- )
   s" ld.param.u64 %rd1, [p_x];" PTX-L
   s" ld.param.u64 %rd2, [p_y];" PTX-L
   s" ld.param.u64 %rd3, [p_dz];" PTX-L
   s" ld.param.u64 %rd4, [p_dxo];" PTX-L
   s" ld.param.u64 %rd5, [p_dyo];" PTX-L
   s" ld.param.u32 %r1, [p_k];" PTX-L ;

: ADE2-FWD ( n -- ) {: op:n :}   \ emit AD2_FWD with z = x <op> y
   ADE2-RESET  CG-HEADER ADE2-FWD-ENTRY CG-SM-OPEN ADE2-FWD-PARAMS
   EMIT-ROW              {: r:n :}
   1 r EMIT-ROW-SPAN     {: xsp:n :}
   xsp EMIT-ROW-CTX      {: c:n :}
   xsp c EMIT-ROW-LOAD   {: x:n :}
   2 r EMIT-ROW-SPAN c EMIT-ROW-LOAD {: y:n :}
   x y op EMIT-BIN-F32
   3 r EMIT-ROW-SPAN c EMIT-ROW-STORE
   CG-SM-RET CG-SM-CLOSE ;

: ADE2-BWD-OPEN ( -- )   \ scaffold + load x, y, dz into the shared cells
   ADE2-BW-RESET  CG-HEADER ADE2-BWD-ENTRY CG-SM-OPEN ADE2-BWD-PARAMS
   EMIT-ROW ADE2-R !
   1 ADE2-R @ EMIT-ROW-SPAN {: xsp:n :}
   xsp EMIT-ROW-CTX ADE2-C !
   xsp ADE2-C @ EMIT-ROW-LOAD ADE2-X !
   2 ADE2-R @ EMIT-ROW-SPAN ADE2-C @ EMIT-ROW-LOAD ADE2-Y !
   3 ADE2-R @ EMIT-ROW-SPAN ADE2-C @ EMIT-ROW-LOAD ADE2-DZ ! ;

: ADE2-DX! ( n -- )
   4 ADE2-R @ EMIT-ROW-SPAN ADE2-C @ EMIT-ROW-STORE ;

: ADE2-DY! ( n -- )
   5 ADE2-R @ EMIT-ROW-SPAN ADE2-C @ EMIT-ROW-STORE ;

: ADE2-CLOSE ( -- )
   CG-SM-RET CG-SM-CLOSE ;

: ADE-ADD2-FWD ( -- )  CG-OP-ADD ADE2-FWD ;
: ADE-SUB2-FWD ( -- )  CG-OP-SUB ADE2-FWD ;
: ADE-MUL2-FWD ( -- )  CG-OP-MUL ADE2-FWD ;
: ADE-DIV2-FWD ( -- )  CG-OP-DIV ADE2-FWD ;

: ADE-ADD2-BWD ( -- )   \ dx = dz, dy = dz
   ADE2-BWD-OPEN
   ADE2-DZ @ ADE2-DX!
   ADE2-DZ @ ADE2-DY!
   ADE2-CLOSE ;

: ADE-SUB2-BWD ( -- )   \ dx = dz, dy = -dz
   ADE2-BWD-OPEN
   ADE2-DZ @ ADE2-DX!
   ADE2-DZ @ EMIT-NEG ADE2-DY!
   ADE2-CLOSE ;

: ADE-MUL2-BWD ( -- )   \ dx = dz*y, dy = dz*x
   ADE2-BWD-OPEN
   ADE2-DZ @ ADE2-Y @ EMIT-MUL ADE2-DX!
   ADE2-DZ @ ADE2-X @ EMIT-MUL ADE2-DY!
   ADE2-CLOSE ;

: ADE-DIV2-BWD ( -- )   \ dx = dz/y, dy = -dz*z/y (z = x/y recomputed)
   ADE2-BWD-OPEN
   ADE2-DZ @ ADE2-Y @ EMIT-DIV ADE2-DX!
   ADE2-X @ ADE2-Y @ EMIT-DIV {: z:n :}
   ADE2-DZ @ z EMIT-MUL ADE2-Y @ EMIT-DIV EMIT-NEG ADE2-DY!
   ADE2-CLOSE ;

\ OVER fan-out composite: z = x*y + x (x copied to both the product and the sum)
: ADE-OVERK-FWD ( -- )
   ADE2-RESET  CG-HEADER ADE2-FWD-ENTRY CG-SM-OPEN ADE2-FWD-PARAMS
   EMIT-ROW              {: r:n :}
   1 r EMIT-ROW-SPAN     {: xsp:n :}
   xsp EMIT-ROW-CTX      {: c:n :}
   xsp c EMIT-ROW-LOAD   {: x:n :}
   2 r EMIT-ROW-SPAN c EMIT-ROW-LOAD {: y:n :}
   x y EMIT-MUL x EMIT-ADD
   3 r EMIT-ROW-SPAN c EMIT-ROW-STORE
   CG-SM-RET CG-SM-CLOSE ;

: ADE-OVERK-BWD ( -- )   \ dx = dz*y + dz (SUM of the copy's two cotangents), dy = dz*x
   ADE2-BWD-OPEN
   ADE2-DZ @ ADE2-Y @ EMIT-MUL ADE2-DZ @ EMIT-ADD ADE2-DX!
   ADE2-DZ @ ADE2-X @ EMIT-MUL ADE2-DY!
   ADE2-CLOSE ;

\ the OVER-as-permutation bug: the copy's second cotangent is DROPPED, not summed
: ADE-OVERK-BWD-WRONG ( -- )
   ADE2-BWD-OPEN
   ADE2-DZ @ ADE2-Y @ EMIT-MUL ADE2-DX!
   ADE2-DZ @ ADE2-X @ EMIT-MUL ADE2-DY!
   ADE2-CLOSE ;

\ DROP composite: z = x*x, y loaded then dropped (no data flow into z)
: ADE-DROPK-FWD ( -- )
   ADE2-RESET  CG-HEADER ADE2-FWD-ENTRY CG-SM-OPEN ADE2-FWD-PARAMS
   EMIT-ROW              {: r:n :}
   1 r EMIT-ROW-SPAN     {: xsp:n :}
   xsp EMIT-ROW-CTX      {: c:n :}
   xsp c EMIT-ROW-LOAD   {: x:n :}
   x x EMIT-MUL
   3 r EMIT-ROW-SPAN c EMIT-ROW-STORE
   CG-SM-RET CG-SM-CLOSE ;

: ADE-DROPK-BWD ( -- )   \ dx = dz*x + dz*x, dy = the TYPED ZERO of the dropped value
   ADE2-BWD-OPEN
   ADE2-DZ @ ADE2-X @ EMIT-MUL ADE2-DZ @ ADE2-X @ EMIT-MUL EMIT-ADD ADE2-DX!
   EMIT-ZERO ADE2-DY!
   ADE2-CLOSE ;

\ the DROP cotangent leak: the dropped value receives dz instead of zero
: ADE-DROPK-BWD-WRONG ( -- )
   ADE2-BWD-OPEN
   ADE2-DZ @ ADE2-X @ EMIT-MUL ADE2-DZ @ ADE2-X @ EMIT-MUL EMIT-ADD ADE2-DX!
   ADE2-DZ @ ADE2-DY!
   ADE2-CLOSE ;

\ ==== scalar-factor entries: SCALE and FMA. (uniform a as a launch param) ======
\ ADS_FWD(x, out, a, k): z = x*a. ADS_BWD(x, dz, dxo, dao, a, k): dx = a*dz,
\ da = Sum(dz*x) stored broadcast (the launcher reads lane 0).
\ ADF_FWD(x, y, out, a, k): z = a*x + y. ADF_BWD(x, dz, dxo, dyo, dao, a, k).

: ADE-PARAM-A ( -- n )   \ ld.param.f32 of p_a into a fresh register
   CG-NEXT-F {: f:n :}
   SB-RESET s" ld.param.f32 " CG-S f CG-F s" , [p_a];" CG-S CG-LINE
   f ;

: ADES-RESET ( -- )
   1 CG-NF !  3 CG-NRD !  2 CG-NR !  1 CG-NP !  0 CG-NL ! ;

: ADES-BW-RESET ( -- )
   1 CG-NF !  5 CG-NRD !  2 CG-NR !  1 CG-NP !  0 CG-NL ! ;

: ADES-FWD-ENTRY ( -- )
   s" .visible .entry ADS_FWD(.param .u64 p_x, .param .u64 p_out, .param .f32 p_a, .param .u32 p_k)" PTX-L ;

: ADES-BWD-ENTRY ( -- )
   s" .visible .entry ADS_BWD(.param .u64 p_x, .param .u64 p_dz, .param .u64 p_dxo, .param .u64 p_dao, .param .f32 p_a, .param .u32 p_k)" PTX-L ;

: ADES-FWD-PARAMS ( -- )
   s" ld.param.u64 %rd1, [p_x];" PTX-L
   s" ld.param.u64 %rd2, [p_out];" PTX-L
   s" ld.param.u32 %r1, [p_k];" PTX-L ;

: ADES-BWD-PARAMS ( -- )
   s" ld.param.u64 %rd1, [p_x];" PTX-L
   s" ld.param.u64 %rd2, [p_dz];" PTX-L
   s" ld.param.u64 %rd3, [p_dxo];" PTX-L
   s" ld.param.u64 %rd4, [p_dao];" PTX-L
   s" ld.param.u32 %r1, [p_k];" PTX-L ;

: ADE-SCALE-FWD ( -- )
   ADES-RESET  CG-HEADER ADES-FWD-ENTRY CG-SM-OPEN ADES-FWD-PARAMS
   ADE-PARAM-A           {: a:n :}
   EMIT-ROW              {: r:n :}
   1 r EMIT-ROW-SPAN     {: xsp:n :}
   xsp EMIT-ROW-CTX      {: c:n :}
   xsp c EMIT-ROW-LOAD a EMIT-SCALE
   2 r EMIT-ROW-SPAN c EMIT-ROW-STORE
   CG-SM-RET CG-SM-CLOSE ;

: ADE-SCALE-BWD ( -- )   \ dx = a*dz; da = Sum(dz*x) broadcast into dao
   ADES-BW-RESET  CG-HEADER ADES-BWD-ENTRY CG-SM-OPEN ADES-BWD-PARAMS
   ADE-PARAM-A           {: a:n :}
   EMIT-ROW              {: r:n :}
   1 r EMIT-ROW-SPAN     {: xsp:n :}
   xsp EMIT-ROW-CTX      {: c:n :}
   xsp c EMIT-ROW-LOAD   {: x:n :}
   2 r EMIT-ROW-SPAN c EMIT-ROW-LOAD {: dz:n :}
   dz a EMIT-SCALE
   3 r EMIT-ROW-SPAN c EMIT-ROW-STORE
   dz x EMIT-MUL EMIT-BLOCK-SUM EMIT-BROADCAST
   4 r EMIT-ROW-SPAN c EMIT-ROW-STORE
   CG-SM-RET CG-SM-CLOSE ;

: ADEF-RESET ( -- )
   1 CG-NF !  4 CG-NRD !  2 CG-NR !  1 CG-NP !  0 CG-NL ! ;

: ADEF-BW-RESET ( -- )
   1 CG-NF !  6 CG-NRD !  2 CG-NR !  1 CG-NP !  0 CG-NL ! ;

: ADEF-FWD-ENTRY ( -- )
   s" .visible .entry ADF_FWD(.param .u64 p_x, .param .u64 p_y, .param .u64 p_out, .param .f32 p_a, .param .u32 p_k)" PTX-L ;

: ADEF-BWD-ENTRY ( -- )
   s" .visible .entry ADF_BWD(.param .u64 p_x, .param .u64 p_dz, .param .u64 p_dxo, .param .u64 p_dyo, .param .u64 p_dao, .param .f32 p_a, .param .u32 p_k)" PTX-L ;

: ADEF-FWD-PARAMS ( -- )
   s" ld.param.u64 %rd1, [p_x];" PTX-L
   s" ld.param.u64 %rd2, [p_y];" PTX-L
   s" ld.param.u64 %rd3, [p_out];" PTX-L
   s" ld.param.u32 %r1, [p_k];" PTX-L ;

: ADEF-BWD-PARAMS ( -- )
   s" ld.param.u64 %rd1, [p_x];" PTX-L
   s" ld.param.u64 %rd2, [p_dz];" PTX-L
   s" ld.param.u64 %rd3, [p_dxo];" PTX-L
   s" ld.param.u64 %rd4, [p_dyo];" PTX-L
   s" ld.param.u64 %rd5, [p_dao];" PTX-L
   s" ld.param.u32 %r1, [p_k];" PTX-L ;

: ADE-FMA-FWD ( -- )   \ z = a*x + y
   ADEF-RESET  CG-HEADER ADEF-FWD-ENTRY CG-SM-OPEN ADEF-FWD-PARAMS
   ADE-PARAM-A           {: a:n :}
   EMIT-ROW              {: r:n :}
   1 r EMIT-ROW-SPAN     {: xsp:n :}
   xsp EMIT-ROW-CTX      {: c:n :}
   xsp c EMIT-ROW-LOAD   {: x:n :}
   2 r EMIT-ROW-SPAN c EMIT-ROW-LOAD {: y:n :}
   a x y EMIT-FMA
   3 r EMIT-ROW-SPAN c EMIT-ROW-STORE
   CG-SM-RET CG-SM-CLOSE ;

: ADE-FMA-BWD ( -- )   \ dx = a*dz; dy = dz; da = Sum(dz*x) broadcast
   ADEF-BW-RESET  CG-HEADER ADEF-BWD-ENTRY CG-SM-OPEN ADEF-BWD-PARAMS
   ADE-PARAM-A           {: a:n :}
   EMIT-ROW              {: r:n :}
   1 r EMIT-ROW-SPAN     {: xsp:n :}
   xsp EMIT-ROW-CTX      {: c:n :}
   xsp c EMIT-ROW-LOAD   {: x:n :}
   2 r EMIT-ROW-SPAN c EMIT-ROW-LOAD {: dz:n :}
   dz a EMIT-SCALE
   3 r EMIT-ROW-SPAN c EMIT-ROW-STORE
   dz 4 r EMIT-ROW-SPAN c EMIT-ROW-STORE
   dz x EMIT-MUL EMIT-BLOCK-SUM EMIT-BROADCAST
   5 r EMIT-ROW-SPAN c EMIT-ROW-STORE
   CG-SM-RET CG-SM-CLOSE ;

\ ==== GENERATED backward via the reverse pass (habu-ad-reverse-pass) ============
\ XSUBSUM: z = x - Sum(x), a LINEAR forward whose backward the reverse pass
\ GENERATES from the vjp.f table (AD-BACKWARD$) and ad-gen.f lowers to a kernel:
\ dx = dz + BROADCAST(-Sum(dz)). Deliberately different from the hand-written
\ softmax path: the emitted backward text comes from the pass, not from a
\ human. Both kernels lower through the same ADG dispatcher, so the forward the
\ central difference probes is exactly the body the reverse pass consumed.

: ADE-XSUBSUM-FWD$ ( -- ptr u8 n )
   s" ROW-LOAD DUP BLOCK-SUM PTX:B- ROW-STORE" ;

: ADE-GEN-FWD ( ptr u8 n -- ) {: a:ptr u:n :}   \ lower a 1-in/1-out forward as AD_FWD
   CG-SM-RESET  CG-HEADER ADE-FWD-ENTRY CG-SM-OPEN CG-SM-PARAMS
   EMIT-ROW              {: r:n :}
   1 r EMIT-ROW-SPAN     {: xsp:n :}
   xsp EMIT-ROW-CTX      {: c:n :}
   2 r EMIT-ROW-SPAN     {: osp:n :}
   a u xsp osp c ADG-LOWER
   CG-SM-RET CG-SM-CLOSE ;

\ Lower a FORWARD body's generated backward as AD_BWD: the reverse pass makes
\ the backward text, and saves-ops resolve by row-local recompute from the
\ primal x span (habu-ad-thread-saved). Linear forwards skip the recompute.
: ADE-GEN-BWD ( ptr u8 n -- ) {: fa:ptr fu:n :}
   fa fu AD-BACKWARD$ {: ba:ptr bu:n :}
   CG-BW-RESET  CG-HEADER ADE-BWD-ENTRY CG-SM-OPEN CG-BW-PARAMS
   EMIT-ROW              {: r:n :}
   1 r EMIT-ROW-SPAN     {: xsp:n :}   \ primal span: read only by saves recompute
   xsp EMIT-ROW-CTX      {: c:n :}
   2 r EMIT-ROW-SPAN     {: dzsp:n :}
   3 r EMIT-ROW-SPAN     {: osp:n :}
   fa fu ba bu xsp dzsp osp c ADG-LOWER-BWD
   CG-SM-RET CG-SM-CLOSE ;

: ADE-XSUBSUM-FWD ( -- )
   ADE-XSUBSUM-FWD$ ADE-GEN-FWD ;

: ADE-XSUBSUM-BWD ( -- )
   ADE-XSUBSUM-FWD$ ADE-GEN-BWD ;

\ EXPGEN: z = exp(x) - the first generated backward with a SAVED value. The
\ reverse pass emits "ROW-LOAD SAVED-Y *. ROW-SCATTER-ADD"; SAVED-Y resolves by
\ recomputing y = exp(x) from the primal span inside the backward kernel.
: ADE-EXPGEN-FWD$ ( -- ptr u8 n )
   s" ROW-LOAD EXP. ROW-STORE" ;

: ADE-EXPGEN-FWD ( -- )
   ADE-EXPGEN-FWD$ ADE-GEN-FWD ;

: ADE-EXPGEN-BWD ( -- )
   ADE-EXPGEN-FWD$ ADE-GEN-BWD ;
