\ ptx-collective-test.f - checked numerically-stable SOFTMAX-ROWS (M6).
\
\ The KERNEL: definition IS the positive proof: the body certifies against its
\ declared parametric effect. in/out share extent-r/extent-c by token, so one
\ row context `c` is valid for both spans; the mask token threads from ROW-LOAD
\ through PTX:B-/EXP./BLOCK-SUM/PTX:B/ to ROW-STORE by unification. A reject would emit a
\ diagnostic and fail the load.
\
\ The kernel locals stay BARE. Every type a kernel body binds - matrix, rowidx,
\ span, rowctx, tile - is a PARAMETRIC family, and a `{: x:fam<..> :}` annotation
\ is fail-closed in the locals parser (docs/type-families.md 17.1; the capability
\ is dot habu-typed-locals-for-b06b6707). A single-letter annotation such as `x:a`
\ is not a substitute: it DECLARES a fresh quantifier `a` for this word, which the
\ body then specializes to matrix - a false parametricity claim the checker
\ rejects with E-NONPARAMETRIC-EFFECT.

require lib/ptx/test-prelude.f
require test/checker-assert.f

package PTX-COLLECTIVE-TEST

T-RESET

256 %BLOCK

: PTXC-CHECK-REJECTS ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

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

\ Exercises BLOCK-MAX-SELECT (the BLOCK-MAX adjoint): load a row, take its max,
\ then scatter a cotangent back to the arg-max lane.
KERNEL: MAX-SELECT-ROWS ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: in out :}
   ROW            {: r :}
   in r ROW-SPAN  {: xs :}
   xs ROW-CTX     {: c :}
   xs c ROW-LOAD  {: x :}
   x BLOCK-MAX    {: mx :}
   mx x mx BLOCK-MAX-SELECT       \ ( ds=mx, x, mx -- dx tile )
   out r ROW-SPAN c ROW-STORE ;

KERNEL: SUM-ROWS ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: in out :}                 \ typed-local-lint: allow-bare-local - parametric kernel type
   ROW            {: r :}       \ typed-local-lint: allow-bare-local - parametric kernel type
   in r ROW-SPAN  {: xs :}      \ typed-local-lint: allow-bare-local - parametric kernel type
   xs ROW-CTX     {: ctx :}     \ typed-local-lint: allow-bare-local - parametric kernel type
   xs ctx ROW-LOAD BLOCK-SUM BROADCAST
   out r ROW-SPAN ctx ROW-STORE ;

KERNEL: ROW-SCATTER-ROWS ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: in out :}                 \ typed-local-lint: allow-bare-local - parametric kernel type
   ROW            {: r :}       \ typed-local-lint: allow-bare-local - parametric kernel type
   in r ROW-SPAN  {: xs :}      \ typed-local-lint: allow-bare-local - parametric kernel type
   xs ROW-CTX     {: ctx :}     \ typed-local-lint: allow-bare-local - parametric kernel type
   xs ctx ROW-LOAD
   out r ROW-SPAN ctx ROW-SCATTER-ADD ;

KERNEL: ROW-ONCE-ROWS ( matrix<space-global-once,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: in :}                        \ typed-local-lint: allow-bare-local - parametric kernel type
   ROW                  {: r :}    \ typed-local-lint: allow-bare-local - parametric kernel type
   in r ROW-SPAN-ONCE   {: xs :}   \ typed-local-lint: allow-bare-local - parametric kernel type
   xs ROW-CTX-ONCE      {: ctx :}  \ typed-local-lint: allow-bare-local - parametric kernel type
   xs ctx ROW-LOAD-ONCE
   xs ctx ROW-STORE-ONCE ;

\ RMSNORM-ROWS: one block per row, coalesced hidden-dim load, then reduce + rsqrt +
\ scale in ONE kernel - y = x / sqrt(mean(x^2)+eps), the maki/rmsnorm.f RMS-FWD
\ closed form. in/out share extent-r/extent-c by token, so one row ctx `c` serves
\ both spans and the mask threads x -> square -> BLOCK-SUM -> scale -> store.
KERNEL: RMSNORM-ROWS ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: in out :}
   ROW            {: r :}
   in r ROW-SPAN  {: xs :}
   xs ROW-CTX     {: c :}
   xs c ROW-LOAD  {: x :}
   x x *. BLOCK-SUM             {: ss :}    \ sum_i x_i^2
   ss UN PTX:U/ RMS-EPS+ USQRT {: rr :}     \ r = sqrt(mean(x^2)+eps)
   x rr PTX:B/  out r ROW-SPAN c ROW-STORE ;

\ RMSNORM-ROWS-BWD: the CHECKED closed-form VJP (maki/rmsnorm.f RMS-BWD),
\ dx = (dy - x*coef)/r with r = sqrt(mean(x^2)+eps), coef = mean(dy*x)/r^2 =
\ S/(n r^2). x/dy/dx share extent-r/extent-c BY TOKEN, so len(dx)=len(x) is proven.
KERNEL: RMSNORM-ROWS-BWD ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: x dy dx :}
   ROW            {: r :}
   x  r ROW-SPAN  {: xs :}
   dy r ROW-SPAN  {: dys :}
   xs ROW-CTX     {: c :}
   xs  c ROW-LOAD {: xt :}
   dys c ROW-LOAD {: dyt :}
   xt xt *. BLOCK-SUM UN PTX:U/  {: u :}      \ mean(x^2)
   u RMS-EPS+                    {: r2 :}     \ mean(x^2)+eps = r^2
   dyt xt *. BLOCK-SUM UN PTX:U/ {: sm :}     \ mean(dy*x) = S/n
   sm r2 PTX:U/                  {: coef :}   \ S/(n r^2)
   dyt  xt coef SCALE  -.  r2 USQRT PTX:B/    \ (dy - x*coef)/r
   dx r ROW-SPAN c ROW-STORE ;

\ ROPE-ROWS: pointwise pair rotation over a row (one head_dim vector per block),
\ adjacent lanes = adjacent head_dim pairs. cos/sin arrive pre-broadcast per row
\ (the table build is a separate op). x/cos/sin/out share extent-r/extent-c by
\ token, so one ctx `c` loads all three tiles at the SAME mask and ROPE-ROT proves
\ cos/sin match x's block+mask.
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

\ ROPE-ROWS-BWD: the VJP (rotation by -angle, maki/rope.f ROPE-BWD). dy is the
\ output cotangent; ROPE-ROT-BWD rotates it back into dx.
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

1024 %BLOCK
SMEM-BYTES 4096 T=

KERNEL: SUM-ROWS-1024 ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-1024
   {: in out :}                 \ typed-local-lint: allow-bare-local - parametric kernel type
   ROW            {: r :}       \ typed-local-lint: allow-bare-local - parametric kernel type
   in r ROW-SPAN  {: xs :}      \ typed-local-lint: allow-bare-local - parametric kernel type
   xs ROW-CTX     {: ctx :}     \ typed-local-lint: allow-bare-local - parametric kernel type
   xs ctx ROW-LOAD BLOCK-SUM BROADCAST
   out r ROW-SPAN ctx ROW-STORE ;

s" PTX-GOOD-MK-MATRIX ( ptr<space-global,f32> u32 u32 -- ) MK-MATRIX ROW ROW-SPAN drop" CHECK! -1 T=
s" PTX-GOOD-MK-MATRIX-ONCE ( ptr<space-global,f32> u32 u32 -- ) MK-MATRIX-ONCE ROW ROW-SPAN-ONCE drop" CHECK! -1 T=
s" PTX-GOOD-ROW-ONCE {: m :} ROW {: r :} m r ROW-SPAN-ONCE {: xs :} xs ROW-CTX-ONCE {: c :} xs c ROW-LOAD-ONCE xs c ROW-STORE-ONCE" CHECK! -1 T= \ typed-local-lint: allow-bare-local
s" PTX-BAD-ROW-ONCE-FROM-PLAIN ( tile<f32,block-256,mask-live> span<space-global,f32,extent-n> rowctx<block-256,extent-n,mask-live> -- ) ROW-STORE-ONCE" PTXC-CHECK-REJECTS
s" PTX-BAD-ROW-PLAIN-FROM-ONCE ( tile<f32,block-256,mask-live> span<space-global-once,f32,extent-n> rowctx<block-256,extent-n,mask-live> -- ) ROW-STORE" PTXC-CHECK-REJECTS
s" PTX-BAD-ROW-ONCE-CTX-FROM-PLAIN ( span<space-global,f32,extent-n> -- rowctx<block-256,extent-n,mask-live> ) ROW-CTX-ONCE" PTXC-CHECK-REJECTS

\ Fail-closed legality of the new RMSNorm/RoPE ops (red-first, per guard):
\ USQRT is a uniform op - a tile operand is REJECTED (never silently scaled);
\ ROPE-ROT needs THREE tiles of the SAME <element,block,mask> - a uniform for the
\ sin operand is REJECTED, so cos/sin cannot be a block-uniform forgery.
s" PTX-BAD-USQRT-ON-TILE ( tile<f32,block-256,mask-live> -- ) USQRT drop" PTXC-CHECK-REJECTS
s" PTX-BAD-ROPE-SIN-UNIFORM ( tile<f32,block-256,mask-live> tile<f32,block-256,mask-live> uniform<f32> -- ) ROPE-ROT drop" PTXC-CHECK-REJECTS

\ Clean load past this point is the positive proof: SOFTMAX-ROWS / RMSNORM-ROWS /
\ RMSNORM-ROWS-BWD / ROPE-ROWS / ROPE-ROWS-BWD certified.

T-REPORT

;package
