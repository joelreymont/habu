\ ad-dag-eval.f - HOST numeric evaluator for the reverse-mode AD DAG semantics.
\
\ One concern: interpret the value-numbered DAG (lib/ptx/ad-dag.f) as NUMBERS on
\ the CPU host - NO PTX emission. It models one row of W lanes as host floats and
\ mirrors the tile/collective dialect EXACTLY: a block-uniform value (BLOCK-MAX /
\ BLOCK-SUM output) is stored REPLICATED across all W lanes (the device holds the
\ reduced scalar in every lane after the smem broadcast), so B-/B/ are plain
\ elementwise row ops against that replicated uniform, BROADCAST is the identity,
\ EXP/NEG are per-lane, and MUL/ADD are row-row. The reverse pass runs the SAME
\ VJP rules as AD-VJP with +. accumulation at fan-out, DRIVEN OFF THE SAME BUILT
\ TABLES (AD-OP/AD-A/AD-B/AD-N/AD-OUT) so the routing gradchecked here is the
\ routing that emits PTX. Load after lib/ptx/ad-dag.f; FEXP is the shared exp core
\ from lib/fmath.f. Checked Habu. Test support (host gradcheck) - not on any device
\ path.

require lib/ptx/ad-dag.f
require lib/fmath.f

8 constant ADE-MAXW                          \ modeled lanes per row (small, > any test W)
variable ADE-W                               \ active lane count (1..ADE-MAXW)

AD-MAXN ADE-MAXW * constant ADE-CELLS
create ADE-FBUF ADE-CELLS cells allot        \ per-node,per-lane forward value
create ADE-CBUF ADE-CELLS cells allot        \ per-node,per-lane accumulated cotangent

\ --- fail-closed range guards (mirror ad-dag.f AD-SLOT-CHECK discipline) ---
: ADE-W-CHECK ( -- )
   ADE-W @ 1 < if E-PTX-AD-OVERFLOW throw then
   ADE-W @ ADE-MAXW > if E-PTX-AD-OVERFLOW throw then ;

: ADE-LANE-CHECK ( n -- )
   dup 0 < if E-PTX-AD-OVERFLOW throw then
   dup ADE-MAXW >= if E-PTX-AD-OVERFLOW throw then
   drop ;

\ --- typed cell addressing (node id checked by ad-dag.f AD-ID-CHECK) ---
: ADE-CELL ( ptr a n n -- ptr a ) {: base:ptr nd:n l:n :}
   nd AD-ID-CHECK  l ADE-LANE-CHECK
   base nd ADE-MAXW * l + cells + ;

: ADE-F@ ( n n -- r ) {: nd:n l:n :}  ADE-FBUF nd l ADE-CELL @ ;
: ADE-F! ( r n n -- ) {: nd:n l:n :}  ADE-FBUF nd l ADE-CELL ! ;
: ADE-C@ ( n n -- r ) {: nd:n l:n :}  ADE-CBUF nd l ADE-CELL @ ;
: ADE-C! ( r n n -- ) {: nd:n l:n :}  ADE-CBUF nd l ADE-CELL ! ;

\ external row buffers (W host floats) supplied by the caller
: ADE-ROW@ ( ptr a n -- r )  cells + @ ;
: ADE-ROW! ( r ptr a n -- )  cells + ! ;

\ --- reductions over the ACTIVE lanes (device: smem fold + broadcast) ---
: ADE-FMAX ( r r -- r )  2dup f< if nip else drop then ;

: ADE-RMAX ( n -- r ) {: nd:n :}                 \ max over A's lanes
   nd 0 ADE-F@  ADE-W @ 1 ?do  nd i ADE-F@ ADE-FMAX  loop ;

: ADE-RSUM ( n -- r ) {: nd:n :}                 \ sum over A's lanes
   0.0  ADE-W @ 0 ?do  nd i ADE-F@ f+  loop ;

: ADE-WRITE-UNIFORM ( r n -- ) {: v:r nd:n :}    \ replicate a scalar to every lane
   ADE-W @ 0 ?do  v nd i ADE-F!  loop ;

\ --- forward per-op (writes node nd's whole row from its input rows) ---
: ADE-FWD-BMAX ( n -- ) {: nd:n :}               \ uniform max(A)
   nd AD-A@ ADE-RMAX  nd ADE-WRITE-UNIFORM ;
: ADE-FWD-BSUM ( n -- ) {: nd:n :}               \ uniform sum(A)
   nd AD-A@ ADE-RSUM  nd ADE-WRITE-UNIFORM ;
: ADE-FWD-EXP ( n -- ) {: nd:n :}                \ per-lane exp
   ADE-W @ 0 ?do  nd AD-A@ i ADE-F@ FEXP  nd i ADE-F!  loop ;
: ADE-FWD-BSUB ( n -- ) {: nd:n :}               \ A - B (B uniform, replicated)
   ADE-W @ 0 ?do  nd AD-A@ i ADE-F@  nd AD-B@ i ADE-F@ f-  nd i ADE-F!  loop ;
: ADE-FWD-BDIV ( n -- ) {: nd:n :}               \ A / B (B uniform, replicated)
   ADE-W @ 0 ?do  nd AD-A@ i ADE-F@  nd AD-B@ i ADE-F@ f/  nd i ADE-F!  loop ;
: ADE-FWD-MUL ( n -- ) {: nd:n :}                \ A * B (row * row)
   ADE-W @ 0 ?do  nd AD-A@ i ADE-F@  nd AD-B@ i ADE-F@ f*  nd i ADE-F!  loop ;
: ADE-FWD-ADD ( n -- ) {: nd:n :}                \ A + B (row + row)
   ADE-W @ 0 ?do  nd AD-A@ i ADE-F@  nd AD-B@ i ADE-F@ f+  nd i ADE-F!  loop ;

: ADE-FWD-NODE ( n -- ) {: nd:n :}
   nd AD-OP@
   case
      OP-BMAX of nd ADE-FWD-BMAX endof
      OP-BSUB of nd ADE-FWD-BSUB endof
      OP-EXP  of nd ADE-FWD-EXP  endof
      OP-BSUM of nd ADE-FWD-BSUM endof
      OP-BDIV of nd ADE-FWD-BDIV endof
      OP-MUL  of nd ADE-FWD-MUL  endof
      OP-ADD  of nd ADE-FWD-ADD  endof
      drop E-PTX-AD-UNKNOWN throw
   endcase ;

: ADE-LOAD-X ( ptr a -- ) {: x:ptr :}            \ node 0 forward = the input row
   ADE-W @ 0 ?do  x i ADE-ROW@  0 i ADE-F!  loop ;

: ADE-FWD-RUN ( -- )                             \ recompute nodes 1..N-1 (node 0 given)
   AD-N @ 1 ?do  i ADE-FWD-NODE  loop ;

\ --- reverse: +. accumulation at fan-out, same VJP rules as AD-VJP ---
: ADE-ACC ( r n n -- ) {: v:r nd:n l:n :}        \ CT[nd,l] += v
   v  nd l ADE-C@  f+  nd l ADE-C! ;

: ADE-CT-SUM ( n -- r ) {: nd:n :}               \ Sum_l ct[l]
   0.0  ADE-W @ 0 ?do  nd i ADE-C@ f+  loop ;
: ADE-CTY-SUM ( n -- r ) {: nd:n :}              \ Sum_l ct[l]*y[l] (y = node forward)
   0.0  ADE-W @ 0 ?do  nd i ADE-C@  nd i ADE-F@  f*  f+  loop ;

\ BLOCK-MAX-SELECT: lowest lane where A == max (device routes ct to that lane)
: ADE-BMAX-ARGMAX ( n -- n ) {: nd:n :}
   nd 0 ADE-F@ {: mx:r :}
   ADE-W @ 0 ?do
      nd AD-A@ i ADE-F@ mx f= if  i unloop exit  then
   loop
   E-PTX-AD-UNKNOWN throw ;                       \ max always equals a lane; unreachable

\ BLOCK-MAX: dx = ct at the arg-max lane, 0 elsewhere (others stay at their +. zero)
: ADE-VJP-BMAX ( n -- ) {: nd:n :}
   nd ADE-BMAX-ARGMAX {: am:n :}
   nd am ADE-C@  nd AD-A@ am ADE-ACC ;
\ B- (A - B): da = ct ; db = -Sum(ct) (uniform, replicated to every lane of B)
: ADE-VJP-BSUB ( n -- ) {: nd:n :}
   nd ADE-CT-SUM fnegate {: s:r :}
   ADE-W @ 0 ?do  nd i ADE-C@  nd AD-A@ i ADE-ACC  loop
   ADE-W @ 0 ?do  s  nd AD-B@ i ADE-ACC  loop ;
\ EXP (e = exp(A)): da = ct * e
: ADE-VJP-EXP ( n -- ) {: nd:n :}
   ADE-W @ 0 ?do  nd i ADE-C@  nd i ADE-F@  f*  nd AD-A@ i ADE-ACC  loop ;
\ BLOCK-SUM: da = BROADCAST(ct) -> ct replicated per lane (ct already uniform)
: ADE-VJP-BSUM ( n -- ) {: nd:n :}
   ADE-W @ 0 ?do  nd i ADE-C@  nd AD-A@ i ADE-ACC  loop ;
\ B/ (y = A/B): da = ct/B ; db = -Sum(ct*y)/B (uniform, replicated to B's lanes)
: ADE-VJP-BDIV ( n -- ) {: nd:n :}
   nd AD-B@ 0 ADE-F@ {: b:r :}
   nd ADE-CTY-SUM  b f/  fnegate {: db:r :}
   ADE-W @ 0 ?do  nd i ADE-C@  nd AD-B@ i ADE-F@ f/  nd AD-A@ i ADE-ACC  loop
   ADE-W @ 0 ?do  db  nd AD-B@ i ADE-ACC  loop ;
\ *. (y = A*B): da = ct*B ; db = ct*A (both rows)
: ADE-VJP-MUL ( n -- ) {: nd:n :}
   ADE-W @ 0 ?do  nd i ADE-C@  nd AD-B@ i ADE-F@  f*  nd AD-A@ i ADE-ACC  loop
   ADE-W @ 0 ?do  nd i ADE-C@  nd AD-A@ i ADE-F@  f*  nd AD-B@ i ADE-ACC  loop ;
\ +. (y = A+B): da = ct ; db = ct (both rows)
: ADE-VJP-ADD ( n -- ) {: nd:n :}
   ADE-W @ 0 ?do  nd i ADE-C@  nd AD-A@ i ADE-ACC  loop
   ADE-W @ 0 ?do  nd i ADE-C@  nd AD-B@ i ADE-ACC  loop ;

: ADE-VJP ( n -- ) {: nd:n :}
   nd AD-OP@
   case
      OP-BMAX of nd ADE-VJP-BMAX endof
      OP-BSUB of nd ADE-VJP-BSUB endof
      OP-EXP  of nd ADE-VJP-EXP  endof
      OP-BSUM of nd ADE-VJP-BSUM endof
      OP-BDIV of nd ADE-VJP-BDIV endof
      OP-MUL  of nd ADE-VJP-MUL  endof
      OP-ADD  of nd ADE-VJP-ADD  endof
      drop E-PTX-AD-UNKNOWN throw
   endcase ;

: ADE-CT-ZERO ( -- )                             \ clear every cotangent cell
   ADE-CELLS 0 ?do  0.0  ADE-CBUF i cells + !  loop ;
: ADE-SEED ( ptr a -- ) {: dy:ptr :}             \ output cotangent = dy
   AD-OUT @ {: o:n :}
   ADE-W @ 0 ?do  dy i ADE-ROW@  o i ADE-C!  loop ;
: ADE-REV-RUN ( -- )                             \ VJP nodes high..1 (leaf 0 has none)
   AD-N @ 1 ?do  AD-N @ i -  ADE-VJP  loop ;

\ --- public entries: run over the currently built DAG (AD-BUILD) at width ADE-W ---
: ADE-FWD ( ptr a ptr a -- ) {: x:ptr y:ptr :}   \ x row -> y = output row
   ADE-W-CHECK
   x ADE-LOAD-X
   ADE-FWD-RUN
   AD-OUT @ {: o:n :}
   ADE-W @ 0 ?do  o i ADE-F@  y i ADE-ROW!  loop ;

: ADE-GRAD ( ptr a ptr a ptr a -- ) {: x:ptr dy:ptr dx:ptr :}   \ x,dy -> dx
   ADE-W-CHECK
   x ADE-LOAD-X
   ADE-FWD-RUN
   ADE-CT-ZERO
   dy ADE-SEED
   ADE-REV-RUN
   ADE-W @ 0 ?do  0 i ADE-C@  dx i ADE-ROW!  loop ;
