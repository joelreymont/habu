\ maki/layernorm-affine-test.f - central-FD gradient parity for affine LayerNorm
\ (dot habu-affine-layernorm-gamma). The no-affine LN already has a one-point host
\ gradcheck (maki/layernorm-test.f); this closes the affine params: for y =
\ gamma*xhat + beta it checks the golden VJP (LN-AFFINE-BWD: dgamma, dbeta, dx) at
\ the LIVE trained parameters across multiple Adam steps against an INDEPENDENT
\ central finite difference of the golden forward - the adam-attn-grad-test.f part C
\ oracle idiom lifted to the golden words (no executor: the op-DSL integration that
\ will bind these adjoints is the recorded follow-on; the golden lands first).
\
\ Fixture: 3 rows x 4 features (R>1 so dgamma/dbeta genuinely SUM over rows), mean
\ MSE against a seeded target. Per step: recompute the analytic gradients at the live
\ params, central-difference the SAME loss w.r.t. every gamma/beta/x element (h=1e-3),
\ assert rel-L2 < 1e-4 (the O(h^2) truncation floor of this smooth block is ~1e-6, so
\ ~100x margin while a wrong adjoint shows >=1e-2), then Adam-update and repeat on the
\ updated params. Plus a convergence leg (a right-signed gradient must lower the loss)
\ and a DETECTION fixture (a corrupted analytic gradient must exceed the tolerance, so
\ the parity is never vacuous). Load via maki/test.f.

require lib/test.f
require lib/float.f
require maki/array.f
require maki/layernorm.f
require maki/loss-tensor.f
require maki/optim-tensor.f

package MAKI

3  constant LNA-R           \ rows
4  constant LNA-C           \ features per row (gamma/beta length)
12 constant LNA-RC          \ LNA-R * LNA-C

create LNA-X    LNA-RC cells allot     \ inputs (R rows x C)
create LNA-Y    LNA-RC cells allot     \ forward output
create LNA-TGT  LNA-RC cells allot     \ regression target
create LNA-DY   LNA-RC cells allot     \ upstream cotangent dL/dy
create LNA-DX   LNA-RC cells allot     \ analytic dx (per-row)
create LNA-G    LNA-C  cells allot      \ gamma (1 x C, shared across rows)
create LNA-B    LNA-C  cells allot      \ beta  (1 x C, shared across rows)
create LNA-DG   LNA-C  cells allot      \ analytic dgamma (summed over rows)
create LNA-DB   LNA-C  cells allot      \ analytic dbeta  (summed over rows)
create LNA-XH   LNA-C  cells allot      \ LN-AFFINE-BWD row scratch (xhat / dxhat)

create LNA-GFD  LNA-C  cells allot       \ central-FD gradient buffers
create LNA-BFD  LNA-C  cells allot
create LNA-XFD  LNA-RC cells allot
create LNA-BAD  LNA-C  cells allot        \ corrupted-analytic scratch (detection)

\ Adam first/second-moment buffers per trained parameter
create LNA-GM LNA-C  cells allot   create LNA-GV LNA-C  cells allot
create LNA-BM LNA-C  cells allot   create LNA-BV LNA-C  cells allot
create LNA-XM LNA-RC cells allot   create LNA-XV LNA-RC cells allot

\ ---- deterministic fill (no transcendentals; a plain LCG mapped to [-0.5,0.5)) --
variable LNA-RNG
: LNA-SEED! ( n -- )  LNA-RNG ! ;
: LNA-NEXT ( -- n )  LNA-RNG @ 1103515245 * 12345 + dup LNA-RNG ! ;
: LNA-UNIT ( -- r )  LNA-NEXT $FFFF and s>f 65536.0 f/ 0.5 f- ;
: LNA-FILL ( ptr a n -- ) {: p:ptr len:n :}  len 0 ?do  LNA-UNIT p i T-SET  loop ;

\ ---- Adam bookkeeping (self-contained; step count + running decay powers) -------
: LNA-B1  ( -- r )  0.9 ;
: LNA-B2  ( -- r )  0.999 ;
: LNA-EPS ( -- r )  0.00000001 ;
: LNA-LR  ( -- r )  0.05 ;
variable LNA-B1T   variable LNA-B2T
: LNA-ADAM-RESET ( -- )  1.0 LNA-B1T !  1.0 LNA-B2T ! ;
: LNA-TICK ( -- )
   LNA-B1T @ LNA-B1 f*  LNA-B1T !
   LNA-B2T @ LNA-B2 f*  LNA-B2T ! ;
: LNA-C1 ( -- r )  1.0 LNA-B1T @ f- ;
: LNA-C2 ( -- r )  1.0 LNA-B2T @ f- ;

\ ---- fixture init: seeded params/target; gamma near 1, beta/x/target small -------
: LNA-INIT ( -- )
   $C0FFEE LNA-SEED!
   LNA-C 0 ?do  1.0 LNA-UNIT 0.4 f* f+  LNA-G i T-SET  loop   \ gamma in [0.8,1.2)
   LNA-B   LNA-C  LNA-FILL
   LNA-X   LNA-RC LNA-FILL
   LNA-TGT LNA-RC LNA-FILL
   LNA-ADAM-RESET
   0.0 LNA-GM LNA-C  T-FILL  0.0 LNA-GV LNA-C  T-FILL
   0.0 LNA-BM LNA-C  T-FILL  0.0 LNA-BV LNA-C  T-FILL
   0.0 LNA-XM LNA-RC T-FILL  0.0 LNA-XV LNA-RC T-FILL ;

\ ---- golden forward over all rows + mean-MSE loss --------------------------------
: LNA-ROW ( ptr a n -- ptr a )  LNA-C * cells + ;   \ row base of an R x C buffer
: LNA-INV-N ( -- r )  1.0 LNA-RC s>f f/ ;
: LNA-FWD! ( -- )
   LNA-R 0 ?do  LNA-X i LNA-ROW  LNA-Y i LNA-ROW  LNA-G  LNA-B  LNA-C  LN-AFFINE-FWD  loop ;
: LNA-LOSS ( -- r )  LNA-FWD!  LNA-Y LNA-TGT LNA-RC LOSS:TT-MSE  LNA-INV-N f* ;

\ ---- analytic gradients at the live params (dgamma/dbeta summed over rows) --------
: LNA-GRADS ( -- )
   LNA-FWD!
   LNA-Y LNA-TGT LNA-DY LNA-RC LOSS:TT-MSE-DY               \ dy = 2*(y-t)
   LNA-RC 0 ?do  LNA-DY i T-GET LNA-INV-N f*  LNA-DY i T-SET  loop   \ mean scale
   0.0 LNA-DG LNA-C T-FILL  0.0 LNA-DB LNA-C T-FILL
   LNA-R 0 ?do
      LNA-DY i LNA-ROW  LNA-X i LNA-ROW  LNA-G
      LNA-DX i LNA-ROW  LNA-DG  LNA-DB  LNA-XH  LNA-C  LN-AFFINE-BWD
   loop ;

\ ---- central finite difference of LNA-LOSS over a parameter buffer ---------------
: LNA-FD-H ( -- r )  0.001 ;
: LNA-FD! ( ptr a n ptr a -- ) {: pb:ptr len:n fdb:ptr :}
   len 0 ?do
      pb i T-GET {: base:r :}
      base LNA-FD-H f+  pb i T-SET  LNA-LOSS {: yp:r :}
      base LNA-FD-H f-  pb i T-SET  LNA-LOSS {: ym:r :}
      base pb i T-SET
      yp ym f-  LNA-FD-H 2.0 f* f/  fdb i T-SET
   loop ;

: LNA-TOL ( -- r )  0.0001 ;   \ 1e-4 FD-vs-analytic rel-L2 (~100x the FD floor)

\ one parameter: central FD vs the analytic gradient, rel-L2 < tol
: LNA-CHECK ( ptr a n ptr a ptr a -- ) {: pb:ptr len:n fdb:ptr anb:ptr :}
   pb len fdb LNA-FD!
   fdb anb len T-REL-L2  LNA-TOL f<  TTRUE ;

\ ---- Adam-update every trained parameter from its analytic gradient --------------
: LNA-APPLY ( -- )
   LNA-TICK
   LNA-LR LNA-B1 LNA-B2 LNA-EPS LNA-C1 LNA-C2  LNA-G LNA-DG LNA-GM LNA-GV LNA-C  OPTIM:TT-ADAM!
   LNA-LR LNA-B1 LNA-B2 LNA-EPS LNA-C1 LNA-C2  LNA-B LNA-DB LNA-BM LNA-BV LNA-C  OPTIM:TT-ADAM!
   LNA-LR LNA-B1 LNA-B2 LNA-EPS LNA-C1 LNA-C2  LNA-X LNA-DX LNA-XM LNA-XV LNA-RC OPTIM:TT-ADAM! ;

\ one FD-parity step: analytic grads, FD-check each parameter, then Adam apply
: LNA-STEP-FD ( -- )
   LNA-GRADS
   LNA-G LNA-C  LNA-GFD LNA-DG  LNA-CHECK
   LNA-B LNA-C  LNA-BFD LNA-DB  LNA-CHECK
   LNA-X LNA-RC LNA-XFD LNA-DX  LNA-CHECK
   LNA-APPLY ;

\ ---- detection: a corrupted analytic dgamma must exceed the tolerance ------------
: LNA-DETECT ( -- r )
   LNA-G LNA-C LNA-GFD LNA-FD!
   LNA-C 0 ?do  LNA-DG i T-GET  LNA-BAD i T-SET  loop
   LNA-BAD 0 T-GET 0.5 f+  LNA-BAD 0 T-SET
   LNA-GFD LNA-BAD LNA-C T-REL-L2 ;

\ ---- convergence: right-signed gradients lower the loss over an Adam run ----------
: LNA-TRAIN ( n -- )  0 ?do  LNA-GRADS LNA-APPLY  loop ;
: LNA-CONVERGES? ( -- bool )
   LNA-INIT  LNA-LOSS {: il:r :}  20 LNA-TRAIN  LNA-LOSS il f< ;

\ ---- fail-closed feature-length guard (control flow lives in a word) --------------
: LNA-FWD-DIM ( -- n )  [: LNA-X LNA-Y LNA-G LNA-B 0 LN-AFFINE-FWD ;] catch ;
: LNA-BWD-DIM ( -- n )  [: LNA-DY LNA-X LNA-G LNA-DX LNA-DG LNA-DB LNA-XH 0 LN-AFFINE-BWD ;] catch ;

T-RESET

\ ---- A: 3 Adam steps, full FD gradient parity per parameter (dgamma/dbeta/dx) -----
LNA-INIT
LNA-STEP-FD
LNA-STEP-FD
LNA-STEP-FD

\ ---- B: the affine params + x actually train (right-signed gradients lower loss) --
LNA-CONVERGES? TTRUE

\ ---- detection: a corrupted analytic gradient is caught (parity has teeth) --------
LNA-INIT
LNA-GRADS
LNA-DETECT LNA-TOL f< TFALSE

\ ---- fail-closed: a non-positive feature length throws E-LN-DIM -------------------
LNA-FWD-DIM E-LN-DIM T=
LNA-BWD-DIM E-LN-DIM T=

T-REPORT

;package
