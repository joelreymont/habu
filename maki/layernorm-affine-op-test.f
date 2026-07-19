\ maki/layernorm-affine-op-test.f - affine LayerNorm through the model-op path
\ (dot habu-affine-layernorm-gamma, stage 2). Stage 1 (maki/layernorm-affine-test.f)
\ FD-checked the GOLDEN words directly; this proves the OP integration: MODEL: capture
\ of the arity-3 LAYERNORM (x, gamma, beta), the captured backward, and the host
\ executor. The op's backward is FD-verified END-TO-END by the host gradcheck (GC-RUN
\ builds the backward IR, runs forward+backward, and central-differences the seeded
\ loss w.r.t. EVERY input - dx AND dgamma AND dbeta), the adam-attn-grad-test.f part-C
\ oracle idiom lifted to the op path. Plus: the no-affine unary form still gradchecks
\ (backward-compat), the 1xC gamma/beta shape guard rejects a non-1xC param, and an
\ affine-LN Adam training run drives the loss down while gamma/beta receive gradients.

require lib/test.f
require lib/float.f
require lib/string.f
require maki/array.f
require maki/cad.f
require maki/gradcheck.f
require maki/loss-tensor.f
require maki/optim-tensor.f

\ ---- audited count projection (typed item-count -> raw compare cell) -------------
package CAD-NUM public
: LNAO-IC>N ( CAD-NUM:item-count -- n )  ITEM-COUNT>N ;
;package

package MAKI

\ ---- shape-guard fixture: EW-SHAPE-CHECK with the LAYERNORM broadcast class -------
\ Direct (catchable) probe of the arity-3 param check, the cad-test.f TRY-SCALE-BAD idiom:
\ a 1xC gamma passes, a 2xC gamma (not row-broadcast) throws E-CAD-PARAM-SHAPE.
: LNAO-GAMMA-BAD ( -- )                       \ gamma 2x4 vs x 2x4: not 1xC -> reject
   TENSOR:TV-RESET
   2 4 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC
   2 4 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC
   MAKI-OPKIND:LAYERNORM EW-SHAPE-CHECK ;
: LNAO-GAMMA-OK ( -- )                        \ gamma 1x4 vs x 2x4: legal row broadcast
   TENSOR:TV-RESET
   2 4 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC
   1 4 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC
   MAKI-OPKIND:LAYERNORM EW-SHAPE-CHECK ;

\ ---- Adam training fixture: 3 rows x 4 features, mean-MSE to a seeded target -------
3  constant LNAO-R    4  constant LNAO-C    12 constant LNAO-RC

create LNAO-X    LNAO-RC cells allot     \ input (slot 0)
create LNAO-G    LNAO-C  cells allot      \ gamma (slot 1, 1xC)
create LNAO-B    LNAO-C  cells allot      \ beta  (slot 2, 1xC)
create LNAO-TGT  LNAO-RC cells allot      \ regression target
create LNAO-SEED LNAO-RC cells allot      \ seed-cotangent buffer (bound to the seed slot)
create LNAO-XM LNAO-RC cells allot   create LNAO-XV LNAO-RC cells allot
create LNAO-GM LNAO-C  cells allot   create LNAO-GV LNAO-C  cells allot
create LNAO-BM LNAO-C  cells allot   create LNAO-BV LNAO-C  cells allot

\ deterministic fill (LCG mapped to [-0.5,0.5), no transcendentals)
variable LNAO-RNG
: LNAO-NEXT ( -- n )  LNAO-RNG @ 1103515245 * 12345 + dup LNAO-RNG ! ;
: LNAO-UNIT ( -- r )  LNAO-NEXT $FFFF and s>f 65536.0 f/ 0.5 f- ;
: LNAO-FILL ( ptr a n -- ) {: p:ptr len:n :}  len 0 ?do  LNAO-UNIT p i T-SET  loop ;

\ Adam bookkeeping (self-contained; step count folds into the running decay powers)
: LNAO-B1 ( -- r )  0.9 ;   : LNAO-B2 ( -- r )  0.999 ;
: LNAO-EPS ( -- r ) 0.00000001 ;   : LNAO-LR ( -- r )  0.05 ;
variable LNAO-B1T   variable LNAO-B2T
: LNAO-ADAM-RESET ( -- )  1.0 LNAO-B1T !  1.0 LNAO-B2T ! ;
: LNAO-TICK ( -- )  LNAO-B1T @ LNAO-B1 f* LNAO-B1T !  LNAO-B2T @ LNAO-B2 f* LNAO-B2T ! ;
: LNAO-C1 ( -- r )  1.0 LNAO-B1T @ f- ;   : LNAO-C2 ( -- r )  1.0 LNAO-B2T @ f- ;

: LNAO-INIT ( -- )
   $C0FFEE LNAO-RNG !
   LNAO-C 0 ?do  1.0 LNAO-UNIT 0.4 f* f+  LNAO-G i T-SET  loop   \ gamma in [0.8,1.2)
   LNAO-B   LNAO-C  LNAO-FILL
   LNAO-X   LNAO-RC LNAO-FILL
   LNAO-TGT LNAO-RC LNAO-FILL
   LNAO-ADAM-RESET
   0.0 LNAO-XM LNAO-RC T-FILL  0.0 LNAO-XV LNAO-RC T-FILL
   0.0 LNAO-GM LNAO-C  T-FILL  0.0 LNAO-GV LNAO-C  T-FILL
   0.0 LNAO-BM LNAO-C  T-FILL  0.0 LNAO-BV LNAO-C  T-FILL ;

: LNAO-OUT ( -- ptr a )  BW-FWD-N@ 1- MIR-NODE-ID EX-OUT@ ;   \ last forward node = y
: LNAO-INV-N ( -- r )  1.0 LNAO-RC s>f f/ ;
: LNAO-GRAD-AT ( n -- ptr a )  MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE EX-OUT@ ;

\ write the mean-scaled seed cotangent from the current output; return the mean MSE
: LNAO-LOSS-SEED ( -- r )
   LNAO-OUT {: ob:ptr :}
   ob LNAO-TGT LNAO-SEED LNAO-RC LOSS:TT-MSE-DY
   LNAO-RC 0 ?do  LNAO-SEED i T-GET LNAO-INV-N f*  LNAO-SEED i T-SET  loop
   ob LNAO-TGT LNAO-RC LOSS:TT-MSE  LNAO-INV-N f* ;

\ forward slice -> loss + seed -> whole IR (gradients land in the backward node buffers)
: LNAO-GRADS ( -- r )
   BW-FWD-N@ EX-RUN-N
   LNAO-LOSS-SEED {: loss:r :}
   EX-RUN
   loss ;

: LNAO-APPLY ( -- )
   LNAO-TICK
   LNAO-LR LNAO-B1 LNAO-B2 LNAO-EPS LNAO-C1 LNAO-C2  LNAO-X 0 LNAO-GRAD-AT LNAO-XM LNAO-XV LNAO-RC OPTIM:TT-ADAM!
   LNAO-LR LNAO-B1 LNAO-B2 LNAO-EPS LNAO-C1 LNAO-C2  LNAO-G 1 LNAO-GRAD-AT LNAO-GM LNAO-GV LNAO-C  OPTIM:TT-ADAM!
   LNAO-LR LNAO-B1 LNAO-B2 LNAO-EPS LNAO-C1 LNAO-C2  LNAO-B 2 LNAO-GRAD-AT LNAO-BM LNAO-BV LNAO-C  OPTIM:TT-ADAM! ;

\ Precondition: MODEL: LNAO-MODEL just captured. Build backward once, bind every buffer.
: LNAO-SETUP ( -- )
   LNAO-INIT
   BW-BUILD
   EX-RESET
   LNAO-X    0 MIR-SLOT-ID EX-BIND
   LNAO-G    1 MIR-SLOT-ID EX-BIND
   LNAO-B    2 MIR-SLOT-ID EX-BIND
   LNAO-SEED BW-SEED-SLOT@ EX-BIND ;

: LNAO-TRAIN ( n -- )  0 ?do  LNAO-GRADS drop  LNAO-APPLY  loop ;

\ 30 Adam steps from the current setup; final mean-MSE below the initial one
: LNAO-CONVERGES? ( -- bool )                 \ precondition: LNAO-SETUP already ran
   LNAO-GRADS {: il:r :}
   30 LNAO-TRAIN
   LNAO-GRADS {: fl:r :}
   fl il f< ;

\ catchable positive control for the shape guard (a legal 1xC gamma must NOT throw)
: LNAO-GAMMA-OK-THROWS ( -- n )  [: LNAO-GAMMA-OK ;] catch ;

T-RESET

\ ---- op-path FD proof: capture the arity-3 LAYERNORM and gradcheck all 3 inputs -----
MODEL: LNAO-GC ( x:3x4 g:1x4 b:1x4 -- y ) g b LAYERNORM ;
MODEL-K 1 T=                                              \ one op node
0 MIR-NODE-ID MIR-OP@ OPR-NAME s" layernorm" STR= TTRUE   \ same OP-LAYERNORM
0 MIR-NODE-ID MIR-IN-COUNT@ 3 T=                          \ arity 3 (x, gamma, beta)
GC-RUN V-PASS T=                                          \ dx AND dgamma AND dbeta match central FD
GC-RE$ s" host: 3 input(s) gradchecked" CONTAINS? TTRUE

\ ---- backward-compat: the no-affine unary form is unchanged and still gradchecks -----
MODEL: LNAO-UNARY ( x:4x8 -- y ) LAYERNORM ;
0 MIR-NODE-ID MIR-IN-COUNT@ 1 T=                          \ arity 1 (no-affine)
GC-RUN V-PASS T=

\ ---- 1xC gamma/beta shape guard: a non-1xC param is a named reject --------------------
' LNAO-GAMMA-BAD E-CAD-PARAM-SHAPE TTHROWS
LNAO-GAMMA-OK-THROWS 0 T=                                 \ a legal 1xC gamma does not throw

\ ---- affine-LN Adam training: gamma/beta receive gradients, the loss decreases --------
MODEL: LNAO-MODEL ( x:3x4 g:1x4 b:1x4 -- y ) g b LAYERNORM ;
LNAO-SETUP
0 MIR-SLOT-ID BW-HAS-GRAD? TTRUE                          \ x     receives a gradient
1 MIR-SLOT-ID BW-HAS-GRAD? TTRUE                          \ gamma receives a gradient
2 MIR-SLOT-ID BW-HAS-GRAD? TTRUE                          \ beta  receives a gradient
LNAO-CONVERGES? TTRUE                                     \ final mean-MSE < initial

T-REPORT

;package
