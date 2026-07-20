\ maki/eval/train.f - the maki model train/eval leg (docs/maki/eval.md (ii)).
\
\ Grades the framework's authoring PROMISE - "author this, it trains" - by walking
\ a small model through the SAME public path every example takes, then locking the
\ result. A user authors the model with the MODEL: composition surface; the leg
\ builds its backward once (maki/backward.f BW-BUILD), runs it on host buffers
\ (maki/executor.f EX-RUN), and updates the parameters with the LIBRARY optimizer
\ (maki/optim-tensor.f OPTIM:TT-ADAM!) from a deterministic LCG init (the
\ maki/train-core.f init-role policy). The eval FAILS if the authored model stops
\ converging OR stops reproducing its committed loss bit-for-bit. Host-only;
\ maki -> habu only.
\
\ AUTHORING SURFACE EXERCISED (grows with the grammar - a new line, not a new harness):
\   MODEL: ET-NET ( x w bw g be -- y ) LINEAR GELU x RESIDUAL-ADD g be LAYERNORM ;
\     LINEAR         - matmul + a 1xC broadcast bias (the landed suffix-broadcast bias add)
\     GELU           - an activation
\     x RESIDUAL-ADD - an elementwise (same-shape) add + a real skip (x fans out)
\     g be LAYERNORM - the EXPLICIT affine LayerNorm (gamma/beta named references)
\   plus the MSE loss (maki/loss-tensor.f) and BOTH opt-in trainer arming words -
\   the per-step LR schedule (ET-LR) and the global-norm gradient clip (ET-CLIP) -
\   armed for the whole run (this leg is where the armed path is exercised continuously).
\
\ HONEST CONSTRAINTS + long-term targets (interim state labeled per the prime directive):
\  - NO SPEC: equation line. A new SPEC: einsum needs TENSOR: rows, and the shared
\    cold image is near TR-CAP (maki/extent-tensor.f, cap 64). The registered
\    equations A-SCORES/A-CTX (maki/attn-eq.f) ARE reusable as composable equation
\    ops with no new rows, but composing+training them needs the attention-sublayer
\    wiring whose equation-composition training path is not on the committed floor
\    (the committed attention trainer uses plain MATMUL). So the model is authored
\    from the op vocabulary now; a SPEC:-equation line (canonical prefix-Σ infix-·)
\    joins when TR-CAP lands or that path is committed.
\  - The armed schedule + clip are eval-leg-local (ET-LR / ET-CLIP over library
\    math), because the framework's generic LR-SCHED (cosine) + GRAD-CLIP facilities
\    and the host Adam step/bias-correction bookkeeping still live in the nanoGPT
\    example (maki/examples/nanogpt/from-scratch-train.f + adam-train.f), not the
\    library. LONG-TERM: decouple those generic facilities into maki/train-core.f -
\    mirroring the init-role/AdamW-policy decouple already there - after which this
\    leg grades the EXACT framework arming words instead of leg-local equivalents.
\
\ RETIREMENT. This landing REPLACES the two hand-wired reference regressions (the
\ scalar y=w*x and per-element tensor y[i]=w[i]*x[i] library SGD trainers - the
\ interim floor). Their convergence coverage is not lost: maki/train-test.f locks
\ both (TRAIN-N -> w=3 / loss->0 ; T-TRAIN-N! -> loss->0), the proper home for the
\ library-trainer unit regressions. The concrete Adam MLP + attention trainers and
\ their per-step loss locks stay in the nanoGPT example (adam-train-test.f).

require lib/test.f
require lib/float.f
require lib/fmt.f
require maki/cad.f            \ MODEL: capture
require maki/backward.f       \ BW-BUILD / BW-FWD-N@ / BW-SEED-SLOT@
require maki/executor.f       \ EX-RESET / EX-BIND / EX-RUN(-N) / EX-OUT@
require maki/train-core.f     \ SC-SEED! / SC-UNIT / INIT-FILL / SC-GRAD-AT
require maki/optim-tensor.f   \ OPTIM:TT-ADAM!
require maki/loss-tensor.f    \ LOSS:TT-MSE / LOSS:TT-MSE-DY
require maki/array.f          \ T-GET / T-SET / T-FILL / T-NORM2

package MAKI

\ ---- committed hyperparameters (the convergence + lock gates pin these) --------
60  constant ET-N            \ training steps (the model converges well within them)
5   constant ET-WARM         \ LR-schedule warmup steps
0.02  constant ET-LMAX       \ schedule peak LR (post-warmup)
0.002 constant ET-LMIN       \ schedule floor LR (end of decay)
1.0   constant ET-CLIPV      \ global-norm gradient clip threshold
$ABCDEF constant ET-LCG      \ committed LCG seed (params + data stream)
0.9   constant ET-BETA1      \ Adam first-moment decay
0.999 constant ET-BETA2      \ Adam second-moment decay
0.00000001 constant ET-EPS   \ Adam denominator epsilon (1e-8)
112759 constant ET-LOCK      \ committed final loss in micro-units (deterministic)

\ ---- operand extents: x,y = 4x8 ; w = 8x8 ; bw,g,be = 1x8 ---------------------
32 constant ET-XN            \ 4x8 input / output / target elements
64 constant ET-WN            \ 8x8 weight elements
8  constant ET-VN            \ 1x8 bias / gamma / beta elements

create ET-X  ET-XN cells allot     \ model input (data, not trained)
create ET-W  ET-WN cells allot     \ LINEAR weight
create ET-BW ET-VN cells allot     \ LINEAR bias (1x8, row broadcast)
create ET-GA ET-VN cells allot     \ LayerNorm gamma
create ET-BE ET-VN cells allot     \ LayerNorm beta
create ET-TGT ET-XN cells allot    \ regression target
create ET-DY  ET-XN cells allot    \ seed cotangent buffer (dL/dy)

\ per-parameter Adam moment buffers (first / second)
create ET-WM  ET-WN cells allot   create ET-WV  ET-WN cells allot
create ET-BWM ET-VN cells allot   create ET-BWV ET-VN cells allot
create ET-GAM ET-VN cells allot   create ET-GAV ET-VN cells allot
create ET-BEM ET-VN cells allot   create ET-BEV ET-VN cells allot

variable ET-STEPCT           \ Adam step count t
variable ET-B1P              \ running beta1^t
variable ET-B2P              \ running beta2^t
variable ET-IL  variable ET-FL   \ this run's initial / final loss
variable ET-F1  variable ET-F2   \ run-1 / run-2 final loss (determinism compare)

\ ---- Adam bias-correction bookkeeping ----------------------------------------
: ET-TICK ( -- )             \ advance t and the running decay powers (before an apply)
   ET-STEPCT @ 1+ ET-STEPCT !
   ET-B1P @ ET-BETA1 f* ET-B1P !
   ET-B2P @ ET-BETA2 f* ET-B2P ! ;
: ET-C1 ( -- r )  1.0 ET-B1P @ f- ;   \ 1 - beta1^t
: ET-C2 ( -- r )  1.0 ET-B2P @ f- ;   \ 1 - beta2^t

: ET-Z ( ptr a n -- )  0.0 -rot T-FILL ;

\ ---- deterministic init: data via the LCG, params via the init-role policy ----
: ET-INIT ( -- )
   ET-LCG SC-SEED!
   ET-XN 0 ?do  SC-UNIT ET-X i T-SET  loop        \ input rows ~ U[-1,1)
   ET-W  ET-WN 1 IR-NORMAL   INIT-FILL            \ weight ~ N(0, 0.02^2)
   ET-BW ET-VN 1 IR-BIAS     INIT-FILL            \ bias  = 0
   ET-GA ET-VN 1 IR-LN-GAMMA INIT-FILL            \ gamma = 1
   ET-BE ET-VN 1 IR-LN-BETA  INIT-FILL            \ beta  = 0
   ET-XN 0 ?do  SC-UNIT ET-TGT i T-SET  loop      \ target rows ~ U[-1,1)
   ET-WM  ET-WN ET-Z  ET-WV  ET-WN ET-Z
   ET-BWM ET-VN ET-Z  ET-BWV ET-VN ET-Z
   ET-GAM ET-VN ET-Z  ET-GAV ET-VN ET-Z
   ET-BEM ET-VN ET-Z  ET-BEV ET-VN ET-Z
   0 ET-STEPCT !  1.0 ET-B1P !  1.0 ET-B2P ! ;

\ bind every model-input slot + the seed slot to its host buffer (capture order)
: ET-BIND ( -- )
   EX-RESET
   ET-X  0 SC-SLOT EX-BIND   ET-W  1 SC-SLOT EX-BIND   ET-BW 2 SC-SLOT EX-BIND
   ET-GA 3 SC-SLOT EX-BIND   ET-BE 4 SC-SLOT EX-BIND
   ET-DY BW-SEED-SLOT@ EX-BIND ;

: ET-OUT ( -- ptr a )  BW-FWD-N@ 1- MIR-NODE-ID EX-OUT@ ;   \ last forward node output
: ET-INVN ( -- r )  1.0 ET-XN s>f f/ ;

\ mean MSE at the current params + its mean-scaled seed cotangents (2*(y-t)/N)
: ET-LOSS-SEED ( -- r )
   ET-OUT {: ob:ptr :}
   ob ET-TGT ET-DY ET-XN LOSS:TT-MSE-DY
   ET-XN 0 ?do  ET-DY i T-GET ET-INVN f*  ET-DY i T-SET  loop
   ob ET-TGT ET-XN LOSS:TT-MSE ET-INVN f* ;

\ ---- armed LR schedule: linear warmup to LMAX, then linear decay to LMIN -------
\ Read at the current 1-based step (ET-STEPCT after ET-TICK), so step s is 0-based.
: ET-LR ( -- r )
   ET-STEPCT @ 1- {: s:n :}
   s ET-WARM < if  ET-LMAX  s 1+ s>f f*  ET-WARM 1+ s>f f/  exit then
   ET-LMAX  ET-LMAX ET-LMIN f-  s ET-WARM - s>f f*  ET-N 1- ET-WARM - s>f f/  f- ;

\ ---- armed global-norm gradient clip over the four trained-param grad buffers --
: ET-GNORM2 ( -- r )
   1 SC-GRAD-AT ET-WN T-NORM2
   2 SC-GRAD-AT ET-VN T-NORM2 f+
   3 SC-GRAD-AT ET-VN T-NORM2 f+
   4 SC-GRAD-AT ET-VN T-NORM2 f+ ;
: ET-GSCALE ( r ptr a n -- ) {: c:r p:ptr len:n :}
   len 0 ?do  p i T-GET c f*  p i T-SET  loop ;
\ coef = clip / (norm + 1e-6); rescale every grad only when coef < 1 (nanoGPT's rule)
: ET-CLIP ( -- )
   ET-GNORM2 fsqrt 0.000001 f+  ET-CLIPV swap f/  {: coef:r :}
   coef 1.0 f< if
      coef 1 SC-GRAD-AT ET-WN ET-GSCALE
      coef 2 SC-GRAD-AT ET-VN ET-GSCALE
      coef 3 SC-GRAD-AT ET-VN ET-GSCALE
      coef 4 SC-GRAD-AT ET-VN ET-GSCALE
   then ;

\ Adam-update one bound parameter in place from its slot's backward gradient node
: ET-UPD ( r n ptr a ptr a ptr a n -- ) {: lr:r slot:n wp:ptr mp:ptr vp:ptr len:n :}
   lr ET-BETA1 ET-BETA2 ET-EPS ET-C1 ET-C2
   wp  slot SC-GRAD-AT  mp vp len  OPTIM:TT-ADAM! ;

\ one training step: forward -> loss + seed -> full IR -> clip -> scheduled Adam.
\ Returns the pre-update mean MSE.
: ET-STEP ( -- r )
   BW-FWD-N@ EX-RUN-N
   ET-LOSS-SEED {: loss:r :}
   EX-RUN
   ET-CLIP  ET-TICK  ET-LR {: lr:r :}
   lr 1 ET-W  ET-WM  ET-WV  ET-WN ET-UPD
   lr 2 ET-BW ET-BWM ET-BWV ET-VN ET-UPD
   lr 3 ET-GA ET-GAM ET-GAV ET-VN ET-UPD
   lr 4 ET-BE ET-BEM ET-BEV ET-VN ET-UPD
   loss ;

\ a fresh run from the just-captured MODEL:: init, build backward once, bind, then
\ ET-N steps; records the initial + final loss. Precondition: MODEL: ET-NET captured.
: ET-RUN ( -- )
   ET-INIT  BW-BUILD  ET-BIND
   ET-N 0 ?do
      ET-STEP {: l:r :}
      i 0=        if l ET-IL ! then
      i ET-N 1- = if l ET-FL ! then
   loop ;

: ET-MICRO ( r -- n )  1000000 s>f f* 0.5 f+ f>s ;   \ loss -> micro-units (round half up)

\ ---- the train/eval report --------------------------------------------------
: ET-YESNO ( bool -- )
   if s" yes" type exit then  s" no" type ;
: ET-INT ( n -- )  SB-RESET FMT:SB-INT SB$ type ;
: ET-CELL ( n -- )  s"  | " type ET-INT ;
: ET-HEADER ( -- )
   s" | model | steps | loss-initial-micro | loss-final-micro | converged |" type cr ;
: ET-ROW ( ptr u8 n n n n bool -- ) {: steps:n init:n final:n ok:bool :}
   s" | " type type
   steps ET-CELL  init ET-CELL  final ET-CELL
   s"  | " type ok ET-YESNO s"  |" type cr ;

T-RESET

\ author the model as a user would, train, capture the result (run 1) ...
MODEL: ET-NET ( x:4x8 w:8x8 bw:1x8 g:1x8 be:1x8 -- y ) LINEAR GELU x RESIDUAL-ADD g be LAYERNORM ;
ET-RUN
ET-FL @ ET-F1 !

\ ... then re-author + retrain from scratch (run 2) to prove bit-identical determinism.
MODEL: ET-NET ( x:4x8 w:8x8 bw:1x8 g:1x8 be:1x8 -- y ) LINEAR GELU x RESIDUAL-ADD g be LAYERNORM ;
ET-RUN
ET-FL @ ET-F2 !

\ --- the authored model trains, deterministically, to its committed loss --------
ET-FL @ ET-IL @ f<               TTRUE   \ loss strictly falls (final < initial)
ET-FL @ ET-IL @ 0.5 f* f<        TTRUE   \ loss at least halved
ET-F1 @ ET-F2 @ f=               TTRUE   \ run-twice bit-identical (exact f64 equality)
ET-FL @ ET-MICRO  ET-LOCK        T=      \ deterministic exact lock (micro-units)

ET-HEADER
s" authored-net"  ET-N  ET-IL @ ET-MICRO  ET-FL @ ET-MICRO  ET-FL @ ET-IL @ 0.5 f* f<  ET-ROW

T-REPORT

;package
