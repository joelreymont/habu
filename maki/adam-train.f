\ maki/adam-train.f - Adam-driven host training steps over the model IR (dots
\ habu-maki-training-loop + habu-autograd-end-to: Wave-3 host scope).
\
\ Composes the LANDED pieces into Adam training: BW-BUILD (maki/backward.f)
\ builds forward+backward ONCE into the shared IR; EX-BIND / EX-RUN(-N) /
\ EX-OUT@ (maki/executor.f) execute it on host buffers; OPTIM:TT-ADAM!
\ (maki/optim-tensor.f) applies the update, with the per-step bias corrections
\ bc1 = 1-b1^t / bc2 = 1-b2^t tracked here as running powers. Two committed
\ trainers share that machinery:
\   AMT-*  the from-scratch windowed MLP (maki/from-scratch-model.f) under Adam
\          instead of plain SGD - same seeds, same batch Gaussian NLL loss and
\          seed cotangents (SC-LOSS-SEED), Adam parameter updates;
\   ATN-*  a 1-layer attention block O = softmax(Q@Kt*s)@V (the gradchecked
\          GC-ATTN shape; K pre-transposed because the single-running-value
\          MODEL: DSL cannot express an internal Q@K^T) trained end-to-end
\          against a committed seeded target under mean-MSE loss; all four
\          inputs (Q, Kt, s, V) are trained parameters.
\
\ Step execution order follows maki/from-scratch-train.f exactly: (1) run the
\ forward node slice; (2) read the output, record the loss, write the
\ mean-scaled seed cotangents; (3) run the WHOLE IR (its deterministic forward
\ recompute + the backward region reading the seed); (4) apply Adam to every
\ parameter from its backward node buffer. Rematerialization here is that full
\ deterministic forward recompute; the segment-boundary checkpointing mode
\ (docs/maki/train.md "Gradient checkpointing", maki/checkpoint.f) is the
\ bit-identical lower-memory alternative to steps (1)/(3).
\
\ Preconditions mirror the from-scratch trainer: AMT-SETUP needs a fresh
\ MODEL: SCRATCH-MLP capture, ATN-SETUP a fresh MODEL: ADAM-ATTN capture
\ (MODEL: is a top-level parsing word; each consumer re-issues the exact line).
\ maki -> habu only; adam-train defines E-ADAM-RUN (-5144), E-ADAMW-POLICY (-5157),
\ E-INIT-ROLE (-5158).

require maki/from-scratch-train.f
require maki/optim-tensor.f
require maki/loss-tensor.f
require maki/array.f

-5144 constant E-ADAM-RUN   \ a trainer accessor was used before its training run
-5157 constant E-ADAMW-POLICY   \ a parameter's decay attribute is not a valid WD flag

package MAKI
public

\ ---- Adam hyperparameters (committed; the convergence gates pin outcomes) ----
: ADAM-B1  ( -- r )  0.9 ;          \ first-moment decay
: ADAM-B2  ( -- r )  0.999 ;        \ second-moment decay
: ADAM-EPS ( -- r )  0.00000001 ;   \ denominator epsilon (1e-8)

\ ---- AdamW decoupled-decay param-group policy -------------------------------
\ Every trained parameter carries an EXPLICIT decay attribute set where the
\ trainer registers its slots (below): 2D weight matrices decay (WD-DECAY);
\ biases and LayerNorm gamma/beta do not (WD-NONE) - the standard GPT-2/nanoGPT
\ grouping. The trainer owns the decay coefficient; the per-parameter flag only
\ GATES it. ADAMW-WD-FOR is the checked resolver: an attribute outside the two
\ named flags is a registration bug, not a silent default -> throw.
0  constant WD-NONE      \ excluded from weight decay (bias, LN gamma/beta)
-1 constant WD-DECAY     \ decoupled decay applies (2D weight matrix)

: ADAMW-WD-FOR ( r n -- r ) {: wd:r flag:n :}
   flag WD-DECAY = if wd exit then
   flag WD-NONE  = if 0.0 exit then
   E-ADAMW-POLICY throw ;

\ ---- per-parameter-role init policy (GPT-2 / nanoGPT initialisation) ---------
\ Every parameter carries an EXPLICIT init role, mirroring the WD flags above:
\ the fill is chosen by role, never by sniffing a buffer name. Weight matrices
\ draw normal(0, 0.02); residual-projection weights draw
\ normal(0, 0.02/sqrt(2*n_layer)) (nanoGPT's per-block output-projection scaling,
\ n_layer supplied at fill time); LayerNorm gamma=1, LayerNorm beta=0 and
\ biases=0 are constant fills. INIT-FILL is the checked applicator - a role
\ outside the five named flags is a registration bug, not a silent default ->
\ throw. NEW surface: no existing fill is rewired here, so the committed AMT/ATN
\ trajectories keep their uniform init.
-5158 constant E-INIT-ROLE   \ a parameter's init role is not a valid init policy flag

1 constant IR-NORMAL         \ weight ~ normal(0, 0.02)
2 constant IR-RESID          \ residual-projection weight ~ normal(0, 0.02/sqrt(2*n_layer))
3 constant IR-LN-GAMMA       \ LayerNorm scale = 1
4 constant IR-LN-BETA        \ LayerNorm shift = 0
5 constant IR-BIAS           \ bias = 0

: INIT-STD ( -- r )  0.02 ;                       \ GPT-2 base weight standard deviation

: INIT-RESID-STD ( n -- r ) {: nlayer:n :}        \ 0.02 / sqrt(2 * n_layer)
   INIT-STD  2 nlayer * s>f fsqrt  f/ ;

: INIT-GFILL ( ptr a n r -- ) {: base:ptr len:n std:r :}   \ fill base with N(0,std^2)
   len 0 ?do  SC-GAUSS std f*  base i T-SET  loop ;

: INIT-FILL ( ptr a n n n -- ) {: base:ptr len:n nlayer:n role:n :}
   role IR-NORMAL   = if  base len INIT-STD              INIT-GFILL  exit then
   role IR-RESID    = if  base len nlayer INIT-RESID-STD INIT-GFILL  exit then
   role IR-LN-GAMMA = if  1.0 base len T-FILL  exit then
   role IR-LN-BETA  = if  0.0 base len T-FILL  exit then
   role IR-BIAS     = if  0.0 base len T-FILL  exit then
   E-INIT-ROLE throw ;

private

\ ---- shared per-run Adam state (step count + running decay powers) ----------
variable ADAM-T             \ optimizer step count t
variable ADAM-B1T           \ running b1^t
variable ADAM-B2T           \ running b2^t

: ADAM-RESET ( -- )  0 ADAM-T !  1.0 ADAM-B1T !  1.0 ADAM-B2T ! ;

: ADAM-TICK ( -- )          \ advance t and the decay powers (before an apply)
   ADAM-T @ 1+ ADAM-T !
   ADAM-B1T @ ADAM-B1 f*  ADAM-B1T !
   ADAM-B2T @ ADAM-B2 f*  ADAM-B2T ! ;

: ADAM-C1 ( -- r )  1.0 ADAM-B1T @ f- ;   \ bias correction 1 - b1^t
: ADAM-C2 ( -- r )  1.0 ADAM-B2T @ f- ;   \ bias correction 1 - b2^t

\ Adam-update one bound parameter buffer in place from its backward node
\ (SC-GRAD-AT: the slot's gradient node output; fails closed via EX-OUT@)
: ADAM-UPD ( r ptr a n ptr a ptr a n -- )
   {: lr:r wp:ptr slot:n mp:ptr vp:ptr len:n :}
   lr ADAM-B1 ADAM-B2 ADAM-EPS ADAM-C1 ADAM-C2
   wp  slot SC-GRAD-AT  mp vp len  OPTIM:TT-ADAM! ;

\ AdamW-update one bound parameter buffer in place: the param's decay attribute
\ (WD-DECAY / WD-NONE) gates the trainer's decoupled decay coefficient wd. wd=0
\ makes this bit-identical to ADAM-UPD.
: ADAMW-UPD ( r ptr a n ptr a ptr a n r n -- )
   {: lr:r wp:ptr slot:n mp:ptr vp:ptr len:n wd:r flag:n :}
   wd flag ADAMW-WD-FOR {: wd2:r :}
   lr ADAM-B1 ADAM-B2 ADAM-EPS ADAM-C1 ADAM-C2 wd2
   wp  slot SC-GRAD-AT  mp vp len  OPTIM:TT-ADAMW! ;

\ ---- Adam MLP trainer: first/second-moment buffers per parameter ------------
create AMT-W1M SC-W1N cells allot   create AMT-W1V SC-W1N cells allot
create AMT-B1M SC-B1N cells allot   create AMT-B1V SC-B1N cells allot
create AMT-W2M SC-W2N cells allot   create AMT-W2V SC-W2N cells allot
create AMT-B2M SC-B2N cells allot   create AMT-B2V SC-B2N cells allot

: AMT-ZERO-MOMENTS ( -- )
   0.0 AMT-W1M SC-W1N T-FILL   0.0 AMT-W1V SC-W1N T-FILL
   0.0 AMT-B1M SC-B1N T-FILL   0.0 AMT-B1V SC-B1N T-FILL
   0.0 AMT-W2M SC-W2N T-FILL   0.0 AMT-W2V SC-W2N T-FILL
   0.0 AMT-B2M SC-B2N T-FILL   0.0 AMT-B2V SC-B2N T-FILL ;

public

: AMT-LR ( -- r )  0.02 ;   \ Adam step size for the MLP trainer

\ MLP-trainer decoupled decay coefficient. Kept 0 on a MEASURED basis (dot
\ habu-adopt-nanogpt-weight-fe6dc7e0): adopting nanoGPT's wd=0.1 on the decaying
\ groups (W1/W2 = WD-DECAY, B1/B2 = WD-NONE, wired in AMT-APPLY) was run over the
\ committed 60-step seed/fixture and measurably HURTS the fit - final mean-NLL
\ -2.241 (mNLL -2241) at wd=0.1 vs -2.749 (mNLL -2749) at wd=0, both deterministic
\ (run twice, bit-identical). wd=0.1 still converges but to a strictly worse
\ training-batch NLL: weight decay only regularizes generalization, and this
\ fixture scores solely the training-batch objective, so decay can only trade it
\ away. 0.0 therefore stays - not to preserve the old lock, but because the
\ re-derived wd>0 trajectory is a recorded negative here. The decoupled-decay math
\ with wd>0 is proven in isolation by maki/adamw-test.f.
: AMT-WD ( -- r )  0.0 ;

\ ---- optional LR schedule (opt-in; default OFF keeps the committed trajectory) --
\ When armed (AMT-SCHED!), each step's step-size is LR-SCHED(warmup,decay,min,max)
\ at the current 0-based Adam step; disarmed, AMT-LR-EFF is exactly AMT-LR, so the
\ committed AMT-RUN path (and its -2749 lock) stays bit-identical. LR-SCHED lives
\ in maki/from-scratch-train.f (required above).
private
variable AMT-SCHED?         \ 0 = fixed AMT-LR; nonzero = scheduled
variable AMT-SCHED-WARM   variable AMT-SCHED-DEC
variable AMT-SCHED-MIN    variable AMT-SCHED-MAX

: AMT-LR-EFF ( -- r )       \ this step's lr (ADAM-T is 1-based after ADAM-TICK)
   AMT-SCHED? @ 0= if AMT-LR exit then
   AMT-SCHED-WARM @ AMT-SCHED-DEC @ AMT-SCHED-MIN @ AMT-SCHED-MAX @  ADAM-T @ 1-  LR-SCHED ;
public

: AMT-SCHED-OFF ( -- )  0 AMT-SCHED? ! ;
: AMT-SCHED! ( n n r r -- )   \ arm warmup/decay/min/max for the next run
   AMT-SCHED-MAX !  AMT-SCHED-MIN !  AMT-SCHED-DEC !  AMT-SCHED-WARM !  -1 AMT-SCHED? ! ;

\ ---- optional global-norm gradient clipping (opt-in; default OFF) -------------
\ When armed (AMT-CLIP!), each step rescales every MLP param gradient so the global
\ L2 norm is clipped to AMT-CLIP-V before Adam (nanoGPT's clip_grad_norm_ at 1.0,
\ generalized to any threshold). Disarmed, AMT-CLIP-MAYBE is a single guarded exit,
\ so the committed AMT-RUN / AMT-RUN-SCHED trajectories stay bit-identical. The clip
\ math (GRAD-CLIP-COEF / GCLIP-SCALE!) lives in maki/from-scratch-train.f.
private
variable AMT-CLIP?          \ 0 = no clip; nonzero = clip armed
variable AMT-CLIP-V         \ global-norm clip threshold

\ sum of squares over ALL four MLP param-grad buffers (the BW gradient nodes)
: AMT-GNORM2 ( -- r )
   SC-W1-SLOT SC-GRAD-AT SC-W1N T-NORM2
   SC-B1-SLOT SC-GRAD-AT SC-B1N T-NORM2 f+
   SC-W2-SLOT SC-GRAD-AT SC-W2N T-NORM2 f+
   SC-B2-SLOT SC-GRAD-AT SC-B2N T-NORM2 f+ ;

\ clip the global grad norm to AMT-CLIP-V when armed; a no-op when disarmed
: AMT-CLIP-MAYBE ( -- )
   AMT-CLIP? @ 0= if exit then
   AMT-GNORM2 fsqrt  AMT-CLIP-V @  GRAD-CLIP-COEF {: coef:r :}
   coef SC-W1-SLOT SC-GRAD-AT SC-W1N GCLIP-SCALE!
   coef SC-B1-SLOT SC-GRAD-AT SC-B1N GCLIP-SCALE!
   coef SC-W2-SLOT SC-GRAD-AT SC-W2N GCLIP-SCALE!
   coef SC-B2-SLOT SC-GRAD-AT SC-B2N GCLIP-SCALE! ;
public

: AMT-CLIP-OFF ( -- )  0 AMT-CLIP? ! ;
: AMT-CLIP! ( r -- )   AMT-CLIP-V !  -1 AMT-CLIP? ! ;   \ arm the global-norm clip threshold

\ fresh Adam-MLP run: the from-scratch setup (init params, gen data, BW-BUILD,
\ bind every buffer) plus zeroed Adam state.
\ Precondition: MODEL: SCRATCH-MLP just captured.
: AMT-SETUP ( -- )
   SC-SETUP
   ADAM-RESET
   AMT-ZERO-MOMENTS ;

\ forward -> loss + seed -> full IR: the parameter gradients materialize in
\ their backward node buffers. Returns the batch mean NLL at current params.
: AMT-GRADS ( -- r )
   BW-FWD-N@ EX-RUN-N
   SC-LOSS-SEED {: loss:r :}
   EX-RUN
   loss ;

\ AdamW-update every MLP parameter from its gradient node under the param-group
\ policy: the 2D weight matrices (W1, W2) decay, the biases (B1, B2) do not.
: AMT-APPLY ( -- )
   ADAM-TICK
   AMT-CLIP-MAYBE
   AMT-LR-EFF {: lr:r :}
   lr SC-W1 SC-W1-SLOT AMT-W1M AMT-W1V SC-W1N AMT-WD WD-DECAY ADAMW-UPD
   lr SC-B1 SC-B1-SLOT AMT-B1M AMT-B1V SC-B1N AMT-WD WD-NONE  ADAMW-UPD
   lr SC-W2 SC-W2-SLOT AMT-W2M AMT-W2V SC-W2N AMT-WD WD-DECAY ADAMW-UPD
   lr SC-B2 SC-B2-SLOT AMT-B2M AMT-B2V SC-B2N AMT-WD WD-NONE  ADAMW-UPD ;

\ one Adam training step; returns the pre-update batch mean NLL
: AMT-STEP ( -- r )
   AMT-GRADS {: loss:r :}
   AMT-APPLY
   loss ;

private

variable AMT-STEPS-V
variable AMT-INIT-L
variable AMT-FINAL-L
variable AMT-RAN?

: AMT-CK ( -- )  AMT-RAN? @ 0= if E-ADAM-RUN throw then ;

public

\ run n Adam steps from a fresh SCRATCH-MLP capture; record initial/final loss
: AMT-RUN ( n -- ) {: n:n :}
   AMT-SETUP
   n AMT-STEPS-V !
   n 0 ?do
      AMT-STEP {: l:r :}
      i 0=     if l AMT-INIT-L  ! then
      i n 1- = if l AMT-FINAL-L ! then
   loop
   -1 AMT-RAN? ! ;

\ scheduled Adam MLP run: arm the LR schedule, run n steps from a fresh capture,
\ then disarm so the committed fixed-lr path is untouched. Records init/final loss
\ like AMT-RUN. Precondition: MODEL: SCRATCH-MLP just captured.
: AMT-RUN-SCHED ( n n r r n -- ) {: warm:n dec:n lmin:r lmax:r n:n :}
   warm dec lmin lmax AMT-SCHED!
   n AMT-RUN
   AMT-SCHED-OFF ;

\ clipped Adam MLP run: arm the global-norm clip, run n steps from a fresh capture,
\ then disarm so the committed unclipped path is untouched. Records init/final loss
\ like AMT-RUN. Precondition: MODEL: SCRATCH-MLP just captured.
: AMT-RUN-CLIP ( r n -- ) {: clip:r n:n :}
   clip AMT-CLIP!
   n AMT-RUN
   AMT-CLIP-OFF ;

: AMT-STEPS@   ( -- n )  AMT-CK AMT-STEPS-V @ ;
: AMT-INITIAL@ ( -- r )  AMT-CK AMT-INIT-L @ ;
: AMT-FINAL@   ( -- r )  AMT-CK AMT-FINAL-L @ ;

\ convergence gate: loss halved AND below the committed NLL floor (shared with
\ the SGD trainer - same model, same loss)
: AMT-CONV-RATIO ( -- r )  0.5 ;

: AMT-CONVERGED? ( -- bool )
   AMT-FINAL@  AMT-INITIAL@ AMT-CONV-RATIO f*  f<
   AMT-FINAL@  SC-CONV-THRESH  f<  and ;

public

\ ---- attention trainer: shapes, slots, parameter + target buffers ------------
\ Public so an independent per-step gradient reference (maki/adam-attn-grad-test.f)
\ can read the live trained parameters + target and perturb them for central finite
\ differences - mirroring the from-scratch MLP's public SC-* parameter surface.
3  constant ATN-D            \ head dim d (the MODEL: signature pins q:4x3 ...)
12 constant ATN-EN           \ elements per 4x3 / 3x4 operand (and the output)
1  constant ATN-SN           \ elements of the 1x1 scale factor

0 constant ATN-Q-SLOT        \ model-input slots (capture order = signature order)
1 constant ATN-KT-SLOT
2 constant ATN-S-SLOT
3 constant ATN-V-SLOT

create ATN-Q    ATN-EN cells allot
create ATN-KT   ATN-EN cells allot
create ATN-S    ATN-SN cells allot
create ATN-V    ATN-EN cells allot
create ATN-TGT  ATN-EN cells allot

private

$A77E17 constant ATN-INIT-SEED   \ committed LCG seed (params + target stream)

create ATN-SEED ATN-EN cells allot

create ATN-QM ATN-EN cells allot   create ATN-QV ATN-EN cells allot
create ATN-KM ATN-EN cells allot   create ATN-KV ATN-EN cells allot
create ATN-SM ATN-SN cells allot   create ATN-SV ATN-SN cells allot
create ATN-VM ATN-EN cells allot   create ATN-VV ATN-EN cells allot

: ATN-ZERO-MOMENTS ( -- )
   0.0 ATN-QM ATN-EN T-FILL   0.0 ATN-QV ATN-EN T-FILL
   0.0 ATN-KM ATN-EN T-FILL   0.0 ATN-KV ATN-EN T-FILL
   0.0 ATN-SM ATN-SN T-FILL   0.0 ATN-SV ATN-SN T-FILL
   0.0 ATN-VM ATN-EN T-FILL   0.0 ATN-VV ATN-EN T-FILL ;

\ small deterministic fill in [-0.5, 0.5) from the shared LCG stream
: ATN-FILL ( ptr a n -- ) {: p:ptr len:n :}
   len 0 ?do  SC-UNIT 0.5 f*  p i T-SET  loop ;

\ init parameters + target from the committed seed; s starts at 1/sqrt(d)
: ATN-INIT ( -- )
   ATN-INIT-SEED SC-RNG !
   ATN-Q  ATN-EN ATN-FILL
   ATN-KT ATN-EN ATN-FILL
   ATN-V  ATN-EN ATN-FILL
   1.0 ATN-D s>f fsqrt f/  ATN-S 0 T-SET
   ATN-TGT ATN-EN ATN-FILL
   ADAM-RESET
   ATN-ZERO-MOMENTS ;

\ ---- mean-MSE loss over the output + its mean-scaled seed cotangents --------
: ATN-INV-N ( -- r )  1.0 ATN-EN s>f f/ ;

: ATN-OUT ( -- ptr a )  BW-FWD-N@ 1- MIR-NODE-ID EX-OUT@ ;   \ last forward node = 4x3 output

: ATN-SEED-SCALE! ( -- )    \ TT-MSE-DY writes 2*(o-t); scale to the MEAN loss
   ATN-EN 0 ?do  ATN-SEED i T-GET ATN-INV-N f*  ATN-SEED i T-SET  loop ;

\ write the seed cotangent buffer from the current forward output; return the
\ mean MSE at the current parameters
: ATN-LOSS-SEED ( -- r )
   ATN-OUT {: ob:ptr :}
   ob ATN-TGT ATN-SEED ATN-EN LOSS:TT-MSE-DY
   ATN-SEED-SCALE!
   ob ATN-TGT ATN-EN LOSS:TT-MSE  ATN-INV-N f* ;

public

\ ---- the canonical attention capture (consumers re-issue this exact line) ----
MODEL: ADAM-ATTN ( q:4x3 kt:3x4 s:1x1 v:4x3 -- o ) MATMUL SCALE SOFTMAX-ROW MATMUL ;

: ATN-LR ( -- r )  0.05 ;   \ Adam step size for the attention trainer

\ fresh attention run: init params/target, build backward ONCE, bind buffers.
\ Precondition: MODEL: ADAM-ATTN just captured.
: ATN-SETUP ( -- )
   ATN-INIT
   BW-BUILD
   EX-RESET
   ATN-Q   ATN-Q-SLOT  SC-SLOT EX-BIND
   ATN-KT  ATN-KT-SLOT SC-SLOT EX-BIND
   ATN-S   ATN-S-SLOT  SC-SLOT EX-BIND
   ATN-V   ATN-V-SLOT  SC-SLOT EX-BIND
   ATN-SEED BW-SEED-SLOT@ EX-BIND ;

\ forward -> loss + seed -> full IR; returns the pre-update mean MSE
: ATN-GRADS ( -- r )
   BW-FWD-N@ EX-RUN-N
   ATN-LOSS-SEED {: loss:r :}
   EX-RUN
   loss ;

\ Adam-update all four attention parameters from their gradient nodes
: ATN-APPLY ( -- )
   ADAM-TICK
   ATN-LR ATN-Q  ATN-Q-SLOT  ATN-QM ATN-QV ATN-EN ADAM-UPD
   ATN-LR ATN-KT ATN-KT-SLOT ATN-KM ATN-KV ATN-EN ADAM-UPD
   ATN-LR ATN-S  ATN-S-SLOT  ATN-SM ATN-SV ATN-SN ADAM-UPD
   ATN-LR ATN-V  ATN-V-SLOT  ATN-VM ATN-VV ATN-EN ADAM-UPD ;

\ one attention Adam training step; returns the pre-update mean MSE
: ATN-STEP ( -- r )
   ATN-GRADS {: loss:r :}
   ATN-APPLY
   loss ;

private

variable ATN-STEPS-V
variable ATN-INIT-L
variable ATN-FINAL-L
variable ATN-RAN?

: ATN-CK ( -- )  ATN-RAN? @ 0= if E-ADAM-RUN throw then ;

public

\ run n Adam steps from a fresh ADAM-ATTN capture; record initial/final loss
: ATN-RUN ( n -- ) {: n:n :}
   ATN-SETUP
   n ATN-STEPS-V !
   n 0 ?do
      ATN-STEP {: l:r :}
      i 0=     if l ATN-INIT-L  ! then
      i n 1- = if l ATN-FINAL-L ! then
   loop
   -1 ATN-RAN? ! ;

: ATN-STEPS@   ( -- n )  ATN-CK ATN-STEPS-V @ ;
: ATN-INITIAL@ ( -- r )  ATN-CK ATN-INIT-L @ ;
: ATN-FINAL@   ( -- r )  ATN-CK ATN-FINAL-L @ ;

\ convergence gate parameters (committed; asserted by maki/adam-train-test.f)
: ATN-CONV-RATIO  ( -- r )  0.5 ;    \ require final < initial * ratio
: ATN-CONV-THRESH ( -- r )  0.02 ;   \ require final mean MSE below this floor

: ATN-CONVERGED? ( -- bool )
   ATN-FINAL@  ATN-INITIAL@ ATN-CONV-RATIO f*  f<
   ATN-FINAL@  ATN-CONV-THRESH  f<  and ;

;package
