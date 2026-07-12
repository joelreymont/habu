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
\ maki -> habu only; adam-train owns -5144..-5146.

require maki/from-scratch-train.f
require maki/optim-tensor.f
require maki/loss-tensor.f
require maki/array.f

-5144 constant E-ADAM-RUN   \ a trainer accessor was used before its training run

package MAKI
public

\ ---- Adam hyperparameters (committed; the convergence gates pin outcomes) ----
: ADAM-B1  ( -- r )  0.9 ;          \ first-moment decay
: ADAM-B2  ( -- r )  0.999 ;        \ second-moment decay
: ADAM-EPS ( -- r )  0.00000001 ;   \ denominator epsilon (1e-8)

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

\ Adam-update every MLP parameter from its gradient node
: AMT-APPLY ( -- )
   ADAM-TICK
   AMT-LR SC-W1 SC-W1-SLOT AMT-W1M AMT-W1V SC-W1N ADAM-UPD
   AMT-LR SC-B1 SC-B1-SLOT AMT-B1M AMT-B1V SC-B1N ADAM-UPD
   AMT-LR SC-W2 SC-W2-SLOT AMT-W2M AMT-W2V SC-W2N ADAM-UPD
   AMT-LR SC-B2 SC-B2-SLOT AMT-B2M AMT-B2V SC-B2N ADAM-UPD ;

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

: AMT-STEPS@   ( -- n )  AMT-CK AMT-STEPS-V @ ;
: AMT-INITIAL@ ( -- r )  AMT-CK AMT-INIT-L @ ;
: AMT-FINAL@   ( -- r )  AMT-CK AMT-FINAL-L @ ;

\ convergence gate: loss halved AND below the committed NLL floor (shared with
\ the SGD trainer - same model, same loss)
: AMT-CONV-RATIO ( -- r )  0.5 ;

: AMT-CONVERGED? ( -- bool )
   AMT-FINAL@  AMT-INITIAL@ AMT-CONV-RATIO f*  f<
   AMT-FINAL@  SC-CONV-THRESH  f<  and ;

private

\ ---- attention trainer: shapes, committed seed, parameter + target buffers ---
3  constant ATN-D            \ head dim d (the MODEL: signature pins q:4x3 ...)
12 constant ATN-EN           \ elements per 4x3 / 3x4 operand (and the output)
1  constant ATN-SN           \ elements of the 1x1 scale factor

0 constant ATN-Q-SLOT        \ model-input slots (capture order = signature order)
1 constant ATN-KT-SLOT
2 constant ATN-S-SLOT
3 constant ATN-V-SLOT

$A77E17 constant ATN-INIT-SEED   \ committed LCG seed (params + target stream)

create ATN-Q    ATN-EN cells allot
create ATN-KT   ATN-EN cells allot
create ATN-S    ATN-SN cells allot
create ATN-V    ATN-EN cells allot
create ATN-TGT  ATN-EN cells allot
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

: ATN-OUT ( -- ptr a )  BW-FWD-N@ 1- EX-OUT@ ;   \ last forward node = 4x3 output

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
   ATN-Q   ATN-Q-SLOT  EX-BIND
   ATN-KT  ATN-KT-SLOT EX-BIND
   ATN-S   ATN-S-SLOT  EX-BIND
   ATN-V   ATN-V-SLOT  EX-BIND
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

end-package
