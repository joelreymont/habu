\ maki/from-scratch-train.f - one host-side training step for the from-scratch flagship
\ and the loop that drives it to convergence (dot habu-maki-from-scratch;
\ docs/model-cad.md Phase 9b; CAD-PLAN section 12 "the training step is one plan
\ unit").
\
\ Composes the LANDED machinery over the windowed MLP (maki/from-scratch-model.f):
\   * BW-BUILD (maki/backward.f) builds forward+backward ONCE into the shared IR
\     and keeps it - the IR is static for the whole session, no MIR-MARK rollback;
\   * EX-BIND / EX-RUN(-N) / EX-OUT@ (maki/executor.f) bind the parameter + input +
\     seed buffers and execute the IR at tensor granularity on the host;
\   * NLL / NLL-MU-GRAD / NLL-LOGVAR-GRAD (maki/loss.f) give the batch Gaussian NLL
\     and the per-element seed cotangent; T-SGD! (maki/array.f) applies the update.
\
\ Output layout: the 8x2 output node is INTERLEAVED per row - mu = column 0, logvar
\ = column 1. The loss is the MEAN Gaussian NLL over the batch, so EVERY gradient
\ is scaled by 1/SC-BATCH: the seed cotangent for the mu-cell of row r is
\ (1/BATCH)*dNLL/dmu and for the logvar-cell (1/BATCH)*dNLL/dlogvar. The backward
\ region seeds the scalar loss L' = sum_k seed_k * out_k, so seed_k = dL/dout_k
\ makes each parameter gradient exactly dL/dparam of the mean loss.
\
\ Step execution order (the executor only runs node prefixes from index 0; there
\ is no "run the backward suffix alone"): (1) EX-RUN-N the forward node count to
\ produce mu/logvar; (2) read them and write the seed cotangent buffer + record
\ the loss; (3) EX-RUN the whole IR - its forward recompute is deterministic and
\ identical, and its backward region now reads the CORRECT seed to yield the
\ parameter gradients; (4) T-SGD! each parameter in place from its backward node
\ buffer. Forward therefore runs twice per step; for this tiny model that is
\ negligible and strictly cheaper than running the full IR twice.
\
\ SC-RUN precondition: the current model IR is a freshly-captured SCRATCH-MLP
\ forward IR (MODEL: SCRATCH-MLP just issued, backward NOT yet built). MODEL: is a
\ top-level parsing word and cannot be wrapped, so a caller re-issues it before
\ each independent run (e.g. the determinism check re-runs from a fresh capture).
\ maki -> habu only; owns -5150..-5159.

require maki/from-scratch-model.f
require maki/backward.f
require maki/executor.f
require maki/loss.f
require maki/array.f
require lib/fmt.f

-5150 constant E-SC-RUN     \ a report / accessor was used before a training run

package MAKI
public

\ ---- hyperparameters (committed; the convergence gate pins the outcome) ------
: SC-LR ( -- r )  0.08 ;                        \ plain-SGD step size
: SC-INV-BATCH ( -- r )  1.0 SC-BATCH s>f f/ ;  \ 1/BATCH (mean-loss / seed scaling)

private

\ ---- seed cotangent buffer (the cotangent for the 8x2 output, interleaved) ----
create SC-SEED SC-BATCH SC-OUT * cells allot

\ ---- forward output access (mu = col 0, logvar = col 1, per row) ------------
: SC-OUT-NODE ( -- CAD-KIND:node-id )  BW-FWD-N@ 1- MIR-NODE-ID ;
: SC-SLOT ( n -- MIR:input-slot )  MIR-SLOT-ID ;
: SC-OUT-MU ( ptr a n -- r ) {: ob:ptr r:n :}  ob r SC-OUT *    T-GET ;
: SC-OUT-LV ( ptr a n -- r ) {: ob:ptr r:n :}  ob r SC-OUT * 1+ T-GET ;

\ one output row: write its two mean-scaled seed cotangents, return its NLL
: SC-SEED-ROW ( ptr a n -- r ) {: ob:ptr r:n :}
   ob r SC-OUT-MU {: mu:r :}
   ob r SC-OUT-LV {: lv:r :}
   SC-Y r T-GET {: y:r :}
   y mu lv LOSS:NLL-MU-GRAD     SC-INV-BATCH f*  SC-SEED r SC-OUT *    T-SET
   y mu lv LOSS:NLL-LOGVAR-GRAD SC-INV-BATCH f*  SC-SEED r SC-OUT * 1+ T-SET
   y mu lv LOSS:NLL ;

\ fill the seed buffer from the current forward output; return the batch MEAN NLL
: SC-LOSS-SEED ( -- r )
   SC-OUT-NODE EX-OUT@ {: ob:ptr :}
   0.0  SC-BATCH 0 ?do  ob i SC-SEED-ROW  f+  loop
   SC-INV-BATCH f* ;

\ parameter gradient buffer: its backward node's output. EX-OUT@ fails closed
\ (E-EX-NODE) if a slot ever carried no gradient node (never, for this model).
: SC-GRAD-AT ( n -- ptr a )  SC-SLOT BW-SLOT-GRAD@ MIR-REF-NODE EX-OUT@ ;

\ SGD one parameter buffer in place from its backward gradient node
: SC-UPD ( ptr a n n -- ) {: wp:ptr s:n len:n :}
   SC-LR  wp  s SC-GRAD-AT  len  T-SGD! ;

: SC-UPDATE-PARAMS ( -- )
   SC-W1 SC-W1-SLOT SC-W1N SC-UPD
   SC-B1 SC-B1-SLOT SC-B1N SC-UPD
   SC-W2 SC-W2-SLOT SC-W2N SC-UPD
   SC-B2 SC-B2-SLOT SC-B2N SC-UPD ;

public

\ one training step: forward -> loss + seed -> full IR (backward) -> SGD update.
\ Returns the batch mean NLL at the CURRENT (pre-update) parameters.
: SC-STEP ( -- r )
   BW-FWD-N@ EX-RUN-N            \ forward slice only (produces mu/logvar)
   SC-LOSS-SEED {: loss:r :}     \ record loss + write the seed cotangents
   EX-RUN                        \ forward recompute + backward (reads the seed)
   SC-UPDATE-PARAMS              \ T-SGD! each parameter from its gradient node
   loss ;

private

variable SC-STEPS-V
variable SC-INIT-L
variable SC-FINAL-L
variable SC-RAN?

public

\ prepare a fresh run: init params, gen data, build forward+backward ONCE, bind
\ every buffer. Precondition: MODEL: SCRATCH-MLP was just captured (a fresh
\ forward IR with no backward built yet), since BW-BUILD appends to the current IR.
: SC-SETUP ( -- )
   SC-INIT-PARAMS
   SC-GEN-DATA
   BW-BUILD                              \ forward+backward once; adds the seed slot
   EX-RESET
   SC-X  SC-X-SLOT SC-SLOT EX-BIND
   SC-W1 SC-W1-SLOT SC-SLOT EX-BIND
   SC-B1 SC-B1-SLOT SC-SLOT EX-BIND
   SC-W2 SC-W2-SLOT SC-SLOT EX-BIND
   SC-B2 SC-B2-SLOT SC-SLOT EX-BIND
   SC-SEED BW-SEED-SLOT@ EX-BIND ;

public

\ run N training steps from a fresh SCRATCH-MLP capture; record initial/final loss
: SC-RUN ( n -- ) {: n:n :}
   SC-SETUP
   n SC-STEPS-V !
   n 0 ?do
      SC-STEP {: l:r :}
      i 0=     if l SC-INIT-L  ! then
      i n 1- = if l SC-FINAL-L ! then
   loop
   -1 SC-RAN? ! ;

private
: SC-CK ( -- )  SC-RAN? @ 0= if E-SC-RUN throw then ;
public

: SC-STEPS@   ( -- n )  SC-CK SC-STEPS-V @ ;
: SC-INITIAL@ ( -- r )  SC-CK SC-INIT-L @ ;
: SC-FINAL@   ( -- r )  SC-CK SC-FINAL-L @ ;

\ ---- convergence gate parameters (committed; asserted by maki/from-scratch-test.f) --
: SC-CONV-RATIO  ( -- r )  0.5 ;    \ require final < initial * ratio (loss halved)
: SC-CONV-THRESH ( -- r )  0.0 ;    \ require final NLL < 0 (confident fit; init is +0.13)

: SC-CONVERGED? ( -- bool )
   SC-FINAL@  SC-INITIAL@ SC-CONV-RATIO f*  f<
   SC-FINAL@  SC-CONV-THRESH  f<  and ;

\ ---- summary line (seed / steps / initial+final loss in milli-NLL / flag) -----
: SC-MILLI ( r -- n )                       \ float -> signed milli-units (round half away)
   1000.0 f*  dup f0< if 0.5 f- else 0.5 f+ then  f>s ;

: SCRATCH-REPORT ( -- ptr u8 n )
   SC-CK
   SB-RESET
   s" scratch.seed data=" SB-APPEND SC-DATA-SEED SB-INT
   s"  param=" SB-APPEND SC-PARAM-SEED SB-INT
   s"  steps=" SB-APPEND SC-STEPS@ SB-INT
   s"  init-mNLL=" SB-APPEND SC-INITIAL@ SC-MILLI SB-INT
   s"  final-mNLL=" SB-APPEND SC-FINAL@ SC-MILLI SB-INT
   s"  converged=" SB-APPEND SC-CONVERGED? if 1 else 0 then SB-INT
   SB$ ;

end-package
