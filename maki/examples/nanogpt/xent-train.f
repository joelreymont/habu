\ maki/xent-train.f - host cross-entropy classifier trainer (dot
\ habu-cross-entropy-loss-93356943).
\
\ The first trainer seeded by softmax cross-entropy over logits with INTEGER token
\ targets - the GPT-2 classifier loss the NLL/MSE trainers (maki/from-scratch-train.f,
\ maki/adam-train.f) never carried. Composes the LANDED machinery exactly like the
\ from-scratch MLP: BW-BUILD builds forward+backward ONCE into the shared IR;
\ EX-BIND / EX-RUN(-N) / EX-OUT@ execute it on host buffers; the stable
\ log-softmax+CE reduction and its y-t seed cotangent (maki/loss-tensor.f
\ LOSS:TT-XENT / LOSS:TT-XENT-SEED) give the batch loss and the cotangent; T-SGD!
\ applies the update.
\
\ Model (captured with MODEL:): a batch of CET-R=10 examples, CET-F=5 features each,
\ mapped by a single LINEAR to logits:10x5 (CET-V=5 classes) - softmax regression.
\   x:10x5 -LINEAR(w:5x5, b:1x5)-> logits:10x5.
\ Data is DETERMINISTIC and committed: example r belongs to class c = r mod V and its
\ feature vector is the class one-hot plus a small deterministic LCG noise term, so a
\ linear classifier separates the classes and the cross-entropy drives to near zero.
\ TARGET CONTRACT: CET-T holds one INTEGER-valued float class id per example (read
\ via f>s), the same integer-target contract EMB-GATHER's index operand uses.
\
\ Step order mirrors maki/from-scratch-train.f: (1) run the forward node slice to
\ produce the logits; (2) read them, write the mean-scaled y-t seed cotangent, record
\ the mean CE; (3) run the WHOLE IR (deterministic forward recompute + the backward
\ region reading the seed); (4) T-SGD! each parameter from its gradient node. The
\ loss is the MEAN cross-entropy over the batch, so the seed and the reported loss
\ are both scaled by 1/CET-R (CET-INV-R). maki -> habu only.

require maki/cad.f
require maki/backward.f
require maki/executor.f
require maki/loss-tensor.f
require maki/array.f

package MAKI
public

\ ---- shapes (examples, features, classes) + element counts + input slots -----
10 constant CET-R          \ examples per batch
5  constant CET-F          \ features per example
5  constant CET-V          \ classes (vocab)
50 constant CET-XN         \ CET-R * CET-F
25 constant CET-WN         \ CET-F * CET-V
5  constant CET-BN         \ 1 * CET-V
10 constant CET-TN         \ CET-R integer targets
50 constant CET-ON         \ CET-R * CET-V logits / seed

0 constant CET-X-SLOT
1 constant CET-W-SLOT
2 constant CET-B-SLOT

\ ---- bound host buffers (params updated in place; T is the integer targets) ----
create CET-X    CET-XN   cells allot
create CET-W    CET-WN   cells allot
create CET-B    CET-BN   cells allot
create CET-T    CET-TN   cells allot
create CET-SEED CET-ON   cells allot

: CET-LR    ( -- r )  0.5 ;                      \ plain-SGD step size
: CET-INV-R ( -- r )  1.0 CET-R s>f f/ ;         \ 1/R (mean-loss / seed scaling)

private

\ ---- deterministic 32-bit LCG (Numerical Recipes constants) -----------------
1664525    constant CET-LCG-A
1013904223 constant CET-LCG-C
$FFFFFFFF  constant CET-LCG-MASK
variable CET-RNG

: CET-NEXT ( -- r )                              \ advance the LCG; float in [0,1)
   CET-RNG @ CET-LCG-A *  CET-LCG-C +  CET-LCG-MASK and  {: s:n :}
   s CET-RNG !  s s>f 4294967296.0 f/ ;
: CET-UNIT ( -- r )  CET-NEXT 2.0 f* 1.0 f- ;    \ float in [-1,1)

$1234ABCD constant CET-DATA-SEED
$00C0FFEE constant CET-PARAM-SEED

\ one example: class c = r mod V, features = class one-hot + small LCG noise; target = c
: CET-GEN-ROW ( n -- ) {: r:n :}
   r CET-V mod {: c:n :}
   CET-F 0 ?do
      i c = if 1.0 else 0.0 then  CET-NEXT 0.05 f* f+
      CET-X r CET-F * i +  T-SET
   loop
   c s>f  CET-T r T-SET ;

: CET-SMALL ( -- r )  CET-UNIT 0.1 f* ;
: CET-FILL ( ptr r n -- ) {: base:ptr len:n :}
   len 0 ?do  CET-SMALL base i T-SET  loop ;

public

\ fill CET-X (10x5 features) and CET-T (10 integer targets) from the committed seed
: CET-GEN-DATA ( -- )
   CET-DATA-SEED CET-RNG !
   CET-R 0 ?do  i CET-GEN-ROW  loop ;

\ init every parameter buffer from the committed parameter seed (small non-zero)
: CET-INIT-PARAMS ( -- )
   CET-PARAM-SEED CET-RNG !
   CET-W CET-WN CET-FILL
   CET-B CET-BN CET-FILL ;

private

\ ---- forward output (the logits node) + per-parameter gradient node ----------
: CET-OUT  ( -- ptr r )  BW-FWD-N@ 1- MIR-NODE-ID EX-OUT@ ;
: CET-GRAD ( n -- ptr r )  MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE EX-OUT@ ;

\ scale the whole seed buffer to the MEAN loss (TT-XENT-SEED writes the SUM's y-t)
: CET-SEED-SCALE! ( -- )
   CET-ON 0 ?do  CET-SEED i T-GET CET-INV-R f*  CET-SEED i T-SET  loop ;

\ write the seed cotangent from the current logits; return the batch MEAN CE
: CET-LOSS-SEED ( -- r )
   CET-OUT {: ob:ptr :}
   ob CET-T CET-SEED CET-R CET-V CET-TN LOSS:TT-XENT-SEED
   CET-SEED-SCALE!
   ob CET-R CET-V CET-T CET-TN LOSS:TT-XENT  CET-INV-R f* ;

\ SGD one parameter buffer in place from its backward gradient node
: CET-UPD ( ptr r n n -- ) {: wp:ptr slot:n len:n :}
   CET-LR  wp  slot CET-GRAD  len  T-SGD! ;

: CET-UPDATE-PARAMS ( -- )
   CET-W CET-W-SLOT CET-WN CET-UPD
   CET-B CET-B-SLOT CET-BN CET-UPD ;

public

\ one training step: forward -> loss + seed -> full IR (backward) -> SGD update.
\ Returns the batch mean CE at the CURRENT (pre-update) parameters.
: CET-STEP ( -- r )
   BW-FWD-N@ EX-RUN-N            \ forward slice only (produces the logits)
   CET-LOSS-SEED {: loss:r :}    \ record loss + write the y-t seed cotangents
   EX-RUN                        \ forward recompute + backward (reads the seed)
   CET-UPDATE-PARAMS
   loss ;

\ prepare a fresh run: init params, gen data, build forward+backward ONCE, bind
\ every buffer. Precondition: MODEL: XENT-MLP was just captured (a fresh forward IR).
: CET-SETUP ( -- )
   CET-INIT-PARAMS
   CET-GEN-DATA
   BW-BUILD
   EX-RESET
   CET-X CET-X-SLOT MIR-SLOT-ID EX-BIND
   CET-W CET-W-SLOT MIR-SLOT-ID EX-BIND
   CET-B CET-B-SLOT MIR-SLOT-ID EX-BIND
   CET-SEED BW-SEED-SLOT@ EX-BIND ;

private

variable CET-STEPS-V
variable CET-INIT-L
variable CET-FINAL-L

public

\ ---- canonical model capture (consumers re-issue this exact line) ------------
MODEL: XENT-MLP ( x:10x5 w:5x5 b:1x5 -- logits ) LINEAR ;

\ run N training steps from a fresh XENT-MLP capture; record initial/final loss
: CET-RUN ( n -- ) {: n:n :}
   CET-SETUP
   n CET-STEPS-V !
   n 0 ?do
      CET-STEP {: l:r :}
      i 0=     if l CET-INIT-L  ! then
      i n 1- = if l CET-FINAL-L ! then
   loop ;

: CET-STEPS@   ( -- n )  CET-STEPS-V @ ;
: CET-INITIAL@ ( -- r )  CET-INIT-L @ ;
: CET-FINAL@   ( -- r )  CET-FINAL-L @ ;

;package
