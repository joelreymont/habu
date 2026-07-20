\ maki/examples/nanogpt/generate.f - autoregressive generation / sampling loop
\ (dot habu-autoregressive-gen-sampling): turn a trained MODEL: from "trains" to
\ "runs". The nanoGPT sample loop, host-side, built on the landed pieces: forward-
\ only execution of the model (the executor runs forward without BW-BUILD), the
\ stable host softmax (maki/softmax.f SM-FWD), and the shared library LCG
\ (maki/train-core.f: SC-SEED! + SC-NEXT, [0,1)) for multinomial sampling.
\
\ Per generated token: crop the running sequence to the model's block T (feed only
\ the last T ids), forward, take the LAST-position logit row, temperature-divide,
\ optionally top-k mask, softmax, and multinomial-sample (or argmax for greedy).
\ v0 is a full-context re-forward per token (honest at toy extents); an incremental
\ KV-cache path is a separate later dot.
\
\ Temperature 0 is TRUE argmax (greedy), never a divide-by-zero: the T->0 limit of
\ logits/T + softmax concentrates on the argmax, and generate short-circuits to it.
\ Top-k masks all but the k largest logits to (rowmax - 50) BEFORE softmax - the
\ finite -inf-equivalent idiom (maki/attn-eq.f A-MASK-NEG / maki/causal.f): exp(-50)
\ ~ 2e-22 is structurally 0 while keeping lib/fmath.f FEXP's range reduction bounded
\ (a real -inf would form an unbounded 2^k and hang). Multinomial is inverse-CDF over
\ the softmax row with the shared LCG - a fixed seed locks the sequence; the final
\ bucket absorbs floating rounding so u in [0,1) never falls off the row end.
\
\ generate owns -5328, -5329, -5399 (the free -53xx tail slots; -5322..-5327 are
\ reserved for the concurrent BPE-tokenizer lane). maki -> habu only.

require maki/array.f          \ T-GET / T-SET / T-AT / T-FILL
require maki/softmax.f        \ SM-FWD (numerically-stable host softmax)
require maki/train-core.f     \ SC-SEED! + the shared 32-bit LCG (SC-NEXT, [0,1))

package MAKI
public

-5328 constant E-GEN-PROMPT   \ empty prompt: no tokens to seed the context window
-5329 constant E-GEN-TOPK     \ top-k parameter outside [1,vocab]
-5399 constant E-GEN-TEMP     \ negative sampling temperature

private

\ finite -inf-equivalent for top-k masking: rowmax + this stays FEXP-safe (|k|~72),
\ exp(-50) ~ 2e-22 is structurally 0 (maki/attn-eq.f A-MASK-NEG uses the same value).
: GEN-NEG ( -- r )  -50.0 ;

variable GEN-BI               \ argmax scratch: winning index
variable GEN-BV               \ argmax scratch: winning value (float bits in a cell)
variable GEN-THR              \ top-k scratch: the kept-set threshold (k-th largest value)

\ count row entries strictly greater than v (rank of v within the row)
: GEN-GT# ( ptr a n r -- n ) {: r:ptr n:n v:r :}
   0  n 0 ?do  r i T-GET v f> if 1+ then  loop ;

public

\ index of the largest entry; the FIRST index wins ties (strict >)
: GEN-ARGMAX ( ptr a n -- n ) {: r:ptr n:n :}
   0 GEN-BI !   r 0 T-GET GEN-BV !
   n 1 ?do
      r i T-GET GEN-BV @ f> if  i GEN-BI !  r i T-GET GEN-BV !  then
   loop
   GEN-BI @ ;

\ scale each logit by 1/temp in place (temp>0; greedy takes the argmax path instead)
: GEN-TEMP! ( ptr a n r -- ) {: r:ptr n:n t:r :}
   n 0 ?do  r i T-GET t f/  r i T-SET  loop ;

\ keep the k largest logits; mask the rest to (rowmax - 50) so the row softmax excludes
\ them (finite -inf-equivalent). Ties at the threshold are all kept (may exceed k), the
\ nanoGPT `logits < topk[-1]` rule. k==n is a no-op (nothing below the min is masked).
: GEN-TOPK! ( ptr a n n -- ) {: r:ptr n:n k:n :}
   r n GEN-ARGMAX {: mi:n :}   r mi T-GET {: mx:r :}      \ rowmax (always kept)
   mx GEN-THR !                                           \ threshold starts at the max
   n 0 ?do
      r i T-GET {: v:r :}
      r n v GEN-GT#  k < if  v GEN-THR @ f< if  v GEN-THR !  then  then
   loop
   n 0 ?do
      r i T-GET GEN-THR @ f< if  mx GEN-NEG f+  r i T-SET  then
   loop ;

\ multinomial sample from a probability row (probs>=0, sum ~ 1) by inverse-CDF over the
\ shared LCG: draw u in [0,1), return the first index whose running cumulative exceeds u.
\ The final bucket absorbs rounding: if the cumulative never overtakes u (u just under 1,
\ sum a hair under 1) the last index is returned - u never falls off the row end.
: GEN-SAMPLE ( ptr a n -- n ) {: p:ptr n:n :}
   SC-NEXT {: u:r :}
   0.0
   n 0 ?do
      p i T-GET f+
      dup u f> if  drop i unloop exit  then
   loop
   drop  n 1- ;

\ pick the next token id from a raw LAST-position logit row. temp<0 and k outside
\ [1,vocab] are rejected up front (named, red-first). temp=0 is TRUE argmax (greedy,
\ no divide). Otherwise: temperature-divide, optional top-k mask, softmax, sample.
: GEN-NEXT ( ptr a n r n -- n ) {: r:ptr n:n temp:r k:n :}
   temp f0< if E-GEN-TEMP throw then
   k 1 <  k n >  or if E-GEN-TOPK throw then
   temp 0.0 f= if  r n GEN-ARGMAX exit  then
   r n temp GEN-TEMP!
   k n < if  r n k GEN-TOPK!  then
   r r n SM-FWD                                           \ softmax in place (SM-FWD is in-place safe)
   r n GEN-SAMPLE ;

\ crop a prompt to the model's block T into the window buffer (nanoGPT "crop to block
\ size"): copy the LAST min(plen,blk) ids. Empty prompt throws; a prompt longer than the
\ block does NOT die - it keeps the last blk. Returns the count written (= min(plen,blk)).
: GEN-CROP ( ptr a n ptr a n -- n ) {: p:ptr plen:n win:ptr blk:n :}
   plen 0 <= if E-GEN-PROMPT throw then
   plen blk min {: k:n :}
   plen k -  {: off:n :}
   k 0 ?do  p off i + T-GET  win i T-SET  loop
   k ;

\ generate GEN tokens autoregressively into OUT (one float-cell id per slot). WIN is the
\ block-T id window already bound to the model's ids slot and seeded (see GEN-CROP); it is
\ rolled left by one and the new id appended each step. XT ( -- logits ) runs the forward
\ pass over the current WIN and returns the (blk x voc) logit buffer base; generate reads
\ its LAST row and mutates that row in place (harmless: the next forward recomputes it).
: GEN-RUN ( ptr a n ptr a n n r n [ -- ptr a ] -- )
   {: win:ptr blk:n out:ptr gen:n voc:n temp:r topk:n xt :}
   gen 0 ?do
      xt execute {: lg:ptr :}
      lg  blk 1- voc *  T-AT  voc temp topk GEN-NEXT {: nid:n :}
      nid s>f out i T-SET
      blk 1 > if  blk 1- 0 ?do  win i 1+ T-GET  win i T-SET  loop  then
      nid s>f  win blk 1- T-SET
   loop ;

;package
