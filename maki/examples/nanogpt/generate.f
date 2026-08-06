\ maki/examples/nanogpt/generate.f - autoregressive generation / sampling loop
\ (dot habu-autoregressive-gen-sampling): turn a trained MODEL: from "trains" to
\ "runs". The nanoGPT sample loop, host-side, built on the landed pieces: forward-
\ only execution of the model (the executor runs forward without BW-BUILD), the
\ stable host softmax (maki/softmax.f SM-FWD), the shared library LCG
\ (maki/train-core.f: SC-SEED! + SC-NEXT, [0,1)), and the host-side sampling ops
\ (maki/sampling.f: SMP-ARGMAX/SMP-TEMP!/SMP-TOPK!/SMP-SAMPLE) that GEN-* re-exports.
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
\ generate owns -5328, -5329, -5399. maki -> habu only.

require maki/array.f          \ T-GET / T-SET / T-AT / T-FILL
require maki/softmax.f        \ SM-FWD (numerically-stable host softmax)
require maki/train-core.f     \ SC-SEED! + the shared 32-bit LCG (SC-NEXT, [0,1))
require maki/sampling.f       \ SMP-* host sampling ops (argmax / temp / top-k / sample)

package MAKI
public

-5328 constant E-GEN-PROMPT   \ empty prompt: no tokens to seed the context window
-5329 constant E-GEN-TOPK     \ top-k parameter outside [1,vocab]
-5399 constant E-GEN-TEMP     \ negative sampling temperature

\ The sampling primitives are folded onto maki/sampling.f (SMP-*), the canonical
\ owner of the host-side algebra; generate re-exports them under its GEN-* names so
\ the committed sampling locks (generate-test.f) keep pinning identical behaviour.
: GEN-ARGMAX ( ptr r n -- n )  SMP-ARGMAX ;      \ FIRST index wins ties (strict f>)
: GEN-TEMP!  ( ptr r n r -- )  SMP-TEMP! ;       \ scale each logit by 1/temp in place
: GEN-TOPK!  ( ptr r n n -- )  SMP-TOPK! ;       \ keep k largest, mask the rest to rowmax-50
: GEN-SAMPLE ( ptr r n -- n )  SMP-SAMPLE ;      \ inverse-CDF multinomial over the shared LCG

\ pick the next token id from a raw LAST-position logit row. temp<0 and k outside
\ [1,vocab] are rejected up front (named, red-first). temp=0 is TRUE argmax (greedy,
\ no divide). Otherwise: temperature-divide, optional top-k mask, softmax, sample.
: GEN-NEXT ( ptr r n r n -- n ) {: r:ptr n:n temp:r k:n :}
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
: GEN-CROP ( ptr r n ptr r n -- n ) {: p:ptr plen:n win:ptr blk:n :}
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
: GEN-RUN ( ptr r n ptr r n n r n [ -- ptr r ] -- )
   {: win:ptr blk:n out:ptr gen:n voc:n temp:r topk:n xt :}
   gen 0 ?do
      xt execute {: lg:ptr :}
      lg  blk 1- voc *  T-AT  voc temp topk GEN-NEXT {: nid:n :}
      nid s>f out i T-SET
      blk 1 > if  blk 1- 0 ?do  win i 1+ T-GET  win i T-SET  loop  then
      nid s>f  win blk 1- T-SET
   loop ;

;package
