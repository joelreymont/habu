\ maki/batch-loader.f - training-data batch loader for the (B,T) sequence layout
\ decided in docs/batch-sequence-design.md section 4 (Option D). Draws B sequence
\ windows of length T from a token corpus and writes them as CONTIGUOUS row blocks,
\ B OUTERMOST: window b occupies the row block [b*T, b*T+T), so row b*T+t holds
\ window b token t. BL-IDS is the embedding-gather input (EMB-GATHER maki/embedding.f)
\ and BL-TGT the cross-entropy target vector, each row shifted forward by one token.
\ BL-SEG-ATTR emits the segment attribute (block width T) the segment attention op
\ reads (SEG-PACK maki/segment.f). Window starts come from a deterministic LCG, so a
\ fixed seed reproduces the exact batch.
\
\ Fail closed on capacity: the batch is rejected (never truncated, never silently
\ shrunk) when B*T overruns the IR row cap or the embedded activation overruns the
\ executor arena, and when the corpus cannot yield a window plus its shifted target.
\ maki -> habu only; batch-loader owns -5148..-5149.

require maki/array.f
require maki/segment.f

-5148 constant E-BL-CAP      \ batch too large: B*T over the row cap, or B*T*dim over the executor arena
-5149 constant E-BL-CORPUS   \ B<1, T<1, or corpus shorter than T+1 (no window plus its shifted target)

package MAKI
private

\ Caps mirror the private runtime limits a batch must not overrun (executor.f
\ mirrors MIR-CAP the same way, executor.f:73-74). BL-ROW-CAP mirrors MIR-CAP (max
\ IR nodes, maki/model-ir.f:100): the batch's B*T rows ride one embedding-input
\ node. BL-ARENA-CELLS mirrors EX-ARENA-CELLS (executor node-buffer pool,
\ maki/executor.f:76): the embedded activation B*T*dim is the FIRST arena buffer the
\ batch forces, so a batch whose embedding alone overflows the arena cannot run.
128   constant BL-ROW-CAP
$8000 constant BL-ARENA-CELLS

\ Deterministic 32-bit LCG (Numerical Recipes constants), the window-start sampler.
\ Same idiom as maki/from-scratch-model.f:58-66 (private there); a data loader must
\ not require that flagship file (its top-level MODEL: mutates the shared IR), so the
\ stream lives here. Same seed -> same batch.
1664525    constant BL-LCG-A
1013904223 constant BL-LCG-C
$FFFFFFFF  constant BL-LCG-MASK
variable BL-RNG
variable BL-B                 \ sequences in the loaded batch
variable BL-T                 \ tokens per sequence (block width)

: BL-NEXT ( -- n )            \ advance the LCG; raw 32-bit state
   BL-RNG @ BL-LCG-A *  BL-LCG-C +  BL-LCG-MASK and  dup BL-RNG ! ;

: BL-RND% ( n -- n ) {: bound:n :}   \ window-start index in [0,bound)
   BL-NEXT bound mod ;

: BL-GUARD ( n n n n -- ) {: clen:n b:n t:n dim:n :}   \ corpus-len B T dim
   b 1 <  t 1 <  or   clen t 1+ <  or  if E-BL-CORPUS throw then
   b t *        BL-ROW-CAP     >  if E-BL-CAP throw then
   b t * dim *  BL-ARENA-CELLS >  if E-BL-CAP throw then ;

public

\ embedding input: B*T token ids (as floats, fed to EMB-GATHER maki/embedding.f).
create BL-IDS  BL-ROW-CAP cells allot
\ cross-entropy targets: B*T next-token ids, each row shifted one token past BL-IDS.
create BL-TGT  BL-ROW-CAP cells allot

private

\ copy one window: corpus[s..s+t) into row block b of BL-IDS, and the shifted
\ corpus[s+1..s+t] into BL-TGT (targets one token ahead of the inputs).
: BL-WINDOW! ( ptr r n n n -- ) {: cb:ptr t:n b:n s:n :}   \ corpus t b s
   t 0 ?do
      cb  s i +     T-GET   BL-IDS  b t * i +  T-SET
      cb  s i + 1+  T-GET   BL-TGT  b t * i +  T-SET
   loop ;

public

\ Fill BL-IDS/BL-TGT with B windows of length T from corpus, B outermost. cb is a
\ float-cell buffer of token ids; dim is the embedding width (the arena factor).
: BL-LOAD ( ptr r n n n n n -- )              \ corpus corpus-len B T dim seed
   {: cb:ptr clen:n b:n t:n dim:n seed:n :}
   clen b t dim BL-GUARD
   b BL-B !  t BL-T !
   seed BL-RNG !
   clen t - {: span:n :}                  \ number of valid window starts
   b 0 ?do
      span BL-RND% {: s:n :}              \ sample this window's start offset
      cb t i s BL-WINDOW!
   loop ;

: BL-ROWS ( -- n )  BL-B @ BL-T @ * ;      \ loaded row count B*T
: BL-T@   ( -- n )  BL-T @ ;               \ block width T
\ segment attrs cell for the attention op (BTC-1): block width T packed with the
\ caller's causal flag via the segment codec (maki/segment.f SEG-PACK).
: BL-SEG-ATTR ( bool -- n )  BL-T @ swap SEG-PACK ;   \ causal -- attrs

;package
