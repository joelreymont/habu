\ maki/sampling.f - host-side sampling ops over a logit span, for the GB10 UMA
\ inference engine (epic habu-epic-gb10-uma-391d12e8, dot
\ habu-infer-sampling-ops-d9c456f7). Lifts the sampling primitives out of the
\ nanoGPT generation loop (maki/examples/nanogpt/generate.f, dot
\ habu-autoregressive-gen-sampling) into a reusable module the decode engine can
\ call on any last-position logit row. On the GB10 a vocab row is ~200KB; the
\ read is cheap on unified memory and the Grace cores run this overlapped with
\ the GPU's next decode step, so everything here is host-side and single-threaded.
\
\ Pipeline for one token (SMP-NEXT): validate the sampling domain up front, then
\ - temperature == 0        -> TRUE argmax (greedy degenerate case, no divide),
\ - top-k == 1              -> TRUE argmax (single-token nucleus == argmax),
\ - otherwise               -> logits/temp, optional top-k mask, stable softmax,
\                              optional top-p/nucleus filter, multinomial draw.
\
\ Numerics: Habu floats are f64 cells (src/core/cell.f CELL=$8), so the softmax
\ normalisation is f64 host-side and uses maki/softmax.f SM-FWD's max-subtraction
\ idiom (SM-MAX before FEXP) - no separate f64 path is needed. Top-k masks the
\ excluded logits to (rowmax - 50): exp(-50) ~ 2e-22 is structurally 0 while
\ keeping lib/fmath.f FEXP's range reduction bounded (a real -inf would form an
\ unbounded 2^k), the finite -inf-equivalent shared with maki/attn-eq.f.
\
\ Determinism / tie-breaking (all deterministic, no data-dependent branches on
\ equal keys):
\   - argmax        : the FIRST index wins ties (strict f>).
\   - top-k         : entries tied with the k-th largest are ALL kept (the
\                     nanoGPT `logits < topk[-1]` rule); may keep more than k.
\   - top-p/nucleus : entries tied with the boundary probability are ALL kept
\                     (keep p_i >= tau); the kept mass is always >= p.
\   - multinomial   : inverse-CDF over the shared LCG (maki/train-core.f SC-NEXT,
\                     [0,1)); the first bucket whose running cumulative exceeds u
\                     wins, and the final bucket absorbs floating rounding so a u
\                     just under 1 never falls off the row end.
\
\ Storage: allocation-free per call. The per-row scratch for top-p is caller-
\ owned (SMP-NEXT takes a scratch span the width of the logit row - the engine
\ allocates it once and reuses it every decode step); the small argmax/top-k/
\ heap cursors are module-owned cells (host is single-threaded here). generate.f
\ still carries its own inline copies of these ops; folding that example onto this
\ module is a behaviour-preserving follow-up (its committed sampling locks pin the
\ algebra) left for a separate change to keep this one addition-only, not a
\ drive-by edit of landed code. maki -> habu only.

require maki/array.f          \ T-GET / T-SET / T-AT / T-SUM
require maki/softmax.f        \ SM-FWD (numerically-stable f64 host softmax)
require maki/train-core.f     \ SC-SEED! + the shared 32-bit LCG (SC-NEXT, [0,1))

package MAKI

-5508 constant E-SMP-TEMP      \ sampling temperature < 0 (0 is the greedy case)
-5509 constant E-SMP-TOPK      \ top-k parameter outside [1, vocab]
-5510 constant E-SMP-TOPP      \ top-p parameter outside (0, 1]

private

\ finite -inf-equivalent for masking (maki/attn-eq.f A-MASK-NEG uses -50): rowmax
\ plus this stays FEXP-safe (|arg| ~ 72) and exp(-50) ~ 2e-22 is structurally 0.
: SMP-NEG ( -- r )  -50.0 ;

variable SMP-BI                \ argmax scratch: winning index
variable SMP-BV                \ argmax scratch: winning value (float bits in a cell)
variable SMP-THR               \ top-k scratch: the kept-set threshold (k-th largest)
variable SMP-SI                \ heap scratch: sift-down cursor
variable SMP-HS                \ heap scratch: live heap size
variable SMP-CUM               \ top-p scratch: running cumulative probability
variable SMP-TAU               \ top-p scratch: nucleus boundary probability

\ count row entries strictly greater than v (rank of v within the row)
: SMP-GT# ( ptr a n r -- n ) {: a:ptr n:n v:r :}
   0  n 0 ?do  a i T-GET v f> if 1+ then  loop ;

public

\ index of the largest entry; the FIRST index wins ties (strict f>)
: SMP-ARGMAX ( ptr a n -- n ) {: a:ptr n:n :}
   0 SMP-BI !   a 0 T-GET SMP-BV !
   n 1 ?do
      a i T-GET SMP-BV @ f> if  i SMP-BI !  a i T-GET SMP-BV !  then
   loop
   SMP-BI @ ;

\ scale each logit by 1/temp in place (temp>0; the greedy paths short-circuit at 0)
: SMP-TEMP! ( ptr a n r -- ) {: a:ptr n:n t:r :}
   n 0 ?do  a i T-GET t f/  a i T-SET  loop ;

\ keep the k largest logits; mask the rest to (rowmax - 50) so the row softmax
\ excludes them. Ties at the threshold are all kept (may exceed k). k>=n is a
\ no-op (nothing is below the minimum).
: SMP-TOPK! ( ptr a n n -- ) {: a:ptr n:n k:n :}
   a n SMP-ARGMAX {: mi:n :}   a mi T-GET {: mx:r :}      \ rowmax (always kept)
   mx SMP-THR !                                           \ threshold starts at the max
   n 0 ?do
      a i T-GET {: v:r :}
      a n v SMP-GT#  k < if  v SMP-THR @ f< if  v SMP-THR !  then  then
   loop
   n 0 ?do
      a i T-GET SMP-THR @ f< if  mx SMP-NEG f+  a i T-SET  then
   loop ;

private

\ larger of node l's two children within the live heap (assumes l < size); the
\ left child wins ties (strict f>), matching the deterministic argmax rule.
: SMP-BIGGER ( ptr a n n -- n ) {: sc:ptr size:n l:n :}
   l 1+ {: rc:n :}
   rc size < if
      sc rc T-GET  sc l T-GET  f>  if rc else l then
   else l then ;

\ swap heap cells i and j
: SMP-SWAP ( ptr a n n -- ) {: sc:ptr i:n j:n :}
   sc i T-GET  sc j T-GET   sc i T-SET  sc j T-SET ;

\ max-heap sift-down of node ROOT within the first SIZE cells of sc
: SMP-SIFT ( ptr a n n -- ) {: sc:ptr size:n root:n :}
   root SMP-SI !
   begin
      SMP-SI @ 2 * 1+ {: l:n :}                          \ left child index
      l size < 0= if exit then                           \ leaf: heap property holds
      sc size l SMP-BIGGER {: c:n :}                     \ larger child
      sc SMP-SI @ T-GET  sc c T-GET  f<  0= if exit then \ parent >= child: settled
      sc SMP-SI @ c SMP-SWAP                             \ demote parent
      c SMP-SI !                                         \ descend into that child
   again ;

\ build a max-heap over the first n cells of sc (bottom-up, O(n))
: SMP-HEAPIFY ( ptr a n -- ) {: sc:ptr n:n :}
   n 2 / 0 ?do  sc n  n 2 / 1- i -  SMP-SIFT  loop ;

public

\ nucleus filter on a probability row p (probs>=0, sum ~ 1) using caller scratch
\ sc (n cells): keep the smallest set of highest-probability tokens whose
\ cumulative reaches pp, zero the rest, and renormalise. pp in (0,1]; pp>=1 is a
\ no-op at the call site. Ties at the boundary are all kept (keep p_i >= tau);
\ the kept mass is always >= pp, and at least the top token survives.
: SMP-TOPP! ( ptr a ptr a n r -- ) {: p:ptr sc:ptr n:n pp:r :}
   n 0 ?do  p i T-GET  sc i T-SET  loop                  \ scratch = copy of the probs
   sc n SMP-HEAPIFY
   0.0 SMP-CUM !   n SMP-HS !
   begin  SMP-HS @ 0 >   SMP-CUM @ pp f<   and  while     \ pop maxima until cum >= pp
      sc 0 T-GET {: mx:r :}
      mx SMP-TAU !                                        \ boundary = last popped
      SMP-CUM @ mx f+ SMP-CUM !
      SMP-HS @ 1- SMP-HS !                                \ drop the root
      SMP-HS @ 0 > if
         sc 0 SMP-HS @ SMP-SWAP                           \ move last into the root
         sc SMP-HS @ 0 SMP-SIFT                           \ restore the heap
      then
   repeat
   n 0 ?do  p i T-GET SMP-TAU @ f< if  0.0 p i T-SET  then  loop   \ mask below tau
   p n T-SUM {: s:r :}
   n 0 ?do  p i T-GET s f/  p i T-SET  loop ;             \ renormalise the nucleus

\ multinomial sample from a probability row by inverse-CDF over the shared LCG:
\ draw u in [0,1), return the first index whose running cumulative exceeds u. The
\ final bucket absorbs rounding so u (just under 1, sum a hair under 1) stays in
\ range.
: SMP-SAMPLE ( ptr a n -- n ) {: p:ptr n:n :}
   SC-NEXT {: u:r :}
   0.0
   n 0 ?do
      p i T-GET f+
      dup u f> if  drop i unloop exit  then
   loop
   drop  n 1- ;

\ pick the next token id from a raw last-position logit row a (n = vocab), using
\ caller scratch sc (n cells, top-p only). Domain guards fire first, named,
\ before any table read: temp<0 -> E-SMP-TEMP, k outside [1,n] -> E-SMP-TOPK, pp
\ outside (0,1] -> E-SMP-TOPP. temp==0 and k==1 are the exact argmax degenerate
\ cases (no divide, no RNG). Otherwise: temperature-scale, optional top-k mask,
\ f64 softmax, optional top-p/nucleus filter, multinomial draw. The row a is
\ consumed in place (scaled then softmaxed).
: SMP-NEXT ( ptr a ptr a n n r r -- n ) {: a:ptr sc:ptr n:n k:n temp:r pp:r :}
   temp f0<                     if E-SMP-TEMP throw then
   k 1 <  k n >  or             if E-SMP-TOPK throw then
   pp 0.0 f<=  pp 1.0 f>  or    if E-SMP-TOPP throw then
   temp 0.0 f= if  a n SMP-ARGMAX exit then               \ greedy: temp->0 limit
   k 1 =    if  a n SMP-ARGMAX exit then                  \ single-token nucleus
   a n temp SMP-TEMP!
   k n < if  a n k SMP-TOPK! then
   a a n SM-FWD                                           \ stable f64 softmax in place
   pp 1.0 f< if  a sc n pp SMP-TOPP! then
   a n SMP-SAMPLE ;

;package
