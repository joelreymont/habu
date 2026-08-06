\ maki/examples/nanogpt/wtie-train-test.f - GPT-2 weight tying between the token
\ embedding table and the LM head (dot habu-weight-tying-wte), proven end to end
\ on the host executor with summed-gradient single-moment Adam training.
\
\ THE TIE (contract, verified against the landed shapes). GPT-2 ties ONE parameter
\ to two roles: the token-embedding table wte:(V,C), read row-wise by GATHER, and
\ the LM-head weight, read by the head MATMUL as a (C,V) matrix. nanoGPT sets
\ lm_head.weight = wte.weight and the linear reads it TRANSPOSED (logits = x @ wteT).
\ Here hidden(T,C) MATMUL wlm(C,V) -> logits(T,V) needs wlm[c][v] = wte[v][c], i.e.
\ wlm = wte^T. So the tie is a TRANSPOSED role, not a shared buffer: the maki IR binds
\ each input slot to a DISTINCT buffer (executor EX-BIND stores one host pointer per
\ slot, maki/executor.f:537 - NO shape check), so the same table cannot be bound to a
\ (V,C) slot and a (C,V) slot. The trainer keeps ONE logical parameter P:(V,C) (the
\ TIE-P table) and materializes a transposed mirror TIE-WT:(C,V)=P^T (TIE-MIRROR)
\ bound to the head slot; the mirror is refreshed after every optimizer step so both
\ roles read bit-identical (transposed) data.
\
\ VERIFIED PREMISE (pinned before building; probe recorded in the dot report). With
\ the same DISTINCT-buffer binding, backward produces a SEPARATE gradient node per
\ slot (BW-SLOT-GRAD@ per slot -> distinct EX-OUT@ buffer): slot 0 (wte) and slot 2
\ (wlm) each carry their own gradient, slot 1 (ids, integer indices) carries none.
\ The two role-gradients must therefore be SUMMED before the step (the executor does
\ not accumulate across slots). dL/dP[v][c] = Gwte[v][c] + Gwlm[c][v] (the head grad
\ transposed): a variable that appears in two independent inputs accumulates the
\ partial from each (multivariate chain rule). TIE-ACCUM sums them into ONE (V,C)
\ buffer and ONE Adam moment pair updates P - a single tied parameter, a single
\ moment pair, exactly as tying intends.
\
\ Model (the smallest honest tie): MODEL: TIED ( wte:6x4 ids:5x1 wlm:4x6 -- logits )
\ GATHER MATMUL ; - embed the ids through the table, then project through the tied
\ head. Copy task (token t -> class t mod V) trained with stable softmax cross-entropy
\ on integer targets (maki/loss-tensor.f).
\
\ Proofs. (a) TIED GRADCHECK: central finite difference of L=sum(seed*logits) on the
\ SHARED buffer P equals Gwte[v][c]+Gwlm[c][v] over sampled elements (corners, middle,
\ the never-gathered token-V-1 row whose embedding-path grad is 0 so the head path is
\ the whole signal, and a gathered diagonal element where both paths couple), plus a
\ teeth case (a corrupted analytic is NOT close). (b) TRAINING: strictly decreasing,
\ more-than-halved, DETERMINISTIC (run-twice bit-identical) mean CE with committed
\ milli locks. (c) TIE IS REAL: TIE-WT[c][v]==TIE-P[v][c] bit-identical checked after
\ EVERY step (zero mismatches across the whole run). (d) UNTIED BASELINE: the SAME
\ model from the SAME init, with wte and wlm as INDEPENDENT parameters (own moment
\ pairs, no summation, no transposed coupling), trains to a DIFFERENT result - the tie
\ changes the optimization, so it is genuinely wired. (e) RED-FIRST GUARD: TIE-CK-SHAPE
\ rejects a non-transpose-shaped role pair with the named E-TIE-SHAPE; the real setup
\ calls it on the true (wte,wlm) pair and passes. maki -> habu only; weight-tying owns
\ -5433. Load via maki/test.f.

require lib/test.f
require lib/float.f
require maki/cad.f
require maki/gradcheck.f
require maki/backward.f
require maki/executor.f
require maki/loss-tensor.f
require maki/optim-tensor.f
require maki/array.f
require maki/embedding.f
require maki/matmul.f

-5433 constant E-TIE-SHAPE   \ tied roles are not transpose-compatible (rows/cols disagree)

package MAKI

\ ---- shapes: vocab, channels, sequence, logit element count ------------------
6 constant KV   4 constant KC   5 constant KS   30 constant KL

\ ---- the tied parameter (V,C), its transposed head mirror (C,V), and the io ---
create TIE-P    KV KC * cells allot     \ shared token-embedding table = the tied parameter
create TIE-WT   KC KV * cells allot     \ head weight mirror = TIE-P^T (materialized each step)
create TIE-IDS  KS cells allot          \ token ids (integer-valued floats)
create TIE-TGT  KS cells allot          \ integer class targets (copy task)
create TIE-SEED KL cells allot          \ loss cotangent (T,V)
\ ONE Adam moment pair for the tied parameter + the summed-gradient accumulator
create TIE-M1   KV KC * cells allot
create TIE-M2   KV KC * cells allot
create TIE-G    KV KC * cells allot     \ Gwte + Gwlm^T, shape (V,C)

\ ---- untied baseline: independent embedding + head, each its own moment pair --
create UN-P    KV KC * cells allot   create UN-W  KC KV * cells allot
create UN-PM   KV KC * cells allot   create UN-PV KV KC * cells allot
create UN-WM   KC KV * cells allot   create UN-WV KC KV * cells allot

\ ---- deterministic 32-bit LCG (Numerical Recipes constants) -> small init -----
variable TIE-RNG
: TIE-NEXT ( -- r )  TIE-RNG @ 1664525 * 1013904223 + $FFFFFFFF and dup TIE-RNG !  s>f 4294967296.0 f/ ;
: TIE-UNIT ( -- r )  TIE-NEXT 2.0 f* 1.0 f- ;
: TIE-SMALL ( ptr r n -- ) {: p:ptr n:n :}  n 0 ?do  TIE-UNIT 0.1 f*  p i T-SET  loop ;

\ ---- transposed mirror: dst(C,V)[c][v] = src(V,C)[v][c] (outer j=c, inner i=v) --
: TIE-XPOSE ( ptr r ptr r -- ) {: src:ptr dst:ptr :}
   KC 0 ?do  KV 0 ?do  src i KC * j +  T-GET   dst j KV * i +  T-SET  loop loop ;
: TIE-MIRROR ( -- )  TIE-P TIE-WT TIE-XPOSE ;

\ ---- committed init: small table + copy task (token/target t = i mod V) -------
: TIE-INIT ( -- )
   $C0FFEE TIE-RNG !
   TIE-P KV KC * TIE-SMALL
   0.0 TIE-M1 KV KC * T-FILL   0.0 TIE-M2 KV KC * T-FILL
   KS 0 ?do  i KV mod s>f  TIE-IDS i T-SET   i KV mod s>f  TIE-TGT i T-SET  loop ;

\ ---- Adam step state (shared by both trainers; reset per run) -----------------
variable TIE-AT   variable TIE-AB1   variable TIE-AB2
: TIE-A-RESET ( -- )  0 TIE-AT !  1.0 TIE-AB1 !  1.0 TIE-AB2 ! ;
: TIE-B1 ( -- r )  0.9 ;   : TIE-B2 ( -- r )  0.999 ;   : TIE-EPS ( -- r )  0.00000001 ;
: TIE-A-TICK ( -- )
   TIE-AT @ 1+ TIE-AT !
   TIE-AB1 @ TIE-B1 f* TIE-AB1 !   TIE-AB2 @ TIE-B2 f* TIE-AB2 ! ;
: TIE-C1 ( -- r )  1.0 TIE-AB1 @ f- ;   : TIE-C2 ( -- r )  1.0 TIE-AB2 @ f- ;
: TIE-LR ( -- r )  0.05 ;

\ ---- slot gradient node reader (the slot's analytic grad buffer) --------------
: TIE-GRD ( n -- ptr r )  MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE EX-OUT@ ;

\ ---- tie-shape guard: the two roles must be transpose-compatible --------------
\ wte(V,C) and wlm(C,V): rows(wte)=cols(wlm) and cols(wte)=rows(wlm). A pair that
\ is not a clean transpose (the mirror + the summed-grad indexing would be
\ ill-defined) dies with the named E-TIE-SHAPE.
: TIE-CK-SHAPE ( MIR:input-slot MIR:input-slot -- ) {: ws:MIR:input-slot hs:MIR:input-slot :}
   ws MIR-SLOT-ROWS@ ROWS-RAW   hs MIR-SLOT-COLS@ COLS-RAW   <> if E-TIE-SHAPE throw then
   ws MIR-SLOT-COLS@ COLS-RAW   hs MIR-SLOT-ROWS@ ROWS-RAW   <> if E-TIE-SHAPE throw then ;

\ ---- forward output + mean-scaled cross-entropy loss/seed ----------------------
: TIE-OUTB ( -- ptr r )  BW-FWD-N@ 1- MIR-NODE-ID EX-OUT@ ;
: TIE-INVR ( -- r )  1.0 KS s>f f/ ;
: TIE-LOSS-SEED ( -- r )                 \ write dL/dlogits seed; return mean CE
   TIE-OUTB {: ob:ptr :}
   ob TIE-TGT TIE-SEED KS KV KS LOSS:TT-XENT-SEED
   KL 0 ?do  TIE-SEED i T-GET TIE-INVR f*  TIE-SEED i T-SET  loop
   ob KS KV TIE-TGT KS LOSS:TT-XENT  TIE-INVR f* ;

\ ================= tied trainer ===================================================
\ setup binds TIE-P to the wte slot and the mirror TIE-WT to the wlm slot, and
\ checks the tie shape on the REAL role pair (proof e, the passing side).
: TIE-SETUP ( -- )
   TIE-INIT  TIE-MIRROR  TIE-A-RESET
   BW-BUILD  EX-RESET
   TIE-P    0 MIR-SLOT-ID EX-BIND
   TIE-IDS  1 MIR-SLOT-ID EX-BIND
   TIE-WT   2 MIR-SLOT-ID EX-BIND
   TIE-SEED BW-SEED-SLOT@ EX-BIND
   0 MIR-SLOT-ID 2 MIR-SLOT-ID TIE-CK-SHAPE ;

\ accumulate the two role gradients into ONE (V,C) buffer: G[v][c] = Gwte[v][c] +
\ Gwlm[c][v] (the head grad TRANSPOSED). Read grads through locals so the fat
\ pointer is fetched once per element.
: TIE-ACCUM ( -- )
   KV 0 ?do  KC 0 ?do
      0 TIE-GRD  j KC * i +  T-GET       \ Gwte[v=j][c=i]
      2 TIE-GRD  i KV * j +  T-GET  f+    \ + Gwlm[c=i][v=j]
      TIE-G  j KC * i +  T-SET
   loop loop ;

\ one Adam step on the tied parameter from the summed grad, then refresh the mirror
: TIE-APPLY ( -- )
   TIE-A-TICK
   TIE-LR TIE-B1 TIE-B2 TIE-EPS TIE-C1 TIE-C2
   TIE-P TIE-G TIE-M1 TIE-M2 KV KC *  OPTIM:TT-ADAM!
   TIE-MIRROR ;

\ mismatches between the two roles: TIE-WT[c][v] vs TIE-P[v][c] (0 = bit-identical)
: TIE-REAL-MISMATCH ( -- n )
   0  KC 0 ?do  KV 0 ?do
      TIE-WT j KV * i +  T-GET   TIE-P i KC * j +  T-GET  f= 0= if 1+ then
   loop loop ;

variable TIE-REAL-BAD    \ per-step tie-is-real violations summed across a run
: TIE-STEP ( -- r )
   BW-FWD-N@ EX-RUN-N
   TIE-LOSS-SEED {: loss:r :}
   EX-RUN
   TIE-ACCUM
   TIE-APPLY
   TIE-REAL-MISMATCH TIE-REAL-BAD +!    \ prove the tie holds after THIS step
   loss ;

variable TIE-IL   variable TIE-FL   variable TIE-BAD   variable TIE-PREV   variable TIE-FL1
: TIE-RUN ( n -- ) {: n:n :}
   0 TIE-BAD !  0 TIE-REAL-BAD !  1000000.0 TIE-PREV !
   n 0 ?do
      TIE-STEP {: l:r :}
      i 0=     if l TIE-IL ! then
      i n 1- = if l TIE-FL ! then
      l TIE-PREV @ f< 0= if TIE-BAD @ 1+ TIE-BAD ! then
      l TIE-PREV !
   loop ;
: TIE-MILLI ( r -- n )  1000.0 f* 0.5 f+ f>s ;

\ ================= untied baseline ===============================================
\ identical init (UN-P = the same small table, UN-W = UN-P^T at step 0) but wte and
\ wlm are INDEPENDENT parameters: each takes only its own slot gradient, its own
\ Adam moment pair, and there is NO mirror refresh - so the two drift apart.
: UN-INIT ( -- )
   $C0FFEE TIE-RNG !
   UN-P KV KC * TIE-SMALL
   UN-P UN-W TIE-XPOSE                   \ head starts as the transpose (same point as tied)
   0.0 UN-PM KV KC * T-FILL   0.0 UN-PV KV KC * T-FILL
   0.0 UN-WM KC KV * T-FILL   0.0 UN-WV KC KV * T-FILL
   KS 0 ?do  i KV mod s>f  TIE-IDS i T-SET   i KV mod s>f  TIE-TGT i T-SET  loop ;
: UN-SETUP ( -- )
   UN-INIT  TIE-A-RESET
   BW-BUILD  EX-RESET
   UN-P     0 MIR-SLOT-ID EX-BIND
   TIE-IDS  1 MIR-SLOT-ID EX-BIND
   UN-W     2 MIR-SLOT-ID EX-BIND
   TIE-SEED BW-SEED-SLOT@ EX-BIND ;
: UN-APPLY ( -- )                        \ update each parameter from ITS OWN grad
   TIE-A-TICK
   TIE-LR TIE-B1 TIE-B2 TIE-EPS TIE-C1 TIE-C2
   UN-P  0 TIE-GRD  UN-PM UN-PV KV KC *  OPTIM:TT-ADAM!
   TIE-LR TIE-B1 TIE-B2 TIE-EPS TIE-C1 TIE-C2
   UN-W  2 TIE-GRD  UN-WM UN-WV KC KV *  OPTIM:TT-ADAM! ;
: UN-STEP ( -- r )
   BW-FWD-N@ EX-RUN-N
   TIE-LOSS-SEED {: loss:r :}
   EX-RUN
   UN-APPLY
   loss ;
variable UN-FL
: UN-RUN ( n -- ) {: n:n :}
   n 0 ?do  UN-STEP {: l:r :}  i n 1- = if l UN-FL ! then  loop ;

\ ================= tied gradcheck (proof a) ======================================
\ Fixed varied cotangent (non-uniform, like maki/gradcheck.f) so the check is not a
\ plain output sum; L = sum_k seed_k * logits_k.
: TIE-GC-SEED ( -- )  KL 0 ?do  i 7 mod s>f 0.13 f* 0.6 f+  TIE-SEED i T-SET  loop ;
: TIE-GC-SETUP ( -- )
   TIE-INIT  TIE-MIRROR
   BW-BUILD  EX-RESET
   TIE-P    0 MIR-SLOT-ID EX-BIND
   TIE-IDS  1 MIR-SLOT-ID EX-BIND
   TIE-WT   2 MIR-SLOT-ID EX-BIND
   TIE-SEED BW-SEED-SLOT@ EX-BIND
   TIE-GC-SEED ;
: TIE-GC-L ( -- r )                      \ forward-slice loss under the fixed seed
   BW-FWD-N@ EX-RUN-N
   0.0  KL 0 ?do  TIE-OUTB i T-GET  TIE-SEED i T-GET  f*  f+  loop ;
: TIE-GC-H ( -- r )  0.001 ;             \ central-difference step (matches gradcheck GC-H)
\ analytic summed transposed grad at P[v][c] (fresh full run to refresh grad nodes)
: TIE-AN ( n n -- r ) {: v:n c:n :}
   TIE-MIRROR  EX-RUN
   0 TIE-GRD  v KC * c +  T-GET
   2 TIE-GRD  c KV * v +  T-GET  f+ ;
\ central FD of the SHARED buffer element P[v][c]; the tie is active (re-mirror each eval)
: TIE-FD ( n n -- r ) {: v:n c:n :}
   v KC * c +  {: idx:n :}
   TIE-P idx T-GET  {: base:r :}
   base TIE-GC-H f+  TIE-P idx T-SET  TIE-MIRROR  TIE-GC-L  {: yp:r :}
   base TIE-GC-H f-  TIE-P idx T-SET  TIE-MIRROR  TIE-GC-L  {: ym:r :}
   base TIE-P idx T-SET  TIE-MIRROR
   yp ym f-  TIE-GC-H 2.0 f* f/ ;
: TIE-GC-OK? ( n n -- bool ) {: v:n c:n :}  v c TIE-AN  v c TIE-FD  GC-CLOSE? ;

T-RESET

\ ================= (a) tied gradcheck: FD(shared P) == Gwte + Gwlm^T =============
MODEL: TIED ( wte:6x4 ids:5x1 wlm:4x6 -- logits ) GATHER MATMUL ;
TIE-GC-SETUP
0 0 TIE-GC-OK? TTRUE                 \ corner
3 2 TIE-GC-OK? TTRUE                 \ interior
5 3 TIE-GC-OK? TTRUE                 \ token V-1 row: never gathered, grad is head-path only
4 1 TIE-GC-OK? TTRUE                 \ gathered diagonal: both roles couple
\ teeth: a corrupted analytic must NOT match the finite difference
0 0 TIE-FD  0 0 TIE-AN 0.5 f+  GC-CLOSE? TFALSE

\ ================= (b) training: strictly decreasing, deterministic mean CE ======
MODEL: TIED ( wte:6x4 ids:5x1 wlm:4x6 -- logits ) GATHER MATMUL ;
TIE-SETUP
60 TIE-RUN
TIE-FL @ TIE-IL @ f< TTRUE                    \ loss decreased
TIE-FL @ TIE-IL @ 0.5 f* f< TTRUE             \ at least halved
TIE-BAD @ 0 T=                                \ strictly decreasing (no non-decreasing step)
TIE-IL @ TIE-MILLI 1781 T=                    \ committed initial mean CE (determinism lock)
TIE-FL @ TIE-MILLI 1 T=                       \ committed final mean CE (determinism lock)
TIE-FL @ TIE-FL1 !                            \ stash run-1 final (raw) for the run-twice check

\ ================= (c) the tie is real: bit-identical roles after EVERY step ======
TIE-REAL-BAD @ 0 T=                           \ zero role mismatches across all 60 steps
TIE-REAL-MISMATCH 0 T=                        \ and at the final state

\ determinism: a second run from a fresh capture reproduces the final loss exactly
MODEL: TIED ( wte:6x4 ids:5x1 wlm:4x6 -- logits ) GATHER MATMUL ;
TIE-SETUP
60 TIE-RUN
TIE-FL @ TIE-FL1 @ f= TTRUE                    \ run-twice bit-identical final (raw float, not just milli)

\ ================= (d) untied baseline trains to a DIFFERENT result ==============
MODEL: TIED ( wte:6x4 ids:5x1 wlm:4x6 -- logits ) GATHER MATMUL ;
UN-SETUP
60 UN-RUN
UN-FL @ TIE-FL @ f= TFALSE                     \ untied final loss differs from tied (raw float)
1.0  TIE-P UN-P KV KC * T-DIST2  f< TTRUE       \ and the LEARNED tables diverge far (dist^2 ~5.09 >> 1):
                                                \ from identical init the tie reaches a different optimum

\ ================= (e) red-first guard: mismatched roles die named ===============
\ wte(6x4) vs ids(5x1) is not a transpose pair -> E-TIE-SHAPE (the real setup above
\ already exercised the PASSING side on the true wte/wlm pair).
MODEL: TIED ( wte:6x4 ids:5x1 wlm:4x6 -- logits ) GATHER MATMUL ;
BW-BUILD
: TIE-BAD-PAIR ( -- )  0 MIR-SLOT-ID 1 MIR-SLOT-ID TIE-CK-SHAPE ;
' TIE-BAD-PAIR E-TIE-SHAPE TTHROWS

T-REPORT

;package
