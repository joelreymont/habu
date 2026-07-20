\ maki/examples/nanogpt/gptblock-torch-ref-data.f - committed PyTorch reference for
\ the full attention-bearing GPT-2 block with the TIED wte/LM head (dot
\ habu-external-deterministic-golden: ground the self-consistent tied GBR/GB-TIE
\ golden against an EXTERNAL golden). GENERATED DATA - regenerate rather than hand-edit.
\
\ Provenance: torch 2.9.1 (CPU, f64), regenerated 2026-07-20 against the TIED
\ composition of maki/examples/nanogpt/gptblock-attn-test.f section (F) (commit
\ 6ea65203, "Tie wte and LM head"). The committed 32-bit LCG (Numerical Recipes
\ a=1664525 c=1013904223 mod 2^32, seed $C0FFEE) reproduces GB-NEXT/GB-UNIT/
\ GB-SMALL bit-exactly in f64 (integer state, then /2^32). Init draw order is the
\ UNCHANGED GB-INIT stream (one stream): wte 6x6, wpe 4x6, wq 6x3, wk 6x3, wv 6x3,
\ wo 3x6, w1 6x8, b1 8, w2 8x6, b2 6, wlm 6x6, blm 6 (each element GB-UNIT*0.1),
\ then sc=1/sqrt(3), the three affine LNs gamma=1/beta=0, ids[t]=tgt[t]=t mod 6,
\ and the causal mask (A-MASK-FILL). THE TIE: wlm's 36 draws are still consumed to
\ keep the LCG stream aligned, then DISCARDED - GB-TIE-SETUP overwrites GB-WLM with
\ GB-TIE-MIRROR (WLM := WTE^T), so the LM head reads logits = F @ wte^T + blm (blm
\ stays an independent trained bias). The replica ties identically: one wte tensor,
\ logits = F @ wte.T, so autograd sums both gradient paths (embedding + head) into
\ wte.grad and torch.optim.Adam sees ONE parameter with one moment pair - exactly
\ GB-TIE-STEP's summed-gradient GB-TIEG = Gwte + Gwlm^T through GB-WTEM/GB-WTEV.
\ Forward, row by row (B=1, T=4, C=6, d=3, V=6, H=8): X0=wte[ids]+wpe; LN1=affine-LN
\ (X0) (population variance, eps=1e-5, maki/layernorm.f); Q=LN1.wq; S=Q.(LN1.wk)^T;
\ S=S*sc+mask; P=row-softmax(S) (stable); O=P.(LN1.wv); attn=O.wo; X1=attn+X0; LN2=
\ affine-LN(X1); H=GELU(LN2.w1+b1) tanh form c=0.7978845608 a=0.044715 (maki/gelu.f);
\ M=H.w2+b2; X2=M+X1; F=affine-LN(X2); logits=F.wte^T+blm. Loss is batch-mean softmax
\ cross-entropy over the T rows (maki/loss-tensor.f TT-XENT * 1/T). Optimizer is
\ torch.optim.Adam(lr=0.05, betas=(0.9,0.999), eps=1e-8, foreach=False), algebraically
\ the maki/optim.f ADAM rule (bias-corrected, eps outside the sqrt), on every trained
\ input; ids and the causal mask are frozen, exactly as GB-TIE-STEP. Torch evaluates
\ exp/tanh with true libm f64 while Habu routes softmax/GELU/logsumexp through the FEXP
\ polynomial core (lib/fmath.f, rel err ~1e-8): that gap is the documented tolerance
\ floor of section (G) - measured executor-vs-torch tied forward rel-L2 2.0e-9 and
\ tied loss-trace max rel 3.8e-7 (step 4), NOT an f32 gap (the composition is f64).
\ Both committed tied milli locks reproduce off this stream: mean CE 1665 milli at
\ step 0, 27 milli at step 11. Layout: the 24 tied forward logits (T x V row-major) on
\ the frozen tied init, then the pre-update mean CE at each of 12 tied Adam steps. Fill
\ words load fail-closed: a count mismatch throws E-GTR-DATA, a step index outside
\ 0..11 throws E-GTR-STEP. gptblock-torch-ref owns -5434..-5439.

require lib/prelude.f
require lib/float.f
require maki/array.f

-5434 constant E-GTR-DATA   \ committed table fill arity mismatch
-5435 constant E-GTR-STEP   \ reference step index outside the committed loss trace

package MAKI
public

24 constant GTR-ON          \ forward logit elements = T*V (4*6)
12 constant GTR-STEPS       \ committed Adam loss-trace steps

private

\ ---- sequential fail-closed table fill (open, append, close-with-count) ----
variable GTR-P              \ open table base
variable GTR-I              \ next element index

: GD( ( ptr a -- )  GTR-P !  0 GTR-I ! ;
: GD  ( r -- )      GTR-P @ GTR-I @ T-SET  GTR-I @ 1+ GTR-I ! ;
: )GD ( n -- )      GTR-I @ <> if E-GTR-DATA throw then ;

create GTR-LOGITS GTR-ON cells allot
create GTR-LOSS   GTR-STEPS cells allot

: GTR-CK ( n -- n )
   dup 0 <  over GTR-STEPS >=  or if E-GTR-STEP throw then ;

public

\ committed torch TIED forward logits (T x V row-major) on the frozen tied init
: GTR-LOG ( -- ptr a )  GTR-LOGITS ;

\ committed torch pre-update mean CE at tied Adam step n (0..11)
: GTR-LOSS@ ( n -- r )  GTR-CK GTR-LOSS swap T-GET ;

private

: GTR-FILL-LOGITS ( -- )
   GTR-LOGITS GD(
   -0.085117689872835 GD  -0.115032806883463 GD  0.082303919328897 GD  0.101423383690452 GD  -0.019870137273255 GD  -0.182590788575583 GD
   -0.024446036531231 GD  0.227793362826011 GD  -0.020078976707893 GD  -0.000236737429612 GD  -0.242350049708743 GD  0.005564896440074 GD
   -0.042621545491427 GD  -0.173649885000057 GD  0.181727903999593 GD  0.084168752266102 GD  -0.098970556382290 GD  -0.211982971621458 GD
   -0.073534363935417 GD  -0.171376418110663 GD  0.031359955529544 GD  0.082820928893602 GD  0.083974353517963 GD  -0.199284538860730 GD
   24 )GD ;

: GTR-FILL-LOSS ( -- )
   GTR-LOSS GD(
   1.665173153094349 GD  1.337984538615042 GD  1.035891534382766 GD  0.784625251179173 GD
   0.570793424179446 GD  0.397915277524943 GD  0.267938441163374 GD  0.175210220189478 GD
   0.111722462812279 GD  0.070123604195404 GD  0.043730382366666 GD  0.027332464848537 GD
   12 )GD ;

GTR-FILL-LOGITS
GTR-FILL-LOSS

;package
