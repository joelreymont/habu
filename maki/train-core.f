\ maki/train-core.f - generic host-training primitives shared by the maki
\ trainers, their library tests, and the eval train leg.
\
\ These are framework facilities, not model-specific ones: the backward-node
\ gradient reader (SC-GRAD-AT), the deterministic 32-bit LCG + polar Box-Muller
\ Gaussian sampler, the per-parameter-role init policy (GPT-2 / nanoGPT
\ initialisation), the AdamW decoupled-decay param-group policy, the nanoGPT LR
\ schedule (linear warmup + cosine decay), and the global-norm gradient clip.
\ They were authored inside the nanoGPT example; this file is the library home so
\ the framework and its tests use them WITHOUT depending on an example (dot
\ habu-decouple-the-maki). The example trainers require this file
\ and keep only their model + committed trainer instances. maki -> habu only.
\ Owns E-ADAMW-POLICY (-5157), E-INIT-ROLE (-5158), E-LR-SCHED (-5159) and
\ E-GRAD-CLIP (-5112).

require maki/fmath.f          \ FLN, fsqrt
require maki/array.f          \ T-FILL, T-SET, T-GET
require maki/backward.f       \ BW-SLOT-GRAD@
require maki/executor.f       \ EX-OUT@
require maki/model-ir.f       \ MIR-SLOT-ID, MIR-REF-NODE

package MAKI
public

\ ---- backward-node gradient reader ------------------------------------------
\ SC-SLOT names a model-input slot; SC-GRAD-AT returns a slot's gradient node
\ output buffer (EX-OUT@ fails closed with E-EX-NODE if a slot carried no
\ gradient node).
: SC-SLOT ( n -- MIR:input-slot )  MIR-SLOT-ID ;
: SC-GRAD-AT ( n -- ptr r )  SC-SLOT BW-SLOT-GRAD@ MIR-REF-NODE EX-OUT@ ;

private

\ ---- deterministic 32-bit LCG (Numerical Recipes constants) -----------------
1664525    constant SC-LCG-A
1013904223 constant SC-LCG-C
$FFFFFFFF  constant SC-LCG-MASK       \ mod 2^32
variable SC-RNG

: SC-LCG-M ( -- r )  4294967296.0 ;   \ 2^32 as a float divisor

: SC-NEXT ( -- r )                    \ advance the LCG; return a float in [0,1)
   SC-RNG @ SC-LCG-A *  SC-LCG-C +  SC-LCG-MASK and  {: s:n :}
   s SC-RNG !
   s s>f SC-LCG-M f/ ;

: SC-UNIT ( -- r )  SC-NEXT 2.0 f*  1.0 f- ;   \ float in [-1,1)

\ ---- Gaussian RNG: polar (Marsaglia) form of Box-Muller over the LCG --------
\ Draw u,v uniform in [-1,1); accept the pair when s=u^2+v^2 lies in (0,1) (the
\ unit disk minus its centre) and map to z0 = u*sqrt(-2 ln s / s). The rejection
\ is the "die" that PROVES the value handed to FLN is nonzero: SC-POLAR-OK? gates
\ s>0, so FLN(0)=-inf is unreachable. This stateless generator returns z0 and
\ redraws on the next call (the paired z1 is not cached). No trig word exists in
\ tree, so the polar form is used in place of the sin/cos Box-Muller.
variable SC-GU               \ accepted u of the current polar pair

: SC-GDRAW ( -- r )          \ redraw a pair; stash u; return s = u^2 + v^2
   SC-UNIT SC-GU !
   SC-UNIT dup f*  SC-GU @ dup f* f+ ;

public

: SC-SEED! ( u -- )  SC-RNG ! ;       \ reseed the shared LCG stream deterministically

: SC-POLAR-OK? ( r -- bool ) {: s:r :}   \ accept 0 < s < 1 (s=0 would feed FLN(0))
   0.0 s f<  s 1.0 f<  and ;

: SC-GAUSS ( -- r )          \ one N(0,1): polar Box-Muller; the guard proves s>0
   SC-GDRAW
   begin dup SC-POLAR-OK? 0= while  drop SC-GDRAW  repeat
   dup FLN -2.0 f*  swap f/  fsqrt  SC-GU @ f* ;

\ ---- per-parameter-role init policy (GPT-2 / nanoGPT initialisation) ---------
\ Every parameter carries an EXPLICIT init role: the fill is chosen by role,
\ never by sniffing a buffer name. Weight matrices draw normal(0, 0.02);
\ residual-projection weights draw normal(0, 0.02/sqrt(2*n_layer)) (nanoGPT's
\ per-block output-projection scaling, n_layer supplied at fill time); LayerNorm
\ gamma=1, LayerNorm beta=0 and biases=0 are constant fills. INIT-FILL is the
\ checked applicator - a role outside the five named flags is a registration
\ bug, not a silent default -> throw.
-5158 constant E-INIT-ROLE   \ a parameter's init role is not a valid init policy flag

1 constant IR-NORMAL         \ weight ~ normal(0, 0.02)
2 constant IR-RESID          \ residual-projection weight ~ normal(0, 0.02/sqrt(2*n_layer))
3 constant IR-LN-GAMMA       \ LayerNorm scale = 1
4 constant IR-LN-BETA        \ LayerNorm shift = 0
5 constant IR-BIAS           \ bias = 0

: INIT-STD ( -- r )  0.02 ;                       \ GPT-2 base weight standard deviation

: INIT-RESID-STD ( n -- r ) {: nlayer:n :}        \ 0.02 / sqrt(2 * n_layer)
   INIT-STD  2 nlayer * s>f fsqrt  f/ ;

: INIT-GFILL ( ptr r n r -- ) {: base:ptr len:n std:r :}   \ fill base with N(0,std^2)
   len 0 ?do  SC-GAUSS std f*  base i T-SET  loop ;

: INIT-FILL ( ptr r n n n -- ) {: base:ptr len:n nlayer:n role:n :}
   role IR-NORMAL   = if  base len INIT-STD              INIT-GFILL  exit then
   role IR-RESID    = if  base len nlayer INIT-RESID-STD INIT-GFILL  exit then
   role IR-LN-GAMMA = if  1.0 base len T-FILL  exit then
   role IR-LN-BETA  = if  0.0 base len T-FILL  exit then
   role IR-BIAS     = if  0.0 base len T-FILL  exit then
   E-INIT-ROLE throw ;

\ ---- AdamW decoupled-decay param-group policy -------------------------------
\ Every trained parameter carries an EXPLICIT decay attribute: 2D weight
\ matrices decay (WD-DECAY); biases and LayerNorm gamma/beta do not (WD-NONE) -
\ the standard GPT-2/nanoGPT grouping. The trainer owns the decay coefficient;
\ the per-parameter flag only GATES it. ADAMW-WD-FOR is the checked resolver: an
\ attribute outside the two named flags is a registration bug, not a silent
\ default -> throw.
-5157 constant E-ADAMW-POLICY   \ a parameter's decay attribute is not a valid WD flag

0  constant WD-NONE      \ excluded from weight decay (bias, LN gamma/beta)
-1 constant WD-DECAY     \ decoupled decay applies (2D weight matrix)

: ADAMW-WD-FOR ( r n -- r ) {: wd:r flag:n :}
   flag WD-DECAY = if wd exit then
   flag WD-NONE  = if 0.0 exit then
   E-ADAMW-POLICY throw ;

\ ---- LR schedule: nanoGPT linear warmup + cosine decay (opt-in facility) ------
\ nanoGPT's get_lr as a pure, checked function of the step t. No trig word exists
\ in the tree, so cosine on [0,pi] is a degree-12 Maclaurin series in x^2 over
\ [0,pi/2] (LR-COS-HALF; the series is alternating with strictly decreasing terms
\ there, so its truncation error is bounded by the first omitted term
\ (pi/2)^14/14! ~ 6.4e-9), reflected onto [pi/2,pi] by the exact identity
\ cos(x) = -cos(pi-x) (LR-COS). LR-SCHED threads warmup/decay/min/max into lr(t).
\ The trainers keep a fixed lr by default; scheduled use is opt-in surface.
-5159 constant E-LR-SCHED   \ LR-schedule domain error: t<0, warmup>=decay, min>max, or non-finite lr

: LR-PI ( -- r )  3.141592653589793 ;

private
: LR-FIN? ( r -- bool )  dup f- 0.0 f= ;   \ finite: NaN/Inf make x-x a NaN (f= 0.0 fails)

\ cos on [0,pi/2]: Horner over u=x^2 with coefficients (-1)^k/(2k)! for k=0..6
: LR-COS-HALF ( r -- r ) {: x:r :}
   x x f* {: u:r :}
   0.0000000020876757   u f*  0.00000027557319224 f-
   u f*  0.000024801587302 f+
   u f*  0.0013888888888889 f-
   u f*  0.041666666666667 f+
   u f*  0.5 f-
   u f*  1.0 f+ ;
public

\ cos on [0,pi]: below pi/2 evaluate directly; above, reflect cos(x) = -cos(pi-x)
: LR-COS ( r -- r ) {: x:r :}
   x LR-PI 0.5 f* f>
   if    LR-PI x f-  LR-COS-HALF  fnegate
   else  x LR-COS-HALF then ;

\ lr at step t (0-based): t<warmup -> lmax*(t+1)/(warmup+1); t>decay -> lmin; else
\ the cosine decay lmin + 0.5*(1+cos(pi*ratio))*(lmax-lmin), ratio=(t-warmup)/
\ (decay-warmup). Checked: t>=0, warmup<decay, lmin<=lmax, lmin/lmax finite.
: LR-SCHED ( n n r r n -- r )
   {: warm:n dec:n lmin:r lmax:r t:n :}
   t 0<           if E-LR-SCHED throw then
   warm dec >=    if E-LR-SCHED throw then
   lmin lmax f>   if E-LR-SCHED throw then
   lmin LR-FIN? 0= if E-LR-SCHED throw then
   lmax LR-FIN? 0= if E-LR-SCHED throw then
   t warm < if  lmax  t 1+ s>f f*  warm 1+ s>f f/  exit then
   t dec  > if  lmin  exit then
   t warm - s>f  dec warm - s>f  f/
   LR-PI f*  LR-COS  1.0 f+  0.5 f*
   lmax lmin f-  f*  lmin f+ ;

\ ---- global-norm gradient clipping (opt-in facility) -------------------------
\ nanoGPT clips the global L2 grad norm before each optimizer step
\ (torch.nn.utils.clip_grad_norm_): total_norm = sqrt(sum over ALL param grads of
\ sum(g^2)); coef = clip/(total_norm+1e-6); when coef<1 every grad is rescaled by
\ coef, otherwise the grads are left bit-identical (no rescale, no eps perturbation
\ - torch skips the step when the norm does not exceed clip). GRAD-CLIP-COEF is the
\ checked coefficient (clip>0 and finite, red-first); GCLIP-SCALE! is the per-buffer
\ rescale that no-ops unless coef<1, so applying one GLOBAL coef over every buffer
\ equals a single global rescale. A trainer arms the clip like the LR schedule;
\ disarmed, the committed trajectories are untouched. Zero-grad is safe: norm 0 ->
\ coef = clip/eps (finite, >=1) -> no rescale, never divides by the bare norm.
-5112 constant E-GRAD-CLIP  \ grad-clip domain error: clip <= 0 or non-finite (the -515x block is full)

: CLIP-EPS ( -- r )  0.000001 ;   \ torch clip_grad_norm_ denominator epsilon (1e-6)

: GRAD-CLIP-COEF ( r r -- r ) {: norm:r clip:r :}   \ ( total-norm clip -- coef )
   clip LR-FIN? 0= if E-GRAD-CLIP throw then   \ non-finite clip
   clip 0.0 f> 0=  if E-GRAD-CLIP throw then   \ clip <= 0
   clip  norm CLIP-EPS f+  f/ ;

: GCLIP-SCALE! ( r ptr r n -- ) {: coef:r base:ptr len:n :}   \ in-place rescale by coef when coef<1
   coef 1.0 f< 0= if exit then                 \ not clipping -> buffer bit-unchanged
   len 0 ?do  base i T-GET coef f*  base i T-SET  loop ;

;package
