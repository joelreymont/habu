\ maki/dropout.f - the dropout op reference: inverted-dropout mask + 1/(1-p) scale.
\
\ nanoGPT dropout (dot habu-dropout-op-train). TRAIN mode zeroes each element with
\ probability p and scales the survivors by 1/(1-p) (inverted dropout, so EVAL is then
\ the exact identity); EVAL mode is the identity regardless of p. Mode (train/eval) and
\ p are OP ATTRIBUTES, decoded at the boundary (DO-MODE / DO-EVAL?), never rediscovered
\ from an input count - the affine-LayerNorm form idiom (maki/model-ir.f lnform).
\
\ MASK. The Bernoulli mask is drawn from a deterministic 32-bit LCG (Numerical-Recipes
\ constants, the shared library sampler design of maki/train-core.f) seeded PER NODE from
\ a run seed and the node's table index (DO-NODE-SEED). A kept element is u32 >= pfix,
\ where pfix = floor(p * 2^32) is the exact-rational p carried in the attr; the drop
\ probability is therefore pfix/2^32 exactly. Because the mask is a pure function of
\ (DROPOUT-SEED!, node index, pfix) it is BIT-REPRODUCIBLE (run-twice locks) and the VJP
\ re-derives THE SAME mask (the backward node reseeds from the forward node's index), so
\ dy is masked and scaled identically - dropped elements get exactly zero gradient. A
\ central-difference gradcheck holds the mask fixed for free (it depends on no input).
\
\ ATTR CELL. bits [31:0] pfix (floor(p*2^32)); bit [32] mode (0=train, 1=eval). A tf32-style
\ pack (maki/prec-attr.f MV-PACK / CPREC-PACK precedent), integer-only, so the float p never
\ enters the cell. DO-APPLY is the single buffer reference the executor runs for BOTH the
\ forward (x->y) and the VJP (dy->dx): eval copies, train masks-and-scales.
\
\ FAIL CLOSED: p<0, p>=1, and non-finite p are the named E-DO-P reject (red-first domain
\ guard). Device lowering is a fail-closed reject (maki/lower/*.f) - dropout on device is a
\ later capability. maki -> habu only; dropout owns -5177.

require lib/prelude.f                 \ true / false
require lib/float.f                   \ f* f/ f< f= f- f>s s>f
require maki/array.f                  \ T-GET / T-SET

-5177 constant E-DO-P                  \ dropout probability domain: p<0, p>=1, or non-finite

package MAKI
public

\ ---- mode attribute (train / eval), the decode boundary consumers dispatch on -------
\ DERIVE eq generates the typed identity compare MAKI-DOMODE:EQ ( domode domode -- bool ).
ENUM domode DERIVE eq
  train eval
;ENUM

\ mode <-> wire codec. DOMODE>N is total (the pack encodes); N>DOMODE decodes the single
\ mode bit (0=train, else eval), total over the one-bit field.
: DOMODE>N ( domode -- n )   MATCH domode  train OF 0 ENDOF  eval OF 1 ENDOF  ;MATCH ;
: N>DOMODE ( n -- domode )   0= if MAKI-DOMODE:TRAIN else MAKI-DOMODE:EVAL then ;

\ ---- attr cell pack / unpack (pfix low 32 bits, mode bit 32) ------------------------
32 constant DO-MODE-SH                 \ mode field shift (pfix occupies the low 32 bits)
: DO-PACK ( domode n -- n )   swap DOMODE>N DO-MODE-SH lshift  or ;
: DO-PFIX ( n -- n )             $FFFFFFFF and ;
: DO-MODE ( n -- domode )        DO-MODE-SH rshift 1 and N>DOMODE ;
: DO-ATTR-EVAL? ( n -- bool )    DO-MODE MAKI-DOMODE:EVAL MAKI-DOMODE:EQ ;

\ ---- probability domain guard + fixed-point / scale codecs --------------------------
\ DO-P>FIX guards p in [0,1) and finite (red-first), then floors p*2^32 into [0,2^32);
\ DO-FIX>SCALE is the survivor scale 2^32/(2^32-pfix) = 1/(1-p) (pfix<2^32 keeps it finite).
: DO-P>FIX ( r -- n )
   dup dup f- 0.0 f= 0= if E-DO-P throw then      \ non-finite (NaN/Inf: p-p is not 0.0)
   dup 0.0 f< if E-DO-P throw then                \ p < 0
   dup 1.0 f< 0= if E-DO-P throw then             \ p >= 1
   4294967296.0 f* f>s ;                          \ floor(p * 2^32)
: DO-FIX>SCALE ( n -- r )
   s>f  4294967296.0 swap f-  4294967296.0 swap f/ ;

private

\ ---- deterministic 32-bit LCG (Numerical-Recipes constants; own stream) --------------
1664525    constant DO-LCG-A
1013904223 constant DO-LCG-C
$FFFFFFFF  constant DO-LCG-MASK        \ mod 2^32
variable DO-RNG                        \ mask-stream state (reseeded per op from the node seed)
$2545F491  constant DO-SEED-INIT       \ default run seed (an arbitrary fixed odd constant)
variable DO-SEED-V
DO-SEED-INIT DO-SEED-V !

: DO-LCG-NEXT ( -- n )                 \ advance the stream; return the raw 32-bit state
   DO-RNG @ DO-LCG-A *  DO-LCG-C +  DO-LCG-MASK and  dup DO-RNG ! ;

: DO-COPY ( ptr r ptr r n -- ) {: sb:ptr db:ptr n:n :}
   n 0 ?do  sb i T-GET  db i T-SET  loop ;

public

\ ---- run seed + per-node seed --------------------------------------------------------
\ A run reseeds the stream deterministically (DROPOUT-SEED!); the per-node seed mixes the
\ run seed with the node's table index so distinct dropout nodes draw distinct masks and
\ the SAME node (forward and its VJP) redraws the SAME mask.
: DROPOUT-SEED! ( n -- )   DO-SEED-V ! ;
: DO-NODE-SEED ( n -- n )   1+ DO-LCG-A *  DO-SEED-V @ +  DO-LCG-MASK and ;

\ ---- the buffer reference (forward x->y AND backward dy->dx are one op) --------------
\ eval = identity copy; train = per-element inverted dropout under the seeded stream.
\ srcb -> dstb over n elements; pfix/eval? decoded from the attr, seed = the node seed.
: DO-APPLY ( ptr r ptr r n n bool n -- )
   {: sb:ptr db:ptr n:n pfix:n ev:bool seed:n :}
   ev if  sb db n DO-COPY  exit  then             \ eval mode is the identity
   seed DO-RNG !
   pfix DO-FIX>SCALE {: sc:r :}                    \ survivor scale 1/(1-p)
   n 0 ?do
      DO-LCG-NEXT pfix >=                          \ keep iff u32 >= pfix (drop prob = pfix/2^32)
      if  sb i T-GET sc f*  else  0.0  then   db i T-SET
   loop ;

\ ---- workload dropout config (the DROPOUT composer resolves it into each node's attr) -
\ The per-workload default the model DROPOUT token stamps into its node (CPREC-DEFAULT!
\ precedent): a run sets the mode (train/eval) and p once, capture stamps each node.
1 LAYOUT-BUFFER DO-MODE-V domode
: DO-MODE-REQ ( -- ptr domode )   0 DO-MODE-V ;
variable DO-P-V
: DROPOUT-MODE! ( domode -- )   DO-MODE-REQ ! ;
: DROPOUT-MODE@ ( -- domode )   DO-MODE-REQ @ ;
: DROPOUT-P! ( r -- )           DO-P-V ! ;
: DROPOUT-P@ ( -- r )           DO-P-V @ ;
MAKI-DOMODE:TRAIN DO-MODE-REQ !                    \ defaults: train, p=0 (identity until set)
0.0 DO-P-V !

\ resolve the current workload config into a node attr (guards p at capture).
: DO-CFG>ATTR ( -- n )   DROPOUT-MODE@ DROPOUT-P@ DO-P>FIX DO-PACK ;

;package
