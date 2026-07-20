\ maki/attn-eq.f - differentiable single-head self-attention as a composition of
\ EQUATION ops (dot habu-differentiable-attention-via-a23b42d4).
\
\ THE WALL THIS CLEARS. maki/mha.f is a forward-only buffer golden; the GPT-2 block
\ lane (maki/gptblock-test.f, wall #1) proved the single-running-value MODEL: DSL
\ cannot root Q@Kᵀ over the running representation, so no block could train its
\ attention (maki/adam-attn-grad-test.f trains only PRE-PROJECTED Q/Kt/V as
\ independent inputs). This file makes attention over the running value X trainable
\ end to end, with gradients flowing to the Q/K/V projection weights.
\
\ THE STRUCTURE (why the projections fold). The DSL threads ONE running value; an op's
\ factor 0 IS that running value and named references (`>V`, a signature name) can only
\ be later factors. Three parallel projections from X therefore cannot each be factor 0
\ - only the FIRST op can root from the running X. So exactly one projection is a
\ genuine standalone op (Q = X·WQ, a MATMUL rooting from running X), and the K and V
\ projections FOLD into the score / context einsums as extra factors, consuming X by
\ named reference. The equation engine expresses each fold as one contraction whose
\ adjoints derive at declaration time (maki/spec.f EQ-ADJ-DERIVE):
\
\   A-SCORES: S[q k] = Q[q d] X[k c] WK[c d] +SUM d c   == Q · (X·WK)ᵀ = Q·Kᵀ
\   A-CTX:    O[q d] = P[q k] X[k c] WV[c d] +SUM k c   == P · (X·WV)  = P·V
\
\ Each is a 3-factor equation (composed via the PLAN-EQUATION3 capture arm this lane
\ added; the executor/backward already ran K factors generically). Gradients reach WK
\ and WV through the fold and X through every factor that references it (summed by the
\ reverse pass's fan-out). Folding both Q and K into one score einsum would need three
\ contraction indices (over the DSL's 2-index limit) and would also collapse WQ·WKᵀ
\ into a single bilinear matrix - losing the separate projection gradients - so Q stays
\ a standalone MATMUL and only ONE projection folds per equation.
\
\ THE COMPOSITION (a consumer re-issues the MODEL: line; it is a top-level parsing word):
\   sublayer  ( x wq wk sc wv    -- ctx )    MATMUL x A-SCORES SCALE SOFTMAX-ROW x A-CTX
\   +LM head  ( x wq wk sc wv wh -- logits ) ... A-CTX MATMUL
\ SCALE carries the 1/sqrt(d) temperature (a trained 1x1, as in maki/adam-train.f).
\
\ CAUSAL MASKING is realized with EXISTING ops - no new op-kind: ADD a causal mask
\ constant (0 on/below the diagonal, a finite large-negative above) to the scaled
\ scores before SOFTMAX-ROW. The masked entries decay to ~0 through the stable row
\ softmax (a finite mask keeps lib/fmath.f FEXP in range; a real -inf must never flow,
\ exactly as maki/causal.f documents). ADD's copy adjoint carries the mask through the
\ reverse pass with zero effect on the constant.
\
\ Toy fixed extents (T=4, C=6, d=3), like every SPEC: golden and maki/mha.f; real
\ GPT-2 shapes and the batch dimension arrive with the extent-role-product capability.
\ maki -> habu only.

require maki/spec.f       \ SPEC: / EXTENT: / TENSOR: : the checked einsum + its derived adjoints
require maki/array.f      \ T-GET / T-SET : causal mask constant fill

package MAKI

\ ---- head geometry. Query positions #DAQ and key positions #DAK are distinct nominal
\ roles that share the magnitude T (self-attention); channels #DAC = heads*headdim = d
\ here (single head); head dim #DAD. ------------------------------------------------
4 EXTENT: #DAQ    \ query positions (sequence length T)
4 EXTENT: #DAK    \ key positions (= T; a distinct role from the query axis)
6 EXTENT: #DAC    \ channels C
3 EXTENT: #DAD    \ head dim d

\ ---- equation tensors (distinct names per factor role; matched to operands by extent)
TENSOR: A-Q  ( #DAQ #DAD )    \ Q = X·WQ : query rows x head dim  (the standalone projection)
TENSOR: A-XK ( #DAK #DAC )    \ X in the key-row role : key rows x channels
TENSOR: A-WK ( #DAC #DAD )    \ WK : channels x head dim  (K projection, folded)
TENSOR: A-S  ( #DAQ #DAK )    \ scores S = Q·Kᵀ : query x key
TENSOR: A-P  ( #DAQ #DAK )    \ attention P = softmax(scaled S) : query x key
TENSOR: A-WV ( #DAC #DAD )    \ WV : channels x head dim  (V projection, folded)
TENSOR: A-O  ( #DAQ #DAD )    \ context O = P·V : query x head dim

\ ---- the two folded contractions. Their adjoints derive at declaration time and are
\ finite-difference-checked by maki/gradcheck.f when a composition using them is built.
SPEC: A-SCORES  A-S[daq dak] = Σdad dac A-Q[daq dad] · A-XK[dak dac] · A-WK[dac dad] ;   \ Q·(X·WK)ᵀ
SPEC: A-CTX     A-O[daq dad] = Σdak dac A-P[daq dak] · A-XK[dak dac] · A-WV[dac dad] ;   \ P·(X·WV)

\ ---- causal mask constant (dot's "a causal mask constant is in scope"): a T x T buffer,
\ 0 where key <= query (attended) and a finite large-negative elsewhere (future keys).
\ Added to the scaled scores before the row softmax; masked entries decay to ~0. The
\ magnitude stays finite so the stable softmax's FEXP never forms a non-finite exponent.
public
: A-MASK-NEG ( -- r )  -50.0 ;   \ exp(-50) ~ 2e-22 : structurally 0, FEXP-safe (|k|~72)
: A-MASK-FILL ( ptr a -- ) {: mb:ptr :}   \ outer j=query row, inner i=key col ; mask key>query
   #DAQ 0 ?do
      #DAK 0 ?do
         i j > if A-MASK-NEG else 0.0 then   mb j #DAK * i + T-SET
      loop
   loop ;

;package
