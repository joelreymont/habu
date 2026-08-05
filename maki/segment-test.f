\ maki/segment-test.f - segment (block-diagonal) causal attention (maki/segment.f):
\ the BTC-1 acceptance. Proves the segment op over B>1 sequences reproduces B
\ INDEPENDENT per-sequence attentions - forward AND VJP - matching the single-sequence
\ ATTN-FWD/ATTN-BWD (maki/attention.f) run per block, with ZERO cross-sequence coupling
\ (perturb one sequence's inputs, every other sequence's outputs/grads stay bit-for-bit
\ identical), and the causal path against a composed CAUSAL-SOFTMAX-ROWS reference.
\ The IR-level VJP is gradchecked with the existing host gradcheck (maki/gradcheck.f).
\
\ Shape: B=2 sequences, T=3 tokens, d=2 -> rows = B*T = 6, head dim 2. Block b is the
\ contiguous row block [b*T,b*T+T) = cells [b*T*d, ...). Names are SGT-prefixed: the
\ maki suite loads every -test.f into one shared dictionary (a clash is rc=78).

require lib/test.f
require lib/prelude.f
require lib/string.f
require maki/attention.f
require maki/segment.f
require maki/gradcheck.f

package MAKI

3 constant SGT-T                \ tokens per sequence (block width)
2 constant SGT-D                \ head dim
2 constant SGT-B                \ sequences (batch)
6 constant SGT-ROWS             \ B*T
6 constant SGT-BD               \ block stride in cells = T*d
12 constant SGT-RC              \ rows*d = one (B*T x d) buffer
36 constant SGT-GC              \ 3*rows*d = combined [dQ;dK;dV] buffer
9  constant SGT-TT              \ T*T score block

create SGT-Q  SGT-RC cells allot   create SGT-K  SGT-RC cells allot
create SGT-V  SGT-RC cells allot   create SGT-DO SGT-RC cells allot
create SGT-OP  SGT-RC cells allot  create SGT-ORP SGT-RC cells allot   \ plain out / ref
create SGT-OC  SGT-RC cells allot  create SGT-ORC SGT-RC cells allot   \ causal out / ref
create SGT-OP2 SGT-RC cells allot                                       \ perturbed out
create SGT-TMPO SGT-RC cells allot                                      \ ref-backward throwaway O
create SGT-G  SGT-GC cells allot   create SGT-GR SGT-GC cells allot     \ combined grad / ref
create SGT-G2 SGT-GC cells allot                                        \ perturbed grad
create SGT-SB  SGT-TT cells allot  create SGT-AB  SGT-TT cells allot    \ score / attn scratch
create SGT-DAB SGT-TT cells allot  create SGT-DSB SGT-TT cells allot    \ dA / dS scratch

\ ---- helpers ----------------------------------------------------------------
\ exact bit-for-bit element compare (equivalence + cross-sequence invariance)
: SGT-EQ? ( ptr r ptr r n -- bool ) {: a:ptr b:ptr n:n :}
   n 0 ?do  a i T-GET b i T-GET f= 0= if false unloop exit then  loop  true ;

: SGT-BLK ( ptr a n -- ptr a )  SGT-BD * T-AT ;    \ block n of a (B*T x d) buffer
: SGT-KREG ( ptr a -- ptr a )  SGT-RC     T-AT ;   \ dK region of a combined [dQ;dK;dV] buffer
: SGT-VREG ( ptr a -- ptr a )  SGT-RC 2 * T-AT ;   \ dV region (dQ region is offset 0)

\ deterministic varied positive fill (distinct per buffer via the seed)
: SGT-FILL ( ptr r n -- ) {: p:ptr seed:n :}
   SGT-RC 0 ?do  i seed + 7 * 11 mod s>f 0.1 f* 0.15 f+  p i T-SET  loop ;

: SGT-FILL-INPUTS ( -- )
   SGT-Q 1 SGT-FILL  SGT-K 4 SGT-FILL  SGT-V 2 SGT-FILL  SGT-DO 3 SGT-FILL ;

\ one block's single-block causal reference, composed from the checked primitives
\ (the causal analogue of ATTN-FWD, swapping CAUSAL-SOFTMAX-ROWS for the plain softmax)
: SGT-CREF ( ptr r ptr r ptr r ptr r -- ) {: q:ptr k:ptr v:ptr o:ptr :}
   q k SGT-SB  SGT-T SGT-T SGT-D  MM-NT
   SGT-SB  SGT-T SGT-T *  SGT-D SEG-1/SQRT  ATTN-SCALE!
   SGT-SB SGT-AB SGT-T  CAUSAL-SOFTMAX-ROWS
   SGT-AB v o  SGT-T SGT-T SGT-D  MATMUL ;

\ one block's reference VJP via the cited single-sequence ATTN-FWD/ATTN-BWD; writes
\ dQ,dK,dV into the ref combined buffer's three regions at block b's offset.
: SGT-BREF ( n -- ) {: b:n :}
   SGT-Q b SGT-BLK {: q:ptr :}  SGT-K b SGT-BLK {: k:ptr :}  SGT-V b SGT-BLK {: v:ptr :}
   q k v SGT-SB SGT-AB  SGT-TMPO b SGT-BLK  SGT-T SGT-D  ATTN-FWD   \ recompute A into SGT-AB
   SGT-DO b SGT-BLK  q k v SGT-AB
      SGT-GR SGT-VREG b SGT-BLK  SGT-DAB SGT-DSB
      SGT-GR b SGT-BLK  SGT-GR SGT-KREG b SGT-BLK  SGT-T SGT-D  ATTN-BWD ;

\ per-block reference builders (top-level ?do is interpret-only, so loop in a word).
: SGT-PLAIN-REF ( -- )     \ SGT-ORP = ATTN-FWD per block (the cited single-sequence forward)
   SGT-B 0 ?do
      SGT-Q i SGT-BLK  SGT-K i SGT-BLK  SGT-V i SGT-BLK
      SGT-SB SGT-AB  SGT-ORP i SGT-BLK  SGT-T SGT-D  ATTN-FWD
   loop ;
: SGT-CAUSAL-REF ( -- )    \ SGT-ORC = composed causal single-block attention per block
   SGT-B 0 ?do
      SGT-Q i SGT-BLK  SGT-K i SGT-BLK  SGT-V i SGT-BLK  SGT-ORC i SGT-BLK  SGT-CREF
   loop ;
: SGT-VJP-REF ( -- )       \ SGT-GR = ATTN-BWD per block (the cited single-sequence VJP)
   SGT-B 0 ?do  i SGT-BREF  loop ;

\ hand-built one-node seg-attn IR for the host gradcheck idiom (maki/gradcheck.f)
: SGT-IR ( bool -- ) {: causal:bool :}
   MIR-RESET
   SGT-ROWS SGT-D SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   SGT-ROWS SGT-D SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   SGT-ROWS SGT-D SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   MAKI-OPKIND:SEG-ATTN MIR-OP-BEGIN
      0 MIR-SLOT-ID MIR-IN-REF MIR-IN+
      1 MIR-SLOT-ID MIR-IN-REF MIR-IN+
      2 MIR-SLOT-ID MIR-IN-REF MIR-IN+
      SGT-ROWS SGT-D SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW  SGT-T causal SEG-PACK  1  MIR-OP+ drop ;

T-RESET
SGT-FILL-INPUTS

\ ---- FORWARD: segment op == B independent per-block attentions ---------------
\ plain segment attention over 2 sequences vs ATTN-FWD (attention.f:37) per block.
SGT-Q SGT-K SGT-V SGT-OP  SGT-ROWS SGT-T SGT-D false  SEG-ATTN-FWD
SGT-PLAIN-REF
SGT-OP SGT-ORP SGT-RC SGT-EQ? TTRUE       \ block-diagonal forward reproduces per-block ATTN-FWD

\ causal segment attention vs the composed causal single-block reference per block.
SGT-Q SGT-K SGT-V SGT-OC  SGT-ROWS SGT-T SGT-D true  SEG-ATTN-FWD
SGT-CAUSAL-REF
SGT-OC SGT-ORC SGT-RC SGT-EQ? TTRUE       \ causal path reproduces the per-block causal reference

\ the causal mask actually bites: masked output differs from the unmasked output.
SGT-OC SGT-OP SGT-RC SGT-EQ? TFALSE

\ ---- ZERO cross-sequence coupling (forward) ---------------------------------
\ perturb sequence 0's Q; sequence 1's output must be bit-for-bit unchanged.
SGT-Q 0 T-GET  0.37 f+  SGT-Q 0 T-SET
SGT-Q SGT-K SGT-V SGT-OP2  SGT-ROWS SGT-T SGT-D false  SEG-ATTN-FWD
SGT-OP 1 SGT-BLK  SGT-OP2 1 SGT-BLK  SGT-BD  SGT-EQ? TTRUE       \ sequence 1 untouched
SGT-OP 0 SGT-BLK  SGT-OP2 0 SGT-BLK  SGT-BD  SGT-EQ? TFALSE      \ sequence 0 did move
SGT-Q 0 T-GET  0.37 f-  SGT-Q 0 T-SET                            \ restore

\ ---- VJP: segment adjoint == B independent per-block ATTN-BWD ----------------
\ plain segment VJP over 2 sequences vs ATTN-BWD (attention.f:71) per block.
SGT-Q SGT-K SGT-V SGT-DO SGT-G  SGT-ROWS SGT-T SGT-D false  SEG-ATTN-BWD
SGT-VJP-REF
SGT-G SGT-GR SGT-GC SGT-EQ? TTRUE          \ combined [dQ;dK;dV] reproduces per-block ATTN-BWD

\ ---- ZERO cross-sequence coupling (VJP) -------------------------------------
\ perturb sequence 0's Q; sequence 1's dQ,dK,dV must be bit-for-bit unchanged.
SGT-Q 0 T-GET  0.37 f+  SGT-Q 0 T-SET
SGT-Q SGT-K SGT-V SGT-DO SGT-G2  SGT-ROWS SGT-T SGT-D false  SEG-ATTN-BWD
SGT-G 1 SGT-BLK              SGT-G2 1 SGT-BLK              SGT-BD  SGT-EQ? TTRUE   \ dQ seq1
SGT-G SGT-KREG 1 SGT-BLK     SGT-G2 SGT-KREG 1 SGT-BLK     SGT-BD  SGT-EQ? TTRUE   \ dK seq1
SGT-G SGT-VREG 1 SGT-BLK     SGT-G2 SGT-VREG 1 SGT-BLK     SGT-BD  SGT-EQ? TTRUE   \ dV seq1
SGT-G 0 SGT-BLK             SGT-G2 0 SGT-BLK              SGT-BD  SGT-EQ? TFALSE   \ dQ seq0 moved
SGT-Q 0 T-GET  0.37 f-  SGT-Q 0 T-SET                            \ restore

\ ---- gradcheck idiom (maki/gradcheck.f): the IR-level VJP is numerically correct ---
true  SGT-IR  GC-RUN V-PASS T=
s" host: 3 input(s) gradchecked" GC-RE$ 2swap CONTAINS? TTRUE
false SGT-IR  GC-RUN V-PASS T=

T-REPORT

;package
