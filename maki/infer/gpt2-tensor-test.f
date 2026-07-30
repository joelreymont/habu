\ gpt2-tensor-test.f - GPT2TENSOR acceptance (rev-3 S6a + rev-4 identity design).
\
\ Legs, all through the public package surface:
\   1. HF tensor-name pins at the real 124M geometry via COPY-NAME? into caller
\      buffers: all four globals, all thirteen layer-0 keys, a two-digit
\      layer-11 name, and the 39-byte largest-representable-index render, each
\      against the exact checkpoint spelling (census 160 = 4 + 12*13, pinned
\      by the safetensors-test real-artifact leg over the SHA-256-pinned
\      checkpoint); a too-small caller buffer answers NONE with the buffer
\      untouched, and two interleaved copies into two distinct buffers leave
\      both intact;
\   2. shape pins at 124M (qkv weight [768,2304], fc [768,3072], mproj
\      [3072,768] in the declared Conv1D [in,out] orientation, mask
\      [1,1,1024,1024], every vector role) and at the tiny
\      distinct-non-square geometry;
\   3. declared orientation: conv1d on exactly the four Conv1D weights
\      (including the square aproj-w, where shape could never decide), plain
\      everywhere else; FORMAT-ID is MODEL's hf-gpt2 adapter under adapter EQ;
\   4. checked arithmetic at its exact boundaries, every bound derived from
\      MAX-N and this suite's own role-table constants: the largest nlayer
\      COUNT accepts and that plus one rejecting; the largest nembd
\      through the 4*nembd multiply and that plus one rejecting; the largest
\      nctx whose mask element product fits and that plus one rejecting; plus
\      the measured full-product escapes (nctx 4e9 mask, nembd 2^40 qkv) and
\      the MAX-N extremes, all E-SIZE;
\   5. LAYER-ID range rejects (E-LAYER) and boundary accepts;
\   6. SLOT bijectivity on the tiny geometry: every slot in
\      [0, COUNT) hit exactly once, so the catalog's slot numbering is a
\      complete permutation; an off-by-one slot formula reds here;
\   7. THE IDENTITY FIXTURE (ratified correction 1): two configs with the
\      SAME nlayer differing in ONE behavioral field (tied); a layer-id minted
\      against A used with B rejects E-CONFIG even though its slot is in
\      bounds for B - with equal nlayer a bounds check can never be the
\      rejector, so the reject is proven to fire before slot arithmetic;
\      dropping the identity assertion reds this leg;
\   8. the re-MAKE forgery caveat: a layer-id rebuilt around LAYER-ID with an
\      out-of-range or negative index rejects E-LAYER at SLOT and at
\      the negative-index name render;
\   9. checker negatives: a global tensor-id cannot carry a layer and a layer
\      tensor-id cannot omit one (by constructor arity/types); reordered layer fields
\      reject, role families do not cross, MDLCFG's proof in the layer-id slot
\      rejects (and GPT2TENSOR's proof in the mcfg slot), raw cells forge
\      neither config key nor proof, and the private mint is unresolvable outside
\      the package.

require lib/prelude.f
require lib/adt/option.f
require lib/cad-num-arithmetic.f
require lib/test.f
require test/checker-assert.f
require maki/infer/gpt2-tensor.f

package GPT2TENSOR-TEST

: YES ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: NO ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

: UNK ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 1 T= ;

: MISSING ( -- )
   s" required option was NONE" T-LABEL
   0 0= 0= TTRUE ;

: LEN-OF ( option<n> -- n )                     \ SOME's length; a NONE fails here
   MATCH option
      none OF MISSING -1 ENDOF
      some OF ENDOF
   ;MATCH ;

: OPT-NONE ( option<n> -- )                     \ assert NONE
   MATCH option
      none OF 0 ENDOF
      some OF drop 1 ENDOF
   ;MATCH
   0 T= ;

\ ---- this suite's own role-table constants and derived bounds ----------------
$7FFFFFFFFFFFFFFF constant MAXN
4 constant GLOBAL-COUNT            \ the four checkpoint-global roles
13 constant LAYER-ROLE-COUNT       \ the thirteen per-layer roles
4 constant FC-MUL                  \ the MLP widening factor (4*nembd)

: MAX-LAYERS ( -- n )  MAXN GLOBAL-COUNT - LAYER-ROLE-COUNT / ;
: MAX-EMBD ( -- n )  MAXN FC-MUL / ;

variable LOW
variable HIGH

: SQUARE-FITS? ( n -- bool )                    \ v*v <= MAXN, judged without overflow
   dup MAXN swap / > 0= ;

: MAX-SQUARE-ROOT ( -- n )                      \ largest v with v*v <= MAXN, derived
   1 LOW !  MAXN HIGH !
   begin LOW @ HIGH @ < while
      LOW @  HIGH @ LOW @ - 1 + 2 /  +
      dup SQUARE-FITS? if LOW ! else 1 - HIGH ! then
   repeat
   LOW @ ;

\ ---- fixture configs (all through the sole MDLCFG constructor) ----------------
: DT0 ( -- MAKI:dtype )  MAKI-DTYPE:DF32 ;
: EPS0 ( -- r )  0.00001 ;

\ the real GPT-2 124M geometry.
: CFG-REAL ( -- MDLCFG:mcfg )
   EPS0 true MDLCFG-ARCH:GPT2
   DT0 1024 50257 12 768 12 true 50256 50256 MDLCFG:BUILD ;

\ tiny geometry: nlayer 2, nembd 4, nhead 2, nctx 8, nvocab 5 - every 2-d
\ shape distinct and non-square, so a transposed or swapped dim cannot pass.
: CFG-A ( -- MDLCFG:mcfg )
   EPS0 true MDLCFG-ARCH:GPT2
   DT0 8 5 2 4 2 true 4 4 MDLCFG:BUILD ;

\ SAME nlayer as CFG-A, ONE behavioral field flipped (tied): the identity twin.
: CFG-B ( -- MDLCFG:mcfg )
   EPS0 true MDLCFG-ARCH:GPT2
   DT0 8 5 2 4 2 false 4 4 MDLCFG:BUILD ;

\ extreme-but-valid gpt2 geometry: BUILD accepts nembd = MAX-N (nvocab 1,
\ nctx 1, nhead 1), so the 3*nembd qkv product must be the overflow rejector.
: CFG-WIDE ( -- MDLCFG:mcfg )
   EPS0 true MDLCFG-ARCH:GPT2
   DT0 1 1 1 MAXN 1 true 0 0 MDLCFG:BUILD ;

\ the same shape at the exact 4*nembd boundary and one past it.
: CFG-WIDE-MAX ( -- MDLCFG:mcfg )
   EPS0 true MDLCFG-ARCH:GPT2
   DT0 1 1 1 MAX-EMBD 1 true 0 0 MDLCFG:BUILD ;

: CFG-WIDE-OVER ( -- MDLCFG:mcfg )
   EPS0 true MDLCFG-ARCH:GPT2
   DT0 1 1 1 MAX-EMBD 1 + 1 true 0 0 MDLCFG:BUILD ;

\ the llama arm never validated the gpt2 census bound, so COUNT's own
\ pre-check must reject; the MAX-N extreme, then the exact boundary pair.
: CFG-DEEP ( -- MDLCFG:mcfg )
   2 8 10000.0 0.000001 MDLCFG-ARCH:LLAMA
   DT0 8 32 MAXN 4 2 false 1 2 MDLCFG:BUILD ;

: CFG-DEEP-MAX ( -- MDLCFG:mcfg )
   2 8 10000.0 0.000001 MDLCFG-ARCH:LLAMA
   DT0 8 32 MAX-LAYERS 4 2 false 1 2 MDLCFG:BUILD ;

: CFG-DEEP-OVER ( -- MDLCFG:mcfg )
   2 8 10000.0 0.000001 MDLCFG-ARCH:LLAMA
   DT0 8 32 MAX-LAYERS 1 + 4 2 false 1 2 MDLCFG:BUILD ;

\ gpt2 arm with everything minimal but nctx, for the mask element product.
: CFG-CTX ( n -- MDLCFG:mcfg ) {: cx:n :}
   EPS0 true MDLCFG-ARCH:GPT2
   DT0 cx 1 1 1 1 true 0 0 MDLCFG:BUILD ;

\ the two measured full-product escapes: every per-dim product fits, the
\ pair product does not.
: CFG-MASK4E9 ( -- MDLCFG:mcfg )
   EPS0 true MDLCFG-ARCH:GPT2
   DT0 4000000000 5 1 768 12 true 4 4 MDLCFG:BUILD ;

: CFG-NE40 ( -- MDLCFG:mcfg )
   EPS0 true MDLCFG-ARCH:GPT2
   DT0 1 1 1 $10000000000 1 true 0 0 MDLCFG:BUILD ;

\ ---- typed probes over the public surface -------------------------------------
64 constant NAME-CAP
create NAME-BUF-1 NAME-CAP allot
create NAME-BUF-2 NAME-CAP allot

: CHECK-GLOBAL-NAME ( GPT2TENSOR:global-role -- ptr u8 n )           \ copied into NAME-BUF-1
   GPT2TENSOR-TENSOR--ID:GLOBAL NAME-BUF-1 NAME-CAP GPT2TENSOR:COPY-NAME? LEN-OF
   NAME-BUF-1 swap ;

: CHECK-LAYER-NAME ( MDLCFG:mcfg n GPT2TENSOR:layer-role -- MDLCFG:mcfg ptr u8 n )
   {: br:GPT2TENSOR:layer-role :}
   GPT2TENSOR:LAYER-ID br GPT2TENSOR-TENSOR--ID:LAYER
   NAME-BUF-1 NAME-CAP GPT2TENSOR:COPY-NAME? LEN-OF
   NAME-BUF-1 swap ;

: COPY-LAYER-NAME ( MDLCFG:mcfg n GPT2TENSOR:layer-role -- MDLCFG:mcfg n )
   {: br:GPT2TENSOR:layer-role :}
   GPT2TENSOR:LAYER-ID br GPT2TENSOR-TENSOR--ID:LAYER
   NAME-BUF-1 NAME-CAP GPT2TENSOR:COPY-NAME? LEN-OF ;

: CHECK-GLOBAL-NAME-2 ( GPT2TENSOR:global-role -- ptr u8 n )         \ copied into NAME-BUF-2
   GPT2TENSOR-TENSOR--ID:GLOBAL NAME-BUF-2 NAME-CAP GPT2TENSOR:COPY-NAME? LEN-OF
   NAME-BUF-2 swap ;

: GLOBAL-SHAPE ( MDLCFG:mcfg GPT2TENSOR:global-role -- MDLCFG:mcfg n n n n n )
   GPT2TENSOR-TENSOR--ID:GLOBAL GPT2TENSOR:SHAPE ;

: LAYER-SHAPE ( MDLCFG:mcfg n GPT2TENSOR:layer-role -- MDLCFG:mcfg n n n n n )
   {: br:GPT2TENSOR:layer-role :}
   GPT2TENSOR:LAYER-ID br GPT2TENSOR-TENSOR--ID:LAYER GPT2TENSOR:SHAPE ;

: GLOBAL-SLOT ( MDLCFG:mcfg GPT2TENSOR:global-role -- MDLCFG:mcfg CAD-NUM:index )
   GPT2TENSOR-TENSOR--ID:GLOBAL GPT2TENSOR:SLOT ;

: LAYER-SLOT ( MDLCFG:mcfg n GPT2TENSOR:layer-role -- MDLCFG:mcfg CAD-NUM:index )
   {: br:GPT2TENSOR:layer-role :}
   GPT2TENSOR:LAYER-ID br GPT2TENSOR-TENSOR--ID:LAYER GPT2TENSOR:SLOT ;

using CAD-NUM

: TEST-INDEX ( n -- CAD-NUM:index )
   INDEX
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                            negative OF GPT2TENSOR:E-SIZE throw ENDOF
      zero OF GPT2TENSOR:E-SIZE throw ENDOF  overflow OF GPT2TENSOR:E-SIZE throw ENDOF
      underflow OF GPT2TENSOR:E-SIZE throw ENDOF
      bad-alignment OF GPT2TENSOR:E-SIZE throw ENDOF
      misaligned OF GPT2TENSOR:E-SIZE throw ENDOF
   ;MATCH ;

: SLOT= ( CAD-NUM:index n -- )
   TEST-INDEX INDEX= TTRUE ;

;using

: GLOBAL-ORIENTATION ( GPT2TENSOR:global-role -- GPT2TENSOR:orientation )
   GPT2TENSOR-TENSOR--ID:GLOBAL GPT2TENSOR:ORIENTATION ;

: LAYER-ORIENTATION
   ( MDLCFG:mcfg GPT2TENSOR:layer-role -- MDLCFG:mcfg GPT2TENSOR:orientation )
   {: br:GPT2TENSOR:layer-role :}
   0 GPT2TENSOR:LAYER-ID br GPT2TENSOR-TENSOR--ID:LAYER GPT2TENSOR:ORIENTATION ;

: SHAPE= ( n n n n n n n n n n -- ) {: r0:n a0:n a1:n a2:n a3:n r1:n b0:n b1:n b2:n b3:n :}
   r0 r1 T=
   a0 b0 T=  a1 b1 T=  a2 b2 T=  a3 b3 T= ;

: CONV1D? ( GPT2TENSOR:orientation -- )
   GPT2TENSOR-ORIENTATION:CONV1D GPT2TENSOR-ORIENTATION:EQ TTRUE ;

: PLAIN? ( GPT2TENSOR:orientation -- )
   GPT2TENSOR-ORIENTATION:PLAIN GPT2TENSOR-ORIENTATION:EQ TTRUE ;

\ ---- 1. HF tensor-name pins via COPY-NAME? ---------------------------------------
: T-GLOBAL-NAMES ( -- )
   GPT2TENSOR-GLOBAL--ROLE:WTE CHECK-GLOBAL-NAME s" wte.weight" T$=
   GPT2TENSOR-GLOBAL--ROLE:WPE CHECK-GLOBAL-NAME s" wpe.weight" T$=
   GPT2TENSOR-GLOBAL--ROLE:LNF-G CHECK-GLOBAL-NAME s" ln_f.weight" T$=
   GPT2TENSOR-GLOBAL--ROLE:LNF-B CHECK-GLOBAL-NAME s" ln_f.bias" T$= ;

: T-LAYER-NAMES ( -- )
   CFG-REAL
   0 GPT2TENSOR-LAYER--ROLE:LN1-G CHECK-LAYER-NAME s" h.0.ln_1.weight" T$=
   0 GPT2TENSOR-LAYER--ROLE:LN1-B CHECK-LAYER-NAME s" h.0.ln_1.bias" T$=
   0 GPT2TENSOR-LAYER--ROLE:MASK CHECK-LAYER-NAME s" h.0.attn.bias" T$=
   0 GPT2TENSOR-LAYER--ROLE:QKV-W CHECK-LAYER-NAME s" h.0.attn.c_attn.weight" T$=
   0 GPT2TENSOR-LAYER--ROLE:QKV-B CHECK-LAYER-NAME s" h.0.attn.c_attn.bias" T$=
   0 GPT2TENSOR-LAYER--ROLE:APROJ-W CHECK-LAYER-NAME s" h.0.attn.c_proj.weight" T$=
   0 GPT2TENSOR-LAYER--ROLE:APROJ-B CHECK-LAYER-NAME s" h.0.attn.c_proj.bias" T$=
   0 GPT2TENSOR-LAYER--ROLE:LN2-G CHECK-LAYER-NAME s" h.0.ln_2.weight" T$=
   0 GPT2TENSOR-LAYER--ROLE:LN2-B CHECK-LAYER-NAME s" h.0.ln_2.bias" T$=
   0 GPT2TENSOR-LAYER--ROLE:FC-W CHECK-LAYER-NAME s" h.0.mlp.c_fc.weight" T$=
   0 GPT2TENSOR-LAYER--ROLE:FC-B CHECK-LAYER-NAME s" h.0.mlp.c_fc.bias" T$=
   0 GPT2TENSOR-LAYER--ROLE:MPROJ-W CHECK-LAYER-NAME s" h.0.mlp.c_proj.weight" T$=
   0 GPT2TENSOR-LAYER--ROLE:MPROJ-B CHECK-LAYER-NAME s" h.0.mlp.c_proj.bias" T$=
   11 GPT2TENSOR-LAYER--ROLE:MPROJ-B CHECK-LAYER-NAME s" h.11.mlp.c_proj.bias" T$=
   drop ;

\ the largest index LAYER-ID can ever mint (MAX-LAYERS - 1) renders at the 39-byte
\ extreme; the digits below are that derived index, pinned byte-exact.
: T-LARGEST-INDEX ( -- )
   CFG-DEEP-MAX MAX-LAYERS 1 - GPT2TENSOR-LAYER--ROLE:QKV-W CHECK-LAYER-NAME
   dup 39 T=
   s" h.709490156681136599.attn.c_attn.weight" T$=
   drop ;

\ a too-small caller buffer answers NONE and writes nothing: NAME-BUF-1 still
\ holds the wte name copied before the refused longer copies; capacity equal
\ to the name length is the exact accept boundary.
: T-SMALL-BUF ( -- )
   GPT2TENSOR-GLOBAL--ROLE:WTE CHECK-GLOBAL-NAME s" wte.weight" T$=
   GPT2TENSOR-GLOBAL--ROLE:LNF-G GPT2TENSOR-TENSOR--ID:GLOBAL NAME-BUF-1 9 GPT2TENSOR:COPY-NAME? OPT-NONE
   GPT2TENSOR-GLOBAL--ROLE:LNF-G GPT2TENSOR-TENSOR--ID:GLOBAL NAME-BUF-1 0 GPT2TENSOR:COPY-NAME? OPT-NONE
   NAME-BUF-1 10 s" wte.weight" T$=
   GPT2TENSOR-GLOBAL--ROLE:WTE GPT2TENSOR-TENSOR--ID:GLOBAL NAME-BUF-1 10 GPT2TENSOR:COPY-NAME? LEN-OF 10 T= ;

\ two successive copies into two DISTINCT caller buffers leave both intact -
\ the exact interleave the old span-returning API could not survive.
: T-INTERLEAVE ( -- )
   CFG-REAL 0 GPT2TENSOR-LAYER--ROLE:QKV-W COPY-LAYER-NAME
   GPT2TENSOR-GLOBAL--ROLE:LNF-B CHECK-GLOBAL-NAME-2 s" ln_f.bias" T$=
   NAME-BUF-1 swap s" h.0.attn.c_attn.weight" T$=
   drop ;

\ ---- 2. shape pins: 124M then tiny ----------------------------------------------
: T-SHAPES-R ( -- )
   CFG-REAL
   GPT2TENSOR-GLOBAL--ROLE:WTE GLOBAL-SHAPE 2 50257 768 1 1 SHAPE=
   GPT2TENSOR-GLOBAL--ROLE:WPE GLOBAL-SHAPE 2 1024 768 1 1 SHAPE=
   GPT2TENSOR-GLOBAL--ROLE:LNF-G GLOBAL-SHAPE 1 768 1 1 1 SHAPE=
   GPT2TENSOR-GLOBAL--ROLE:LNF-B GLOBAL-SHAPE 1 768 1 1 1 SHAPE=
   0 GPT2TENSOR-LAYER--ROLE:LN1-G LAYER-SHAPE 1 768 1 1 1 SHAPE=
   0 GPT2TENSOR-LAYER--ROLE:LN1-B LAYER-SHAPE 1 768 1 1 1 SHAPE=
   0 GPT2TENSOR-LAYER--ROLE:MASK LAYER-SHAPE 4 1 1 1024 1024 SHAPE=
   0 GPT2TENSOR-LAYER--ROLE:QKV-W LAYER-SHAPE 2 768 2304 1 1 SHAPE=
   0 GPT2TENSOR-LAYER--ROLE:QKV-B LAYER-SHAPE 1 2304 1 1 1 SHAPE=
   0 GPT2TENSOR-LAYER--ROLE:APROJ-W LAYER-SHAPE 2 768 768 1 1 SHAPE=
   0 GPT2TENSOR-LAYER--ROLE:APROJ-B LAYER-SHAPE 1 768 1 1 1 SHAPE=
   0 GPT2TENSOR-LAYER--ROLE:LN2-G LAYER-SHAPE 1 768 1 1 1 SHAPE=
   0 GPT2TENSOR-LAYER--ROLE:LN2-B LAYER-SHAPE 1 768 1 1 1 SHAPE=
   0 GPT2TENSOR-LAYER--ROLE:FC-W LAYER-SHAPE 2 768 3072 1 1 SHAPE=
   0 GPT2TENSOR-LAYER--ROLE:FC-B LAYER-SHAPE 1 3072 1 1 1 SHAPE=
   0 GPT2TENSOR-LAYER--ROLE:MPROJ-W LAYER-SHAPE 2 3072 768 1 1 SHAPE=
   0 GPT2TENSOR-LAYER--ROLE:MPROJ-B LAYER-SHAPE 1 768 1 1 1 SHAPE=
   GPT2TENSOR:COUNT 160 T=
   drop ;

: T-SHAPES-TINY ( -- )
   CFG-A
   GPT2TENSOR-GLOBAL--ROLE:WTE GLOBAL-SHAPE 2 5 4 1 1 SHAPE=
   GPT2TENSOR-GLOBAL--ROLE:WPE GLOBAL-SHAPE 2 8 4 1 1 SHAPE=
   1 GPT2TENSOR-LAYER--ROLE:QKV-W LAYER-SHAPE 2 4 12 1 1 SHAPE=
   1 GPT2TENSOR-LAYER--ROLE:QKV-B LAYER-SHAPE 1 12 1 1 1 SHAPE=
   1 GPT2TENSOR-LAYER--ROLE:FC-W LAYER-SHAPE 2 4 16 1 1 SHAPE=
   0 GPT2TENSOR-LAYER--ROLE:MPROJ-W LAYER-SHAPE 2 16 4 1 1 SHAPE=
   0 GPT2TENSOR-LAYER--ROLE:MASK LAYER-SHAPE 4 1 1 8 8 SHAPE=
   GPT2TENSOR:COUNT 30 T=
   drop ;

\ ---- 3. declared orientation + the typed adapter identity -------------------------
: T-ORIENT ( -- )
   GPT2TENSOR-GLOBAL--ROLE:WTE GLOBAL-ORIENTATION PLAIN?
   GPT2TENSOR-GLOBAL--ROLE:WPE GLOBAL-ORIENTATION PLAIN?
   GPT2TENSOR-GLOBAL--ROLE:LNF-G GLOBAL-ORIENTATION PLAIN?
   GPT2TENSOR-GLOBAL--ROLE:LNF-B GLOBAL-ORIENTATION PLAIN?
   CFG-A
   GPT2TENSOR-LAYER--ROLE:QKV-W LAYER-ORIENTATION CONV1D?
   GPT2TENSOR-LAYER--ROLE:APROJ-W LAYER-ORIENTATION CONV1D?
   GPT2TENSOR-LAYER--ROLE:FC-W LAYER-ORIENTATION CONV1D?
   GPT2TENSOR-LAYER--ROLE:MPROJ-W LAYER-ORIENTATION CONV1D?
   GPT2TENSOR-LAYER--ROLE:LN1-G LAYER-ORIENTATION PLAIN?
   GPT2TENSOR-LAYER--ROLE:LN1-B LAYER-ORIENTATION PLAIN?
   GPT2TENSOR-LAYER--ROLE:MASK LAYER-ORIENTATION PLAIN?
   GPT2TENSOR-LAYER--ROLE:QKV-B LAYER-ORIENTATION PLAIN?
   GPT2TENSOR-LAYER--ROLE:APROJ-B LAYER-ORIENTATION PLAIN?
   GPT2TENSOR-LAYER--ROLE:LN2-G LAYER-ORIENTATION PLAIN?
   GPT2TENSOR-LAYER--ROLE:LN2-B LAYER-ORIENTATION PLAIN?
   GPT2TENSOR-LAYER--ROLE:FC-B LAYER-ORIENTATION PLAIN?
   GPT2TENSOR-LAYER--ROLE:MPROJ-B LAYER-ORIENTATION PLAIN?
   drop ;

: T-FORMAT ( -- )
   GPT2TENSOR:FORMAT-ID MODEL-ADAPTER:HF-GPT2 MODEL-ADAPTER:EQ TTRUE ;

\ ---- 4. checked arithmetic at its exact boundaries ---------------------------------
: REJECT-SHAPE-OVERFLOW ( -- )
   CFG-WIDE 0 GPT2TENSOR-LAYER--ROLE:QKV-W LAYER-SHAPE
   drop drop drop drop drop drop ;

: REJECT-COUNT-OVERFLOW ( -- )
   CFG-DEEP GPT2TENSOR:COUNT drop drop ;

: REJECT-COUNT-EDGE ( -- )
   CFG-DEEP-OVER GPT2TENSOR:COUNT drop drop ;

: REJECT-MUL-EDGE ( -- )
   CFG-WIDE-OVER 0 GPT2TENSOR-LAYER--ROLE:FC-B LAYER-SHAPE
   drop drop drop drop drop drop ;

: REJECT-SQUARE-EDGE ( -- )
   MAX-SQUARE-ROOT 1 + CFG-CTX 0 GPT2TENSOR-LAYER--ROLE:MASK LAYER-SHAPE
   drop drop drop drop drop drop ;

: REJECT-LARGE-MASK ( -- )
   CFG-MASK4E9 0 GPT2TENSOR-LAYER--ROLE:MASK LAYER-SHAPE
   drop drop drop drop drop drop ;

: REJECT-LARGE-QKV ( -- )
   CFG-NE40 0 GPT2TENSOR-LAYER--ROLE:QKV-W LAYER-SHAPE
   drop drop drop drop drop drop ;

: T-EXTENT ( -- )
   [: REJECT-SHAPE-OVERFLOW ;] GPT2TENSOR:E-SIZE TTHROWSQ
   [: REJECT-COUNT-OVERFLOW ;] GPT2TENSOR:E-SIZE TTHROWSQ
   [: REJECT-LARGE-MASK ;] GPT2TENSOR:E-SIZE TTHROWSQ
   [: REJECT-LARGE-QKV ;] GPT2TENSOR:E-SIZE TTHROWSQ ;

: T-EDGES ( -- )
   \ census: the largest nlayer is accepted with the exact count...
   CFG-DEEP-MAX GPT2TENSOR:COUNT  MAX-LAYERS LAYER-ROLE-COUNT * GLOBAL-COUNT +  T= drop
   \ ...and one more layer rejects.
   [: REJECT-COUNT-EDGE ;] GPT2TENSOR:E-SIZE TTHROWSQ
   \ 4*nembd: the largest nembd passes with the exact product...
   CFG-WIDE-MAX 0 GPT2TENSOR-LAYER--ROLE:FC-B LAYER-SHAPE  1 MAX-EMBD FC-MUL * 1 1 1 SHAPE= drop
   \ ...and one more rejects inside the checked multiply.
   [: REJECT-MUL-EDGE ;] GPT2TENSOR:E-SIZE TTHROWSQ
   \ mask element product: the largest nctx whose square fits passes...
   MAX-SQUARE-ROOT CFG-CTX 0 GPT2TENSOR-LAYER--ROLE:MASK LAYER-SHAPE
   4 1 1 MAX-SQUARE-ROOT MAX-SQUARE-ROOT SHAPE= drop
   \ ...and one more rejects on the full product, not any single dim.
   [: REJECT-SQUARE-EDGE ;] GPT2TENSOR:E-SIZE TTHROWSQ ;

\ ---- 5. LAYER-ID range ----------------------------------------------------------
: REJECT-NEGATIVE-LAYER ( -- )
   CFG-A -1 GPT2TENSOR:LAYER-ID drop drop ;

: REJECT-LARGE-LAYER ( -- )
   CFG-A 2 GPT2TENSOR:LAYER-ID drop drop ;

: T-LAYER-RANGE ( -- )
   [: REJECT-NEGATIVE-LAYER ;] GPT2TENSOR:E-LAYER TTHROWSQ
   [: REJECT-LARGE-LAYER ;] GPT2TENSOR:E-LAYER TTHROWSQ
   CFG-A
   0 GPT2TENSOR-LAYER--ROLE:LN1-G LAYER-SLOT 4 SLOT=
   1 GPT2TENSOR-LAYER--ROLE:MPROJ-B LAYER-SLOT 29 SLOT=
   drop ;

\ ---- 6. slot layout + bijectivity on the tiny geometry ------------------------------
30 constant TINY-CENSUS
variable HIT-I

: SLOTS-RESET ( -- )
   0 HIT-I ! ;

: MARK ( CAD-NUM:index -- )
   HIT-I @ SLOT=
   1 HIT-I +! ;

: SLOTS-COMPLETE ( -- )
   HIT-I @ TINY-CENSUS T= ;

: MARK-GLOBAL ( MDLCFG:mcfg GPT2TENSOR:global-role -- MDLCFG:mcfg )
   GLOBAL-SLOT MARK ;

: MARK-ROLE ( MDLCFG:mcfg n GPT2TENSOR:layer-role -- MDLCFG:mcfg )
   LAYER-SLOT MARK ;

: MARK-LAYER ( MDLCFG:mcfg n -- MDLCFG:mcfg ) {: l:n :}
   l GPT2TENSOR-LAYER--ROLE:LN1-G MARK-ROLE
   l GPT2TENSOR-LAYER--ROLE:LN1-B MARK-ROLE
   l GPT2TENSOR-LAYER--ROLE:MASK MARK-ROLE
   l GPT2TENSOR-LAYER--ROLE:QKV-W MARK-ROLE
   l GPT2TENSOR-LAYER--ROLE:QKV-B MARK-ROLE
   l GPT2TENSOR-LAYER--ROLE:APROJ-W MARK-ROLE
   l GPT2TENSOR-LAYER--ROLE:APROJ-B MARK-ROLE
   l GPT2TENSOR-LAYER--ROLE:LN2-G MARK-ROLE
   l GPT2TENSOR-LAYER--ROLE:LN2-B MARK-ROLE
   l GPT2TENSOR-LAYER--ROLE:FC-W MARK-ROLE
   l GPT2TENSOR-LAYER--ROLE:FC-B MARK-ROLE
   l GPT2TENSOR-LAYER--ROLE:MPROJ-W MARK-ROLE
   l GPT2TENSOR-LAYER--ROLE:MPROJ-B MARK-ROLE ;

: T-SLOTS ( -- )
   CFG-A
   GPT2TENSOR-GLOBAL--ROLE:WTE GLOBAL-SLOT 0 SLOT=
   GPT2TENSOR-GLOBAL--ROLE:WPE GLOBAL-SLOT 1 SLOT=
   GPT2TENSOR-GLOBAL--ROLE:LNF-G GLOBAL-SLOT 2 SLOT=
   GPT2TENSOR-GLOBAL--ROLE:LNF-B GLOBAL-SLOT 3 SLOT=
   0 GPT2TENSOR-LAYER--ROLE:MPROJ-B LAYER-SLOT 16 SLOT=
   1 GPT2TENSOR-LAYER--ROLE:LN1-G LAYER-SLOT 17 SLOT=
   1 GPT2TENSOR-LAYER--ROLE:QKV-W LAYER-SLOT 20 SLOT=
   drop
   CFG-REAL 11 GPT2TENSOR-LAYER--ROLE:MPROJ-B LAYER-SLOT 159 SLOT=
   drop ;

: T-BIJECT ( -- )
   SLOTS-RESET
   CFG-A
   GPT2TENSOR-GLOBAL--ROLE:WTE MARK-GLOBAL
   GPT2TENSOR-GLOBAL--ROLE:WPE MARK-GLOBAL
   GPT2TENSOR-GLOBAL--ROLE:LNF-G MARK-GLOBAL
   GPT2TENSOR-GLOBAL--ROLE:LNF-B MARK-GLOBAL
   0 MARK-LAYER
   1 MARK-LAYER
   GPT2TENSOR:COUNT TINY-CENSUS T=
   drop
   SLOTS-COMPLETE ;

\ ---- 7. THE IDENTITY FIXTURE ---------------------------------------------------------
\ A layer-id minted against config A, resolved against same-nlayer config B.
: CFG-A-LAYER-ID-1 ( -- GPT2TENSOR:layer-id )
   CFG-A 1 GPT2TENSOR:LAYER-ID >r drop r> ;

: CFG-A-LAYER-ID-0 ( -- GPT2TENSOR:layer-id )
   CFG-A 0 GPT2TENSOR:LAYER-ID >r drop r> ;

: REJECT-FOREIGN-MIDDLE ( -- )
   CFG-B CFG-A-LAYER-ID-1 GPT2TENSOR-LAYER--ROLE:QKV-W GPT2TENSOR-TENSOR--ID:LAYER GPT2TENSOR:SLOT
   drop drop ;

: REJECT-FOREIGN-LOW ( -- )
   CFG-B CFG-A-LAYER-ID-0 GPT2TENSOR-LAYER--ROLE:LN1-G GPT2TENSOR-TENSOR--ID:LAYER GPT2TENSOR:SLOT
   drop drop ;

: T-FOREIGN ( -- )
   \ both foreign slots (20 and 4) are strictly inside B's census (30), so a
   \ bounds check can never be the rejector: E-CONFIG fires before slot math.
   CFG-B GPT2TENSOR:COUNT 30 T= drop
   [: REJECT-FOREIGN-MIDDLE ;] GPT2TENSOR:E-CONFIG TTHROWSQ
   [: REJECT-FOREIGN-LOW ;] GPT2TENSOR:E-CONFIG TTHROWSQ
   \ positive control: the same layer IDs resolve on their OWN config.
   CFG-A CFG-A-LAYER-ID-1 GPT2TENSOR-LAYER--ROLE:QKV-W GPT2TENSOR-TENSOR--ID:LAYER GPT2TENSOR:SLOT 20 SLOT=
   CFG-A-LAYER-ID-0 GPT2TENSOR-LAYER--ROLE:LN1-G GPT2TENSOR-TENSOR--ID:LAYER GPT2TENSOR:SLOT 4 SLOT=
   drop ;

\ ---- 8. the re-MAKE forgery caveat ----------------------------------------------------
\ UNMAKE hands back the genuine proof, so a holder can rebuild a layer-id with
\ an arbitrary index around LAYER-ID (the sealed-destructure gap). The rebuilt
\ identity passes CFGKEY= but its index must reject E-LAYER.
variable FORGE-INDEX

: FORGE-LAYER-ID ( n -- GPT2TENSOR:layer-id )
   FORGE-INDEX !
   CFG-A-LAYER-ID-0 GPT2TENSOR-LAYER--ID:UNMAKE {: i:n tok:layer-proof :}
   FORGE-INDEX @ tok GPT2TENSOR-LAYER--ID:MAKE ;

: REJECT-FORGED-LARGE ( -- )
   CFG-A 999 FORGE-LAYER-ID GPT2TENSOR-LAYER--ROLE:LN1-G GPT2TENSOR-TENSOR--ID:LAYER GPT2TENSOR:SLOT
   drop drop ;

: REJECT-FORGED-NEGATIVE ( -- )
   -1 FORGE-LAYER-ID GPT2TENSOR-LAYER--ROLE:LN1-G GPT2TENSOR-TENSOR--ID:LAYER
   NAME-BUF-1 NAME-CAP GPT2TENSOR:COPY-NAME? LEN-OF drop ;

: T-FORGED ( -- )
   [: REJECT-FORGED-LARGE ;] GPT2TENSOR:E-LAYER TTHROWSQ
   [: REJECT-FORGED-NEGATIVE ;] GPT2TENSOR:E-LAYER TTHROWSQ ;

T-RESET

T-GLOBAL-NAMES
T-LAYER-NAMES
T-LARGEST-INDEX
T-SMALL-BUF
T-INTERLEAVE
T-SHAPES-R
T-SHAPES-TINY
T-ORIENT
T-FORMAT
T-EXTENT
T-EDGES
T-LAYER-RANGE
T-SLOTS
T-BIJECT
T-FOREIGN
T-FORGED

\ ---- 9. checker negatives -------------------------------------------------------
\ SLOT preserves the nominal index role through the public boundary.
s" SLOT-OK ( MDLCFG:mcfg GPT2TENSOR:tensor-id -- MDLCFG:mcfg CAD-NUM:index ) GPT2TENSOR:SLOT" YES
s" SLOT-RAW ( MDLCFG:mcfg GPT2TENSOR:tensor-id -- MDLCFG:mcfg n ) GPT2TENSOR:SLOT" NO
\ a global tensor-id takes exactly a global-role; a layer cannot ride along...
s" GLOBAL-OK ( GPT2TENSOR:global-role -- GPT2TENSOR:tensor-id ) GPT2TENSOR-TENSOR--ID:GLOBAL" YES
s" GLOBAL-WITH-LAYER ( GPT2TENSOR:layer-id GPT2TENSOR:global-role -- GPT2TENSOR:tensor-id ) GPT2TENSOR-TENSOR--ID:GLOBAL" NO
\ ...a layer tensor-id requires its layer-id, in declared field order...
s" LAYER-OK ( GPT2TENSOR:layer-id GPT2TENSOR:layer-role -- GPT2TENSOR:tensor-id ) GPT2TENSOR-TENSOR--ID:LAYER" YES
s" LAYER-NO-ID ( GPT2TENSOR:layer-role -- GPT2TENSOR:tensor-id ) GPT2TENSOR-TENSOR--ID:LAYER" NO
s" LAYER-WRONG-ORDER ( GPT2TENSOR:layer-role GPT2TENSOR:layer-id -- GPT2TENSOR:tensor-id ) GPT2TENSOR-TENSOR--ID:LAYER" NO
\ ...and the role families do not cross.
s" LAYER-WRONG-ROLE ( GPT2TENSOR:layer-id GPT2TENSOR:global-role -- GPT2TENSOR:tensor-id ) GPT2TENSOR-TENSOR--ID:LAYER" NO
s" GLOBAL-WRONG-ROLE ( GPT2TENSOR:layer-role -- GPT2TENSOR:tensor-id ) GPT2TENSOR-TENSOR--ID:GLOBAL" NO
\ the layer-id MAKE certifies only with a genuine GPT2TENSOR proof...
s" LAYER-ID-OK ( MDLCFG:cfgkey n GPT2TENSOR:layer-proof -- GPT2TENSOR:layer-id ) GPT2TENSOR-LAYER--ID:MAKE" YES
s" LAYER-ID-RAW-PROOF ( MDLCFG:cfgkey n n -- GPT2TENSOR:layer-id ) GPT2TENSOR-LAYER--ID:MAKE" NO
\ ...MDLCFG's proof cannot substitute (cross-package proof domains), nor can
\ GPT2TENSOR's proof seal an mcfg...
s" LAYER-ID-WRONG-PROOF ( MDLCFG:cfgkey n MDLCFG:cfg-proof -- GPT2TENSOR:layer-id ) GPT2TENSOR-LAYER--ID:MAKE" NO
s" CONFIG-WRONG-PROOF ( n MAKI:dtype n n n n n bool n n MDLCFG:arch MDLCFG:cfgkey GPT2TENSOR:layer-proof -- MDLCFG:mcfg ) MDLCFG-MCFG:MAKE" NO
\ ...raw cells are not a cfgkey, and the private mint is unresolvable outside.
s" LAYER-ID-RAW-KEY ( n n n n n GPT2TENSOR:layer-proof -- GPT2TENSOR:layer-id ) GPT2TENSOR-LAYER--ID:MAKE" NO
s" PRIVATE-MINT ( -- GPT2TENSOR:layer-proof ) GPT2TENSOR:MINT-LAYER-PROOF" UNK
s" BARE-PRIVATE-MINT ( -- GPT2TENSOR:layer-proof ) MINT-LAYER-PROOF" UNK
\ a tensor-id is nominal: neither an orientation value nor a raw n can stand in the
\ COPY-NAME? tensor-id slot, and the genuine signature certifies.
s" NAME-WRONG-TYPE ( GPT2TENSOR:orientation ptr u8 n -- option<n> ) GPT2TENSOR:COPY-NAME?" NO
s" NAME-RAW-ID ( n ptr u8 n -- option<n> ) GPT2TENSOR:COPY-NAME?" NO
s" NAME-OK ( GPT2TENSOR:tensor-id ptr u8 n -- option<n> ) GPT2TENSOR:COPY-NAME?" YES

T-REPORT

;package
