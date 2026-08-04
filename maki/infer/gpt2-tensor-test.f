\ gpt2-tensor-test.f - GPT2 acceptance (rev-3 S6a).
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
\      everywhere else;
\   4. checked arithmetic at its exact boundaries, every bound derived from
\      MAX-N and this suite's own role-table constants: the largest nlayer
\      COUNT accepts and that plus one rejecting; the largest nembd
\      through the 4*nembd multiply and that plus one rejecting; the largest
\      nctx whose mask element product fits and that plus one rejecting; plus
\      the measured full-product escapes (nctx 4e9 mask, nembd 2^40 qkv) and
\      the MAX-N extremes, all E-SIZE;
\   5. LAYER-ID range rejects (E-LAYER) and boundary accepts;
\   6. SLOT bijectivity on the tiny geometry: every slot in
\      [0, COUNT) hit exactly once and TENSOR-ID-FOR-SLOT returns the exact
\      tensor identity at every slot; one-over rejects E-SLOT;
\   7. the re-MAKE forgery caveat: a layer-id rebuilt around LAYER-ID with a
\      negative or one-over index rejects E-LAYER through SLOT against the
\      consuming config;
\   8. checker negatives: a global tensor-id cannot carry a layer and a layer
\      tensor-id cannot omit one (by constructor arity/types); reordered layer fields
\      reject, role families do not cross, cfg-proof in the layer-id slot
\      rejects (and layer-proof in the config slot), the removed record
\      arities reject, and the private mint is unresolvable outside the package.

require lib/prelude.f
require lib/adt/option.f
require lib/cad-num-arithmetic.f
require lib/test.f
require test/checker-assert.f
require maki/infer/gpt2-tensor.f

package GPT2-TENSOR-TEST

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

\ ---- fixture configs (all through the sole GPT2 constructor) ------------------
: DT0 ( -- MAKI:datatype )  MAKI-DATATYPE:DF32 ;
: EPS0 ( -- r )  0.00001 ;

\ the real GPT-2 124M geometry.
: CFG-REAL ( -- GPT2:config )
   DT0 1024 50257 12 768 12 true 50256 50256 EPS0 true GPT2:BUILD ;

\ tiny geometry: nlayer 2, nembd 4, nhead 2, nctx 8, nvocab 5 - every 2-d
\ shape distinct and non-square, so a transposed or swapped dim cannot pass.
: CFG-A ( -- GPT2:config )
   DT0 8 5 2 4 2 true 4 4 EPS0 true GPT2:BUILD ;

\ extreme-but-valid gpt2 geometry: BUILD accepts nembd = MAX-N (nvocab 1,
\ nctx 1, nhead 1), so the 3*nembd qkv product must be the overflow rejector.
: CFG-WIDE ( -- GPT2:config )
   DT0 1 1 1 MAXN 1 true 0 0 EPS0 true GPT2:BUILD ;

\ the same shape at the exact 4*nembd boundary and one past it.
: CFG-WIDE-MAX ( -- GPT2:config )
   DT0 1 1 1 MAX-EMBD 1 true 0 0 EPS0 true GPT2:BUILD ;

: CFG-WIDE-OVER ( -- GPT2:config )
   DT0 1 1 1 MAX-EMBD 1 + 1 true 0 0 EPS0 true GPT2:BUILD ;

\ GPT2 owns model geometry, not this product census. These configs
\ therefore build through the production constructor; COUNT owns the overflow
\ rejection and its exact boundary pair.
: CFG-DEEP ( -- GPT2:config )
   DT0 1 1 MAXN 1 1 true 0 0 EPS0 true GPT2:BUILD ;

: CFG-DEEP-MAX ( -- GPT2:config )
   DT0 1 1 MAX-LAYERS 1 1 true 0 0 EPS0 true GPT2:BUILD ;

: CFG-DEEP-OVER ( -- GPT2:config )
   DT0 1 1 MAX-LAYERS 1 + 1 1 true 0 0 EPS0 true GPT2:BUILD ;

\ gpt2 arm with everything minimal but nctx, for the mask element product.
: CFG-CTX ( n -- GPT2:config ) {: cx:n :}
   DT0 cx 1 1 1 1 true 0 0 EPS0 true GPT2:BUILD ;

\ the two measured full-product escapes: every per-dim product fits, the
\ pair product does not.
: CFG-MASK4E9 ( -- GPT2:config )
   DT0 4000000000 5 1 768 12 true 4 4 EPS0 true GPT2:BUILD ;

: CFG-NE40 ( -- GPT2:config )
   DT0 1 1 1 $10000000000 1 true 0 0 EPS0 true GPT2:BUILD ;

\ ---- typed probes over the public surface -------------------------------------
64 constant NAME-CAP
create NAME-BUF-1 NAME-CAP allot
create NAME-BUF-2 NAME-CAP allot

: CHECK-GLOBAL-NAME ( GPT2:global-role -- ptr u8 n )           \ copied into NAME-BUF-1
   GPT2-TENSOR--ID:GLOBAL NAME-BUF-1 NAME-CAP GPT2:COPY-NAME? LEN-OF
   NAME-BUF-1 swap ;

: CHECK-LAYER-NAME ( GPT2:config n GPT2:layer-role -- GPT2:config ptr u8 n )
   {: br:GPT2:layer-role :}
   GPT2:LAYER-ID br GPT2-TENSOR--ID:LAYER
   NAME-BUF-1 NAME-CAP GPT2:COPY-NAME? LEN-OF
   NAME-BUF-1 swap ;

: COPY-LAYER-NAME ( GPT2:config n GPT2:layer-role -- GPT2:config n )
   {: br:GPT2:layer-role :}
   GPT2:LAYER-ID br GPT2-TENSOR--ID:LAYER
   NAME-BUF-1 NAME-CAP GPT2:COPY-NAME? LEN-OF ;

: CHECK-GLOBAL-NAME-2 ( GPT2:global-role -- ptr u8 n )         \ copied into NAME-BUF-2
   GPT2-TENSOR--ID:GLOBAL NAME-BUF-2 NAME-CAP GPT2:COPY-NAME? LEN-OF
   NAME-BUF-2 swap ;

: GLOBAL-SHAPE ( GPT2:config GPT2:global-role -- GPT2:config n n n n n )
   GPT2-TENSOR--ID:GLOBAL GPT2:SHAPE ;

: LAYER-SHAPE ( GPT2:config n GPT2:layer-role -- GPT2:config n n n n n )
   {: br:GPT2:layer-role :}
   GPT2:LAYER-ID br GPT2-TENSOR--ID:LAYER GPT2:SHAPE ;

: GLOBAL-SLOT ( GPT2:config GPT2:global-role -- GPT2:config CAD-NUM:index )
   GPT2-TENSOR--ID:GLOBAL GPT2:SLOT ;

: LAYER-SLOT ( GPT2:config n GPT2:layer-role -- GPT2:config CAD-NUM:index )
   {: br:GPT2:layer-role :}
   GPT2:LAYER-ID br GPT2-TENSOR--ID:LAYER GPT2:SLOT ;

using CAD-NUM

: TEST-INDEX ( n -- CAD-NUM:index )
   INDEX
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                            negative OF GPT2:E-SIZE throw ENDOF
      zero OF GPT2:E-SIZE throw ENDOF  overflow OF GPT2:E-SIZE throw ENDOF
      underflow OF GPT2:E-SIZE throw ENDOF
      bad-alignment OF GPT2:E-SIZE throw ENDOF
      misaligned OF GPT2:E-SIZE throw ENDOF
   ;MATCH ;

: SLOT= ( CAD-NUM:index n -- )
   TEST-INDEX INDEX= TTRUE ;

;using

variable EXPECT-LEN

: EXPECT-GLOBAL ( GPT2:global-role -- )
   GPT2-TENSOR--ID:GLOBAL
   NAME-BUF-1 NAME-CAP GPT2:COPY-NAME? LEN-OF EXPECT-LEN ! ;

: EXPECT-LAYER ( GPT2:config n GPT2:layer-role -- GPT2:config )
   {: br:GPT2:layer-role :}
   GPT2:LAYER-ID br GPT2-TENSOR--ID:LAYER
   NAME-BUF-1 NAME-CAP GPT2:COPY-NAME? LEN-OF EXPECT-LEN ! ;

: ACTUAL-NAME= ( GPT2:tensor-id -- )
   NAME-BUF-2 NAME-CAP GPT2:COPY-NAME? LEN-OF {: n:n :}
   NAME-BUF-1 EXPECT-LEN @ NAME-BUF-2 n T$= ;

: GLOBAL-ORIENTATION ( GPT2:global-role -- GPT2:orientation )
   GPT2-TENSOR--ID:GLOBAL GPT2:ORIENTATION ;

: LAYER-ORIENTATION
   ( GPT2:config GPT2:layer-role -- GPT2:config GPT2:orientation )
   {: br:GPT2:layer-role :}
   0 GPT2:LAYER-ID br GPT2-TENSOR--ID:LAYER GPT2:ORIENTATION ;

: SHAPE= ( n n n n n n n n n n -- ) {: r0:n a0:n a1:n a2:n a3:n r1:n b0:n b1:n b2:n b3:n :}
   r0 r1 T=
   a0 b0 T=  a1 b1 T=  a2 b2 T=  a3 b3 T= ;

: CONV1D? ( GPT2:orientation -- )
   GPT2-ORIENTATION:CONV1D GPT2-ORIENTATION:EQ TTRUE ;

: PLAIN? ( GPT2:orientation -- )
   GPT2-ORIENTATION:PLAIN GPT2-ORIENTATION:EQ TTRUE ;

\ ---- 1. HF tensor-name pins via COPY-NAME? ---------------------------------------
: T-GLOBAL-NAMES ( -- )
   GPT2-GLOBAL--ROLE:WTE CHECK-GLOBAL-NAME s" wte.weight" T$=
   GPT2-GLOBAL--ROLE:WPE CHECK-GLOBAL-NAME s" wpe.weight" T$=
   GPT2-GLOBAL--ROLE:LNF-G CHECK-GLOBAL-NAME s" ln_f.weight" T$=
   GPT2-GLOBAL--ROLE:LNF-B CHECK-GLOBAL-NAME s" ln_f.bias" T$= ;

: T-LAYER-NAMES ( -- )
   CFG-REAL
   0 GPT2-LAYER--ROLE:LN1-G CHECK-LAYER-NAME s" h.0.ln_1.weight" T$=
   0 GPT2-LAYER--ROLE:LN1-B CHECK-LAYER-NAME s" h.0.ln_1.bias" T$=
   0 GPT2-LAYER--ROLE:MASK CHECK-LAYER-NAME s" h.0.attn.bias" T$=
   0 GPT2-LAYER--ROLE:QKV-W CHECK-LAYER-NAME s" h.0.attn.c_attn.weight" T$=
   0 GPT2-LAYER--ROLE:QKV-B CHECK-LAYER-NAME s" h.0.attn.c_attn.bias" T$=
   0 GPT2-LAYER--ROLE:APROJ-W CHECK-LAYER-NAME s" h.0.attn.c_proj.weight" T$=
   0 GPT2-LAYER--ROLE:APROJ-B CHECK-LAYER-NAME s" h.0.attn.c_proj.bias" T$=
   0 GPT2-LAYER--ROLE:LN2-G CHECK-LAYER-NAME s" h.0.ln_2.weight" T$=
   0 GPT2-LAYER--ROLE:LN2-B CHECK-LAYER-NAME s" h.0.ln_2.bias" T$=
   0 GPT2-LAYER--ROLE:FC-W CHECK-LAYER-NAME s" h.0.mlp.c_fc.weight" T$=
   0 GPT2-LAYER--ROLE:FC-B CHECK-LAYER-NAME s" h.0.mlp.c_fc.bias" T$=
   0 GPT2-LAYER--ROLE:MPROJ-W CHECK-LAYER-NAME s" h.0.mlp.c_proj.weight" T$=
   0 GPT2-LAYER--ROLE:MPROJ-B CHECK-LAYER-NAME s" h.0.mlp.c_proj.bias" T$=
   11 GPT2-LAYER--ROLE:MPROJ-B CHECK-LAYER-NAME s" h.11.mlp.c_proj.bias" T$=
   drop ;

\ the largest index LAYER-ID can ever mint (MAX-LAYERS - 1) renders at the 39-byte
\ extreme; the digits below are that derived index, pinned byte-exact.
: T-LARGEST-INDEX ( -- )
   CFG-DEEP-MAX MAX-LAYERS 1 - GPT2-LAYER--ROLE:QKV-W CHECK-LAYER-NAME
   dup 39 T=
   s" h.709490156681136599.attn.c_attn.weight" T$=
   drop ;

\ a too-small caller buffer answers NONE and writes nothing: NAME-BUF-1 still
\ holds the wte name copied before the refused longer copies; capacity equal
\ to the name length is the exact accept boundary.
: T-SMALL-BUF ( -- )
   GPT2-GLOBAL--ROLE:WTE CHECK-GLOBAL-NAME s" wte.weight" T$=
   GPT2-GLOBAL--ROLE:LNF-G GPT2-TENSOR--ID:GLOBAL NAME-BUF-1 9 GPT2:COPY-NAME? OPT-NONE
   GPT2-GLOBAL--ROLE:LNF-G GPT2-TENSOR--ID:GLOBAL NAME-BUF-1 0 GPT2:COPY-NAME? OPT-NONE
   NAME-BUF-1 10 s" wte.weight" T$=
   GPT2-GLOBAL--ROLE:WTE GPT2-TENSOR--ID:GLOBAL NAME-BUF-1 10 GPT2:COPY-NAME? LEN-OF 10 T= ;

\ two successive copies into two DISTINCT caller buffers leave both intact -
\ the exact interleave the old span-returning API could not survive.
: T-INTERLEAVE ( -- )
   CFG-REAL 0 GPT2-LAYER--ROLE:QKV-W COPY-LAYER-NAME
   GPT2-GLOBAL--ROLE:LNF-B CHECK-GLOBAL-NAME-2 s" ln_f.bias" T$=
   NAME-BUF-1 swap s" h.0.attn.c_attn.weight" T$=
   drop ;

\ ---- 2. shape pins: 124M then tiny ----------------------------------------------
: T-SHAPES-R ( -- )
   CFG-REAL
   GPT2-GLOBAL--ROLE:WTE GLOBAL-SHAPE 2 50257 768 1 1 SHAPE=
   GPT2-GLOBAL--ROLE:WPE GLOBAL-SHAPE 2 1024 768 1 1 SHAPE=
   GPT2-GLOBAL--ROLE:LNF-G GLOBAL-SHAPE 1 768 1 1 1 SHAPE=
   GPT2-GLOBAL--ROLE:LNF-B GLOBAL-SHAPE 1 768 1 1 1 SHAPE=
   0 GPT2-LAYER--ROLE:LN1-G LAYER-SHAPE 1 768 1 1 1 SHAPE=
   0 GPT2-LAYER--ROLE:LN1-B LAYER-SHAPE 1 768 1 1 1 SHAPE=
   0 GPT2-LAYER--ROLE:MASK LAYER-SHAPE 4 1 1 1024 1024 SHAPE=
   0 GPT2-LAYER--ROLE:QKV-W LAYER-SHAPE 2 768 2304 1 1 SHAPE=
   0 GPT2-LAYER--ROLE:QKV-B LAYER-SHAPE 1 2304 1 1 1 SHAPE=
   0 GPT2-LAYER--ROLE:APROJ-W LAYER-SHAPE 2 768 768 1 1 SHAPE=
   0 GPT2-LAYER--ROLE:APROJ-B LAYER-SHAPE 1 768 1 1 1 SHAPE=
   0 GPT2-LAYER--ROLE:LN2-G LAYER-SHAPE 1 768 1 1 1 SHAPE=
   0 GPT2-LAYER--ROLE:LN2-B LAYER-SHAPE 1 768 1 1 1 SHAPE=
   0 GPT2-LAYER--ROLE:FC-W LAYER-SHAPE 2 768 3072 1 1 SHAPE=
   0 GPT2-LAYER--ROLE:FC-B LAYER-SHAPE 1 3072 1 1 1 SHAPE=
   0 GPT2-LAYER--ROLE:MPROJ-W LAYER-SHAPE 2 3072 768 1 1 SHAPE=
   0 GPT2-LAYER--ROLE:MPROJ-B LAYER-SHAPE 1 768 1 1 1 SHAPE=
   GPT2:COUNT 160 T=
   drop ;

: T-SHAPES-TINY ( -- )
   CFG-A
   GPT2-GLOBAL--ROLE:WTE GLOBAL-SHAPE 2 5 4 1 1 SHAPE=
   GPT2-GLOBAL--ROLE:WPE GLOBAL-SHAPE 2 8 4 1 1 SHAPE=
   1 GPT2-LAYER--ROLE:QKV-W LAYER-SHAPE 2 4 12 1 1 SHAPE=
   1 GPT2-LAYER--ROLE:QKV-B LAYER-SHAPE 1 12 1 1 1 SHAPE=
   1 GPT2-LAYER--ROLE:FC-W LAYER-SHAPE 2 4 16 1 1 SHAPE=
   0 GPT2-LAYER--ROLE:MPROJ-W LAYER-SHAPE 2 16 4 1 1 SHAPE=
   0 GPT2-LAYER--ROLE:MASK LAYER-SHAPE 4 1 1 8 8 SHAPE=
   GPT2:COUNT 30 T=
   drop ;

\ ---- 3. declared orientation -------------------------------------------------------
: T-ORIENT ( -- )
   GPT2-GLOBAL--ROLE:WTE GLOBAL-ORIENTATION PLAIN?
   GPT2-GLOBAL--ROLE:WPE GLOBAL-ORIENTATION PLAIN?
   GPT2-GLOBAL--ROLE:LNF-G GLOBAL-ORIENTATION PLAIN?
   GPT2-GLOBAL--ROLE:LNF-B GLOBAL-ORIENTATION PLAIN?
   CFG-A
   GPT2-LAYER--ROLE:QKV-W LAYER-ORIENTATION CONV1D?
   GPT2-LAYER--ROLE:APROJ-W LAYER-ORIENTATION CONV1D?
   GPT2-LAYER--ROLE:FC-W LAYER-ORIENTATION CONV1D?
   GPT2-LAYER--ROLE:MPROJ-W LAYER-ORIENTATION CONV1D?
   GPT2-LAYER--ROLE:LN1-G LAYER-ORIENTATION PLAIN?
   GPT2-LAYER--ROLE:LN1-B LAYER-ORIENTATION PLAIN?
   GPT2-LAYER--ROLE:MASK LAYER-ORIENTATION PLAIN?
   GPT2-LAYER--ROLE:QKV-B LAYER-ORIENTATION PLAIN?
   GPT2-LAYER--ROLE:APROJ-B LAYER-ORIENTATION PLAIN?
   GPT2-LAYER--ROLE:LN2-G LAYER-ORIENTATION PLAIN?
   GPT2-LAYER--ROLE:LN2-B LAYER-ORIENTATION PLAIN?
   GPT2-LAYER--ROLE:FC-B LAYER-ORIENTATION PLAIN?
   GPT2-LAYER--ROLE:MPROJ-B LAYER-ORIENTATION PLAIN?
   drop ;

\ ---- 4. checked arithmetic at its exact boundaries ---------------------------------
: REJECT-SHAPE-OVERFLOW ( -- )
   CFG-WIDE 0 GPT2-LAYER--ROLE:QKV-W LAYER-SHAPE
   drop drop drop drop drop drop ;

: REJECT-COUNT-OVERFLOW ( -- )
   CFG-DEEP GPT2:COUNT drop drop ;

: REJECT-COUNT-EDGE ( -- )
   CFG-DEEP-OVER GPT2:COUNT drop drop ;

: REJECT-MUL-EDGE ( -- )
   CFG-WIDE-OVER 0 GPT2-LAYER--ROLE:FC-B LAYER-SHAPE
   drop drop drop drop drop drop ;

: REJECT-SQUARE-EDGE ( -- )
   MAX-SQUARE-ROOT 1 + CFG-CTX 0 GPT2-LAYER--ROLE:MASK LAYER-SHAPE
   drop drop drop drop drop drop ;

: REJECT-LARGE-MASK ( -- )
   CFG-MASK4E9 0 GPT2-LAYER--ROLE:MASK LAYER-SHAPE
   drop drop drop drop drop drop ;

: REJECT-LARGE-QKV ( -- )
   CFG-NE40 0 GPT2-LAYER--ROLE:QKV-W LAYER-SHAPE
   drop drop drop drop drop drop ;

: T-EXTENT ( -- )
   [: REJECT-SHAPE-OVERFLOW ;] GPT2:E-SIZE TTHROWSQ
   [: REJECT-COUNT-OVERFLOW ;] GPT2:E-SIZE TTHROWSQ
   [: REJECT-LARGE-MASK ;] GPT2:E-SIZE TTHROWSQ
   [: REJECT-LARGE-QKV ;] GPT2:E-SIZE TTHROWSQ ;

: T-EDGES ( -- )
   \ census: the largest nlayer is accepted with the exact count...
   CFG-DEEP-MAX GPT2:COUNT  MAX-LAYERS LAYER-ROLE-COUNT * GLOBAL-COUNT +  T= drop
   \ ...and one more layer rejects.
   [: REJECT-COUNT-EDGE ;] GPT2:E-SIZE TTHROWSQ
   \ 4*nembd: the largest nembd passes with the exact product...
   CFG-WIDE-MAX 0 GPT2-LAYER--ROLE:FC-B LAYER-SHAPE  1 MAX-EMBD FC-MUL * 1 1 1 SHAPE= drop
   \ ...and one more rejects inside the checked multiply.
   [: REJECT-MUL-EDGE ;] GPT2:E-SIZE TTHROWSQ
   \ mask element product: the largest nctx whose square fits passes...
   MAX-SQUARE-ROOT CFG-CTX 0 GPT2-LAYER--ROLE:MASK LAYER-SHAPE
   4 1 1 MAX-SQUARE-ROOT MAX-SQUARE-ROOT SHAPE= drop
   \ ...and one more rejects on the full product, not any single dim.
   [: REJECT-SQUARE-EDGE ;] GPT2:E-SIZE TTHROWSQ ;

\ ---- 5. LAYER-ID range ----------------------------------------------------------
: REJECT-NEGATIVE-LAYER ( -- )
   CFG-A -1 GPT2:LAYER-ID drop drop ;

: REJECT-LARGE-LAYER ( -- )
   CFG-A 2 GPT2:LAYER-ID drop drop ;

: T-LAYER-RANGE ( -- )
   [: REJECT-NEGATIVE-LAYER ;] GPT2:E-LAYER TTHROWSQ
   [: REJECT-LARGE-LAYER ;] GPT2:E-LAYER TTHROWSQ
   CFG-A
   0 GPT2-LAYER--ROLE:LN1-G LAYER-SLOT 4 SLOT=
   1 GPT2-LAYER--ROLE:MPROJ-B LAYER-SLOT 29 SLOT=
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

: MARK-GLOBAL ( GPT2:config GPT2:global-role -- GPT2:config )
   {: br:GPT2:global-role :}
   br EXPECT-GLOBAL
   br GLOBAL-SLOT dup MARK
   GPT2:TENSOR-ID-FOR-SLOT ACTUAL-NAME= ;

: MARK-ROLE ( GPT2:config n GPT2:layer-role -- GPT2:config )
   {: l:n br:GPT2:layer-role :}
   l br EXPECT-LAYER
   l br LAYER-SLOT dup MARK
   GPT2:TENSOR-ID-FOR-SLOT ACTUAL-NAME= ;

: MARK-LAYER ( GPT2:config n -- GPT2:config ) {: l:n :}
   l GPT2-LAYER--ROLE:LN1-G MARK-ROLE
   l GPT2-LAYER--ROLE:LN1-B MARK-ROLE
   l GPT2-LAYER--ROLE:MASK MARK-ROLE
   l GPT2-LAYER--ROLE:QKV-W MARK-ROLE
   l GPT2-LAYER--ROLE:QKV-B MARK-ROLE
   l GPT2-LAYER--ROLE:APROJ-W MARK-ROLE
   l GPT2-LAYER--ROLE:APROJ-B MARK-ROLE
   l GPT2-LAYER--ROLE:LN2-G MARK-ROLE
   l GPT2-LAYER--ROLE:LN2-B MARK-ROLE
   l GPT2-LAYER--ROLE:FC-W MARK-ROLE
   l GPT2-LAYER--ROLE:FC-B MARK-ROLE
   l GPT2-LAYER--ROLE:MPROJ-W MARK-ROLE
   l GPT2-LAYER--ROLE:MPROJ-B MARK-ROLE ;

: T-SLOTS ( -- )
   CFG-A
   GPT2-GLOBAL--ROLE:WTE GLOBAL-SLOT 0 SLOT=
   GPT2-GLOBAL--ROLE:WPE GLOBAL-SLOT 1 SLOT=
   GPT2-GLOBAL--ROLE:LNF-G GLOBAL-SLOT 2 SLOT=
   GPT2-GLOBAL--ROLE:LNF-B GLOBAL-SLOT 3 SLOT=
   0 GPT2-LAYER--ROLE:MPROJ-B LAYER-SLOT 16 SLOT=
   1 GPT2-LAYER--ROLE:LN1-G LAYER-SLOT 17 SLOT=
   1 GPT2-LAYER--ROLE:QKV-W LAYER-SLOT 20 SLOT=
   drop
   CFG-REAL 11 GPT2-LAYER--ROLE:MPROJ-B LAYER-SLOT 159 SLOT=
   drop ;

: T-BIJECT ( -- )
   SLOTS-RESET
   CFG-A
   GPT2-GLOBAL--ROLE:WTE MARK-GLOBAL
   GPT2-GLOBAL--ROLE:WPE MARK-GLOBAL
   GPT2-GLOBAL--ROLE:LNF-G MARK-GLOBAL
   GPT2-GLOBAL--ROLE:LNF-B MARK-GLOBAL
   0 MARK-LAYER
   1 MARK-LAYER
   GPT2:COUNT TINY-CENSUS T=
   drop
   SLOTS-COMPLETE ;

: REJECT-SLOT-OVER ( -- )
   CFG-A TINY-CENSUS TEST-INDEX GPT2:TENSOR-ID-FOR-SLOT drop drop ;

: T-INVERSE-REJECT ( -- )
   [: REJECT-SLOT-OVER ;] GPT2:E-SLOT TTHROWSQ ;

: CFG-A-LAYER-ID-0 ( -- GPT2:layer-id )
   CFG-A 0 GPT2:LAYER-ID >r drop r> ;

\ ---- 7. the re-MAKE forgery caveat ----------------------------------------------------
\ UNMAKE hands back the genuine proof, so a holder can rebuild a layer-id with
\ an arbitrary index around LAYER-ID (the sealed-destructure gap). SLOT must
\ reject it against the consuming config before any slot arithmetic.
variable FORGE-INDEX

: FORGE-LAYER-ID ( n -- GPT2:layer-id )
   FORGE-INDEX !
   CFG-A-LAYER-ID-0 GPT2-LAYER--ID:UNMAKE {: i:n tok:layer-proof :}
   FORGE-INDEX @ tok GPT2-LAYER--ID:MAKE ;

: REJECT-FORGED-ONE-OVER ( -- )
   CFG-A 2 FORGE-LAYER-ID GPT2-LAYER--ROLE:LN1-G GPT2-TENSOR--ID:LAYER GPT2:SLOT
   drop drop ;

: REJECT-FORGED-NEGATIVE ( -- )
   CFG-A -1 FORGE-LAYER-ID GPT2-LAYER--ROLE:LN1-G GPT2-TENSOR--ID:LAYER GPT2:SLOT
   drop drop ;

: T-FORGED ( -- )
   [: REJECT-FORGED-ONE-OVER ;] GPT2:E-LAYER TTHROWSQ
   [: REJECT-FORGED-NEGATIVE ;] GPT2:E-LAYER TTHROWSQ ;

T-RESET

T-GLOBAL-NAMES
T-LAYER-NAMES
T-LARGEST-INDEX
T-SMALL-BUF
T-INTERLEAVE
T-SHAPES-R
T-SHAPES-TINY
T-ORIENT
T-EXTENT
T-EDGES
T-LAYER-RANGE
T-SLOTS
T-BIJECT
T-INVERSE-REJECT
T-FORGED

\ ---- 8. checker negatives -------------------------------------------------------
\ SLOT preserves the nominal index role through the public boundary.
s" SLOT-OK ( GPT2:config GPT2:tensor-id -- GPT2:config CAD-NUM:index ) GPT2:SLOT" YES
s" SLOT-RAW ( GPT2:config GPT2:tensor-id -- GPT2:config n ) GPT2:SLOT" NO
\ the inverse accepts and returns only nominal boundary types; its cast stays private.
s" INVERSE-OK ( GPT2:config CAD-NUM:index -- GPT2:config GPT2:tensor-id ) GPT2:TENSOR-ID-FOR-SLOT" YES
s" INVERSE-RAW-IN ( GPT2:config n -- GPT2:config GPT2:tensor-id ) GPT2:TENSOR-ID-FOR-SLOT" NO
s" INVERSE-RAW-OUT ( GPT2:config CAD-NUM:index -- GPT2:config n ) GPT2:TENSOR-ID-FOR-SLOT" NO
s" PRIVATE-SLOT-CAST ( CAD-NUM:index -- n ) GPT2:SLOT>N" UNK
s" GPT2:SLOT>N" XREF-FIND XREF-FOUND? TFALSE
\ a global tensor-id takes exactly a global-role; a layer cannot ride along...
s" GLOBAL-OK ( GPT2:global-role -- GPT2:tensor-id ) GPT2-TENSOR--ID:GLOBAL" YES
s" GLOBAL-WITH-LAYER ( GPT2:layer-id GPT2:global-role -- GPT2:tensor-id ) GPT2-TENSOR--ID:GLOBAL" NO
\ ...a layer tensor-id requires its layer-id, in declared field order...
s" LAYER-OK ( GPT2:layer-id GPT2:layer-role -- GPT2:tensor-id ) GPT2-TENSOR--ID:LAYER" YES
s" LAYER-NO-ID ( GPT2:layer-role -- GPT2:tensor-id ) GPT2-TENSOR--ID:LAYER" NO
s" LAYER-WRONG-ORDER ( GPT2:layer-role GPT2:layer-id -- GPT2:tensor-id ) GPT2-TENSOR--ID:LAYER" NO
\ ...and the role families do not cross.
s" LAYER-WRONG-ROLE ( GPT2:layer-id GPT2:global-role -- GPT2:tensor-id ) GPT2-TENSOR--ID:LAYER" NO
s" GLOBAL-WRONG-ROLE ( GPT2:layer-role -- GPT2:tensor-id ) GPT2-TENSOR--ID:GLOBAL" NO
\ the layer-id MAKE certifies only with a genuine layer-proof...
s" LAYER-ID-OK ( n GPT2:layer-proof -- GPT2:layer-id ) GPT2-LAYER--ID:MAKE" YES
s" LAYER-ID-RAW-PROOF ( n n -- GPT2:layer-id ) GPT2-LAYER--ID:MAKE" NO
\ ...cfg-proof cannot substitute (distinct proof domains), nor can
\ layer-proof seal a config...
s" LAYER-ID-WRONG-PROOF ( n GPT2:cfg-proof -- GPT2:layer-id ) GPT2-LAYER--ID:MAKE" NO
s" CONFIG-WRONG-PROOF ( MAKI:datatype n n n n n bool n n r bool GPT2:layer-proof -- GPT2:config ) GPT2-CONFIG:MAKE" NO
\ ...the removed generated record cell's old arity rejects, and the private
\ mint is unresolvable outside.
s" LAYER-ID-OLD-MAKE ( n n GPT2:layer-proof -- GPT2:layer-id ) GPT2-LAYER--ID:MAKE" NO
s" PRIVATE-MINT ( -- GPT2:layer-proof ) GPT2:MINT-LAYER-PROOF" UNK
s" BARE-PRIVATE-MINT ( -- GPT2:layer-proof ) MINT-LAYER-PROOF" UNK
\ a tensor-id is nominal: neither an orientation value nor a raw n can stand in the
\ COPY-NAME? tensor-id slot, and the genuine signature certifies.
s" NAME-WRONG-TYPE ( GPT2:orientation ptr u8 n -- option<n> ) GPT2:COPY-NAME?" NO
s" NAME-RAW-ID ( n ptr u8 n -- option<n> ) GPT2:COPY-NAME?" NO
s" NAME-OK ( GPT2:tensor-id ptr u8 n -- option<n> ) GPT2:COPY-NAME?" YES

T-REPORT

;package
