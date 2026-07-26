\ gpt2-roles-test.f - GPT2BIND acceptance (rev-3 S6a + rev-4 identity design).
\
\ Legs, all through the public package surface:
\   1. HF key pins at the real 124M geometry via COPY-KEY? into caller
\      buffers: all four globals, all thirteen layer-0 keys, a two-digit
\      layer-11 key, and the 39-byte largest-representable-index render, each
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
\      CENSUS-COUNT accepts and that plus one rejecting; the largest nembd
\      through the 4*nembd multiply and that plus one rejecting; the largest
\      nctx whose mask element product fits and that plus one rejecting; plus
\      the measured full-product escapes (nctx 4e9 mask, nembd 2^40 qkv) and
\      the MAX-N extremes, all E-GB-EXTENT;
\   5. LAYER range rejects (E-GB-LAYER) and boundary accepts;
\   6. TID-SLOT bijectivity on the tiny geometry: every slot in
\      [0, CENSUS-COUNT) hit exactly once - the fixture that later drives
\      WSTORE SEAL; an off-by-one slot formula reds here;
\   7. THE IDENTITY FIXTURE (ratified correction 1): two configs with the
\      SAME nlayer differing in ONE behavioral field (tied); a layerid minted
\      against A used with B rejects E-GB-FOREIGN even though its slot is in
\      bounds for B - with equal nlayer a bounds check can never be the
\      rejector, so the reject is proven to fire before slot arithmetic;
\      dropping the identity assertion reds this leg;
\   8. the re-MAKE forgery caveat: a layerid rebuilt around LAYER with an
\      out-of-range or negative index rejects E-GB-LAYER at TID-SLOT and at
\      the negative-index key render;
\   9. checker negatives: a global tid cannot carry a layer and a block tid
\      cannot omit one (by constructor arity/types), reordered block fields
\      reject, role families do not cross, MDLCFG's proof in the layerid slot
\      rejects (and GPT2BIND's proof in the mcfg slot), raw cells forge
\      neither key nor proof, and the private mint is unresolvable outside
\      the package.

require lib/prelude.f
require lib/adt/option.f
require lib/test.f
require test/checker-assert.f
require maki/infer/gpt2-roles.f

package GPT2BIND-TEST

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
4 constant GCOUNT                  \ the four checkpoint-global roles
13 constant BCOUNT                 \ the thirteen per-layer roles
4 constant FC-MUL                  \ the MLP widening factor (4*nembd)

: NL-MAX ( -- n )  MAXN GCOUNT - BCOUNT / ;     \ largest census-accepted nlayer
: NE-XNMAX ( -- n )  MAXN FC-MUL / ;            \ largest nembd through 4*nembd

variable BLO
variable BHI

: SQ-OK? ( n -- bool )                          \ v*v <= MAXN, judged without overflow
   dup MAXN swap / > 0= ;

: ISQRT-MAXN ( -- n )                           \ largest v with v*v <= MAXN, derived
   1 BLO !  MAXN BHI !
   begin BLO @ BHI @ < while
      BLO @  BHI @ BLO @ - 1 + 2 /  +
      dup SQ-OK? if BLO ! else 1 - BHI ! then
   repeat
   BLO @ ;

\ ---- fixture configs (all through the sole MDLCFG constructor) ----------------
: DT0 ( -- MAKI:dtype )  MAKI-DTYPE:DF32 ;
: EPS0 ( -- r )  0.00001 ;

\ the real GPT-2 124M geometry (the model-config-test B-G commons).
: B-R ( -- MDLCFG:mcfg )
   EPS0 true MDLCFG-ARCH:GPT2
   1 DT0 1024 50257 12 768 12 true 50256 50256 MDLCFG:BUILD ;

\ tiny geometry: nlayer 2, nembd 4, nhead 2, nctx 8, nvocab 5 - every 2-d
\ shape distinct and non-square, so a transposed or swapped dim cannot pass.
: B-A ( -- MDLCFG:mcfg )
   EPS0 true MDLCFG-ARCH:GPT2
   1 DT0 8 5 2 4 2 true 4 4 MDLCFG:BUILD ;

\ SAME nlayer as B-A, ONE behavioral field flipped (tied): the identity twin.
: B-B ( -- MDLCFG:mcfg )
   EPS0 true MDLCFG-ARCH:GPT2
   1 DT0 8 5 2 4 2 false 4 4 MDLCFG:BUILD ;

\ extreme-but-valid gpt2 geometry: BUILD accepts nembd = MAX-N (nvocab 1,
\ nctx 1, nhead 1), so the 3*nembd qkv product must be the overflow rejector.
: B-WIDE ( -- MDLCFG:mcfg )
   EPS0 true MDLCFG-ARCH:GPT2
   1 DT0 1 1 1 MAXN 1 true 0 0 MDLCFG:BUILD ;

\ the same shape at the exact 4*nembd boundary and one past it.
: B-WIDE-MAX ( -- MDLCFG:mcfg )
   EPS0 true MDLCFG-ARCH:GPT2
   1 DT0 1 1 1 NE-XNMAX 1 true 0 0 MDLCFG:BUILD ;

: B-WIDE-OVER ( -- MDLCFG:mcfg )
   EPS0 true MDLCFG-ARCH:GPT2
   1 DT0 1 1 1 NE-XNMAX 1 + 1 true 0 0 MDLCFG:BUILD ;

\ the llama arm never validated the gpt2 census bound, so CENSUS-COUNT's own
\ pre-check must reject; the MAX-N extreme, then the exact boundary pair.
: B-DEEP ( -- MDLCFG:mcfg )
   2 8 10000.0 0.000001 MDLCFG-ARCH:LLAMA
   1 DT0 8 32 MAXN 4 2 false 1 2 MDLCFG:BUILD ;

: B-DEEP-MAX ( -- MDLCFG:mcfg )
   2 8 10000.0 0.000001 MDLCFG-ARCH:LLAMA
   1 DT0 8 32 NL-MAX 4 2 false 1 2 MDLCFG:BUILD ;

: B-DEEP-OVER ( -- MDLCFG:mcfg )
   2 8 10000.0 0.000001 MDLCFG-ARCH:LLAMA
   1 DT0 8 32 NL-MAX 1 + 4 2 false 1 2 MDLCFG:BUILD ;

\ gpt2 arm with everything minimal but nctx, for the mask element product.
: B-CTX ( n -- MDLCFG:mcfg ) {: cx:n :}
   EPS0 true MDLCFG-ARCH:GPT2
   1 DT0 cx 1 1 1 1 true 0 0 MDLCFG:BUILD ;

\ the two measured full-product escapes: every per-dim product fits, the
\ pair product does not.
: B-MASK4E9 ( -- MDLCFG:mcfg )
   EPS0 true MDLCFG-ARCH:GPT2
   1 DT0 4000000000 5 1 768 12 true 4 4 MDLCFG:BUILD ;

: B-NE40 ( -- MDLCFG:mcfg )
   EPS0 true MDLCFG-ARCH:GPT2
   1 DT0 1 1 1 $10000000000 1 true 0 0 MDLCFG:BUILD ;

\ ---- typed probes over the public surface -------------------------------------
64 constant KBUF-CAP
create KBUF-1 KBUF-CAP allot
create KBUF-2 KBUF-CAP allot

: CK-G ( GPT2BIND:grole -- ptr u8 n )           \ global key copied into KBUF-1
   GPT2BIND-TID:GLOBAL KBUF-1 KBUF-CAP GPT2BIND:COPY-KEY? LEN-OF
   KBUF-1 swap ;

: CK-B ( MDLCFG:mcfg n GPT2BIND:brole -- MDLCFG:mcfg ptr u8 n ) {: br:GPT2BIND:brole :}
   GPT2BIND:LAYER br GPT2BIND-TID:BLOCK
   KBUF-1 KBUF-CAP GPT2BIND:COPY-KEY? LEN-OF
   KBUF-1 swap ;

: CP1-B ( MDLCFG:mcfg n GPT2BIND:brole -- MDLCFG:mcfg n ) {: br:GPT2BIND:brole :}
   GPT2BIND:LAYER br GPT2BIND-TID:BLOCK
   KBUF-1 KBUF-CAP GPT2BIND:COPY-KEY? LEN-OF ;

: CK2-G ( GPT2BIND:grole -- ptr u8 n )          \ global key copied into KBUF-2
   GPT2BIND-TID:GLOBAL KBUF-2 KBUF-CAP GPT2BIND:COPY-KEY? LEN-OF
   KBUF-2 swap ;

: SH-OF-G ( MDLCFG:mcfg GPT2BIND:grole -- MDLCFG:mcfg n n n n n )
   GPT2BIND-TID:GLOBAL GPT2BIND:TID-SHAPE ;

: SH-OF-B ( MDLCFG:mcfg n GPT2BIND:brole -- MDLCFG:mcfg n n n n n ) {: br:GPT2BIND:brole :}
   GPT2BIND:LAYER br GPT2BIND-TID:BLOCK GPT2BIND:TID-SHAPE ;

: SL-OF-G ( MDLCFG:mcfg GPT2BIND:grole -- MDLCFG:mcfg n )
   GPT2BIND-TID:GLOBAL GPT2BIND:TID-SLOT ;

: SL-OF-B ( MDLCFG:mcfg n GPT2BIND:brole -- MDLCFG:mcfg n ) {: br:GPT2BIND:brole :}
   GPT2BIND:LAYER br GPT2BIND-TID:BLOCK GPT2BIND:TID-SLOT ;

: OR-OF-G ( GPT2BIND:grole -- GPT2BIND:orient )
   GPT2BIND-TID:GLOBAL GPT2BIND:TID-ORIENT ;

: OR-OF-B ( MDLCFG:mcfg GPT2BIND:brole -- MDLCFG:mcfg GPT2BIND:orient ) {: br:GPT2BIND:brole :}
   0 GPT2BIND:LAYER br GPT2BIND-TID:BLOCK GPT2BIND:TID-ORIENT ;

: SH= ( n n n n n n n n n n -- ) {: r0:n a0:n a1:n a2:n a3:n r1:n b0:n b1:n b2:n b3:n :}
   r0 r1 T=
   a0 b0 T=  a1 b1 T=  a2 b2 T=  a3 b3 T= ;

: CONV1D? ( GPT2BIND:orient -- )
   GPT2BIND-ORIENT:CONV1D GPT2BIND-ORIENT:EQ TTRUE ;

: PLAIN? ( GPT2BIND:orient -- )
   GPT2BIND-ORIENT:PLAIN GPT2BIND-ORIENT:EQ TTRUE ;

\ ---- 1. HF key pins via COPY-KEY? -----------------------------------------------
: T-KEYS-G ( -- )
   GPT2BIND-GROLE:WTE CK-G s" wte.weight" T$=
   GPT2BIND-GROLE:WPE CK-G s" wpe.weight" T$=
   GPT2BIND-GROLE:LNF-G CK-G s" ln_f.weight" T$=
   GPT2BIND-GROLE:LNF-B CK-G s" ln_f.bias" T$= ;

: T-KEYS-B ( -- )
   B-R
   0 GPT2BIND-BROLE:LN1-G CK-B s" h.0.ln_1.weight" T$=
   0 GPT2BIND-BROLE:LN1-B CK-B s" h.0.ln_1.bias" T$=
   0 GPT2BIND-BROLE:MASK CK-B s" h.0.attn.bias" T$=
   0 GPT2BIND-BROLE:QKV-W CK-B s" h.0.attn.c_attn.weight" T$=
   0 GPT2BIND-BROLE:QKV-B CK-B s" h.0.attn.c_attn.bias" T$=
   0 GPT2BIND-BROLE:APROJ-W CK-B s" h.0.attn.c_proj.weight" T$=
   0 GPT2BIND-BROLE:APROJ-B CK-B s" h.0.attn.c_proj.bias" T$=
   0 GPT2BIND-BROLE:LN2-G CK-B s" h.0.ln_2.weight" T$=
   0 GPT2BIND-BROLE:LN2-B CK-B s" h.0.ln_2.bias" T$=
   0 GPT2BIND-BROLE:FC-W CK-B s" h.0.mlp.c_fc.weight" T$=
   0 GPT2BIND-BROLE:FC-B CK-B s" h.0.mlp.c_fc.bias" T$=
   0 GPT2BIND-BROLE:MPROJ-W CK-B s" h.0.mlp.c_proj.weight" T$=
   0 GPT2BIND-BROLE:MPROJ-B CK-B s" h.0.mlp.c_proj.bias" T$=
   11 GPT2BIND-BROLE:MPROJ-B CK-B s" h.11.mlp.c_proj.bias" T$=
   drop ;

\ the largest index LAYER can ever mint (NL-MAX - 1) renders at the 39-byte
\ extreme; the digits below are that derived index, pinned byte-exact.
: T-BIGIDX ( -- )
   B-DEEP-MAX NL-MAX 1 - GPT2BIND-BROLE:QKV-W CK-B
   dup 39 T=
   s" h.709490156681136599.attn.c_attn.weight" T$=
   drop ;

\ a too-small caller buffer answers NONE and writes nothing: KBUF-1 still
\ holds the wte key copied before the refused longer copies; capacity equal
\ to the key length is the exact accept boundary.
: T-SMALLBUF ( -- )
   GPT2BIND-GROLE:WTE CK-G s" wte.weight" T$=
   GPT2BIND-GROLE:LNF-G GPT2BIND-TID:GLOBAL KBUF-1 9 GPT2BIND:COPY-KEY? OPT-NONE
   GPT2BIND-GROLE:LNF-G GPT2BIND-TID:GLOBAL KBUF-1 0 GPT2BIND:COPY-KEY? OPT-NONE
   KBUF-1 10 s" wte.weight" T$=
   GPT2BIND-GROLE:WTE GPT2BIND-TID:GLOBAL KBUF-1 10 GPT2BIND:COPY-KEY? LEN-OF 10 T= ;

\ two successive copies into two DISTINCT caller buffers leave both intact -
\ the exact interleave the old span-returning API could not survive.
: T-INTERLEAVE ( -- )
   B-R 0 GPT2BIND-BROLE:QKV-W CP1-B
   GPT2BIND-GROLE:LNF-B CK2-G s" ln_f.bias" T$=
   KBUF-1 swap s" h.0.attn.c_attn.weight" T$=
   drop ;

\ ---- 2. shape pins: 124M then tiny ----------------------------------------------
: T-SHAPES-R ( -- )
   B-R
   GPT2BIND-GROLE:WTE SH-OF-G 2 50257 768 1 1 SH=
   GPT2BIND-GROLE:WPE SH-OF-G 2 1024 768 1 1 SH=
   GPT2BIND-GROLE:LNF-G SH-OF-G 1 768 1 1 1 SH=
   GPT2BIND-GROLE:LNF-B SH-OF-G 1 768 1 1 1 SH=
   0 GPT2BIND-BROLE:LN1-G SH-OF-B 1 768 1 1 1 SH=
   0 GPT2BIND-BROLE:LN1-B SH-OF-B 1 768 1 1 1 SH=
   0 GPT2BIND-BROLE:MASK SH-OF-B 4 1 1 1024 1024 SH=
   0 GPT2BIND-BROLE:QKV-W SH-OF-B 2 768 2304 1 1 SH=
   0 GPT2BIND-BROLE:QKV-B SH-OF-B 1 2304 1 1 1 SH=
   0 GPT2BIND-BROLE:APROJ-W SH-OF-B 2 768 768 1 1 SH=
   0 GPT2BIND-BROLE:APROJ-B SH-OF-B 1 768 1 1 1 SH=
   0 GPT2BIND-BROLE:LN2-G SH-OF-B 1 768 1 1 1 SH=
   0 GPT2BIND-BROLE:LN2-B SH-OF-B 1 768 1 1 1 SH=
   0 GPT2BIND-BROLE:FC-W SH-OF-B 2 768 3072 1 1 SH=
   0 GPT2BIND-BROLE:FC-B SH-OF-B 1 3072 1 1 1 SH=
   0 GPT2BIND-BROLE:MPROJ-W SH-OF-B 2 3072 768 1 1 SH=
   0 GPT2BIND-BROLE:MPROJ-B SH-OF-B 1 768 1 1 1 SH=
   GPT2BIND:CENSUS-COUNT 160 T=
   drop ;

: T-SHAPES-TINY ( -- )
   B-A
   GPT2BIND-GROLE:WTE SH-OF-G 2 5 4 1 1 SH=
   GPT2BIND-GROLE:WPE SH-OF-G 2 8 4 1 1 SH=
   1 GPT2BIND-BROLE:QKV-W SH-OF-B 2 4 12 1 1 SH=
   1 GPT2BIND-BROLE:QKV-B SH-OF-B 1 12 1 1 1 SH=
   1 GPT2BIND-BROLE:FC-W SH-OF-B 2 4 16 1 1 SH=
   0 GPT2BIND-BROLE:MPROJ-W SH-OF-B 2 16 4 1 1 SH=
   0 GPT2BIND-BROLE:MASK SH-OF-B 4 1 1 8 8 SH=
   GPT2BIND:CENSUS-COUNT 30 T=
   drop ;

\ ---- 3. declared orientation + the typed adapter identity -------------------------
: T-ORIENT ( -- )
   GPT2BIND-GROLE:WTE OR-OF-G PLAIN?
   GPT2BIND-GROLE:WPE OR-OF-G PLAIN?
   GPT2BIND-GROLE:LNF-G OR-OF-G PLAIN?
   GPT2BIND-GROLE:LNF-B OR-OF-G PLAIN?
   B-A
   GPT2BIND-BROLE:QKV-W OR-OF-B CONV1D?
   GPT2BIND-BROLE:APROJ-W OR-OF-B CONV1D?
   GPT2BIND-BROLE:FC-W OR-OF-B CONV1D?
   GPT2BIND-BROLE:MPROJ-W OR-OF-B CONV1D?
   GPT2BIND-BROLE:LN1-G OR-OF-B PLAIN?
   GPT2BIND-BROLE:LN1-B OR-OF-B PLAIN?
   GPT2BIND-BROLE:MASK OR-OF-B PLAIN?
   GPT2BIND-BROLE:QKV-B OR-OF-B PLAIN?
   GPT2BIND-BROLE:APROJ-B OR-OF-B PLAIN?
   GPT2BIND-BROLE:LN2-G OR-OF-B PLAIN?
   GPT2BIND-BROLE:LN2-B OR-OF-B PLAIN?
   GPT2BIND-BROLE:FC-B OR-OF-B PLAIN?
   GPT2BIND-BROLE:MPROJ-B OR-OF-B PLAIN?
   drop ;

: T-FORMAT ( -- )
   GPT2BIND:FORMAT-ID MODEL-ADAPTER:HF-GPT2 MODEL-ADAPTER:EQ TTRUE ;

\ ---- 4. checked arithmetic at its exact boundaries ---------------------------------
: RJ-SHOVF ( -- )
   B-WIDE 0 GPT2BIND-BROLE:QKV-W SH-OF-B
   drop drop drop drop drop drop ;

: RJ-CCOVF ( -- )
   B-DEEP GPT2BIND:CENSUS-COUNT drop drop ;

: RJ-CC-EDGE ( -- )
   B-DEEP-OVER GPT2BIND:CENSUS-COUNT drop drop ;

: RJ-XN-EDGE ( -- )
   B-WIDE-OVER 0 GPT2BIND-BROLE:FC-B SH-OF-B
   drop drop drop drop drop drop ;

: RJ-SQ-EDGE ( -- )
   ISQRT-MAXN 1 + B-CTX 0 GPT2BIND-BROLE:MASK SH-OF-B
   drop drop drop drop drop drop ;

: RJ-MASK4E9 ( -- )
   B-MASK4E9 0 GPT2BIND-BROLE:MASK SH-OF-B
   drop drop drop drop drop drop ;

: RJ-QKV40 ( -- )
   B-NE40 0 GPT2BIND-BROLE:QKV-W SH-OF-B
   drop drop drop drop drop drop ;

: T-EXTENT ( -- )
   [: RJ-SHOVF ;] GPT2BIND:E-GB-EXTENT TTHROWSQ
   [: RJ-CCOVF ;] GPT2BIND:E-GB-EXTENT TTHROWSQ
   [: RJ-MASK4E9 ;] GPT2BIND:E-GB-EXTENT TTHROWSQ
   [: RJ-QKV40 ;] GPT2BIND:E-GB-EXTENT TTHROWSQ ;

: T-EDGES ( -- )
   \ census: the largest nlayer is accepted with the exact count...
   B-DEEP-MAX GPT2BIND:CENSUS-COUNT  NL-MAX BCOUNT * GCOUNT +  T= drop
   \ ...and one more layer rejects.
   [: RJ-CC-EDGE ;] GPT2BIND:E-GB-EXTENT TTHROWSQ
   \ 4*nembd: the largest nembd passes with the exact product...
   B-WIDE-MAX 0 GPT2BIND-BROLE:FC-B SH-OF-B  1 NE-XNMAX FC-MUL * 1 1 1 SH= drop
   \ ...and one more rejects inside the checked multiply.
   [: RJ-XN-EDGE ;] GPT2BIND:E-GB-EXTENT TTHROWSQ
   \ mask element product: the largest nctx whose square fits passes...
   ISQRT-MAXN B-CTX 0 GPT2BIND-BROLE:MASK SH-OF-B
   4 1 1 ISQRT-MAXN ISQRT-MAXN SH= drop
   \ ...and one more rejects on the full product, not any single dim.
   [: RJ-SQ-EDGE ;] GPT2BIND:E-GB-EXTENT TTHROWSQ ;

\ ---- 5. LAYER range ----------------------------------------------------------------
: RJ-L-NEG ( -- )
   B-A -1 GPT2BIND:LAYER drop drop ;

: RJ-L-BIG ( -- )
   B-A 2 GPT2BIND:LAYER drop drop ;

: T-LAYER-RANGE ( -- )
   [: RJ-L-NEG ;] GPT2BIND:E-GB-LAYER TTHROWSQ
   [: RJ-L-BIG ;] GPT2BIND:E-GB-LAYER TTHROWSQ
   B-A
   0 GPT2BIND-BROLE:LN1-G SL-OF-B 4 T=
   1 GPT2BIND-BROLE:MPROJ-B SL-OF-B 29 T=
   drop ;

\ ---- 6. slot layout + bijectivity on the tiny geometry ------------------------------
30 constant TINY-CENSUS
create HITS TINY-CENSUS cells allot
variable HIT-I

: HITS-RESET ( -- )
   0 HIT-I !
   begin HIT-I @ TINY-CENSUS < while
      0 HITS HIT-I @ cells + !
      HIT-I @ 1 + HIT-I !
   repeat ;

: MARK ( n -- ) {: s:n :}
   s 0 >= s TINY-CENSUS < and TTRUE
   s 0 < if exit then
   s TINY-CENSUS >= if exit then
   HITS s cells + dup @ 1 + swap ! ;

: HITS-ONES ( -- )
   0 HIT-I !
   begin HIT-I @ TINY-CENSUS < while
      HITS HIT-I @ cells + @ 1 T=
      HIT-I @ 1 + HIT-I !
   repeat ;

: MARK-G ( MDLCFG:mcfg GPT2BIND:grole -- MDLCFG:mcfg )
   SL-OF-G MARK ;

: MARK-B ( MDLCFG:mcfg n GPT2BIND:brole -- MDLCFG:mcfg )
   SL-OF-B MARK ;

: MARK-LAYER ( MDLCFG:mcfg n -- MDLCFG:mcfg ) {: l:n :}
   l GPT2BIND-BROLE:LN1-G MARK-B
   l GPT2BIND-BROLE:LN1-B MARK-B
   l GPT2BIND-BROLE:MASK MARK-B
   l GPT2BIND-BROLE:QKV-W MARK-B
   l GPT2BIND-BROLE:QKV-B MARK-B
   l GPT2BIND-BROLE:APROJ-W MARK-B
   l GPT2BIND-BROLE:APROJ-B MARK-B
   l GPT2BIND-BROLE:LN2-G MARK-B
   l GPT2BIND-BROLE:LN2-B MARK-B
   l GPT2BIND-BROLE:FC-W MARK-B
   l GPT2BIND-BROLE:FC-B MARK-B
   l GPT2BIND-BROLE:MPROJ-W MARK-B
   l GPT2BIND-BROLE:MPROJ-B MARK-B ;

: T-SLOTS ( -- )
   B-A
   GPT2BIND-GROLE:WTE SL-OF-G 0 T=
   GPT2BIND-GROLE:WPE SL-OF-G 1 T=
   GPT2BIND-GROLE:LNF-G SL-OF-G 2 T=
   GPT2BIND-GROLE:LNF-B SL-OF-G 3 T=
   0 GPT2BIND-BROLE:MPROJ-B SL-OF-B 16 T=
   1 GPT2BIND-BROLE:LN1-G SL-OF-B 17 T=
   1 GPT2BIND-BROLE:QKV-W SL-OF-B 20 T=
   drop
   B-R 11 GPT2BIND-BROLE:MPROJ-B SL-OF-B 159 T=
   drop ;

: T-BIJECT ( -- )
   HITS-RESET
   B-A
   GPT2BIND-GROLE:WTE MARK-G
   GPT2BIND-GROLE:WPE MARK-G
   GPT2BIND-GROLE:LNF-G MARK-G
   GPT2BIND-GROLE:LNF-B MARK-G
   0 MARK-LAYER
   1 MARK-LAYER
   GPT2BIND:CENSUS-COUNT TINY-CENSUS T=
   drop
   HITS-ONES ;

\ ---- 7. THE IDENTITY FIXTURE ---------------------------------------------------------
\ A layerid minted against config A, resolved against same-nlayer config B.
: A-LID1 ( -- GPT2BIND:layerid )
   B-A 1 GPT2BIND:LAYER >r drop r> ;

: A-LID0 ( -- GPT2BIND:layerid )
   B-A 0 GPT2BIND:LAYER >r drop r> ;

: RJ-FOREIGN-MID ( -- )
   B-B A-LID1 GPT2BIND-BROLE:QKV-W GPT2BIND-TID:BLOCK GPT2BIND:TID-SLOT
   drop drop ;

: RJ-FOREIGN-LOW ( -- )
   B-B A-LID0 GPT2BIND-BROLE:LN1-G GPT2BIND-TID:BLOCK GPT2BIND:TID-SLOT
   drop drop ;

: T-FOREIGN ( -- )
   \ both foreign slots (20 and 4) are strictly inside B's census (30), so a
   \ bounds check can never be the rejector: E-GB-FOREIGN fires before slot math.
   B-B GPT2BIND:CENSUS-COUNT 30 T= drop
   [: RJ-FOREIGN-MID ;] GPT2BIND:E-GB-FOREIGN TTHROWSQ
   [: RJ-FOREIGN-LOW ;] GPT2BIND:E-GB-FOREIGN TTHROWSQ
   \ positive control: the same layerids resolve on their OWN config.
   B-A A-LID1 GPT2BIND-BROLE:QKV-W GPT2BIND-TID:BLOCK GPT2BIND:TID-SLOT 20 T=
   A-LID0 GPT2BIND-BROLE:LN1-G GPT2BIND-TID:BLOCK GPT2BIND:TID-SLOT 4 T=
   drop ;

\ ---- 8. the re-MAKE forgery caveat ----------------------------------------------------
\ UNMAKE hands back the genuine proof, so a holder can rebuild a layerid with
\ an arbitrary index around LAYER (the sealed-destructure gap). The rebuilt
\ identity passes CFGKEY= but its index must reject E-GB-LAYER.
variable FORGE-I

: FORGE-LID ( n -- GPT2BIND:layerid )
   FORGE-I !
   A-LID0 GPT2BIND-LAYERID:UNMAKE {: i:n tok:gb-proof :}
   FORGE-I @ tok GPT2BIND-LAYERID:MAKE ;

: RJ-FORGED-BIG ( -- )
   B-A 999 FORGE-LID GPT2BIND-BROLE:LN1-G GPT2BIND-TID:BLOCK GPT2BIND:TID-SLOT
   drop drop ;

: RJ-FORGED-NEG ( -- )
   -1 FORGE-LID GPT2BIND-BROLE:LN1-G GPT2BIND-TID:BLOCK
   KBUF-1 KBUF-CAP GPT2BIND:COPY-KEY? LEN-OF drop ;

: T-FORGED ( -- )
   [: RJ-FORGED-BIG ;] GPT2BIND:E-GB-LAYER TTHROWSQ
   [: RJ-FORGED-NEG ;] GPT2BIND:E-GB-LAYER TTHROWSQ ;

T-RESET

T-KEYS-G
T-KEYS-B
T-BIGIDX
T-SMALLBUF
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

\ ---- 9. checker negatives -------------------------------------------------------------
\ a global tid takes exactly a grole; a layer cannot ride along...
s" GBP-G ( GPT2BIND:grole -- GPT2BIND:tid ) GPT2BIND-TID:GLOBAL" YES
s" GBN-GWL ( GPT2BIND:layerid GPT2BIND:grole -- GPT2BIND:tid ) GPT2BIND-TID:GLOBAL" NO
\ ...a block tid requires its layerid, in declared field order...
s" GBP-B ( GPT2BIND:layerid GPT2BIND:brole -- GPT2BIND:tid ) GPT2BIND-TID:BLOCK" YES
s" GBN-BNL ( GPT2BIND:brole -- GPT2BIND:tid ) GPT2BIND-TID:BLOCK" NO
s" GBN-BREO ( GPT2BIND:brole GPT2BIND:layerid -- GPT2BIND:tid ) GPT2BIND-TID:BLOCK" NO
\ ...and the role families do not cross.
s" GBN-BROLEX ( GPT2BIND:layerid GPT2BIND:grole -- GPT2BIND:tid ) GPT2BIND-TID:BLOCK" NO
s" GBN-GROLEX ( GPT2BIND:brole -- GPT2BIND:tid ) GPT2BIND-TID:GLOBAL" NO
\ the layerid MAKE certifies only with a genuine GPT2BIND proof...
s" GBP-MAKE ( MDLCFG:cfgkey n GPT2BIND:gb-proof -- GPT2BIND:layerid ) GPT2BIND-LAYERID:MAKE" YES
s" GBN-RAWP ( MDLCFG:cfgkey n n -- GPT2BIND:layerid ) GPT2BIND-LAYERID:MAKE" NO
\ ...MDLCFG's proof cannot substitute (cross-package proof domains), nor can
\ GPT2BIND's proof seal an mcfg...
s" GBN-XPROOF ( MDLCFG:cfgkey n MDLCFG:cfg-proof -- GPT2BIND:layerid ) GPT2BIND-LAYERID:MAKE" NO
s" GBN-XPROOF2 ( n MAKI:dtype n n n n n bool n n MDLCFG:arch MDLCFG:cfgkey GPT2BIND:gb-proof -- MDLCFG:mcfg ) MDLCFG-MCFG:MAKE" NO
\ ...raw cells are not a cfgkey, and the private mint is unresolvable outside.
s" GBN-RAWKEY ( n n n n n GPT2BIND:gb-proof -- GPT2BIND:layerid ) GPT2BIND-LAYERID:MAKE" NO
s" GBN-MINT ( -- GPT2BIND:gb-proof ) GPT2BIND:MINT-GB-PROOF" UNK
s" GBN-MINT2 ( -- GPT2BIND:gb-proof ) MINT-GB-PROOF" UNK
\ a tid is nominal: neither an orient value nor a raw n can stand in the
\ COPY-KEY? tid slot, and the genuine signature certifies.
s" GBN-OTID ( GPT2BIND:orient ptr u8 n -- option<n> ) GPT2BIND:COPY-KEY?" NO
s" GBN-RAWTID ( n ptr u8 n -- option<n> ) GPT2BIND:COPY-KEY?" NO
s" GBP-CPY ( GPT2BIND:tid ptr u8 n -- option<n> ) GPT2BIND:COPY-KEY?" YES

T-REPORT

;package
