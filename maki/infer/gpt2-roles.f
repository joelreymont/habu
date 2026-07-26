\ gpt2-roles.f - the GPT-2 tensor vocabulary and typed identity (package
\ GPT2BIND; inference design rev 3 S6a, blackboard 20260724-185632.302, as
\ amended by the rev-4 identity design, 20260724-191041.846).
\
\ CONCERN: one typed name for every tensor in a HuggingFace GPT-2 checkpoint,
\ plus the authenticated layer identity those names ride on. ENUM `grole` is
\ the four checkpoint-global tensors and ENUM `brole` the thirteen per-layer
\ tensors; `tid` = global(grole) | block(layerid, brole), so BY CONSTRUCTION a
\ global tensor cannot carry a layer and a block tensor cannot omit one.
\
\ ROLE TABLE DERIVATION (pinned against the real checkpoint). The real
\ openai-community/gpt2 model.safetensors (SHA-256 pinned in
\ maki/infer/gpt2-reference-data.f) publishes a census of 160 tensors
\ (maki/infer/safetensors-test.f real-artifact leg): 4 globals (wte.weight,
\ wpe.weight, ln_f.weight, ln_f.bias) + 12 layers x 13. The thirteen, in HF
\ GPT2Block state-dict order (ln_1, then the attention module's causal-mask
\ buffer and its two Conv1D projections, ln_2, then the MLP's two Conv1D
\ projections):
\   ln1-g   h.<n>.ln_1.weight          [nembd]
\   ln1-b   h.<n>.ln_1.bias            [nembd]
\   mask    h.<n>.attn.bias            [1,1,nctx,nctx]  (causal-mask buffer)
\   qkv-w   h.<n>.attn.c_attn.weight   [nembd,3*nembd]  Conv1D
\   qkv-b   h.<n>.attn.c_attn.bias     [3*nembd]
\   aproj-w h.<n>.attn.c_proj.weight   [nembd,nembd]    Conv1D
\   aproj-b h.<n>.attn.c_proj.bias     [nembd]
\   ln2-g   h.<n>.ln_2.weight          [nembd]
\   ln2-b   h.<n>.ln_2.bias            [nembd]
\   fc-w    h.<n>.mlp.c_fc.weight      [nembd,4*nembd]  Conv1D
\   fc-b    h.<n>.mlp.c_fc.bias        [4*nembd]
\   mproj-w h.<n>.mlp.c_proj.weight    [4*nembd,nembd]  Conv1D
\   mproj-b h.<n>.mlp.c_proj.bias      [nembd]
\ Declaration order IS the slot ordinal consumed by TID-SLOT. The 13-role
\ census INCLUDING attn.bias is pinned to this specific checkpoint export: HF
\ treats attn.bias as a non-persistent buffer, so other exports of the same
\ weights may omit it, and binding such an artifact is a census mismatch by
\ design, not a vocabulary variant.
\
\ ORIENTATION CONVENTION (DECLARED per exact HF key, never inferred from
\ shape). HF GPT-2 stores its four projection weights as `Conv1D`, whose
\ weight is [in_features, out_features] - the transpose of nn.Linear's
\ [out, in] (maki/infer/safetensors.f TENSOR ORIENTATION note; the real
\ h.0.attn.c_attn.weight is [768, 2304]). A conv1d tensor is consumed as
\ y = x @ W with NO transpose. TID-ORIENT declares conv1d for exactly the
\ four *.weight keys of Conv1D modules - qkv-w, aproj-w, fc-w, mproj-w -
\ including the SQUARE aproj-w [nembd,nembd], where shape could never decide;
\ every other role (embeddings, norms, biases, the mask) is plain: consumed
\ exactly as stored. This is an adapter-owned convention of MODEL-ADAPTER:
\ HF-GPT2 (FORMAT-ID), not a property derivable from the data.
\
\ AUTHENTICATED LAYER IDENTITY (rev-4 correction 1). STRUCTURE `layerid`
\ embeds the minting config's content key: (MDLCFG:cfgkey, index, private
\ GPT2BIND proof). The sole constructor LAYER ( mcfg n -- mcfg layerid )
\ validates n against NLAYER@ (E-GB-LAYER) AND captures that mcfg's cfgkey.
\ TID-SLOT's block arm asserts CFGKEY= between the layerid's embedded key and
\ the consuming mcfg's key BEFORE any slot arithmetic - a layerid minted
\ against a different behavioral config throws E-GB-FOREIGN even when its
\ index would be in bounds. By spec the identity assertion lives ONLY in
\ TID-SLOT: a foreign layerid passes TID-SHAPE and TID-ORIENT (both are
\ layer-value-independent) and rejects at the table row - fail-closed, but
\ late by design; do not widen. The proof is an arity-0 NEWTYPE exactly
\ like MDLCFG's cfg-proof (see model-config.f header for the engine
\ constraint and the UNMAKE/re-MAKE scope caveat, closed by the
\ sealed-destructure dot). Because that caveat still lets a holder re-MAKE a
\ layerid around LAYER with an arbitrary index, TID-SLOT revalidates the
\ embedded index against [0, nlayer) and rejects E-GB-LAYER - the same code
\ LAYER uses, because it is the same violated contract - BEFORE the slot
\ multiply, so the returned slot is inside [0, CENSUS-COUNT) unconditionally
\ and a forged index can neither read a wrong row nor wrap the arithmetic.
\
\ SHAPE ENCODING. TID-SHAPE ( mcfg tid -- mcfg n n n n n ) returns
\ rank d0 d1 d2 d3: rank in {1,2,4}, dims exactly as the checkpoint header
\ lists them (row-major), unused trailing slots hold 1 so d0*d1*d2*d3 is
\ always the element count. Every composed dim AND the full element product
\ d0*d1*d2*d3 are proven to fit a cell through the checked multiply
\ (E-GB-EXTENT), as is CENSUS-COUNT = 4 + 13*nlayer; no shape TID-SHAPE
\ returns can overflow a downstream extent computation's numerator.
\
\ KEY ACCESS. COPY-KEY? copies the exact HF tensor name into a CALLER buffer
\ and answers option<n> - NONE when the capacity is too small, SOME holding
\ the copied length - the SAFET:COPY-NAME? contract. No public GPT2BIND word
\ returns a pointer into package or global statics, and no public word
\ touches the shared lib/string builder. The private render scratch KEY-BUF
\ is sized by the static bound in the KEY-CAP comment and never escapes.
\
\ ARM SCOPE. Every derivation here reads only the COMMON mcfg fields (nctx,
\ nvocab, nlayer, nembd) plus cfgkey; which adapter runs against which arch
\ arm is the binder's dispatch concern (S6b), not this vocabulary's.
\
\ maki -> habu only. Owns -5650..-5659.

require lib/prelude.f
require lib/adt/option.f
require maki/infer/model-types.f
require maki/infer/model-config.f

package GPT2BIND

public

\ ---- named rejection codes ----------------------------------------------------
-5650 constant E-GB-LAYER    \ a layer index outside [0, nlayer), fresh or embedded
-5651 constant E-GB-FOREIGN  \ layerid minted against a different behavioral config
-5652 constant E-GB-EXTENT   \ a composed shape/census product overflows a cell

\ ---- the four checkpoint-global tensor roles ------------------------------------
ENUM grole
   wte
   wpe
   lnf-g
   lnf-b
;ENUM

\ ---- the thirteen per-layer roles (HF GPT2Block state-dict order; see header) ---
ENUM brole
   ln1-g
   ln1-b
   mask
   qkv-w
   qkv-b
   aproj-w
   aproj-b
   ln2-g
   ln2-b
   fc-w
   fc-b
   mproj-w
   mproj-b
;ENUM

\ ---- storage orientation, declared per key (see header convention) --------------
ENUM orient DERIVE eq
   plain
   conv1d
;ENUM

\ ---- the private-mint proof (arity-0 nominal; MDLCFG cfg-proof shape) -----------
NEWTYPE gb-proof 0

\ ---- authenticated layer identity: cfgkey copy + index + private proof ----------
STRUCTURE layerid 0
   FIELD key MDLCFG:cfgkey
   FIELD idx n
   FIELD tok gb-proof
;STRUCTURE

\ ---- the tensor identity ---------------------------------------------------------
ENUM tid 0
   VARIANT global
      FIELD role grole
   ;VARIANT
   VARIANT block
      FIELD lid layerid
      FIELD role brole
   ;VARIANT
;ENUM

private

TRUSTED: MINT-GB-PROOF ( -- gb-proof )  0 ;

4 constant NGLOBAL                 \ |grole|: the checkpoint-global tensors
13 constant NBLOCK                 \ |brole|: tensors per transformer block
$7FFFFFFFFFFFFFFF constant MAX-N
\ static render bound: "h." (2) + at most 19 index digits + "." (1) + the
\ longest role path "attn.c_attn.weight" (18) = 40; every append below is one
\ of those pieces, so 64 can never be reached.
64 constant KEY-CAP

create KEY-BUF KEY-CAP allot       \ private render scratch (never escapes; see header)
variable KEY-U

\ overflow-checked product of BUILD-validated positive extents.
: XN ( n n -- n ) {: a:n b:n :}
   a MAX-N b / > if E-GB-EXTENT throw then
   a b * ;

\ shared index range gate: LAYER validates a fresh index, SLOT-B revalidates
\ the embedded one against the identity-matched config (see header on the
\ UNMAKE/re-MAKE forgery caveat).
: ASSERT-IDX ( MDLCFG:mcfg n -- MDLCFG:mcfg n ) {: i:n :}
   i 0 < if E-GB-LAYER throw then
   MDLCFG:NLAYER@ i > 0= if E-GB-LAYER throw then
   i ;

public

\ ---- the sole layerid constructor -------------------------------------------------
: LAYER ( MDLCFG:mcfg n -- MDLCFG:mcfg layerid )
   ASSERT-IDX {: i:n :}
   MDLCFG:CFGKEY@ i MINT-GB-PROOF GPT2BIND-LAYERID:MAKE ;

\ ---- census: 4 + 13*nlayer, overflow-checked --------------------------------------
\ The pre-check bounds nlayer so the multiply AND the add both fit a cell (the
\ MDLCFG V-CENSUS shape; the gpt2 BUILD arm enforces the same bound, the llama
\ arm does not, so this word carries its own).
: CENSUS-COUNT ( MDLCFG:mcfg -- MDLCFG:mcfg n )
   MDLCFG:NLAYER@
   dup MAX-N NGLOBAL - NBLOCK / > if E-GB-EXTENT throw then
   NBLOCK * NGLOBAL + ;

\ ---- the adapter identity provenance consumes (rev-4 correction 4) ----------------
: FORMAT-ID ( -- MODEL:adapter )
   MODEL-ADAPTER:HF-GPT2 ;

private

\ ---- HF key rendering (into KEY-BUF; the public copy-out is COPY-KEY?) -------------
: GR-KEY ( grole -- ptr u8 n )
   MATCH grole
      wte   OF s" wte.weight" ENDOF
      wpe   OF s" wpe.weight" ENDOF
      lnf-g OF s" ln_f.weight" ENDOF
      lnf-b OF s" ln_f.bias" ENDOF
   ;MATCH ;

: BR-PATH ( brole -- ptr u8 n )
   MATCH brole
      ln1-g   OF s" ln_1.weight" ENDOF
      ln1-b   OF s" ln_1.bias" ENDOF
      mask    OF s" attn.bias" ENDOF
      qkv-w   OF s" attn.c_attn.weight" ENDOF
      qkv-b   OF s" attn.c_attn.bias" ENDOF
      aproj-w OF s" attn.c_proj.weight" ENDOF
      aproj-b OF s" attn.c_proj.bias" ENDOF
      ln2-g   OF s" ln_2.weight" ENDOF
      ln2-b   OF s" ln_2.bias" ENDOF
      fc-w    OF s" mlp.c_fc.weight" ENDOF
      fc-b    OF s" mlp.c_fc.bias" ENDOF
      mproj-w OF s" mlp.c_proj.weight" ENDOF
      mproj-b OF s" mlp.c_proj.bias" ENDOF
   ;MATCH ;

: LID-IDX ( layerid -- n )
   GPT2BIND-LAYERID:UNMAKE {: i:n tok:gb-proof :}
   drop i ;

: K+ ( ptr u8 n -- )               \ append a piece to the key render
   dup >r
   KEY-BUF KEY-U @ + swap BYTE-COPY
   KEY-U @ r> + KEY-U ! ;

: K+DIGIT ( n -- )                 \ append one decimal digit 0..9
   $30 +
   KEY-BUF KEY-U @ + c!
   KEY-U @ 1 + KEY-U ! ;

: K+U ( n -- )                     \ append a nonnegative index in decimal
   dup 10 < if K+DIGIT exit then
   dup 10 / RECURSE
   10 mod K+DIGIT ;

\ a negative embedded index is only reachable through the re-MAKE forgery
\ caveat (see header); it rejects with the range code instead of rendering
\ garbage digits.
: BK-RENDER ( layerid brole -- ptr u8 n ) {: br:brole :}
   LID-IDX
   dup 0 < if E-GB-LAYER throw then
   0 KEY-U !
   s" h." K+
   K+U
   s" ." K+  br BR-PATH K+
   KEY-BUF KEY-U @ ;

: KEY$ ( tid -- ptr u8 n )         \ private span; public access copies out below
   MATCH tid
      global OF GR-KEY ENDOF
      block  OF BK-RENDER ENDOF
   ;MATCH ;

\ ---- shapes from the common geometry (each ( nctx nvocab nembd -- rank d0..d3 )) ----
: GEO ( MDLCFG:mcfg -- MDLCFG:mcfg n n n )
   MDLCFG:NEMBD@ >r MDLCFG:NVOCAB@ >r MDLCFG:NCTX@ r> r> ;

: SH-WTE ( n n n -- n n n n n ) {: cx:n vo:n ne:n :}
   2 vo ne 1 1 ;
: SH-WPE ( n n n -- n n n n n ) {: cx:n vo:n ne:n :}
   2 cx ne 1 1 ;
: SH-VEC ( n n n -- n n n n n ) {: cx:n vo:n ne:n :}
   1 ne 1 1 1 ;
: SH-MASK ( n n n -- n n n n n ) {: cx:n vo:n ne:n :}
   4 1 1 cx cx ;
: SH-QKVW ( n n n -- n n n n n ) {: cx:n vo:n ne:n :}
   2 ne  ne 3 XN  1 1 ;
: SH-QKVB ( n n n -- n n n n n ) {: cx:n vo:n ne:n :}
   1  ne 3 XN  1 1 1 ;
: SH-APW ( n n n -- n n n n n ) {: cx:n vo:n ne:n :}
   2 ne ne 1 1 ;
: SH-FCW ( n n n -- n n n n n ) {: cx:n vo:n ne:n :}
   2 ne  ne 4 XN  1 1 ;
: SH-FCB ( n n n -- n n n n n ) {: cx:n vo:n ne:n :}
   1  ne 4 XN  1 1 1 ;
: SH-MPW ( n n n -- n n n n n ) {: cx:n vo:n ne:n :}
   2  ne 4 XN  ne 1 1 ;

: SH-G ( n n n grole -- n n n n n )
   MATCH grole
      wte   OF SH-WTE ENDOF
      wpe   OF SH-WPE ENDOF
      lnf-g OF SH-VEC ENDOF
      lnf-b OF SH-VEC ENDOF
   ;MATCH ;

: SH-B ( n n n brole -- n n n n n )
   MATCH brole
      ln1-g   OF SH-VEC ENDOF
      ln1-b   OF SH-VEC ENDOF
      mask    OF SH-MASK ENDOF
      qkv-w   OF SH-QKVW ENDOF
      qkv-b   OF SH-QKVB ENDOF
      aproj-w OF SH-APW ENDOF
      aproj-b OF SH-VEC ENDOF
      ln2-g   OF SH-VEC ENDOF
      ln2-b   OF SH-VEC ENDOF
      fc-w    OF SH-FCW ENDOF
      fc-b    OF SH-FCB ENDOF
      mproj-w OF SH-MPW ENDOF
      mproj-b OF SH-VEC ENDOF
   ;MATCH ;

\ the full element count d0*d1*d2*d3 must fit a cell, not just each composed
\ dim: a per-dim-valid mask [1,1,cx,cx] or qkv [ne,3ne] can still overflow the
\ pair product (the measured 4e9-nctx and 2^40-nembd cases).
: SH-FIT ( n n n n n -- n n n n n ) {: r:n d0:n d1:n d2:n d3:n :}
   d0 d1 XN d2 XN d3 XN drop
   r d0 d1 d2 d3 ;

: TS-GLOBAL ( MDLCFG:mcfg grole -- MDLCFG:mcfg n n n n n )
   >r GEO r> SH-G SH-FIT ;

\ shape is layer-independent: the layerid is dropped unread.
: TS-BLOCK ( MDLCFG:mcfg layerid brole -- MDLCFG:mcfg n n n n n ) {: br:brole :}
   drop
   GEO br SH-B SH-FIT ;

\ ---- slot ordinals (declaration order; exhaustive MATCH pins the role count) -------
: GR-ORD ( grole -- n )
   MATCH grole
      wte   OF 0 ENDOF
      wpe   OF 1 ENDOF
      lnf-g OF 2 ENDOF
      lnf-b OF 3 ENDOF
   ;MATCH ;

: BR-ORD ( brole -- n )
   MATCH brole
      ln1-g   OF 0 ENDOF
      ln1-b   OF 1 ENDOF
      mask    OF 2 ENDOF
      qkv-w   OF 3 ENDOF
      qkv-b   OF 4 ENDOF
      aproj-w OF 5 ENDOF
      aproj-b OF 6 ENDOF
      ln2-g   OF 7 ENDOF
      ln2-b   OF 8 ENDOF
      fc-w    OF 9 ENDOF
      fc-b    OF 10 ENDOF
      mproj-w OF 11 ENDOF
      mproj-b OF 12 ENDOF
   ;MATCH ;

\ the identity assertion: the layerid's embedded key must be THIS mcfg's key.
: ASSERT-OWN ( MDLCFG:mcfg MDLCFG:cfgkey -- MDLCFG:mcfg )
   >r MDLCFG:CFGKEY@ r> MDLCFG:CFGKEY=
   0= if E-GB-FOREIGN throw then ;

: SLOT-G ( MDLCFG:mcfg grole -- MDLCFG:mcfg n )
   GR-ORD ;

\ Order: E-GB-FOREIGN identity FIRST; then the embedded index revalidates
\ against the now-proven-same config (E-GB-LAYER, header forgery note);
\ CENSUS-COUNT then proves 4 + 13*nlayer fits a cell. After those three gates
\ the multiply cannot wrap and the slot is in [NGLOBAL, census) always.
: SLOT-B ( MDLCFG:mcfg layerid brole -- MDLCFG:mcfg n ) {: br:brole :}
   GPT2BIND-LAYERID:UNMAKE drop
   >r ASSERT-OWN r>
   ASSERT-IDX
   >r CENSUS-COUNT drop r>
   NBLOCK * br BR-ORD + NGLOBAL + ;

: OR-B ( layerid brole -- orient ) {: br:brole :}
   drop br
   MATCH brole
      ln1-g   OF GPT2BIND-ORIENT:PLAIN ENDOF
      ln1-b   OF GPT2BIND-ORIENT:PLAIN ENDOF
      mask    OF GPT2BIND-ORIENT:PLAIN ENDOF
      qkv-w   OF GPT2BIND-ORIENT:CONV1D ENDOF
      qkv-b   OF GPT2BIND-ORIENT:PLAIN ENDOF
      aproj-w OF GPT2BIND-ORIENT:CONV1D ENDOF
      aproj-b OF GPT2BIND-ORIENT:PLAIN ENDOF
      ln2-g   OF GPT2BIND-ORIENT:PLAIN ENDOF
      ln2-b   OF GPT2BIND-ORIENT:PLAIN ENDOF
      fc-w    OF GPT2BIND-ORIENT:CONV1D ENDOF
      fc-b    OF GPT2BIND-ORIENT:PLAIN ENDOF
      mproj-w OF GPT2BIND-ORIENT:CONV1D ENDOF
      mproj-b OF GPT2BIND-ORIENT:PLAIN ENDOF
   ;MATCH ;

public

\ ---- the exact HF tensor name, copied into the caller's buffer ----------------------
\ NONE when the destination capacity is smaller than the key, with nothing
\ written (the SAFET:COPY-NAME? contract); SOME holds the copied byte length.
: COPY-KEY? ( tid ptr u8 n -- option<n> ) {: cap:n :}
   >r KEY$ r>
   over cap > if
      drop drop drop OPTION:NONE exit
   then
   over >r
   swap BYTE-COPY
   r> OPTION:SOME ;

\ ---- expected shape: rank d0 d1 d2 d3, 1-padded (see header) ------------------------
: TID-SHAPE ( MDLCFG:mcfg tid -- MDLCFG:mcfg n n n n n )
   MATCH tid
      global OF TS-GLOBAL ENDOF
      block  OF TS-BLOCK ENDOF
   ;MATCH ;

\ ---- declared storage orientation (adapter convention; see header) ------------------
: TID-ORIENT ( tid -- orient )
   MATCH tid
      global OF drop GPT2BIND-ORIENT:PLAIN ENDOF
      block  OF OR-B ENDOF
   ;MATCH ;

\ ---- dense slot: globals 0..3, then 4 + layer*13 + role ordinal ---------------------
\ The block arm asserts identity (E-GB-FOREIGN) BEFORE any slot arithmetic,
\ then revalidates the embedded index (E-GB-LAYER; see header).
: TID-SLOT ( MDLCFG:mcfg tid -- MDLCFG:mcfg n )
   MATCH tid
      global OF SLOT-G ENDOF
      block  OF SLOT-B ENDOF
   ;MATCH ;

;package
