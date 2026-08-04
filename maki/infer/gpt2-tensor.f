\ gpt2-tensor.f - the GPT-2 tensor vocabulary and typed layer index (package
\ GPT2; inference design rev 3 S6a, blackboard 20260724-185632.302).
\
\ CONCERN: one typed name for every tensor in a HuggingFace GPT-2 checkpoint,
\ plus the nominal layer index those names ride on. ENUM `global-role` is
\ the four checkpoint-global tensors and ENUM `layer-role` the thirteen per-layer
\ tensors; `tensor-id` = global(global-role) | layer(layer-id, layer-role), so
\ BY CONSTRUCTION a global tensor cannot carry a layer and a layer tensor
\ cannot omit one.
\
\ ROLE TABLE DERIVATION (pinned against the real checkpoint). The real
\ openai-community/gpt2 model.safetensors (SHA-256 pinned by
\ GPT2PIN:MODEL-SHA256$ in maki/infer/gpt2-pin.f) publishes a census of 160 tensors
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
\ Declaration order IS the slot ordinal consumed by SLOT and its checked
\ inverse TENSOR-ID-FOR-SLOT. The 13-role
\ census INCLUDING attn.bias is pinned to this specific checkpoint export: HF
\ treats attn.bias as a non-persistent buffer, so other exports of the same
\ weights may omit it, and loading such an artifact is a census mismatch by
\ design, not a vocabulary variant.
\
\ ORIENTATION CONVENTION (DECLARED per exact HF tensor name, never inferred from
\ shape). HF GPT-2 stores its four projection weights as `Conv1D`, whose
\ weight is [in_features, out_features] - the transpose of nn.Linear's
\ [out, in] (maki/infer/safetensors.f TENSOR ORIENTATION note; the real
\ h.0.attn.c_attn.weight is [768, 2304]). A conv1d tensor is consumed as
\ y = x @ W with NO transpose. ORIENTATION declares conv1d for exactly the
\ four *.weight tensor names of Conv1D modules - qkv-w, aproj-w, fc-w, mproj-w -
\ including the SQUARE aproj-w [nembd,nembd], where shape could never decide;
\ every other role (embeddings, norms, biases, the mask) is plain: consumed
\ exactly as stored. This is a GPT-2 checkpoint convention, not a property
\ derivable from the data.
\
\ NOMINAL LAYER INDEX. STRUCTURE `layer-id` holds an index and a private
\ layer-proof. The sole constructor LAYER-ID ( config n -- config layer-id )
\ validates n against NLAYER@ (E-LAYER) before minting. The proof is an
\ arity-0 NEWTYPE exactly like GPT2's cfg-proof (see gpt2-config.f header
\ for the engine constraint and the UNMAKE/re-MAKE scope caveat, closed by
\ the sealed-destructure dot). Because that caveat still lets a holder re-MAKE
\ a layer-id around LAYER-ID with an arbitrary index, SLOT revalidates the
\ embedded index against its consuming config on every lookup and rejects
\ E-LAYER before slot arithmetic. The returned slot is therefore inside
\ [0, COUNT) unconditionally, and a forged index can neither select a wrong
\ row nor wrap the arithmetic.
\
\ SHAPE ENCODING. SHAPE ( config tensor-id -- config n n n n n ) returns
\ rank d0 d1 d2 d3: rank in {1,2,4}, dims exactly as the checkpoint header
\ lists them (row-major), unused trailing slots hold 1 so d0*d1*d2*d3 is
\ always the element count. Every composed dim AND the full element product
\ d0*d1*d2*d3 are proven to fit a cell through the checked multiply
\ (E-SIZE), as is COUNT = 4 + 13*nlayer; no shape SHAPE
\ returns can overflow a downstream extent computation's numerator.
\
\ TENSOR NAME ACCESS. COPY-NAME? copies the exact HF tensor name into a CALLER buffer
\ and answers option<n> - NONE when the capacity is too small, SOME holding
\ the copied length - the SAFET:COPY-NAME? contract. No public word in this file
\ returns a pointer into package or global statics, and no public word
\ touches the shared lib/string builder. The private render scratch NAME-BUF
\ is sized by the static bound in the NAME-CAP comment and never escapes.
\
\ Every derivation here reads the canonical GPT-2 geometry directly.
\
\ maki -> habu only. Owns -5650..-5659.

require lib/prelude.f
require lib/adt/option.f
require lib/cad-num-types.f
require maki/infer/gpt2-config.f

package GPT2

public

\ ---- named rejection codes ----------------------------------------------------
-5650 constant E-LAYER   \ a layer index outside [0, nlayer), fresh or embedded
-5652 constant E-SIZE    \ a composed shape/census product overflows a cell
-5653 constant E-SLOT    \ a slot outside this config's tensor census

\ ---- the four checkpoint-global tensor roles ------------------------------------
ENUM global-role
   wte
   wpe
   lnf-g
   lnf-b
;ENUM

\ ---- the thirteen per-layer roles (HF GPT2Block state-dict order; see header) ---
ENUM layer-role
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

\ ---- storage orientation, declared per tensor name (see header convention) ------
ENUM orientation DERIVE eq
   plain
   conv1d
;ENUM

\ ---- the private-mint proof (arity-0 nominal; GPT2 cfg-proof shape) -------------
NEWTYPE layer-proof 0

\ ---- nominal layer index: checked index + private proof ------------------------
STRUCTURE layer-id 0
   FIELD idx n
   FIELD tok layer-proof
;STRUCTURE

\ ---- the tensor identity ---------------------------------------------------------
ENUM tensor-id 0
   VARIANT global
      FIELD role global-role
   ;VARIANT
   VARIANT layer
      FIELD id layer-id
      FIELD role layer-role
   ;VARIANT
;ENUM

private

TRUSTED: MINT-LAYER-PROOF ( -- layer-proof )  0 ;

4 constant GLOBAL-COUNT          \ |global-role|: checkpoint-global tensors
13 constant LAYER-ROLE-COUNT     \ |layer-role|: tensors per transformer block
\ static render bound: "h." (2) + at most 19 index digits + "." (1) + the
\ longest role path "attn.c_attn.weight" (18) = 40; every append below is one
\ of those pieces, so 64 can never be reached.
64 constant NAME-CAP

create NAME-BUF NAME-CAP allot     \ private render scratch (never escapes; see header)
variable NAME-LEN

\ overflow-checked product of BUILD-validated positive extents.
: CHECKED-MUL ( n n -- n ) {: a:n b:n :}
   a MAX-N b / > if E-SIZE throw then
   a b * ;

\ shared index range gate: LAYER-ID validates a fresh index; SLOT-LAYER
\ revalidates the embedded one against its consuming config.
: CHECK-LAYER-INDEX ( GPT2:config n -- GPT2:config n ) {: i:n :}
   i 0 < if E-LAYER throw then
   NLAYER@ i > 0= if E-LAYER throw then
   i ;

public

\ ---- the sole layer-id constructor -------------------------------------------------
: LAYER-ID ( GPT2:config n -- GPT2:config layer-id )
   CHECK-LAYER-INDEX {: i:n :}
   i MINT-LAYER-PROOF GPT2-LAYER--ID:MAKE ;

\ ---- census: 4 + 13*nlayer, overflow-checked --------------------------------------
\ The pre-check bounds nlayer so the multiply AND the add both fit a cell.
: COUNT ( GPT2:config -- GPT2:config n )
   NLAYER@
   dup MAX-N GLOBAL-COUNT - LAYER-ROLE-COUNT / > if E-SIZE throw then
   LAYER-ROLE-COUNT * GLOBAL-COUNT + ;

private

\ ---- HF name rendering (into NAME-BUF; the public copy-out is COPY-NAME?) ------------
: GLOBAL-NAME ( global-role -- ptr u8 n )
   MATCH global-role
      wte   OF s" wte.weight" ENDOF
      wpe   OF s" wpe.weight" ENDOF
      lnf-g OF s" ln_f.weight" ENDOF
      lnf-b OF s" ln_f.bias" ENDOF
   ;MATCH ;

: LAYER-PATH ( layer-role -- ptr u8 n )
   MATCH layer-role
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

: LAYER-INDEX ( layer-id -- n )
   GPT2-LAYER--ID:UNMAKE {: i:n tok:layer-proof :}
   i ;

: APPEND-NAME ( ptr u8 n -- )
   dup >r
   NAME-BUF NAME-LEN @ + swap BYTE-COPY
   NAME-LEN @ r> + NAME-LEN ! ;

: APPEND-DIGIT ( n -- )            \ append one decimal digit 0..9
   $30 +
   NAME-BUF NAME-LEN @ + c!
   NAME-LEN @ 1 + NAME-LEN ! ;

: APPEND-INDEX ( n -- )            \ append a nonnegative index in decimal
   dup 10 < if APPEND-DIGIT exit then
   dup 10 / RECURSE
   10 mod APPEND-DIGIT ;

\ a negative embedded index is only reachable through the re-MAKE forgery
\ caveat (see header); it rejects with the range code instead of rendering
\ garbage digits.
: LAYER-NAME ( layer-id layer-role -- ptr u8 n ) {: br:layer-role :}
   LAYER-INDEX
   dup 0 < if E-LAYER throw then
   0 NAME-LEN !
   s" h." APPEND-NAME
   APPEND-INDEX
   s" ." APPEND-NAME  br LAYER-PATH APPEND-NAME
   NAME-BUF NAME-LEN @ ;

: NAME-SPAN ( tensor-id -- ptr u8 n )      \ private span; public access copies out below
   MATCH tensor-id
      global OF GLOBAL-NAME ENDOF
      layer  OF LAYER-NAME ENDOF
   ;MATCH ;

\ ---- shapes from the common geometry (each ( nctx nvocab nembd -- rank d0..d3 )) ----
: GEOMETRY ( GPT2:config -- GPT2:config n n n )
   NEMBD@ >r NVOCAB@ >r NCTX@ r> r> ;

: WTE-SHAPE ( n n n -- n n n n n ) {: cx:n vo:n ne:n :}
   2 vo ne 1 1 ;
: WPE-SHAPE ( n n n -- n n n n n ) {: cx:n vo:n ne:n :}
   2 cx ne 1 1 ;
: VECTOR-SHAPE ( n n n -- n n n n n ) {: cx:n vo:n ne:n :}
   1 ne 1 1 1 ;
: MASK-SHAPE ( n n n -- n n n n n ) {: cx:n vo:n ne:n :}
   4 1 1 cx cx ;
: QKV-W-SHAPE ( n n n -- n n n n n ) {: cx:n vo:n ne:n :}
   2 ne  ne 3 CHECKED-MUL  1 1 ;
: QKV-B-SHAPE ( n n n -- n n n n n ) {: cx:n vo:n ne:n :}
   1  ne 3 CHECKED-MUL  1 1 1 ;
: ATTN-PROJ-SHAPE ( n n n -- n n n n n ) {: cx:n vo:n ne:n :}
   2 ne ne 1 1 ;
: FC-W-SHAPE ( n n n -- n n n n n ) {: cx:n vo:n ne:n :}
   2 ne  ne 4 CHECKED-MUL  1 1 ;
: FC-B-SHAPE ( n n n -- n n n n n ) {: cx:n vo:n ne:n :}
   1  ne 4 CHECKED-MUL  1 1 1 ;
: MLP-PROJ-SHAPE ( n n n -- n n n n n ) {: cx:n vo:n ne:n :}
   2  ne 4 CHECKED-MUL  ne 1 1 ;

: SHAPE-GLOBAL-ROLE ( n n n global-role -- n n n n n )
   MATCH global-role
      wte   OF WTE-SHAPE ENDOF
      wpe   OF WPE-SHAPE ENDOF
      lnf-g OF VECTOR-SHAPE ENDOF
      lnf-b OF VECTOR-SHAPE ENDOF
   ;MATCH ;

: SHAPE-LAYER-ROLE ( n n n layer-role -- n n n n n )
   MATCH layer-role
      ln1-g   OF VECTOR-SHAPE ENDOF
      ln1-b   OF VECTOR-SHAPE ENDOF
      mask    OF MASK-SHAPE ENDOF
      qkv-w   OF QKV-W-SHAPE ENDOF
      qkv-b   OF QKV-B-SHAPE ENDOF
      aproj-w OF ATTN-PROJ-SHAPE ENDOF
      aproj-b OF VECTOR-SHAPE ENDOF
      ln2-g   OF VECTOR-SHAPE ENDOF
      ln2-b   OF VECTOR-SHAPE ENDOF
      fc-w    OF FC-W-SHAPE ENDOF
      fc-b    OF FC-B-SHAPE ENDOF
      mproj-w OF MLP-PROJ-SHAPE ENDOF
      mproj-b OF VECTOR-SHAPE ENDOF
   ;MATCH ;

\ the full element count d0*d1*d2*d3 must fit a cell, not just each composed
\ dim: a per-dim-valid mask [1,1,cx,cx] or qkv [ne,3ne] can still overflow the
\ pair product (the measured 4e9-nctx and 2^40-nembd cases).
: CHECK-SHAPE ( n n n n n -- n n n n n ) {: r:n d0:n d1:n d2:n d3:n :}
   d0 d1 CHECKED-MUL d2 CHECKED-MUL d3 CHECKED-MUL drop
   r d0 d1 d2 d3 ;

: SHAPE-GLOBAL ( GPT2:config global-role -- GPT2:config n n n n n )
   >r GEOMETRY r> SHAPE-GLOBAL-ROLE CHECK-SHAPE ;

\ shape is layer-independent: the layer-id is dropped unread.
: SHAPE-LAYER ( GPT2:config layer-id layer-role -- GPT2:config n n n n n ) {: br:layer-role :}
   drop
   GEOMETRY br SHAPE-LAYER-ROLE CHECK-SHAPE ;

\ ---- slot ordinals (declaration order; exhaustive MATCH pins the role count) -------
: GLOBAL-ORD ( global-role -- n )
   MATCH global-role
      wte   OF 0 ENDOF
      wpe   OF 1 ENDOF
      lnf-g OF 2 ENDOF
      lnf-b OF 3 ENDOF
   ;MATCH ;

: LAYER-ORD ( layer-role -- n )
   MATCH layer-role
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

: SLOT-GLOBAL ( GPT2:config global-role -- GPT2:config n )
   GLOBAL-ORD ;

\ The embedded index revalidates against the consuming config on every lookup;
\ COUNT then proves 4 + 13*nlayer fits a cell. After those gates the multiply
\ cannot wrap and the slot is in [GLOBAL-COUNT, census) always.
: SLOT-LAYER ( GPT2:config layer-id layer-role -- GPT2:config n ) {: br:layer-role :}
   LAYER-INDEX
   CHECK-LAYER-INDEX
   >r COUNT drop r>
   LAYER-ROLE-COUNT * br LAYER-ORD + GLOBAL-COUNT + ;

: SLOT-INDEX ( n -- CAD-NUM:index )
   CAD-NUM:INDEX
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                            negative OF E-SIZE throw ENDOF
      zero OF E-SIZE throw ENDOF             overflow OF E-SIZE throw ENDOF
      underflow OF E-SIZE throw ENDOF        bad-alignment OF E-SIZE throw ENDOF
      misaligned OF E-SIZE throw ENDOF
   ;MATCH ;

CAST: SLOT>N ( CAD-NUM:index -- n ) ;

: ORD>GLOBAL ( n -- global-role )
   case
      0 of GPT2-GLOBAL--ROLE:WTE endof
      1 of GPT2-GLOBAL--ROLE:WPE endof
      2 of GPT2-GLOBAL--ROLE:LNF-G endof
      3 of GPT2-GLOBAL--ROLE:LNF-B endof
      E-SLOT throw
   endcase ;

: ORD>LAYER ( n -- layer-role )
   case
      0 of GPT2-LAYER--ROLE:LN1-G endof
      1 of GPT2-LAYER--ROLE:LN1-B endof
      2 of GPT2-LAYER--ROLE:MASK endof
      3 of GPT2-LAYER--ROLE:QKV-W endof
      4 of GPT2-LAYER--ROLE:QKV-B endof
      5 of GPT2-LAYER--ROLE:APROJ-W endof
      6 of GPT2-LAYER--ROLE:APROJ-B endof
      7 of GPT2-LAYER--ROLE:LN2-G endof
      8 of GPT2-LAYER--ROLE:LN2-B endof
      9 of GPT2-LAYER--ROLE:FC-W endof
      10 of GPT2-LAYER--ROLE:FC-B endof
      11 of GPT2-LAYER--ROLE:MPROJ-W endof
      12 of GPT2-LAYER--ROLE:MPROJ-B endof
      E-SLOT throw
   endcase ;

: GLOBAL-FOR-SLOT ( GPT2:config n -- GPT2:config tensor-id )
   ORD>GLOBAL GPT2-TENSOR--ID:GLOBAL ;

: LAYER-FOR-SLOT ( GPT2:config n -- GPT2:config tensor-id )
   GLOBAL-COUNT -
   LAYER-ROLE-COUNT /mod {: br:n l:n :}
   l LAYER-ID br ORD>LAYER GPT2-TENSOR--ID:LAYER ;

: LAYER-ORIENTATION ( layer-id layer-role -- orientation ) {: br:layer-role :}
   drop br
   MATCH layer-role
      ln1-g   OF GPT2-ORIENTATION:PLAIN ENDOF
      ln1-b   OF GPT2-ORIENTATION:PLAIN ENDOF
      mask    OF GPT2-ORIENTATION:PLAIN ENDOF
      qkv-w   OF GPT2-ORIENTATION:CONV1D ENDOF
      qkv-b   OF GPT2-ORIENTATION:PLAIN ENDOF
      aproj-w OF GPT2-ORIENTATION:CONV1D ENDOF
      aproj-b OF GPT2-ORIENTATION:PLAIN ENDOF
      ln2-g   OF GPT2-ORIENTATION:PLAIN ENDOF
      ln2-b   OF GPT2-ORIENTATION:PLAIN ENDOF
      fc-w    OF GPT2-ORIENTATION:CONV1D ENDOF
      fc-b    OF GPT2-ORIENTATION:PLAIN ENDOF
      mproj-w OF GPT2-ORIENTATION:CONV1D ENDOF
      mproj-b OF GPT2-ORIENTATION:PLAIN ENDOF
   ;MATCH ;

public

\ ---- the exact HF tensor name, copied into the caller's buffer ----------------------
\ NONE when the destination capacity is smaller than the name, with nothing
\ written (the SAFET:COPY-NAME? contract); SOME holds the copied byte length.
: COPY-NAME? ( tensor-id ptr u8 n -- option<n> ) {: cap:n :}
   >r NAME-SPAN r>
   over cap > if
      drop drop drop OPTION:NONE exit
   then
   over >r
   swap BYTE-COPY
   r> OPTION:SOME ;

\ ---- expected shape: rank d0 d1 d2 d3, 1-padded (see header) ------------------------
: SHAPE ( GPT2:config tensor-id -- GPT2:config n n n n n )
   MATCH tensor-id
      global OF SHAPE-GLOBAL ENDOF
      layer  OF SHAPE-LAYER ENDOF
   ;MATCH ;

\ ---- declared storage orientation (adapter convention; see header) ------------------
: ORIENTATION ( tensor-id -- orientation )
   MATCH tensor-id
      global OF drop GPT2-ORIENTATION:PLAIN ENDOF
      layer  OF LAYER-ORIENTATION ENDOF
   ;MATCH ;

\ ---- dense slot: globals 0..3, then 4 + layer*13 + role ordinal ---------------------
\ The layer arm revalidates the embedded index against its consuming config
\ before any slot arithmetic (E-LAYER; see header).
: SLOT ( GPT2:config tensor-id -- GPT2:config CAD-NUM:index )
   MATCH tensor-id
      global OF SLOT-GLOBAL ENDOF
      layer  OF SLOT-LAYER ENDOF
   ;MATCH
   SLOT-INDEX ;

\ ---- checked inverse: consuming config census defines the valid slot set -----------
: TENSOR-ID-FOR-SLOT
   ( GPT2:config CAD-NUM:index -- GPT2:config tensor-id )
   SLOT>N {: s:n :}
   COUNT {: census:n :}
   s 0 <  s census >=  or if E-SLOT throw then
   s GLOBAL-COUNT < if
      s GLOBAL-FOR-SLOT
   else
      s LAYER-FOR-SLOT
   then ;

;package
