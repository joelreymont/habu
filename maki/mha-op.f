\ maki/mha-op.f - the fused multi-head causal self-attention OP-SURFACE BRIDGE
\ (dot habu-op-mha-fused). One concern: the pieces the generic op machinery
\ (op-kind / op-registry / executor / backward / cad) needs to thread the raw
\ maki/mha.f kernel (MHA-FWD / MHA-BWD) through the model op surface as a seg-attn-
\ style fused node, WITHOUT touching mha.f. This is to segment.f/seg-attn what mha.f
\ is to the raw kernel: segment.f owns SEG-PACK/SEG-T@/SEG-CAUSAL@ next to its
\ references, and this file owns the analogous MHA op surface. It lives in its own
\ file because the package diff gate correctly reds MHA-* appends to mha.f
\ (E-REDUNDANT-FILE-PREFIX: the file stem `mha` equals the word prefix); the bridge
\ role prefix belongs in a bridge file.
\
\ What it owns:
\   - the ATTR codec carrying the head count H, the sequence length T, and the causal
\     flag - the geometry NOT recoverable from the 2D (B*T, C) tensor descriptors
\     (B = rows/T and hd = C/H are DERIVED; the seg-attn attr shape extended with H);
\   - the WORKLOAD CONFIG the MODEL: composer resolves into each node's attr, the
\     dropout config precedent (maki/dropout.f DO-CFG>ATTR): a run sets heads / seq
\     once, capture stamps every MHA node. v1 is CAUSAL-ONLY (GPT-2 c_attn), so
\     MHA-CFG>ATTR always packs causal = true and the geometry validator rejects a
\     non-causal bind - non-causal MHA is successor work;
\   - the fail-closed GEOMETRY validators MODEL: capture, bind-shape reprop, and the
\     host executor run:
\     every extent is a compile-time SPEC: constant (#MB/#MQ/#MC/#MH/#MD), so the op
\     runs at exactly the toy oracle shape and any other bind is E-MHA-GEOM - never a
\     silent wrong-shape run. Extents come from the BIND (input shapes + attr),
\     VALIDATED AGAINST - never sourced from - those constants;
\   - the COMBINED-GRADIENT buffer layout the fused backward writes and the reverse
\     transform slices (the seg-attn combined [dQ;dK;dV] precedent, generalized to five
\     differently-shaped gradients laid out contiguously in one (TOTAL, 1) buffer);
\   - the BACKWARD self-containment recompute (MHA-BWD-RECOMPUTE): MHA-BWD reads the
\     module-private tape that the LAST MHA-FWD wrote, so a model with two MHA nodes
\     would otherwise differentiate node 1 from node 2's tape (silently wrong). The
\     mha-bwd op node carries all five forward inputs plus dY, and EX-MHA-BWD re-runs
\     MHA-FWD from THIS node's own inputs into private scratch immediately before
\     MHA-BWD - so every backward derives from its own forward (segment.f's backward
\     recomputes attention from saved inputs; same discipline, whole-kernel scale).
\
\ maki -> habu only; mha-op owns -5178.

require maki/mha.f          \ MHA-FWD / MHA-BWD + the private extents #MB/#MQ/#MC/#MH/#MD, MHA-BT, MHA-3C

package MAKI
public

\ mha-op owns -5178 (package-scoped, consumed only inside MAKI: the geometry validators here
\ and the mha-op-test negative fixtures; the package diff gate requires new-file definitions to
\ have a package owner).
-5178 constant E-MHA-GEOM   \ fused MHA op bind extents / attr disagree with the host reference's fixed geometry

\ ---- attr cell codec: T (bits 19:0) + H (bits 31:20) + causal flag (bit 32) ----------
$FFFFF constant MHA-TMASK        \ 20-bit sequence length T (max 1048575)
20     constant MHA-H-SH         \ head-count field shift
$FFF   constant MHA-HMASK        \ 12-bit head count H (max 4095)
32     constant MHA-CAUSAL-SH    \ causal flag bit
private
: MHA-FIELD-CK ( n n -- n ) {: value:n mask:n :}
   value 0 < value mask > or if E-MHA-GEOM throw then
   value ;
public
: MHA-PACK ( n n bool -- n ) {: t:n h:n c:bool :}
   t MHA-TMASK MHA-FIELD-CK
   h MHA-HMASK MHA-FIELD-CK MHA-H-SH lshift or
   c if 1 MHA-CAUSAL-SH lshift or then ;
: MHA-T@ ( n -- n )      MHA-TMASK and ;
: MHA-H@ ( n -- n )      MHA-H-SH rshift MHA-HMASK and ;
: MHA-CAUSAL@ ( n -- bool )  MHA-CAUSAL-SH rshift 1 and 0= 0= ;

\ ---- workload config (the PLAN:MHA composer resolves it into each node's attr) --------
\ Heads (n_head) and sequence length (block size T) are model-level GPT-2 hyperparameters,
\ set once per run like the dropout mode/p. They default to the reference toy shape so an
\ unconfigured composition still binds at the oracle geometry; a fixture sets them to prove
\ the config path. v1 is causal-only, so the attr always packs causal = true and the
\ geometry validator rejects a non-causal bind.
variable MHA-HEADS-V   #MH MHA-HEADS-V !
variable MHA-SEQ-V     #MQ MHA-SEQ-V !
: MHA-HEADS! ( n -- )  MHA-HEADS-V ! ;
: MHA-HEADS@ ( -- n )  MHA-HEADS-V @ ;
: MHA-SEQ! ( n -- )    MHA-SEQ-V ! ;
: MHA-SEQ@ ( -- n )    MHA-SEQ-V @ ;
: MHA-CFG>ATTR ( -- n )  MHA-SEQ@ MHA-HEADS@ true MHA-PACK ;

\ ---- fail-closed geometry validation against the fixed host-reference oracle shape ----
\ MHA-GEOM-CK proves the input X extents (B*T, C) plus the attr's H / T / causal match the
\ compile-time reference geometry: MHA-FWD/MHA-BWD read module scratch sized to #MB/#MQ/#MC/
\ #MH/#MD, so any other bind would over/under-read. B = rows/T and hd = C/H fall out once
\ rows = #MB*#MQ, cols = #MC, H = #MH, T = #MQ hold; causal must be true (v1 causal-only).
: MHA-GEOM-CK ( n n n n bool -- ) {: xrows:n xcols:n h:n t:n c:bool :}
   xrows #MB #MQ * <> if E-MHA-GEOM throw then      \ B*T
   xcols #MC <> if E-MHA-GEOM throw then            \ C
   h #MH <> if E-MHA-GEOM throw then                \ heads
   t #MQ <> if E-MHA-GEOM throw then                \ sequence length (B = B*T / T)
   c 0= if E-MHA-GEOM throw then ;                  \ non-causal is unimplemented in v1
: MHA-DY-CK ( n n n n -- ) {: rows:n cols:n er:n ec:n :}
   rows er <> cols ec <> or if E-MHA-GEOM throw then ;
\ One operand-geometry authority serves MODEL: capture, BIND-SHAPES, and executor
\ defense-in-depth. Zero means an extent is not bound yet; every known relationship
\ is still checked. The 3C check divides the actual width, avoiding overflow in 3*C.
private
: MHA-DIM-OK? ( n n -- bool ) {: actual:n expected:n :}
   actual 0= expected 0= or if true exit then
   actual expected = ;
: MHA-3C-OK? ( n n -- bool ) {: c:n actual:n :}
   c 0= actual 0= or if true exit then
   actual 3 mod 0= actual 3 / c = and ;
: MHA-FACTOR-OK? ( n n -- bool ) {: extent:n factor:n :}
   factor 0 <= if false exit then
   extent 0= if true exit then
   extent factor mod 0= ;
: MHA-PARAMS-OK? ( n n n n n n n n n -- bool ) \ ( c wqr wqc bqr bqc wor woc bor boc -- bool )
   {: c:n wqr:n wqc:n bqr:n bqc:n wor:n woc:n bor:n boc:n :}
   wqr c MHA-DIM-OK?
   c wqc MHA-3C-OK? and
   bqr 1 MHA-DIM-OK? and
   c bqc MHA-3C-OK? and
   wor c MHA-DIM-OK? and
   woc c MHA-DIM-OK? and
   bor 1 MHA-DIM-OK? and
   boc c MHA-DIM-OK? and ;
: MHA-SHAPE-OK? ( n n n n n n n n n n n n -- bool )
   {: xr:n xc:n h:n t:n wqr:n wqc:n bqr:n bqc:n wor:n woc:n bor:n boc:n :}
   xr t MHA-FACTOR-OK?
   xc h MHA-FACTOR-OK? and
   xc wqr wqc bqr bqc wor woc bor boc MHA-PARAMS-OK? and ;
public
: MHA-PARAMS-CK ( n n n n n n n n n -- )
   MHA-PARAMS-OK? 0= if E-MHA-GEOM throw then ;

\ ---- combined-gradient buffer layout (the fused backward writes it, the reverse
\ transform slices it). One (TOTAL, 1) buffer holds dX | dWqkv | dbqkv | dWo | dbo
\ contiguously in row-major; MHA-BWD-OFF is the cumulative CELL boundary of gradient
\ idx (0..5), so gradient i occupies cells [OFF(i), OFF(i+1)) and OFF(5) is TOTAL. bt =
\ B*T (X rows), c = C (X cols); all sizes derive from the bind, no baked constant.
: MHA-BWD-OFF ( n n n -- n ) {: bt:n c:n idx:n :}
   0
   idx 0 > if bt c *   + then        \ + dX    size B*T*C   -> dWqkv start
   idx 1 > if c 3 * c * + then       \ + dWqkv size C*3C    -> dbqkv start
   idx 2 > if c 3 *   + then         \ + dbqkv size 3C      -> dWo start
   idx 3 > if c c *   + then         \ + dWo   size C*C     -> dbo start
   idx 4 > if c       + then ;       \ + dbo   size C       -> TOTAL

private

\ throwaway forward-output buffer for the backward self-containment recompute: the real
\ product is the module-private tape MHA-FWD saves; this holds the discarded Y. Bounded by
\ the frozen toy geometry (B*T*C cells).
create MHA-RECOMP-Y  MHA-BT #MC * cells allot

public

\ Re-run MHA-FWD from THIS backward node's own five forward inputs so MHA-BWD differentiates
\ its OWN forward, not whichever MHA node ran last. The executor calls this immediately before
\ MHA-BWD; the recomputed Y is discarded (the saved tape is what MHA-BWD reads).
: MHA-BWD-RECOMPUTE ( ptr r ptr r ptr r ptr r ptr r -- )   \ ( xb wqkvb bqkvb wob bob -- )
   MHA-RECOMP-Y MHA-FWD ;

;package
