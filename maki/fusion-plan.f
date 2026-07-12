\ maki/fusion-plan.f - the fusion region planner (dot cad-2, traffic-only v1).
\
\ CAD-PLAN section 5. Deterministic greedy region growth over the model-IR node
\ table (maki/model-ir.f) in node order, guarded by the section 5.2 legality matrix
\ AND a backend-capability gate (FP-BACKEND-EMITS?) so every planned region is
\ lowerable-by-construction: FP-JOIN? refuses a legal-but-not-yet-emittable fusion
\ (an EW prologue into a matmul today) instead of minting an unlowerable region.
\ RESTRICTED to what is computable before the layout/schedule phases: class-pair
\ legality, backend capability, single-use producers, movement dissolution verdicts,
\ one contraction per region (matmul boundary), and the two-row-reduce softmax budget.
\ The section 5.4 resource bounds (registers / smem / occupancy) land with the schedule
\ work (cad-4/cad-6), so this planner emits only the splits derivable today.
\
\ It SUPERSEDES maki/regions.f: region membership is a per-node region-id array, not
\ an adjacent span, so params and dissolved movement do not break a chain. Output:
\ per-node region id, per-region facts (member count, class bitmask), the typed
\ split list (reason + node), and the materialization flags written back into the IR
\ (interior compute nodes cleared; region outputs, model outputs - compute OR a trailing
\ movement node, multi-use, and materialize/gathered movement set). Traffic bytes are
\ maki/traffic.f's concern.
\
\ Fail closed: node/region/split index out of range and an accessor used before
\ FP-BUILD are named throws. maki -> habu only; fusion-plan owns -5072..-5074.

require lib/prelude.f
require lib/string.f
require lib/fmt.f
require maki/model-ir.f
require maki/op-registry.f
require maki/move-facts.f
require maki/report.f

-5072 constant E-FP-CAP     \ region / split table capacity exceeded
-5073 constant E-FP-IDX     \ node / region / split index out of range
-5074 constant E-FP-STATE   \ region facts requested before FP-BUILD

package MAKI
public

\ ---- split reason family (CAD-PLAN 5.6 subset computable in cad-2) -----------
\ Real ENUM (width-1 layout value; dot habu-cad-adt-swap, capability S1 of
\ habu-checker-capability-typed): the per-split reason is stored/fetched through a
\ typed `ptr reason` slot (FP-SP-REASON-AT) and every reader dispatches with an
\ exhaustive MATCH, so adding a variant is a checker error at each dispatch site
\ rather than silent tag drift. There is NO durable/wire encoding of a reason -- it
\ only ever renders to report text (FP-REASON-NAME, the one boundary word) -- so no
\ numeric form is kept and the old SR-N range sentinel is retired by exhaustiveness.
\ Constructors: MAKI-REASON:MULTI-USE .. MAKI-REASON:BACKEND. Variants:
\   multi-use        multi-use producer materialized (no recompute in v1)
\   matmul           a second contraction cannot share the region
\   layout-conflict  movement verdict materialize/gathered breaks fusion
\   barrier          reduction barrier / row-reduce budget exhausted
\   backend          base-legal, but the lowering backend cannot emit this fusion (yet)
\ (the ENUM block reads raw variant tokens, so keep the descriptions here.)
\ NB: the variant is `layout-conflict`, not `layout`: `layout` is a type family
\ (tensor-value.f) and a variant tail may not share a name with a family.
ENUM reason
  multi-use
  matmul
  layout-conflict
  barrier
  backend
;ENUM

private

128 constant FP-CAP         \ matches MIR-CAP (max nodes, hence max regions/splits)

create FP-RID FP-CAP cells allot        \ per-node region id
create FP-MMC FP-CAP cells allot         \ per-region matmul count
create FP-RRC FP-CAP cells allot         \ per-region row-reduce count
create FP-MEM FP-CAP cells allot         \ per-region member count
create FP-MIX FP-CAP cells allot         \ per-region class bitmask (1<<class)
variable FP-RN                            \ region count

create FP-SP-NODE   FP-CAP cells allot    \ per-split node id
FP-CAP LAYOUT-BUFFER FP-SP-REASON reason  \ per-split reason enum
variable FP-SP-N
variable FP-BUILT?

\ The generative buffer owns extent, stride, bounds, and family provenance.
: FP-SP-REASON-AT ( n -- ptr reason )  FP-SP-REASON ;

\ ---- dataflow queries ------------------------------------------------------
: FP-CLASS ( CAD-KIND:node-id -- n )  MIR-OP@ OPR-CLASS ;   \ node -> op class

\ occurrences of operand ref among a node's operands
: FP-USES ( CAD-KIND:node-id MIR:operand-ref -- n )
   {: nd:CAD-KIND:node-id ref:MIR:operand-ref :}
   0 nd MIR-IN-COUNT@ 0 ?do
      nd i MIR-INPUT-IDX MIR-IN@ ref MIR-REF= if 1+ then
   loop ;

\ total times producer node's output is consumed across the table
: FP-REF-USES ( CAD-KIND:node-id -- n )
   MIR-NODE-REF {: ref:MIR:operand-ref :}
   0 MIR-N@ 0 ?do
      i MIR-NODE-ID ref FP-USES +
   loop ;

\ chain producer reference; call only when the node has an operand.
: FP-PROD ( CAD-KIND:node-id -- MIR:operand-ref )
   0 MIR-INPUT-IDX MIR-IN@ ;

\ ---- movement dissolution (verdict facts, maki/move-facts.f) ----------------
: NODE-MAT-VD? ( CAD-KIND:node-id -- bool )
   {: nd:CAD-KIND:node-id :}                \ movement w/ materialize|gathered verdict
   nd MIR-MOVE? 0= if false exit then
   nd MIR-MOVE-VERDICT@ MV-VD-REPORTS? ;

: NODE-DISSOLVE? ( CAD-KIND:node-id -- bool )
   {: nd:CAD-KIND:node-id :}                \ movement w/ free|staged verdict
   nd MIR-MOVE? 0= if false exit then
   nd MIR-MOVE-VERDICT@ MV-VD-REPORTS? 0= ;

\ ---- legality (CAD-PLAN 5.2, restricted) -----------------------------------
\ class-pair fusability ignoring per-region budgets ( cP cK -- bool ); a dissolved
\ movement producer is transparent (the chain reads through its index rewrite).
: FP-BASE-FUSE? ( n n -- bool ) {: cp:n ck:n :}
   cp CLASS-MOVEMENT = if true exit then
   cp CLASS-EW = if
      ck CLASS-EW = ck CLASS-ROW-REDUCE = or ck CLASS-MATMUL = or exit then
   cp CLASS-ROW-REDUCE = if
      ck CLASS-EW = ck CLASS-ROW-REDUCE = or exit then
   cp CLASS-MATMUL = if ck CLASS-EW = exit then
   cp CLASS-DECODE = if ck CLASS-EW = exit then
   false ;

\ ---- backend-capability gate (CAD-PLAN 5.2) --------------------------------
\ FP-BASE-FUSE? says what is LEGAL to want; this table says what the lowering backend
\ can actually EMIT, per (producer-class -> consumer-class). Every emittable pair is a
\ subset of the legal ones; a legal pair the backend cannot yet emit is cleared so the
\ planner never mints an unlowerable region (plan is lowerable-by-construction). Keeping
\ the two matrices separate preserves the desired-fusion knowledge (CAD-PLAN 8.1 prologue
\ lever) while matching the backend today: when the backend gains a capability, flip one
\ bit here + add its regression, and the planner fuses it again with no legality edit.
\
\ DATA: FP-CAP-ROW[cp] is a bitmask of emittable consumer classes (bit ck set = yes).
\   EW         -> EW, ROW-REDUCE           (NOT MATMUL: lower-mm cannot pre-transform A/B,
\                                            i.e. no prologue fusion -> E-LMM-PROLOGUE; the fix)
\   ROW-REDUCE -> EW, ROW-REDUCE           (softmax max+sum budget, backend-proven)
\   MATMUL     -> EW                        (fused epilogue)
\   MOVEMENT   -> (unused)                  verdict-governed in FP-JOIN?, never consulted here:
\                                            a dissolved FREE movement folds into index math for
\                                            every consumer; a STAGED (transpose) folds only into
\                                            an EW consumer (FP-STAGED-FOLDABLE?, lower-ew
\                                            EMIT-XPOSE-OFF) and is MATERIALIZED for RED/MM (its own
\                                            copy region, read coalesced); materialize/gathered are
\                                            boundaries. So every planned region stays lowerable.
\   DECODE     -> EW                        (fused epilogue)
create FP-CAP-ROW  CLASS-N cells allot
: FP-CAP! ( n n -- )  cells FP-CAP-ROW + ! ;      \ ( mask cls -- ) store row cls's emit-mask
: FP-CAP-INIT ( -- )
   1 CLASS-EW lshift  1 CLASS-ROW-REDUCE lshift or   CLASS-EW          FP-CAP!
   1 CLASS-EW lshift  1 CLASS-ROW-REDUCE lshift or   CLASS-ROW-REDUCE  FP-CAP!
   1 CLASS-EW lshift                                 CLASS-MATMUL      FP-CAP!
   0                                                 CLASS-MOVEMENT    FP-CAP!
   1 CLASS-EW lshift                                 CLASS-DECODE      FP-CAP! ;
FP-CAP-INIT

\ ---- fusion ON/OFF ablation toggle (docs/ablation.md; the paper's fusion ON/OFF row) --------
\ OFF ablates the fusion CAPABILITY, not the plan's soundness: every compute pair becomes
\ non-emittable (zeroed FP-CAP-ROW) AND movements stop dissolving (FP-MV-FUSE? off), so FP-JOIN?
\ refuses every edge and each node lands in its own region - "same per-node kernels, regions split"
\ (the paper's fusion ablation baseline; each singleton region is still lowerable-by-construction).
\ ON restores the default capability table + the movement folds. The state is PERSISTENT across
\ FP-BUILD (a build never resets it); FP-RESET restores ON as a fail-safe.
variable FP-MV-FUSE?                          \ movements dissolve/fuse? (default on)
-1 FP-MV-FUSE? !
: FP-CAP-ZERO ( -- )  CLASS-N 0 ?do  0 i FP-CAP!  loop ;   \ no compute pair emittable

\ can the backend EMIT a fused cp->ck edge?  ( cP cK -- bool )
: FP-BACKEND-EMITS? ( n n -- bool ) {: cp:n ck:n :}
   cp cells FP-CAP-ROW + @  ck rshift  1 and  0<> ;

\ can the backend fold a dissolved STAGED (transpose) movement into a consumer of class ck?
\ A transpose is a lane permutation, folded per-element off the reading kernel's flat index.
\ Only the FLAT EW kernel can absorb that (maki/lower-ew.f MVW-CHECK-EW / EMIT-XPOSE-OFF); RED's
\ coalesced row loads and MM's K-loop A/B addressing cannot express a transposed column read, so
\ the planner materializes the transpose for them (its own copy region) and the consumer reads it
\ coalesced. v1: EW only - flip this when a strided-column loader lands (habu-maki-fold-staged).
: FP-STAGED-FOLDABLE? ( n -- bool )  CLASS-EW = ;

\ per-region capacity ( r cK -- bool ): one contraction per region; at most two
\ same-row reductions (softmax max+sum) and never mixed with a contraction.
: FP-CAP-OK? ( n n -- bool ) {: r:n ck:n :}
   ck CLASS-MATMUL = if
      r cells FP-MMC + @ 0=  r cells FP-RRC + @ 0=  and  exit then
   ck CLASS-ROW-REDUCE = if
      r cells FP-MMC + @ 0=  r cells FP-RRC + @ 2 <  and  exit then
   true ;

: FP-RID-RAW ( CAD-KIND:node-id -- n )         \ node -> region id (unchecked)
   NODE>RAW cells FP-RID + @ ;

\ does consumer K fuse into producer P's region?
: FP-JOIN? ( CAD-KIND:node-id CAD-KIND:node-id -- bool )
   {: k:CAD-KIND:node-id p:CAD-KIND:node-id :}
   p FP-REF-USES 1 > if false exit then       \ multi-use producer is materialized
   k MIR-MOVE? if
      FP-MV-FUSE? @ 0= if false exit then     \ OFF: a movement never dissolves -> its own region
      k NODE-DISSOLVE? exit then              \ movement K: join iff free/staged (verdict)
   p NODE-MAT-VD? if false exit then          \ materialized movement producer is a boundary
   p MIR-MOVE? FP-MV-FUSE? @ 0= and if false exit then   \ OFF: a movement producer is a boundary too
   p NODE-DISSOLVE? if                        \ dissolved movement producer: gate STAGED by consumer class
      p MIR-MOVE-VERDICT@ MVV-STAGED =  k FP-CLASS FP-STAGED-FOLDABLE? 0=  and if false exit then
   then                                       \ FREE folds into all consumers; STAGED into EW only (else materialize)
   p FP-CLASS k FP-CLASS FP-BASE-FUSE? 0= if false exit then
   p MIR-MOVE? 0= if                          \ backend-capability gate: compute producers only
      p FP-CLASS k FP-CLASS FP-BACKEND-EMITS? 0= if false exit then
   then                                       \ (dissolved movement folds are verdict-governed above)
   p FP-RID-RAW k FP-CLASS FP-CAP-OK? ;

\ ---- region assignment -----------------------------------------------------
: FP-NEW-REGION ( -- n )                     \ allocate a fresh region, zero its facts
   FP-RN @ FP-CAP >= if E-FP-CAP throw then
   FP-RN @ {: r:n :}
   0 r cells FP-MMC + !  0 r cells FP-RRC + !
   0 r cells FP-MEM + !  0 r cells FP-MIX + !
   FP-RN @ 1+ FP-RN !
   r ;

: FP-ADD ( CAD-KIND:node-id n -- )           \ place K in region r; bump facts + budgets
   {: k:CAD-KIND:node-id r:n :}
   r k NODE>RAW cells FP-RID + !
   r cells FP-MEM + dup @ 1+ swap !
   k FP-CLASS {: c:n :}
   1 c lshift  r cells FP-MIX + dup @ rot or swap !
   c CLASS-MATMUL     = if r cells FP-MMC + dup @ 1+ swap ! then
   c CLASS-ROW-REDUCE = if r cells FP-RRC + dup @ 1+ swap ! then ;

: FP-STEP ( CAD-KIND:node-id -- ) {: k:CAD-KIND:node-id :}
   k MIR-IN-COUNT@ 0= if k FP-NEW-REGION FP-ADD exit then
   k FP-PROD {: ref:MIR:operand-ref :}
   ref MIR-REF-INPUT? if k FP-NEW-REGION FP-ADD exit then
   ref MIR-REF-NODE {: p:CAD-KIND:node-id :}
   k p FP-JOIN? if
      k p FP-RID-RAW FP-ADD
   else
      k FP-NEW-REGION FP-ADD
   then ;

: FP-ASSIGN ( -- )
   0 FP-RN !
   MIR-N@ 0 ?do  i MIR-NODE-ID FP-STEP  loop ;

\ ---- materialization flags (interior cleared; boundaries set) ---------------
: FP-REGION-OUT? ( CAD-KIND:node-id -- bool )   \ consumed by a node in another region
   {: nd:CAD-KIND:node-id :}
   nd MIR-NODE-REF {: ref:MIR:operand-ref :}
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node ref FP-USES 0 >
      node FP-RID-RAW nd FP-RID-RAW <> and if unloop true exit then
   loop false ;

: FP-MAT-FLAG ( CAD-KIND:node-id -- bool ) {: nd:CAD-KIND:node-id :}
   \ A model output (no consumer) always materializes - whether it is a compute node
   \ or a trailing movement node (a standalone/trailing TRANSPOSE/SLICE that IS the model
   \ output; without this its region has zero materialized outputs -> lower E-LMV-NOOUT).
   nd FP-REF-USES 0= if true exit then        \ model output (compute or movement)
   \ movement (non-output): materialize/gathered verdict, or a free/staged rewrite that
   \ still crosses a region boundary (it could not dissolve into the consumer).
   nd MIR-MOVE? if  nd NODE-MAT-VD?  nd FP-REGION-OUT?  or  exit then
   nd FP-REF-USES 1 > if true exit then       \ multi-use producer
   nd FP-REGION-OUT? ;                          \ region output

: FP-MARK ( -- )
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node FP-MAT-FLAG node MIR-MAT!
   loop ;

\ ---- split classification (typed reason per broken chain edge) --------------
\ K did not fuse into P; both are committed nodes.
: FP-REASON ( CAD-KIND:node-id CAD-KIND:node-id -- reason )
   {: k:CAD-KIND:node-id p:CAD-KIND:node-id :}
   k MIR-MOVE? if k NODE-MAT-VD? if MAKI-REASON:LAYOUT-CONFLICT exit then then
   p NODE-MAT-VD? if MAKI-REASON:LAYOUT-CONFLICT exit then
   p FP-CLASS CLASS-MATMUL = k FP-CLASS CLASS-MATMUL = and if MAKI-REASON:MATMUL exit then
   k FP-CLASS CLASS-MATMUL = p FP-RID-RAW cells FP-MMC + @ 0 > and if MAKI-REASON:MATMUL exit then
   p MIR-MOVE? 0=                                       \ compute edge, base-legal, but the
   p FP-CLASS k FP-CLASS FP-BASE-FUSE?      and         \ backend cannot emit it (e.g. EW prologue
   p FP-CLASS k FP-CLASS FP-BACKEND-EMITS? 0= and if MAKI-REASON:BACKEND exit then  \ into a matmul)
   MAKI-REASON:BARRIER ;

\ append one split row. The reason enum arrives on TOP and is stored first: a
\ width-1 layout value cannot be bound into a local (it is a whole bundle), so it
\ stays on the stack and crosses `!` through its typed slot with no juggling.
: FP-SP-NODE! ( CAD-KIND:node-id n -- )
   cells FP-SP-NODE + ! ;

: FP-SP-NODE@ ( n -- CAD-KIND:node-id )
   cells FP-SP-NODE + @ ;

: FP-SPLIT+ ( CAD-KIND:node-id reason -- )
   FP-SP-N @ FP-CAP >= if E-FP-CAP throw then
   FP-SP-N @ FP-SP-REASON-AT !               \ reason (top) -> its `ptr reason` slot
   FP-SP-N @ FP-SP-NODE!                     \ node id -> the node column
   FP-SP-N @ 1+ FP-SP-N ! ;

\ one node's contribution: its own multi-use materialize row, then its broken edge
: FP-SPLIT-STEP ( CAD-KIND:node-id -- ) {: k:CAD-KIND:node-id :}
   k FP-REF-USES 1 > if k MAKI-REASON:MULTI-USE FP-SPLIT+ then
   k MIR-IN-COUNT@ 0= if exit then
   k FP-PROD {: ref:MIR:operand-ref :}
   ref MIR-REF-INPUT? if exit then
   ref MIR-REF-NODE {: p:CAD-KIND:node-id :}
   k FP-RID-RAW p FP-RID-RAW = if exit then   \ fused: no split
   p FP-REF-USES 1 > if exit then              \ producer already reported multi-use
   k  k p FP-REASON  FP-SPLIT+ ;

: FP-SPLITS ( -- )
   0 FP-SP-N !
   MIR-N@ 0 ?do  i MIR-NODE-ID FP-SPLIT-STEP  loop ;

public

\ ---- fusion ON/OFF toggle (persistent across FP-BUILD; FP-RESET restores ON) -----------------
: FP-FUSE-ON! ( -- )   FP-CAP-INIT  -1 FP-MV-FUSE? ! ;   \ default capability table + movement folds
: FP-FUSE-OFF! ( -- )  FP-CAP-ZERO   0 FP-MV-FUSE? ! ;   \ ablate fusion: every node its own region
: FP-FUSED? ( -- bool )  FP-MV-FUSE? @ 0<> ;             \ true when fusion is ON

: FP-RESET ( -- )                            \ drop any prior plan (facts unusable); restore fusion ON
   FP-FUSE-ON!  0 FP-RN !  0 FP-SP-N !  0 FP-BUILT? ! ;

: FP-BUILD ( -- )                            \ plan regions over the current IR
   FP-ASSIGN  FP-MARK  FP-SPLITS  -1 FP-BUILT? ! ;

: FP-CK ( -- )  FP-BUILT? @ 0= if E-FP-STATE throw then ;

: FP-REGION-COUNT ( -- n )  FP-CK FP-RN @ ;

private

\ ---- nominal region handle (Model-CAD V2 R3, dot habu-maki-apply-cad-27b7a7d7)
\ These private identity boundaries are the only raw representation authority;
\ the public constructors (FP-REGION-ID / FP-RID@) validate the raw table
\ position first, and the FP-RID table stays indexed behind RGN>RAW.
TRUSTED: RAW>RGN ( n -- CAD-KIND:region ) ;
TRUSTED: RGN>RAW ( CAD-KIND:region -- n ) ;

: RGN-RAW-CK ( n -- n )
   dup 0 < over FP-RN @ >= or if E-FP-IDX throw then ;

public

\ ---- validated raw-index refinement (loop counters enter here) --------------
: FP-REGION-ID ( n -- CAD-KIND:region )
   FP-CK RGN-RAW-CK RAW>RGN ;

: FP-RID@ ( CAD-KIND:node-id -- CAD-KIND:region )   \ node -> region id (checked)
   {: nd:CAD-KIND:node-id :}
   FP-CK
   nd NODE>RAW {: raw:n :}
   raw 0 < raw MIR-N@ >= or if E-FP-IDX throw then
   nd FP-RID-RAW RAW>RGN ;

: FP-RGN= ( CAD-KIND:region CAD-KIND:region -- bool )
   RGN>RAW swap RGN>RAW = ;

private

\ region -> revalidated raw table position (a stale id after a rebuild rejects)
: FP-RGN-CK ( CAD-KIND:region -- n )
   FP-CK RGN>RAW RGN-RAW-CK ;

public

: FP-REGION-MEMBERS  ( CAD-KIND:region -- n )  FP-RGN-CK cells FP-MEM + @ ;
: FP-REGION-CLASSMIX ( CAD-KIND:region -- n )  FP-RGN-CK cells FP-MIX + @ ;

: FP-SPLIT-COUNT ( -- n )  FP-CK FP-SP-N @ ;
: FP-SP-CK ( n -- n )  FP-CK  dup 0 < over FP-SP-N @ >= or if E-FP-IDX throw then ;
: FP-SPLIT-NODE@ ( n -- CAD-KIND:node-id )  FP-SP-CK FP-SP-NODE@ ;
: FP-SPLIT-REASON@ ( n -- reason )  FP-SP-CK FP-SP-REASON-AT @ ;

\ the one reason render boundary: enum -> report text. Exhaustive MATCH means a bad
\ tag is unrepresentable, so the old E-FP-IDX default is gone; a new variant makes
\ this fail to compile until it is rendered here.
: FP-REASON-NAME ( reason -- ptr u8 n )
   MATCH reason
      multi-use OF s" multi-use-materialize" ENDOF
      matmul    OF s" matmul-boundary"       ENDOF
      layout-conflict OF s" layout-conflict" ENDOF
      barrier   OF s" barrier-boundary"      ENDOF
      backend   OF s" backend-capability"    ENDOF
   ;MATCH ;

\ ---- report integration (one "<reason> at node K" split row each) -----------
: FP-SPLIT-ROW$ ( n -- ptr u8 n ) {: i:n :}
   SB-RESET
   i FP-SPLIT-REASON@ FP-REASON-NAME SB-APPEND
   s"  at node " SB-APPEND i FP-SPLIT-NODE@ NODE>RAW SB-INT
   SB$ ;

: FP-REPORT+ ( report -- report )
   FP-SPLIT-COUNT 0 ?do  i FP-SPLIT-ROW$ REPORT:SPLIT+  loop ;

end-package
