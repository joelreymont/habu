\ maki/fusion-plan.f - the fusion region planner (dot cad-2, traffic-only v1).
\
\ CAD-PLAN section 5. Deterministic greedy region growth over the model-IR node
\ table (maki/model-ir.f) in node order, guarded by the section 5.2 legality matrix
\ RESTRICTED to what is computable before the layout/schedule phases: class-pair
\ legality, single-use producers, movement dissolution verdicts, one contraction
\ per region (matmul boundary), and the two-row-reduce softmax budget. The section
\ 5.4 resource bounds (registers / smem / occupancy) land with the schedule work
\ (cad-4/cad-6), so this planner emits only the splits derivable today.
\
\ It SUPERSEDES maki/regions.f: region membership is a per-node region-id array, not
\ an adjacent span, so params and dissolved movement do not break a chain. Output:
\ per-node region id, per-region facts (member count, class bitmask), the typed
\ split list (reason + node), and the materialization flags written back into the IR
\ (interior compute nodes cleared; region outputs, model outputs, multi-use, and
\ materialize/gathered movement set). Traffic bytes are maki/traffic.f's concern.
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

\ ---- split reason tags (subset of CAD-PLAN 5.6 computable in cad-2) ----------
0 constant SR-MULTI-USE     \ multi-use producer materialized (no recompute in v1)
1 constant SR-MATMUL        \ a second contraction cannot share the region
2 constant SR-LAYOUT        \ movement verdict materialize/gathered breaks fusion
3 constant SR-BARRIER       \ reduction barrier / row-reduce budget exhausted
4 constant SR-N

private

128 constant FP-CAP         \ matches MIR-CAP (max nodes, hence max regions/splits)

create FP-RID FP-CAP cells allot        \ per-node region id
create FP-MMC FP-CAP cells allot         \ per-region matmul count
create FP-RRC FP-CAP cells allot         \ per-region row-reduce count
create FP-MEM FP-CAP cells allot         \ per-region member count
create FP-MIX FP-CAP cells allot         \ per-region class bitmask (1<<class)
variable FP-RN                            \ region count

create FP-SP-NODE   FP-CAP cells allot    \ per-split node id
create FP-SP-REASON FP-CAP cells allot    \ per-split reason tag
variable FP-SP-N
variable FP-BUILT?

\ ---- dataflow queries ------------------------------------------------------
: FP-CLASS ( n -- n )  MIR-OP@ OPR-CLASS ;   \ node -> op class

\ occurrences of operand ref among a node's operands  ( node ref -- count )
: FP-USES ( n n -- n ) {: nd:n ref:n :}
   0  nd MIR-IN-COUNT@ 0 ?do  nd i MIR-IN@ ref = if 1+ then  loop ;

\ total times producer node's output is consumed across the table  ( prod -- count )
: FP-REF-USES ( n -- n ) {: prod:n :}
   0  MIR-N@ 0 ?do  i prod FP-USES +  loop ;

\ chain producer: operand-0 node ref, or -1 when it names a model input / none
: FP-PROD ( n -- n ) {: nd:n :}
   nd MIR-IN-COUNT@ 0= if -1 exit then
   nd 0 MIR-IN@ {: ref:n :}
   ref MIR-REF-INPUT? if -1 else ref then ;

\ ---- movement dissolution (verdict facts, maki/move-facts.f) ----------------
: NODE-MAT-VD? ( n -- bool ) {: nd:n :}   \ movement w/ materialize|gathered verdict
   nd MIR-MOVE? 0= if false exit then
   nd MIR-MOVE-VERDICT@ MV-VD-REPORTS? ;

: NODE-DISSOLVE? ( n -- bool ) {: nd:n :} \ movement w/ free|staged verdict
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

\ per-region capacity ( r cK -- bool ): one contraction per region; at most two
\ same-row reductions (softmax max+sum) and never mixed with a contraction.
: FP-CAP-OK? ( n n -- bool ) {: r:n ck:n :}
   ck CLASS-MATMUL = if
      r cells FP-MMC + @ 0=  r cells FP-RRC + @ 0=  and  exit then
   ck CLASS-ROW-REDUCE = if
      r cells FP-MMC + @ 0=  r cells FP-RRC + @ 2 <  and  exit then
   true ;

: FP-RID-RAW ( n -- n )  cells FP-RID + @ ;   \ node -> region id (unchecked)

\ does consumer K fuse into producer P's region?  ( K P -- bool )
: FP-JOIN? ( n n -- bool ) {: k:n p:n :}
   p 0 < if false exit then
   p FP-REF-USES 1 > if false exit then       \ multi-use producer is materialized
   k MIR-MOVE? if k NODE-DISSOLVE? exit then  \ movement K: join iff free/staged
   p NODE-MAT-VD? if false exit then          \ materialized movement is a boundary
   p FP-CLASS k FP-CLASS FP-BASE-FUSE? 0= if false exit then
   p FP-RID-RAW k FP-CLASS FP-CAP-OK? ;

\ ---- region assignment -----------------------------------------------------
: FP-NEW-REGION ( -- n )                     \ allocate a fresh region, zero its facts
   FP-RN @ FP-CAP >= if E-FP-CAP throw then
   FP-RN @ {: r:n :}
   0 r cells FP-MMC + !  0 r cells FP-RRC + !
   0 r cells FP-MEM + !  0 r cells FP-MIX + !
   FP-RN @ 1+ FP-RN !
   r ;

: FP-ADD ( n n -- ) {: k:n r:n :}            \ place K in region r; bump facts + budgets
   r k cells FP-RID + !
   r cells FP-MEM + dup @ 1+ swap !
   k FP-CLASS {: c:n :}
   1 c lshift  r cells FP-MIX + dup @ rot or swap !
   c CLASS-MATMUL     = if r cells FP-MMC + dup @ 1+ swap ! then
   c CLASS-ROW-REDUCE = if r cells FP-RRC + dup @ 1+ swap ! then ;

: FP-STEP ( n -- ) {: k:n :}
   k FP-PROD {: p:n :}
   k p FP-JOIN? if  k p FP-RID-RAW FP-ADD  else  k FP-NEW-REGION FP-ADD  then ;

: FP-ASSIGN ( -- )
   0 FP-RN !
   MIR-N@ 0 ?do  i FP-STEP  loop ;

\ ---- materialization flags (interior cleared; boundaries set) ---------------
: FP-REGION-OUT? ( n -- bool ) {: nd:n :}    \ consumed by a node in another region
   MIR-N@ 0 ?do
      i nd FP-USES 0 >  i FP-RID-RAW nd FP-RID-RAW <>  and if unloop true exit then
   loop false ;

: FP-MAT-FLAG ( n -- bool ) {: nd:n :}
   \ movement: materialize/gathered verdict, or a free/staged rewrite that still
   \ crosses a region boundary (it could not dissolve into the consumer).
   nd MIR-MOVE? if  nd NODE-MAT-VD?  nd FP-REGION-OUT?  or  exit then
   nd FP-REF-USES 0= if true exit then        \ model output
   nd FP-REF-USES 1 > if true exit then       \ multi-use producer
   nd FP-REGION-OUT? ;                          \ region output

: FP-MARK ( -- )
   MIR-N@ 0 ?do  i FP-MAT-FLAG i MIR-MAT!  loop ;

\ ---- split classification (typed reason per broken chain edge) --------------
\ ( K P -- tag ): P>=0, P not multi-use, K did not fuse into P
: FP-REASON ( n n -- n ) {: k:n p:n :}
   k MIR-MOVE? if k NODE-MAT-VD? if SR-LAYOUT exit then then
   p NODE-MAT-VD? if SR-LAYOUT exit then
   p FP-CLASS CLASS-MATMUL = k FP-CLASS CLASS-MATMUL = and if SR-MATMUL exit then
   k FP-CLASS CLASS-MATMUL = p FP-RID-RAW cells FP-MMC + @ 0 > and if SR-MATMUL exit then
   SR-BARRIER ;

: FP-SPLIT+ ( n n -- ) {: k:n tag:n :}
   FP-SP-N @ FP-CAP >= if E-FP-CAP throw then
   k   FP-SP-N @ cells FP-SP-NODE   + !
   tag FP-SP-N @ cells FP-SP-REASON + !
   FP-SP-N @ 1+ FP-SP-N ! ;

\ one node's contribution: its own multi-use materialize row, then its broken edge
: FP-SPLIT-STEP ( n -- ) {: k:n :}
   k FP-REF-USES 1 > if k SR-MULTI-USE FP-SPLIT+ then
   k FP-PROD {: p:n :}
   p 0 < if exit then
   k FP-RID-RAW p FP-RID-RAW = if exit then   \ fused: no split
   p FP-REF-USES 1 > if exit then              \ producer already reported multi-use
   k  k p FP-REASON  FP-SPLIT+ ;

: FP-SPLITS ( -- )
   0 FP-SP-N !
   MIR-N@ 0 ?do  i FP-SPLIT-STEP  loop ;

public

: FP-RESET ( -- )                            \ drop any prior plan (facts unusable)
   0 FP-RN !  0 FP-SP-N !  0 FP-BUILT? ! ;

: FP-BUILD ( -- )                            \ plan regions over the current IR
   FP-ASSIGN  FP-MARK  FP-SPLITS  -1 FP-BUILT? ! ;

: FP-CK ( -- )  FP-BUILT? @ 0= if E-FP-STATE throw then ;

: FP-REGION-COUNT ( -- n )  FP-CK FP-RN @ ;

: FP-RID@ ( n -- n ) {: nd:n :}              \ node -> region id (checked)
   nd 0 < nd MIR-N@ >= or if E-FP-IDX throw then
   nd FP-RID-RAW ;

: FP-RGN-CK ( n -- n )  FP-CK  dup 0 < over FP-RN @ >= or if E-FP-IDX throw then ;
: FP-REGION-MEMBERS  ( n -- n )  FP-RGN-CK cells FP-MEM + @ ;
: FP-REGION-CLASSMIX ( n -- n )  FP-RGN-CK cells FP-MIX + @ ;

: FP-SPLIT-COUNT ( -- n )  FP-CK FP-SP-N @ ;
: FP-SP-CK ( n -- n )  FP-CK  dup 0 < over FP-SP-N @ >= or if E-FP-IDX throw then ;
: FP-SPLIT-NODE@   ( n -- n )  FP-SP-CK cells FP-SP-NODE   + @ ;
: FP-SPLIT-REASON@ ( n -- n )  FP-SP-CK cells FP-SP-REASON + @ ;

: FP-REASON-NAME ( n -- ptr u8 n )           \ reason tag -> text
   case
      SR-MULTI-USE of s" multi-use-materialize" endof
      SR-MATMUL    of s" matmul-boundary"       endof
      SR-LAYOUT    of s" layout-conflict"       endof
      SR-BARRIER   of s" barrier-boundary"      endof
      E-FP-IDX throw
   endcase ;

\ ---- report integration (one "<reason> at node K" split row each) -----------
: FP-SPLIT-ROW$ ( n -- ptr u8 n ) {: i:n :}
   SB-RESET
   i FP-SPLIT-REASON@ FP-REASON-NAME SB-APPEND
   s"  at node " SB-APPEND  i FP-SPLIT-NODE@ SB-INT
   SB$ ;

: FP-REPORT+ ( report -- report )
   FP-SPLIT-COUNT 0 ?do  i FP-SPLIT-ROW$ RPT-SPLIT+  loop ;

end-package
