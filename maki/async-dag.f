\ maki/async-dag.f - the typed immutable async execution DAG (V2 section 22.3, host schema).
\
\ MODEL-CAD-V2-PLAN section 22.3: streams, events, and dependencies are typed
\ resources, not hidden driver state. This file is the HOST schema: a flat node
\ table (the model-ir.f typed-index pattern) whose nodes carry the async op sum
\ `akind` - kernel / copy / memset / event-record / event-wait - and each node
\ is owned by exactly one stream. Identities are nominal (ADAG:node-id /
\ ADAG:stream-id / ADAG:event-id): a raw n cannot forge one, and the private
\ RAW>* mints are the only representation authority (R3 owner-module rule).
\
\ Ordering is explicit edges, never hidden driver state: appending a node adds
\ the same-stream program-order edge from its stream's tail; ADAG-WAIT+ adds
\ the record->wait edge from the event's latest record node; ADAG-DEP+ adds an
\ explicit same-stream data edge. Cross-stream ordering is ONLY expressible
\ through an event record/wait pair - a raw cross-stream ADAG-DEP+ rejects
\ (E-ADAG-XSTREAM), and a wait on a never-recorded event is use-before-ready
\ (E-ADAG-UNREADY). Because every legal edge points from an earlier to a later
\ node, insertion order is a topological order by construction; ADAG-SEAL does
\ not trust the builder - it freezes the graph (mutators reject E-ADAG-SEALED)
\ and runs Kahn's algorithm with the smallest-ready-index tie break, fixing the
\ canonical replay order once (same DAG -> byte-identical order, pinned by
\ ADAG-RENDER) and rejecting any leftover node as a cycle (E-ADAG-CYCLE).
\
\ Event destruction is handle-lifecycle bookkeeping, not graph mutation, so it
\ stays legal after seal: double destroy rejects (E-ADAG-EDESTROY) and
\ record/wait on a destroyed event rejects (E-ADAG-EUSE). ADAG-RESET is the
\ deterministic cleanup: it retires every stream/event/node handle at once
\ (a held handle fails its bound revalidation, E-ADAG-IDX).
\
\ One concern: the DAG schema + its sealed replay order. Lowering the model IR
\ into this DAG and driving the host executor is maki/plan-ir.f; executing the
\ same schema on device streams is the Orin-gated leg (dot
\ habu-v2-checked-async-8d460576). maki -> habu only; async-dag owns -5290..-5299.

require lib/string.f
require lib/fmt.f
require maki/cad-kinds.f

-5290 constant E-ADAG-CAP       \ node / stream / event / edge / render capacity exceeded
-5291 constant E-ADAG-IDX       \ node / stream / event / order index out of range (incl. stale handles)
-5292 constant E-ADAG-SEALED    \ graph mutation or reseal after ADAG-SEAL
-5293 constant E-ADAG-UNSEALED  \ replay order / render read before ADAG-SEAL
-5294 constant E-ADAG-KIND      \ payload accessor used on the wrong node kind
-5295 constant E-ADAG-UNREADY   \ event-wait on an event with no record node (use-before-ready)
-5296 constant E-ADAG-XSTREAM   \ raw cross-stream dependency (must use an event record/wait pair)
-5297 constant E-ADAG-EDESTROY  \ event destroyed twice
-5298 constant E-ADAG-EUSE      \ record / wait on a destroyed event
-5299 constant E-ADAG-CYCLE     \ dependency cycle detected at seal

package ADAG
public

NEWTYPE stream-id 0
NEWTYPE event-id 0
NEWTYPE node-id 0

PRODUCT stats 0
   FIELD nodes n
   FIELD streams n
   FIELD events n
   FIELD edges n
;PRODUCT

;package

package MAKI
public

\ ---- the async node-kind sum (one variant per V2 22.3 node class) -----------
\ DERIVE eq generates the typed identity compare MAKI-AKIND:EQ ( akind akind -- bool ).
ENUM akind DERIVE eq
  kernel copy memset event-record event-wait
;ENUM

\ named render boundary (the DT-KEY / PROV-KEY convention): exhaustive MATCH,
\ so a bad tag is unrepresentable and no throw arm exists.
: AKIND-KEY ( akind -- ptr u8 n )
   MATCH akind
      kernel       OF s" kernel"       ENDOF
      copy         OF s" copy"         ENDOF
      memset       OF s" memset"       ENDOF
      event-record OF s" event-record" ENDOF
      event-wait   OF s" event-wait"   ENDOF
   ;MATCH ;

private

64  constant ADAG-NCAP        \ max DAG nodes
16  constant ADAG-SCAP        \ max streams
32  constant ADAG-ECAP        \ max events
256 constant ADAG-DCAP        \ max dependency edges

\ node table: one column per field; the kind / stream / event / kernel columns
\ are generative LAYOUT-BUFFER columns, so a raw n or a foreign family can
\ never enter or leave one of those cells.
ADAG-NCAP LAYOUT-BUFFER AN-KIND-AT akind              \ node-kind column ( n -- ptr akind )
ADAG-NCAP LAYOUT-BUFFER AN-STREAM-AT ADAG:stream-id   \ owning-stream column
ADAG-NCAP LAYOUT-BUFFER AN-EVENT-AT ADAG:event-id     \ event payload (record / wait)
ADAG-NCAP LAYOUT-BUFFER AN-KIR-AT CAD-KIND:node-id    \ kernel payload: the model-IR node
create AN-A ADAG-NCAP cells allot                     \ payload scalar (copy dst / memset dst)
create AN-B ADAG-NCAP cells allot                     \ payload scalar (copy src / memset value)
create AN-C ADAG-NCAP cells allot                     \ payload scalar (transfer byte count)
variable ADAG-N

\ streams: creation count + per-stream program-order tail (node raw + 1; 0 = none)
create AS-TAIL ADAG-SCAP cells allot
variable ADAG-S-N

\ events: latest record node (node raw + 1; 0 = unrecorded) + destroyed flag
create AEV-REC  ADAG-ECAP cells allot
create AEV-DEAD ADAG-ECAP cells allot
variable ADAG-E-N

\ dependency edges: from / to raw node index pairs
create AE-FROM ADAG-DCAP cells allot
create AE-TO   ADAG-DCAP cells allot
variable ADAG-D-N

\ seal flag + the replay order fixed at seal (+ Kahn scratch; -1 = emitted)
variable ADAG-SEAL-ON
create AO-ORDER ADAG-NCAP cells allot
create AO-INDEG ADAG-NCAP cells allot

\ ---- nominal identity boundaries (R3 owner-module rule) ----------------------
\ These private mints are the only raw representation authority; every public
\ path revalidates the raw index against the live table before use. The three
\ mints stay trust rows: package ADAG declares the families (above) but they are
\ minted here inside package MAKI, so CAST: refuses the introduction with 7135
\ E-CAST-OWNER. The three projections out are checked casts.
TRUSTED: RAW>ANODE ( n -- ADAG:node-id ) ;
CAST: ANODE>RAW ( ADAG:node-id -- n )
TRUSTED: RAW>ASTREAM ( n -- ADAG:stream-id ) ;
CAST: ASTREAM>RAW ( ADAG:stream-id -- n )
TRUSTED: RAW>AEVENT ( n -- ADAG:event-id ) ;
CAST: AEVENT>RAW ( ADAG:event-id -- n )

: ANODE-CK ( ADAG:node-id -- n )
   ANODE>RAW dup 0 < over ADAG-N @ >= or if E-ADAG-IDX throw then ;

: ASTREAM-CK ( ADAG:stream-id -- n )
   ASTREAM>RAW dup 0 < over ADAG-S-N @ >= or if E-ADAG-IDX throw then ;

: AEVENT-CK ( ADAG:event-id -- n )
   AEVENT>RAW dup 0 < over ADAG-E-N @ >= or if E-ADAG-IDX throw then ;

: AEV-LIVE-CK ( ADAG:event-id -- n )    \ validated raw of a live (undestroyed) event
   AEVENT-CK {: eraw:n :}
   eraw cells AEV-DEAD + @ if E-ADAG-EUSE throw then
   eraw ;

: ADAG-MUT-CK ( -- )                    \ graph mutation is legal only before seal
   ADAG-SEAL-ON @ if E-ADAG-SEALED throw then ;

: ADAG-RO-CK ( -- )                     \ replay order exists only after seal
   ADAG-SEAL-ON @ 0= if E-ADAG-UNSEALED throw then ;

\ ---- edge pool + per-stream program-order threading --------------------------
: AE+RAW ( n n -- ) {: fr:n tr:n :}     \ from-raw to-raw
   ADAG-D-N @ ADAG-DCAP >= if E-ADAG-CAP throw then
   fr ADAG-D-N @ cells AE-FROM + !
   tr ADAG-D-N @ cells AE-TO   + !
   ADAG-D-N @ 1+ ADAG-D-N ! ;

: AS-TAIL-AT ( n -- ptr a )  cells AS-TAIL + ;

: AS-THREAD ( n n -- ) {: sraw:n raw:n :}   \ stream-raw node-raw: program-order edge + new tail
   sraw AS-TAIL-AT @ dup 0 = if drop else 1- raw AE+RAW then
   raw 1+ sraw AS-TAIL-AT ! ;

\ ---- node append core --------------------------------------------------------
\ the akind rides the stack under the stream (a layout family stores from the
\ stack into its typed column); the stream local threads program order.
: ANODE+ ( akind ADAG:stream-id -- ADAG:node-id )
   ADAG-MUT-CK
   {: st:ADAG:stream-id :}
   st ASTREAM-CK {: sraw:n :}
   ADAG-N @ ADAG-NCAP >= if E-ADAG-CAP throw then
   ADAG-N @ {: raw:n :}
   raw AN-KIND-AT !                     \ the akind (still on stack) -> kind column
   st raw AN-STREAM-AT !
   sraw raw AS-THREAD
   raw 1+ ADAG-N !
   raw RAW>ANODE ;

public

\ ---- lifecycle ---------------------------------------------------------------
: ADAG-RESET ( -- )
   0 ADAG-N !  0 ADAG-S-N !  0 ADAG-E-N !  0 ADAG-D-N !  0 ADAG-SEAL-ON ! ;

: ADAG-SEALED? ( -- bool )  ADAG-SEAL-ON @ 0= 0= ;

: ADAG-N@ ( -- n )        ADAG-N @ ;
: ADAG-STREAMS@ ( -- n )  ADAG-S-N @ ;
: ADAG-EVENTS@ ( -- n )   ADAG-E-N @ ;
: ADAG-EDGES@ ( -- n )    ADAG-D-N @ ;

: ADAG-STATS@ ( -- ADAG:stats )
   ADAG-N @ ADAG-S-N @ ADAG-E-N @ ADAG-D-N @ ADAG-STATS:MAKE ;

\ ---- validated raw-index refinements (loop counters enter here) --------------
: ADAG-NODE-ID ( n -- ADAG:node-id )
   dup 0 < over ADAG-N @ >= or if E-ADAG-IDX throw then
   RAW>ANODE ;

: ADAG-STREAM-ID ( n -- ADAG:stream-id )
   dup 0 < over ADAG-S-N @ >= or if E-ADAG-IDX throw then
   RAW>ASTREAM ;

: ADAG-EVENT-ID ( n -- ADAG:event-id )
   dup 0 < over ADAG-E-N @ >= or if E-ADAG-IDX throw then
   RAW>AEVENT ;

: ADAG-NODE= ( ADAG:node-id ADAG:node-id -- bool )
   {: a:ADAG:node-id b:ADAG:node-id :}
   a ANODE>RAW b ANODE>RAW = ;

: ADAG-STREAM= ( ADAG:stream-id ADAG:stream-id -- bool )
   {: a:ADAG:stream-id b:ADAG:stream-id :}
   a ASTREAM>RAW b ASTREAM>RAW = ;

: ADAG-EVENT= ( ADAG:event-id ADAG:event-id -- bool )
   {: a:ADAG:event-id b:ADAG:event-id :}
   a AEVENT>RAW b AEVENT>RAW = ;

\ ---- stream / event construction ---------------------------------------------
: ADAG-STREAM+ ( -- ADAG:stream-id )
   ADAG-MUT-CK
   ADAG-S-N @ ADAG-SCAP >= if E-ADAG-CAP throw then
   ADAG-S-N @ {: raw:n :}
   0 raw AS-TAIL-AT !
   raw 1+ ADAG-S-N !
   raw RAW>ASTREAM ;

: ADAG-EVENT+ ( -- ADAG:event-id )
   ADAG-MUT-CK
   ADAG-E-N @ ADAG-ECAP >= if E-ADAG-CAP throw then
   ADAG-E-N @ {: raw:n :}
   0 raw cells AEV-REC + !
   0 raw cells AEV-DEAD + !
   raw 1+ ADAG-E-N !
   raw RAW>AEVENT ;

: ADAG-EVENT-LIVE? ( ADAG:event-id -- bool )
   AEVENT-CK cells AEV-DEAD + @ 0= ;

\ handle-lifecycle bookkeeping, not graph mutation: legal before or after seal;
\ a second destroy of the same event fails closed.
: ADAG-EVENT-DESTROY ( ADAG:event-id -- )
   AEVENT-CK {: eraw:n :}
   eraw cells AEV-DEAD + @ if E-ADAG-EDESTROY throw then
   1 eraw cells AEV-DEAD + ! ;

\ ---- node construction (each returns the new node's nominal id) ---------------
: ADAG-KERNEL+ ( CAD-KIND:node-id ADAG:stream-id -- ADAG:node-id )
   {: kir:CAD-KIND:node-id st:ADAG:stream-id :}
   MAKI-AKIND:KERNEL st ANODE+ {: nd:ADAG:node-id :}
   kir nd ANODE>RAW AN-KIR-AT !
   nd ;

: ADAG-COPY+ ( n n n ADAG:stream-id -- ADAG:node-id )   \ dst src bytes stream
   {: dst:n src:n nbytes:n st:ADAG:stream-id :}
   MAKI-AKIND:COPY st ANODE+ {: nd:ADAG:node-id :}
   nd ANODE>RAW {: raw:n :}
   dst raw cells AN-A + !
   src raw cells AN-B + !
   nbytes raw cells AN-C + !
   nd ;

: ADAG-MEMSET+ ( n n n ADAG:stream-id -- ADAG:node-id )   \ dst value bytes stream
   {: dst:n val:n nbytes:n st:ADAG:stream-id :}
   MAKI-AKIND:MEMSET st ANODE+ {: nd:ADAG:node-id :}
   nd ANODE>RAW {: raw:n :}
   dst raw cells AN-A + !
   val raw cells AN-B + !
   nbytes raw cells AN-C + !
   nd ;

: ADAG-RECORD+ ( ADAG:event-id ADAG:stream-id -- ADAG:node-id )
   {: ev:ADAG:event-id st:ADAG:stream-id :}
   ev AEV-LIVE-CK {: eraw:n :}
   MAKI-AKIND:EVENT-RECORD st ANODE+ {: nd:ADAG:node-id :}
   ev nd ANODE>RAW AN-EVENT-AT !
   nd ANODE>RAW 1+ eraw cells AEV-REC + !
   nd ;

\ use-before-ready fails closed: the event must already carry a record node;
\ the wait depends on the LATEST record (the record->wait edge).
: ADAG-WAIT+ ( ADAG:event-id ADAG:stream-id -- ADAG:node-id )
   {: ev:ADAG:event-id st:ADAG:stream-id :}
   ev AEV-LIVE-CK {: eraw:n :}
   eraw cells AEV-REC + @ dup 0 = if E-ADAG-UNREADY throw then
   1- {: recraw:n :}
   MAKI-AKIND:EVENT-WAIT st ANODE+ {: nd:ADAG:node-id :}
   ev nd ANODE>RAW AN-EVENT-AT !
   recraw nd ANODE>RAW AE+RAW
   nd ;

\ an explicit producer -> consumer data dependency. Cross-stream order must go
\ through an event record/wait pair, so a cross-stream edge fails closed.
: ADAG-DEP+ ( ADAG:node-id ADAG:node-id -- )
   ADAG-MUT-CK
   {: prod:ADAG:node-id cons:ADAG:node-id :}
   prod ANODE-CK {: praw:n :}
   cons ANODE-CK {: craw:n :}
   praw AN-STREAM-AT @ ASTREAM>RAW  craw AN-STREAM-AT @ ASTREAM>RAW
   <> if E-ADAG-XSTREAM throw then
   praw craw AE+RAW ;

\ ---- node accessors (kind-gated payloads fail closed on the wrong kind) -------
: ADAG-KIND@ ( ADAG:node-id -- akind )
   ANODE-CK AN-KIND-AT @ ;

: ADAG-NODE-STREAM@ ( ADAG:node-id -- ADAG:stream-id )
   ANODE-CK AN-STREAM-AT @ ;

private

: AK-KERNEL? ( akind -- bool )
   MATCH akind
      kernel       OF 0 0= ENDOF
      copy         OF 1 0= ENDOF
      memset       OF 1 0= ENDOF
      event-record OF 1 0= ENDOF
      event-wait   OF 1 0= ENDOF
   ;MATCH ;

: AK-EVENT? ( akind -- bool )           \ carries an event payload?
   MATCH akind
      event-record OF 0 0= ENDOF
      event-wait   OF 0 0= ENDOF
      kernel       OF 1 0= ENDOF
      copy         OF 1 0= ENDOF
      memset       OF 1 0= ENDOF
   ;MATCH ;

: AK-XFER? ( akind -- bool )            \ carries transfer scalars (dst/bytes)?
   MATCH akind
      copy         OF 0 0= ENDOF
      memset       OF 0 0= ENDOF
      kernel       OF 1 0= ENDOF
      event-record OF 1 0= ENDOF
      event-wait   OF 1 0= ENDOF
   ;MATCH ;

: AK-COPY? ( akind -- bool )
   MATCH akind
      copy         OF 0 0= ENDOF
      kernel       OF 1 0= ENDOF
      memset       OF 1 0= ENDOF
      event-record OF 1 0= ENDOF
      event-wait   OF 1 0= ENDOF
   ;MATCH ;

: AK-MEMSET? ( akind -- bool )
   MATCH akind
      memset       OF 0 0= ENDOF
      kernel       OF 1 0= ENDOF
      copy         OF 1 0= ENDOF
      event-record OF 1 0= ENDOF
      event-wait   OF 1 0= ENDOF
   ;MATCH ;

public

: ADAG-KERNEL-IR@ ( ADAG:node-id -- CAD-KIND:node-id ) {: nd:ADAG:node-id :}
   nd ADAG-KIND@ AK-KERNEL? 0= if E-ADAG-KIND throw then
   nd ANODE-CK AN-KIR-AT @ ;

: ADAG-EVENT@ ( ADAG:node-id -- ADAG:event-id ) {: nd:ADAG:node-id :}
   nd ADAG-KIND@ AK-EVENT? 0= if E-ADAG-KIND throw then
   nd ANODE-CK AN-EVENT-AT @ ;

: ADAG-DST@ ( ADAG:node-id -- n ) {: nd:ADAG:node-id :}
   nd ADAG-KIND@ AK-XFER? 0= if E-ADAG-KIND throw then
   nd ANODE-CK cells AN-A + @ ;

: ADAG-SRC@ ( ADAG:node-id -- n ) {: nd:ADAG:node-id :}
   nd ADAG-KIND@ AK-COPY? 0= if E-ADAG-KIND throw then
   nd ANODE-CK cells AN-B + @ ;

: ADAG-VAL@ ( ADAG:node-id -- n ) {: nd:ADAG:node-id :}
   nd ADAG-KIND@ AK-MEMSET? 0= if E-ADAG-KIND throw then
   nd ANODE-CK cells AN-B + @ ;

: ADAG-BYTES@ ( ADAG:node-id -- n ) {: nd:ADAG:node-id :}
   nd ADAG-KIND@ AK-XFER? 0= if E-ADAG-KIND throw then
   nd ANODE-CK cells AN-C + @ ;

private

\ ---- deterministic topological order (Kahn, smallest-ready-index tie break) ---
: AO-INDEG-INIT ( -- )
   ADAG-N @ 0 ?do  0 i cells AO-INDEG + !  loop
   ADAG-D-N @ 0 ?do
      i cells AE-TO + @ {: tr:n :}
      tr cells AO-INDEG + @ 1+ tr cells AO-INDEG + !
   loop ;

: AO-PICK ( -- n )                      \ smallest unemitted ready node; -1 when none
   -1 ADAG-N @ 0 ?do
      dup 0 < if
         i cells AO-INDEG + @ 0= if drop i then
      then
   loop ;

: AO-RELAX ( n -- ) {: raw:n :}         \ emitted node: release its successors
   ADAG-D-N @ 0 ?do
      i cells AE-FROM + @ raw = if
         i cells AE-TO + @ {: tr:n :}
         tr cells AO-INDEG + @ 1- tr cells AO-INDEG + !
      then
   loop ;

: AO-TOPO ( -- )                        \ fix AO-ORDER; a leftover node is a cycle
   AO-INDEG-INIT
   ADAG-N @ 0 ?do
      AO-PICK dup 0 < if E-ADAG-CYCLE throw then
      {: pick:n :}
      pick i cells AO-ORDER + !
      -1 pick cells AO-INDEG + !
      pick AO-RELAX
   loop ;

public

\ ---- seal: freeze the graph and fix the canonical replay order ----------------
: ADAG-SEAL ( -- )
   ADAG-MUT-CK
   AO-TOPO
   1 ADAG-SEAL-ON ! ;

: ADAG-ORDER@ ( n -- ADAG:node-id ) {: k:n :}
   ADAG-RO-CK
   k 0 < k ADAG-N @ >= or if E-ADAG-IDX throw then
   k cells AO-ORDER + @ RAW>ANODE ;

private

\ ---- line-oriented render buffer (report.f machine-render discipline) ---------
$2000 constant AR-CAP
create AR-BUF AR-CAP allot
variable AR-U

: AR-RESET ( -- )  0 AR-U ! ;
: AR-C ( n -- ) {: c:n :}
   AR-U @ AR-CAP >= if E-ADAG-CAP throw then
   c AR-BUF AR-U @ + c!  AR-U @ 1+ AR-U ! ;
: AR+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while  dup a + c@ AR-C  1+  repeat drop ;
: AR-INT ( n -- )  SB-RESET FMT:SB-INT SB$ AR+ ;
: AR-NL ( -- )  $0A AR-C ;
: AR$ ( -- ptr u8 n )  AR-BUF AR-U @ ;

: AR-NODE ( n -- ) {: raw:n :}          \ "node<i>: <kind> s<stream>"
   s" node" AR+ raw AR-INT s" : " AR+
   raw ADAG-NODE-ID ADAG-KIND@ AKIND-KEY AR+
   s"  s" AR+ raw ADAG-NODE-ID ADAG-NODE-STREAM@ ASTREAM>RAW AR-INT
   AR-NL ;

: AR-EDGE ( n -- ) {: k:n :}            \ "edge<k>: n<from> n<to>"
   s" edge" AR+ k AR-INT s" : n" AR+
   k cells AE-FROM + @ AR-INT s"  n" AR+
   k cells AE-TO   + @ AR-INT
   AR-NL ;

public

\ machine render of the sealed DAG including its fixed replay order - the
\ canonical byte form the determinism tests compare byte-identically.
: ADAG-RENDER ( -- ptr u8 n )
   ADAG-RO-CK
   AR-RESET
   s" adag.nodes: "   AR+ ADAG-N @   AR-INT AR-NL
   s" adag.streams: " AR+ ADAG-S-N @ AR-INT AR-NL
   s" adag.events: "  AR+ ADAG-E-N @ AR-INT AR-NL
   s" adag.edges: "   AR+ ADAG-D-N @ AR-INT AR-NL
   ADAG-N @ 0 ?do  i AR-NODE  loop
   ADAG-D-N @ 0 ?do  i AR-EDGE  loop
   s" replay:" AR+
   ADAG-N @ 0 ?do  $20 AR-C s" n" AR+ i cells AO-ORDER + @ AR-INT  loop
   AR-NL
   AR$ ;

;package
