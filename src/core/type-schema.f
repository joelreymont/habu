\ type-schema.f — persistent schema node arena for the checker's type families
\ (TFAM): families, variants and product fields store schema roots, never live
\ checker variables, and the checker instantiates them per use. Node fields hold
\ integers only (con codes, family ids, root indices), so a grow is a cell copy
\ and snapshot persist bakes stores with no rebase. Loaded unchecked right after
\ checker.f, as package SCHEMA-REG: a name used outside this file is public.

\ --- schema node kinds (node tag values). Node id 0 is the reserved nil node.
\ SCH-NIL is the reserved sentinel and SCH-KIND-MAX the creatable-tag ceiling
\ SCHEMA-KIND? tests against; neither is a caller's value, so both stay private.
package SCHEMA-REG

0 constant SCH-NIL

public

1 constant SCH-PARAM      \ A = parameter index (>= 0)
2 constant SCH-CON        \ A = concrete con code
3 constant SCH-APP        \ A = family-id, B = arg-root start, C = arg count
4 constant SCH-QUOT       \ A = hasr flag, B = side-row-root start, C = SCH-QUOT-ROWS
5 constant SCH-PTR        \ A = pointee schema node
6 constant SCH-ROW        \ A = element-root start, B = element count (one quotation effect side)
7 constant SCH-XT         \ no fields

private

7 constant SCH-KIND-MAX   \ highest valid creatable tag

\ A quotation payload has four effect SIDES stored as four schema roots contiguous
\ in the schema-root pool: din-row, dout-row, rin-row, rout-row (in that order). B
\ indexes the first. Each root is a SCH-ROW node whose own element roots list that
\ side's ordered type nodes (possibly empty), so a side can be multi-type or empty.
\ The hasr flag (A) records whether the return-stack sides carry a non-neutral
\ effect (an explicit `| rin -- rout` clause).
public

4 constant SCH-QUOT-ROWS

\ --- node record layout (interleaved cell arena, one grow buffer).
private

0 cells constant SCH.TAG-OFF
1 cells constant SCH.A-OFF
2 cells constant SCH.B-OFF
3 cells constant SCH.C-OFF

public

4 cells constant SCH-REC
private

: SCH.TAG ( ^a -- ^a ) SCH.TAG-OFF + ;
: SCH.A ( ^a -- ^a ) SCH.A-OFF + ;
: SCH.B ( ^a -- ^a ) SCH.B-OFF + ;
: SCH.C ( ^a -- ^a ) SCH.C-OFF + ;

16 constant SCH-CAP-INIT        \ holds the built-ins' schemas (TF-CAP-INIT)
16 constant SCH-ROOT-INIT       \ cells; holds the built-ins' schema roots

public

variable SCH-CAP-V   SCH-CAP-INIT SCH-CAP-V !

private

: SCH-CAP ( -- n ) SCH-CAP-V @ ;
create SCH-A-BOOT   SCH-CAP-INIT SCH-REC * allot
PERSISTED-PTR-VARIABLE SCH-A-P    SCH-A-BOOT SCH-A-P !

public

: SCH-BASE ( -- ^n ) SCH-A-P @ ;

variable SCH-ROOT-CAP-V   SCH-ROOT-INIT SCH-ROOT-CAP-V !

private

: SCH-ROOT-CAP ( -- n ) SCH-ROOT-CAP-V @ ;
create SCH-ROOT-BOOT   SCH-ROOT-INIT cells allot
PERSISTED-PTR-VARIABLE SCH-ROOT-P   SCH-ROOT-BOOT SCH-ROOT-P !

public

: SCH-ROOT-BASE ( -- ^n ) SCH-ROOT-P @ ;

variable SCH-N   \ next node id; 1 leaves node 0 as the nil sentinel
variable SCH-ROOT-N   \ next schema-root index

private

\ Retiring a node or root scrubs it back to canonical zero, as PF-SCRUB
\ (src/core/type-family.f) does for field rows: a zeroed row is the canonical
\ absent row, so retired rows leave no bytes in reflection or snapshot identity.
: SCH-SCRUB ( n n -- ) {: lo:n hi:n :}      \ zero the nodes [lo,hi)
   SCH-BASE  lo SCH-REC CELL / *  hi SCH-REC CELL / *  ARENA-CELLS-ZERO ;
: SCH-ROOT-SCRUB ( n n -- ) {: lo:n hi:n :} \ zero the roots [lo,hi)
   SCH-ROOT-BASE lo hi ARENA-CELLS-ZERO ;

EXPORT CHECKER-REG:GROW

public

: SCHEMA-RESET ( -- )           \ base state; node 0, the nil sentinel, stays
   1 SCH-N @ SCH-SCRUB
   0 SCH-ROOT-N @ SCH-ROOT-SCRUB
   1 SCH-N !
   0 SCH-ROOT-N ! ;
SCHEMA-RESET

: SCH-GROW ( n -- ) {: need:n :}
   need SCH-CAP-V @ 2 * max {: nc:n :}
   SCH-A-P  SCH-CAP-V @ SCH-REC *  nc SCH-REC *  GROW
   nc SCH-CAP-V ! ;

private

: SCH-ENSURE ( -- )             \ room for the next node id (SCH-N)
   SCH-N @ SCH-CAP-V @ < IF exit THEN
   SCH-N @ 1 + SCH-GROW ;

public

: SCH-ROOT-GROW ( n -- ) {: need:n :}
   need SCH-ROOT-CAP-V @ 2 * max {: nc:n :}
   SCH-ROOT-P  SCH-ROOT-CAP-V @ cells  nc cells  GROW
   nc SCH-ROOT-CAP-V ! ;

private

: SCH-ROOT-ENSURE ( -- )        \ room for the next root index (SCH-ROOT-N)
   SCH-ROOT-N @ SCH-ROOT-CAP-V @ < IF exit THEN
   SCH-ROOT-N @ 1 + SCH-ROOT-GROW ;

\ --- the transient region: the nodes and roots made for a definition being
\ built, apart from the persistent store, so no snapshot or build writes one.
\ Its node ids and root indices count from TRANSIENT-BASE, above any
\ persistent count; transient mode sends SCHEMA-NEW and SCHEMA-ROOT+ there.
\ Its cells are scratch, so a written image starts with an empty region.
$10000000000 constant TRANSIENT-BASE
PTR-VARIABLE SCH-T-P   SCRATCH-VARIABLE SCH-T-CAP   SCRATCH-VARIABLE SCH-T-N  \ nodes, in rows
PTR-VARIABLE SCH-T-ROOT-P   SCRATCH-VARIABLE SCH-T-ROOT-CAP   SCRATCH-VARIABLE SCH-T-ROOT-N
SCRATCH-VARIABLE SCH-T-PARAM-N   \ parameter indices handed out
SCRATCH-VARIABLE SCH-MODE        \ 0 persistent, 1 transient

: TRANSIENT-MODE? ( -- bool ) SCH-MODE @ 0 <> ;

\ SCHEMA-REG:WORD was given V, which is WHAT; FIX says what to pass instead.
: SCHEMA-BAD ( n ^u8 n ^u8 n ^u8 n -- )
   {: v:n w:^ wu:n what:^ whatu:n fix:^ fixu:n :}
   s" E-SCHEMA-BAD" REJECTION:OPEN
   s" `SCHEMA-REG:" REJECTION:EMIT-STR  w wu REJECTION:EMIT-STR
   s" ` was given " REJECTION:EMIT-STR  what whatu REJECTION:EMIT-STR
   s" value" REJECTION:NOTE  v REJECTION:EMIT-N
   REJECTION:HELP  fix fixu REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ A persistent node or root never names a transient one.
: KEEP-CK ( n ^u8 n -- ) {: id:n w:^ wu:n :}
   TRANSIENT-MODE? IF EXIT THEN
   id TRANSIENT-BASE >= IF
      id w wu s" a transient node outside transient mode"
      s" pass a node made outside transient mode" SCHEMA-BAD
   THEN ;

: T-NODE? ( n -- bool ) {: id:n :}
   id TRANSIENT-BASE - {: i:n :}
   i 0 >= i SCH-T-N @ < and ;

: NODE-LIVE? ( n -- bool ) {: id:n :}
   id T-NODE? IF RES-TRUE EXIT THEN
   id 0 > id SCH-N @ < and ;

EXPORT CHECK:ARENA-ROWS-ENSURE
EXPORT CHECK:ARENA-ROWS-RELEASE
EXPORT CHECK:WRITE-WINDOW-CK
: T-NEW-ID ( -- n )
   SCH-T-P SCH-T-CAP SCH-T-N @ 1 + SCH-REC CELL / ARENA-ROWS-ENSURE
   SCH-T-N @ {: i:n :}
   i 1 + SCH-T-N !
   i TRANSIENT-BASE + ;

: SCH-NEW-ID ( -- n )
   TRANSIENT-MODE? IF T-NEW-ID EXIT THEN
   SCH-ENSURE
   SCH-N @ {: id:n :}
   id 1 + SCH-N !
   id ;

: T-ROOT+ ( n -- n ) {: node:n :}
   node NODE-LIVE? 0= IF s" tfam: bad schema root" 76 die THEN
   SCH-T-ROOT-P SCH-T-ROOT-CAP SCH-T-ROOT-N @ 1 + 1 ARENA-ROWS-ENSURE
   SCH-T-ROOT-N @ {: i:n :}
   node i cells SCH-T-ROOT-P @ + !
   i 1 + SCH-T-ROOT-N !
   i TRANSIENT-BASE + ;

: T-ROOT@ ( n -- n ) {: idx:n :}
   idx TRANSIENT-BASE - {: i:n :}
   i SCH-T-ROOT-N @ >= IF s" tfam: bad schema root index" 76 die THEN
   i cells SCH-T-ROOT-P @ + @ ;

: SCH-REC@ ( n -- ^n ) {: id:n :}     \ address of node `id` (nil/oob is a bug)
   id T-NODE? IF id TRANSIENT-BASE - SCH-REC * SCH-T-P @ + EXIT THEN
   id 0 <= IF s" tfam: bad schema node" 76 die THEN
   id SCH-N @ >= IF s" tfam: bad schema node" 76 die THEN
   id SCH-REC * SCH-BASE + ;

: SCHEMA-KIND? ( n -- bool ) {: tag:n :}   \ tag is a creatable node kind
   tag SCH-PARAM >= tag SCH-KIND-MAX <= and ;

public

: SCHEMA-NEW ( n n n n -- n ) {: tag:n a:n b:n c:n :}
   WRITE-WINDOW-CK                        \ src/core/checker.f WRITE-WINDOW
   tag SCHEMA-KIND? 0= IF
      tag s" SCHEMA-NEW" s" a tag that is no schema node kind"
      s" pass a kind from `SCHEMA-REG:SCH-PARAM` to `SCHEMA-REG:SCH-ROW`"
      SCHEMA-BAD
   THEN
   SCH-NEW-ID {: id:n :}
   id SCH-REC@ {: r:^ :}
   tag r SCH.TAG !   a r SCH.A !   b r SCH.B !   c r SCH.C !
   id ;

: SCHEMA-PARAM ( n -- n )
   dup 0 < IF
      s" SCHEMA-PARAM" s" a negative parameter index"
      s" pass a parameter index of 0 or more" SCHEMA-BAD
   THEN
   SCH-PARAM swap 0 0 SCHEMA-NEW ;
: SCHEMA-CON ( n -- n )
   SCH-CON swap 0 0 SCHEMA-NEW ;
: SCHEMA-APP ( n n n -- n )
   {: fam:n start:n count:n :}
   count 0 < IF
      count s" SCHEMA-APP" s" a negative argument count"
      s" pass an argument count of 0 or more" SCHEMA-BAD
   THEN
   start s" SCHEMA-APP" KEEP-CK
   \ A nullary application (`fam<>`) has an empty argument range: start 0,
   \ as for SCHEMA-ROW.
   SCH-APP fam count 0= IF 0 ELSE start THEN count SCHEMA-NEW ;

: SCHEMA-TAG@ ( n -- n ) SCH-REC@ SCH.TAG @ ;
: SCHEMA-A@ ( n -- n ) SCH-REC@ SCH.A @ ;
: SCHEMA-B@ ( n -- n ) SCH-REC@ SCH.B @ ;
: SCHEMA-C@ ( n -- n ) SCH-REC@ SCH.C @ ;

: SCHEMA-PARAM? ( n -- bool ) SCHEMA-TAG@ SCH-PARAM = ;
: SCHEMA-CON? ( n -- bool ) SCHEMA-TAG@ SCH-CON = ;
: SCHEMA-APP? ( n -- bool ) SCHEMA-TAG@ SCH-APP = ;

\ --- schema-root pool: a flat, growable list of node ids that SUMV variants and
\ product fields reference as contiguous [start,start+count) ranges.
: SCHEMA-ROOT+ ( n -- n ) {: node:n :}
   WRITE-WINDOW-CK
   node s" SCHEMA-ROOT+" KEEP-CK
   TRANSIENT-MODE? IF node T-ROOT+ EXIT THEN
   node 0 <= IF s" tfam: bad schema root" 76 die THEN
   node SCH-N @ >= IF s" tfam: bad schema root" 76 die THEN
   SCH-ROOT-ENSURE
   SCH-ROOT-N @ {: idx:n :}
   node idx cells SCH-ROOT-BASE + !
   idx 1 + SCH-ROOT-N !
   idx ;
: SCHEMA-ROOT@ ( n -- n ) {: idx:n :}
   idx TRANSIENT-BASE >= IF idx T-ROOT@ EXIT THEN
   idx 0 < IF s" tfam: bad schema root index" 76 die THEN
   idx SCH-ROOT-N @ >= IF s" tfam: bad schema root index" 76 die THEN
   idx cells SCH-ROOT-BASE + @ ;
\ The index the next SCHEMA-ROOT+ answers, in the mode's region.
: SCHEMA-ROOT-N@ ( -- n )
   TRANSIENT-MODE? IF SCH-T-ROOT-N @ TRANSIENT-BASE + EXIT THEN
   SCH-ROOT-N @ ;

\ Transient mode, for the nodes a definition being built is given.
: TRANSIENT-ON ( -- ) 1 SCH-MODE ! ;
: TRANSIENT-OFF ( -- ) 0 SCH-MODE ! ;

\ A parameter node no other transient node holds, so each family a definer
\ applies has its own parameters.
: FRESH-PARAM ( -- n )
   SCH-T-PARAM-N @ {: i:n :}
   i 1 + SCH-T-PARAM-N !
   i SCHEMA-PARAM ;

\ A load takes the mark as it starts and releases back to it as it ends, so
\ the nodes, roots and parameters a load made last only until it ends, while
\ those its loader made stay live through it.
: TRANSIENT-MARK ( -- n n n ) SCH-T-N @  SCH-T-ROOT-N @  SCH-T-PARAM-N @ ;

\ An empty region gives its rows back.
: TRANSIENT-RELEASE ( n n n -- ) {: nodes:n roots:n params:n :}
   nodes SCH-T-N !  roots SCH-T-ROOT-N !  params SCH-T-PARAM-N !
   nodes 0= IF SCH-T-P SCH-T-CAP SCH-REC CELL / ARENA-ROWS-RELEASE THEN
   roots 0= IF SCH-T-ROOT-P SCH-T-ROOT-CAP 1 ARENA-ROWS-RELEASE THEN ;

\ --- SCH-ROW effect-side node: one ordered row of schema element type nodes,
\ referenced by [start, start+count) contiguous schema roots. An empty side has
\ count 0. SCH-ROW nodes never stand alone as a payload element — only SCH-QUOT
\ reaches them (as its four side roots), and PF-NODE-KIND? walks into them there.
: SCHEMA-NODE-OK? ( n -- bool ) NODE-LIVE? ;
\ An empty row (count 0) is start 0, never the root store's high-water: it
\ names no root, so its start is the same whatever an earlier file left.
: SCHEMA-ROW ( n n -- n ) {: start:n count:n :}   \ row over [start,start+count) element roots
   count 0 < IF
      count s" SCHEMA-ROW" s" a negative element count"
      s" pass an element count of 0 or more" SCHEMA-BAD
   THEN
   start s" SCHEMA-ROW" KEEP-CK
   SCH-ROW count 0= IF 0 ELSE start THEN count 0 SCHEMA-NEW ;
: SCHEMA-ROW? ( n -- bool ) SCHEMA-TAG@ SCH-ROW = ;
: SCHEMA-ROW-START@ ( n -- n ) SCHEMA-A@ ;
: SCHEMA-ROW-COUNT@ ( n -- n ) SCHEMA-B@ ;
: SCHEMA-ROW-ELEM@ ( n n -- n ) {: node:n i:n :}   \ i-th element type node of the row
   i 0 < i node SCHEMA-ROW-COUNT@ >= or IF s" tfam: bad quot row elem index" 76 die THEN
   node SCHEMA-ROW-START@ i + SCHEMA-ROOT@ ;
: SCHEMA-ROW-OK? ( n -- bool )                     \ live SCH-ROW node (no die on nil/oob)
   dup SCHEMA-NODE-OK? IF SCHEMA-ROW? ELSE drop RES-FALSE THEN ;

\ --- SCH-QUOT quotation payload node: the checker's quotation effect sides (din,
\ dout, rin, rout) as four SCH-ROW roots plus a hasr flag, so a schema can carry a
\ quotation-typed argument. A side that is not a live SCH-ROW node is rejected.
private

: QUOT-SIDE-CK ( n -- ) {: side:n :}
   side s" SCHEMA-QUOT" KEEP-CK
   side SCHEMA-ROW-OK? IF EXIT THEN
   side s" SCHEMA-QUOT" s" a side that is not a live row node"
   s" pass four sides `SCHEMA-REG:SCHEMA-ROW` made" SCHEMA-BAD ;

: SCH-FLAG ( n -- n )                       \ canonical 0/-1 flag as a plain cell (any nonzero -> -1)
   0= IF 0 ELSE -1 THEN ;

public

: SCHEMA-QUOT ( n n n n n -- n )
   {: din:n dout:n rin:n rout:n hasr:n :}
   din QUOT-SIDE-CK  dout QUOT-SIDE-CK  rin QUOT-SIDE-CK  rout QUOT-SIDE-CK
   din SCHEMA-ROOT+ {: start:n :}          \ first side root; dout/rin/rout follow contiguously
   dout SCHEMA-ROOT+ drop
   rin SCHEMA-ROOT+ drop
   rout SCHEMA-ROOT+ drop
   SCH-QUOT hasr SCH-FLAG start SCH-QUOT-ROWS SCHEMA-NEW ;

: SCHEMA-QUOT? ( n -- bool ) SCHEMA-TAG@ SCH-QUOT = ;
: SCHEMA-QUOT-HASR@ ( n -- n ) SCHEMA-A@ ;
: SCHEMA-QUOT-ROW@ ( n n -- n ) {: node:n i:n :}    \ i-th effect side's SCH-ROW node
   i 0 < i SCH-QUOT-ROWS >= or IF s" tfam: bad quot side index" 76 die THEN
   node SCHEMA-B@ i + SCHEMA-ROOT@ ;
: SCHEMA-QUOT-DIN@ ( n -- n ) 0 SCHEMA-QUOT-ROW@ ;
: SCHEMA-QUOT-DOUT@ ( n -- n ) 1 SCHEMA-QUOT-ROW@ ;
: SCHEMA-QUOT-RIN@ ( n -- n ) 2 SCHEMA-QUOT-ROW@ ;
: SCHEMA-QUOT-ROUT@ ( n -- n ) 3 SCHEMA-QUOT-ROW@ ;

\ The effect with no inputs and no outputs.
: EMPTY-EFFECT ( -- n )
   0 0 SCHEMA-ROW {: row:n :}
   row row row row 0 SCHEMA-QUOT ;

\ Row with node after its elements.
private

: SIDE+ ( n n -- n ) {: row:n node:n :}
   row SCHEMA-ROW-COUNT@ {: k:n :}
   SCHEMA-ROOT-N@ {: start:n :}
   k 0 ?do row i SCHEMA-ROW-ELEM@ SCHEMA-ROOT+ drop loop
   node SCHEMA-ROOT+ drop
   start k 1 + SCHEMA-ROW ;

public

\ Effect EFF with NODE as its new top input.
: EFFECT-IN ( n n -- n ) {: eff:n node:n :}
   eff SCHEMA-QUOT-DIN@ node SIDE+  eff SCHEMA-QUOT-DOUT@
   eff SCHEMA-QUOT-RIN@  eff SCHEMA-QUOT-ROUT@
   eff SCHEMA-QUOT-HASR@ SCHEMA-QUOT ;

\ Effect EFF with NODE as its new top output.
: EFFECT-OUT ( n n -- n ) {: eff:n node:n :}
   eff SCHEMA-QUOT-DIN@  eff SCHEMA-QUOT-DOUT@ node SIDE+
   eff SCHEMA-QUOT-RIN@  eff SCHEMA-QUOT-ROUT@
   eff SCHEMA-QUOT-HASR@ SCHEMA-QUOT ;

\ --- SCH-PTR: a `^T` variant payload element. A malformed child (nil or out
\ of range) is rejected.
: SCHEMA-PTR ( n -- n )
   dup s" SCHEMA-PTR" KEEP-CK
   dup SCHEMA-NODE-OK? 0= IF
      s" SCHEMA-PTR" s" a node that is not a live schema node"
      s" pass a node a `SCHEMA-REG` word made" SCHEMA-BAD
   THEN
   SCH-PTR swap 0 0 SCHEMA-NEW ;
: SCHEMA-PTR? ( n -- bool ) SCHEMA-TAG@ SCH-PTR = ;

\ --- SCH-XT: an `xt`, the opaque token `'` pushes.
: SCHEMA-XT ( -- n ) SCH-XT 0 0 0 SCHEMA-NEW ;
: SCHEMA-XT? ( n -- bool ) SCHEMA-TAG@ SCH-XT = ;

\ Rollback frame stack: the schema half of the checker's transactional rollback,
\ pushed and popped in lockstep with checker.f's core frame through the
\ REG-EXT-RB-* hooks. Rejecting a scope retires its nodes; later nodes reuse the
\ ids.
private

0 cells constant SCHRB.N-OFF
1 cells constant SCHRB.ROOTN-OFF

public

2 cells constant SCH-RBF-REC
private

: SCHRB.N ( ^a -- ^a ) SCHRB.N-OFF + ;
: SCHRB.ROOTN ( ^a -- ^a ) SCHRB.ROOTN-OFF + ;


16 constant SCH-RBF-CAP-INIT
variable SCH-RBF-CAP-V   SCH-RBF-CAP-INIT SCH-RBF-CAP-V !
create SCH-RBF-BOOT   SCH-RBF-CAP-INIT SCH-RBF-REC * allot
PERSISTED-PTR-VARIABLE SCH-RBF-P    SCH-RBF-BOOT SCH-RBF-P !
: SCH-RBF-BASE ( -- ^n ) SCH-RBF-P @ ;

public

variable SCH-RBF-DEPTH   0 SCH-RBF-DEPTH !

private

: SCH-RBF-GROW ( -- )
   SCH-RBF-CAP-V @ 2 * {: nc:n :}
   SCH-RBF-P  SCH-RBF-CAP-V @ SCH-RBF-REC *  nc SCH-RBF-REC *  GROW
   nc SCH-RBF-CAP-V ! ;
: SCH-RBF-ENSURE ( -- )
   SCH-RBF-DEPTH @ SCH-RBF-CAP-V @ < IF exit THEN
   SCH-RBF-GROW ;
: SCH-RBF-CUR ( -- ^n ) SCH-RBF-DEPTH @ SCH-RBF-REC * SCH-RBF-BASE + ;

\ ---- rewind to a recorded pair ----------------------------------------------
\ Every place that lowers the schema counts comes through REWIND, which scrubs
\ the retired nodes and roots and moves no frame depth: SCHEMA-ROLLBACK-RESTORE
\ (the checker frame) and src/core/sumtype.f TDECL-RESTORE. SCHEMA-RESET runs
\ at load, before REWIND exists, and scrubs itself.

public

: REWIND ( n n -- ) {: n:n rootn:n :}
   n 0 < n SCH-N @ > or rootn 0 < or rootn SCH-ROOT-N @ > or IF
      s" checker: schema rewind outside the registry" 76 die
   THEN
   n SCH-N @ SCH-SCRUB
   rootn SCH-ROOT-N @ SCH-ROOT-SCRUB
   n SCH-N !
   rootn SCH-ROOT-N ! ;

: SCHEMA-ROLLBACK-SAVE ( -- )
   SCH-RBF-ENSURE
   SCH-RBF-CUR {: r:^ :}
   SCH-N @ r SCHRB.N !
   SCH-ROOT-N @ r SCHRB.ROOTN !
   SCH-RBF-DEPTH @ 1 + SCH-RBF-DEPTH ! ;
: SCHEMA-ROLLBACK-RESTORE ( -- )
   SCH-RBF-DEPTH @ 1 - SCH-RBF-DEPTH !
   SCH-RBF-CUR {: r:^ :}
   r SCHRB.N @ r SCHRB.ROOTN @ REWIND ;
: SCHEMA-ROLLBACK-FINALIZE ( -- )
   SCH-RBF-DEPTH @ 1 - SCH-RBF-DEPTH ! ;

;package
