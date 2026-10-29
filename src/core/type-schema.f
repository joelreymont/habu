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

private

6 constant SCH-KIND-MAX   \ highest valid creatable tag

\ A quotation payload has four effect SIDES stored as four schema roots contiguous
\ in the schema-root pool: din-row, dout-row, rin-row, rout-row (in that order). B
\ indexes the first. Each root is a SCH-ROW node whose own element roots list that
\ side's ordered type nodes (possibly empty), so a side can be multi-type or empty.
\ The hasr flag (A) records whether the return-stack sides carry a non-neutral
\ effect (an explicit `| rin -- rout` clause).
public

4 constant SCH-QUOT-ROWS

\ --- named reject code. Thrown (not `die`d) so the parser/CHECK path and unit
\ tests can trap a malformed schema node with `catch` instead of aborting.
7103 constant E-SCHEMA-BAD

\ --- node record layout (interleaved cell arena, one grow buffer).
private

0 cells constant SCH.TAG-OFF
1 cells constant SCH.A-OFF
2 cells constant SCH.B-OFF
3 cells constant SCH.C-OFF

public

4 cells constant SCH-REC
private

: SCH.TAG ( ptr a -- ptr a ) SCH.TAG-OFF + ;
: SCH.A ( ptr a -- ptr a ) SCH.A-OFF + ;
: SCH.B ( ptr a -- ptr a ) SCH.B-OFF + ;
: SCH.C ( ptr a -- ptr a ) SCH.C-OFF + ;

4 constant SCH-CAP-INIT         \ small seed; grows geometrically (doubles) on demand
4 constant SCH-ROOT-INIT        \ small seed schema-root pool cells; grows on demand

\ Registry control cells are REG-PROTECTed (src/core/util.f): read them through
\ SCHEMA-N@ / SCHEMA-ROOT-N@.
public

variable SCH-CAP-V   SCH-CAP-INIT SCH-CAP-V !   REG-PROTECT

private

: SCH-CAP ( -- n ) SCH-CAP-V @ ;
create SCH-A-BOOT   SCH-CAP-INIT SCH-REC * allot   REG-PROTECT
PERSISTED-PTR-VARIABLE SCH-A-P    SCH-A-BOOT SCH-A-P !   REG-PROTECT

public

: SCH-BASE ( -- ptr n ) SCH-A-P @ ;

variable SCH-ROOT-CAP-V   SCH-ROOT-INIT SCH-ROOT-CAP-V !   REG-PROTECT

private

: SCH-ROOT-CAP ( -- n ) SCH-ROOT-CAP-V @ ;
create SCH-ROOT-BOOT   SCH-ROOT-INIT cells allot   REG-PROTECT
PERSISTED-PTR-VARIABLE SCH-ROOT-P   SCH-ROOT-BOOT SCH-ROOT-P !   REG-PROTECT

public

: SCH-ROOT-BASE ( -- ptr n ) SCH-ROOT-P @ ;

variable SCH-N   REG-PROTECT        \ next node id; 1 leaves node 0 as the nil sentinel
variable SCH-ROOT-N   REG-PROTECT   \ next schema-root index

private

variable SCH-I        \ private scan index

\ Retiring a node or root scrubs it back to canonical zero, as PF-SCRUB
\ (src/core/type-family.f) does for field rows: a zeroed row is the canonical
\ absent row, so retired rows leave no bytes in reflection or snapshot identity.
: SCH-SCRUB ( n n -- ) {: lo:n hi:n :}      \ zero the nodes [lo,hi)
   SCH-BASE  lo SCH-REC CELL / *  hi SCH-REC CELL / *  ARENA-CELLS-ZERO ;
: SCH-ROOT-SCRUB ( n n -- ) {: lo:n hi:n :} \ zero the roots [lo,hi)
   SCH-ROOT-BASE lo hi ARENA-CELLS-ZERO ;

public

: SCHEMA-RESET ( -- )           \ base state; node 0, the nil sentinel, stays
   1 SCH-N @ SCH-SCRUB
   0 SCH-ROOT-N @ SCH-ROOT-SCRUB
   1 SCH-N !
   0 SCH-ROOT-N ! ;
SCHEMA-RESET

: SCH-GROW ( n -- ) {: need:n :}
   need SCH-CAP-V @ 2 * max {: nc:n :}
   SCH-A-P  SCH-CAP-V @ SCH-REC *  nc SCH-REC *  REG-GROW1
   nc SCH-CAP-V ! ;

private

: SCH-ENSURE ( -- )             \ room for the next node id (SCH-N)
   SCH-N @ SCH-CAP-V @ < IF exit THEN
   SCH-N @ 1 + SCH-GROW ;

public

: SCH-ROOT-GROW ( n -- ) {: need:n :}
   need SCH-ROOT-CAP-V @ 2 * max {: nc:n :}
   SCH-ROOT-P  SCH-ROOT-CAP-V @ cells  nc cells  REG-GROW1
   nc SCH-ROOT-CAP-V ! ;

private

: SCH-ROOT-ENSURE ( -- )        \ room for the next root index (SCH-ROOT-N)
   SCH-ROOT-N @ SCH-ROOT-CAP-V @ < IF exit THEN
   SCH-ROOT-N @ 1 + SCH-ROOT-GROW ;

: SCH-REC@ ( n -- ptr n ) {: id:n :}     \ address of node `id` (nil/oob is a bug)
   id 0 <= IF s" tfam: bad schema node" 76 die THEN
   id SCH-N @ >= IF s" tfam: bad schema node" 76 die THEN
   id SCH-REC * SCH-BASE + ;

: SCHEMA-KIND? ( n -- bool ) {: tag:n :}   \ tag is a creatable node kind
   tag SCH-PARAM >= tag SCH-KIND-MAX <= and ;

public

: SCHEMA-NEW ( n n n n -- n ) {: tag:n a:n b:n c:n :}
   WRITE-WINDOW-CK                        \ src/core/checker.f WRITE-WINDOW
   tag SCHEMA-KIND? 0= IF E-SCHEMA-BAD throw THEN
   SCH-ENSURE
   SCH-N @ {: id:n :}
   id 1 + SCH-N !
   id SCH-REC@ {: r:ptr :}
   tag r SCH.TAG !   a r SCH.A !   b r SCH.B !   c r SCH.C !
   id ;

: SCHEMA-PARAM ( n -- n )
   dup 0 < IF drop E-SCHEMA-BAD throw THEN
   SCH-PARAM swap 0 0 SCHEMA-NEW ;
: SCHEMA-CON ( n -- n )
   SCH-CON swap 0 0 SCHEMA-NEW ;
: SCHEMA-APP ( n n n -- n )
   {: fam:n start:n count:n :}
   count 0 < IF E-SCHEMA-BAD throw THEN
   \ A nullary application (`fam<>`) has an empty argument range: start 0,
   \ as for SCHEMA-ROW.
   SCH-APP fam count 0= IF 0 ELSE start THEN count SCHEMA-NEW ;

: SCHEMA-TAG@ ( n -- n ) SCH-REC@ SCH.TAG @ ;
: SCHEMA-A@ ( n -- n ) SCH-REC@ SCH.A @ ;
: SCHEMA-B@ ( n -- n ) SCH-REC@ SCH.B @ ;
: SCHEMA-C@ ( n -- n ) SCH-REC@ SCH.C @ ;
\ Publics carry no checker effect (loaded before the hook), so internal-mark.f
\ marks them DNAME-INT; SCHEMA-N@ and SCHEMA-ROOT-N@ alone carry PPRIM: axioms
\ in checker.f, because suites read the high-water marks at top level.
: SCHEMA-N@ ( -- n ) SCH-N @ ;             \ node high-water (for rollback/tests)

: SCHEMA-PARAM? ( n -- bool ) SCHEMA-TAG@ SCH-PARAM = ;
: SCHEMA-CON? ( n -- bool ) SCHEMA-TAG@ SCH-CON = ;
: SCHEMA-APP? ( n -- bool ) SCHEMA-TAG@ SCH-APP = ;

\ --- schema-root pool: a flat, growable list of node ids that SUMV variants and
\ product fields reference as contiguous [start,start+count) ranges.
: SCHEMA-ROOT+ ( n -- n ) {: node:n :}
   WRITE-WINDOW-CK
   node 0 <= IF s" tfam: bad schema root" 76 die THEN
   node SCH-N @ >= IF s" tfam: bad schema root" 76 die THEN
   SCH-ROOT-ENSURE
   SCH-ROOT-N @ {: idx:n :}
   node idx cells SCH-ROOT-BASE + !
   idx 1 + SCH-ROOT-N !
   idx ;
: SCHEMA-ROOT@ ( n -- n ) {: idx:n :}
   idx 0 < IF s" tfam: bad schema root index" 76 die THEN
   idx SCH-ROOT-N @ >= IF s" tfam: bad schema root index" 76 die THEN
   idx cells SCH-ROOT-BASE + @ ;
: SCHEMA-ROOT-N@ ( -- n ) SCH-ROOT-N @ ;

\ --- SCH-ROW effect-side node: one ordered row of schema element type nodes,
\ referenced by [start, start+count) contiguous schema roots. An empty side has
\ count 0. SCH-ROW nodes never stand alone as a payload element — only SCH-QUOT
\ reaches them (as its four side roots), and PF-NODE-KIND? walks into them there.
: SCHEMA-NODE-OK? ( n -- bool ) {: node:n :}
   node 0 > node SCH-N @ < and ;
\ An empty row (count 0) is start 0, never the root store's high-water: it
\ names no root, so its start is the same whatever an earlier file left.
: SCHEMA-ROW ( n n -- n ) {: start:n count:n :}   \ row over [start,start+count) element roots
   count 0 < IF E-SCHEMA-BAD throw THEN
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
\ quotation-typed argument. A side that is not a live SCH-ROW node throws
\ E-SCHEMA-BAD.
private

: SCH-FLAG ( n -- n )                       \ canonical 0/-1 flag as a plain cell (any nonzero -> -1)
   0= IF 0 ELSE -1 THEN ;

public

: SCHEMA-QUOT ( n n n n n -- n )
   {: din:n dout:n rin:n rout:n hasr:n :}
   din SCHEMA-ROW-OK? dout SCHEMA-ROW-OK? and
   rin SCHEMA-ROW-OK? and rout SCHEMA-ROW-OK? and 0= IF E-SCHEMA-BAD throw THEN
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

\ --- SCH-PTR: a `ptr T` variant payload element. A malformed child (nil or out
\ of range) throws E-SCHEMA-BAD.
: SCHEMA-PTR ( n -- n )
   dup SCHEMA-NODE-OK? 0= IF drop E-SCHEMA-BAD throw THEN
   SCH-PTR swap 0 0 SCHEMA-NEW ;
: SCHEMA-PTR? ( n -- bool ) SCHEMA-TAG@ SCH-PTR = ;

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

: SCHRB.N ( ptr a -- ptr a ) SCHRB.N-OFF + ;
: SCHRB.ROOTN ( ptr a -- ptr a ) SCHRB.ROOTN-OFF + ;


16 constant SCH-RBF-CAP-INIT
variable SCH-RBF-CAP-V   SCH-RBF-CAP-INIT SCH-RBF-CAP-V !
create SCH-RBF-BOOT   SCH-RBF-CAP-INIT SCH-RBF-REC * allot
PERSISTED-PTR-VARIABLE SCH-RBF-P    SCH-RBF-BOOT SCH-RBF-P !
: SCH-RBF-BASE ( -- ptr n ) SCH-RBF-P @ ;

\ The frame depth has readers outside this file (type-family.f
\ SCHEMA-REG:AT-REST, CHECKER-DECL-FRAME:TYPES-READY), so it is public and
\ REG-PROTECTed, as PF-TX-DEPTH is.
public

variable SCH-RBF-DEPTH   0 SCH-RBF-DEPTH !   REG-PROTECT

private

: SCH-RBF-GROW ( -- )
   SCH-RBF-CAP-V @ 2 * {: nc:n :}
   SCH-RBF-P  SCH-RBF-CAP-V @ SCH-RBF-REC *  nc SCH-RBF-REC *  REG-GROW1
   nc SCH-RBF-CAP-V ! ;
: SCH-RBF-ENSURE ( -- )
   SCH-RBF-DEPTH @ SCH-RBF-CAP-V @ < IF exit THEN
   SCH-RBF-GROW ;
: SCH-RBF-CUR ( -- ptr n ) SCH-RBF-DEPTH @ SCH-RBF-REC * SCH-RBF-BASE + ;

\ ---- rewind to a recorded pair ----------------------------------------------
\ Every place that lowers the schema counts comes through REWIND, which scrubs
\ the retired nodes and roots and moves no frame depth: SCHEMA-ROLLBACK-RESTORE
\ (the checker frame), src/core/sumtype.f TDECL-RESTORE and type-family.f
\ PFX-REWIND. SCHEMA-RESET runs at load, before REWIND exists, and scrubs itself.
\ The prefix boundary is not a scope: it records COUNTS beside the family
\ registry's counters instead of holding a frame open across the boot.

: COUNTS ( -- n n )
   SCH-N @ SCH-ROOT-N @ ;

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
   SCH-RBF-CUR {: r:ptr :}
   SCH-N @ r SCHRB.N !
   SCH-ROOT-N @ r SCHRB.ROOTN !
   SCH-RBF-DEPTH @ 1 + SCH-RBF-DEPTH ! ;
: SCHEMA-ROLLBACK-RESTORE ( -- )
   SCH-RBF-DEPTH @ 1 - SCH-RBF-DEPTH !
   SCH-RBF-CUR {: r:ptr :}
   r SCHRB.N @ r SCHRB.ROOTN @ REWIND ;
: SCHEMA-ROLLBACK-FINALIZE ( -- )
   SCH-RBF-DEPTH @ 1 - SCH-RBF-DEPTH ! ;

\ Snapshot prepare: frames are transient (depth 0 at snapshot), so drop any grown
\ arena back to the baked boot store.
: SCHEMA-RBF-SNAP-RESET ( -- )
   SCH-RBF-DEPTH @ IF s" checker: snapshot inside rollback scope" 76 die THEN
   SCH-RBF-BOOT SCH-RBF-P !
   SCH-RBF-CAP-INIT SCH-RBF-CAP-V !
   SCH-RBF-BOOT 0 SCH-RBF-CAP-INIT SCH-RBF-REC * CELL / ARENA-CELLS-ZERO
   0 SCH-RBF-DEPTH ! ;

\ --- snapshot persist: bake any grown store into fresh image DATA, through the
\ checker's REG-EXT-PERSIST hook.
: SCHEMA-SNAPSHOT-PERSIST ( -- )
   SCH-A-P    SCH-CAP-V @ SCH-REC *  REG-PERSIST-BUF drop
   SCH-ROOT-P SCH-ROOT-CAP-V @ cells REG-PERSIST-BUF drop ;

;package
