\ type-schema.f — persistent type-schema node arena for the checker's type-family
\ system (package TFAM). Schema nodes are a generic type tree that families,
\ variants, and product fields reference as "schema roots" (see
\ docs/type-families.md §6-7, §21). Every node field holds only integers
\ (con codes, family ids, root indices) — never a relocating pointer — so a grow
\ is a plain cell copy and snapshot persist bakes stores verbatim with no rebase.
\ Loaded unchecked in the checker prefix, right after checker.f, mirroring the
\ VREC value-record registry it is modelled on. Mutators are package-private
\ implementation words; only read-only queries are meant to leave the package
\ once sealing (dot 2b) lands.

\ --- schema node kinds (node tag values). Node id 0 is the reserved nil node.
0 constant SCH-NIL
1 constant SCH-PARAM      \ A = parameter index (>= 0)
2 constant SCH-CON        \ A = concrete con code
3 constant SCH-APP        \ A = family-id, B = arg-root start, C = arg count
4 constant SCH-QUOT       \ A = hasr flag, B = side-row-root start, C = SCH-QUOT-ROWS
5 constant SCH-PTR        \ A = pointee schema node (docs §8 SC-PTR)
6 constant SCH-ROW        \ A = element-root start, B = element count (one quotation effect side)
6 constant SCH-KIND-MAX   \ highest valid creatable tag

\ A quotation payload has four effect SIDES stored as four schema roots contiguous
\ in the schema-root pool: din-row, dout-row, rin-row, rout-row (in that order). B
\ indexes the first. Each root is a SCH-ROW node whose own element roots list that
\ side's ordered type nodes (possibly empty), so a side can be multi-type or empty.
\ The hasr flag (A) records whether the return-stack sides carry a non-neutral
\ effect (an explicit `| rin -- rout` clause).
4 constant SCH-QUOT-ROWS

\ --- named reject code. Thrown (not `die`d) so the parser/CHECK path and unit
\ tests can trap a malformed schema node with `catch` instead of aborting.
7103 constant E-SCHEMA-BAD

\ --- node record layout (interleaved cell arena, one grow buffer).
\ Bit i in a PTR-MASK marks slot i as a relocating pointer. Schema records
\ contain only scalar ids/codes, so both masks are zero.
0 cells constant SCH.TAG-OFF
1 cells constant SCH.A-OFF
2 cells constant SCH.B-OFF
3 cells constant SCH.C-OFF
4 cells constant SCH-REC
CELL constant SCH-REC-ALIGN
0 constant SCH-REC-PTR-MASK

: SCH.TAG ( ptr a -- ptr a ) SCH.TAG-OFF + ;
: SCH.A ( ptr a -- ptr a ) SCH.A-OFF + ;
: SCH.B ( ptr a -- ptr a ) SCH.B-OFF + ;
: SCH.C ( ptr a -- ptr a ) SCH.C-OFF + ;

: SCH-LAYOUT= ( n n -- )
   <> if s" type-schema: layout drift" CORE-LAYOUT-RC die then ;

SCH.TAG-OFF 0 cells SCH-LAYOUT=
SCH.A-OFF 1 cells SCH-LAYOUT=
SCH.B-OFF 2 cells SCH-LAYOUT=
SCH.C-OFF 3 cells SCH-LAYOUT=
SCH-REC 4 cells SCH-LAYOUT=
SCH-REC-ALIGN CELL SCH-LAYOUT=
SCH-REC SCH-REC-ALIGN mod 0 SCH-LAYOUT=
SCH-REC-PTR-MASK 0 SCH-LAYOUT=
0 SCH.TAG SCH.TAG-OFF SCH-LAYOUT=
0 SCH.A SCH.A-OFF SCH-LAYOUT=
0 SCH.B SCH.B-OFF SCH-LAYOUT=
0 SCH.C SCH.C-OFF SCH-LAYOUT=

4 constant SCH-CAP-INIT         \ small seed; grows geometrically (doubles) on demand
4 constant SCH-ROOT-INIT        \ small seed schema-root pool cells; grows on demand

\ Registry control cells are sealed DNAME-INT by REG-PROTECT / IMK-SEAL-REGISTRY
\ (src/core/util.f, src/core/internal-mark.f): a bare `<cell> @`/`<cell> !` or
\ `' <cell>` fails closed rc 70. Read the schema registry through the certified
\ SCHEMA-N@ / SCHEMA-ROOT-N@ accessors, never the raw cell.
variable SCH-CAP-V   SCH-CAP-INIT SCH-CAP-V !   REG-PROTECT
: SCH-CAP ( -- n ) SCH-CAP-V @ ;
create SCH-A-BOOT   SCH-CAP-INIT SCH-REC * allot   REG-PROTECT
variable SCH-A-P    SCH-A-BOOT SCH-A-P !   REG-PROTECT
: SCH-BASE ( -- ptr a ) SCH-A-P @ ;

variable SCH-ROOT-CAP-V   SCH-ROOT-INIT SCH-ROOT-CAP-V !   REG-PROTECT
: SCH-ROOT-CAP ( -- n ) SCH-ROOT-CAP-V @ ;
create SCH-ROOT-BOOT   SCH-ROOT-INIT cells allot   REG-PROTECT
variable SCH-ROOT-P   SCH-ROOT-BOOT SCH-ROOT-P !   REG-PROTECT
: SCH-ROOT-BASE ( -- ptr a ) SCH-ROOT-P @ ;

variable SCH-N   REG-PROTECT        \ next node id; 1 leaves node 0 as the nil sentinel
variable SCH-ROOT-N   REG-PROTECT   \ next schema-root index
variable SCH-I        \ private scan index

: SCHEMA-RESET ( -- )           \ base state (item 3 will add high-water rollback)
   1 SCH-N !
   0 SCH-ROOT-N ! ;
SCHEMA-RESET

\ --- geometric grow. Node/root cells are pointer-free, so a straight cell copy
\ (REG-GROW1) suffices and no stored value needs rebasing.
: SCH-GROW ( n -- ) {: need:n :}
   need SCH-CAP-V @ 2 * max {: nc:n :}
   SCH-A-P  SCH-CAP-V @ SCH-REC *  nc SCH-REC *  REG-GROW1
   nc SCH-CAP-V ! ;
: SCH-ENSURE ( -- )             \ room for the next node id (SCH-N)
   SCH-N @ SCH-CAP-V @ < IF exit THEN
   SCH-N @ 1 + SCH-GROW ;

: SCH-ROOT-GROW ( n -- ) {: need:n :}
   need SCH-ROOT-CAP-V @ 2 * max {: nc:n :}
   SCH-ROOT-P  SCH-ROOT-CAP-V @ cells  nc cells  REG-GROW1
   nc SCH-ROOT-CAP-V ! ;
: SCH-ROOT-ENSURE ( -- )        \ room for the next root index (SCH-ROOT-N)
   SCH-ROOT-N @ SCH-ROOT-CAP-V @ < IF exit THEN
   SCH-ROOT-N @ 1 + SCH-ROOT-GROW ;

: SCH-REC@ ( n -- ptr a ) {: id:n :}     \ address of node `id` (nil/oob is a bug)
   id 0 <= IF s" tfam: bad schema node" 76 die THEN
   id SCH-N @ >= IF s" tfam: bad schema node" 76 die THEN
   id SCH-REC * SCH-BASE + ;

: SCHEMA-KIND? ( n -- bool ) {: tag:n :}   \ tag is a creatable node kind
   tag SCH-PARAM >= tag SCH-KIND-MAX <= and ;

: SCHEMA-NEW ( n n n n -- n ) {: tag:n a:n b:n c:n :}
   tag SCHEMA-KIND? 0= IF E-SCHEMA-BAD throw THEN
   SCH-ENSURE
   SCH-N @ {: id:n :}
   id 1 + SCH-N !
   id SCH-REC@ {: r:ptr :}
   tag r SCH.TAG !   a r SCH.A !   b r SCH.B !   c r SCH.C !
   id ;

: SCHEMA-PARAM ( n -- n )                  \ paramref schema node
   dup 0 < IF drop E-SCHEMA-BAD throw THEN
   SCH-PARAM swap 0 0 SCHEMA-NEW ;
: SCHEMA-CON ( n -- n )                    \ concrete-type schema node
   SCH-CON swap 0 0 SCHEMA-NEW ;
: SCHEMA-APP ( n n n -- n )                \ family application node
   {: fam:n start:n count:n :}
   count 0 < IF E-SCHEMA-BAD throw THEN
   SCH-APP fam start count SCHEMA-NEW ;

: SCHEMA-TAG@ ( n -- n ) SCH-REC@ SCH.TAG @ ;
: SCHEMA-A@ ( n -- n ) SCH-REC@ SCH.A @ ;
: SCHEMA-B@ ( n -- n ) SCH-REC@ SCH.B @ ;
: SCHEMA-C@ ( n -- n ) SCH-REC@ SCH.C @ ;
: SCHEMA-N@ ( -- n ) SCH-N @ ;             \ node high-water (for rollback/tests)

: SCHEMA-PARAM? ( n -- bool ) SCHEMA-TAG@ SCH-PARAM = ;
: SCHEMA-CON? ( n -- bool ) SCHEMA-TAG@ SCH-CON = ;
: SCHEMA-APP? ( n -- bool ) SCHEMA-TAG@ SCH-APP = ;

\ --- schema-root pool: a flat, growable list of node ids that SUMV variants and
\ product fields reference as contiguous [start,start+count) ranges.
: SCHEMA-ROOT+ ( n -- n ) {: node:n :}
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
: SCHEMA-ROW ( n n -- n ) {: start:n count:n :}   \ row over [start,start+count) element roots
   count 0 < IF E-SCHEMA-BAD throw THEN
   SCH-ROW start count 0 SCHEMA-NEW ;
: SCHEMA-ROW? ( n -- bool ) SCHEMA-TAG@ SCH-ROW = ;
: SCHEMA-ROW-START@ ( n -- n ) SCHEMA-A@ ;
: SCHEMA-ROW-COUNT@ ( n -- n ) SCHEMA-B@ ;
: SCHEMA-ROW-ELEM@ ( n n -- n ) {: node:n i:n :}   \ i-th element type node of the row
   i 0 < i node SCHEMA-ROW-COUNT@ >= or IF s" tfam: bad quot row elem index" 76 die THEN
   node SCHEMA-ROW-START@ i + SCHEMA-ROOT@ ;
: SCHEMA-ROW-OK? ( n -- bool )                     \ live SCH-ROW node (no die on nil/oob)
   dup SCHEMA-NODE-OK? IF SCHEMA-ROW? ELSE drop RES-FALSE THEN ;

\ --- SCH-QUOT quotation payload node. Mirrors the checker's VR-QUOT/EN-QUOT/MK-QUOT
\ effect sides (din, dout, rin, rout) as four SCH-ROW roots plus a hasr flag, so a
\ family or product schema can carry a quotation-typed argument without collapsing it
\ to a string. Each side is a full ordered row (multi-type or empty). A side that is
\ not a live SCH-ROW node throws E-SCHEMA-BAD so parse/CHECK and unit tests can trap
\ it with `catch`.
: SCH-FLAG ( n -- n )                       \ canonical 0/-1 flag as a plain cell (any nonzero -> -1)
   0= IF 0 ELSE -1 THEN ;
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

\ --- SCH-PTR pointer payload node (docs §8 SC-PTR): a `ptr T` variant payload
\ element. A malformed child (nil / out-of-range) throws E-SCHEMA-BAD so the
\ declaration parser and unit tests can trap it with `catch`.
: SCHEMA-PTR ( n -- n )
   dup SCHEMA-NODE-OK? 0= IF drop E-SCHEMA-BAD throw THEN
   SCH-PTR swap 0 0 SCHEMA-NEW ;
: SCHEMA-PTR? ( n -- bool ) SCHEMA-TAG@ SCH-PTR = ;

\ ---------------------------------------------------------------------------
\ rollback frame stack (SCHEMA half of the checker's transactional rollback).
\ Each checker scope/candidate saves the node + schema-root high-water marks;
\ rejecting a scope/candidate pops them so a rejected family's schema nodes are
\ retired (a later scan/new-node reuses the freed ids). Node/root fields are
\ pointer-free, so restoring the counters fully retires the entries. Pushed/popped
\ in lockstep with checker.f's core frame via the REG-EXT-RB-* hooks.
\ ---------------------------------------------------------------------------
0 cells constant SCHRB.N-OFF
1 cells constant SCHRB.ROOTN-OFF
2 cells constant SCH-RBF-REC
CELL constant SCH-RBF-REC-ALIGN
0 constant SCH-RBF-REC-PTR-MASK

: SCHRB.N ( ptr a -- ptr a ) SCHRB.N-OFF + ;
: SCHRB.ROOTN ( ptr a -- ptr a ) SCHRB.ROOTN-OFF + ;

SCHRB.N-OFF 0 cells SCH-LAYOUT=
SCHRB.ROOTN-OFF 1 cells SCH-LAYOUT=
SCH-RBF-REC 2 cells SCH-LAYOUT=
SCH-RBF-REC-ALIGN CELL SCH-LAYOUT=
SCH-RBF-REC SCH-RBF-REC-ALIGN mod 0 SCH-LAYOUT=
SCH-RBF-REC-PTR-MASK 0 SCH-LAYOUT=
0 SCHRB.N SCHRB.N-OFF SCH-LAYOUT=
0 SCHRB.ROOTN SCHRB.ROOTN-OFF SCH-LAYOUT=

16 constant SCH-RBF-CAP-INIT
variable SCH-RBF-CAP-V   SCH-RBF-CAP-INIT SCH-RBF-CAP-V !
create SCH-RBF-BOOT   SCH-RBF-CAP-INIT SCH-RBF-REC * allot
variable SCH-RBF-P    SCH-RBF-BOOT SCH-RBF-P !
: SCH-RBF-BASE ( -- ptr a ) SCH-RBF-P @ ;
variable SCH-RBF-DEPTH   0 SCH-RBF-DEPTH !

: SCH-RBF-GROW ( -- )
   SCH-RBF-CAP-V @ 2 * {: nc:n :}
   SCH-RBF-P  SCH-RBF-CAP-V @ SCH-RBF-REC *  nc SCH-RBF-REC *  REG-GROW1
   nc SCH-RBF-CAP-V ! ;
: SCH-RBF-ENSURE ( -- )
   SCH-RBF-DEPTH @ SCH-RBF-CAP-V @ < IF exit THEN
   SCH-RBF-GROW ;
: SCH-RBF-CUR ( -- ptr a ) SCH-RBF-DEPTH @ SCH-RBF-REC * SCH-RBF-BASE + ;

: SCHEMA-ROLLBACK-SAVE ( -- )
   SCH-RBF-ENSURE
   SCH-RBF-CUR {: r:ptr :}
   SCH-N @ r SCHRB.N !
   SCH-ROOT-N @ r SCHRB.ROOTN !
   SCH-RBF-DEPTH @ 1 + SCH-RBF-DEPTH ! ;
: SCHEMA-ROLLBACK-RESTORE ( -- )
   SCH-RBF-DEPTH @ 1 - SCH-RBF-DEPTH !
   SCH-RBF-CUR {: r:ptr :}
   r SCHRB.N @ SCH-N !
   r SCHRB.ROOTN @ SCH-ROOT-N ! ;
: SCHEMA-ROLLBACK-FINALIZE ( -- )
   SCH-RBF-DEPTH @ 1 - SCH-RBF-DEPTH ! ;

\ SCHEMA-RBF-SNAP-RESET ( -- ) : snapshot prepare — frames are transient (depth 0
\ at snapshot), so drop any grown arena back to the baked boot store.
: SCHEMA-RBF-SNAP-RESET ( -- )
   SCH-RBF-DEPTH @ IF s" checker: snapshot inside rollback scope" 76 die THEN
   SCH-RBF-BOOT SCH-RBF-P !
   SCH-RBF-CAP-INIT SCH-RBF-CAP-V !
   0 SCH-RBF-DEPTH ! ;

\ --- snapshot persist: bake any grown store into fresh image DATA. Fields hold no
\ pointers, so nothing rebases. Called through the checker's REG-EXT-PERSIST hook.
: SCHEMA-SNAPSHOT-PERSIST ( -- )
   SCH-A-P    SCH-A-BOOT    SCH-CAP-V @ SCH-REC *  REG-PERSIST-BUF drop
   SCH-ROOT-P SCH-ROOT-BOOT SCH-ROOT-CAP-V @ cells REG-PERSIST-BUF drop ;
