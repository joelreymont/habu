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
3 constant SCH-KIND-MAX   \ highest valid creatable tag

\ --- named reject code. Thrown (not `die`d) so the parser/CHECK path and unit
\ tests can trap a malformed schema node with `catch` instead of aborting.
7103 constant E-SCHEMA-BAD

\ --- node record layout (interleaved cell arena, one grow buffer).
BEGIN-STRUCTURE SCH-REC
   CELL +FIELD SCH.TAG
   CELL +FIELD SCH.A
   CELL +FIELD SCH.B
   CELL +FIELD SCH.C
END-STRUCTURE

4 constant SCH-CAP-INIT         \ small seed; grows geometrically (doubles) on demand
4 constant SCH-ROOT-INIT        \ small seed schema-root pool cells; grows on demand

variable SCH-CAP-V   SCH-CAP-INIT SCH-CAP-V !
: SCH-CAP ( -- n ) SCH-CAP-V @ ;
create SCH-A-BOOT   SCH-CAP-INIT SCH-REC * allot
variable SCH-A-P    SCH-A-BOOT SCH-A-P !
: SCH-BASE ( -- ptr ) SCH-A-P @ ;

variable SCH-ROOT-CAP-V   SCH-ROOT-INIT SCH-ROOT-CAP-V !
: SCH-ROOT-CAP ( -- n ) SCH-ROOT-CAP-V @ ;
create SCH-ROOT-BOOT   SCH-ROOT-INIT cells allot
variable SCH-ROOT-P   SCH-ROOT-BOOT SCH-ROOT-P !
: SCH-ROOT-BASE ( -- ptr ) SCH-ROOT-P @ ;

variable SCH-N        \ next node id; 1 leaves node 0 as the nil sentinel
variable SCH-ROOT-N   \ next schema-root index
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

: SCH-REC@ ( n -- ptr ) {: id:n :}       \ address of node `id` (nil/oob is a bug)
   id 0 <= IF s" tfam: bad schema node" 76 die THEN
   id SCH-N @ >= IF s" tfam: bad schema node" 76 die THEN
   id SCH-REC * SCH-BASE + ;

: SCHEMA-KIND? ( n -- bool ) {: tag:n :}   \ tag is a creatable node kind
   tag SCH-PARAM >= tag SCH-KIND-MAX <= and ;

: SCHEMA-NEW ( tag a b c -- node ) {: tag:n a:n b:n c:n :}
   tag SCHEMA-KIND? 0= IF E-SCHEMA-BAD throw THEN
   SCH-ENSURE
   SCH-N @ {: id:n :}
   id 1 + SCH-N !
   id SCH-REC@ {: r:ptr :}
   tag r SCH.TAG !   a r SCH.A !   b r SCH.B !   c r SCH.C !
   id ;

: SCHEMA-PARAM ( param-idx -- node )       \ paramref schema node
   dup 0 < IF drop E-SCHEMA-BAD throw THEN
   SCH-PARAM swap 0 0 SCHEMA-NEW ;
: SCHEMA-CON ( con-code -- node )          \ concrete-type schema node
   SCH-CON swap 0 0 SCHEMA-NEW ;
: SCHEMA-APP ( family-id arg-start arg-count -- node )   \ family application node
   {: fam:n start:n count:n :}
   count 0 < IF E-SCHEMA-BAD throw THEN
   SCH-APP fam start count SCHEMA-NEW ;

: SCHEMA-TAG@ ( node -- tag ) SCH-REC@ SCH.TAG @ ;
: SCHEMA-A@ ( node -- n ) SCH-REC@ SCH.A @ ;
: SCHEMA-B@ ( node -- n ) SCH-REC@ SCH.B @ ;
: SCHEMA-C@ ( node -- n ) SCH-REC@ SCH.C @ ;
: SCHEMA-N@ ( -- n ) SCH-N @ ;             \ node high-water (for rollback/tests)

: SCHEMA-PARAM? ( node -- bool ) SCHEMA-TAG@ SCH-PARAM = ;
: SCHEMA-CON? ( node -- bool ) SCHEMA-TAG@ SCH-CON = ;
: SCHEMA-APP? ( node -- bool ) SCHEMA-TAG@ SCH-APP = ;

\ --- schema-root pool: a flat, growable list of node ids that SUMV variants and
\ product fields reference as contiguous [start,start+count) ranges.
: SCHEMA-ROOT+ ( node -- idx ) {: node:n :}
   node 0 <= IF s" tfam: bad schema root" 76 die THEN
   node SCH-N @ >= IF s" tfam: bad schema root" 76 die THEN
   SCH-ROOT-ENSURE
   SCH-ROOT-N @ {: idx:n :}
   node idx cells SCH-ROOT-BASE + !
   idx 1 + SCH-ROOT-N !
   idx ;
: SCHEMA-ROOT@ ( idx -- node ) {: idx:n :}
   idx 0 < IF s" tfam: bad schema root index" 76 die THEN
   idx SCH-ROOT-N @ >= IF s" tfam: bad schema root index" 76 die THEN
   idx cells SCH-ROOT-BASE + @ ;
: SCHEMA-ROOT-N@ ( -- n ) SCH-ROOT-N @ ;

\ ---------------------------------------------------------------------------
\ rollback frame stack (SCHEMA half of the checker's transactional rollback).
\ Each checker scope/candidate saves the node + schema-root high-water marks;
\ rejecting a scope/candidate pops them so a rejected family's schema nodes are
\ retired (a later scan/new-node reuses the freed ids). Node/root fields are
\ pointer-free, so restoring the counters fully retires the entries. Pushed/popped
\ in lockstep with checker.f's core frame via the REG-EXT-RB-* hooks.
\ ---------------------------------------------------------------------------
BEGIN-STRUCTURE SCH-RBF-REC
   CELL +FIELD SCHRB.N
   CELL +FIELD SCHRB.ROOTN
END-STRUCTURE

16 constant SCH-RBF-CAP-INIT
variable SCH-RBF-CAP-V   SCH-RBF-CAP-INIT SCH-RBF-CAP-V !
create SCH-RBF-BOOT   SCH-RBF-CAP-INIT SCH-RBF-REC * allot
variable SCH-RBF-P    SCH-RBF-BOOT SCH-RBF-P !
: SCH-RBF-BASE ( -- ptr ) SCH-RBF-P @ ;
variable SCH-RBF-DEPTH   0 SCH-RBF-DEPTH !

: SCH-RBF-GROW ( -- )
   SCH-RBF-CAP-V @ 2 * {: nc:n :}
   SCH-RBF-P  SCH-RBF-CAP-V @ SCH-RBF-REC *  nc SCH-RBF-REC *  REG-GROW1
   nc SCH-RBF-CAP-V ! ;
: SCH-RBF-ENSURE ( -- )
   SCH-RBF-DEPTH @ SCH-RBF-CAP-V @ < IF exit THEN
   SCH-RBF-GROW ;
: SCH-RBF-CUR ( -- ptr ) SCH-RBF-DEPTH @ SCH-RBF-REC * SCH-RBF-BASE + ;

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

\ SCHEMA-RBF-SNAP-RESET ( -- ) : snapshot prepare — frames are transient (depth 0
\ at snapshot), so drop any grown arena back to the baked boot store.
: SCHEMA-RBF-SNAP-RESET ( -- )
   SCH-RBF-BOOT SCH-RBF-P !
   SCH-RBF-CAP-INIT SCH-RBF-CAP-V !
   0 SCH-RBF-DEPTH ! ;

\ --- snapshot persist: bake any grown store into fresh image DATA. Fields hold no
\ pointers, so nothing rebases. Called through the checker's REG-EXT-PERSIST hook.
: SCHEMA-SNAPSHOT-PERSIST ( -- )
   SCH-A-P    SCH-A-BOOT    SCH-CAP-V @ SCH-REC *  REG-PERSIST-BUF drop
   SCH-ROOT-P SCH-ROOT-BOOT SCH-ROOT-CAP-V @ cells REG-PERSIST-BUF drop ;
