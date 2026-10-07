\ Build-side primitive metadata, shared by every backend's engine builder. A
\ builder registers each primitive body and engine helper it emits; this file
\ checks each body's name against the specification table, answers the seed
\ dictionary's name and wid cells, and refuses a kept row that no body answered.
\ A row the captured runtime provides (src/habu/prims.f EPREFIX-PROVIDED!)
\ registers the DATA cell its seeded stub jumps through, and the gate refuses
\ one registered without it, or a seeded build whose captured runtime leaves
\ the cell empty.
\ It takes code positions only as `label` values and never emits code, so it
\ loads without any instruction set. Names are offsets, so growing either buffer
\ cannot invalidate an earlier row. The emitted dictionary format is unchanged.
require src/core/layout-buffer.f
require src/core/bytes.f
require src/core/roles.f
require src/habu/layout.f               \ OWNER-API-PRI-WID, DNAME-INT
require src/habu/prims.f                \ PRIM-SPEC, the specification table
require src/habu/treeshake.f            \ KEEP?

package ENGINE-PRIMS

7 constant ROW-CELLS
$7FFFFFFFFFFFFFFF constant MAX-N
\ The generated storage accessors' size and index refusals: src/core/
\ layout-buffer.f's codes, which every chain that reaches this file (the native
\ prefix, the Gforth recovery prefix) has loaded before it.
E-LAYOUT-BUFFER constant E-SIZE
E-LAYOUT-BOUNDS constant E-INDEX
\ Both specification gates exit with the table's own refusal status.
76 constant SPEC-RC
DYNAMIC-BUFFER ROWS n
DYNAMIC-BUFFER NAMES n
variable USED
variable NAME-BYTES
variable SCALAR-ROW
variable SEEDED

: ROW-FIELD ( n n -- ptr n ) {: row:n field:n :}
   row 0 < row USED @ >= or if E-INDEX throw then
   row ROW-CELLS * field + ROWS ;

: RESERVE ( n -- ) {: size:n :}
   size 0 <= if E-SIZE throw then
   USED @ MAX-N CELL / ROW-CELLS / >= if E-SIZE throw then
   size MAX-N CELL 1- - NAME-BYTES @ - > if E-SIZE throw then
   USED @ 1+ ROW-CELLS * ROWS-RESERVE
   NAME-BYTES @ size + CELL 1- + CELL / NAMES-RESERVE ;

: SPEC-MISSING ( ptr u8 n -- )
   s" prims: primitive with no row in src/habu/prims.f: " type type cr
   s" prims: engine primitive absent from the specification table" SPEC-RC die ;

public

: COUNT ( -- n ) USED @ ;
: RESET ( -- ) 0 USED ! 0 NAME-BYTES ! 0 SCALAR-ROW ! 0 SEEDED ! ;
: SEEDED! ( bool -- ) if 1 else 0 then SEEDED ! ;
: SEEDED? ( -- bool ) SEEDED @ 0<> ;
: RELEASE ( -- ) RESET ROWS-RELEASE NAMES-RELEASE ;

: ADD ( ptr u8 n label label -- n ) {: name:ptr size:n first:label last:label :}
   size RESERVE
   USED @ {: row:n :}
   row ROW-CELLS * ROWS {: dst:ptr :}
   first LABEL>N dst ! last LABEL>N dst cell+ !
   size dst 2 cells + ! NAME-BYTES @ dst 3 cells + !
   -1 dst 4 cells + ! 0 dst 5 cells + ! -1 dst 6 cells + !
   name 0 NAMES BYTE-VIEW NAME-BYTES @ + size BYTE-COPY
   NAME-BYTES @ size + NAME-BYTES !
   row 1+ USED !
   row ;

: FIRST-LABEL ( n -- label ) 0 ROW-FIELD @ >LABEL ;
: LAST-LABEL ( n -- label ) 1 ROW-FIELD @ >LABEL ;
: NAME-LEN ( n -- n ) 2 ROW-FIELD @ ;
: NAME$ ( n -- ptr u8 n ) {: row:n :}
   0 NAMES BYTE-VIEW row 3 ROW-FIELD @ + row NAME-LEN ;
: NAME-LABEL ( n -- label ) 4 ROW-FIELD @ >LABEL ;
: NAME-LABEL! ( label n -- ) swap LABEL>N swap 4 ROW-FIELD ! ;
: WID ( n -- n ) 5 ROW-FIELD @ ;
: WID! ( n n -- ) 5 ROW-FIELD ! ;

\ Only an emitter that owns the completed body may mark it. The specification
\ proves the declared n -> n effect; a helper has no checked row and cannot
\ pass this gate. The marked row is carried into the booted dictionary by its
\ exact ordinal, independently of its spelling or a later checker verdict.
: MARK-SCALAR ( n -- ) {: row:n :}
   SCALAR-ROW @ 0<> if E-INDEX throw then
   row NAME$ PRIM-SPEC:FIND {: spec:n :}
   spec 0 < if E-INDEX throw then
   spec PRIM-SPEC:CODE-LEN@ 4 <> if E-INDEX throw then
   spec 0 PRIM-SPEC:CODE@ PRIM-SPEC:A-NUM <> if E-INDEX throw then
   spec 1 PRIM-SPEC:CODE@ PRIM-SPEC:A-IN <> if E-INDEX throw then
   spec 2 PRIM-SPEC:CODE@ PRIM-SPEC:A-NUM <> if E-INDEX throw then
   spec 3 PRIM-SPEC:CODE@ PRIM-SPEC:A-OUT <> if E-INDEX throw then
   row 1+ SCALAR-ROW ! ;

: SCALAR-ROW@ ( -- n ) SCALAR-ROW @ ;

\ THE TABLE IS WHICH PRIMITIVES EXIST. Every body registers under a name that
\ src/habu/prims.f already specifies - the row states the effect, the body
\ states the machine code, and neither restates the other. A body whose name has
\ no row is a second primitive list starting, so SPEC-CHECK refuses it; the
\ other half of the pair - a row that no body answers - is COMPLETE below. A
\ builder checks each body before KEEP?, so a subset build cannot hide an
\ unspecified primitive.
: SPEC-CHECK ( ptr u8 n -- ) {: name:ptr size:n :}
   name size PRIM-SPEC:FIND 0 < if name size SPEC-MISSING then ;

private

: PROVIDED-ROW? ( ptr u8 n -- bool )
   PRIM-SPEC:FIND {: spec:n :}
   spec 0 < if false exit then
   spec PRIM-SPEC:PREFIX-PROVIDED? ;

: UNMARKED-DISPATCH ( ptr u8 n -- )
   s" prims: dispatch cell for a primitive prims.f does not mark prefix-provided: " type type cr
   s" prims: dispatch cell for an unmarked row" SPEC-RC die ;

public

\ A row the captured runtime provides keeps its record in every build; in a
\ seeded build its body jumps through this DATA cell (layout.f PROVIDED-XT).
\ Every other row has none, -1.
: DISPATCH ( n -- n ) 6 ROW-FIELD @ ;
: DISPATCH! ( n n -- ) {: cell:n row:n :}
   row NAME$ PROVIDED-ROW? 0= if row NAME$ UNMARKED-DISPATCH then
   cell row 6 ROW-FIELD ! ;

\ A primitive whose dictionary record is globally searchable but cannot be
\ executed or ticked by ordinary source. The sentinel lives only in this
\ build-side registry; the emitted record carries WID 0 and DNAME-INT, and
\ DNAME-OWNED as well when a package row types the primitive (OWNED?): its
\ owner's checked callers compile.
-1 constant GLOBAL-INT-WID

\ An engine helper is an engine-resident routine that guarded primitives reach
\ by a direct branch (a shared span guard, a bounds loop). HELPER-REGISTER
\ records it as a sealed, system-private dictionary record spanning
\ [start-addr, end-addr) and stamps it with OWNER-API-PRI-WID. That stamp is the
\ closed registration marker: it hides the helper from raw word searches (BSWL)
\ and gives it a sealed dictionary record, so the ahead-of-time closure walker
\ resolves a direct branch into the helper by its exact code entry like any
\ record (aot-closure.f FINDADDR-PTR). Only a record's exact code entry is ever
\ followed, so the AOT image can never pull in unintended code. DNAME folds the
\ same marker into a record's baked name/flags cell so the seed dictionary
\ carries the internal bit for helper records.
: HELPER-REGISTER ( ptr u8 n n n -- )
   >LABEL swap >LABEL swap ADD OWNER-API-PRI-WID swap WID! ;

\ THE SEED RECORD STATES ITS PRIMITIVE'S MINIMUM INPUT DEPTH (DNAME-MIN-IN,
\ bits 52-59), read from the specification row, so the interpret band refuses
\ a bare primitive on a shallower stack before its body runs, exactly as it
\ refuses a certified word. An engine helper has no row and states 0.
private

: MIN-IN-BITS ( ptr u8 n -- n ) {: name:ptr size:n :}
   name size PRIM-SPEC:FIND {: row:n :}
   row 0 < if 0 exit then
   row PRIM-SPEC:MIN-IN {: depth:n :}
   depth DNAME-MIN-IN-MASK 52 rshift > if
      name size type cr
      s" prims: minimum input depth exceeds the record field" SPEC-RC die
   then
   depth 52 lshift ;

\ Whether a package row (src/habu/prims.f EPPRIM: ... ECLOSE-PRIVATE) states the
\ primitive's effect. An internal primitive with one is its owner's to call, so
\ DNAME stamps it DNAME-OWNED (src/habu/layout.f).
: OWNED? ( ptr u8 n -- bool ) {: name:ptr size:n :}
   PRIM-SPEC:COUNT 0 ?do
      i PRIM-SPEC:KIND@ PRIM-SPEC:K-PKG-PRIVATE = if
         i PRIM-SPEC:NAME$ name size CORE-STR= if unloop 0 0= exit then
      then
   loop
   0 0= 0= ;

public

: DNAME ( n -- n ) {: idx:n :}
   idx NAME-LEN  idx NAME$ MIN-IN-BITS or
   idx WID {: wid:n :}
   wid OWNER-API-PRI-WID =  wid GLOBAL-INT-WID = or if DNAME-INT or then
   wid GLOBAL-INT-WID = if
      idx NAME$ OWNED? if DNAME-OWNED or then
   then ;

: HELPER-WID ( n -- n )
   WID dup GLOBAL-INT-WID = if drop 0 then ;

\ ---- the specification's other half ------------------------------------------
\ SPEC-CHECK refuses a body whose name has no row. This is the converse: a row
\ that this backend never answered. Without it a primitive could be specified,
\ and every checked caller believe the effect, while the engine carried no code
\ for the name - which is the same fork the table exists to close, read from the
\ other end.
\
\ IT ASKS KEEP?, because a subset build drops bodies on purpose (the builder asks
\ KEEP? before it emits a body). A row the treeshaker drops is not a missing
\ body; a row it keeps and no body registered is. Run it after the last body.
\
\ A ROW THE CAPTURED RUNTIME PROVIDES (src/habu/prims.f EPREFIX-PROVIDED!) must
\ also register its dispatch cell: a body without one is an assembly body under
\ the runtime's name, which a seeded build would run in place of the runtime's
\ word. A seeded build's body is the stub alone, so its captured runtime must
\ fill the cell; the builder's provider answers that for a cell.
private

\ The registered body of the name, or -1.
: BODY-ROW ( ptr u8 n -- n ) {: name:ptr size:n :}
   USED @ 0 ?do
      i NAME$ name size CORE-STR= if i unloop exit then
   loop
   -1 ;

: NO-BODY ( ptr u8 n -- )
   s" prims: no backend body for specified primitive " type type cr
   s" prims: specification row without a backend body" SPEC-RC die ;

: NO-DISPATCH ( ptr u8 n -- )
   s" prims: no dispatch cell for prefix-provided primitive " type type cr
   s" prims: prefix-provided row registered without its dispatch cell" SPEC-RC die ;

: UNFILLED ( ptr u8 n -- )
   s" prims: captured runtime leaves the dispatch cell empty for prefix-provided primitive " type type cr
   s" prims: prefix-provided row not in the captured runtime" SPEC-RC die ;

: CHECK-ROW ( n [ n -- bool ] -- ) {: spec:n filled :}
   spec PRIM-SPEC:NAME$ {: name:ptr size:n :}
   name size KEEP? 0= if exit then
   name size BODY-ROW {: body:n :}
   body 0 < if name size NO-BODY then
   spec PRIM-SPEC:PREFIX-PROVIDED? 0= if exit then
   body DISPATCH {: cell:n :}
   cell 0 < if name size NO-DISPATCH then
   SEEDED? 0= if exit then
   cell filled execute 0= if name size UNFILLED then ;

public

: COMPLETE ( [ n -- bool ] -- ) {: filled :}
   PRIM-SPEC:COUNT 0 ?do i filled CHECK-ROW loop ;

;package
