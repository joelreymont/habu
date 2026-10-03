\ Build-side primitive metadata, shared by every backend's engine builder. A
\ builder registers each primitive body and engine helper it emits; this file
\ checks each body's name against the specification table, answers the seed
\ dictionary's name and wid cells, and refuses a kept row that no body answered.
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

6 constant ROW-CELLS
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
: RESET ( -- ) 0 USED ! 0 NAME-BYTES ! ;
: RELEASE ( -- ) RESET ROWS-RELEASE NAMES-RELEASE ;

: ADD ( ptr u8 n label label -- n ) {: name:ptr size:n first:label last:label :}
   size RESERVE
   USED @ {: row:n :}
   row ROW-CELLS * ROWS {: dst:ptr :}
   first LABEL>N dst ! last LABEL>N dst cell+ !
   size dst 2 cells + ! NAME-BYTES @ dst 3 cells + !
   -1 dst 4 cells + ! 0 dst 5 cells + !
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

\ THE TABLE IS WHICH PRIMITIVES EXIST. Every body registers under a name that
\ src/habu/prims.f already specifies - the row states the effect, the body
\ states the machine code, and neither restates the other. A body whose name has
\ no row is a second primitive list starting, so SPEC-CHECK refuses it; the
\ other half of the pair - a row that no body answers - is COMPLETE below. A
\ builder checks each body before KEEP?, so a subset build cannot hide an
\ unspecified primitive.
: SPEC-CHECK ( ptr u8 n -- ) {: name:ptr size:n :}
   name size PRIM-SPEC:FIND 0 < if name size SPEC-MISSING then ;

\ A primitive whose dictionary record is globally searchable but cannot be
\ executed or ticked by ordinary source. The sentinel lives only in this
\ build-side registry; the emitted record carries WID 0 and DNAME-INT.
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

public

: DNAME ( n -- n ) {: idx:n :}
   idx NAME-LEN  idx NAME$ MIN-IN-BITS or
   idx WID {: wid:n :}
   wid OWNER-API-PRI-WID =  wid GLOBAL-INT-WID = or if DNAME-INT or then ;

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
private

: BODY? ( ptr u8 n -- bool ) {: name:ptr size:n :}
   USED @ 0 ?do
      i NAME$ name size CORE-STR= if unloop 0 0= exit then
   loop
   0 0= 0= ;

: NO-BODY ( ptr u8 n -- )
   s" prims: no backend body for specified primitive " type type cr
   s" prims: specification row without a backend body" SPEC-RC die ;

: CHECK-ROW ( n -- )
   PRIM-SPEC:NAME$
   2dup KEEP? if
      2dup BODY? 0= if NO-BODY else 2drop then
   else 2drop then ;

public

: COMPLETE ( -- )
   PRIM-SPEC:COUNT 0 ?do i CHECK-ROW loop ;

;package
