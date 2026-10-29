\ primitive-registry.f - build-side primitive metadata shared by every backend's
\ engine builder: checks each registered body's name against the specification
\ table, answers the seed dictionary's name and wid cells, and refuses a kept row
\ no body answered. Code positions arrive only as `label` values and no code is
\ emitted, so it loads without an instruction set. Names are offsets, so growing
\ either buffer cannot invalidate an earlier row.
require src/core/layout-buffer.f
require src/core/bytes.f
require src/core/roles.f
require src/habu/layout.f               \ OWNER-API-PRI-WID, DNAME-INT
require src/habu/prims.f                \ PRIM-SPEC, the specification table

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
variable DIVISION-ROW
\ Nonzero while an x86-64 build carries a complete captured runtime; RESET
\ clears it and the x86-64 builder arms it (tools/native-emit-x64.f STREAM,).
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
: RESET ( -- ) 0 USED ! 0 NAME-BYTES ! 0 SCALAR-ROW ! 0 DIVISION-ROW ! 0 SEEDED ! ;
: SEEDED! ( bool -- ) if 1 else 0 then SEEDED ! ;
: SEEDED? ( -- bool ) SEEDED @ 0<> ;

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

\ The division emitter owns the guarded body and its terminal refusal helper.
\ This gate checks the declared shape only; the emitter supplies the behavior.
: MARK-DIVISION ( n -- ) {: row:n :}
   DIVISION-ROW @ 0<> if E-INDEX throw then
   row NAME$ PRIM-SPEC:FIND {: spec:n :}
   spec 0 < if E-INDEX throw then
   spec PRIM-SPEC:CODE-LEN@ 6 <> if E-INDEX throw then
   spec 0 PRIM-SPEC:CODE@ PRIM-SPEC:A-NUM <> if E-INDEX throw then
   spec 1 PRIM-SPEC:CODE@ PRIM-SPEC:A-IN <> if E-INDEX throw then
   spec 2 PRIM-SPEC:CODE@ PRIM-SPEC:A-NUM <> if E-INDEX throw then
   spec 3 PRIM-SPEC:CODE@ PRIM-SPEC:A-IN <> if E-INDEX throw then
   spec 4 PRIM-SPEC:CODE@ PRIM-SPEC:A-NUM <> if E-INDEX throw then
   spec 5 PRIM-SPEC:CODE@ PRIM-SPEC:A-OUT <> if E-INDEX throw then
   row 1+ DIVISION-ROW ! ;

: DIVISION-ROW@ ( -- n ) DIVISION-ROW @ ;

\ The table is which primitives exist: every body registers under a name
\ src/habu/prims.f specifies; the row states the effect, the body the machine
\ code. SPEC-CHECK refuses a body with no row, COMPLETE a row no body answered.
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

\ On x86-64 a row the captured runtime provides keeps its record in every
\ build; in a seeded build its body jumps through this DATA cell (layout.f
\ PROVIDED-XT). Every other row, and every ARM64 row, has none, -1.
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

\ An engine helper is an engine-resident routine guarded primitives reach by a
\ direct branch (a span guard, a bounds loop). HELPER-REGISTER records it as a
\ sealed system-private record over [start-addr, end-addr) stamped
\ OWNER-API-PRI-WID: hidden from raw searches (BSWL), and resolved by the AOT
\ closure walker by its exact code entry (aot-closure.f FINDADDR-PTR), the only
\ entry ever followed. DNAME folds the marker into the baked name/flags cell.
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

\ COMPLETE, run after the last body, refuses a row this backend never answered:
\ an effect checked callers believe, with no code for the name.
\ On x86-64 a row the captured runtime provides (src/habu/prims.f
\ EPREFIX-PROVIDED!) must also register its dispatch cell: without one its body
\ is an assembly body under the runtime's name, which a seeded build would run.
\ A seeded build's body is the stub alone, so its runtime must fill the cell.
\ COMPLETE-PROVIDED checks both after COMPLETE; an ARM64 record registers no cell.
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

: CHECK-ROW ( n -- )
   PRIM-SPEC:NAME$
   2dup BODY-ROW 0 < if NO-BODY else 2drop then ;

: CHECK-PROVIDED ( n [ n -- bool ] -- ) {: spec:n filled :}
   spec PRIM-SPEC:PREFIX-PROVIDED? 0= if exit then
   spec PRIM-SPEC:NAME$ {: name:ptr size:n :}
   name size BODY-ROW {: body:n :}
   body 0 < if name size NO-BODY then
   body DISPATCH {: cell:n :}
   cell 0 < if name size NO-DISPATCH then
   SEEDED? 0= if exit then
   cell filled execute 0= if name size UNFILLED then ;

public

: COMPLETE ( -- )
   PRIM-SPEC:COUNT 0 ?do i CHECK-ROW loop ;

: COMPLETE-PROVIDED ( [ n -- bool ] -- ) {: filled :}
   PRIM-SPEC:COUNT 0 ?do i filled CHECK-PROVIDED loop ;

;package
