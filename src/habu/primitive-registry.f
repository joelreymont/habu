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
require src/habu/wl.f                   \ WL:PRIMITIVES, a built-in row's wid

package ENGINE-PRIMS

6 constant ROW-CELLS
$7FFFFFFFFFFFFFFF constant MAX-N
\ The generated storage accessors' size and index refusals: src/core/
\ layout-buffer.f's codes, which this file requires.
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

: ADD ( ptr u8 n label label -- n ) {: name:ptr size:n first:label last:label :}
   size RESERVE
   USED @ {: row:n :}
   row ROW-CELLS * ROWS {: dst:ptr :}
   first LABEL>N dst ! last LABEL>N dst cell+ !
   size dst 2 cells + ! NAME-BYTES @ dst 3 cells + !
   -1 dst 4 cells + ! WL:PRIMITIVES dst 5 cells + !
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

\ The table is which primitives exist: every body registers under a name
\ src/habu/prims.f specifies; the row states the effect, the body the machine
\ code. SPEC-CHECK refuses a body with no row, COMPLETE a row no body answered.
: SPEC-CHECK ( ptr u8 n -- ) {: name:ptr size:n :}
   name size PRIM-SPEC:FIND 0 < if name size SPEC-MISSING then ;

\ A primitive whose dictionary record is globally searchable but cannot be
\ executed or ticked by ordinary source. The sentinel lives only in this
\ build-side registry; the emitted record carries WL:PRIMITIVES and DNAME-INT,
\ and DNAME-OWNED as well when a package row types the primitive (OWNED?): its
\ owner's checked callers compile.
-1 constant GLOBAL-INT-WID

\ An engine helper is an engine-resident routine primitives reach by a direct
\ branch (a shared refusal, a bounds loop). HELPER-REGISTER records it as a
\ sealed system-private record over [start-addr, end-addr) stamped
\ OWNER-API-PRI-WID, hidden from raw searches (BSWL). DNAME folds the marker into
\ the baked name/flags cell.
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
   WID dup GLOBAL-INT-WID = if drop WL:PRIMITIVES then ;

\ COMPLETE, run after the last body, refuses a row this backend never answered:
\ an effect checked callers believe, with no code for the name.
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

: CHECK-ROW ( n -- )
   PRIM-SPEC:NAME$
   2dup BODY-ROW 0 < if NO-BODY else 2drop then ;

public

: COMPLETE ( -- )
   PRIM-SPEC:COUNT 0 ?do i CHECK-ROW loop ;

;package
