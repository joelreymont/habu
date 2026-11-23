split-str
\ primitive-registry.f - build-side primitive metadata shared by every backend's
\ engine builder: checks each registered body's name against the specification
\ table, answers the seed dictionary's name and wid cells, and refuses a kept row
\ no body answered. Code positions arrive only as `label` values and no code is
\ emitted, so it loads without an instruction set. Names are offsets, so growing
\ either buffer cannot invalidate an earlier row.
require src/core/exit-status.f
require src/core/layout-buffer.f
require src/core/bytes.f
require src/core/roles.f
require src/habu/layout.f               \ OWNER-API-PRI-WID, DNAME-INT
require src/habu/prims.f                \ PRIM-SPEC, the specification table
require src/habu/wl.f                   \ WL:PRIMITIVES, a built-in row's wid

package ENGINE-PRIMS

6 constant ROW-CELLS
$7FFFFFFFFFFFFFFF constant MAX-N
DYNAMIC-BUFFER ROWS n
DYNAMIC-BUFFER NAMES n
variable USED
variable NAME-BYTES

: ROW-FIELD ( n n -- ^n ) {: row:n field:n :}
   row 0 < row USED @ >= or if E-LAYOUT-BOUNDS throw then
   row ROW-CELLS * field + ROWS ;

: RESERVE ( n -- ) {: size:n :}
   size 0 <= if s" prims: a primitive's name is empty" EXIT-STATUS:INTERNAL die then
   USED @ MAX-N CELL / ROW-CELLS / >= if
      s" prims: ROWS is full at MAX-N bytes" EXIT-STATUS:INTERNAL die
   then
   size MAX-N CELL 1- - NAME-BYTES @ - > if
      s" prims: NAMES is full at MAX-N bytes" EXIT-STATUS:INTERNAL die
   then
   USED @ 1+ ROW-CELLS * ROWS-RESERVE
   NAME-BYTES @ size + CELL 1- + CELL / NAMES-RESERVE ;

: SPEC-MISSING ( str -- )
   s" prims: primitive with no row in src/habu/prims.f: " type type cr
   s" prims: engine primitive absent from the specification table" EXIT-STATUS:INTERNAL die ;

public

: COUNT ( -- n ) USED @ ;
: RESET ( -- ) 0 USED ! 0 NAME-BYTES ! ;

: ADD ( str label label -- n ) {: name:^ size:n first:label last:label :}
   size RESERVE
   USED @ {: row:n :}
   row ROW-CELLS * ROWS {: dst:^ :}
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
: NAME$ ( n -- str ) {: row:n :}
   0 NAMES BYTE-VIEW row 3 ROW-FIELD @ + row NAME-LEN ;
: NAME-LABEL ( n -- label ) 4 ROW-FIELD @ >LABEL ;
: NAME-LABEL! ( label n -- ) swap LABEL>N swap 4 ROW-FIELD ! ;
: WID ( n -- n ) 5 ROW-FIELD @ ;
: WID! ( n n -- ) 5 ROW-FIELD ! ;

\ The table is which primitives exist: every body registers under a name
\ src/habu/prims.f specifies; the row states the effect, the body the machine
\ code. SPEC-CHECK refuses a body with no row, COMPLETE a row no body answered.
: SPEC-CHECK ( str -- ) {: name:^ size:n :}
   name size PRIM-SPEC:FIND 0 < if name size SPEC-MISSING then ;

\ A primitive no source may run or tick (DNAME-INT). The sentinel lives only in
\ this build-side registry: HELPER-WID answers the emitted record's wordlist.
-1 constant GLOBAL-INT-WID

\ An engine helper is an engine-resident routine primitives reach by a direct
\ branch (a shared refusal, a bounds loop). HELPER-REGISTER records it as a
\ sealed system-private record over [start-addr, end-addr) stamped
\ OWNER-API-PRI-WID, hidden from raw searches (BSWL). DNAME folds the marker into
\ the baked name/flags cell.
: HELPER-REGISTER ( str n n -- )
   >LABEL swap >LABEL swap ADD OWNER-API-PRI-WID swap WID! ;

private

\ Whether a package row (src/habu/prims.f EPPRIM: ... ECLOSE-PRIVATE) states the
\ primitive's effect. An internal primitive with one is its owner's to call, so
\ DNAME stamps it DNAME-OWNED (src/habu/layout.f).
: OWNED? ( str -- bool ) {: name:^ size:n :}
   PRIM-SPEC:COUNT 0 ?do
      i PRIM-SPEC:KIND@ PRIM-SPEC:K-PKG-PRIVATE = if
         i PRIM-SPEC:NAME$ name size CORE-STR= if unloop 0 0= exit then
      then
   loop
   0 0= 0= ;

\ DNAME-IMM for an immediate row.
: IMM-BIT ( str -- n )
   PRIM-SPEC:FIND {: row:n :}
   row 0 < if 0 exit then
   row PRIM-SPEC:IMMEDIATE? if DNAME-IMM else 0 then ;

public

: DNAME ( n -- n ) {: idx:n :}
   idx NAME-LEN  idx NAME$ IMM-BIT or
   idx WID {: wid:n :}
   wid OWNER-API-PRI-WID =  wid GLOBAL-INT-WID = or if DNAME-INT or then
   wid GLOBAL-INT-WID = if
      idx NAME$ OWNED? if DNAME-OWNED or then
   then ;

\ An owned primitive's seed row is in OWNER-API-PRI-WID, which no source lookup
\ searches, whatever wordlist its body passed.
: HELPER-WID ( n -- n ) {: idx:n :}
   idx NAME$ OWNED? if OWNER-API-PRI-WID exit then
   idx WID dup GLOBAL-INT-WID = if drop WL:PRIMITIVES then ;

\ COMPLETE, run after the last body, refuses a row this backend never answered:
\ an effect checked callers believe, with no code for the name.
private

\ The registered body of the name, or -1.
: BODY-ROW ( str -- n ) {: name:^ size:n :}
   USED @ 0 ?do
      i NAME$ name size CORE-STR= if i unloop exit then
   loop
   -1 ;

: NO-BODY ( str -- )
   s" prims: no backend body for specified primitive " type type cr
   s" prims: specification row without a backend body" EXIT-STATUS:INTERNAL die ;

: CHECK-ROW ( n -- )
   PRIM-SPEC:NAME$
   2dup BODY-ROW 0 < if NO-BODY else 2drop then ;

: TWO-BODIES ( str -- )
   s" prims: two backend bodies for primitive " type type cr
   s" prims: two bodies register one primitive name" EXIT-STATUS:INTERNAL die ;

\ Names fold case: rows that differ only in case register one name.
: CHECK-UNIQUE ( n -- ) {: row:n :}
   USED @ row 1+ ?do
      i NAME$ row NAME$ CORE-STR=CI if row NAME$ TWO-BODIES then
   loop ;

public

: COMPLETE ( -- )
   PRIM-SPEC:COUNT 0 ?do i CHECK-ROW loop ;

\ Every registered name is distinct, because the link maps rows by name.
: DISTINCT ( -- )
   USED @ 0 ?do i CHECK-UNIQUE loop ;

;package
