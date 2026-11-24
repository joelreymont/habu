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
DYNAMIC-BUFFER ROWS n
DYNAMIC-BUFFER NAMES n
variable USED
variable NAME-BYTES

: ROW-FIELD ( n n -- ^n ) {: row:n field:n :}
   row 0 < row USED @ >= or if E-LAYOUT-BOUNDS throw then
   row ROW-CELLS * field + ROWS ;

: RESERVE ( n -- ) {: size:n :}
   USED @ 1+ ROW-CELLS * ROWS-RESERVE
   NAME-BYTES @ size + CELL 1- + CELL / NAMES-RESERVE ;

: OPEN-RECORD ( str -- ) {: name:^ size:n :}
   s" E-PRIM-TABLE" REJECTION:OPEN
   s" primitive `" REJECTION:EMIT-STR  name size REJECTION:EMIT-STR
   s" `" REJECTION:EMIT-STR ;

: STOP-RECORD ( str -- )
   REJECTION:HELP  REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: SPEC-MISSING ( str -- )
   OPEN-RECORD
   s"  has a backend body but no row in src/habu/prims.f" REJECTION:EMIT-STR
   s" add its row to src/habu/prims.f, or remove the body" STOP-RECORD ;

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
private
: WID ( n -- n ) 5 ROW-FIELD @ ;
: WID! ( n n -- ) 5 ROW-FIELD ! ;
public

\ The table is which primitives exist: every body registers under a name
\ src/habu/prims.f specifies; the row states the effect, the body the machine
\ code. SPEC-CHECK refuses a body with no row, COMPLETE a row no body answered.
: SPEC-CHECK ( str -- ) {: name:^ size:n :}
   name size PRIM-SPEC:FIND 0 < if name size SPEC-MISSING then ;

\ An engine helper is an engine-resident routine primitives reach by a direct
\ branch (a shared refusal, a bounds loop). HELPER-REGISTER records it as a
\ sealed system-private record over [start-addr, end-addr) stamped
\ OWNER-API-PRI-WID, hidden from raw searches (BSWL). DNAME folds the marker into
\ the baked name/flags cell.
: HELPER-REGISTER ( str n n -- )
   >LABEL swap >LABEL swap ADD OWNER-API-PRI-WID swap WID! ;

private

\ Whether a package row (src/habu/prims.f EPPRIM: ... ECLOSE-PRIVATE) states the
\ primitive's effect.
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

\ An owned primitive's seed row is in OWNER-API-PRI-WID, which no source lookup
\ searches, whatever wordlist its body passed.
: HELPER-WID ( n -- n ) {: idx:n :}
   idx NAME$ OWNED? if OWNER-API-PRI-WID exit then
   idx WID ;

: DNAME ( n -- n ) {: idx:n :}
   idx NAME-LEN  idx NAME$ IMM-BIT or
   idx HELPER-WID OWNER-API-PRI-WID = if DNAME-INT or then ;

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
   OPEN-RECORD
   s"  has a row in src/habu/prims.f but no body in this backend" REJECTION:EMIT-STR
   s" give it a body in the backend, or remove the row" STOP-RECORD ;

: CHECK-ROW ( n -- )
   PRIM-SPEC:NAME$
   2dup BODY-ROW 0 < if NO-BODY else 2drop then ;

: TWO-BODIES ( str -- )
   OPEN-RECORD
   s"  has two backend bodies; names fold case" REJECTION:EMIT-STR
   s" remove one body" STOP-RECORD ;

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
