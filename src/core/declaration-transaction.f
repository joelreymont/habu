\ declaration-transaction.f - reusable ordered transaction coordinator.
\
\ One coordinator instance owns a participant table and transaction telemetry.
\ Callers provide the instance record, its initial row arena, an allocator, and
\ a diagnostic callback.  Registration is ordered and becomes immutable at
\ SEAL.  RUN retains every participant savepoint through body, prepare, and
\ reversible commit; rollback and finalization run in reverse order.
\
\ Participant cleanup failures never replace the primary error.  They poison
\ only that coordinator instance, so no later transaction can run against a
\ participant whose private savepoint may still be live.

package DECLARATION-TRANSACTION

public

7165 constant E-PARTICIPANT-DUP
7166 constant E-REGISTRATION-ACTIVE
7167 constant E-TRANSACTION-POISONED
7168 constant E-PARTICIPANT-CAPACITY
7170 constant E-REGISTRATION-SEALED
7171 constant E-PARTICIPANT-DEPTH

1 constant PHASE-SNAPSHOT
2 constant PHASE-BODY
3 constant PHASE-PREPARE
4 constant PHASE-COMMIT
5 constant PHASE-ROLLBACK
6 constant PHASE-FINALIZE

7 constant ROW-CELLS
13 constant STATE-CELLS

private

ROW-CELLS cells constant ROW-BYTES
$7FFFFFFFFFFFFFFF ROW-BYTES / constant MAX-ROWS

: ROWS>BYTES ( n -- n )
   dup 0 < over MAX-ROWS > or if E-PARTICIPANT-CAPACITY throw then
   ROW-BYTES * ;

: NEXT-CAP ( n -- n )
   dup MAX-ROWS >= if E-PARTICIPANT-CAPACITY throw then
   dup MAX-ROWS 2 / <= if 2 * exit then
   drop MAX-ROWS ;

0 cells constant ROW.ID-OFF
1 cells constant ROW.ORDER-OFF
2 cells constant ROW.SNAPSHOT-OFF
3 cells constant ROW.PREPARE-OFF
4 cells constant ROW.COMMIT-OFF
5 cells constant ROW.ROLLBACK-OFF
6 cells constant ROW.FINALIZE-OFF

0 cells constant ST.TABLE-OFF
1 cells constant ST.CAP-OFF
2 cells constant ST.N-OFF
3 cells constant ST.DEPTH-OFF
4 cells constant ST.SEALED-OFF
5 cells constant ST.POISON-OFF
6 cells constant ST.CURRENT-PHASE-OFF
7 cells constant ST.CURRENT-PARTICIPANT-OFF
8 cells constant ST.FAILURE-PHASE-OFF
9 cells constant ST.FAILURE-PARTICIPANT-OFF
10 cells constant ST.CLEANUP-PARTICIPANT-OFF
11 cells constant ST.ALLOCATOR-OFF
12 cells constant ST.DIAGNOSTIC-OFF

: ST.TABLE ( ptr a -- ptr ptr a ) ST.TABLE-OFF ptr-field ;
: ST.CAP ( ptr a -- ptr a ) ST.CAP-OFF + ;
: ST.N ( ptr a -- ptr a ) ST.N-OFF + ;
: ST.DEPTH ( ptr a -- ptr a ) ST.DEPTH-OFF + ;
: ST.SEALED ( ptr a -- ptr a ) ST.SEALED-OFF + ;
: ST.POISON ( ptr a -- ptr a ) ST.POISON-OFF + ;
: ST.CURRENT-PHASE ( ptr a -- ptr a ) ST.CURRENT-PHASE-OFF + ;
: ST.CURRENT-PARTICIPANT ( ptr a -- ptr a ) ST.CURRENT-PARTICIPANT-OFF + ;
: ST.FAILURE-PHASE ( ptr a -- ptr a ) ST.FAILURE-PHASE-OFF + ;
: ST.FAILURE-PARTICIPANT ( ptr a -- ptr a ) ST.FAILURE-PARTICIPANT-OFF + ;
: ST.CLEANUP-PARTICIPANT ( ptr a -- ptr a ) ST.CLEANUP-PARTICIPANT-OFF + ;
TRUSTED: ST.ALLOCATOR ( ptr a -- ptr [ ptr a n n -- ptr a ] )
   ST.ALLOCATOR-OFF + ;
TRUSTED: ST.DIAGNOSTIC ( ptr a -- ptr [ n n -- ] )
   ST.DIAGNOSTIC-OFF + ;

: TABLE@ ( ptr a -- ptr a ) ST.TABLE @ ;
: CAP@ ( ptr a -- n ) ST.CAP @ ;
: N@ ( ptr a -- n ) ST.N @ ;
: DEPTH@ ( ptr a -- n ) ST.DEPTH @ ;
: SEALED@ ( ptr a -- n ) ST.SEALED @ ;
: POISON@ ( ptr a -- n ) ST.POISON @ ;
: ALLOCATOR@ ( ptr a -- [ ptr a n n -- ptr a ] ) ST.ALLOCATOR @ ;
: DIAGNOSTIC@ ( ptr a -- [ n n -- ] ) ST.DIAGNOSTIC @ ;

: ROW ( ptr a n -- ptr a ) {: state:ptr idx:n :}
   state TABLE@ idx ROW-CELLS * cells + ;

: ROW.ID ( ptr a -- ptr a ) ROW.ID-OFF + ;
: ROW.ORDER ( ptr a -- ptr a ) ROW.ORDER-OFF + ;
TRUSTED: ROW.SNAPSHOT ( ptr a -- ptr [ n -- n ] ) ROW.SNAPSHOT-OFF + ;
TRUSTED: ROW.PREPARE ( ptr a -- ptr [ n -- n ] ) ROW.PREPARE-OFF + ;
TRUSTED: ROW.COMMIT ( ptr a -- ptr [ n -- n ] ) ROW.COMMIT-OFF + ;
TRUSTED: ROW.ROLLBACK ( ptr a -- ptr [ n -- n ] ) ROW.ROLLBACK-OFF + ;
TRUSTED: ROW.FINALIZE ( ptr a -- ptr [ n -- n ] ) ROW.FINALIZE-OFF + ;

: ID@ ( ptr a n -- n ) ROW ROW.ID @ ;
: ORDER@ ( ptr a n -- n ) ROW ROW.ORDER @ ;
: SNAPSHOT@ ( ptr a n -- [ n -- n ] ) ROW ROW.SNAPSHOT @ ;
: PREPARE@ ( ptr a n -- [ n -- n ] ) ROW ROW.PREPARE @ ;
: COMMIT@ ( ptr a n -- [ n -- n ] ) ROW ROW.COMMIT @ ;
: ROLLBACK@ ( ptr a n -- [ n -- n ] ) ROW ROW.ROLLBACK @ ;
: FINALIZE@ ( ptr a n -- [ n -- n ] ) ROW ROW.FINALIZE @ ;

: ID! ( n ptr a n -- ) ROW ROW.ID ! ;
: ORDER! ( n ptr a n -- ) ROW ROW.ORDER ! ;
: SNAPSHOT! ( [ n -- n ] ptr a n -- ) ROW ROW.SNAPSHOT ! ;
: PREPARE! ( [ n -- n ] ptr a n -- ) ROW ROW.PREPARE ! ;
: COMMIT! ( [ n -- n ] ptr a n -- ) ROW ROW.COMMIT ! ;
: ROLLBACK! ( [ n -- n ] ptr a n -- ) ROW ROW.ROLLBACK ! ;
: FINALIZE! ( [ n -- n ] ptr a n -- ) ROW ROW.FINALIZE ! ;

TRUSTED: TABLE-ARENA-GROW ( ptr a n n -- ptr a ) ARENA-BYTES-GROW ;

: DEFAULT-ALLOCATE ( ptr a n n -- ptr a )
   TABLE-ARENA-GROW ;

: NO-DIAGNOSTIC ( n n -- )
   2drop ;

: INIT-STATE ( ptr a ptr a n [ ptr a n n -- ptr a ] [ n n -- ] -- )
   {: state:ptr table:ptr cap:n allocator diagnostic :} \ typed-local-lint: allow-bare-local
   cap 1 < cap MAX-ROWS > or if E-PARTICIPANT-CAPACITY throw then
   table state ST.TABLE !
   cap state ST.CAP !
   0 state ST.N !
   0 state ST.DEPTH !
   0 state ST.SEALED !
   0 state ST.POISON !
   0 state ST.CURRENT-PHASE !
   -1 state ST.CURRENT-PARTICIPANT !
   0 state ST.FAILURE-PHASE !
   -1 state ST.FAILURE-PARTICIPANT !
   -1 state ST.CLEANUP-PARTICIPANT !
   allocator state ST.ALLOCATOR !
   diagnostic state ST.DIAGNOSTIC ! ;

: DUPLICATE? ( ptr a n -- bool ) {: state:ptr id:n :}
   0
   BEGIN dup state N@ < WHILE
      state over ID@ id = IF drop 0 0= EXIT THEN
      1 +
   REPEAT
   drop 0 0= 0= ;

: REQUIRE-REGISTRATION ( ptr a n -- ) {: state:ptr id:n :}
   state DEPTH@ 0 <> IF E-REGISTRATION-ACTIVE throw THEN
   state SEALED@ 0 <> IF E-REGISTRATION-SEALED throw THEN
   state N@ MAX-ROWS >= if E-PARTICIPANT-CAPACITY throw then
   state id DUPLICATE? IF E-PARTICIPANT-DUP throw THEN ;

: INSERT-AT ( ptr a n -- n ) {: state:ptr order:n :}
   0
   BEGIN dup state N@ < WHILE
      state over ORDER@ order > IF EXIT THEN
      1 +
   REPEAT ;

: MOVE-ROW ( ptr a n n -- ) {: state:ptr from:n to:n :}
   state from ID@ state to ID!
   state from ORDER@ state to ORDER!
   state from SNAPSHOT@ state to SNAPSHOT!
   state from PREPARE@ state to PREPARE!
   state from COMMIT@ state to COMMIT!
   state from ROLLBACK@ state to ROLLBACK!
   state from FINALIZE@ state to FINALIZE! ;

: GROW-TABLE ( ptr a -- ) {: state:ptr :}
   state CAP@ {: oldcap:n :}
   oldcap NEXT-CAP {: newcap:n :}
   state TABLE@ oldcap ROWS>BYTES newcap ROWS>BYTES
      state ALLOCATOR@ execute {: grown:ptr :}
   grown state ST.TABLE !
   newcap state ST.CAP ! ;

: ENSURE-ROOM ( ptr a -- ) {: state:ptr :}
   state N@ state CAP@ < IF EXIT THEN
   state GROW-TABLE ;

: OPEN-SLOT ( ptr a n -- n ) {: state:ptr order:n :}
   state ENSURE-ROOM
   state order INSERT-AT {: idx:n :}
   state N@
   BEGIN dup idx > WHILE
      dup {: to:n :}
      state to 1 - to MOVE-ROW
      1 -
   REPEAT
   drop
   idx ;

: REGISTER-ROW
   ( ptr a n n [ n -- n ] [ n -- n ] [ n -- n ] [ n -- n ] [ n -- n ] -- )
   {: state:ptr id:n order:n snapshot prepare commit rollback finalize :} \ typed-local-lint: allow-bare-local
   state id REQUIRE-REGISTRATION
   state order OPEN-SLOT {: idx:n :}
   id state idx ID!
   order state idx ORDER!
   snapshot state idx SNAPSHOT!
   prepare state idx PREPARE!
   commit state idx COMMIT!
   rollback state idx ROLLBACK!
   finalize state idx FINALIZE!
   state N@ 1 + state ST.N ! ;

: ENTER-PHASE ( ptr a n n -- ) {: state:ptr phase:n idx:n :}
   phase state ST.CURRENT-PHASE !
   state idx ID@ state ST.CURRENT-PARTICIPANT ! ;

: POISON ( ptr a n -- ) {: state:ptr code:n :}
   state POISON@ 0= IF code state ST.POISON ! THEN ;

: CALL-PARTICIPANT ( ptr a n n [ n -- n ] -- n )
   {: state:ptr phase:n idx:n callback :} \ typed-local-lint: allow-bare-local
   state phase idx ENTER-PHASE
   state DEPTH@ {: expected:n :}
   expected callback catch {: actual:n code:n :}
   code 0 <> IF code EXIT THEN
   actual expected <> IF
      state E-PARTICIPANT-DEPTH POISON
      E-PARTICIPANT-DEPTH EXIT
   THEN
   0 ;

: SNAPSHOT-ONE ( ptr a n -- n ) {: state:ptr idx:n :}
   state PHASE-SNAPSHOT idx state idx SNAPSHOT@ CALL-PARTICIPANT ;

: SNAPSHOT-ALL ( ptr a -- n n ) {: state:ptr :}
   0
   BEGIN dup state N@ < WHILE
      dup state swap SNAPSHOT-ONE
      dup 0 <> IF swap 1 + swap EXIT THEN
      drop 1 +
   REPEAT
   0 ;

: FORWARD-ONE ( ptr a n n -- n ) {: state:ptr phase:n idx:n :}
   phase PHASE-PREPARE = IF
      state phase idx state idx PREPARE@ CALL-PARTICIPANT EXIT
   THEN
   state phase idx state idx COMMIT@ CALL-PARTICIPANT ;

: RUN-FORWARD ( ptr a n -- n ) {: state:ptr phase:n :}
   0
   BEGIN dup state N@ < WHILE
      dup state phase rot FORWARD-ONE
      dup 0 <> IF nip EXIT THEN
      drop 1 +
   REPEAT
   drop 0 ;

: FIRST-ERROR ( n n -- n ) {: prior:n code:n :}
   prior 0 <> IF prior EXIT THEN
   code ;

: REMEMBER-CLEANUP ( ptr a n n -- n ) {: state:ptr prior:n code:n :}
   prior 0= code 0 <> and IF
      state ST.CURRENT-PARTICIPANT @ state ST.CLEANUP-PARTICIPANT !
   THEN
   code 0 <> IF state code POISON THEN
   prior code FIRST-ERROR ;

: ROLLBACK-ONE ( ptr a n -- n ) {: state:ptr idx:n :}
   state PHASE-ROLLBACK idx state idx ROLLBACK@ CALL-PARTICIPANT ;

: ROLLBACK-ALL ( ptr a n -- n ) {: state:ptr active:n :}
   0
   active 0 ?do
      state active 1 - i - ROLLBACK-ONE
      state -rot REMEMBER-CLEANUP
   loop ;

: FINALIZE-ONE ( ptr a n -- n ) {: state:ptr idx:n :}
   state PHASE-FINALIZE idx state idx FINALIZE@ CALL-PARTICIPANT ;

: FINALIZE-ALL ( ptr a -- n ) {: state:ptr :}
   0
   state N@ 0 ?do
      state state N@ 1 - i - FINALIZE-ONE
      state -rot REMEMBER-CLEANUP
   loop ;

: RUN-DIAGNOSTIC ( n n [ n n -- ] -- n )
   execute 0 ;

TRUSTED: CATCH-DIAGNOSTIC ( n n [ n n -- ] -- )
   [: RUN-DIAGNOSTIC ;] catch
   dup 0 <> IF 2drop 2drop EXIT THEN
   2drop ;

: REPORT-FAILURE ( ptr a n n -- ) {: state:ptr primary:n cleanup:n :}
   primary cleanup state DIAGNOSTIC@ CATCH-DIAGNOSTIC ;

: LEAVE-TRANSACTION ( ptr a -- ) {: state:ptr :}
   state DEPTH@ 1 - state ST.DEPTH ! ;

: FAIL ( ptr a n n -- ) {: state:ptr active:n primary:n :}
   state ST.CURRENT-PHASE @ state ST.FAILURE-PHASE !
   state ST.CURRENT-PARTICIPANT @ state ST.FAILURE-PARTICIPANT !
   -1 state ST.CLEANUP-PARTICIPANT !
   state active ROLLBACK-ALL {: cleanup:n :}
   state LEAVE-TRANSACTION
   state primary cleanup REPORT-FAILURE
   primary throw ;

: RUN-TRANSACTION ( ptr a [ -- ] -- )
   {: state:ptr body :} \ typed-local-lint: allow-bare-local
   state POISON@ 0 <> IF E-TRANSACTION-POISONED throw THEN
   state DEPTH@ 1 + state ST.DEPTH !
   state SNAPSHOT-ALL {: active:n code:n :}
   code 0 <> IF state active code FAIL THEN
   PHASE-BODY state ST.CURRENT-PHASE !
   -1 state ST.CURRENT-PARTICIPANT !
   body catch {: body-code:n :}
   body-code 0 <> IF state active body-code FAIL THEN
   state PHASE-PREPARE RUN-FORWARD {: prepare-code:n :}
   prepare-code 0 <> IF state active prepare-code FAIL THEN
   state PHASE-COMMIT RUN-FORWARD {: commit-code:n :}
   commit-code 0 <> IF state active commit-code FAIL THEN
   -1 state ST.CLEANUP-PARTICIPANT !
   state FINALIZE-ALL {: cleanup:n :}
   state LEAVE-TRANSACTION
   cleanup 0 <> IF
      state 0 cleanup REPORT-FAILURE
      cleanup throw
   THEN ;

public

: INIT ( ptr a ptr a n [ ptr a n n -- ptr a ] [ n n -- ] -- )
   INIT-STATE ;

: REGISTER
   ( ptr a n n [ n -- n ] [ n -- n ] [ n -- n ] [ n -- n ] [ n -- n ] -- )
   REGISTER-ROW ;

: SEAL ( ptr a -- )
   -1 swap ST.SEALED ! ;

: RUN ( ptr a [ -- ] -- )
   RUN-TRANSACTION ;

: COUNT ( ptr a -- n ) N@ ;
: CAPACITY ( ptr a -- n ) CAP@ ;
: TABLE ( ptr a -- ptr a ) TABLE@ ;
: DEPTH ( ptr a -- n ) DEPTH@ ;
: SEALED? ( ptr a -- bool ) SEALED@ 0 <> ;
: POISONED? ( ptr a -- bool ) POISON@ 0 <> ;
: LAST-FAILURE-PHASE ( ptr a -- n ) ST.FAILURE-PHASE @ ;
: LAST-FAILURE-PARTICIPANT ( ptr a -- n ) ST.FAILURE-PARTICIPANT @ ;
: LAST-CLEANUP-PARTICIPANT ( ptr a -- n ) ST.CLEANUP-PARTICIPANT @ ;

: DEFAULT-ALLOCATOR ( ptr a n n -- ptr a ) DEFAULT-ALLOCATE ;
: DEFAULT-DIAGNOSTIC ( n n -- ) NO-DIAGNOSTIC ;

;package
