\ The parent saves two application images and runs the later phases in those
\ restored processes. Each phase asks the active checker what source can do.
package CONTROL-CAPTURE-SUBJECT
public

: CC-MAKE ( n -- ) create , does> ( -- ptr n ) ;
8 CC-MAKE CC-CELL

private

variable SAVED-CREATES
variable TEST-CASE
: STAGE ( n -- ) TEST-CASE ! ;

: ASSERT ( bool -- )
   if exit then
   s" control capture case " type TEST-CASE @ . cr
   s" control capture assertion failed" 79 die ;
: EQ! ( n n -- ) = ASSERT ;

: CREATED ( -- n )
   s" CONTROL-CAPTURE-SUBJECT:CC-MAKE" CHECKER-FIND-ACTIVE-SYM CHECKER-CREATES-SYM? ;
: RECORD-CREATED ( ptr u8 n -- bool )
   s" CONTROL-CAPTURE-SUBJECT:CC-MAKE" CHECKER-FIND-ACTIVE-SYM CHECKER-RECORD-CREATED ;
: THROW-STATE! ( n -- ) s" throw" rot NORET-ADD ;
: CLEAR-MAKER ( -- ) s" CONTROL-CAPTURE-SUBJECT:CC-MAKE" CHECKER-UNDEFINE ;
TRUSTED: RESET-SOURCE ( -- ) CHECKER-RESET-SOURCE ;
: SCOPE+ ( -- ) CHECKER-SCOPE-START ;
: SCOPE- ( -- ) CHECKER-SCOPE-DONE ;
: BAD-DEF ( -- ) s" : CC-FAILED ( n -- n ) drop ;" evaluate-closed ;

\ The arm loses a value. Only a call known to end the path can certify it.
: DEAD-JOIN? ( -- bool )
   s" CC-JOIN ( n n -- n ) 0 = if drop 5 throw then" CHECK-CANDIDATE! 0<> ;

: FLAGS! ( n n -- ) {: dead:n thrown:n :}
   s" throw" CTL-FLAGS {: flags:n :}
   flags CTL-DEAD and dead EQ!
   flags CTL-THROW and thrown EQ! ;

: BOUNDARY ( -- )
   CTL-DEAD 0 FLAGS!
   DEAD-JOIN? ASSERT
   CREATED SAVED-CREATES @ EQ!
   s" CC-BOUND-CREATED" RECORD-CREATED ASSERT ;

public

: PREPARE ( -- )
   1 STAGE
   CTL-DEAD CTL-THROW FLAGS!        \ the primitive checkpoint
   2 STAGE
   CC-CELL @ 8 EQ!                 \ a real checked DOES contract
   3 STAGE
   CREATED dup 0 > ASSERT SAVED-CREATES !
   CTL-DEAD THROW-STATE!
   CHECKER-BOUND:MARK              \ same primitive symbol, different boundary state
   4 STAGE
   BOUNDARY
   0 THROW-STATE!
   CLEAR-MAKER                     \ the current checkpoint retracts both facts
   5 STAGE
   0 0 FLAGS!
   DEAD-JOIN? 0= ASSERT
   CREATED 0 EQ!
   s" CC-NOT-CREATED" RECORD-CREATED 0= ASSERT
   6 STAGE
   0 0 FLAGS!
   CREATED 0 EQ!
   CTL-DEAD CTL-THROW or THROW-STATE!
   0 THROW-STATE!
   7 STAGE
   0 0 FLAGS!
   DEAD-JOIN? 0= ASSERT
   s" control capture: prepared" type cr ;

: RECAPTURE ( -- )
   0 0 FLAGS!                      \ first saved image's current state
   CREATED 0 EQ!
   CHECKER-BOUND:REWIND
   BOUNDARY
   SCOPE+
   0 THROW-STATE!
   DEAD-JOIN? 0= ASSERT
   SCOPE-
   BOUNDARY
   ['] BAD-DEF catch 0<> ASSERT
   s" CC-FAILED" SIG-MIN-IN -1 EQ!
   BOUNDARY
   0 THROW-STATE!
   0 0 FLAGS!
   CREATED SAVED-CREATES @ EQ!
   s" control capture: recaptured" type cr ;

: REFUSE-IN-SCOPE ( -- )
   SCOPE+
   0 THROW-STATE! ;

: FINAL ( -- )
   0 0 FLAGS!                      \ second saved image's current state
   CREATED SAVED-CREATES @ EQ!
   s" CC-FINAL-CREATED" RECORD-CREATED ASSERT
   CHECKER-BOUND:REWIND
   BOUNDARY
   RESET-SOURCE
   CTL-DEAD CTL-THROW FLAGS!      \ primitive prefix survived both captures
   CREATED 0 EQ!
   s" control capture: restored" type cr ;

;package

CONTROL-CAPTURE-SUBJECT:PREPARE
