\ Loaded after the native build's real replacement-checker handoff. No cached
\ require of checker.f can substitute the host's implementation for this one.
package DEFER-HISTORY-TEST

: ASSERT ( bool -- )
   if exit then s" defer history assertion failed" 76 die ;

: =ASSERT ( n n -- ) = ASSERT ;

\ The checker stores and rollback operations are private. These test-only
\ boundaries expose their state without changing the implementation under test.
: END@ ( -- n ) DFER-END @ ;
: ROW-BYTES ( -- n ) DFER-REC ;
TRUSTED: STATE@ ( n -- bool ) DFER-FIND-SYM ;
TRUSTED: STATE! ( n bool -- ) DFER-ADD-SYM ;
TRUSTED: INTERN ( ptr u8 n -- n ) {: a:ptr u:n :}
   s" " SYM-GLOBAL a u SYM-INTERN ;
TRUSTED: ACTIVE ( ptr u8 n -- n ) CHECKER-FIND-ACTIVE-SYM ;
TRUSTED: MIN-IN ( ptr u8 n -- n ) SIG-MIN-IN ;
: SCOPE+ ( -- ) CHECKER-SCOPE-START ;
: SCOPE- ( -- ) CHECKER-SCOPE-DONE ;
TRUSTED: CANDIDATE+ ( -- ) CHECK-CANDIDATE-START ;
TRUSTED: CANDIDATE- ( -- ) 0 CHECK-CANDIDATE-DONE drop ;
TRUSTED: MARK ( -- ) CHECKER-BOUND:MARK ;
TRUSTED: REWIND ( -- ) CHECKER-BOUND:REWIND ;
: TERM? ( -- bool ) DFERS DFER-END @ + @ 0= ;
TRUSTED: ROW-CHANGED? ( ptr n -- bool ) {: row:ptr :}
   0 0= 0= DFERS begin dup row < while
      dup DFER-SYM@ row DFER-SYM@ = if swap drop dup DFER-FLAG@ swap then
      DFER-NEXT
   repeat drop
   row DFER-FLAG@ xor ;
: TRANSITIONS? ( -- bool )
   DFER-END @ DFER-REC / 0 ?do
      DFERS i DFER-REC * + ROW-CHANGED? 0= if 0 0= 0= unloop exit then
   loop 0 0= ;
TRUSTED: TRANSFER-COUNTS ( -- n n )
   0 0 SYM-N @ 1 ?do
      i USIG-NEWEST dup 0= 0= if
         1- E-PTR ER.ACTIVE @ if
            i DFER-FIND-SYM if 1+ else swap 1+ swap then
         then
      else drop then
   loop ;

\ evaluate is the public define/undefine boundary; its dynamic effect is known
\ only for the source strings supplied by these cases.
TRUSTED: EV ( ptr u8 n -- ) evaluate ;
TRUSTED: EV-N ( ptr u8 n -- n ) evaluate ;

variable MARK0
variable MARK1
variable KEY

: SAME-STATE ( -- )
   s" DH-STATE" INTERN KEY !
   KEY @ STATE@ 0= ASSERT
   END@ MARK0 !
   \ More calls than the whole fixed table can hold must cost no history.
   5000 0 do KEY @ 0 0= 0= STATE! loop
   0 0 0= 0= STATE!                  \ an absent symbol is false too
   END@ MARK0 @ =ASSERT
   KEY @ 0 0= STATE!
   END@ MARK0 @ ROW-BYTES + =ASSERT
   KEY @ STATE@ ASSERT
   END@ MARK1 !
   5000 0 do KEY @ 0 0= STATE! loop
   END@ MARK1 @ =ASSERT
   TERM? ASSERT ;

: SCOPE-ROLLBACK ( -- )
   \ Prime true, then false inside a scope. The first operation after rollback
   \ is another false store: it must invalidate the cached false and append.
   KEY @ STATE@ ASSERT
   END@ MARK0 !
   SCOPE+
   KEY @ 0 0= 0= STATE!
   KEY @ STATE@ 0= ASSERT
   END@ MARK0 @ ROW-BYTES + =ASSERT
   SCOPE-
   END@ MARK0 @ =ASSERT
   KEY @ 0 0= 0= STATE!
   END@ MARK0 @ ROW-BYTES + =ASSERT
   KEY @ STATE@ 0= ASSERT
   TERM? ASSERT ;

: CANDIDATE-ROLLBACK ( -- )
   \ The inverse case: a cached true from a discarded candidate must not make
   \ a true store appear unchanged when the effective pre-candidate state is false.
   END@ MARK0 !
   CANDIDATE+
   KEY @ 0 0= STATE!
   KEY @ STATE@ ASSERT
   CANDIDATE-
   END@ MARK0 @ =ASSERT
   KEY @ 0 0= STATE!
   END@ MARK0 @ ROW-BYTES + =ASSERT
   KEY @ STATE@ ASSERT
   TERM? ASSERT ;

: MARKER-ROLLBACK ( -- )
   \ The prefix mark uses the same saved offsets without opening a scope.
   END@ MARK0 ! MARK
   KEY @ 0 0= 0= STATE!
   KEY @ STATE@ 0= ASSERT
   REWIND
   END@ MARK0 @ =ASSERT
   KEY @ STATE@ ASSERT
   5000 0 do KEY @ 0 0= STATE! loop
   END@ MARK0 @ =ASSERT
   TERM? ASSERT ;

: ORDINARY-LIFETIME ( -- )
   END@ MARK0 !
   5000 0 do
      s" : DH-ORDINARY ( n -- n ) 1+ ;" EV
      s" 41 DH-ORDINARY" EV-N 42 =ASSERT
      s" undefine DH-ORDINARY" EV
   loop
   END@ MARK0 @ =ASSERT
   s" DH-ORDINARY" MIN-IN -1 =ASSERT
   TERM? ASSERT ;

: DEFER-LIFETIME ( -- )
   END@ MARK0 !
   s" defer DH-HOOK ( n -- n )" EV
   END@ MARK0 @ ROW-BYTES + =ASSERT
   s" DH-HOOK" ACTIVE KEY !
   KEY @ STATE@ ASSERT
   s" DH-HOOK" MIN-IN 1 =ASSERT
   s" : DH-INSTALL ( -- ) [: 1+ ;] is DH-HOOK ; DH-INSTALL" EV
   s" : DH-CALL ( n -- n ) DH-HOOK ; 41 DH-CALL" EV-N 42 =ASSERT
   s" DH-BAD ( -- ) [: 0 0= ;] is DH-HOOK" CHECK-CANDIDATE! 0 =ASSERT
   s" undefine DH-CALL undefine DH-INSTALL undefine DH-HOOK" EV
   END@ MARK0 @ ROW-BYTES 2 * + =ASSERT
   KEY @ STATE@ 0= ASSERT
   s" : DH-HOOK ( n n -- n ) + ; 20 22 DH-HOOK" EV-N 42 =ASSERT
   s" DH-HOOK" MIN-IN 2 =ASSERT
   s" DH-BAD-PLAIN ( -- ) [: + ;] is DH-HOOK" CHECK-CANDIDATE! 0 =ASSERT
   s" undefine DH-HOOK" EV
   END@ MARK0 @ ROW-BYTES 2 * + =ASSERT
   TERM? ASSERT ;

: RUN ( -- )
   \ Thousands of ordinary checked rows really crossed into this fresh owner.
   \ They must not manufacture false defer history; real defers still arrive.
   TRANSFER-COUNTS 0 > ASSERT 1000 > ASSERT
   TRANSITIONS? 0= if s" defer history contains unchanged state" 76 die then
   SAME-STATE
   SCOPE-ROLLBACK
   CANDIDATE-ROLLBACK
   MARKER-ROLLBACK
   ORDINARY-LIFETIME
   DEFER-LIFETIME
   s" defer history: ok" type cr ;

RUN
;package
