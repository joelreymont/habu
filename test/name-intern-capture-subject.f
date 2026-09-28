\ One spelling belongs to four distinct declarations. The saved images keep
\ their effects and source order while later scopes rewind symbol text.
package NI-ALPHA
private
: NiShArEd ( n -- n ) 1+ ;
public
: RUN ( n -- n ) NISHARED ;
;package

package NI-BETA
public
: NISHARED ( -- n ) 42 ;
: RUN ( -- n ) NISHARED ;
;package

package NI-GAMMA
private
: NISHARED ( n n -- n ) + ;
public
: RUN ( n n -- n ) NISHARED ;
;package

package NAME-INTERN-SUBJECT
private

variable STAGE
: STEP ( n -- ) STAGE ! ;
: ASSERT ( bool -- )
   if exit then
   s" name intern stage " type STAGE @ . cr
   s" name intern assertion failed" 79 die ;
: EQ! ( n n -- ) = ASSERT ;

TYPED-VARIABLE SRC-A ptr u8
variable SRC-U
: VERIFY-ACT ( -- ) SRC-A @ SRC-U @ VERIFY:SOURCE-BUF ;
: VERDICT ( ptr u8 n -- n ) {: a:ptr u:n :}
   a SRC-A ! u SRC-U !
   [: VERIFY-ACT ;] catch ;

: PROGRAM$ ( -- ptr u8 n )
   S\" package NI-ALPHA\nprivate\n: NiShArEd ( n -- n ) 1+ ;\npublic\n: RUN ( n -- n ) NISHARED ;\n;package\npackage NI-BETA\npublic\n: NISHARED ( -- n ) 42 ;\n: RUN ( -- n ) NISHARED ;\n;package\npackage NI-GAMMA\nprivate\n: NISHARED ( n n -- n ) + ;\npublic\n: RUN ( n n -- n ) NISHARED ;\n;package\n" ;

: BEHAVIOR ( -- )
   2 NI-ALPHA:RUN 3 EQ!
   NI-BETA:RUN 42 EQ!
   2 3 NI-GAMMA:RUN 5 EQ!
   s" NI-OK ( n -- n ) NI-ALPHA:RUN" CHECK-CANDIDATE! -1 EQ!
   s" NI-BAD ( -- n ) NI-ALPHA:RUN" CHECK-CANDIDATE! 0 EQ!
   PROGRAM$ VERDICT 0 EQ! ;

TRUSTED: ID ( ptr u8 n n ptr u8 n -- n )
   SYM-FIND if exit then
   drop s" name intern: missing symbol" 79 die ;
TRUSTED: OFF ( n -- n ) SYM-NAME-A-FIELD @ ;
: ROW. ( ptr u8 n n ptr u8 n -- )
   ID dup . OFF . cr ;

variable FIRST-ID
variable BEFORE-N
variable BEFORE-U

\ Direct symbol publications here exercise the rollback seam underneath normal
\ checked definitions. The outer and inner frames both use a new spelling;
\ the inner also shares text that predates either frame.
TRUSTED: REWIND-ROWS ( -- )
   SYM-N @ BEFORE-N ! SYM-STR-U @ BEFORE-U !
   CHECKER-SCOPE-START
      s" NI-TEMP-A" SYM-PUBLIC s" NiReWiNd" SYM-INTERN FIRST-ID !
      CHECKER-SCOPE-START
         s" NI-TEMP-B" SYM-PUBLIC s" nirewind" SYM-INTERN drop
         s" NI-TEMP-B" SYM-PRIVATE s" nIsHaReD" SYM-INTERN drop
      CHECKER-SCOPE-DONE
      s" NI-TEMP-A" SYM-PUBLIC s" NIREWIND" SYM-FIND nip ASSERT
   CHECKER-SCOPE-DONE
   SYM-N @ BEFORE-N @ EQ!
   SYM-STR-U @ BEFORE-U @ EQ!
   s" NI-REUSE" SYM-PUBLIC s" NIREPLAC" SYM-INTERN FIRST-ID @ EQ!
   s" NI-TEMP-C" SYM-PUBLIC s" nirewind" SYM-INTERN drop
   s" NI-REUSE" SYM-PUBLIC s" NIREPLAC" SYM-FIND nip ASSERT
   s" NI-TEMP-C" SYM-PUBLIC s" NIREWIND" SYM-FIND nip ASSERT ;

TRUSTED: DEFINE-DELTA ( -- )
   1 set-tier
   s" package NI-DELTA public : NiShArEd ( n -- n ) 4 * ; ;package" evaluate
   0 set-tier ;
TRUSTED: DELTA-RUN ( -- n )
   s" 3 NI-DELTA:NISHARED" evaluate ;

TRUSTED: MEASURE ( -- )
   s" name intern rows (id offset):" type cr
   s" NI-ALPHA" SYM-PRIVATE s" NISHARED" ROW.
   s" NI-BETA" SYM-PUBLIC s" nIsHaReD" ROW.
   s" NI-GAMMA" SYM-PRIVATE s" NISHARED" ROW.
   s" NI-DELTA" SYM-PUBLIC s" NISHARED" ROW.
   SYM-STR-U @ BEFORE-U !
   s" NI-BETA" SYM-PRIVATE s" nIsHaReD" SYM-INTERN drop
   s" name intern duplicate pool delta: " type
   SYM-STR-U @ BEFORE-U @ - . cr ;

public

: PREPARE ( -- )
   1 STEP BEHAVIOR
   s" name intern: prepared" type cr ;

: RECAPTURE ( -- )
   2 STEP BEHAVIOR
   3 STEP REWIND-ROWS
   4 STEP BEHAVIOR
   DEFINE-DELTA
   DELTA-RUN 12 EQ!
   s" NI-DELTA-CALL ( n -- n ) NI-DELTA:NISHARED" CHECK-CANDIDATE! -1 EQ!
   s" NI-DELTA-BAD ( -- n ) NI-DELTA:NISHARED" CHECK-CANDIDATE! 0 EQ!
   s" name intern: recaptured" type cr ;

: FINAL ( -- )
   5 STEP BEHAVIOR
   DELTA-RUN 12 EQ!
   s" NI-DELTA-CALL2 ( n -- n ) NI-DELTA:NISHARED" CHECK-CANDIDATE! -1 EQ!
   6 STEP MEASURE
   s" name intern: restored" type cr ;

;package

NAME-INTERN-SUBJECT:PREPARE
