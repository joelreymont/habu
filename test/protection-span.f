split-str
\ protection-span.f - code-ceiling, task user-area bounds and long-body
\ regressions.

require src/lib/error.f
require src/lib/test.f
require src/lib/test/ending.f
require src/lib/test/subject.f

package PROTECTION-TEST

$800 constant CAP

create OUT CAP allot
create ERR CAP allot

: CAPTURE ( ^u8 n -- len len PROCESS:ending ) {: src:^ u:n :}
   src u OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN ;

: EXPECT ( ^u8 n n -- ) {: src:^ u:n want:n :}
   src u CAPTURE {: outu:len erru:len oc :}
   src u OUT outu LEN>N ERR erru LEN>N oc want T-ENDING-EXITED= ;

: ACCEPTS ( ^u8 n -- )
   0 EXPECT ;

: TEST-TASK-USER ( -- )
   s" require src/lib/task.f -1 TASK:+USERS PST-WRAP u8" 67 EXPECT ;

\ An ordinary definition whose body text passes 20000 bytes: PST-LONG calls a
\ word with a 59-byte name 350 times, 60 bytes of text each with its separator.
\ The child's check spells the count.
$6000 constant LONG-CAP
create LONG-SRC LONG-CAP allot
variable LONG-U
: LONG+ ( str -- )
   {: a:^ u:n :}
   u 0 ?do a i + c@ LONG-SRC LONG-U @ + i + c! loop
   LONG-U @ u + LONG-U ! ;
: STEP$ ( -- str )
   s" PST-ADD-ONE-TO-THE-COUNT-OF-STEPS-IN-A-LONG-DEFINITION-BODY" ;
: LONG$ ( -- str )
   0 LONG-U !
   s" : " LONG+  STEP$ LONG+  s"  ( n -- n ) 1 + ; : PST-LONG ( -- n ) 0" LONG+
   350 0 ?do s"  " LONG+  STEP$ LONG+ loop
   s"  ; : PST-RUN ( -- ) PST-LONG 350 <> if 1 throw then ; PST-RUN" LONG+
   LONG-SRC LONG-U @ ;

\ The body text grows with the definition: one past 20000 bytes of text runs.
: TEST-BODY-TEXT ( -- )
   LONG$ ACCEPTS ;

\ Its own process, since the fill leaves the image no code space.
: TEST-CODE-FULL ( -- )
   s" require test/fixtures/protection-span-full.f" {: src:^ u:n :}
   src u CAPTURE {: outu:len erru:len oc :}
   src u OUT outu LEN>N ERR erru LEN>N oc 0 T-ENDING-EXITED=
   ERR erru LEN>N s" the code space is full at `create`" CONTAINS? TTRUE
   ERR erru LEN>N s" the code space is full at `constant`" CONTAINS? TTRUE ;

public

: RUN ( -- )
   T-RESET
   TEST-CODE-FULL
   TEST-TASK-USER
   TEST-BODY-TEXT
   T-REPORT
   s" protection-span: ok" type cr ;

;package

PROTECTION-TEST:RUN
