\ protection-span.f - code-area bounds and long-body regressions.

require src/lib/error.f
require src/lib/test.f
require src/lib/test/ending.f
require src/lib/test/subject.f

package PROTECTION-TEST

$800 constant CAP
ENGINE-ERROR:SEAL-VIOLATION constant VIOLATION-RC
67 constant UNCAUGHT-RC
76 constant CODE-FULL-RC

create OUT CAP allot
create ERR CAP allot

: CAPTURE ( ptr u8 n -- len len PROCESS:ending ) {: src:ptr u:n :}
   src u OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN ;

: EXPECT ( ptr u8 n n -- ) {: src:ptr u:n want:n :}
   src u CAPTURE {: outu:len erru:len oc :}
   src u OUT outu LEN>N ERR erru LEN>N oc want T-ENDING-EXITED= ;

: REJECTS ( ptr u8 n -- )
   VIOLATION-RC EXPECT ;

: ACCEPTS ( ptr u8 n -- )
   0 EXPECT ;

\ At the code ceiling `create` and `:` refuse before they write (src/habu/interpreter.f
\ DEF-CODE-ROOM); a constant's body write reaches the guard.
: TEST-CODE ( -- )
   s" dbase@ DICT-SIZE + cp!" ACCEPTS
   s" dbase@ REGION + $4000 - 4 - cp!" ACCEPTS
   s" dbase@ REGION + 4 - cp!" ACCEPTS
   s" dbase@ REGION + 4 - cp! create PST-END" CODE-FULL-RC EXPECT
   s" dbase@ REGION + 4 - cp! 1 constant PST-END" REJECTS
   s" dbase@ REGION + 4 - cp! : PST-END ( -- n ) 1 ;" CODE-FULL-RC EXPECT ;

: TEST-TASK-USER ( -- )
   s" require src/lib/task.f -1 TASK:+USERS PST-WRAP u8" UNCAUGHT-RC EXPECT ;

\ An ordinary definition whose body text passes 20000 bytes: PST-LONG calls a
\ word with a 59-byte name 350 times, 60 bytes of text each with its separator.
\ The child's check spells the count.
$6000 constant LONG-CAP
create LONG-SRC LONG-CAP allot
variable LONG-U
: LONG+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do a i + c@ LONG-SRC LONG-U @ + i + c! loop
   LONG-U @ u + LONG-U ! ;
: STEP$ ( -- ptr u8 n )
   s" PST-ADD-ONE-TO-THE-COUNT-OF-STEPS-IN-A-LONG-DEFINITION-BODY" ;
: LONG$ ( -- ptr u8 n )
   0 LONG-U !
   s" : " LONG+  STEP$ LONG+  s"  ( n -- n ) 1 + ; : PST-LONG ( -- n ) 0" LONG+
   350 0 ?do s"  " LONG+  STEP$ LONG+ loop
   s"  ; : PST-RUN ( -- ) PST-LONG 350 <> if 1 throw then ; PST-RUN" LONG+
   LONG-SRC LONG-U @ ;

\ The body text grows with the definition: one past 20000 bytes of text runs.
: TEST-BODY-TEXT ( -- )
   LONG$ ACCEPTS ;

public

: RUN ( -- )
   T-RESET
   TEST-CODE
   TEST-TASK-USER
   TEST-BODY-TEXT
   T-REPORT
   s" protection-span: ok" type cr ;

;package

PROTECTION-TEST:RUN
