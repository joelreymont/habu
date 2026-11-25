\ protection-span.f - code-ceiling, task user-area bounds and long-body
\ regressions.

require src/lib/error.f
require src/lib/test/assert.f
require src/lib/test/ending.f
require src/lib/test/subject.f
require src/lib/process.f

package PROTECTION-TEST

$2000 constant CAP                  \ the fill's eleven rejection records pass 4 KiB

create OUT CAP allot
create ERR CAP allot

: CAPTURE ( str -- len len PROCESS:ending ) {: src:str :}
   src OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN ;

: EXPECT ( str n -- ) {: src:str want:n :}
   src CAPTURE {: outu:len erru:len oc :}
   src OUT outu LEN>N >str ERR erru LEN>N >str oc want T-ENDING-EXITED= ;

: ACCEPTS ( str -- )
   0 EXPECT ;

: TEST-TASK-USER ( -- )
   s" require src/lib/task.f -1 TASK:+USERS PST-WRAP u8" EXIT-STATUS:UNCAUGHT EXPECT ;

\ An ordinary definition whose body text passes 20000 bytes: PST-LONG calls a
\ word with a 59-byte name 350 times, 60 bytes of text each with its separator.
\ The child's check spells the count.
$6000 constant LONG-CAP
create LONG-SRC LONG-CAP allot
variable LONG-U
: LONG+ ( str -- )
   {: s:str :}
   s STR:LEN 0 ?do s STR:DATA i + c@ LONG-SRC LONG-U @ + i + c! loop
   LONG-U @ s STR:LEN + LONG-U ! ;
: STEP$ ( -- str )
   s" PST-ADD-ONE-TO-THE-COUNT-OF-STEPS-IN-A-LONG-DEFINITION-BODY" ;
: LONG$ ( -- str )
   0 LONG-U !
   s" : " LONG+  STEP$ LONG+  s"  ( n -- n ) 1 + ; : PST-LONG ( -- n ) 0" LONG+
   350 0 ?do s"  " LONG+  STEP$ LONG+ loop
   s"  ; : PST-RUN ( -- ) PST-LONG 350 <> if 1 throw then ; PST-RUN" LONG+
   LONG-SRC LONG-U @ >str ;

\ The body text grows with the definition: one past 20000 bytes of text runs.
: TEST-BODY-TEXT ( -- )
   LONG$ ACCEPTS ;

\ Its own process, since the fill leaves the image no code space.
: TEST-CODE-FULL ( -- )
   s" require test/fixtures/protection-span-full.f" {: src:str :}
   src CAPTURE {: outu:len erru:len oc :}
   src OUT outu LEN>N >str ERR erru LEN>N >str oc 0 T-ENDING-EXITED=
   ERR erru LEN>N >str s" the code space is full at `PST0" STR:CONTAINS? TTRUE
   ERR erru LEN>N >str s" the code space is full at `PST-LONG-NAME`" STR:CONTAINS? TTRUE
   ERR erru LEN>N >str s" the code space is full at `PST-A`" STR:CONTAINS? TTRUE
   ERR erru LEN>N >str s" the code space is full at `PST-B`" STR:CONTAINS? TTRUE
   ERR erru LEN>N >str s" the code space is full at `PST-C`" STR:CONTAINS? TTRUE ;

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
