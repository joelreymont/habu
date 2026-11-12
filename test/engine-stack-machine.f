\ A machine-stack overflow ends named, as a VM stack's does: `E-STACK-BOUNDS
\ habu: stack bounds exceeded (machine) lr <hex>` on fd 2 and the
\ ENGINE-ERROR:STACK-BOUNDS exit, from the crash handler on the thread's
\ alternate signal stack. Control: a data-stack overflow still reports (data).
\ Every subject recurses through a self call with work after it, so it cannot
\ become a jump.
require test/gate-common.f
require src/lib/engine-candidate.f
require src/core/engine-error.f

package MACHINE-STACK-TEST
private

\ ms for the stripped application's hb-build, held to an engine build's limit
300000 constant BUILD-MS

create SUBJECT PATH-CAP allot
create IMAGE PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: MACHINE$ ( -- ptr u8 n ) s" E-STACK-BOUNDS habu: stack bounds exceeded (machine) lr " ;
: DATA$ ( -- ptr u8 n ) s" E-STACK-BOUNDS habu: stack bounds exceeded (data) lr " ;

: SELF$ ( -- ptr u8 n ) S\" TRUSTED: R ( -- ) recurse recurse ;\nR\n" ;
: CELLS$ ( -- ptr u8 n ) S\" : R ( n -- n ) 1 + recurse 1 + ;\n0 R\n" ;
: SELF-CATCH$ ( -- ptr u8 n )
   S\" TRUSTED: R ( -- ) recurse recurse ;\n: T ( -- n ) ['] R catch ;\nT\n" ;
: CELLS-CATCH$ ( -- ptr u8 n )
   S\" : R ( n -- n ) 1 + recurse 1 + ;\n: T ( -- n n ) 0 [: R ;] catch ;\nT\n" ;

: TASK$ ( -- ptr u8 n )
   S\" require src/lib/task.f\nTRUSTED: R ( -- ) recurse recurse ;\n: R0 ( -- ) R ;\nTASK:MIN-STACK TASK:TASK WORKER\n: GO ( -- ) ['] R0 WORKER TASK:ACTIVATE begin WORKER TASK:DONE? 0= while TASK:PAUSE repeat ;\nGO\n" ;

\ pthread_create starts the thread at the callback's C entry, on an exposed
\ task's context, as test/ffi-callback-fixture.f THREAD-START does: the thread
\ is C's, and Habu code first runs on it inside the callback thunk.
: CALLBACK-THREAD$ ( -- ptr u8 n )
   S\" require src/lib/ffi-abi.f\nrequire src/lib/ffi-callback.f\nrequire src/lib/task.f\nusing FFI-CB\nPROCESS-SYMBOLS\nFUNCTION: PTHREAD-CREATE pthread_create ( ptr u8 n n n -- i32 )\n   0 8 WRITES-BYTES\n;FUNCTION\nFUNCTION: PTHREAD-JOIN pthread_join ( n ptr u8 -- i32 )\n   1 8 WRITES-BYTES\n;FUNCTION\nCALLBACK: START ( n -- n ) 0 FALLBACK ;CALLBACK\nTRUSTED: R ( -- ) recurse recurse ;\n: START-IMPL ( n -- n ) R ;\n' START-IMPL START-BODY !\nTASK:MIN-STACK TASK:TASK CTX\nvariable THREAD\nvariable RET\n: GO ( -- )\n   CTX TASK:EXPOSE\n   THREAD BYTE-VIEW 0 START CTX TASK:CONTEXT ENTRY 0 PTHREAD-CREATE\n   0 <> if s\q callback thread: pthread_create refused\q 1 die then\n   THREAD @ RET BYTE-VIEW PTHREAD-JOIN drop ;\nGO\n;using\n" ;

\ stderr starts with the line's text; its lr varies with the build.
: EXPECT-LINE ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu:n label:ptr labelu:n :}
   GT-ERR$ want wantu STARTS-WITH? 0= if label labelu GE-FAIL then ;

: RUN-SRC ( ptr u8 n -- ) {: src:ptr srcu:n :}
   GE-HB-RESET
   GE-HB$ src srcu GE-TIMEOUT-MS GE-RUN-STDIN ;

: MACHINE-EXIT ( ptr u8 n -- ) {: label:ptr labelu:n :}
   ENGINE-ERROR:STACK-BOUNDS label labelu GE-EXPECT-RC
   MACHINE$ label labelu EXPECT-LINE ;

: MACHINE ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n label:ptr labelu:n :}
   src srcu RUN-SRC
   label labelu MACHINE-EXIT ;

: REPRODUCERS ( -- )
   SELF$ s" a word calling itself, tier 0" MACHINE
   CELLS$ s" a word adding a cell per call, tier 0" MACHINE
   SELF-CATCH$ s" a word calling itself under catch, tier 0" MACHINE
   CELLS-CATCH$ s" a word adding a cell per call under catch, tier 0" MACHINE
   s" PASS: the reproducers, plain and under catch" type cr ;

: THREADS ( -- )
   TASK$ s" a task thread" MACHINE
   CALLBACK-THREAD$ s" a thread C started, inside a callback" MACHINE
   s" PASS: a task thread and a callback thread" type cr ;

\ MAIN catches a load from address 0, prints its code and overflows the machine
\ stack; given an argument, it leaves the load uncaught.
: STRIPPED-SRC$ ( -- ptr u8 n )
   S\" TRUSTED: R ( -- ) recurse recurse ;\n: W ( -- ) NULL-PTR BYTE-VIEW c@ drop ;\n: MAIN ( -- ) ARGC 1 > if W then ['] W catch . R ;\n" ;

\ The fault's line up to its address, written on fd 2 at the fault, caught or
\ not.
: FAULT-LINE$ ( -- ptr u8 n ) s" E-MEMORY-FAULT habu: memory fault at " ;

: FAULT-LINE ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GT-ERR$ FAULT-LINE$ STARTS-WITH? 0= if label labelu GE-FAIL then ;

\ The stripped application's own startup registers the alternate stack before it
\ installs the handler (src/arch/arm64/boot.f ENTRY,). It has no interpreter
\ catch: its own text faults, and its uncaught throw reaches its own reporter.
: STRIPPED ( -- )
   SUBJECT$ STRIPPED-SRC$ FS:WRITE-ALL
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --load" GE-ARG+ s" tools/hb-build.f" GE-ARG+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+
   s" -o" GE-ARG+ IMAGE$ GE-ARG+
   ENGINE-CANDIDATE:PATH$ BUILD-MS GE-RUN-ENV
   s" stripped application build" GE-EXPECT-OK
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   IMAGE$ GE-TIMEOUT-MS GE-RUN-ENV
   S\" 110\n" s" a stripped application's caught fault" GE-EXPECT-OUT
   s" a stripped application's caught fault" FAULT-LINE
   ENGINE-ERROR:STACK-BOUNDS s" a stripped application" GE-EXPECT-RC
   MACHINE$ s" a stripped application" GE-EXPECT-ERR-HAS
   GE-HB-RESET
   IMAGE$ GE-ARGV+ s" uncaught" GE-ARG+
   IMAGE$ GE-TIMEOUT-MS GE-RUN-ENV
   ENGINE-ERROR:MEMORY-FAULT s" a stripped application's uncaught fault"
   GE-EXPECT-RC
   s" a stripped application's uncaught fault" FAULT-LINE
   s" PASS: a stripped application, its fault caught and uncaught" type cr ;

: CONTROLS ( -- )
   S\" : X ( n -- ) begin dup recurse again ;\n0 X\n" RUN-SRC
   ENGINE-ERROR:STACK-BOUNDS s" a data-stack overflow" GE-EXPECT-RC
   DATA$ s" a data-stack overflow" EXPECT-LINE
   s" PASS: the data-stack control" type cr ;

: BODY ( -- )
   s" engine-stack-machine" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" application" IMAGE GT-PATH IMAGE-U !
   REPRODUCERS THREADS STRIPPED CONTROLS ;

public
: RUN ( -- ) [: BODY ;] [: GT-CLEANUP ;] finally ;

;package

MACHINE-STACK-TEST:RUN
