\ A machine-stack overflow ends named, as a VM stack's does: `hb: stack bounds
\ exceeded (machine)` on fd 2 and the ENGINE-ERROR:STACK-BOUNDS exit, from the
\ crash handler on the thread's alternate signal stack (src/habu/crash.f
\ C-SIGNAL-STACK). Controls: a data-stack overflow still reports (data); a load
\ from address 0 still dumps registers and dies of the SIGSEGV. Tier 0 compiles
\ a final recurse as a jump, so every self call has work after it.
require test/gate-common.f
require src/lib/engine-candidate.f
require src/core/engine-error.f
require test/aot-builder-engine.f

package MACHINE-STACK-TEST
private

600000 constant BUILD-MS
11 constant SIGSEGV

create SUBJECT PATH-CAP allot
create IMAGE PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: MACHINE$ ( -- ptr u8 n ) S\" hb: stack bounds exceeded (machine)\n" ;
: DATA$ ( -- ptr u8 n ) S\" hb: stack bounds exceeded (data)\n" ;

: SELF$ ( -- ptr u8 n ) S\" TRUSTED: R ( -- ) recurse recurse ;\nR\n" ;
: CELLS$ ( -- ptr u8 n ) S\" : R ( n -- n ) 1 + recurse 1 + ;\n0 R\n" ;
: SELF-CATCH$ ( -- ptr u8 n )
   S\" TRUSTED: R ( -- ) recurse recurse ;\n: T ( -- n ) ['] R catch ;\nT\n" ;
: CELLS-CATCH$ ( -- ptr u8 n )
   S\" : R ( n -- n ) 1 + recurse 1 + ;\n: T ( -- n n ) 0 [: R ;] catch ;\nT\n" ;
: TIER1$ ( -- ptr u8 n ) S\" 1 set-tier\n" ;

: TASK$ ( -- ptr u8 n )
   S\" require src/lib/task.f\nTRUSTED: R ( -- ) recurse recurse ;\n: R0 ( -- ) R ;\nTASK:MIN-STACK TASK:TASK WORKER\n: GO ( -- ) ['] R0 WORKER TASK:ACTIVATE begin WORKER TASK:DONE? 0= while TASK:PAUSE repeat ;\nGO\n" ;

\ pthread_create starts the thread at the callback's C entry, on an exposed
\ task's context, as test/ffi-callback-fixture.f THREAD-START does: the thread
\ is C's, and Habu code first runs on it inside the callback thunk.
: CALLBACK-THREAD$ ( -- ptr u8 n )
   S\" require src/lib/ffi-abi.f\nrequire src/lib/ffi-callback.f\nrequire src/lib/task.f\nusing FFI-CB\nPROCESS-SYMBOLS\nFUNCTION: PTHREAD-CREATE pthread_create ( ptr u8 n n n -- i32 )\n   0 8 WRITES-BYTES\n;FUNCTION\nFUNCTION: PTHREAD-JOIN pthread_join ( n ptr u8 -- i32 )\n   1 8 WRITES-BYTES\n;FUNCTION\nCALLBACK: START ( n -- n ) 0 FALLBACK ;CALLBACK\nTRUSTED: R ( -- ) recurse recurse ;\n: START-IMPL ( n -- n ) R ;\n' START-IMPL START-BODY !\nTASK:MIN-STACK TASK:TASK CTX\nvariable THREAD\nvariable RET\n: GO ( -- )\n   CTX TASK:EXPOSE\n   THREAD BYTE-VIEW 0 START CTX TASK:CONTEXT ENTRY 0 PTHREAD-CREATE\n   0 <> if s\q callback thread: pthread_create refused\q 1 die then\n   THREAD @ RET BYTE-VIEW PTHREAD-JOIN drop ;\nGO\n;using\n" ;

: RUN-SRC ( ptr u8 n -- ) {: src:ptr srcu:n :}
   GE-HB-RESET
   GE-HB$ src srcu GE-TIMEOUT-MS GE-RUN-STDIN ;

\ The child ended by the signal, not by an exit: crash.f EMIT-CRASH-HANDLER
\ re-raises it after the dump. An exit of 128 + n is no signal death.
: KILLED-BY ( n ptr u8 n -- ) {: sig:n label:ptr labelu:n :}
   GT-OUTCOME@ MATCH outcome
     exited OF drop label labelu GE-FAIL ENDOF
     signaled OF sig <> if label labelu GE-FAIL then ENDOF
     timeout OF label labelu GE-FAIL ENDOF
   ;MATCH ;

: MACHINE-EXIT ( ptr u8 n -- ) {: label:ptr labelu:n :}
   ENGINE-ERROR:STACK-BOUNDS label labelu GE-EXPECT-RC
   MACHINE$ label labelu GE-EXPECT-ERR ;

: MACHINE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: pre:ptr preu:n src:ptr srcu:n label:ptr labelu:n :}
   GE-SRC-RESET
   pre preu GE-SRC+
   src srcu GE-SRC+
   GE-SRC-BUF GE-SRC-U @ RUN-SRC
   label labelu MACHINE-EXIT ;

: REPRODUCERS ( -- )
   s" " SELF$ s" a word calling itself, tier 0" MACHINE
   s" " CELLS$ s" a word adding a cell per call, tier 0" MACHINE
   TIER1$ SELF$ s" a word calling itself, tier 1" MACHINE
   TIER1$ CELLS$ s" a word adding a cell per call, tier 1" MACHINE
   s" " SELF-CATCH$ s" a word calling itself under catch, tier 0" MACHINE
   s" " CELLS-CATCH$ s" a word adding a cell per call under catch, tier 0" MACHINE
   TIER1$ SELF-CATCH$ s" a word calling itself under catch, tier 1" MACHINE
   TIER1$ CELLS-CATCH$ s" a word adding a cell per call under catch, tier 1" MACHINE
   s" PASS: the reproducers at both tiers, plain and under catch" type cr ;

: THREADS ( -- )
   s" " TASK$ s" a task thread" MACHINE
   s" " CALLBACK-THREAD$ s" a thread C started, inside a callback" MACHINE
   s" PASS: a task thread and a callback thread" type cr ;

\ The stripped application's own startup registers the alternate stack before it
\ installs the handler (src/habu/aot-link-arm.f EMIT-ENTRY). The fresh subject
\ path has no observed-input index, so hb-build links it with the shared maker.
: STRIPPED ( -- )
   SUBJECT$ S\" TRUSTED: R ( -- ) recurse recurse ;\n: MAIN ( -- ) R ;\n" FS:WRITE-ALL
   AOT-BUILDER-ENGINE:ENSURE
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --load" GE-ARG+ s" tools/hb-build.f" GE-ARG+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+
   s" -o" GE-ARG+ IMAGE$ GE-ARG+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   ENGINE-CANDIDATE:PATH$ BUILD-MS GE-RUN-ENV
   s" stripped application build" GE-EXPECT-OK
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   IMAGE$ GE-TIMEOUT-MS GE-RUN-ENV
   s" a stripped application" MACHINE-EXIT
   s" PASS: a stripped application" type cr ;

: CONTROLS ( -- )
   S\" : X ( n -- ) begin dup recurse again ;\n0 X\n" RUN-SRC
   ENGINE-ERROR:STACK-BOUNDS s" a data-stack overflow" GE-EXPECT-RC
   DATA$ s" a data-stack overflow" GE-EXPECT-ERR
   S\" TRUSTED: W ( -- ) 0 @ drop ;\nW\n" RUN-SRC
   SIGSEGV s" a load from address 0" KILLED-BY
   s" habu-crash regs [sig" s" a load from address 0" GE-EXPECT-ERR-HAS
   s" stack bounds" s" a load from address 0" GE-EXPECT-ERR-LACKS
   s" PASS: the data-stack and address-0 controls" type cr ;

: BODY ( -- )
   s" engine-stack-machine" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" application" IMAGE GT-PATH IMAGE-U !
   REPRODUCERS THREADS STRIPPED CONTROLS ;

public
: RUN ( -- ) [: BODY ;] [: GT-CLEANUP ;] finally ;

;package

MACHINE-STACK-TEST:RUN
