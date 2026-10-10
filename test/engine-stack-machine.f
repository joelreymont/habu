\ engine-stack-machine.f - a machine-stack overflow ends named, as a VM
\ stack's does: `hb: stack bounds exceeded (machine)` on fd 2 and the
\ ENGINE-ERROR:STACK-BOUNDS exit, from the crash handler running on the
\ thread's alternate signal stack (src/habu/crash.f C-SIGNAL-STACK). Each case
\ is a child fed its source on stdin, whose exit and fd 2 this row reads: the
\ two reproducers, a word that calls itself with nothing on the data stack and
\ one that adds a cell per call, at tier 0 and tier 1, each plain and under
\ catch; the same overflow on a task thread, on a thread C started that
\ enters through a callback, and in a stripped application. Two controls keep
\ the other reports: a data-stack overflow is still (data), and a load from
\ address 0 is still the register dump.
require test/gate-common.f
require lib/engine-candidate.f
require src/core/engine-error.f

package MACHINE-STACK-TEST
private

600000 constant BUILD-MS
134 constant DUMP-RC                    \ crash.f EMIT-CRASH-HANDLER's exit after the dump

create SUBJECT FS-PATH-CAP allot
create IMAGE FS-PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: MACHINE$ ( -- ptr u8 n ) S\" hb: stack bounds exceeded (machine)\n" ;
: DATA$ ( -- ptr u8 n ) S\" hb: stack bounds exceeded (data)\n" ;

: SELF$ ( -- ptr u8 n ) S\" TRUSTED: R ( -- ) recurse ;\nR\n" ;
: CELLS$ ( -- ptr u8 n ) S\" : R ( n -- n ) 1 + recurse ;\n0 R\n" ;
: SELF-CATCH$ ( -- ptr u8 n )
   S\" TRUSTED: R ( -- ) recurse ;\n: T ( -- n ) ['] R catch ;\nT\n" ;
: CELLS-CATCH$ ( -- ptr u8 n )
   S\" : R ( n -- n ) 1 + recurse ;\n: T ( -- n n ) 0 [: R ;] catch ;\nT\n" ;
: TIER1$ ( -- ptr u8 n ) S\" 1 set-tier\n" ;

: TASK$ ( -- ptr u8 n )
   S\" require lib/task.f\nTRUSTED: R ( -- ) recurse ;\n: R0 ( -- ) R ;\nTASK:MIN-STACK TASK:TASK WORKER\n: GO ( -- ) ['] R0 WORKER TASK:ACTIVATE begin WORKER TASK:DONE? 0= while TASK:PAUSE repeat ;\nGO\n" ;

\ pthread_create starts the thread at the callback's C entry, on an exposed
\ task's context, as test/ffi-callback-fixture.f THREAD-START does: the thread
\ is C's, and Habu code first runs on it inside the callback thunk.
: CALLBACK-THREAD$ ( -- ptr u8 n )
   S\" require lib/ffi-abi.f\nrequire lib/ffi-callback.f\nrequire lib/task.f\nusing FFI-CB\nPROCESS-SYMBOLS\nFUNCTION: PTHREAD-CREATE pthread_create ( ptr u8 n n n -- i32 )\n   0 8 WRITES-BYTES\n;FUNCTION\nFUNCTION: PTHREAD-JOIN pthread_join ( n ptr u8 -- i32 )\n   1 8 WRITES-BYTES\n;FUNCTION\nCALLBACK: START ( n -- n ) 0 FALLBACK ;CALLBACK\nTRUSTED: R ( -- ) recurse ;\n: START-IMPL ( n -- n ) R ;\n' START-IMPL START-BODY !\nTASK:MIN-STACK TASK:TASK CTX\nvariable THREAD\nvariable RET\n: GO ( -- )\n   CTX TASK:EXPOSE\n   THREAD BYTE-VIEW 0 START CTX TASK:CONTEXT ENTRY 0 PTHREAD-CREATE\n   0 <> if s\q callback thread: pthread_create refused\q 1 die then\n   THREAD @ RET BYTE-VIEW PTHREAD-JOIN drop ;\nGO\n;using\n" ;

: RUN-SRC ( ptr u8 n -- ) {: src:ptr srcu:n :}
   GE-HB-RESET
   GE-HB$ src srcu GE-TIMEOUT-MS GE-RUN-STDIN ;

: MACHINE-EXIT ( ptr u8 n -- ) {: label:ptr labelu:n :}
   ENGINE-ERROR:STACK-BOUNDS label labelu GE-EXPECT-RC
   MACHINE$ label labelu GE-EXPECT-ERR ;

\ The source, after the tier prefix when there is one, ends the child named.
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

\ The stripped application's own startup registers the alternate stack before
\ it installs the handler (src/habu/aot-lib.f EMIT-ENTRY). A private cache root,
\ as test/stripped-image.f uses: the maker links this subject on every run.
: STRIPPED ( -- )
   SUBJECT$ S\" TRUSTED: R ( -- ) recurse ;\n: MAIN ( -- ) R ;\n" WRITE-ALL
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --load" GE-ARG+ s" tools/hb-build.f" GE-ARG+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+
   s" -o" GE-ARG+ IMAGE$ GE-ARG+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN GT-ROOT >LEN PROC-ENV+
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
   DUMP-RC s" a load from address 0" GE-EXPECT-RC
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
