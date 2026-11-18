split-str
\ A machine-stack overflow ends named, as a VM stack's does: the record `the
\ machine stack ran past its bounds` on fd 2 and the exit
\ status 102, from the crash handler on the thread's alternate signal stack. Control:
\ a data-stack overflow's record is the one package REJECTION writes for the
\ same parts, byte for byte, in prose and in JSON; so is a memory fault's JSON
\ record, which has notes and no help.
\ Every subject recurses through a self call with work after it, so it cannot
\ become a jump.
require src/lib/process.f
require test/gate-common.f
require src/lib/engine-candidate.f
require src/core/rejection.f

package MACHINE-STACK-TEST
private

create SUBJECT PATH-CAP allot
create IMAGE PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U

: SUBJECT$ ( -- str ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- str ) IMAGE IMAGE-U @ ;

: MACHINE$ ( -- str )
   S\" error[E-STACK-BOUNDS]: the machine stack ran past its bounds\n" ;

: SELF$ ( -- str ) S\" TRUSTED: R ( -- ) recurse recurse ;\nR\n" ;
: CELLS$ ( -- str ) S\" : R ( n -- n ) 1 + recurse 1 + ;\n0 R\n" ;
: SELF-CATCH$ ( -- str )
   S\" TRUSTED: R ( -- ) recurse recurse ;\n: T ( -- n ) ['] R catch ;\nT\n" ;
: CELLS-CATCH$ ( -- str )
   S\" : R ( n -- n ) 1 + recurse 1 + ;\n: T ( -- n n ) 0 [: R ;] catch ;\nT\n" ;

: TASK$ ( -- str )
   S\" require src/lib/task.f\nTRUSTED: R ( -- ) recurse recurse ;\n: R0 ( -- ) R ;\nTASK:MIN-STACK TASK:TASK WORKER\n: GO ( -- ) ['] R0 WORKER TASK:ACTIVATE begin WORKER TASK:DONE? 0= while TASK:PAUSE repeat ;\nGO\n" ;

\ stderr starts with the record's first line.
: EXPECT-LINE ( str str -- ) {: want:^ wantu:n label:^ labelu:n :}
   GT-ERR$ want wantu STARTS-WITH? 0= if label labelu GE-FAIL then ;

: RUN-SRC ( str -- ) {: src:^ srcu:n :}
   GE-HB-RESET
   GE-HB$ src srcu PROCESS:WAIT-FOREVER GE-RUN-STDIN ;

: MACHINE-EXIT ( str -- ) {: label:^ labelu:n :}
   102 label labelu GE-EXPECT-RC
   MACHINE$ label labelu EXPECT-LINE ;

: MACHINE ( ^u8 n ^u8 n -- ) {: src:^ srcu:n label:^ labelu:n :}
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
   s" PASS: a task thread" type cr ;

\ MAIN catches a load from address 0, prints its code and overflows the machine
\ stack; given an argument, it leaves the load uncaught.
: STRIPPED-SRC$ ( -- str )
   S\" TRUSTED: R ( -- ) recurse recurse ;\n: W ( -- ) NULL-PTR BYTE-VIEW c@ drop ;\n: MAIN ( -- ) ARGC 1 > if W then ['] W catch . R ;\n" ;

\ The fault record's first line, written on fd 2 at the fault, caught or not.
: FAULT-LINE$ ( -- str )
   S\" error[E-MEMORY-FAULT]: a read or write faulted\n" ;

: FAULT-LINE ( str -- ) {: label:^ labelu:n :}
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
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER GE-RUN-ENV
   s" stripped application build" GE-EXPECT-OK
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   IMAGE$ PROCESS:WAIT-FOREVER GE-RUN-ENV
   S\" 110\n" s" a stripped application's caught fault" GE-EXPECT-OUT
   s" a stripped application's caught fault" FAULT-LINE
   102 s" a stripped application" GE-EXPECT-RC
   MACHINE$ s" a stripped application" GE-EXPECT-ERR-HAS
   GE-HB-RESET
   IMAGE$ GE-ARGV+ s" uncaught" GE-ARG+
   IMAGE$ PROCESS:WAIT-FOREVER GE-RUN-ENV
   110 s" a stripped application's uncaught fault"
   GE-EXPECT-RC
   s" a stripped application's uncaught fault" FAULT-LINE
   s" PASS: a stripped application, its fault caught and uncaught" type cr ;

: OVERFLOW$ ( -- str ) S\" : X ( n -- ) begin dup recurse again ;\n0 X\n" ;

\ The record REJECTION writes for a data-stack overflow in X, as JSON when json
\ is set.
: RECORD$ ( bool -- str ) {: json:bool :}
   REJECTION:JSON @ {: was:n :}
   json REJECTION:JSON !  REJECTION:CAPTURE
   s" E-STACK-BOUNDS" REJECTION:OPEN
   s" the data stack ran past its bounds" REJECTION:EMIT-STR
   s" in" REJECTION:NOTE  s" X" REJECTION:EMIT-STR
   REJECTION:HELP
   s" give a recursion an exit, or keep fewer values on the stack" REJECTION:EMIT-STR
   REJECTION:KEEP  REJECTION:BUFFER-OFF  was REJECTION:JSON !
   REJECTION:BUFFER$ ;

: CONTROL ( ^u8 n bool ^u8 n -- )
   {: src:^ srcu:n json:bool label:^ labelu:n :}
   src srcu RUN-SRC
   102 label labelu GE-EXPECT-RC
   json RECORD$ label labelu GE-EXPECT-ERR ;

: CONTROLS ( -- )
   OVERFLOW$ false s" a data-stack overflow" CONTROL
   SB-RESET  S\" -1 REJECTION:JSON !\n" SB-APPEND  OVERFLOW$ SB-APPEND
   SB$ true s" a data-stack overflow under REJECTION:JSON" CONTROL
   s" PASS: the data-stack control, in prose and in JSON" type cr ;

: FAULT$ ( -- str )
   S\" -1 REJECTION:JSON !\n: W ( -- ) NULL-PTR BYTE-VIEW c@ drop ;\nW\n" ;

\ The JSON record REJECTION writes for a load from address 0 in W.
: FAULT-RECORD$ ( -- str )
   REJECTION:JSON @ {: was:n :}
   -1 REJECTION:JSON !  REJECTION:CAPTURE
   s" E-MEMORY-FAULT" REJECTION:OPEN
   s" a read or write faulted" REJECTION:EMIT-STR
   s" address" REJECTION:NOTE  s" 0x0000000000000000" REJECTION:EMIT-STR
   s" in" REJECTION:NOTE  s" W" REJECTION:EMIT-STR
   REJECTION:KEEP  REJECTION:BUFFER-OFF  was REJECTION:JSON !
   REJECTION:BUFFER$ ;

\ The record closes its notes and itself, so it parses.
: FAULT-CONTROL ( -- )
   s" a memory fault under REJECTION:JSON" {: label:^ labelu:n :}
   FAULT$ RUN-SRC
   110 label labelu GE-EXPECT-RC
   FAULT-RECORD$ label labelu GE-EXPECT-ERR
   GT-ERR$ S\" }}\n" ENDS-WITH? 0= if label labelu GE-FAIL then
   s" PASS: a memory fault's JSON record, notes and no help" type cr ;

: BODY ( -- )
   s" engine-stack-machine" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" application" IMAGE GT-PATH IMAGE-U !
   REPRODUCERS THREADS STRIPPED CONTROLS FAULT-CONTROL ;

public
: RUN ( -- ) [: BODY ;] [: GT-CLEANUP ;] finally ;

;package

MACHINE-STACK-TEST:RUN
