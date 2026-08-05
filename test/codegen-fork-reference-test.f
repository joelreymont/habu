\ codegen-fork-reference-test.f - the clang reference column under fork.
\ Run: bin/hb --load test/codegen-fork-reference-test.f
\
\ WHAT BROKE, AND WHY A PLAIN SUITE COULD NOT SEE IT. Both codegen-compare
\ members exited 134 under the gate and passed standalone. The difference was
\ not load, not a sibling and not a temp-path collision: it was the fork. dyld
\ is not fork-safe, so a forked child that asks it to map an image the parent
\ never mapped faults inside /usr/lib/dyld -- measured on macOS 26.5.1 as SIGBUS
\ with the pc in dyld's __TEXT, reproduced by ONE fork member with no siblings
\ at all. Standalone there is no fork, so standalone could never fail.
\
\ These cases therefore run the FORK, through the same test/gate-pool.f the gate
\ forks its members with. A test that only called the reference column in this
\ process would pass against the broken code.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-env.f
require lib/process-argv.f
require lib/process-fork.f
require lib/test.f
require lib/test/runner.f
require test/gate-stats.f
require test/gate-pool.f
require tools/codegen-compare-cabi.f

package CGFORK

variable PREPARED?

\ ---- the fact PROC-FORK records ---------------------------------------------

: CHILD-FLAG-ACT ( -- )
   PROC-FORK:CHILD? 0= if E-CODEGEN-COMPARE-STAGE throw then ;

\ ---- a child that would have to map the image itself ------------------------
\ It must be refused BY NAME. Before the guard this was the 134: dyld faulted
\ and the member died with no diagnosis at all. The child cleans up the tree it
\ built on the way in, because it owns that one.

: REFUSE-ACT ( -- )
   [: CODEGEN-CABI:OPEN ;] catch {: rc:n :}
   CODEGEN-CC:REMOVE
   rc E-CODEGEN-CLANG-FORK <> if E-CODEGEN-COMPARE-STAGE throw then ;

\ ---- a child of a parent that mapped it -------------------------------------
\ The supported shape: the forking process maps, the child inherits and calls.
\ hf_i1 is one of the twins tools/clang/twins.c carries; FN throws by itself if
\ the library is not really there, and OPEN is the word that used to abort.

: USE-ACT ( -- )
   CODEGEN-CABI:OPEN
   s" hf_i1" CODEGEN-CABI:FN {: fn:n :}
   fn 0= if E-CODEGEN-COMPARE-STAGE throw then ;

\ ---- a child must not remove the owner's tree -------------------------------

: CHILD-REMOVE-ACT ( -- )
   CODEGEN-CC:OWNER? if E-CODEGEN-COMPARE-STAGE throw then
   CODEGEN-CC:REMOVE ;

\ typed-local-lint: allow-bare-local - q keeps the forked member quotation effect.
: FORK-CASE ( ptr u8 n [ -- ] -- ) {: label:ptr labelu:n q :}
   GT-POOL-RESET
   label labelu 180000 q GT-POOL-START-FORK
   GT-POOL-DRAIN ;

: RED# ( -- n )
   GT-POOL-RED# ;

public

: RUN ( -- )
   T-RESET

   s" PROC-FORK:CHILD? is false in the forking process" T-LABEL
   PROC-FORK:CHILD? TFALSE

   s" PROC-FORK:CHILD? is true in the forked child" T-LABEL
   s" child-flag" [: CHILD-FLAG-ACT ;] FORK-CASE
   RED# 0 T=

   CODEGEN-CC:READY? PREPARED? !
   PREPARED? @ 0= if
      s" codegen-fork-reference: no C toolchain; the fork cases need the reference" type cr
      T-REPORT exit
   then

   s" a child that would have to map the image is refused by name" T-LABEL
   s" refuse" [: REFUSE-ACT ;] FORK-CASE
   RED# 0 T=

   CODEGEN-CABI:PREPARE drop

   s" a child of a process that mapped it calls the reference" T-LABEL
   s" use" [: USE-ACT ;] FORK-CASE
   RED# 0 T=

   s" a child does not remove the tree it did not build" T-LABEL
   s" child-remove" [: CHILD-REMOVE-ACT ;] FORK-CASE
   RED# 0 T=

   s" the reference still answers after that child exited" T-LABEL
   s" use-again" [: USE-ACT ;] FORK-CASE
   RED# 0 T=

   CODEGEN-CC:REMOVE
   T-REPORT ;

;package

CGFORK:RUN
