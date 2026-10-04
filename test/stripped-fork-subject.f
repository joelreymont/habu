\ stripped-fork-subject.f - a stripped image forks through PROC-FORK.
\ tools/hb-build-stripped-test.f HBT-STRIPPED-FORK builds it with the product
\ hb-build and runs it. The parent makes the main thread's park and forks; the
\ child parks and wakes on its own and exits 0. On Darwin the park is a Mach
\ port, which fork does not copy, so the child has a park of its own only once
\ TASK:CHILD-RESET has run in it: lib/process-fork.f registers that reset with
\ lib/fork-child.f as it loads, and the image carries the registration.
require lib/task.f
require lib/process.f
require lib/process-fork.f

package STRIPPED-FORK

: PARK ( -- )
   TASK:SELF TASK:WAKE TASK:STOP ;

\ The child's whole life. A throw must not leave it: it would unwind the
\ child's copy of the forking thread's stack, so the exit code carries it.
: CHILD ( -- )
   [: PARK ;] catch {: code:n :}
   s" " code 0= if 0 else 1 then die ;

public

: RUN ( -- )
   PARK
   PROC-FORK:CHECKED dup PID>N 0= if drop CHILD then
   PROC-WAIT-RC MATCH result
      ok OF drop s" child=ok" ENDOF
      err OF drop s" child=failed" ENDOF
   ;MATCH type cr ;

;package

: MAIN ( -- ) STRIPPED-FORK:RUN ;
