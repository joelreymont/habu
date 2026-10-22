\ A failed later stack allocation leaves no earlier task mappings behind.
\ PREPARE maps three guarded 64K stacks and a 64K region (src/lib/task.f
\ TASK-PREPARE-RESOURCES). Each stack reserves four STACK-ABI:PAGE-BYTES pages
\ and keeps three, its capacity and two guards (src/lib/memory.f
\ ALLOC-GUARDED), so a whole PREPARE peaks at ten pages. A virtual-memory
\ limit of eight pages over the start admits the first two stacks and refuses
\ the third's reserve; compare /proc/self/statm before and after the caught
\ error.
require src/lib/test.f
require src/lib/task.f
require src/lib/fs.f
require src/lib/ffi-abi.f
require src/habu/task-abi.f

package TASK-PREP-FAIL-TEST

9 constant RLIMIT-AS
8 constant LIMIT-PAGES
128 constant STATM-CAP

create STATM STATM-CAP allot
create OLD-LIMIT 16 allot
create NEW-LIMIT 16 allot
variable START-VM

PROCESS-SYMBOLS
FUNCTION: GET-LIMIT getrlimit ( n ptr u8 -- i32 )
   1 16 WRITES-BYTES
;FUNCTION
FUNCTION: SET-LIMIT setrlimit ( n ptr u8 -- i32 ) ;FUNCTION
FUNCTION: PAGE-SIZE getpagesize ( -- i32 ) ;FUNCTION

TASK:MIN-STACK TASK:TASK WORKER

: IDLE ( -- ) ;

: STATM-PAGES ( -- n )
   s" /proc/self/statm" STATM STATM-CAP READ-ALL {: u:n :}
   0 u 0 do
      STATM i + c@ dup $20 = if drop unloop exit then
      $30 - swap 10 * +
   loop
   E-FS-IO throw ;

: VM-BYTES ( -- n )
   STATM-PAGES PAGE-SIZE * ;

: TCB@ ( n -- n )
   WORKER BYTE-VIEW + CELL-VIEW @ ;

: FAIL-PREPARE ( -- )
   WORKER TASK:PREPARE ;

: WARM ( -- )
   ['] IDLE WORKER TASK:ACTIVATE
   WORKER TASK:KILL ;

: RUN ( -- )
   HB-TARGET-LINUX-X86-64? if
      T-RESET
      WARM
      VM-BYTES START-VM !
      RLIMIT-AS OLD-LIMIT GET-LIMIT 0 <> if
         s" task-prepare-failure: cannot read address-space limit" 78 die
      then
      START-VM @ LIMIT-PAGES STACK-ABI:PAGE-BYTES * + NEW-LIMIT !
      OLD-LIMIT cell+ @ NEW-LIMIT cell+ !
      RLIMIT-AS NEW-LIMIT SET-LIMIT 0 <> if
         s" task-prepare-failure: cannot set address-space limit" 78 die
      then
      ['] FAIL-PREPARE catch {: rc:n :}
      RLIMIT-AS OLD-LIMIT SET-LIMIT 0 <> if
         s" task-prepare-failure: cannot restore address-space limit" 78 die
      then
      s" later task allocation is refused" T-LABEL
      rc E-MEM-MAP T=
      s" task remains empty" T-LABEL
      TASK-ABI:STATUS-OFF TCB@ TASK-ABI:EMPTY T=
      s" no task stack pointer survives refusal" T-LABEL
      TASK-ABI:STACK-U-OFF TCB@ 0 T=
      TASK-ABI:RSTACK-U-OFF TCB@ 0 T=
      TASK-ABI:LSTACK-U-OFF TCB@ 0 T=
      s" failed preparation restores mapped bytes" T-LABEL
      VM-BYTES START-VM @ T=
      T-REPORT
   else
      s" test: ok" type cr
   then ;

RUN
;package
