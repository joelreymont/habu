\ Count OS threads independently of Habu's task registry.
require lib/ffi-abi.f
require lib/fs-list.f
require lib/le.f

package TEST-HOST
96 constant TASK-INFO-BYTES
84 constant THREAD-COUNT-OFF
TASK-INFO-BYTES BUFFER: TASK-INFO
variable THREAD-N

PROCESS-SYMBOLS
FUNCTION: PID-INFO proc_pidinfo ( n n n ptr u8 n -- n )
   3 TASK-INFO-BYTES WRITES-BYTES
;FUNCTION

: TALLY ( ptr u8 n -- ) 2drop 1 THREAD-N +! ;

public
: THREADS ( -- n )
   HB-TARGET-MACOS? if
      getpid 4 0 TASK-INFO TASK-INFO-BYTES PID-INFO
      TASK-INFO-BYTES <> if s" thread count: proc_pidinfo failed" 74 die then
      TASK-INFO THREAD-COUNT-OFF + LE:U32@ exit
   then
   0 THREAD-N !
   s" /proc/self/task" [: TALLY ;] FS-LIST:EACH
   THREAD-N @ ;
;package
