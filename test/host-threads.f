\ Observe OS thread identities independently of Habu's task registry.
require lib/ffi-abi.f
require lib/fs-list.f
require lib/le.f
require lib/string.f

package TEST-HOST
512 constant THREAD-CAP
8 constant THREAD-ID-BYTES
THREAD-CAP THREAD-ID-BYTES * constant THREAD-BYTES
THREAD-BYTES BUFFER: BASE-IDS
THREAD-BYTES BUFFER: NOW-IDS
TYPED-VARIABLE OUT-IDS ptr u8
variable BASE-N
variable THREAD-N

PROCESS-SYMBOLS
FUNCTION: PID-THREADS proc_pidinfo ( n n n ptr u8 n -- i32 )
   3 THREAD-BYTES WRITES-BYTES
;FUNCTION

: ADD-ID ( n -- ) {: id:n :}
   THREAD-N @ THREAD-CAP >= if s" thread snapshot: capacity exceeded" 74 die then
   id OUT-IDS @ THREAD-N @ THREAD-ID-BYTES * + LE:U64!
   1 THREAD-N +! ;

: ADD-NAME ( ptr u8 n -- )
   STR>NUMBER? MATCH option
      some OF ADD-ID ENDOF
      none OF s" thread snapshot: invalid task name" 74 die ENDOF
   ;MATCH ;

: COLLECT ( ptr u8 -- n ) {: out:ptr :}
   HB-TARGET-MACOS? if
      getpid 6 0 out THREAD-BYTES PID-THREADS {: bytes:n :}
      bytes 0 <= bytes THREAD-BYTES >= or
      bytes THREAD-ID-BYTES mod 0<> or if
         s" thread snapshot: proc_pidinfo failed or overflowed" 74 die
      then
      bytes THREAD-ID-BYTES / exit
   then
   out OUT-IDS !
   0 THREAD-N !
   s" /proc/self/task" [: ADD-NAME ;] FS-LIST:EACH
   THREAD-N @ ;

: IN-BASE? ( n -- bool ) {: id:n :}
   BASE-N @ 0 ?do
      BASE-IDS i THREAD-ID-BYTES * + LE:U64@ id = if true unloop exit then
   loop
   false ;

public
: THREADS ( -- n ) NOW-IDS COLLECT ;

: SNAPSHOT ( -- ) BASE-IDS COLLECT BASE-N ! ;

: NEW-THREADS ( -- n )
   NOW-IDS COLLECT {: count:n :}
   0 count 0 ?do
      NOW-IDS i THREAD-ID-BYTES * + LE:U64@ IN-BASE? 0= if 1+ then
   loop ;
;package
