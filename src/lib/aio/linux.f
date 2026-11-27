\ aio/linux.f - AIO's host layer on Linux, loaded by src/lib/aio.f before its
\ shared code: Q-ARM arms the epoll queue it is given, one-shot per descriptor;
\ Q-DISARM and Q-WAIT use the calling task's (TASK:QUEUE), Q-WAIT into a
\ task-local event buffer. Contract: src/lib/aio.f.
\ STORAGE CLASS. TASK-LOCAL: the event buffers, TASK:+USERS rows.
HB-TARGET-LINUX? ?TARGET
require src/lib/error.f
require src/lib/le.f
require src/lib/log.f
require src/lib/ffi-abi.f
require src/lib/task.f

package AIO
private

PROCESS-SYMBOLS
FUNCTION: EPOLL-CTL-CALL epoll_ctl ( n n n ^u8 -- i32 ) ;FUNCTION
FUNCTION: EPOLL-WAIT-CALL epoll_wait ( n ^u8 n n -- i32 ) ;FUNCTION
FUNCTION: FCNTL-CALL fcntl ( n n n -- i32 ) 2 VARIADIC ;FUNCTION

1 constant CTL-ADD
2 constant CTL-DEL
3 constant CTL-MOD
$40000000 constant EV-ONESHOT
1 constant ERR-PERM
2 constant ERR-NOENT
4 constant ERR-INTR
12 constant ERR-NOMEM
17 constant ERR-EXIST
1 constant F-GETFD

\ Q-ARM's answer for a descriptor epoll does not watch, a regular file: it is
\ ready at once, as poll(2) answers it.
ERR-PERM constant Q-READY-NOW

\ The events one Q-WAIT takes; more stay queued for the next.
64 constant EV-MAX

\ struct epoll_event { u32 events; u64 data }: packed on x86-64, the data at
\ 8 after padding on arm64.
: EV-BYTES ( -- n )
   HB-TARGET-LINUX-X86-64? if 12 else 16 then ;

: EV-DATA ( -- n )
   HB-TARGET-LINUX-X86-64? if 4 else 8 then ;

16 TASK:+USERS CTL-EVENT u8
EV-MAX 16 * TASK:+USERS EVENTS u8

\ Logs a failed call's errno before the throw, so the errno is not lost behind
\ a named code.
: DIAG-ERRNO ( str -- ) {: label:str :}
   FFI:ERRNO {: errno:n :}
   LOG:ERROR s" aio" LOG:LINE
   label LOG:TEXT+
   errno LOG:INT+
   LOG:;LINE ;

: CTL ( n n n -- n ) {: q:n op:n fd:n :}
   q op fd 0 CTL-EVENT EPOLL-CTL-CALL
   0 < if FFI:ERRNO exit then
   0 ;

\ Arms fd one-shot in queue q for the events in n under event data id,
\ re-arming a registration q keeps, else adding one: 0 or the errno. An ADD
\ that finds a registration another task added since is a MOD.
: Q-ARM ( n n n n -- n ) {: q:n fd:n events:n id:n :}
   events EV-ONESHOT or 0 CTL-EVENT LE:U32!
   id 0 CTL-EVENT EV-DATA + LE:U64!
   q CTL-MOD fd CTL dup ERR-NOENT <> if exit then drop
   q CTL-ADD fd CTL dup ERR-EXIST <> if exit then drop
   q CTL-MOD fd CTL ;

\ 0 when the process has the descriptor open, else F_GETFD's errno.
: FD-ERRNO ( n -- n )
   F-GETFD 0 FCNTL-CALL 0 < if FFI:ERRNO exit then
   0 ;

\ A closed descriptor has left the queue unless a duplicate keeps its file open;
\ then its registration stays and can fire once for the number's next record.
: Q-DISARM ( n -- ) {: fd:n :}
   TASK:QUEUE CTL-DEL fd CTL drop ;

\ Waits up to ms, -1 for no bound, and answers the events taken. A signal
\ answers none.
: Q-WAIT ( n -- n ) {: ms:n :}
   TASK:QUEUE 0 EVENTS EV-MAX ms EPOLL-WAIT-CALL {: n:n :}
   n 0 >= if n exit then
   FFI:ERRNO ERR-INTR = if 0 exit then
   s" epoll_wait errno " DIAG-ERRNO
   E-AIO-WAIT throw ;

: EV-AT ( n -- ^u8 )
   EV-BYTES * EVENTS ;

: EV-ID ( n -- n )
   EV-AT EV-DATA + LE:U64@ ;

\ Its bits are poll(2)'s revents.
: EV-REVENTS ( n -- n )
   EV-AT LE:U32@ ;

;package
