\ fd.f - the standard descriptors, exact reads and full writes on blocking
\ descriptors (the engine reports EAGAIN as a failed call). A refused read or
\ write, a write that takes no byte and a count past the asked one are E-FS-IO;
\ a negative length is E-SPAN-LENGTH. Signals and descriptors stay the caller's.
\ STORAGE CLASS. TASK-LOCAL: one pollfd row, and src/lib/fs.f's task-local path
\ and stat buffers for TRY-FIFO-WRITER; byte spans stay with the caller.

require src/lib/errors.f
require src/lib/span.f
require src/lib/ffi-abi.f
require src/lib/task.f
require src/lib/le.f
require src/lib/fs.f
require src/lib/adt/option.f

package FD
using SPAN
public

: STDIN ( -- fd ) 0 >FD ;
: STDOUT ( -- fd ) 1 >FD ;
: STDERR ( -- fd ) 2 >FD ;

\ How READ-EXACT ended: the span is full, or end of file came first, after the
\ bytes the span now starts with.
ENUM fill
   VARIANT full ;VARIANT
   VARIANT eof FIELD bytes n ;VARIANT
;ENUM

private

3 constant F-GETFL
4 constant F-SETFL
4 constant POLLOUT
4 constant EINTR
6 constant ENXIO                     \ the same number on macOS and Linux
1000000 constant NS-PER-MS
$7FFFFFFF constant MAX-POLL-MS
: O-NONBLOCK ( -- n ) HB-TARGET-MACOS? if 4 else $800 then ;
: EAGAIN ( -- n ) HB-TARGET-MACOS? if 35 else 11 then ;

PROCESS-SYMBOLS
FUNCTION: WRITE-CALL write ( n ptr u8 n -- n ) ;FUNCTION
FUNCTION: OPEN-CALL open ( ptr u8 n -- i32 ) ;FUNCTION

TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and 8 TASK:+USER POLL-STORAGE drop
: POLL-FD ( -- ptr u8 ) POLL-STORAGE BYTE-VIEW ;

: FLAGS ( fd -- n )
   FD>N F-GETFL 0 fcntl dup 0 < if E-FS-IO throw then ;

: RESTORE ( fd n -- )
   {: f:fd flags:n :}
   f FD>N F-SETFL flags fcntl 0 < if E-FS-IO throw then ;

\ F_SETFL may silently ignore an unknown flag, so check the bit it accepted.
: ARM ( fd n -- bool )
   {: f:fd flags:n :}
   flags O-NONBLOCK and 0<> if false exit then
   f FD>N F-SETFL flags O-NONBLOCK or fcntl 0 < if E-FS-IO throw then
   f FD>N F-GETFL 0 fcntl {: armed:n :}
   armed 0 < armed O-NONBLOCK and 0= or if
      f flags RESTORE
      E-FS-IO throw
   then
   true ;

\ A positive poll count includes POLLERR/HUP/NVAL. The next write names that
\ fault as E-FS-IO; POLLOUT readiness alone never proves a write will succeed.
: WAIT-OUT ( fd n -- )
   {: f:fd deadline:n :}
   begin
      deadline mono-ns - {: left:n :}
      left 0 <= if E-FS-TIMEOUT throw then
      left NS-PER-MS / left NS-PER-MS mod 0<> if 1+ then
      MAX-POLL-MS min {: ms:n :}
      f FD>N POLL-FD LE:U32!
      POLLOUT POLL-FD 4 + LE:U16!
      0 POLL-FD 6 + LE:U16!
      POLL-FD 1 ms poll {: rc:n :}
      rc 0 > if exit then
      rc 0 < rc EINTR negate <> and if E-FS-IO throw then
   again ;

\ A positive result advances; EINTR and EAGAIN retry against the same deadline.
\ A zero write for positive length is an I/O failure, not progress.
: SEND-UNTIL ( fd ptr u8 n n n -- n )
   {: f:fd src:ptr u:n done:n deadline:n :}
   u done - {: want:n :}
   f FD>N src done + want WRITE-CALL {: k:n :}
   k 0 > if
      k want > if E-FS-IO throw then
      k exit
   then
   k 0= if E-FS-IO throw then
   FFI:ERRNO {: cause:n :}
   cause EINTR = if 0 exit then
   cause EAGAIN = if f deadline WAIT-OUT 0 exit then
   E-FS-IO throw ;

: SEND-ALL-UNTIL ( fd ptr u8 n n -- )
   {: f:fd src:ptr u:n deadline:n :}
   0 begin dup u < while
      mono-ns deadline >= if E-FS-TIMEOUT throw then
      dup {: done:n :}
      f src u done deadline SEND-UNTIL +
   repeat drop ;

: SEND-KEEP ( fd ptr u8 n n -- fd ptr u8 n n )
   {: f:fd src:ptr u:n deadline:n :}
   f src u deadline SEND-ALL-UNTIL
   f src u deadline ;

: MORE ( n fd SPAN:span<u8> -- n )
   {: got:n f:fd dst :}
   dst got SKIP $ {: at:ptr want:n :}
   f FD>N at want read {: k:n :}
   k 0 < if E-FS-IO throw then
   k want > if E-FS-IO throw then
   k ;

: SENT ( n fd ptr u8 n -- n )
   {: done:n f:fd src:ptr u:n :}
   u done - {: want:n :}
   f FD>N src done + want write {: k:n :}
   k 0 <= if E-FS-IO throw then
   k want > if E-FS-IO throw then
   k ;

public

\ A read may answer short (a pipe holds only what its writer has put in), so it
\ reads until the span is full. `eof` after zero bytes is a stream that ended
\ on the boundary.
: READ-EXACT ( fd SPAN:span<u8> -- fill )
   {: f:fd dst :}
   dst LEN {: want:n :}
   0 begin dup want < while
      dup f dst MORE
      dup 0= if drop FD-FILL:eof exit then
      +
   repeat
   drop FD-FILL:full ;

\ A blocking write answers short when a caught signal lands after some bytes
\ moved. Every handler that returns (the profiler's, src/lib/signal.f's) sets
\ SA_RESTART, so a blocked write that moved nothing resumes. A write to a pipe
\ with no reader raises SIGPIPE, which ends the process unless the caller armed
\ FD-NOSIGPIPE! (src/lib/process.f); then it is E-FS-IO.
: WRITE-FULL ( fd ptr u8 n -- )
   {: f:fd src:ptr u:n :}
   u 0 < if E-SPAN-LENGTH throw then
   0 begin dup u < while
      dup f src u SENT +
   repeat drop ;

\ Arms O_NONBLOCK for the call, writes through libc to keep errno, and waits for
\ POLLOUT until the deadline, an absolute mono-ns reading a caller can share
\ with a later reply wait. The caller owns the open-file description while it
\ runs: duped and inherited descriptors share O_NONBLOCK. The original status
\ flags are restored before success or a catchable failure is reported. On
\ timeout or failure some bytes may have reached the peer; the caller decides
\ whether to discard that conversation.
: WRITE-FULL-UNTIL ( fd ptr u8 n n -- )
   {: f:fd src:ptr u:n deadline:n :}
   u 0 < if E-SPAN-LENGTH throw then
   u 0= if exit then
   f FLAGS {: flags:n :}
   f flags ARM {: changed:bool :}
   f src u deadline [: SEND-KEEP ;] catch
   >r 2drop 2drop r> {: code:n :}
   changed if f flags RESTORE then
   code 0<> if code throw then ;

\ One attempt to open the write end of the existing FIFO at the path, followed
\ through symlinks; O_NONBLOCK answers ENXIO at once while no reader has it
\ open. none: no reader, and nothing stays open. some: the write end, blocking,
\ which the caller closes. A path that is not a FIFO and any other refusal of
\ the open throw E-FS-OPEN; a descriptor whose O_NONBLOCK cannot be cleared is
\ closed and throws E-FS-IO. It neither sleeps nor polls: the caller owns the
\ deadline and the retries.
: TRY-FIFO-WRITER ( ptr u8 n -- option<fd> )
   {: pa:ptr pu:n :}
   pa pu FS:FIFO? 0= if E-FS-OPEN throw then
   pa pu FS:>PATHZ FS:O-WRONLY O-NONBLOCK or OPEN-CALL {: opened:n :}
   opened 0 < if
      FFI:ERRNO ENXIO = if OPTION:NONE exit then
      E-FS-OPEN throw
   then
   \ F_SETFL 0 clears the status flags F_SETFL can change; of those, the open
   \ set only O_NONBLOCK, so this leaves the write end blocking.
   opened F-SETFL 0 fcntl 0 < if opened close E-FS-IO throw then
   opened >FD OPTION:SOME ;

;using
;package
