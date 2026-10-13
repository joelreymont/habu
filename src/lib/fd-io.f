\ fd-io.f - exact reads and full writes on a file descriptor.
\
\ READ-EXACT fills the caller's span from a descriptor. A read may answer fewer
\ bytes than asked - a pipe holds only what its writer has put in it - so it
\ reads again until the span is full, and answers `full`. End of file first is
\ an answer, not an error: `eof` carrying how many bytes arrived, which head the
\ span; `eof` after zero bytes is a stream that ended on the boundary.
\ WRITE-FULL writes all of a byte string, again until the kernel has taken
\ every byte: a blocking write answers short when a caught signal lands after
\ some bytes moved. A read or write the kernel refuses throws E-FS-IO, and so
\ does a write that takes no byte, which would never finish, and a count past
\ what was asked. A negative length is E-SPAN-LENGTH.
\
\ Signals and descriptors stay the caller's. Every handler that returns - the
\ profiler's and src/lib/signal.f's - sets SA_RESTART, so a blocked read or write
\ that moved nothing resumes by itself (docs/signal.md). A write to a pipe with
\ no reader raises SIGPIPE, which ends the process, unless the caller armed
\ FD-NOSIGPIPE! (src/lib/process.f); then it throws E-FS-IO here. The engine
\ reports EAGAIN as a failed call, so these words are for blocking descriptors.
\ WRITE-FULL-UNTIL temporarily arms O_NONBLOCK, writes through libc to retain
\ errno, and waits for POLLOUT only until the caller's absolute mono-ns deadline.
\ The caller exclusively owns the descriptor's open-file description while this
\ word runs: O_NONBLOCK is shared by duped and inherited descriptors. Original
\ status flags are restored before success or a catchable failure is reported.
\
\ STORAGE CLASS. TASK-LOCAL: one pollfd row belongs to the calling task. Byte
\ spans and descriptor ownership stay with the caller.

require src/lib/errors.f
require src/lib/span.f
require src/lib/ffi-abi.f
require src/lib/task.f
require src/lib/le.f

package FD-IO
using SPAN
public

\ How READ-EXACT ended: the span is full, or end of file came first, after the
\ bytes the span now starts with.
ENUM fill 0
   VARIANT full ;VARIANT
   VARIANT eof FIELD bytes n ;VARIANT
;ENUM

private

3 constant F-GETFL
4 constant F-SETFL
4 constant POLLOUT
4 constant EINTR
1000000 constant NS-PER-MS
$7FFFFFFF constant MAX-POLL-MS
: O-NONBLOCK ( -- n ) HB-TARGET-MACOS? if 4 else $800 then ;
: EAGAIN ( -- n ) HB-TARGET-MACOS? if 35 else 11 then ;

PROCESS-SYMBOLS
FUNCTION: WRITE-CALL write ( n ptr u8 n -- n ) ;FUNCTION

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

\ One read into the span past its first `got` bytes: how many more arrived,
\ zero at end of file.
: MORE ( n fd SPAN:span<u8> -- n )
   {: got:n f:fd dst :}
   dst got SKIP $ {: at:ptr want:n :}
   f FD>N at want read {: k:n :}
   k 0 < if E-FS-IO throw then
   k want > if E-FS-IO throw then
   k ;

\ One write of the string past its first `done` bytes: how many the kernel took.
: SENT ( n fd ptr u8 n -- n )
   {: done:n f:fd src:ptr u:n :}
   u done - {: want:n :}
   f FD>N src done + want write {: k:n :}
   k 0 <= if E-FS-IO throw then
   k want > if E-FS-IO throw then
   k ;

public

: READ-EXACT ( fd SPAN:span<u8> -- fill )
   {: f:fd dst :}
   dst LEN {: want:n :}
   0 begin dup want < while
      dup f dst MORE
      dup 0= if drop FD--IO-FILL:eof exit then
      +
   repeat
   drop FD--IO-FILL:full ;

: WRITE-FULL ( fd ptr u8 n -- )
   {: f:fd src:ptr u:n :}
   u 0 < if E-SPAN-LENGTH throw then
   0 begin dup u < while
      dup f src u SENT +
   repeat drop ;

\ The deadline is an absolute mono-ns reading, so a caller can share it with a
\ subsequent reply wait. On timeout or failure, some bytes may already have
\ reached the peer; the caller decides whether to discard that conversation.
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

;using
;package
