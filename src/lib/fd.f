\ fd.f - the standard descriptors, close-on-exec pipes, the non-blocking mark,
\ exact reads and full writes on blocking descriptors (the engine reports EAGAIN
\ as a failed call), one write that never waits on a non-blocking descriptor,
\ and syncing and resizing a file. A refused PIPE, mark, read, write, sync or
\ resize, a write that takes no byte and a count past the asked one are E-FS-IO,
\ which TRY-WRITE-FULL answers as how far its write got; TRY-WRITE answers 0 for
\ a full descriptor or a signal. A negative length is E-SPAN-LENGTH. A refused
\ PIPE-PAIR, FD-CLOEXEC!, FD-NOSIGPIPE! or WITH-NOSIGPIPE fcntl is
\ E-PROC-OUTPUT.
\ Signals and descriptors stay the caller's.
\ STORAGE CLASS. TASK-LOCAL: one pollfd row, and src/lib/fs.f's task-local path
\ and stat buffers for TRY-FIFO-WRITER; byte spans stay with the caller.

require src/lib/error.f
require src/lib/span.f
require src/lib/ffi-abi.f
require src/lib/task.f
require src/lib/fd-lock.f
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

\ How TRY-WRITE-FULL ended: every byte moved, the first write was refused with
\ nothing moved, or some bytes moved and then a write was refused.
ENUM wrote all none part ;ENUM

1 constant POLLIN
4 constant POLLOUT
8 constant POLLERR
16 constant POLLHUP
32 constant POLLNVAL
4 constant EINTR#                  \ "interrupted by a signal" (POSIX, identical on Linux/macOS)
2 constant F-SETFD
3 constant F-GETFL
4 constant F-SETFL
73 constant F-SETNOSIGPIPE
74 constant F-GETNOSIGPIPE
1 constant FD-CLOEXEC

\ O_NONBLOCK is not one number: $800 (0o4000) on Linux, 4 on macOS. F_SETFL
\ silently drops a bit the host does not know, so the wrong constant arms
\ nothing at all and the descriptor stays blocking.
: O-NONBLOCK ( -- n )
   HB-TARGET-LINUX-KERNEL? if $800 else 4 then ;

\ F_DUPFD_CLOEXEC is not one number either: 1030 (F_LINUX_SPECIFIC_BASE + 6) on
\ both Linux targets, 67 on macOS.
: F-DUPFD-CLOEXEC ( -- n )
   HB-TARGET-LINUX-KERNEL? if 1030 else 67 then ;

: FD-CLOEXEC! ( fd -- ) {: fd :}
   fd FD>N F-SETFD FD-CLOEXEC fcntl 0 <> if E-PROC-OUTPUT throw then ;

: FD-NOSIGPIPE! ( fd -- ) {: fd :}
   fd FD>N F-SETNOSIGPIPE 1 fcntl 0 <> if E-PROC-OUTPUT throw then ;


\ Holds SIGPIPE off on fd for the quotation, then puts fd's flag back, throw or
\ not. On Linux F-SETNOSIGPIPE ignores SIGPIPE process-wide: no flag goes back.
\ On macOS the flag belongs to the open file description, which dups and
\ inherited copies share: overlapping holds there restore it only when the later
\ one ends first. When the first to begin ends first it clears the flag under the
\ other, whose write can then raise SIGPIPE, and the other then leaves it set.
: WITH-NOSIGPIPE ( fd [ -- ] -- )
   {: f:fd body :}
   HB-TARGET-LINUX-KERNEL? if f FD-NOSIGPIPE! body execute exit then
   f FD>N F-GETNOSIGPIPE 0 fcntl {: old:n :}
   old 0 < if E-PROC-OUTPUT throw then
   f FD-NOSIGPIPE!
   body catch {: code:n :}
   f FD>N F-SETNOSIGPIPE old fcntl 0 <> if E-PROC-OUTPUT throw then
   code 0<> if code throw then ;


: PIPE-PAIR ( -- fd fd )
   pipe {: r w rc :}
   rc 0 <> if E-PROC-OUTPUT throw then
   r >FD w >FD ;

private

6 constant ENXIO                     \ the same number on macOS and Linux
1000000 constant NS-PER-MS
$7FFFFFFF constant MAX-POLL-MS
: EAGAIN ( -- n ) HB-TARGET-MACOS? if 35 else 11 then ;

PROCESS-SYMBOLS
FUNCTION: WRITE-CALL write ( n ptr u8 n -- n ) ;FUNCTION
FUNCTION: OPEN-CALL open ( ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: SYNC-CALL fsync ( n -- i32 ) ;FUNCTION
FUNCTION: RESIZE-CALL ftruncate ( n n -- i32 ) ;FUNCTION

8 TASK:+USERS POLL-STORAGE u8
: POLL-FD ( -- ptr u8 ) 0 POLL-STORAGE ;

: FLAGS ( fd -- n )
   FD>N F-GETFL 0 fcntl dup 0 < if E-FS-IO throw then ;

: RESTORE ( fd n -- )
   {: f:fd flags:n :}
   f FD>N F-SETFL flags fcntl 0 < if E-FS-IO throw then ;

\ F_SETFL may silently ignore an unknown flag, so check the bit it accepted.
: ARM ( fd n -- bool )
   {: f:fd flags:n :}
   flags O-NONBLOCK and 0<> if false exit then
   f FD>N F-SETFL flags O-NONBLOCK or fcntl 0 < if f flags RESTORE E-FS-IO throw then
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
      POLLOUT 4 POLL-STORAGE LE:U16!
      0 6 POLL-STORAGE LE:U16!
      POLL-FD 1 ms poll {: rc:n :}
      rc 0 > if exit then
      rc 0 < rc EINTR# negate <> and if E-FS-IO throw then
   again ;

public

\ One write to a descriptor the caller made non-blocking: the bytes it moved.
: TRY-WRITE ( fd ptr u8 n -- n )
   {: f:fd src:ptr u:n :}
   u 0 < if E-SPAN-LENGTH throw then
   u 0= if 0 exit then
   f FD>N src u WRITE-CALL {: k:n :}
   k u > if E-FS-IO throw then
   k 0 > if k exit then
   k 0= if E-FS-IO throw then
   FFI:ERRNO {: cause:n :}
   cause EINTR# = cause EAGAIN = or if 0 exit then
   E-FS-IO throw ;

private

\ A positive result advances; a 0 waits for POLLOUT against the same deadline.
: SEND-UNTIL ( fd ptr u8 n n n -- n )
   {: f:fd src:ptr u:n done:n deadline:n :}
   f src done + u done - TRY-WRITE {: k:n :}
   k 0= if f deadline WAIT-OUT then
   k ;

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

\ The bytes one write moved, 0 when it moved none; a count past the asked one is
\ answered as the call gave it.
: SENT ( n fd ptr u8 n -- n )
   {: done:n f:fd src:ptr u:n :}
   f FD>N src done + u done - write 0 max ;

: PIPE-HELD ( -- fd fd )
   pipe {: r:n w:n rc:n :}
   rc 0 <> if E-FS-IO throw then
   r F-SETFD FD-CLOEXEC fcntl
   w F-SETFD FD-CLOEXEC fcntl or 0 <> if
      r close w close E-FS-IO throw
   then
   r >FD w >FD ;

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
\ FD-NOSIGPIPE! or writes inside WITH-NOSIGPIPE; then the write is refused. A
\ count past the asked one is `part`: bytes moved, but not the ones asked.
: TRY-WRITE-FULL ( fd ptr u8 n -- wrote )
   {: f:fd src:ptr u:n :}
   u 0 < if E-SPAN-LENGTH throw then
   0 begin dup u < while
      dup dup f src u SENT +
      2dup = if drop 0= if FD-WROTE:none else FD-WROTE:part then exit then
      dup u > if 2drop FD-WROTE:part exit then
      nip
   repeat drop FD-WROTE:all ;


\ TRY-WRITE-FULL, with E-FS-IO for anything short of `all`.
: WRITE-FULL ( fd ptr u8 n -- )
   TRY-WRITE-FULL MATCH wrote
      all OF ENDOF
      none OF E-FS-IO throw ENDOF
      part OF E-FS-IO throw ENDOF
   ;MATCH ;

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

\ Returns once the file's bytes and length, or a directory's entries, are on the
\ device; on macOS that is the drive's cache, as PostgreSQL's default is.
: SYNC ( fd -- )
   FD>N SYNC-CALL 0<> if E-FS-IO throw then ;

\ Sets the file's length: bytes past it go and a shorter file grows with zero
\ bytes. A descriptor opened for append writes next at the new end.
: RESIZE ( fd n -- )
   {: f:fd len:n :}
   len 0 < if E-SPAN-LENGTH throw then
   f FD>N len RESIZE-CALL 0<> if E-FS-IO throw then ;

\ Marks the descriptor non-blocking, or leaves it so; a mark refused or not kept
\ restores its flags.
: NONBLOCK! ( fd -- )
   {: f:fd :}
   f f FLAGS ARM drop ;

\ A pipe, read end then write end, both close-on-exec, made and marked under
\ FD-LOCK:WITH (see fd-lock.f); a refused mark closes both before E-FS-IO.
: PIPE ( -- fd fd )
   [: PIPE-HELD ;] FD-LOCK:WITH ;

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
