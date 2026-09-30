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
\ profiler's and lib/signal.f's - sets SA_RESTART, so a blocked read or write
\ that moved nothing resumes by itself (docs/signal.md). A write to a pipe with
\ no reader raises SIGPIPE, which ends the process, unless the caller armed
\ FD-NOSIGPIPE! (lib/process.f); then it throws E-FS-IO here. The engine
\ reports EAGAIN as a failed call, so these words are for blocking descriptors.
\
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require lib/errors.f
require lib/span.f

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

;using
;package
