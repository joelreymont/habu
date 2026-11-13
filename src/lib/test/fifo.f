\ fifo.f - a FIFO a test reads to learn when every process of a tree is gone. A
\ spawned child holds only the descriptors its spawn maps (src/lib/spawn.f), so
\ the test names the FIFO's path in an environment variable; each test-code
\ process of the tree opens the write end first (WRITER) and maps it into each
\ child (SPAWN). A FIFO no writer has opened reads end of file too, so end of
\ file means gone only after a tree process has reported on it, and the wait for
\ that report polls for bytes: neither host's poll reports an unopened FIFO.
\ macOS's poll never reports a FIFO's end of file (Darwin 27: no POLLIN, no
\ POLLHUP, last writer closed or died), so EOF-WITHIN reads the nonblocking end
\ (0 with no writer, EAGAIN with one), polling at most SLICE-MS between reads,
\ through its own read: no library word reads a nonblocking fd with its errno.
\ STORAGE CLASS. PROCESS-WIDE: MADE, the counter naming each FIFO, and SINK, the
\ bytes EOF-WITHIN reads past, never read back.

require src/lib/error.f
require src/lib/ffi-abi.f
require src/lib/fmt.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/spawn.f
require src/lib/fd.f
require src/lib/adt/option.f

package TEST-FIFO

private

$180 constant MODE                   \ 0600
10 constant SLICE-MS                 \ how late EOF-WITHIN sees end of file on macOS
4 constant EINTR
64 constant SINK-CAP
: EAGAIN ( -- n ) HB-TARGET-MACOS? if 35 else 11 then ;

create SINK SINK-CAP allot
variable MADE

PROCESS-SYMBOLS
FUNCTION: MKFIFO-CALL mkfifo ( ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: READ-CALL read ( n ptr u8 n -- n )
   1 2 WRITES-ARG
;FUNCTION

\ The child's fd n is the parent's fd; a negative parent fd maps nothing.
: SLOT ( PROCESS:request n fd -- PROCESS:request ) {: child:n parent:fd :}
   parent FD>N 0 >= if child >FD parent PROCESS:FD then ;

: READ-END ( ptr u8 n -- fd ) {: a:ptr u:n :}
   a u FS:>PATHZ FS:O-NONBLOCK 0 open {: rd:n :}
   rd 0 < if E-FS-OPEN throw then
   rd >FD {: r:fd :}
   r FD:FD-CLOEXEC!
   r ;

public

\ A new FIFO in dir, named fifo-<k> by a count no earlier MAKE of this process
\ used, so a writer a failed case left cannot open a later case's. Its path goes
\ to dst (PATH-CAP bytes); the read end is nonblocking and close-on-exec. A
\ refused mkfifo or open throws E-FS-OPEN.
: MAKE ( ptr u8 n ptr u8 -- n fd ) {: dir:ptr diru:n dst:ptr :}
   MADE @ 1+ MADE !
   SB-RESET s" fifo-" SB-APPEND MADE @ FMT:SB-INT
   dir diru SB$ dst FS:JOIN-PATH {: u:n :}
   dst u FS:>PATHZ MODE MKFIFO-CALL 0<> if E-FS-OPEN throw then
   u dst u READ-END ;

\ The write end of the FIFO the environment variable name gives the path of,
\ blocking and inheritable. A variable unset or empty, a path that is no FIFO,
\ and a FIFO no reader has open throw E-FS-OPEN.
: WRITER ( ptr u8 n -- fd ) {: name:ptr nameu:n :}
   name nameu GETENV {: a:ptr u:n :}
   u 0= if E-FS-OPEN throw then
   a u FD:TRY-FIFO-WRITER MATCH option
      none OF E-FS-OPEN throw ENDOF
      some OF ENDOF
   ;MATCH ;

\ PROC-SPAWN-ARGV-ENV-IO with one descriptor more: the staged argv and env,
\ stdin, stdout and stderr (a negative one inherits this process's), and the
\ write end w at its own number in the child; a negative w maps none. The
\ staging is reset once the request holds its copy, so a refused spawn, which
\ throws E-PROC-SPAWN, leaves none.
: SPAWN ( ptr u8 len fd fd fd fd -- pid ) {: a:ptr u in:fd out:fd err:fd w:fd :}
   a u PROC-ARGV-PREPARE {: pathz:ptr argv :}
   PROC-ENV-PREPARE {: envp :}
   pathz dup ZLEN PROCESS:NEW PROCESS:HERMETIC
   argv PROCESS:ARGV-ROWS
   envp PROCESS:ENV-ROWS
   0 in SLOT
   1 out SLOT
   2 err SLOT
   w FD>N w SLOT
   PROC-ARGV-ENV-RESET
   PROCESS:SPAWN ;

\ End of file within ms, past bytes still in it: 1 at end of file, 0 when ms
\ ran out, a negative errno on failure; 0 ms is one read, negative ms no limit.
: EOF-WITHIN ( fd n -- n ) {: rd:fd ms:n :}
   ms >MS PROCESS:DEADLINE-AT {: deadline:n :}
   begin
      rd FD>N SINK SINK-CAP READ-CALL {: got:n :}
      got 0= if 1 exit then
      got 0 < if
         FFI:ERRNO {: cause:n :}
         cause EAGAIN <> cause EINTR <> and if cause negate exit then
         ms 0 < if SLICE-MS else
            deadline PROCESS:LEFT-MS MS>N SLICE-MS min
         then {: left:n :}
         left 0= if 0 exit then
         rd FD:POLLIN PROCESS:PFD!
         1 left left >MS PROCESS:DEADLINE-AT PROCESS:POLL-RESTART {: rc:n :}
         rc 0 < if rc exit then
      then
   again ;

\ The poll reported slot idx broken (POLLERR or POLLNVAL).
: BROKEN? ( idx -- bool )
   PROCESS:PFD-REVENTS FD:POLLERR FD:POLLNVAL or and 0 <> ;

\ The FIFO's read end in slot 0 and the watch in slot 1, with no limit. A failed
\ poll, or a slot the poll reports broken, throws E-PROC-OUTPUT.
: REPORT-OR-END ( fd fd -- ) {: rd:fd watch:fd :}
   rd FD:POLLIN 0 >IDX PROCESS:PFD-AT!
   watch FD:POLLIN 1 >IDX PROCESS:PFD-AT!
   2 -1 -1 >MS PROCESS:DEADLINE-AT PROCESS:POLL-RESTART
   0 < if E-PROC-OUTPUT throw then
   0 >IDX BROKEN? 1 >IDX BROKEN? or if E-PROC-OUTPUT throw then ;

;package
