\ fifo.f - a FIFO a test reads to learn when every process of a tree is gone.
\
\ A spawned child holds exactly the descriptors its spawn maps
\ (src/lib/spawn.f), so a tree under library spawns inherits no pipe end from
\ the test. Instead the test MAKEs a FIFO and names its path to the tree in an
\ environment variable; each process of the tree that is test code opens the
\ write end by path as its first act (WRITER) and maps it into each child it
\ spawns (SPAWN). The test's read end reports end of file once none of them is
\ left.
\
\ A FIFO NO WRITER HAS OPENED YET READS END OF FILE TOO, so EOF-WITHIN means
\ "gone" only after a process of the tree has shown it holds the write end, by
\ a report written to it. The wait for that report polls for bytes: neither
\ host's poll reports a FIFO that no writer has opened yet.
\
\ macOS's poll never reports a FIFO's end of file, whether its last writer
\ closed or died (measured on Darwin 27: no POLLIN, no POLLHUP), so EOF-WITHIN
\ reads the nonblocking read end, which answers 0 with no writer and EAGAIN
\ with one, and polls for at most SLICE-MS between reads. It declares its own
\ read because no library word reads a nonblocking descriptor with its errno.
\
\ STORAGE CLASS. PROCESS-WIDE: the counter that names each FIFO, and SINK, the
\ bytes EOF-WITHIN reads past, which nobody reads back.

require src/lib/errors.f
require src/lib/ffi-abi.f
require src/lib/fmt.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/spawn.f
require src/lib/fd-io.f
require src/lib/adt/option.f

package TEST-FIFO

private

$180 constant MODE                   \ 0600
10 constant SLICE-MS                 \ how late EOF-WITHIN sees end of file on macOS
4 constant EINTR
64 constant SINK-CAP
: EAGAIN ( -- n ) HB-TARGET-MACOS? if 35 else 11 then ;

create SINK SINK-CAP allot
variable MADE                        \ FIFOs MAKE has named

PROCESS-SYMBOLS
FUNCTION: MKFIFO-CALL mkfifo ( ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: READ-CALL read ( n ptr u8 n -- n )
   1 2 WRITES-ARG
;FUNCTION

\ The child's fd n is the parent's fd; a negative parent fd maps nothing.
: SLOT ( PROCESS:request n fd -- PROCESS:request ) {: child:n parent:fd :}
   parent FD>N 0 >= if child >FD parent PROCESS:FD then ;

: READ-END ( ptr u8 n -- fd ) {: a:ptr u:n :}
   a u FS-PATHZ O-NONBLOCK 0 open {: rd:n :}
   rd 0 < if E-FS-OPEN throw then
   rd >FD {: r:fd :}
   r FD-CLOEXEC!
   r ;

public

\ A new FIFO in the directory dir, named fifo-<k> by a count no earlier MAKE
\ of this process used, so a writer a failed case left cannot open a later
\ case's. Its path goes to dst, FS-PATH-CAP bytes, and its length is answered
\ with the read end: nonblocking and close-on-exec. A refused mkfifo or open
\ throws E-FS-OPEN.
: MAKE ( ptr u8 n ptr u8 -- n fd ) {: dir:ptr diru:n dst:ptr :}
   MADE @ 1+ MADE !
   SB-RESET s" fifo-" SB-APPEND MADE @ FMT:SB-INT
   dir diru SB$ dst JOIN-PATH {: u:n :}
   dst u FS-PATHZ MODE MKFIFO-CALL 0<> if E-FS-OPEN throw then
   u dst u READ-END ;

\ The write end of the FIFO the environment variable name gives the path of,
\ blocking and inheritable. A variable unset or empty, a path that is no FIFO,
\ and a FIFO no reader has open throw E-FS-OPEN.
: WRITER ( ptr u8 n -- fd ) {: name:ptr nameu:n :}
   name nameu GETENV {: a:ptr u:n :}
   u 0= if E-FS-OPEN throw then
   a u FD-IO:TRY-FIFO-WRITER MATCH option
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

\ End of file on the read end within ms, past whatever bytes are still in it:
\ 1 at end of file, 0 when ms ran out first, and a negative errno when a read
\ or a poll failed. ms 0 is one read.
: EOF-WITHIN ( fd n -- n ) {: rd:fd ms:n :}
   ms >MS PROC-DEADLINE-AT {: deadline:n :}
   begin
      rd FD>N SINK SINK-CAP READ-CALL {: got:n :}
      got 0= if 1 exit then
      got 0 < if
         FFI:ERRNO {: cause:n :}
         cause EAGAIN <> cause EINTR <> and if cause negate exit then
         deadline PROC-LEFT-MS MS>N SLICE-MS min {: left:n :}
         left 0= if 0 exit then
         rd POLLIN PROC-PFD!
         1 left left >MS PROC-DEADLINE-AT PROC-POLL-RESTART {: rc:n :}
         rc 0 < if rc exit then
      then
   again ;

;package
