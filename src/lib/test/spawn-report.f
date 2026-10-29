\ spawn-report.f - launch-context and child-outcome reporter for process fixtures.
\ CONTEXT prints the launch shape (pid, script argv, cwd markers, fds 0/1/2, the
\ environment), each line starting `ctx `, so reports taken under different
\ launchers sort and diff directly (tools/launch-context.f). CHILD prints a
\ failing child's rc, captured stdout and stderr, and that context.
\ STORAGE CLASS. PROCESS-WIDE: static buffers; one task reports at a time.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f

package SPAWN-REPORT
private

\ The terminal-attribute request is host-specific: the Linux TCGETS on macOS
\ kills the process (measured: exit 83), so the engine's target predicate picks
\ it and an unknown host throws.
$50 constant TIO-CAP                 \ termios: 60 bytes on macOS, 44 on Linux
$40487413 constant TIO-GET-BSD       \ TIOCGETA
$5401 constant TIO-GET-SYSV          \ TCGETS
1 constant F-GETFD

create TIO-BUF TIO-CAP allot
create MARK-BUF PATH-CAP allot

variable MARK-U

: TRUE ( -- bool )
   0 0= ;

: YN$ ( bool -- ptr u8 n )
   if s" yes" else s" no" then ;

\ `.` ends the line; a report line carries several fields, so print digits
\ inline. fcntl answers -1 on a closed fd, so the signed wrapper is not optional.
: U-TYPE ( n -- ) {: n:n :}
   n 10 >= if n 10 / RECURSE then
   n 10 mod STR-ZERO + emit ;

: N-TYPE ( n -- ) {: n:n :}
   n 0 < if [char] - emit 0 n - U-TYPE exit then
   n U-TYPE ;

: KEY ( ptr u8 n -- ) {: a:ptr u:n :}
   s" ctx " type a u type s"  " type ;

: LINE$ ( ptr u8 n ptr u8 n -- ) {: ka:ptr ku:n va:ptr vu:n :}
   ka ku KEY va vu type cr ;

: LINE# ( ptr u8 n n -- ) {: ka:ptr ku:n v:n :}
   ka ku KEY v N-TYPE cr ;

: FD-OPEN? ( n -- bool ) {: fd:n :}
   fd F-GETFD 0 fcntl 0 >= ;

: FD-FLAGS ( n -- n ) {: fd:n :}
   fd F-GETFL 0 fcntl ;

: TIO-GET ( -- n )
   HB-TARGET-MACOS? if TIO-GET-BSD exit then
   HB-TARGET-LINUX-KERNEL? if TIO-GET-SYSV exit then
   E-PROC-HOST throw ;

: FD-TTY? ( n -- bool ) {: fd:n :}
   fd TIO-GET TIO-BUF ioctl 0 = ;

: FD-LINE ( ptr u8 n n -- ) {: ka:ptr ku:n fd:n :}
   ka ku KEY
   s" open " type fd FD-OPEN? YN$ type
   s"  flags " type fd FD-FLAGS N-TYPE
   s"  tty " type fd FD-TTY? YN$ type cr ;

: FD-LINES ( -- )
   s" fd0" 0 FD-LINE
   s" fd1" 1 FD-LINE
   s" fd2" 2 FD-LINE ;

: ARGV-LINE ( n -- ) {: i:n :}
   s" ctx argv " type i N-TYPE s"  " type i SCRIPT-ARGV$ type cr ;

: ARGV-LINES ( -- )
   s" argc" SCRIPT-ARGC LINE#
   0 begin dup SCRIPT-ARGC < while
      dup ARGV-LINE
      1+
   repeat drop ;

: ENV-LINE ( n -- ) {: i:n :}
   s" ctx env " type i ENVP dup ZLEN type cr ;

: ENV-LINES ( -- )
   0 begin dup ENVP 0= 0= while
      dup ENV-LINE
      1+
   repeat drop ;

: ENGINE-REL$ ( -- ptr u8 n )
   s" bin/hb" ;

: MARK$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: ba:ptr bu:n ra:ptr ru:n :}
   ba bu ra ru MARK-BUF FS:JOIN-PATH MARK-U !
   MARK-BUF MARK-U @ ;

: PWD-MARKER ( -- )
   s" PWD" GETENV dup 0= if
      2drop s" pwd-has bin/hb" s" (PWD unset)" LINE$ exit
   then
   ENGINE-REL$ MARK$ FS:FILE? {: found:bool :}
   s" pwd-has bin/hb" found YN$ LINE$ ;

\ Whether the engine a caller resolves relatively is reachable from the cwd, and
\ whether the inherited PWD names the same tree: a stale PWD splits yes/no.
: MARKERS ( -- )
   s" cwd-has bin/hb" ENGINE-REL$ FS:FILE? YN$ LINE$
   PWD-MARKER ;

public

: CONTEXT ( -- )
   s" pid" getpid LINE#
   ARGV-LINES
   MARKERS
   FD-LINES
   ENV-LINES ;

: CHILD ( ptr u8 n n n ptr u8 n ptr u8 n -- )
   {: la:ptr lu:n want:n got:n oa:ptr ou:n ea:ptr eu:n :}
   s" child-outcome: " type la lu type cr
   s" child rc: want " type want N-TYPE s"  got " type got N-TYPE cr
   s" child stdout (" type ou N-TYPE s"  bytes):" type cr
   oa ou type cr
   s" child stderr (" type eu N-TYPE s"  bytes):" type cr
   ea eu type cr
   CONTEXT ;

;package
