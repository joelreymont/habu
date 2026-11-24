\ spawn-report.f - launch-context and child-outcome reporter for process fixtures.
\ CONTEXT prints the launch shape (pid, script argv, cwd markers, fds 0/1/2, the
\ environment), each line starting `ctx `, so reports taken under different
\ launchers sort and diff directly (tools/launch-context.f). CHILD prints a
\ failing child's rc, captured stdout and stderr, and that context.
\ STORAGE CLASS. PROCESS-WIDE: static buffers; one task reports at a time.

require src/lib/error.f
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

: YN$ ( bool -- str )
   if s" yes" else s" no" then ;

\ `.` ends the line; a report line carries several fields, so print digits
\ inline.
: U-TYPE ( n -- ) {: n:n :}
   n 10 >= if n 10 / RECURSE then
   n 10 mod STR:ZERO + emit ;

\ fcntl answers -errno for a closed fd's flags, so they print signed.
: N-TYPE ( n -- ) {: n:n :}
   n 0 < if [char] - emit 0 n - U-TYPE exit then
   n U-TYPE ;

: KEY ( str -- ) {: s:str :}
   s" ctx " type s type s"  " type ;

: LINE$ ( str str -- ) {: ks:str vs:str :}
   ks KEY vs type cr ;

: LINE# ( str n -- ) {: ks:str v:n :}
   ks KEY v N-TYPE cr ;

: FD-OPEN? ( n -- bool ) {: fd:n :}
   fd F-GETFD 0 fcntl 0 >= ;

: FD-FLAGS ( n -- n ) {: fd:n :}
   fd FD:F-GETFL 0 fcntl ;

: TIO-GET ( -- n )
   HB-TARGET-MACOS? if TIO-GET-BSD else TIO-GET-SYSV then ;

: FD-TTY? ( n -- bool ) {: fd:n :}
   fd TIO-GET TIO-BUF ioctl 0 = ;

: FD-LINE ( str n -- ) {: ks:str fd:n :}
   ks KEY
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
   s" ctx env " type i ENVP dup ZLEN >str type cr ;

: ENV-LINES ( -- )
   0 begin dup ENVP 0= 0= while
      dup ENV-LINE
      1+
   repeat drop ;

: ENGINE-REL$ ( -- str )
   s" bin/hb" ;

: MARK$ ( str str -- str ) {: bs:str rs:str :}
   bs rs MARK-BUF FS:JOIN-PATH MARK-U !
   MARK-BUF MARK-U @ >str ;

: PWD-MARKER ( -- )
   s" PWD" GETENV dup STR:LEN 0= if
      drop s" pwd-has bin/hb" s" (PWD unset)" LINE$ exit
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

: CHILD ( str n n str str -- )
   {: ls:str want:n got:n os:str es:str :}
   s" child-outcome: " type ls type cr
   s" child rc: want " type want N-TYPE s"  got " type got N-TYPE cr
   s" child stdout (" type os STR:LEN N-TYPE s"  bytes):" type cr
   os type cr
   s" child stderr (" type es STR:LEN N-TYPE s"  bytes):" type cr
   es type cr
   CONTEXT ;

;package
