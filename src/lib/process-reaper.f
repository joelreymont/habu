\ process-reaper.f - a pool's reaper: an hb running src/lib/process-reaper-main.f,
\ which leads a process group of its own and holds the read end of a pipe whose
\ write end only the pool keeps. A child spawned REAPED is in that group from
\ its first instruction. When the write end closes, by REAPER-STOP or by the
\ pool's death, SIGKILL included, the reaper reads end of file and kills every
\ other process in its group, each with its tree.
\ A child that leaves the group, by setsid or setpgid, is out of reach.

require src/lib/error.f
require src/lib/span.f
require src/lib/fd.f
require src/lib/fs.f
require src/lib/process-path.f
require src/lib/spawn.f
require src/lib/process.f

package PROCESS
public

\ A reaper's pipe write end and pid; plain data, not linear.
STRUCTURE reaper OPAQUE
   FIELD writer fd
   FIELD leader pid
;STRUCTURE

private

1 constant HUP-BIT                       \ SIGHUP's bit, signal 1 on both hosts

: MAIN$ ( -- ^u8 n ) s" src/lib/process-reaper-main.f" ;

\ The program under the tree above the engine's bin directory.
: MAIN-PATH ( -- ^u8 n )
   OWN-PATH SOURCE-ROOT:DIRNAME SOURCE-ROOT:DIRNAME
   MAIN$ PATHZ-BUF PATH-CAP SPAN:MAKE FS:JOIN-PATH-INTO
   PATHZ-BUF swap ;

\ The pool's death orphans the reaper's group, which the kernel then sends
\ SIGHUP if a member is stopped; the child starts with SIGHUP blocked.
: BLOCK-HUP ( PROCESS:request -- PROCESS:request )
   ERASE {: h:^ :}
   h BLOCKED-CELL @ HUP-BIT or h BLOCKED-CELL !
   h MINT ;

\ The engine on the program, the pipe's read end its fd 3, fds 0 and 1 closed.
: REAPER-REQUEST ( ^u8 n fd -- PROCESS:request )
   {: main:^ mainu r:fd :}
   OWN-PATH NEW BLOCK-HUP
   main mainu ARG
   3 >FD r FD
   0 >FD CLOSED
   1 >FD CLOSED ;

\ The top cell, a placeholder, carries the pid out, so catch keeps the depth.
: REAPER-SPAWN ( ^u8 n fd n -- ^u8 n fd n )
   drop {: main:^ mainu r:fd :}
   main mainu r
   main mainu r REAPER-REQUEST SPAWN PID>N ;

public

\ Spawns a reaper on a fresh pipe and keeps its write end. A program missing
\ from the engine's tree throws E-PROC-PATH before any spawn.
: REAPER-START ( -- PROCESS:reaper )
   MAIN-PATH {: main:^ mainu :}
   main mainu FS:FILE? 0= if E-PROC-PATH throw then
   FD:PIPE {: r:fd w:fd :}
   main mainu r 0 [: REAPER-SPAWN ;] catch {: rc :}
   r FD>N close
   rc 0<> if 2drop 2drop w FD>N close rc throw then
   >PID {: pid:pid :} 2drop drop
   w pid REAPER-MAKE ;

\ The child joins the reaper's group before it execs, so the reaper's kill at
\ end of file reaches it.
: REAPED ( PROCESS:request PROCESS:reaper -- PROCESS:request )
   REAPER-UNMAKE nip {: pid:pid :}
   ERASE {: h:^ :}
   h pid [: SET-GROUP ;] catch {: rc :} drop drop
   rc h SETTLE
   h MINT BLOCK-HUP ;

\ Closes the write end and waits for the reaper, which kills its group first.
\ A reaper that ended other than by exit 0 throws E-PROC-WAIT after the wait.
: REAPER-STOP ( PROCESS:reaper -- )
   REAPER-UNMAKE {: w:fd pid:pid :}
   w FD>N close
   pid WAIT STATUS>RC RC>N 0<> if E-PROC-WAIT throw then ;

;package
