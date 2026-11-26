\ process-reaper.f - a pool's reaper: the pool's own program, started with
\ MARK$ as argv[0], which leads a process group of its own and holds the read
\ end of a pipe whose write end only the pool keeps. A child spawned REAPED is
\ in that group from its first instruction. When the write end closes, by
\ REAPER-STOP or by the pool's death, SIGKILL included, the reaper reads end of
\ file and kills every other process in its group, each with its tree.
\ A child that leaves the group, by setsid or setpgid, is out of reach.

require src/core/exit-status.f
require src/lib/error.f
require src/lib/vector.f
require src/lib/fd.f
require src/lib/spawn.f
require src/lib/process.f
require src/lib/process-tree.f

package PROCESS
public

\ A reaper's pipe write end and pid; plain data, not linear.
STRUCTURE reaper OPAQUE
   FIELD writer fd
   FIELD leader pid
;STRUCTURE

private

1 constant HUP-BIT                       \ SIGHUP's bit, signal 1 on both hosts
3 constant WATCH-FD
64 constant CHUNK
CHUNK SCRATCH-BUFFER: SINK

\ A reaper's argv[0], which a program never reads as an argument.
: MARK$ ( -- str ) s" habu-reaper" ;

: DRAIN ( -- )
   begin WATCH-FD SINK CHUNK SPAN:MAKE read 0 > while repeat ;

: REAP ( -- )
   DRAIN
   PROC-TREE:KILL-PEERS ;

: ADD-MARK ( ^n -- ^n )
   {: h:^ :}
   MARK$ h CHUNK-ZCOPY h ARGS @ VEC:PUSH !
   h ;

\ MARK$ as the request's argv[0], in place of its path; call it before any ARG.
: MARKED ( PROCESS:request -- PROCESS:request )
   ERASE {: h:^ :}
   h [: ADD-MARK ;] catch {: rc :} drop
   rc h SETTLE
   h MINT ;

\ The pool's death orphans the reaper's group, which the kernel then sends
\ SIGHUP if a member is stopped; the child starts with SIGHUP blocked.
: BLOCK-HUP ( PROCESS:request -- PROCESS:request )
   ERASE {: h:^ :}
   h BLOCKED-CELL @ HUP-BIT or h BLOCKED-CELL !
   h MINT ;

\ This program as a reaper, the pipe's read end its fd 3, fds 0 and 1 closed.
: REAPER-REQUEST ( fd -- PROCESS:request )
   {: r:fd :}
   OWN-PATH NEW MARKED BLOCK-HUP
   WATCH-FD >FD r FD
   0 >FD CLOSED
   1 >FD CLOSED ;

\ The top cell, a placeholder, carries the pid out, so catch keeps the depth.
: REAPER-SPAWN ( fd n -- fd n )
   drop {: r:fd :}
   r
   r REAPER-REQUEST SPAWN PID>N ;

public

\ An entry calls this first: with MARK$ as argv[0] it runs the reaper and exits,
\ 0 once its group is killed; with any other argv[0] it returns.
: REAPER-ENTRY ( -- )
   ARGC 1 < if exit then
   0 ARGV$ MARK$ STR:= 0= if exit then
   [: REAP ;] ERROR:TOP-LEVEL
   NULL$ EXIT-STATUS:SUCCESS die ;

\ Spawns this program as a reaper on a fresh pipe and keeps the write end.
: REAPER-START ( -- PROCESS:reaper )
   FD:PIPE {: r:fd w:fd :}
   r 0 [: REAPER-SPAWN ;] catch {: rc :}
   r FD>N close
   rc 0<> if drop drop w FD>N close rc throw then
   >PID {: pid:pid :} drop
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
