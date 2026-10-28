\ spawn.f - a child-process request, built word by word and spawned once.
\
\ SPAWN:NEW starts a request for a program path; SPAWN:ARGV, SPAWN:ENV,
\ SPAWN:CWD, SPAWN:FD and SPAWN:CLOSE add to it; SPAWN:SPAWN, published as
\ PROCESS:SPAWN too, spawns the child through the engine's spawn-fds
\ (src/habu/spawn-abi.f lays out what it reads) and answers its pid.
\
\ CONTRACT.
\ - argv: a copy of every string each ARGV array holds before its NULL, in
\   order, argv[0] included. A request with no argument spawns argv {path}.
\ - env: a copy of every row each ENV array holds; with none the child gets an
\   empty environment.
\ - cwd: the last CWD's directory, else this process's.
\ - descriptors: the map starts [0 1 2]. `child parent FD` makes the child's fd
\   `child` the parent's fd `parent`, a child fd past the map growing it with
\   closed slots; CLOSE closes a slot inside the map and does nothing past it.
\   Every child fd at or past the map's end is closed.
\ - CLOSED PARENT STDIO. A slot of 0-2 that no FD set inherits the parent's fd
\   of that number when it is open and is closed when it is not, so a parent
\   running with a closed stdio fd still spawns. An FD naming a closed parent fd
\   fails the spawn with EBADF.
\ - A request is used once. SPAWN:SPAWN frees it whatever the outcome, and a
\   builder that throws frees it before the throw goes on; a request a caller
\   drops on its own throw is lost. A refused spawn throws E-PROC-SPAWN with
\   the positive errno in SPAWN:ERRNO; a spawn that succeeds sets SPAWN:ERRNO
\   to 0.
\ - The spawn holds FD-LOCK (src/lib/fd-lock.f).
\
\ STORAGE CLASS. CALLER-OWNED: each request maps its own record, string chunks
\ and vectors, and nothing it holds is shared with the process-wide argv and
\ env staging. SPAWN:ERRNO is TASK-LOCAL, a TASK:+USER cell.

require src/lib/errors.f
require src/lib/memory.f
require src/lib/task.f
require src/lib/vector.f
require src/lib/fd-lock.f
require src/lib/process-chunks.f
require src/habu/spawn-abi.f

package SPAWN
public

DEFLINEAR SPAWN:request

private

TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and 8 TASK:+USER ERRNO-CELL drop

LINEAR: MINT ( ptr n -- SPAWN:request )
LINEAR: ERASE ( SPAWN:request -- ptr n )

CAST: ITEM>N ( NUM:item-count -- n )

\ fcntl's F_GETFD on Darwin and Linux alike.
1 constant F-GETFD

\ ---- the record ---------------------------------------------------------------
\ The SPAWN-ABI cells spawn-fds reads come first; PATH and CWD are written as
\ they are given, the rest just before the spawn. Then the owned string chunks,
\ the argv, envp and descriptor-map vectors, and the mask of stdio slots an FD
\ set.
SPAWN-ABI:BYTES 1 cells / constant ABI-CELLS
ABI-CELLS constant CHUNKS-AT
CHUNKS-AT PROC-CHUNKS:OWNED-CELLS + constant ARGS-AT
ARGS-AT VEC:HEADER-CELLS + constant ENVS-AT
ENVS-AT VEC:HEADER-CELLS + constant MAP-AT
MAP-AT VEC:HEADER-CELLS + constant SET-AT
SET-AT 1+ constant RECORD-CELLS

: PATH-CELL ( ptr n -- ptr ptr u8 ) SPAWN-ABI:PATH 1 cells / ptr-field ;
: ARGV-CELL ( ptr n -- ptr ptr a ) SPAWN-ABI:ARGV 1 cells / ptr-field ;
: ENVP-CELL ( ptr n -- ptr ptr a ) SPAWN-ABI:ENVP 1 cells / ptr-field ;
: CWD-CELL ( ptr n -- ptr ptr u8 ) SPAWN-ABI:CWD 1 cells / ptr-field ;
: FDS-CELL ( ptr n -- ptr ptr a ) SPAWN-ABI:FDS 1 cells / ptr-field ;
: NFDS-CELL ( ptr n -- ptr n ) SPAWN-ABI:NFDS + ;
: CHUNKS ( ptr n -- ptr n ) CHUNKS-AT cells + ;
: ARGS ( ptr n -- ptr ptr u8 ) ARGS-AT ptr-field ;
: ENVS ( ptr n -- ptr ptr u8 ) ENVS-AT ptr-field ;
: MAP ( ptr n -- ptr n ) MAP-AT cells + ;
: SET ( ptr n -- ptr n ) SET-AT cells + ;

: MAP-LEN ( ptr n -- n ) MAP VEC:LEN@ ITEM>N ;
: MAP! ( n ptr n n -- ) VEC:N>INDEX swap MAP swap VEC:! ;


\ A member never initialised is a dead header, which VEC:DISPOSE ignores.
: RELEASE ( ptr n -- )
   {: h:ptr :}
   h CHUNKS PROC-CHUNKS:OWNED-FREE
   h ARGS VEC:DISPOSE
   h ENVS VEC:DISPOSE
   h MAP VEC:DISPOSE
   h RECORD-CELLS MEM:CELLS-ALLOC-COUNT MEM:RELEASE-CELLS ;


\ The end of a builder's catch: a throw frees the request and goes on.
: SETTLE ( n ptr n -- )
   {: rc h:ptr :}
   rc 0= if exit then
   h RELEASE
   rc throw ;


\ A NUL-terminated copy of the bytes in the request's own chunks.
: ZCOPY ( ptr u8 n ptr n -- ptr u8 )
   {: src:ptr len h:ptr :}
   len 1+ h CHUNKS PROC-CHUNKS:OWNED-ROOM {: dst:ptr :}
   src dst len BYTE-COPY
   0 dst len + c!
   dst ;


\ Pushes a copy of every string before the array's NULL.
: ROWS+ ( ptr ptr u8 ptr n ptr ptr u8 -- )
   {: rows:ptr h:ptr vec:ptr :}
   0 begin rows over cells + @ dup NULL-PTR - 0<> while
      dup ZLEN h ZCOPY vec VEC:PUSH drop
      1+
   repeat 2drop ;


: START ( ptr n ptr u8 n -- ptr n ptr u8 n )
   {: h:ptr path:ptr len :}
   h ARGS 4 VEC:N>ITEM VEC:INIT
   h ENVS 4 VEC:N>ITEM VEC:INIT
   h MAP 4 VEC:N>ITEM VEC:INIT
   0 h MAP VEC:PUSH drop
   1 h MAP VEC:PUSH drop
   2 h MAP VEC:PUSH drop
   path len h ZCOPY h PATH-CELL !
   h path len ;


: ADD-ARGV ( ptr n ptr ptr u8 -- ptr n ptr ptr u8 )
   2dup swap dup ARGS ROWS+ ;

: ADD-ENV ( ptr n ptr ptr u8 -- ptr n ptr ptr u8 )
   2dup swap dup ENVS ROWS+ ;


: SET-CWD ( ptr n ptr u8 n -- ptr n ptr u8 n )
   {: h:ptr dir:ptr len :}
   len 0 <= if E-PROC-PATH throw then
   dir len h ZCOPY h CWD-CELL !
   h dir len ;


: SET-FD ( ptr n fd fd -- ptr n fd fd )
   {: h:ptr child parent :}
   child FD>N 0 < parent FD>N 0 < or if E-PROC-OUTPUT throw then
   begin h MAP-LEN child FD>N <= while -1 h MAP VEC:PUSH drop repeat
   parent FD>N h child FD>N MAP!
   child FD>N 3 < if h SET @ 1 child FD>N lshift or h SET ! then
   h child parent ;


: SET-CLOSE ( ptr n fd -- ptr n fd )
   {: h:ptr fd :}
   fd FD>N 0 < if E-PROC-OUTPUT throw then
   fd FD>N h MAP-LEN < if -1 h fd FD>N MAP! then
   h fd ;


\ A stdio slot no FD set keeps the parent's fd only while that fd is open.
: INHERIT-SLOT ( ptr n n -- )
   {: h:ptr slot :}
   h SET @ 1 slot lshift and 0<> if exit then
   h MAP slot VEC:N>INDEX VEC:@ {: parent :}
   parent 0 < if exit then
   parent F-GETFD 0 fcntl 0 >= if exit then
   -1 h slot MAP! ;


: LAUNCH ( ptr n -- n )
   {: h:ptr :}
   h ARGS VEC:LEN@ ITEM>N 0= if h PATH-CELL @ h ARGS VEC:PUSH drop then
   NULL-PTR h ARGS VEC:PUSH drop
   NULL-PTR h ENVS VEC:PUSH drop
   3 0 do h i INHERIT-SLOT loop
   h ARGS VEC:DATA@ h ARGV-CELL !
   h ENVS VEC:DATA@ h ENVP-CELL !
   h MAP VEC:DATA@ h FDS-CELL !
   h MAP-LEN h NFDS-CELL !
   h [: spawn-fds ;] FD-LOCK:WITH ;

public

\ Positive errno of this task's last refused spawn; 0 after one that succeeded.
: ERRNO ( -- n ) ERRNO-CELL @ ;


\ A request for the program at path, map [0 1 2], no argument, an empty
\ environment and this process's directory. A negative length: E-PROC-PATH.
: NEW ( ptr u8 n -- SPAWN:request )
   {: path:ptr len :}
   len 0 < if E-PROC-PATH throw then
   RECORD-CELLS MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS {: h:ptr :}
   h path len [: START ;] catch {: rc :} drop drop drop
   rc h SETTLE
   h MINT ;


\ Appends a copy of each string of a NULL-terminated array to argv.
: ARGV ( SPAWN:request ptr ptr u8 -- SPAWN:request )
   swap ERASE {: rows:ptr h:ptr :}
   h rows [: ADD-ARGV ;] catch {: rc :} drop drop
   rc h SETTLE
   h MINT ;


\ Appends a copy of each row of a NULL-terminated array to the environment.
: ENV ( SPAWN:request ptr ptr u8 -- SPAWN:request )
   swap ERASE {: rows:ptr h:ptr :}
   h rows [: ADD-ENV ;] catch {: rc :} drop drop
   rc h SETTLE
   h MINT ;


\ The child's directory; a later call replaces it. An empty one: E-PROC-PATH.
: CWD ( SPAWN:request ptr u8 n -- SPAWN:request )
   rot ERASE {: dir:ptr len h:ptr :}
   h dir len [: SET-CWD ;] catch {: rc :} drop drop drop
   rc h SETTLE
   h MINT ;


\ The child's fd child is the parent's fd parent. Either negative: E-PROC-OUTPUT.
: FD ( SPAWN:request fd fd -- SPAWN:request )
   rot ERASE {: child parent h:ptr :}
   h child parent [: SET-FD ;] catch {: rc :} drop drop drop
   rc h SETTLE
   h MINT ;


\ The child's fd starts closed. A negative fd: E-PROC-OUTPUT.
: CLOSE ( SPAWN:request fd -- SPAWN:request )
   swap ERASE {: fd h:ptr :}
   h fd [: SET-CLOSE ;] catch {: rc :} drop drop
   rc h SETTLE
   h MINT ;


\ Spawns the request and frees it. A refusal throws E-PROC-SPAWN, its errno
\ in ERRNO.
: SPAWN ( SPAWN:request -- pid )
   ERASE {: h:ptr :}
   h 0 [: drop dup LAUNCH ;] catch {: rc :}
   rc 0<> if drop drop h RELEASE rc throw then
   nip {: result :}
   h RELEASE
   result 0 < if result negate ERRNO-CELL ! E-PROC-SPAWN throw then
   0 ERRNO-CELL !
   result >PID ;

;package

package PROCESS
public
EXPORT SPAWN:SPAWN
;package
