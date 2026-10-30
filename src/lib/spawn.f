\ spawn.f - a child-process request, built word by word and spawned once.
\
\ PROCESS:NEW starts a request for a program path; PROCESS:ARG, PROCESS:ENV,
\ PROCESS:HERMETIC, PROCESS:CWD, PROCESS:FD and PROCESS:CLOSED add to it;
\ PROCESS:SPAWN spawns the child through the engine's spawn-fds
\ (src/habu/spawn-abi.f lays out what it reads) and answers its pid;
\ PROCESS:DISCARD frees a request that will not be spawned.
\ PROCESS:ARGV-ROWS and PROCESS:ENV-ROWS bridge the RAW family's
\ NULL-terminated arrays; lane 7 deletes them along with the RAW words.
\
\ CONTRACT.
\ - argv: the path as argv[0], then a copy of each ARG's string, in order. An
\   ARGV-ROWS array is copied as given, argv[0] included, so a request whose
\   first rows come from one takes its argv[0] from the array. A request with no
\   argument spawns argv {path}.
\ - env: a copy of each ENV's name=value row, in order, then, unless HERMETIC,
\   this process's own environment rows, read at SPAWN. An inherited row whose
\   name a request row already gives is skipped, so the request's row wins. An
\   inherited name that the parent holds twice reaches the child once, as its
\   first row, because HAS-NAME? also checks the rows already inherited. A
\   HERMETIC request gives the child only its own rows. ENV refuses an empty
\   name and one holding `=` with E-PROC-ENV; ENV-ROWS copies its rows unchecked.
\ - cwd: the last CWD's directory, else this process's.
\ - A path, argument, environment name or value, or directory holding a 0 byte
\   cannot cross to C: E-FFI-NUL (src/lib/ffi-abi.f's CSTR).
\ - descriptors: the map starts [0 1 2]. `child parent FD` makes the child's fd
\   `child` the parent's fd `parent`, a child fd past the map growing it with
\   closed slots; CLOSED closes a slot inside the map and does nothing past it.
\   Every child fd at or past the map's end is closed.
\ - CLOSED PARENT STDIO. A slot of 0-2 that no FD set inherits the parent's fd
\   of that number when it is open and is closed when it is not, so a parent
\   running with a closed stdio fd still spawns. An FD naming a closed parent fd
\   fails the spawn with EBADF. PROCESS:CHECK-FD refuses a closed fd the same way
\   without a request, for a caller that must check before it opens descriptors
\   that could take the closed number.
\ - A request is used once. PROCESS:SPAWN frees it whatever the outcome, and a
\   builder that throws frees it before the throw goes on; a caller that will
\   not spawn a request DISCARDs it, on its throw paths too. A refused spawn
\   throws E-PROC-SPAWN with the positive errno in PROCESS:ERRNO; a spawn that
\   succeeds sets PROCESS:ERRNO to 0.
\ - The spawn holds FD-LOCK (src/lib/fd-lock.f).
\
\ STORAGE CLASS. CALLER-OWNED: each request maps its own record, string chunks
\ and vectors, and nothing it holds is shared with the process-wide argv and
\ env staging. An inherited row is the address of this process's own row, which
\ never moves. PROCESS:ERRNO is TASK-LOCAL, a TASK:+USER cell.

require src/lib/errors.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/ffi-abi.f
require src/lib/string.f
require src/lib/task.f
require src/lib/vector.f
require src/lib/fd-lock.f
require src/lib/process-chunks.f
require src/habu/spawn-abi.f

package PROCESS
public

DEFLINEAR PROCESS:request

private

TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and 8 TASK:+USER ERRNO-CELL drop

LINEAR: MINT ( ptr n -- PROCESS:request )
LINEAR: ERASE ( PROCESS:request -- ptr n )

CAST: ITEM>N ( NUM:item-count -- n )

\ fcntl's F_GETFD and the errno of a closed fd, on Darwin and Linux alike.
1 constant F-GETFD
9 constant EBADF

\ The byte that ends an environment row's name.
$3D constant EQUAL-BYTE

\ ---- the record ---------------------------------------------------------------
\ The SPAWN-ABI cells spawn-fds reads come first; PATH and CWD are written as
\ they are given, the rest just before the spawn. Then the owned string chunks,
\ the argv, envp and descriptor-map vectors, the mask of stdio slots an FD set
\ and the HERMETIC flag, 0 for a request that inherits.
SPAWN-ABI:BYTES 1 cells / constant ABI-CELLS
ABI-CELLS constant CHUNKS-AT
CHUNKS-AT PROC-CHUNKS:OWNED-CELLS + constant ARGS-AT
ARGS-AT VEC:HEADER-CELLS + constant ENVS-AT
ENVS-AT VEC:HEADER-CELLS + constant MAP-AT
MAP-AT VEC:HEADER-CELLS + constant SET-AT
SET-AT 1+ constant HERM-AT
HERM-AT 1+ constant RECORD-CELLS

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
: HERM ( ptr n -- ptr n ) HERM-AT cells + ;

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


\ A C-string copy of the bytes in the request's own chunks.
: ZCOPY ( ptr u8 n ptr n -- ptr u8 )
   {: src:ptr len h:ptr :}
   len FFI:CSTR-ROOM {: need :}
   src len need h CHUNKS PROC-CHUNKS:OWNED-ROOM need SPAN:MAKE FFI:CSTR ;


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


\ argv[0] is the path unless the first rows came from ARGV-ROWS.
: ADD-ARG ( ptr n ptr u8 n -- ptr n ptr u8 n )
   {: h:ptr a:ptr len :}
   len 0 < if E-PROC-OUTPUT throw then
   h ARGS VEC:LEN@ ITEM>N 0= if h PATH-CELL @ h ARGS VEC:PUSH drop then
   a len h ZCOPY h ARGS VEC:PUSH drop
   h a len ;


\ process-env.f mirrors this check and SAME-NAME?; lane 7 deletes it.
: CHECK-NAME ( ptr u8 n -- )
   {: a:ptr len :}
   len 0= if E-PROC-ENV throw then
   0 begin dup len < while
      dup a + c@ EQUAL-BYTE = if E-PROC-ENV throw then
      1+
   repeat drop ;


\ A C-string name=value copy in the request's own chunks.
: ADD-ENV ( ptr n ptr u8 n ptr u8 n -- ptr n ptr u8 n ptr u8 n )
   {: h:ptr name:ptr namelen val:ptr vallen :}
   namelen 0 < vallen 0 < or if E-PROC-ENV throw then
   name namelen CHECK-NAME
   namelen 1+ vallen + {: rowlen :}
   rowlen FFI:CSTR-ROOM {: need :}
   need h CHUNKS PROC-CHUNKS:OWNED-ROOM need SPAN:MAKE {: room :}
   name namelen room SPAN:COPY
   EQUAL-BYTE room namelen SPAN:U8!
   val vallen room namelen 1+ SPAN:SKIP SPAN:COPY
   rowlen room FFI:CSTR! h ENVS VEC:PUSH drop
   h name namelen val vallen ;


: ADD-ARGV-ROWS ( ptr n ptr ptr u8 -- ptr n ptr ptr u8 )
   2dup swap dup ARGS ROWS+ ;

: ADD-ENV-ROWS ( ptr n ptr ptr u8 -- ptr n ptr ptr u8 )
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


: SET-CLOSED ( ptr n fd -- ptr n fd )
   {: h:ptr fd :}
   fd FD>N 0 < if E-PROC-OUTPUT throw then
   fd FD>N h MAP-LEN < if -1 h fd FD>N MAP! then
   h fd ;


\ Whether this process's fd n is open.
: OPEN? ( n -- bool ) F-GETFD 0 fcntl 0 >= ;


\ A stdio slot no FD set keeps the parent's fd only while that fd is open.
: INHERIT-SLOT ( ptr n n -- )
   {: h:ptr slot :}
   h SET @ 1 slot lshift and 0<> if exit then
   h MAP slot VEC:N>INDEX VEC:@ {: parent :}
   parent 0 < if exit then
   parent OPEN? if exit then
   -1 h slot MAP! ;


\ The length of a NUL-terminated row's name: the bytes before its first `=`,
\ or the whole row.
: NAME-LEN ( ptr u8 -- n )
   {: z:ptr :}
   0 begin dup z + c@ dup 0<> swap EQUAL-BYTE <> and while 1+ repeat ;

: SAME-NAME? ( ptr u8 ptr u8 -- bool )
   {: a:ptr b:ptr :}
   a NAME-LEN {: alen :}
   b NAME-LEN {: blen :}
   alen blen <> if false exit then
   a alen b blen STR= ;

\ A request row names the variable of z. The vector is re-read each turn, as
\ PUSH may move it. It holds the rows already inherited too, so an inherited
\ name that the parent holds twice reaches the child once, as its first row.
: HAS-NAME? ( ptr n ptr u8 -- bool )
   {: h:ptr z:ptr :}
   h ENVS VEC:LEN@ ITEM>N 0 ?do
      h ENVS i VEC:N>INDEX VEC:@ z SAME-NAME? if true unloop exit then
   loop false ;


\ Inheritance appends the address of this process's own row, never a copy, and
\ skips a row whose name a request row already gives.
: INHERIT-ROW ( ptr n ptr u8 -- )
   {: h:ptr z:ptr :}
   h z HAS-NAME? if exit then
   z h ENVS VEC:PUSH drop ;

: INHERIT-PARENT ( ptr n -- )
   {: h:ptr :}
   0 begin dup ENVP NULL-PTR - 0<> while
      h over ENVP INHERIT-ROW
      1+
   repeat drop ;


: LAUNCH ( ptr n -- n )
   {: h:ptr :}
   h ARGS VEC:LEN@ ITEM>N 0= if h PATH-CELL @ h ARGS VEC:PUSH drop then
   NULL-PTR h ARGS VEC:PUSH drop
   h HERM @ 0= if h INHERIT-PARENT then
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


\ A request for the program at path, map [0 1 2], no argument, this process's
\ environment and directory. A negative length: E-PROC-PATH.
: NEW ( ptr u8 n -- PROCESS:request )
   {: path:ptr len :}
   len 0 < if E-PROC-PATH throw then
   RECORD-CELLS MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS {: h:ptr :}
   h path len [: START ;] catch {: rc :} drop drop drop
   rc h SETTLE
   h MINT ;


\ The child gets only the rows the request adds.
: HERMETIC ( PROCESS:request -- PROCESS:request )
   ERASE {: h:ptr :}
   1 h HERM !
   h MINT ;


\ Appends a copy of one argument; the first is argv[1]. A negative length:
\ E-PROC-OUTPUT.
: ARG ( PROCESS:request ptr u8 n -- PROCESS:request )
   rot ERASE {: a:ptr len h:ptr :}
   h a len [: ADD-ARG ;] catch {: rc :} drop drop drop
   rc h SETTLE
   h MINT ;


\ Appends a copy of the row name=value. A negative length, an empty name or one
\ holding `=`: E-PROC-ENV.
: ENV ( PROCESS:request ptr u8 n ptr u8 n -- PROCESS:request )
   {: name:ptr namelen val:ptr vallen :}
   ERASE {: h:ptr :}
   h name namelen val vallen [: ADD-ENV ;] catch {: rc :} drop drop drop drop drop
   rc h SETTLE
   h MINT ;


\ The child's directory; a later call replaces it. An empty one: E-PROC-PATH.
: CWD ( PROCESS:request ptr u8 n -- PROCESS:request )
   rot ERASE {: dir:ptr len h:ptr :}
   h dir len [: SET-CWD ;] catch {: rc :} drop drop drop
   rc h SETTLE
   h MINT ;


\ The child's fd child is the parent's fd parent. Either negative: E-PROC-OUTPUT.
: FD ( PROCESS:request fd fd -- PROCESS:request )
   rot ERASE {: child parent h:ptr :}
   h child parent [: SET-FD ;] catch {: rc :} drop drop drop
   rc h SETTLE
   h MINT ;


\ The child's fd starts closed. A negative fd: E-PROC-OUTPUT.
: CLOSED ( PROCESS:request fd -- PROCESS:request )
   swap ERASE {: fd h:ptr :}
   h fd [: SET-CLOSED ;] catch {: rc :} drop drop
   rc h SETTLE
   h MINT ;


\ Appends a copy of each string of a NULL-terminated array to argv. A RAW-family
\ bridge.
: ARGV-ROWS ( PROCESS:request ptr ptr u8 -- PROCESS:request )
   swap ERASE {: rows:ptr h:ptr :}
   h rows [: ADD-ARGV-ROWS ;] catch {: rc :} drop drop
   rc h SETTLE
   h MINT ;


\ Appends a copy of each row of a NULL-terminated array to the environment. A
\ RAW-family bridge.
: ENV-ROWS ( PROCESS:request ptr ptr u8 -- PROCESS:request )
   swap ERASE {: rows:ptr h:ptr :}
   h rows [: ADD-ENV-ROWS ;] catch {: rc :} drop drop
   rc h SETTLE
   h MINT ;


\ Refuses a closed parent fd as SPAWN refuses an FD naming one: E-PROC-SPAWN,
\ EBADF in ERRNO. An open fd passes and leaves ERRNO as it was.
: CHECK-FD ( fd -- )
   FD>N OPEN? if exit then
   EBADF ERRNO-CELL !
   E-PROC-SPAWN throw ;


\ Spawns the request and frees it. A refusal throws E-PROC-SPAWN, its errno
\ in ERRNO.
: SPAWN ( PROCESS:request -- pid )
   ERASE {: h:ptr :}
   h 0 [: drop dup LAUNCH ;] catch {: rc :}
   rc 0<> if drop drop h RELEASE rc throw then
   nip {: result :}
   h RELEASE
   result 0 < if result negate ERRNO-CELL ! E-PROC-SPAWN throw then
   0 ERRNO-CELL !
   result >PID ;


\ Frees a request that will not be spawned and everything it holds.
: DISCARD ( PROCESS:request -- ) ERASE RELEASE ;

;package
