split-str
\ spawn.f - a child-process request, built word by word and spawned once:
\ PROCESS:NEW starts one for a program path, the builders add to it, PROCESS:SPAWN
\ spawns it through the engine's spawn-fds (src/habu/spawn-abi.f lays out what it
\ reads) under FD-LOCK (src/lib/fd-lock.f) and answers its pid.
\ STORAGE CLASS. CALLER-OWNED: each request maps its own record, string chunks and
\ vectors, shared with no process-wide staging. PROCESS:ERRNO is a TASK:+USER slot.

require src/lib/error.f
require src/core/compile-reads.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/ffi-abi.f
require src/lib/string.f
require src/lib/task.f
require src/lib/vector.f
require src/lib/heap.f
require src/lib/fd-lock.f
require src/lib/process-chunks.f
require src/habu/spawn-abi.f

package PROCESS
public

DEFLINEAR PROCESS:request

private

TASK:+USER ERRNO-CELL n

LINEAR: MINT ( ^n -- PROCESS:request )
LINEAR: ERASE ( PROCESS:request -- ^n )

\ fcntl's F_GETFD and the errno of a closed fd, on Darwin and Linux alike.
1 constant F-GETFD
9 constant EBADF

\ The byte that ends an environment row's name.
$3D constant EQUAL-BYTE

STRUCTURE vectors DERIVE addr
   FIELD args ^VEC:vector<^u8>
   FIELD envs ^VEC:vector<^u8>
   FIELD map ^VEC:vector<n>
;STRUCTURE

CAST: >VECTORS ( ^n -- ^vectors )

\ The SPAWN-ABI cells spawn-fds reads come first: PATH, CWD, PGROUP
\ and BLOCKED written as given, the rest just before the spawn.
SPAWN-ABI:BYTES 1 cells / constant ABI-CELLS
\ The owned string chunks.
ABI-CELLS constant CHUNKS-AT
CHUNKS-AT PROC-CHUNKS:OWNED-CELLS + constant ARGS-AT
\ The mask of stdio slots an FD set.
ARGS-AT VECTORS-CELLS + constant SET-AT
\ The HERMETIC flag, 0 for a request that inherits.
SET-AT 1+ constant HERM-AT
HERM-AT 1+ constant RECORD-CELLS

: PATH-CELL ( ^n -- ^^u8 ) SPAWN-ABI:PATH 1 cells / ptr-field ;
: ARGV-CELL ( ^n -- ^^a ) SPAWN-ABI:ARGV 1 cells / ptr-field ;
: ENVP-CELL ( ^n -- ^^a ) SPAWN-ABI:ENVP 1 cells / ptr-field ;
: CWD-CELL ( ^n -- ^^u8 ) SPAWN-ABI:CWD 1 cells / ptr-field ;
: FDS-CELL ( ^n -- ^^a ) SPAWN-ABI:FDS 1 cells / ptr-field ;
: NFDS-CELL ( ^n -- ^n ) SPAWN-ABI:NFDS + ;
: PGROUP-CELL ( ^n -- ^n ) SPAWN-ABI:PGROUP + ;
: BLOCKED-CELL ( ^n -- ^n ) SPAWN-ABI:BLOCKED + ;
: CHUNKS ( ^n -- ^n ) CHUNKS-AT cells + ;
: ARGS ( ^n -- ^^VEC:vector<^u8> )
   ARGS-AT cells + >VECTORS VECTORS-ARGS ;
: ENVS ( ^n -- ^^VEC:vector<^u8> )
   ARGS-AT cells + >VECTORS VECTORS-ENVS ;
: MAP ( ^n -- ^^VEC:vector<n> )
   ARGS-AT cells + >VECTORS VECTORS-MAP ;
: SET ( ^n -- ^n ) SET-AT cells + ;
: HERM ( ^n -- ^n ) HERM-AT cells + ;

: MAP-LEN ( ^n -- n ) MAP @ VEC:COUNT ;
: MAP! ( n ^n n -- ) swap MAP @ swap VEC:AT ! ;


\ A vector never made is NULL-PTR in its cell, which VEC:DELETE ignores.
: RELEASE ( ^n -- )
   {: h:^ :}
   h CHUNKS PROC-CHUNKS:OWNED-FREE
   h ARGS @ VEC:DELETE
   h ENVS @ VEC:DELETE
   h MAP @ VEC:DELETE
   h RECORD-CELLS MEM:CELLS-ALLOC-COUNT MEM:RELEASE-CELLS ;


\ The end of a builder's catch: a throw frees the request and goes on.
: SETTLE ( n ^n -- )
   {: rc h:^ :}
   rc 0= if exit then
   h RELEASE
   rc throw ;


\ A C-string copy of the bytes in the request's own chunks. This and ADD-ENV's
\ FFI:CSTR! refuse a path, argument, row or directory holding a 0 byte:
\ E-FFI-NUL.
: CHUNK-ZCOPY ( str ^n -- ^u8 )
   {: src:^ len h:^ :}
   len FFI:CSTR-ROOM {: need :}
   src len need h CHUNKS PROC-CHUNKS:OWNED-ROOM need SPAN:MAKE FFI:CSTR ;


\ Pushes a copy of every string before the array's NULL.
: ROWS+ ( ^^u8 ^n ^VEC:vector<^u8> -- )
   {: rows:^ h:^ vec:^ :}
   0 begin rows over cells + @ dup NULL-PTR <> while
      dup ZLEN h CHUNK-ZCOPY vec VEC:PUSH !
      1+
   repeat 2drop ;


: START ( ^n str -- ^n str )
   {: h:^ path:^ len :}
   HEAP:ALLOCATOR 0 [type] ^u8 VEC:NEW h ARGS !
   HEAP:ALLOCATOR 0 [type] ^u8 VEC:NEW h ENVS !
   HEAP:ALLOCATOR 0 [type] n VEC:NEW h MAP !
   0 h MAP @ VEC:PUSH !
   1 h MAP @ VEC:PUSH !
   2 h MAP @ VEC:PUSH !
   path len h CHUNK-ZCOPY h PATH-CELL !
   h path len ;


\ argv[0] is the path unless the first rows came from ARGV-ROWS.
: ADD-ARG ( ^n str -- ^n str )
   {: h:^ a:^ len :}
   len 0 < if E-PROC-OUTPUT throw then
   h ARGS @ VEC:COUNT 0= if h PATH-CELL @ h ARGS @ VEC:PUSH ! then
   a len h CHUNK-ZCOPY h ARGS @ VEC:PUSH !
   h a len ;


\ process-env.f mirrors this check and SAME-NAME?.
: CHECK-NAME ( str -- )
   {: a:^ len :}
   len 0= if E-PROC-ENV throw then
   0 begin dup len < while
      dup a + c@ EQUAL-BYTE = if E-PROC-ENV throw then
      1+
   repeat drop ;


\ A C-string name=value copy in the request's own chunks.
: ADD-ENV ( ^n str str -- ^n str str )
   {: h:^ name:^ namelen val:^ vallen :}
   namelen 0 < vallen 0 < or if E-PROC-ENV throw then
   name namelen CHECK-NAME
   namelen 1+ vallen + {: rowlen :}
   rowlen FFI:CSTR-ROOM {: need :}
   need h CHUNKS PROC-CHUNKS:OWNED-ROOM need SPAN:MAKE {: room :}
   name namelen room SPAN:COPY
   EQUAL-BYTE room namelen SPAN:U8!
   val vallen room namelen 1+ SPAN:SKIP SPAN:COPY
   rowlen room FFI:CSTR! h ENVS @ VEC:PUSH !
   h name namelen val vallen ;


: ADD-ARGV-ROWS ( ^n ^^u8 -- ^n ^^u8 )
   2dup swap dup ARGS @ ROWS+ ;

: ADD-ENV-ROWS ( ^n ^^u8 -- ^n ^^u8 )
   2dup swap dup ENVS @ ROWS+ ;


: SET-CWD ( ^n str -- ^n str )
   {: h:^ dir:^ len :}
   len 0 <= if E-PROC-PATH throw then
   dir len h CHUNK-ZCOPY h CWD-CELL !
   h dir len ;


: SET-FD ( ^n fd fd -- ^n fd fd )
   {: h:^ child parent :}
   child FD>N 0 < parent FD>N 0 < or if E-PROC-OUTPUT throw then
   begin h MAP-LEN child FD>N <= while -1 h MAP @ VEC:PUSH ! repeat
   parent FD>N h child FD>N MAP!
   child FD>N 3 < if h SET @ 1 child FD>N lshift or h SET ! then
   h child parent ;


: SET-CLOSED ( ^n fd -- ^n fd )
   {: h:^ fd :}
   fd FD>N 0 < if E-PROC-OUTPUT throw then
   fd FD>N h MAP-LEN < if -1 h fd FD>N MAP! then
   h fd ;


: SET-GROUP ( ^n pid -- ^n pid )
   {: h:^ pid:pid :}
   pid PID>N 1 <= if E-PROC-OUTPUT throw then
   pid PID>N h PGROUP-CELL !
   h pid ;


: OPEN? ( n -- bool ) F-GETFD 0 fcntl 0 >= ;


\ A stdio slot no FD set keeps the parent's fd only while that fd is open, so a
\ parent running with a closed stdio fd still spawns; an FD naming a closed
\ parent fd fails the spawn with EBADF.
: INHERIT-SLOT ( ^n n -- )
   {: h:^ slot :}
   h SET @ 1 slot lshift and 0<> if exit then
   h MAP @ slot VEC:AT @ {: parent :}
   parent 0 < if exit then
   parent OPEN? if exit then
   -1 h slot MAP! ;


\ The length of a NUL-terminated row's name: the bytes before its first `=`,
\ or the whole row.
: NAME-LEN ( ^u8 -- n )
   {: z:^ :}
   0 begin dup z + c@ dup 0<> swap EQUAL-BYTE <> and while 1+ repeat ;

: SAME-NAME? ( ^u8 ^u8 -- bool )
   {: a:^ b:^ :}
   a NAME-LEN {: alen :}
   b NAME-LEN {: blen :}
   alen blen <> if false exit then
   a alen b blen STR= ;

\ A request row names the variable of z. The vector is re-read each turn, as
\ PUSH may move it. It holds the rows already inherited too, so an inherited
\ name that the parent holds twice reaches the child once, as its first row.
: HAS-NAME? ( ^n ^u8 -- bool )
   {: h:^ z:^ :}
   h ENVS @ VEC:COUNT 0 ?do
      h ENVS @ i VEC:AT @ z SAME-NAME? if true unloop exit then
   loop false ;


\ Inheritance appends the address of this process's own row, which never moves,
\ and skips a row whose name a request row already gives.
: INHERIT-ROW ( ^n ^u8 -- )
   {: h:^ z:^ :}
   h z HAS-NAME? if exit then
   z h ENVS @ VEC:PUSH ! ;

: INHERIT-PARENT ( ^n -- )
   {: h:^ :}
   0 begin dup ENVP NULL-PTR <> while
      h over ENVP INHERIT-ROW
      1+
   repeat drop ;


: LAUNCH ( ^n -- n )
   {: h:^ :}
   h ARGS @ VEC:COUNT 0= if h PATH-CELL @ h ARGS @ VEC:PUSH ! then
   NULL-PTR h ARGS @ VEC:PUSH !
   h HERM @ 0= if h INHERIT-PARENT then
   NULL-PTR h ENVS @ VEC:PUSH !
   3 0 do h i INHERIT-SLOT loop
   h ARGS @ 0 VEC:AT h ARGV-CELL !
   h ENVS @ 0 VEC:AT h ENVP-CELL !
   h MAP @ 0 VEC:AT h FDS-CELL !
   h MAP-LEN h NFDS-CELL !
   h [: spawn-fds ;] FD-LOCK:WITH ;

public

\ Positive errno of this task's last refused spawn; 0 after one that succeeded.
: ERRNO ( -- n ) ERRNO-CELL @ ;


\ A request for the program at path, map [0 1 2], no argument, this process's
\ environment and directory. A negative length: E-PROC-PATH.
: NEW ( str -- PROCESS:request )
   {: path:^ len :}
   len 0 < if E-PROC-PATH throw then
   RECORD-CELLS MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS {: h:^ :}
   h path len [: START ;] catch {: rc :} drop drop drop
   rc h SETTLE
   h MINT ;


\ The child gets only the rows the request adds.
: HERMETIC ( PROCESS:request -- PROCESS:request )
   ERASE {: h:^ :}
   1 h HERM !
   h MINT ;


\ Appends a copy of one argument; the first is argv[1], and a request with none
\ spawns argv {path}. A negative length: E-PROC-OUTPUT.
: ARG ( PROCESS:request str -- PROCESS:request )
   rot ERASE {: a:^ len h:^ :}
   h a len [: ADD-ARG ;] catch {: rc :} drop drop drop
   rc h SETTLE
   h MINT ;


\ Appends a copy of the row name=value. The child gets the request's rows in
\ order, then, unless HERMETIC, this process's rows read at SPAWN, skipping a name
\ a request row gives. A negative length, an empty name or one holding `=`:
\ E-PROC-ENV.
: ENV ( PROCESS:request str str -- PROCESS:request )
   {: name:^ namelen val:^ vallen :}
   ERASE {: h:^ :}
   h name namelen val vallen [: ADD-ENV ;] catch {: rc :} drop drop drop drop drop
   rc h SETTLE
   h MINT ;


\ The child's directory; a later call replaces it. An empty one: E-PROC-PATH.
: CWD ( PROCESS:request str -- PROCESS:request )
   rot ERASE {: dir:^ len h:^ :}
   h dir len [: SET-CWD ;] catch {: rc :} drop drop drop
   rc h SETTLE
   h MINT ;


\ The child's fd child is the parent's fd parent; the map starts [0 1 2], and a
\ child fd past it grows it with closed slots. Every child fd at or past the map's
\ end is closed. Either negative: E-PROC-OUTPUT.
: FD ( PROCESS:request fd fd -- PROCESS:request )
   rot ERASE {: child parent h:^ :}
   h child parent [: SET-FD ;] catch {: rc :} drop drop drop
   rc h SETTLE
   h MINT ;


\ The child's fd starts closed; past the map's end this does nothing. A negative
\ fd: E-PROC-OUTPUT.
: CLOSED ( PROCESS:request fd -- PROCESS:request )
   swap ERASE {: fd h:^ :}
   h fd [: SET-CLOSED ;] catch {: rc :} drop drop
   rc h SETTLE
   h MINT ;


\ Appends a copy of each string of a NULL-terminated array to argv, argv[0]
\ included, so a request whose first rows come from one takes its argv[0] from
\ it. A RAW-family bridge.
: ARGV-ROWS ( PROCESS:request ^^u8 -- PROCESS:request )
   swap ERASE {: rows:^ h:^ :}
   h rows [: ADD-ARGV-ROWS ;] catch {: rc :} drop drop
   rc h SETTLE
   h MINT ;


\ Appends a copy of each row of a NULL-terminated array to the environment,
\ unchecked. A RAW-family bridge.
: ENV-ROWS ( PROCESS:request ^^u8 -- PROCESS:request )
   swap ERASE {: rows:^ h:^ :}
   h rows [: ADD-ENV-ROWS ;] catch {: rc :} drop drop
   rc h SETTLE
   h MINT ;


\ Refuses a closed parent fd as SPAWN refuses an FD naming one: E-PROC-SPAWN,
\ EBADF in ERRNO, for a caller that checks before it opens descriptors that
\ could take the closed number. An open fd passes and leaves ERRNO as it was.
: CHECK-FD ( fd -- )
   FD>N OPEN? if exit then
   EBADF ERRNO-CELL !
   E-PROC-SPAWN throw ;


\ Spawns the request and frees it whatever the outcome. A refusal throws
\ E-PROC-SPAWN with the positive errno in ERRNO; a spawn that succeeds sets it 0.
: SPAWN ( PROCESS:request -- pid )
   COMPILE-READS:MISS
   ERASE {: h:^ :}
   [: h LAUNCH ;] catch {: result rc :}
   rc 0<> if h RELEASE rc throw then
   h RELEASE
   result 0 < if result negate ERRNO-CELL ! E-PROC-SPAWN throw then
   0 ERRNO-CELL !
   result >PID ;


\ Frees a request that will not be spawned and everything it holds; a caller
\ DISCARDs on its throw paths too.
: DISCARD ( PROCESS:request -- ) ERASE RELEASE ;

;package
