\ aio-macos.f - Darwin backend for src/lib/aio.f's records (fields in REC.HOST).
\ One poll thread services all records; a nonblocking pipe wakes it when a
\ submission changes the descriptor set or the nearest deadline. The caller
\ keeps a pending operation's descriptor open and its flags unchanged. Regular
\ files use pread/pwrite on the poll thread, so slow disk I/O delays other
\ completions.
require src/lib/string.f
require src/lib/aio.f
require src/lib/le.f
require src/lib/fd-lock.f                \ FD-LOCK:WITH orders the wake pipe against spawns

package AIO
private

35 constant MAC-AGAIN
36 constant MAC-INPROGRESS
37 constant MAC-ALREADY
9 constant MAC-BADF
22 constant MAC-INVAL
29 constant MAC-SPIPE
4 constant MAC-NONBLOCK
8 constant MAC-POLL-BYTES

\ Darwin's poll(2) refuses more than OPEN_MAX (sys/syslimits.h) entries in one
\ call with EINVAL. One entry is the wake pipe's and every other a distinct
\ descriptor in flight, so a submission that would need one more descriptor is
\ E-AIO-POLL-BOUND. The pollfd array is that size, mapped while the loop runs
\ and committed only as far as the entries in use.
10240 constant OPEN-MAX

BEGIN-STRUCTURE MAC-REC-BYTES
   CELL +FIELD MR.FD
   CELL +FIELD MR.EVENTS
   CELL +FIELD MR.COUNT
   CELL +FIELD MR.OFFSET
   CELL +FIELD MR.DEADLINE
   CELL +FIELD MR.OWNS-FLAGS
   CELL +FIELD MR.OLD-FLAGS
   CELL +FIELD MR.OLD-SIGPIPE
   CELL +FIELD MR.SEEKABLE
   CELL +FIELD MR.GEN               \ the generation MAC-PREPARE polled it at, or -1
   CELL +FIELD MR.SLOT              \ its descriptor's pollfd entry that turn, or -1
END-STRUCTURE

\ These fields live in the record's REC.HOST; one past it is this file's defect.
: MAC-FITS ( -- )
   MAC-REC-BYTES HOST-BYTES > if
      s" aio: Darwin record fields past REC.HOST" E-AIO-STATE die
   then ;
MAC-FITS

\ Per descriptor, under AIO-LOCK: the active records on it, and its pollfd
\ entry in the turn that wrote it. A mapping indexed by descriptor, grown to
\ the highest one submitted and given back with the loop.
BEGIN-STRUCTURE FD-ROW-BYTES
   CELL +FIELD FD.REF
   CELL +FIELD FD.SLOT
   CELL +FIELD FD.TURN
END-STRUCTURE
variable FD-ROWS                    \ the mapping's address as n, 0 when none
variable FD-CAP                     \ the descriptors it covers
variable FD-DISTINCT                \ the descriptors with a record on them
variable FD-WANT                    \ the descriptor FD-GROW covers
variable MAC-TURN

PTR-VARIABLE MAC-POLLS
variable MAC-POLL-COUNT
8 BUFFER: MAC-SOCKERR
create MAC-BYTE 1 c,
64 BUFFER: MAC-DRAIN
variable MAC-READ-FD
variable MAC-WRITE-FD

PROCESS-SYMBOLS
FUNCTION: MAC-FCNTL-CALL fcntl ( n n n -- i32 ) 2 VARIADIC ;FUNCTION
FUNCTION: MAC-POLL poll ( ptr u8 n n -- i32 )
   0 OPEN-MAX MAC-POLL-BYTES * WRITES-BYTES
;FUNCTION
FUNCTION: MAC-READ read ( n ptr u8 n -- n ) 1 2 WRITES-ARG ;FUNCTION
FUNCTION: MAC-PREAD pread ( n ptr u8 n n -- n ) 1 2 WRITES-ARG ;FUNCTION
FUNCTION: MAC-WRITE write ( n ptr u8 n -- n ) ;FUNCTION
FUNCTION: MAC-PWRITE pwrite ( n ptr u8 n n -- n ) ;FUNCTION
FUNCTION: MAC-SEEK lseek ( n n n -- n ) ;FUNCTION
FUNCTION: MAC-ACCEPT accept ( n n n -- i32 ) ;FUNCTION
FUNCTION: MAC-CONNECT connect ( n ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: MAC-SOCKET-ERROR getsockopt ( n n n ptr u8 ptr u8 -- i32 )
   3 4 WRITES-BYTES 4 4 WRITES-BYTES
;FUNCTION

: MR ( n -- ptr n ) REC REC.HOST ;

: MAC-PFD ( n -- ptr u8 ) {: slot:n :}
   slot 0 < slot OPEN-MAX >= or if s" aio: poll entry" E-AIO-STATE die then
   MAC-POLLS @ slot MAC-POLL-BYTES * + ;

: MAC-POLLS-LEN ( -- NUM:alloc-byte-len )
   OPEN-MAX MAC-POLL-BYTES * MEM:BYTES-ALLOC-LEN ;

: MAC-POLLS-FREE ( -- )
   MAC-POLLS @ MAC-POLLS-LEN MEM:RELEASE-BYTES ;

: FD-ROW ( n -- ptr n ) {: fd:n :}
   fd 0 < fd FD-CAP @ >= or if s" aio: descriptor row" E-AIO-STATE die then
   FD-ROWS @ fd FD-ROW-BYTES * + AIO-N>PTR ;

: FD-REF@ ( n -- n ) {: fd:n :}
   fd FD-CAP @ >= if 0 exit then
   fd FD-ROW FD.REF @ ;

: FD-TABLE-LEN ( n -- NUM:alloc-byte-len )
   FD-ROW-BYTES * MEM:BYTES-ALLOC-LEN ;

: FD-TABLE-FREE ( -- )
   FD-CAP @ 0= if exit then
   FD-ROWS @ AIO-N>PTR BYTE-VIEW FD-CAP @ FD-TABLE-LEN MEM:RELEASE-BYTES ;

\ The rows move to a mapping that covers FD-WANT, and the old one goes back at
\ once: only a holder of AIO-LOCK reads them.
: FD-GROW ( -- )
   FD-WANT @ 1 + FD-CAP @ 2 * max {: cap:n :}
   cap FD-TABLE-LEN MEM:ALLOC-BYTES drop {: rows :}
   FD-CAP @ 0 > if
      FD-ROWS @ AIO-N>PTR BYTE-VIEW rows FD-CAP @ FD-ROW-BYTES * BYTE-COPY
   then
   FD-TABLE-FREE
   rows FFI:>CELL FD-ROWS !
   cap FD-CAP ! ;

\ Room for a record on descriptor fd, before anything is claimed: a descriptor
\ already in flight shares its pollfd entry, and a new one needs an entry
\ beside the wake pipe's within OPEN-MAX. Answers 0 or a code.
: MAC-FD-ROOM ( n -- n ) {: fd:n :}
   fd 0 < if 0 exit then
   fd FD-REF@ 0= FD-DISTINCT @ 2 + OPEN-MAX > and if E-AIO-POLL-BOUND exit then
   fd FD-CAP @ < if 0 exit then
   fd FD-WANT !
   [: FD-GROW ;] catch ;

\ A record's descriptor counts from the stage that sets it to MAC-FINISH,
\ which every active record reaches exactly once.
: MAC-FD! ( n n -- ) {: fd:n idx:n :}
   fd idx MR MR.FD !
   fd 0 < if exit then
   fd FD-ROW {: row:ptr :}
   row FD.REF @ 0= if 1 FD-DISTINCT +! then
   1 row FD.REF +! ;

: MAC-FD-DROP ( n -- ) {: idx:n :}
   idx MR MR.FD @ {: fd:n :}
   fd 0 < if exit then
   fd FD-ROW {: row:ptr :}
   -1 row FD.REF +!
   row FD.REF @ 0= if -1 FD-DISTINCT +! then ;
: MAC-ACTIVE? ( n -- bool )
   REC-STATE@ dup STATE-SUBMITTED = swap STATE-FORGET = or ;

: MAC-RESULT ( n -- n ) dup 0 < if drop FFI:ERRNO negate then ;

: MAC-FCNTL ( n n n -- n ) MAC-FCNTL-CALL MAC-RESULT ;

\ CONNECT supplies the caller's borrowed sockaddr through SOCK-SUBMIT's cell
\ ABI. The caller retains it until AWAIT, exactly as on the Linux backend.
CAST: MAC-SOCKADDR ( n -- ptr u8 )

: MAC-NOTIFY ( -- n )
   MAC-WRITE-FD @ MAC-BYTE 1 MAC-WRITE MAC-RESULT
   dup 1 = swap MAC-AGAIN negate = or if 0 else E-AIO-ENTER then ;

: MAC-CLEAR-WAKE ( -- )
   begin
      MAC-READ-FD @ MAC-DRAIN 64 MAC-READ MAC-RESULT
      dup MAC-AGAIN negate = if drop exit then
      dup ERR-INTR negate <> if
         0 <= if s" aio: wake pipe closed" E-AIO-STATE die then
      else drop then
   again ;

\ A record for a stage on descriptor fd, or -1 for none, its fields cleared.
\ Answers a code and the index, as REC-CLAIM.
: MAC-CLAIM ( n n -- n n ) {: fd:n kind:n :}
   fd MAC-FD-ROOM dup 0<> if -1 exit then drop
   REC-CLAIM {: rc:n idx:n :}
   rc 0<> if rc -1 exit then
   idx kind 1 REC-ARM
   idx MR {: row:ptr :}
   MAC-REC-BYTES 0 ?do 0 row BYTE-VIEW i + c! loop
   -1 row MR.FD ! -1 row MR.DEADLINE ! -1 row MR.GEN ! -1 row MR.SLOT !
   fd idx MAC-FD!
   0 idx ;

\ Status flags belong to the open file description, shared by dup aliases.
\ Save and restore around each host call under AIO-LOCK, never across waits.
: MAC-TAKE-FLAGS ( n -- n ) {: idx:n :}
   idx MR {: row:ptr :} row MR.FD @ {: fd:n :}
   fd 3 0 MAC-FCNTL dup 0 < if negate exit then {: flags:n :}
   fd 74 0 MAC-FCNTL dup 0 < if negate exit then row MR.OLD-SIGPIPE !
   fd 4 flags MAC-NONBLOCK or MAC-FCNTL dup 0 < if negate exit then drop
   flags row MR.OLD-FLAGS ! 1 row MR.OWNS-FLAGS !
   fd 73 1 MAC-FCNTL negate ;

: MAC-GIVE-FLAGS ( n -- ) {: idx:n :}
   idx MR {: row:ptr :}
   row MR.OWNS-FLAGS @ 0= if exit then
   0 row MR.OWNS-FLAGS !
   row MR.FD @ 4 row MR.OLD-FLAGS @ MAC-FCNTL 0 < if
      s" aio: descriptor flags could not be restored" E-AIO-STATE die
   then
   row MR.FD @ 73 row MR.OLD-SIGPIPE @ MAC-FCNTL 0 < if
      s" aio: descriptor SIGPIPE flag could not be restored" E-AIO-STATE die
   then ;

: MAC-FINISH ( n n -- ) {: idx:n result:n :}
   idx MAC-GIVE-FLAGS
   idx MAC-FD-DROP
   result idx REC REC.RES !
   idx SETTLE ;

: MAC-DEADLINE ( n -- n )
   dup 0 < if drop -1 exit then
   mono-ns {: now:n :}
   $7FFFFFFFFFFFFFFF now - NS-PER-MS / min NS-PER-MS * now + ;

: MAC-POLL-STAGE ( n n n -- n n ) {: fd:n events:n ms:n :}
   fd KIND-POLL MAC-CLAIM {: rc:n idx:n :}
   rc 0<> if rc -1 exit then
   events idx MR MR.EVENTS !
   ms MAC-DEADLINE idx MR MR.DEADLINE !
   fd 0 < if idx MAC-BADF negate MAC-FINISH then
   MAC-NOTIFY idx ;

: MAC-TIMEOUT-STAGE ( n -- n n ) {: ms:n :}
   -1 KIND-TIMEOUT MAC-CLAIM {: rc:n idx:n :}
   rc 0<> if rc -1 exit then
   ms MAC-DEADLINE idx MR MR.DEADLINE !
   ms 0 < if idx MAC-INVAL negate MAC-FINISH then
   MAC-NOTIFY idx ;

: MAC-XFER-STAGE ( n ptr u8 NUM:alloc-byte-len n n n -- n n )
   {: fd:n buf:ptr cap:NUM:alloc-byte-len count:n off:n kind:n :}
   fd kind MAC-CLAIM {: rc:n idx:n :}
   rc 0<> if rc -1 exit then
   buf idx REC-BUF ! cap idx REC-BUF-LEN! 1 idx REC REC.HOLD !
   count idx MR MR.COUNT ! off idx MR MR.OFFSET !
   kind KIND-READ = if EV-READABLE else EV-WRITABLE then idx MR MR.EVENTS !
   fd 1 0 MAC-FCNTL dup 0 < if idx swap MAC-FINISH MAC-NOTIFY idx exit then drop
   count 0= if idx 0 MAC-FINISH MAC-NOTIFY idx exit then
   fd 0 1 MAC-SEEK 0 >= if 1 idx MR MR.SEEKABLE !
   else FFI:ERRNO MAC-SPIPE <> if idx FFI:ERRNO negate MAC-FINISH then then
   MAC-NOTIFY idx ;

: MAC-SOCKET-STAGE ( n n n n -- n n ) {: fd:n addr:n len:n kind:n :}
   fd kind MAC-CLAIM {: rc:n idx:n :}
   rc 0<> if rc -1 exit then
   len idx MR MR.COUNT !
   fd 0 < if idx MAC-BADF negate MAC-FINISH MAC-NOTIFY idx exit then
   kind KIND-ACCEPT = if EV-READABLE else EV-WRITABLE then idx MR MR.EVENTS !
   kind KIND-CONNECT = if
      idx MAC-TAKE-FLAGS dup 0 <> if negate idx swap MAC-FINISH MAC-NOTIFY idx exit then drop
      fd addr MAC-SOCKADDR len MAC-CONNECT MAC-RESULT {: result:n :}
      idx MAC-GIVE-FLAGS
      result MAC-INPROGRESS negate <> result MAC-ALREADY negate <> and if
         idx result MAC-FINISH
      then
   then MAC-NOTIFY idx ;

: MAC-CANCEL-STAGE ( n -- n ) {: idx:n :}
   idx MAC-ACTIVE? if idx ERR-CANCELED negate MAC-FINISH then
   MAC-NOTIFY ;

: MAC-PIPE-HELD ( -- )
   pipe {: rd:n wr:n rc:n :}
   rc 0 <> if E-AIO-SETUP throw then
   rd MAC-READ-FD ! wr MAC-WRITE-FD !
   rd 2 1 MAC-FCNTL wr 2 1 MAC-FCNTL or rd 4 MAC-NONBLOCK MAC-FCNTL or
   wr 4 MAC-NONBLOCK MAC-FCNTL or 0 <> if
      rd close wr close E-AIO-SETUP throw
   then ;

: MAC-PIPE ( -- )
   [: MAC-PIPE-HELD ;] FD-LOCK:WITH ;

: MAC-SETUP ( -- )
   MAC-POLLS-LEN MEM:ALLOC-BYTES drop MAC-POLLS !
   [: MAC-PIPE ;] catch dup 0<> if MAC-POLLS-FREE throw then drop ;

\ STOP has seen every record free, so no descriptor is counted.
: MAC-CLOSE ( -- )
   MAC-READ-FD @ close MAC-WRITE-FD @ close
   -1 MAC-READ-FD ! -1 MAC-WRITE-FD !
   MAC-POLLS-FREE
   FD-TABLE-FREE
   0 FD-ROWS ! 0 FD-CAP ! 0 FD-DISTINCT ! ;

: MAC-TRANSFER ( n -- n ) {: idx:n :}
   idx MR {: row:ptr :}
   row MR.FD @ {: fd:n :} idx REC-BUF @ {: buf:ptr :}
   row MR.COUNT @ {: count:n :} row MR.OFFSET @ {: off:n :}
   row MR.SEEKABLE @ 0 <> off 0 >= and {: positioned:bool :}
   idx REC REC.KIND @ KIND-READ = if
      positioned if fd buf count off MAC-PREAD else fd buf count MAC-READ then
   else
      positioned if fd buf count off MAC-PWRITE else fd buf count MAC-WRITE then
   then MAC-RESULT ;

: MAC-ACCEPT-READY ( n -- n )
   MR MR.FD @ 0 0 MAC-ACCEPT MAC-RESULT
   dup 0 < if exit then {: fd:n :}
   fd 2 1 MAC-FCNTL dup 0 < if fd close exit then drop
   \ accept inherits O_NONBLOCK on Darwin; the public accepted socket is blocking.
   fd 3 0 MAC-FCNTL dup 0 < if fd close exit then
   MAC-NONBLOCK invert and fd 4 rot MAC-FCNTL dup 0 < if fd close exit then drop
   fd ;

: MAC-CONNECT-READY ( n -- n )
   0 MAC-SOCKERR LE:U32! 4 MAC-SOCKERR 4 + LE:U32!
   MR MR.FD @ $FFFF $1007 MAC-SOCKERR MAC-SOCKERR 4 + MAC-SOCKET-ERROR
   MAC-RESULT dup 0 < if exit then drop
   MAC-SOCKERR LE:U32@ negate ;

: MAC-SERVICE ( n n -- ) {: idx:n events:n :}
   idx REC REC.KIND @ {: kind:n :}
   kind KIND-POLL = if idx events MAC-FINISH exit then
   events $20 and 0<> if idx MAC-BADF negate MAC-FINISH exit then
   idx MAC-TAKE-FLAGS dup 0 <> if negate idx swap MAC-FINISH exit then drop
   kind KIND-ACCEPT = if idx MAC-ACCEPT-READY else
      kind KIND-CONNECT = if idx MAC-CONNECT-READY else idx MAC-TRANSFER then
   then {: result:n :}
   idx MAC-GIVE-FLAGS
   result MAC-AGAIN negate = result ERR-INTR negate = or if exit then
   idx result MAC-FINISH ;

: MAC-PFD! ( n n n -- ) {: fd:n events:n slot:n :}
   slot MAC-PFD {: out:ptr :}
   fd out LE:U32! events out 4 + LE:U16! 0 out 6 + LE:U16! ;

\ One entry per distinct descriptor, its events the union of the records on it;
\ timers occupy none. The descriptor's row says whether this turn has written
\ its entry yet.
: MAC-POLL-SLOT ( n n -- n ) {: fd:n events:n :}
   fd FD-ROW {: row:ptr :}
   row FD.TURN @ MAC-TURN @ = if
      row FD.SLOT @ MAC-PFD 4 + dup LE:U16@ events or swap LE:U16!
      row FD.SLOT @ exit
   then
   MAC-POLL-COUNT @ {: slot:n :}
   fd events slot MAC-PFD!
   MAC-TURN @ row FD.TURN !
   slot row FD.SLOT !
   1 MAC-POLL-COUNT +! slot ;

\ Snapshot each record's generation while holding the facility. A cancelled
\ record may be reused while poll is asleep; only its matching generation runs.
: MAC-PREPARE ( -- n n )
   1 MAC-TURN +!
   MAC-READ-FD @ EV-READABLE 0 MAC-PFD!
   1 MAC-POLL-COUNT !
   -1
   REC-CAP @ 0 ?do
      i MR {: row:ptr :}
      -1 row MR.GEN !
      -1 row MR.SLOT !
      i MAC-ACTIVE? if
         i REC REC.GEN @ row MR.GEN !
         row MR.FD @ 0 >= if
            row MR.FD @ row MR.EVENTS @ MAC-POLL-SLOT row MR.SLOT !
         then
         row MR.DEADLINE @ dup 0 >= if
            mono-ns - 0 max NS-PER-MS /mod swap 0<> if 1+ then
            $7FFFFFFF min
            over 0 < if nip else min then
         else drop then
      then
   loop MAC-POLL-COUNT @ ;

: MAC-COLLECT ( -- )
   0 MAC-PFD 6 + LE:U16@ EV-READABLE and 0<> if MAC-CLEAR-WAKE then
   REC-CAP @ 0 ?do
      i MAC-ACTIVE? i REC REC.GEN @ i MR MR.GEN @ = and if
         i MR MR.SLOT @ dup 0 >= if
            MAC-PFD 6 + LE:U16@ i MR MR.EVENTS @ $38 or and
            dup 0 <> if i swap MAC-SERVICE else drop then
         else drop then
         i MAC-ACTIVE? if
            i MR MR.DEADLINE @ dup 0 >= swap mono-ns <= and if
               i ERR-TIME negate MAC-FINISH
            then
         then
      then
   loop ;

: MAC-LOOP ( -- )
   begin
      AIO-LOCK TASK:GET MAC-PREPARE {: timeout:n slots:n :} AIO-LOCK TASK:RELEASE
      MAC-POLLS @ slots timeout MAC-POLL MAC-RESULT
      dup 0 < if ERR-INTR negate <> if E-AIO-ENTER throw then else drop then
      AIO-LOCK TASK:GET MAC-COLLECT AIO-LOCK TASK:RELEASE
      RING-STOP atomic@ 0 <> if exit then
   again ;

: INSTALL-MAC-HOST ( -- )
   ['] MAC-POLL-STAGE is HOST-POLL
   ['] MAC-TIMEOUT-STAGE is HOST-TIMEOUT
   ['] MAC-XFER-STAGE is HOST-XFER
   ['] MAC-SOCKET-STAGE is HOST-SOCKET
   ['] MAC-CANCEL-STAGE is HOST-CANCEL
   ['] MAC-NOTIFY is HOST-WAKE
   ['] MAC-SETUP is HOST-SETUP
   ['] MAC-CLOSE is HOST-CLOSE
   ['] MAC-LOOP is HOST-LOOP ;
INSTALL-MAC-HOST
;package
