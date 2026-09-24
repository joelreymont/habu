\ Darwin backend for AIO's shared tickets and buffer ownership.
\ One poll thread services all records. A nonblocking pipe wakes it when a
\ submission changes the descriptor set or the nearest deadline. Generations
\ fence the snapshot across poll, so cancellation/reuse cannot deliver an old
\ event to a new operation. Each serialized host call temporarily sets
\ nonblocking status and restores it before releasing the facility.
require lib/aio.f
require lib/le.f

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
OPS-MAX 1+ constant MAC-POLL-N

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
END-STRUCTURE
OPS-MAX MAC-REC-BYTES * BUFFER: MAC-RECS
OPS-MAX TYPED-BUFFER MAC-GEN n
OPS-MAX TYPED-BUFFER MAC-SLOT n
MAC-POLL-N MAC-POLL-BYTES * BUFFER: MAC-POLLS
variable MAC-POLL-COUNT
8 BUFFER: MAC-SOCKERR
create MAC-BYTE 1 c,
64 BUFFER: MAC-DRAIN
variable MAC-READ-FD
variable MAC-WRITE-FD

PROCESS-SYMBOLS
FUNCTION: MAC-FCNTL-CALL fcntl ( n n n -- n ) 2 VARIADIC ;FUNCTION
FUNCTION: MAC-POLL poll ( ptr u8 n n -- n )
   0 MAC-POLL-N MAC-POLL-BYTES * WRITES-BYTES
;FUNCTION
FUNCTION: MAC-READ read ( n ptr u8 n -- n ) 1 2 WRITES-ARG ;FUNCTION
FUNCTION: MAC-PREAD pread ( n ptr u8 n n -- n ) 1 2 WRITES-ARG ;FUNCTION
FUNCTION: MAC-WRITE write ( n ptr u8 n -- n ) ;FUNCTION
FUNCTION: MAC-PWRITE pwrite ( n ptr u8 n n -- n ) ;FUNCTION
FUNCTION: MAC-SEEK lseek ( n n n -- n ) ;FUNCTION
FUNCTION: MAC-ACCEPT accept ( n n n -- n ) ;FUNCTION
FUNCTION: MAC-CONNECT connect ( n ptr u8 n -- n ) ;FUNCTION
FUNCTION: MAC-SOCKET-ERROR getsockopt ( n n n ptr u8 ptr u8 -- n )
   3 4 WRITES-BYTES 4 4 WRITES-BYTES
;FUNCTION

: MR ( n -- ptr n ) MAC-REC-BYTES * MAC-RECS + CELL-VIEW ;
: MAC-PFD ( n -- ptr u8 ) MAC-POLL-BYTES * MAC-POLLS + ;
: MAC-ACTIVE? ( n -- bool )
   REC-STATE@ dup STATE-SUBMITTED = swap STATE-FORGET = or ;
: MAC-CINT ( n -- n ) $FFFFFFFF and dup $80000000 and 0<> if $100000000 - then ;

: MAC-RESULT ( n -- n ) dup 0 < if drop FFI:ERRNO negate then ;

: MAC-FCNTL ( n n n -- n ) MAC-FCNTL-CALL MAC-CINT MAC-RESULT ;

\ CONNECT supplies the caller's borrowed sockaddr through SOCK-SUBMIT's cell
\ ABI. The caller retains it until AWAIT, exactly as on the Linux backend.
TRUSTED: MAC-SOCKADDR ( n -- ptr u8 ) ;

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

: MAC-CLAIM ( n -- n ) {: kind:n :}
   REC-CLAIM dup 0 < if exit then {: idx:n :}
   idx kind 1 REC-ARM
   idx MR {: row:ptr :}
   MAC-REC-BYTES 0 ?do 0 row BYTE-VIEW i + c! loop
   -1 row MR.FD ! -1 row MR.DEADLINE !
   idx ;

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
   result idx REC REC.RES !
   idx SETTLE ;

: MAC-DEADLINE ( n -- n )
   dup 0 < if drop -1 exit then
   mono-ns {: now:n :}
   $7FFFFFFFFFFFFFFF now - NS-PER-MS / min NS-PER-MS * now + ;

: MAC-POLL-STAGE ( n n n -- n n ) {: fd:n events:n ms:n :}
   KIND-POLL MAC-CLAIM dup 0 < if drop E-AIO-FULL -1 exit then {: idx:n :}
   fd idx MR MR.FD ! events idx MR MR.EVENTS !
   ms MAC-DEADLINE idx MR MR.DEADLINE !
   fd 0 < if idx MAC-BADF negate MAC-FINISH then
   MAC-NOTIFY idx ;

: MAC-TIMEOUT-STAGE ( n -- n n ) {: ms:n :}
   KIND-TIMEOUT MAC-CLAIM dup 0 < if drop E-AIO-FULL -1 exit then {: idx:n :}
   ms MAC-DEADLINE idx MR MR.DEADLINE !
   ms 0 < if idx MAC-INVAL negate MAC-FINISH then
   MAC-NOTIFY idx ;

: MAC-XFER-STAGE ( n ptr u8 NUM:alloc-byte-len n n n -- n n )
   {: fd:n buf:ptr cap:NUM:alloc-byte-len count:n off:n kind:n :}
   kind MAC-CLAIM dup 0 < if drop E-AIO-FULL -1 exit then {: idx:n :}
   buf idx REC-BUF ! cap idx REC-BUF-LEN ! 1 idx REC REC.HOLD !
   fd idx MR MR.FD ! count idx MR MR.COUNT ! off idx MR MR.OFFSET !
   kind KIND-READ = if EV-READABLE else EV-WRITABLE then idx MR MR.EVENTS !
   fd 1 0 MAC-FCNTL dup 0 < if idx swap MAC-FINISH MAC-NOTIFY idx exit then drop
   count 0= if idx 0 MAC-FINISH MAC-NOTIFY idx exit then
   fd 0 1 MAC-SEEK 0 >= if 1 idx MR MR.SEEKABLE !
   else FFI:ERRNO MAC-SPIPE <> if idx FFI:ERRNO negate MAC-FINISH then then
   MAC-NOTIFY idx ;

: MAC-SOCKET-STAGE ( n n n n -- n n ) {: fd:n addr:n len:n kind:n :}
   kind MAC-CLAIM dup 0 < if drop E-AIO-FULL -1 exit then {: idx:n :}
   fd idx MR MR.FD ! len idx MR MR.COUNT !
   fd 0 < if idx MAC-BADF negate MAC-FINISH MAC-NOTIFY idx exit then
   kind KIND-ACCEPT = if EV-READABLE else EV-WRITABLE then idx MR MR.EVENTS !
   kind KIND-CONNECT = if
      idx MAC-TAKE-FLAGS dup 0 <> if negate idx swap MAC-FINISH MAC-NOTIFY idx exit then drop
      fd addr MAC-SOCKADDR len MAC-CONNECT MAC-CINT
      MAC-RESULT {: result:n :}
      idx MAC-GIVE-FLAGS
      result MAC-INPROGRESS negate <> result MAC-ALREADY negate <> and if
         idx result MAC-FINISH
      then
   then MAC-NOTIFY idx ;

: MAC-CANCEL-STAGE ( n -- n ) {: idx:n :}
   idx MAC-ACTIVE? if idx ERR-CANCELED negate MAC-FINISH then
   MAC-NOTIFY ;

: MAC-SETUP ( -- )
   pipe {: rd:n wr:n rc:n :}
   rc 0 <> if E-AIO-SETUP throw then
   rd MAC-READ-FD ! wr MAC-WRITE-FD !
   rd 2 1 MAC-FCNTL wr 2 1 MAC-FCNTL or rd 4 MAC-NONBLOCK MAC-FCNTL or
   wr 4 MAC-NONBLOCK MAC-FCNTL or 0 <> if
      rd close wr close E-AIO-SETUP throw
   then
   OPS-MAX 0 ?do 0 i MR MR.OWNS-FLAGS ! loop ;

: MAC-CLOSE ( -- )
   MAC-READ-FD @ close MAC-WRITE-FD @ close
   -1 MAC-READ-FD ! -1 MAC-WRITE-FD ! ;

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
   MR MR.FD @ 0 0 MAC-ACCEPT MAC-CINT MAC-RESULT
   dup 0 < if exit then {: fd:n :}
   fd 2 1 MAC-FCNTL dup 0 < if fd close exit then drop
   \ accept inherits O_NONBLOCK on Darwin; the public accepted socket is blocking.
   fd 3 0 MAC-FCNTL dup 0 < if fd close exit then
   MAC-NONBLOCK invert and fd 4 rot MAC-FCNTL dup 0 < if fd close exit then drop
   fd ;

: MAC-CONNECT-READY ( n -- n )
   0 MAC-SOCKERR LE:U32! 4 MAC-SOCKERR 4 + LE:U32!
   MR MR.FD @ $FFFF $1007 MAC-SOCKERR MAC-SOCKERR 4 + MAC-SOCKET-ERROR
   MAC-CINT MAC-RESULT dup 0 < if exit then drop
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

\ poll's nfds is bounded by the host descriptor limit, even for fd=-1 rows.
\ Only distinct active descriptors occupy slots; timers occupy none.
: MAC-POLL-SLOT ( n n -- n ) {: fd:n events:n :}
   MAC-POLL-COUNT @ 0 ?do
      i MAC-PFD LE:S32@ fd = if
         i MAC-PFD 4 + dup LE:U16@ events or swap LE:U16!
         i unloop exit
      then
   loop
   MAC-POLL-COUNT @ {: slot:n :}
   fd events slot MAC-PFD!
   1 MAC-POLL-COUNT +! slot ;

\ Snapshot each record's generation while holding the facility. A cancelled
\ record may be reused while poll is asleep; only its matching generation runs.
: MAC-PREPARE ( -- n n )
   MAC-READ-FD @ EV-READABLE 0 MAC-PFD!
   1 MAC-POLL-COUNT !
   -1
   OPS-MAX 0 ?do
      -1 i MAC-GEN !
      -1 i MAC-SLOT !
      i MAC-ACTIVE? if
         i REC REC.GEN @ i MAC-GEN !
         i MR {: row:ptr :}
         row MR.FD @ 0 >= if
            row MR.FD @ row MR.EVENTS @ MAC-POLL-SLOT i MAC-SLOT !
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
   OPS-MAX 0 ?do
      i MAC-ACTIVE? i REC REC.GEN @ i MAC-GEN @ = and if
         i MAC-SLOT @ dup 0 >= if
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
      MAC-POLLS slots timeout MAC-POLL MAC-CINT MAC-RESULT
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
