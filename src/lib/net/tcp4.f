split-str
\ tcp4.f - Linux and Darwin IPv4 stream sockets through exact, bounded libc
\ bindings; no protocol, framing, name resolution or connection policy. Every
\ connection this module makes is non-blocking.
\ STORAGE CLASS. TASK-LOCAL: the endpoint storage is one TASK:+USERS row; the
\ payload spans the transfers take are caller-owned.
require src/lib/error.f
require src/lib/ffi-abi.f
require src/lib/type/deftype.f
require src/lib/num-types.f
require src/lib/task.f
require src/lib/aio.f
require src/lib/le.f
require src/lib/fd.f

package TCP4
public

DEFTYPE ADDRESS
DEFTYPE PORT
DEFTYPE LISTENER
DEFTYPE CONNECTION
DEFTYPE ERRNO

ENUM direction receiving sending both ;ENUM

SUMTYPE bind-result
   VARIANT bound listener ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

SUMTYPE connect-result
   VARIANT connected connection ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

SUMTYPE accept-result
   VARIANT accepted connection address port ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

\ Why an ACCEPT failed, as >ACCEPT-FAULT classifies its errno. `aborted`: the
\ pending connection failed before accept took it, so the next one may succeed
\ at once. `starved`: the process or the system is out of descriptors, buffers
\ or memory, which a closed descriptor or freed memory may relieve. `broken`:
\ anything else, such as a listener that is no longer a listening socket.
ENUM accept-fault aborted starved broken ;ENUM

SUMTYPE endpoint-result
   VARIANT endpoint address port ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

SUMTYPE status
   VARIANT ok ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

\ One transfer's outcome. `closed` carries the bytes delivered into the caller's
\ buffer before the peer's end of stream, which READ-EXACT reports as a partial;
\ it is the only way a transfer answers zero bytes.
SUMTYPE read-result
   VARIANT data NUM:byte-len ;VARIANT
   VARIANT closed NUM:byte-len ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

SUMTYPE try-read-result
   VARIANT data NUM:byte-len ;VARIANT
   VARIANT closed NUM:byte-len ;VARIANT
   VARIANT empty ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

SUMTYPE ready-result
   VARIANT ready ;VARIANT
   VARIANT idle ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

\ One send slice's outcome. `moved` carries the bytes the OS accepted, at least
\ one and no more than the span; `empty` says there was no room, for SEND-SLICE
\ even once a wait of SEND-SLICE-MS for some had ended; `failed` carries the
\ errno.
ENUM slice-result
   VARIANT moved FIELD sent NUM:byte-len ;VARIANT
   VARIANT empty ;VARIANT
   VARIANT failed FIELD cause errno ;VARIANT
;ENUM

\ A socket that exists but is connected to nothing yet. It is a `connection`
\ because that is what it becomes and what closes it; until something connects
\ it, reading or writing it is the OS's ENOTCONN.
SUMTYPE socket-result
   VARIANT opened connection ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

\ How many bytes one of a connection's two queues holds, as the OS counts them.
SUMTYPE queue-result
   VARIANT queued NUM:byte-len ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

E-TCP4-OPERAND constant E-OPERAND
E-TCP4-RESULT constant E-RESULT

\ The longest a send slice waits for room. A slice returns within this and two
\ sends that never wait, so this is also how long WRITE goes between looks at a
\ halt while the peer takes nothing.
100 constant SEND-SLICE-MS

private

$10 constant SOCKADDR-BYTES
$FFFF constant MAX-PORT
$FFFFFFFF constant MAX-ADDRESS
$7FFFF000 constant MAX-TRANSFER    \ Linux transfers at most this many bytes per call.
$1000 constant MAX-BACKLOG         \ Linux SOMAXCONN.
: SOCKET-FLAGS ( -- n ) HB-TARGET-LINUX-KERNEL? if $80001 else 1 then ;   \ SOCK_STREAM, with SOCK_CLOEXEC on Linux.
$80800 constant ACCEPT-FLAGS       \ SOCK_CLOEXEC | SOCK_NONBLOCK on the accepted connection.
: MSG-NOSIGNAL ( -- n ) HB-TARGET-LINUX-KERNEL? if $4000 else $80000 then ;   \ A write to a closed peer never signals.
4 constant F-SETFL
: O-NONBLOCK ( -- n ) HB-TARGET-LINUX-KERNEL? if $800 else 4 then ;   \ Linux's SOCK_NONBLOCK is the same bit.
$39 constant POLL-DONE             \ POLLIN|POLLERR|POLLHUP|POLLNVAL: a read will not block.
$01 constant POLLIN                \ a receive will not block, on both.
$04 constant POLLOUT               \ a send will not block, on both.
\ Linux raises POLLHUP on a peer's FIN only once this end has shut too, and
\ POLLRDHUP only when asked for; macOS raises POLLHUP only through POLLIN's read filter.
: HANG-EVENTS ( -- n ) HB-TARGET-LINUX-KERNEL? if $2001 else $01 then ;   \ POLLIN, with POLLRDHUP on Linux.
: HANG-BITS ( -- n ) HB-TARGET-LINUX-KERNEL? if $2038 else $38 then ;   \ POLLERR|POLLHUP|POLLNVAL, with POLLRDHUP on Linux.
-1 constant NO-TIMEOUT             \ poll(2) waits until ready; Darwin takes no other negative one.
$7FFFFFFF constant MAX-TIMEOUT     \ the widest deadline this module accepts, in milliseconds.
1000000 constant NS-PER-MS
$7FFFFFFFFFFFFFFF constant NO-DEADLINE   \ a mono-ns deadline that never passes.
\ Errno values from the macOS SDK's usr/include/sys/errno.h and Linux's
\ include/uapi/asm-generic/errno-base.h and errno.h.
4 constant EINTR
12 constant ENOMEM
23 constant ENFILE
24 constant EMFILE
: EAGAIN ( -- n ) HB-TARGET-LINUX-KERNEL? if 11 else 35 then ;   \ EWOULDBLOCK on both.
: ECONNABORTED ( -- n ) HB-TARGET-LINUX-KERNEL? if 103 else 53 then ;
: ENOBUFS ( -- n ) HB-TARGET-LINUX-KERNEL? if 105 else 55 then ;
6 constant IPPROTO-TCP
1 constant TCP-NODELAY
: SOL-SOCKET ( -- n ) HB-TARGET-LINUX-KERNEL? if 1 else $FFFF then ;
: SO-REUSEADDR ( -- n ) HB-TARGET-LINUX-KERNEL? if 2 else 4 then ;
: FIONREAD ( -- n ) HB-TARGET-LINUX-KERNEL? if $541B else $4004667F then ;   \ bytes received and not yet read.
$5411 constant SIOCOUTQ            \ Linux: bytes written and not yet acknowledged.
$1024 constant SO-NWRITE           \ Darwin's count of SIOCOUTQ, a socket option rather than an ioctl.
0 constant SHUT-RD
1 constant SHUT-WR
2 constant SHUT-RDWR

\ Endpoint storage is per task: a $10 sockaddr_in, a $04 socklen_t and the
\ $04 int a socket option is set from, raw bytes the kernel reads and
\ fills, and the pollfd a transfer waits through. Calls must not nest
\ within one task; readers sharing a connection coordinate themselves.
$10 TASK:+USERS SOCKADDR-ROW u8
4 TASK:+USERS ADDRLEN-ROW u8
4 TASK:+USERS OPTION-ROW u8
TASK:+USER POLLFD-ROW FD:pollfd
: SOCKADDR ( -- ^u8 ) 0 SOCKADDR-ROW ;
: ADDRLEN ( -- ^u8 ) 0 ADDRLEN-ROW ;
: OPTION ( -- ^u8 ) 0 OPTION-ROW ;

CAST: BLEN>N ( NUM:byte-len -- n )
\ Each caller has shown the cell nonnegative.
CAST: N>BLEN ( n -- NUM:byte-len )


: WITHIN-RANGE ( n n n -- ) {: value:n minimum:n maximum:n :}
   value minimum < value maximum > or if E-OPERAND throw then ;


: LENGTH ( n -- NUM:byte-len )
   NUM:BYTE-LEN MATCH option
      none OF E-OPERAND throw ENDOF
      some OF ENDOF
   ;MATCH ;


: BE16! ( n ^u8 -- ) {: value:n target :}
   value 8 rshift $FF and target c! value $FF and target $01 + c! ;


: BE16@ ( ^u8 -- n ) {: source :}
   source c@ 8 lshift source $01 + c@ or ;


: BE32! ( n ^u8 -- ) {: value:n target :}
   value 16 rshift target BE16! value target $02 + BE16! ;


: BE32@ ( ^u8 -- n ) {: source :}
   source BE16@ 16 lshift source $02 + BE16@ or ;


: CLEAR-ENDPOINT ( -- )
   SOCKADDR-BYTES 0 do 0 SOCKADDR i + c! loop ;


: ENDPOINT! ( address port -- ) {: address:address port:port :}
   address ADDRESS>N 0 MAX-ADDRESS WITHIN-RANGE
   port PORT>N 0 MAX-PORT WITHIN-RANGE
   CLEAR-ENDPOINT
   HB-TARGET-LINUX-KERNEL? if
      2 SOCKADDR c!
   else
      SOCKADDR-BYTES SOCKADDR c! 2 SOCKADDR 1+ c!
   then
   port PORT>N SOCKADDR $02 + BE16!
   address ADDRESS>N SOCKADDR $04 + BE32! ;


: ENDPOINT-OUTPUT ( -- )
   CLEAR-ENDPOINT SOCKADDR-BYTES ADDRLEN LE:U32! ;


: ENDPOINT@ ( -- address port )
   SOCKADDR $04 + BE32@ >ADDRESS SOCKADDR $02 + BE16@ >PORT ;


: SHUT-CODE ( direction -- n )
   MATCH direction
      receiving OF SHUT-RD ENDOF
      sending OF SHUT-WR ENDOF
      both OF SHUT-RDWR ENDOF
   ;MATCH ;


\ RTLD_DEFAULT borrows process symbols; the native executable already needs
\ libc.so.6, so no library reference is acquired or retained here.
PROCESS-SYMBOLS

FUNCTION: SOCKET-CALL socket ( n n n -- i32 ) ;FUNCTION
FUNCTION: BIND-CALL bind ( n ^u8 n -- i32 ) ;FUNCTION
FUNCTION: LISTEN-CALL listen ( n n -- i32 ) ;FUNCTION
FUNCTION: CONNECT-CALL connect ( n ^u8 n -- i32 ) ;FUNCTION
FUNCTION: SEND-CALL send ( n ^u8 n n -- n ) ;FUNCTION
FUNCTION: OPTION-CALL setsockopt ( n n n ^u8 n -- i32 ) ;FUNCTION
FUNCTION: SHUTDOWN-CALL shutdown ( n n -- i32 ) ;FUNCTION
FUNCTION: CLOSE-CALL close ( n -- i32 ) ;FUNCTION
FUNCTION: DARWIN-ACCEPT accept ( n ^u8 ^u8 -- i32 ) ;FUNCTION
FUNCTION: ACCEPT-CALL accept4 ( n ^u8 ^u8 n -- i32 ) ;FUNCTION
FUNCTION: LOCAL-CALL getsockname ( n ^u8 ^u8 -- i32 ) ;FUNCTION
FUNCTION: RECEIVE-CALL recv ( n ^u8 n n -- n ) ;FUNCTION

\ ioctl's third argument is variadic: Darwin passes it on the stack.
FUNCTION: QUEUE-CALL ioctl ( n n ^u8 -- i32 )
   2 VARIADIC
;FUNCTION

FUNCTION: OPTION-GET-CALL getsockopt ( n n n ^u8 ^u8 -- i32 ) ;FUNCTION


: LAST-ERROR ( -- errno )
   FFI:ERRNO >ERRNO ;


: INTERRUPTED? ( errno -- bool )
   ERRNO>N EINTR = ;


: WOULD-BLOCK? ( errno -- bool )
   ERRNO>N EAGAIN = ;


: OWN-SOCKET ( n -- n ) {: fd:n :}
   HB-TARGET-LINUX-KERNEL? if fd exit then
   fd 0 < if fd exit then
   fd 2 1 fcntl 0 <> if fd CLOSE-CALL drop E-RESULT throw then
   fd ;


\ The status flags are set outright: a fresh or just-connected socket has no
\ other to keep, and Darwin's accepted socket, which inherits its listener's, is
\ left with this one alone, as Linux's accept4 leaves it.
: NONBLOCK ( n -- n ) {: fd:n :}
   fd F-SETFL O-NONBLOCK fcntl 0 <> if fd CLOSE-CALL drop E-RESULT throw then
   fd ;

: SOCKET-RAW ( -- n )
   2 SOCKET-FLAGS 0 SOCKET-CALL OWN-SOCKET ;


: BIND-RAW ( n -- n ) {: fd:n :}
   fd SOCKADDR SOCKADDR-BYTES BIND-CALL ;


: REUSEADDR-RAW ( n -- n ) {: fd:n :}
   1 OPTION LE:U32!
   fd SOL-SOCKET SO-REUSEADDR OPTION 4 OPTION-CALL ;


: LISTEN-RAW ( n n -- n )
   LISTEN-CALL ;


\ Linux's accept4 makes the connection close-on-exec and non-blocking as it
\ accepts it; Darwin's accept takes no flags, so the connection gets both after.
: ACCEPT-RAW ( n -- n ) {: fd:n :}
   HB-TARGET-LINUX-KERNEL? if
      fd SOCKADDR ADDRLEN ACCEPT-FLAGS ACCEPT-CALL exit
   then
   fd SOCKADDR ADDRLEN DARWIN-ACCEPT OWN-SOCKET
   dup 0 < if exit then NONBLOCK ;


: CONNECT-RAW ( n -- n ) {: fd:n :}
   fd SOCKADDR SOCKADDR-BYTES CONNECT-CALL ;


: LOCAL-RAW ( n -- n ) {: fd:n :}
   fd SOCKADDR ADDRLEN LOCAL-CALL ;


: RECEIVE-RAW ( n str -- n ) {: fd:n bytes size:n :}
   fd bytes size 0 RECEIVE-CALL ;


: SEND-RAW ( n str -- n ) {: fd:n bytes size:n :}
   fd bytes size MSG-NOSIGNAL SEND-CALL ;


: SHUTDOWN-RAW ( n n -- n )
   SHUTDOWN-CALL ;


: CLOSE-RAW ( n -- n )
   CLOSE-CALL ;


: BIND-FAILED ( n -- bind-result ) {: fd:n :}
   LAST-ERROR fd CLOSE-RAW drop TCP4-BIND--RESULT:FAILED ;


: RC>STATUS ( n -- status )
   0 < if LAST-ERROR TCP4-STATUS:FAILED else TCP4-STATUS:OK then ;


: QUEUED ( n -- queue-result )
   0 < if LAST-ERROR TCP4-QUEUE--RESULT:FAILED exit then
   OPTION LE:U32@ N>BLEN TCP4-QUEUE--RESULT:QUEUED ;


\ POLLERR, POLLHUP and POLLNVAL all mean a read returns at once, with the end of
\ stream or the error as its answer, so they are readiness and not a failure.
: REVENTS>READY ( n -- ready-result )
   POLL-DONE and 0 <> if TCP4-READY--RESULT:READY exit then
   TCP4-READY--RESULT:IDLE ;


\ The deadline is the poll's own linked timeout, so a signal does not cut the
\ wait short; a zero timeout still answers `ready` for a descriptor already
\ ready. A wait without AIO:START is E-AIO-STATE. `cancelled` cannot arrive
\ (nothing here cancels, the ticket never leaves this word, and AIO's cleanup on
\ a submitting task runs only after the task ends), so it is a broken foreign
\ result.
: WAIT-READY ( n ms -- ready-result ) {: fd:n timeout:ms :}
   timeout MS>N MAX-TIMEOUT > if E-OPERAND throw then
   fd >FD AIO:READABLE timeout AIO:POLL AIO:AWAIT
   MATCH AIO:outcome
      ready OF REVENTS>READY ENDOF
      timed-out OF TCP4-READY--RESULT:IDLE ENDOF
      cancelled OF E-RESULT throw ENDOF
      refused OF >ERRNO TCP4-READY--RESULT:FAILED ENDOF
   ;MATCH ;


\ One poll(2) of the descriptor for the events, on this task's own thread with
\ no AIO loop: how many descriptors are ready, 0 once the timeout in
\ milliseconds has passed, or the errno negated.
: POLL-FD ( n n n -- n ) {: fd:n events:n timeout:n :}
   fd >FD events FD:>POLLFD POLLFD-ROW !
   POLLFD-ROW 1 timeout poll ;


\ Blocks until a receive on the descriptor will not wait - there is data, the
\ end of stream, or an error the receive reports - which is `ok`. A signal
\ restarts the wait; any other refusal is `failed` with its errno.
: DATA-READY ( n -- status ) {: fd:n :}
   begin
      fd POLLIN NO-TIMEOUT POLL-FD {: rc:n :}
      rc 0 >= if TCP4-STATUS:OK exit then
      rc negate EINTR <> if rc negate >ERRNO TCP4-STATUS:FAILED exit then
   again ;


\ A stream read of zero bytes is the peer's orderly end of stream, never an
\ empty message.
: TRY-CHUNK ( n str -- try-read-result ) {: fd:n bytes size:n :}
   begin
      fd bytes size RECEIVE-RAW dup 0 > if N>BLEN TCP4-TRY--READ--RESULT:DATA exit then
      0= if 0 N>BLEN TCP4-TRY--READ--RESULT:CLOSED exit then
      LAST-ERROR dup WOULD-BLOCK? if drop TCP4-TRY--READ--RESULT:EMPTY exit then
      dup INTERRUPTED? 0= if TCP4-TRY--READ--RESULT:FAILED exit then drop
   again ;


\ One receive, tried again after a wait for data while there is nothing to
\ receive yet, so a read on a non-blocking connection still blocks until the
\ stream answers.
: READ-CHUNK ( n str -- read-result ) {: fd:n bytes size:n :}
   begin
      fd bytes size TRY-CHUNK
      MATCH try-read-result
         data OF TCP4-READ--RESULT:DATA exit ENDOF
         closed OF TCP4-READ--RESULT:CLOSED exit ENDOF
         empty OF ENDOF
         failed OF TCP4-READ--RESULT:FAILED exit ENDOF
      ;MATCH
      fd DATA-READY
      MATCH status
         ok OF ENDOF
         failed OF TCP4-READ--RESULT:FAILED exit ENDOF
      ;MATCH
   again ;


: EXACT-STEP ( n connection ^u8 NUM:byte-len -- read-result )
   {: got:n conn:connection bytes want:NUM:byte-len :}
   conn CONNECTION>N bytes got + want BLEN>N got - READ-CHUNK ;


\ One send that never waits: `moved`, `empty` when there was no room, or
\ `failed`. An interrupt retries it until the mono-ns deadline has passed and
\ is `empty` after that, so no retry carries a slice past it; NO-DEADLINE
\ retries every interrupt.
: SEND-BY ( n str n -- slice-result ) {: fd:n bytes size:n deadline:n :}
   begin
      fd bytes size SEND-RAW
      dup 0 > if N>BLEN TCP4-SLICE--RESULT:MOVED exit then
      0= if E-RESULT throw then
      LAST-ERROR dup WOULD-BLOCK? if drop TCP4-SLICE--RESULT:EMPTY exit then
      dup INTERRUPTED? 0= if TCP4-SLICE--RESULT:FAILED exit then drop
      mono-ns deadline > if TCP4-SLICE--RESULT:EMPTY exit then
   again ;


: SEND-SPAN ( NUM:byte-len -- n )
   BLEN>N dup 1 < if E-OPERAND throw then
   MAX-TRANSFER min ;


\ Milliseconds left before a mono-ns deadline, rounded up so a poll cannot end
\ before it; 0 once it has passed.
: LEFT-MS ( n -- n ) {: deadline:n :}
   deadline mono-ns - {: left:n :}
   left 0 <= if 0 exit then
   left NS-PER-MS 1- + NS-PER-MS / ;


\ Blocks until a send on the descriptor will not wait - there is room, or an
\ error the send reports - which is `ready`, or the mono-ns deadline passes,
\ `idle`. A signal restarts the wait with what is left of it, so none stretches
\ it; any other refusal is `failed` with its errno.
: ROOM-BY ( n n -- ready-result ) {: fd:n deadline:n :}
   begin
      fd POLLOUT deadline LEFT-MS POLL-FD {: rc:n :}
      rc 0 > if TCP4-READY--RESULT:READY exit then
      rc 0= if TCP4-READY--RESULT:IDLE exit then
      rc negate EINTR <> if rc negate >ERRNO TCP4-READY--RESULT:FAILED exit then
   again ;


: AFTER-ROOM ( n str n -- slice-result ) {: fd:n bytes size:n deadline:n :}
   fd deadline ROOM-BY
   MATCH ready-result
      ready OF fd bytes size deadline SEND-BY ENDOF
      idle OF TCP4-SLICE--RESULT:EMPTY ENDOF
      failed OF TCP4-SLICE--RESULT:FAILED ENDOF
   ;MATCH ;


: ACCEPT-LOOP ( n -- accept-result ) {: fd:n :}
   begin
      ENDPOINT-OUTPUT fd ACCEPT-RAW dup 0 >= if
         >CONNECTION ENDPOINT@ TCP4-ACCEPT--RESULT:ACCEPTED exit
      then drop
      LAST-ERROR dup INTERRUPTED? 0= if TCP4-ACCEPT--RESULT:FAILED exit then drop
   again ;


\ An errno Linux's accept(2) passes on from the pending connection, not the
\ listener, and says to treat like EAGAIN and retry, by its Linux number
\ (asm-generic/errno.h), which names another errno on macOS: ENONET 64, EPROTO
\ 71, ENOPROTOOPT 92, EOPNOTSUPP 95, ENETDOWN 100, ENETUNREACH 101, EHOSTDOWN
\ 112, EHOSTUNREACH 113.
: LINUX-PENDING? ( n -- bool ) {: code:n :}
   code 64 = code 71 = or code 92 = or code 95 = or
   code 100 = or code 101 = or code 112 = or code 113 = or ;


: ACCEPT-ABORTED? ( n -- bool ) {: code:n :}
   code ECONNABORTED = code EAGAIN = or if true exit then
   HB-TARGET-LINUX-KERNEL? if code LINUX-PENDING? exit then
   false ;


: ACCEPT-STARVED? ( n -- bool ) {: code:n :}
   code EMFILE = code ENFILE = or code ENOBUFS = or code ENOMEM = or ;


public


\ A numeric IPv4 address, e.g. $7F000001 for 127.0.0.1. Address zero binds every
\ local IPv4 interface.
: ADDRESS ( n -- address )
   dup 0 MAX-ADDRESS WITHIN-RANGE >ADDRESS ;


: PORT ( n -- port )
   dup 0 MAX-PORT WITHIN-RANGE >PORT ;


\ A span's length for the transfers below, each of which checks it against its
\ own range: READ, READ-EXACT, TRY-READ and WRITE refuse more than one call
\ moves, and SEND-SLICE and TRY-SEND hand the OS part of a longer span.
: TRANSFER-BYTES ( n -- NUM:byte-len )
   LENGTH ;


: RECEIVING ( -- direction )
   construct direction receiving ;


: SENDING ( -- direction )
   construct direction sending ;


: BOTH ( -- direction )
   construct direction both ;


\ BIND returns an owned listener; the caller closes it exactly once. Port zero
\ asks the OS for an ephemeral port, which LOCAL then reports. SO_REUSEADDR
\ permits an immediate restart while the prior server peer remains in TIME_WAIT.
\ A listener stays blocking.
: BIND ( address port -- bind-result )
   ENDPOINT! SOCKET-RAW dup 0 < if drop LAST-ERROR TCP4-BIND--RESULT:FAILED exit then
   {: fd:n :}
   fd REUSEADDR-RAW 0 < if fd BIND-FAILED exit then
   fd BIND-RAW 0 < if fd BIND-FAILED exit then
   fd >LISTENER TCP4-BIND--RESULT:BOUND ;


: LISTEN ( listener n -- status ) {: lis:listener backlog:n :}
   backlog 1 MAX-BACKLOG WITHIN-RANGE
   lis LISTENER>N backlog LISTEN-RAW RC>STATUS ;


: LOCAL ( listener -- endpoint-result )
   LISTENER>N ENDPOINT-OUTPUT
   LOCAL-RAW 0 < if LAST-ERROR TCP4-ENDPOINT--RESULT:FAILED
   else ENDPOINT@ TCP4-ENDPOINT--RESULT:ENDPOINT then ;


\ Blocks until a connection arrives. The accepted connection is owned by the
\ caller and closed separately from the listener.
: ACCEPT ( listener -- accept-result )
   LISTENER>N ACCEPT-LOOP ;


: >ACCEPT-FAULT ( errno -- accept-fault )
   ERRNO>N {: code:n :}
   code ACCEPT-ABORTED? if construct accept-fault aborted exit then
   code ACCEPT-STARVED? if construct accept-fault starved exit then
   construct accept-fault broken ;


\ Blocks until the handshake completes or the OS refuses it. An interrupted
\ CONNECT is `failed` with EINTR and its socket closed, because Linux goes on
\ completing that connection asynchronously. The connection turns non-blocking
\ only once connected, so the connect itself still blocks and answers the
\ refusal.
: CONNECT ( address port -- connect-result )
   ENDPOINT! SOCKET-RAW dup 0 < if drop LAST-ERROR TCP4-CONNECT--RESULT:FAILED exit then
   {: fd:n :}
   fd CONNECT-RAW 0 < if LAST-ERROR fd CLOSE-RAW drop TCP4-CONNECT--RESULT:FAILED exit then
   fd NONBLOCK >CONNECTION TCP4-CONNECT--RESULT:CONNECTED ;


\ One partial read of 1..capacity bytes, waiting in poll(2) on the task's own
\ thread, with no AIO loop or deadline, until the stream answers. The caller owns
\ the writable span. `closed` carries zero bytes.
: READ ( connection ^u8 NUM:byte-len -- read-result )
   {: conn:connection bytes capacity:NUM:byte-len :}
   capacity BLEN>N 1 MAX-TRANSFER WITHIN-RANGE
   conn CONNECTION>N bytes capacity BLEN>N READ-CHUNK ;


\ One receive of 1..capacity bytes that never waits: `empty` when the stream
\ has nothing to answer yet, so one task can serve many connections from its own
\ readiness loop even where the OS reports readiness spuriously, as Linux may.
\ The caller owns the writable span. `closed` carries zero bytes. The connection
\ must be non-blocking.
: TRY-READ ( connection ^u8 NUM:byte-len -- try-read-result )
   {: conn:connection bytes capacity:NUM:byte-len :}
   capacity BLEN>N 1 MAX-TRANSFER WITHIN-RANGE
   conn CONNECTION>N bytes capacity BLEN>N TRY-CHUNK ;


\ Blocks until the whole span is filled. An end of stream before that is
\ `closed` carrying the bytes already written into the span.
: READ-EXACT ( connection ^u8 NUM:byte-len -- read-result )
   {: conn:connection bytes want:NUM:byte-len :}
   want BLEN>N 1 MAX-TRANSFER WITHIN-RANGE
   0
   begin
      dup want BLEN>N < 0= if drop want TCP4-READ--RESULT:DATA exit then
      dup conn bytes want EXACT-STEP
      MATCH read-result
         data OF BLEN>N + ENDOF
         closed OF drop N>BLEN TCP4-READ--RESULT:CLOSED exit ENDOF
         failed OF swap drop TCP4-READ--RESULT:FAILED exit ENDOF
      ;MATCH
   again ;


\ Part of the span, within SEND-SLICE-MS and two sends that never wait, whatever
\ the peer does: a send; when it finds no room, a wait of SEND-SLICE-MS at most
\ for some, then one more send. `moved` carries the bytes the OS took, at most
\ what one call moves of a longer span; `empty` says the last send found no room
\ or the wait ended with none; `failed` carries the errno. An interrupt retries
\ a send or the rest of the wait, never past the slice. The bound holds on every
\ connection this module makes (ACCEPT, CONNECT, SOCKET), each non-blocking.
\ >CONNECTION sets no flag, so on a descriptor made elsewhere it holds once the
\ descriptor's owner has set O_NONBLOCK; a send on a blocking one waits in the
\ kernel for as long as the peer takes. Nothing here pauses or weighs a bound;
\ a caller writing a span in slices does both between them.
: SEND-SLICE ( connection ^u8 NUM:byte-len -- slice-result )
   {: conn:connection bytes size:NUM:byte-len :}
   size SEND-SPAN {: span:n :}
   conn CONNECTION>N {: fd:n :}
   mono-ns SEND-SLICE-MS NS-PER-MS * + {: deadline:n :}
   fd bytes span deadline SEND-BY
   MATCH slice-result
      moved OF TCP4-SLICE--RESULT:MOVED ENDOF
      empty OF fd bytes span deadline AFTER-ROOM ENDOF
      failed OF TCP4-SLICE--RESULT:FAILED ENDOF
   ;MATCH ;


\ One send of the span's first MAX-TRANSFER bytes at most that never waits:
\ `moved` carries the bytes the OS took, `empty` says there was no room, and
\ `failed` carries the errno. The connection must be non-blocking.
: TRY-SEND ( connection ^u8 NUM:byte-len -- slice-result )
   {: conn:connection bytes size:NUM:byte-len :}
   size SEND-SPAN {: span:n :}
   conn CONNECTION>N bytes span NO-DEADLINE SEND-BY ;


\ Blocks until every byte is accepted by the OS, which is not delivery. A failed
\ write may have transmitted part of the span; the stream is then unusable.
\ Every send slice that leaves bytes to send ends in a TASK:PAUSE, where a halt
\ ends the task, whether it moved some or none: a peer that takes a few bytes in
\ every slice holds the write for as long as it reads, but not past a halt.
: WRITE ( connection ^u8 NUM:byte-len -- status )
   {: conn:connection bytes size:NUM:byte-len :}
   size BLEN>N 0 MAX-TRANSFER WITHIN-RANGE
   size BLEN>N 0= if TCP4-STATUS:OK exit then
   0
   begin {: sent:n :}
      conn bytes sent + size BLEN>N sent - N>BLEN SEND-SLICE
      MATCH slice-result
         moved OF BLEN>N sent + ENDOF
         empty OF sent ENDOF
         failed OF TCP4-STATUS:FAILED exit ENDOF
      ;MATCH
      dup size BLEN>N >= if drop TCP4-STATUS:OK exit then
      TASK:PAUSE
   again ;


\ Turns Nagle's algorithm off (true) or back on (false), so a small send leaves
\ at once instead of waiting for the previous segment's acknowledgement.
: NODELAY! ( connection bool -- status ) {: conn:connection on:bool :}
   on if 1 else 0 then OPTION LE:U32!
   conn CONNECTION>N IPPROTO-TCP TCP-NODELAY OPTION 4 OPTION-CALL RC>STATUS ;


: UNREAD ( connection -- queue-result )
   CONNECTION>N {: fd:n :}
   fd FIONREAD OPTION QUEUE-CALL QUEUED ;


\ The bytes this end has written that the connection still holds, unsent or
\ sent and not yet acknowledged: Darwin's SO_NWRITE, Linux's SIOCOUTQ.
: UNSENT ( connection -- queue-result )
   CONNECTION>N {: fd:n :}
   HB-TARGET-MACOS? if
      4 ADDRLEN LE:U32!
      fd SOL-SOCKET SO-NWRITE OPTION ADDRLEN OPTION-GET-CALL QUEUED exit
   then
   fd SIOCOUTQ OPTION QUEUE-CALL QUEUED ;


\ Answers whether a READ would return at once. `ready` includes the end of
\ stream and a failed connection, whose answers the following READ reports.
: READABLE? ( connection -- ready-result )
   CONNECTION>N 0 >MS WAIT-READY ;


\ True once the peer has closed its side of the connection or the connection
\ has failed, whatever it left unread. One poll that never waits; it takes no byte.
: HUNG-UP? ( connection -- bool )
   CONNECTION>N {: fd:n :}
   begin
      fd HANG-EVENTS 0 POLL-FD {: rc:n :}
      rc 0 > if POLLFD-ROW @ FD:REVENTS HANG-BITS and 0<> exit then
      rc 0= if false exit then
      rc negate EINTR <> if E-RESULT throw then
   again ;


\ Parks the task on the AIO loop until the stream would answer a READ or the
\ deadline passes, which is `idle`; ms -1 parks it with no deadline.
: READABLE-WITHIN? ( connection ms -- ready-result )
   {: conn:connection timeout:ms :}
   conn CONNECTION>N timeout WAIT-READY ;


: PENDING? ( listener -- ready-result )
   LISTENER>N 0 >MS WAIT-READY ;


\ The same wait for an arriving connection: `ready` means ACCEPT answers at
\ once, `idle` that the deadline passed with the queue still empty; ms -1 parks
\ it with no deadline.
: PENDING-WITHIN? ( listener ms -- ready-result )
   {: lis:listener timeout:ms :}
   lis LISTENER>N timeout WAIT-READY ;


\ Half-closes the stream in one or both directions; the descriptor stays open
\ until CLOSE. Shutting down sending sends the peer the end of stream.
: SHUTDOWN ( connection direction -- status ) {: conn:connection how:direction :}
   conn CONNECTION>N how SHUT-CODE SHUTDOWN-RAW RC>STATUS ;


\ The caller closes each handle once and never reuses it, even after an
\ interrupted close: Linux releases the descriptor before reporting the error.
\ Descriptors are process resources: close them and stop concurrent use before
\ image capture.
: CLOSE ( connection -- status )
   CONNECTION>N CLOSE-RAW RC>STATUS ;


: CLOSE-LISTENER ( listener -- status )
   LISTENER>N CLOSE-RAW RC>STATUS ;

\ The descriptor under a listener or a connection, for a readiness or completion
\ loop. A descriptor read out still belongs to its handle; CLOSE or
\ CLOSE-LISTENER ends it.
: LISTENER-FD ( listener -- fd )
   LISTENER>N >FD ;

: CONNECTION-FD ( connection -- fd )
   CONNECTION>N >FD ;

\ A fresh AF_INET stream socket, bound to nothing and connected to nothing, with
\ the flags this module's own BIND and CONNECT ask for, and non-blocking like
\ every connection this module makes. The caller owns it and closes it with
\ CLOSE.
: SOCKET ( -- socket-result )
   SOCKET-RAW dup 0 < if drop LAST-ERROR TCP4-SOCKET--RESULT:FAILED exit then
   NONBLOCK >CONNECTION TCP4-SOCKET--RESULT:OPENED ;

;package
