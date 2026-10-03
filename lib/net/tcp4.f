\ Linux and Darwin AArch64 IPv4 stream sockets through exact, bounded libc bindings.
\
\ STORAGE CLASS. TASK-LOCAL. The endpoint storage is one $20 TASK:+USER row, so
\ each task holds its own sockaddr, socklen, socket option int and pollfd, and
\ any number of tasks may bind, accept, read and write at once. The payload
\ spans READ, READ-EXACT, SEND-SLICE and WRITE take are caller-owned. See
\ docs/threads.md.
\
\ Every readiness wait a caller asks for runs on the AIO loop (docs/aio.md):
\ WAIT-READY submits one POLL and awaits it, so a waiting task costs no thread of
\ its own. A program calls AIO:START before its first wait; a wait with no loop
\ is E-AIO-STATE.
\
\ Every connection this module makes is non-blocking before the caller has it:
\ ACCEPT's and SOCKET's from the start, CONNECT's once its blocking connect has
\ answered. So no send or receive waits in the kernel. READ and READ-EXACT wait
\ for data in poll(2) on the task's own thread instead, with no AIO loop and no
\ deadline, as the blocking receive did. SEND-SLICE waits there at most
\ SEND-SLICE-MS for room between two sends, so a slice returns within
\ SEND-SLICE-MS and those two sends, whatever the peer does. WRITE loops over
\ slices with a TASK:PAUSE before each next one, so a halt ends a task writing
\ to any peer within a slice, one that stopped reading or one that reads slowly.
require lib/errors.f
require lib/ffi-abi.f
require lib/type/deftype.f
require lib/num-types.f
require lib/task.f
require lib/aio.f
require lib/le.f                  \ the socklen cell the endpoint carries

package TCP4
public

DEFTYPE ADDRESS
DEFTYPE PORT
DEFTYPE LISTENER
DEFTYPE CONNECTION
DEFTYPE ERRNO

ENUM direction receiving sending both ;ENUM

SUMTYPE bind-result 0
   VARIANT bound listener ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

SUMTYPE connect-result 0
   VARIANT connected connection ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

SUMTYPE accept-result 0
   VARIANT accepted connection address port ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

SUMTYPE endpoint-result 0
   VARIANT endpoint address port ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

SUMTYPE status 0
   VARIANT ok ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

\ One transfer's outcome. `closed` carries the bytes delivered into the caller's
\ buffer before the peer's end of stream, which READ-EXACT reports as a partial.
SUMTYPE read-result 0
   VARIANT data NUM:byte-len ;VARIANT
   VARIANT closed NUM:byte-len ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

SUMTYPE ready-result 0
   VARIANT ready ;VARIANT
   VARIANT idle ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

\ One send slice's outcome. `moved` carries the bytes the OS accepted, at least
\ one and no more than the span; `empty` says there was no room, even once a
\ wait of SEND-SLICE-MS for some had ended; `failed` carries the errno.
ENUM slice-result 0
   VARIANT moved FIELD sent NUM:byte-len ;VARIANT
   VARIANT empty ;VARIANT
   VARIANT failed FIELD cause errno ;VARIANT
;ENUM

\ A socket that exists but is connected to nothing yet. It is a `connection`
\ because that is what it becomes and what closes it; until something connects
\ it, reading or writing it is the OS's ENOTCONN.
SUMTYPE socket-result 0
   VARIANT opened connection ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

\ How many bytes one of a connection's two queues holds, as the OS counts them.
SUMTYPE queue-result 0
   VARIANT queued NUM:byte-len ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

E-TCP4-OPERAND constant E-OPERAND
E-TCP4-PLATFORM constant E-PLATFORM
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
: SOCKET-FLAGS ( -- n ) HB-TARGET-MACOS? if 1 else $80001 then ;       \ SOCK_STREAM | SOCK_CLOEXEC.
$80800 constant ACCEPT-FLAGS       \ SOCK_CLOEXEC | SOCK_NONBLOCK on the accepted connection.
: MSG-NOSIGNAL ( -- n ) HB-TARGET-MACOS? if $80000 else $4000 then ;        \ A write to a closed peer fails; it never signals.
4 constant F-SETFL
: O-NONBLOCK ( -- n ) HB-TARGET-MACOS? if 4 else $800 then ;               \ Linux's SOCK_NONBLOCK is the same bit.
$39 constant POLL-DONE             \ POLLIN|POLLERR|POLLHUP|POLLNVAL: a read will not block.
$01 constant POLLIN                \ a receive will not block, on both.
$04 constant POLLOUT               \ a send will not block, on both.
-1 constant NO-TIMEOUT             \ poll(2) waits until ready; Darwin takes no other negative one.
$7FFFFFFF constant MAX-TIMEOUT     \ the widest deadline this module accepts, in milliseconds.
1000000 constant NS-PER-MS
4 constant EINTR
: EAGAIN ( -- n ) HB-TARGET-MACOS? if 35 else 11 then ;               \ EWOULDBLOCK on both.
6 constant IPPROTO-TCP
1 constant TCP-NODELAY
: FIONREAD ( -- n ) HB-TARGET-MACOS? if $4004667F else $541B then ;     \ bytes received and not yet read, on both.
$5411 constant SIOCOUTQ            \ Linux: bytes written and not yet acknowledged.
$FFFF constant DARWIN-SOL-SOCKET
$1024 constant SO-NWRITE           \ Darwin's count of SIOCOUTQ, a socket option rather than an ioctl.
0 constant SHUT-RD
1 constant SHUT-WR
2 constant SHUT-RDWR

\ Endpoint storage is per task, matching FFI's argument/extent tables: a $10
\ sockaddr_in, a $04 socklen_t, the $04 int a socket option is set from, and the
\ $08 pollfd a transfer waits through (int fd, short events, short revents),
\ which end on the cell so the row after this one still starts aligned.
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and $20 TASK:+USER IO-STORAGE drop
: SOCKADDR ( -- ptr u8 ) IO-STORAGE BYTE-VIEW ;
: ADDRLEN ( -- ptr u8 ) IO-STORAGE BYTE-VIEW $10 + ;
: OPTION ( -- ptr u8 ) IO-STORAGE BYTE-VIEW $14 + ;
: POLLFD ( -- ptr u8 ) IO-STORAGE BYTE-VIEW $18 + ;

CAST: BLEN>N ( NUM:byte-len -- n )


: WITHIN-RANGE ( n n n -- ) {: value:n minimum:n maximum:n :}
   value minimum < value maximum > or if E-OPERAND throw then ;


: LENGTH ( n -- NUM:byte-len )
   NUM:BYTE-LEN MATCH NUM:numeric-result
      ok OF ENDOF
      negative OF E-OPERAND throw ENDOF
      zero OF E-OPERAND throw ENDOF
      overflow OF E-OPERAND throw ENDOF
      underflow OF E-OPERAND throw ENDOF
      bad-alignment OF E-OPERAND throw ENDOF
      misaligned OF E-OPERAND throw ENDOF
   ;MATCH ;


: CHECK-FD ( n -- )
   0 $7FFFFFFF WITHIN-RANGE ;


: LISTENER-FD ( listener -- n )
   LISTENER>N dup CHECK-FD ;


: CONNECTION-FD ( connection -- n )
   CONNECTION>N dup CHECK-FD ;


: BE16! ( n ptr u8 -- ) {: value:n target :}
   value 8 rshift $FF and target c! value $FF and target $01 + c! ;


: BE16@ ( ptr u8 -- n ) {: source :}
   source c@ 8 lshift source $01 + c@ or ;


: BE32! ( n ptr u8 -- ) {: value:n target :}
   value 16 rshift target BE16! value target $02 + BE16! ;


: BE32@ ( ptr u8 -- n ) {: source :}
   source BE16@ 16 lshift source $02 + BE16@ or ;


: CLEAR-ENDPOINT ( -- )
   SOCKADDR-BYTES 0 do 0 SOCKADDR i + c! loop ;


: ENDPOINT! ( address port -- ) {: address:address port:port :}
   address ADDRESS>N 0 MAX-ADDRESS WITHIN-RANGE
   port PORT>N 0 MAX-PORT WITHIN-RANGE
   CLEAR-ENDPOINT
   HB-TARGET-MACOS? if
      SOCKADDR-BYTES SOCKADDR c! 2 SOCKADDR 1+ c!
   else 2 SOCKADDR c! then
   port PORT>N SOCKADDR $02 + BE16!
   address ADDRESS>N SOCKADDR $04 + BE32! ;


: ENDPOINT-OUTPUT ( -- )
   CLEAR-ENDPOINT SOCKADDR-BYTES ADDRLEN LE:U32! ;


: ENDPOINT@ ( -- address port )
   ADDRLEN LE:U32@ SOCKADDR-BYTES <> if E-RESULT throw then
   HB-TARGET-MACOS? if
      SOCKADDR c@ SOCKADDR-BYTES <> SOCKADDR 1+ c@ 2 <> or
   else SOCKADDR c@ 2 <> SOCKADDR 1+ c@ 0 <> or then
   if E-RESULT throw then
   SOCKADDR $04 + BE32@ >ADDRESS SOCKADDR $02 + BE16@ >PORT ;


: SHUT-CODE ( direction -- n )
   MATCH direction
      receiving OF SHUT-RD ENDOF
      sending OF SHUT-WR ENDOF
      both OF SHUT-RDWR ENDOF
   ;MATCH ;


\ The exact Linux AArch64 libc schemas. RTLD_DEFAULT borrows process symbols;
\ the native executable already needs libc.so.6, so no library reference is
\ acquired or retained here. Each declaration states the C function's own effect
\ and the extent of every buffer the callee writes, which is what the bounded
\ call guards; package FFI resolves each symbol on its first call and clears the
\ cache for image capture. Coverage: lib/net/tcp4-test.f drives every binding
\ over a loopback connection.
PROCESS-SYMBOLS

FUNCTION: SOCKET-CALL socket ( n n n -- i32 ) ;FUNCTION
FUNCTION: BIND-CALL bind ( n ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: LISTEN-CALL listen ( n n -- i32 ) ;FUNCTION
FUNCTION: CONNECT-CALL connect ( n ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: SEND-CALL send ( n ptr u8 n n -- n ) ;FUNCTION
FUNCTION: OPTION-CALL setsockopt ( n n n ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: SHUTDOWN-CALL shutdown ( n n -- i32 ) ;FUNCTION
FUNCTION: CLOSE-CALL close ( n -- i32 ) ;FUNCTION

FUNCTION: DARWIN-ACCEPT accept ( n ptr u8 ptr u8 -- i32 )
   1 $10 WRITES-BYTES
   2 $04 WRITES-BYTES
;FUNCTION

FUNCTION: ACCEPT-CALL accept4 ( n ptr u8 ptr u8 n -- i32 )
   1 $10 WRITES-BYTES                     \ sockaddr_in
   2 $04 WRITES-BYTES                     \ socklen_t
;FUNCTION

FUNCTION: LOCAL-CALL getsockname ( n ptr u8 ptr u8 -- i32 )
   1 $10 WRITES-BYTES                     \ sockaddr_in
   2 $04 WRITES-BYTES                     \ socklen_t
;FUNCTION

FUNCTION: RECEIVE-CALL recv ( n ptr u8 n n -- n )
   1 2 WRITES-ARG                         \ the caller's payload span
;FUNCTION

\ ioctl's third argument is variadic: Darwin passes it on the stack.
FUNCTION: QUEUE-CALL ioctl ( n n ptr u8 -- i32 )
   2 VARIADIC
   2 $04 WRITES-BYTES                     \ the int count
;FUNCTION

FUNCTION: OPTION-GET-CALL getsockopt ( n n n ptr u8 ptr u8 -- i32 )
   3 $04 WRITES-BYTES                     \ the int value
   4 $04 WRITES-BYTES                     \ socklen_t
;FUNCTION


: LAST-ERROR ( -- errno )
   FFI:ERRNO >ERRNO ;


: INTERRUPTED? ( errno -- bool )
   ERRNO>N EINTR = ;


\ The call would have waited: no room to send, or nothing to receive.
: WOULD-BLOCK? ( errno -- bool )
   ERRNO>N EAGAIN = ;


: OWN-SOCKET ( n -- n ) {: fd:n :}
   fd 0 < HB-TARGET-MACOS? 0= or if fd exit then
   fd 2 1 fcntl 0 <> if fd CLOSE-CALL drop E-RESULT throw then
   fd ;


\ Makes a connection non-blocking, or closes it and throws. The status flags are
\ set outright: a fresh or just-connected socket has no other to keep, and
\ Darwin's accepted socket, which inherits its listener's, is left with this one
\ alone, as Linux's accept4 leaves it.
: NONBLOCK ( n -- n ) {: fd:n :}
   fd F-SETFL O-NONBLOCK fcntl 0 <> if fd CLOSE-CALL drop E-RESULT throw then
   fd ;

: SOCKET-RAW ( -- n )
   2 SOCKET-FLAGS 0 SOCKET-CALL OWN-SOCKET ;


: BIND-RAW ( n -- n ) {: fd:n :}
   fd SOCKADDR SOCKADDR-BYTES BIND-CALL ;


: LISTEN-RAW ( n n -- n )                 \ fd backlog
   LISTEN-CALL ;


\ Linux's accept4 makes the connection close-on-exec and non-blocking as it
\ accepts it; Darwin's accept takes no flags, so the connection gets both after.
: ACCEPT-RAW ( n -- n ) {: fd:n :}
   HB-TARGET-MACOS? 0= if fd SOCKADDR ADDRLEN ACCEPT-FLAGS ACCEPT-CALL exit then
   fd SOCKADDR ADDRLEN DARWIN-ACCEPT OWN-SOCKET
   dup 0 < if exit then NONBLOCK ;


: CONNECT-RAW ( n -- n ) {: fd:n :}
   fd SOCKADDR SOCKADDR-BYTES CONNECT-CALL ;


: LOCAL-RAW ( n -- n ) {: fd:n :}
   fd SOCKADDR ADDRLEN LOCAL-CALL ;


: RECEIVE-RAW ( n ptr u8 n -- n ) {: fd:n bytes size:n :}
   fd bytes size 0 RECEIVE-CALL ;


: SEND-RAW ( n ptr u8 n -- n ) {: fd:n bytes size:n :}
   fd bytes size MSG-NOSIGNAL SEND-CALL ;


: SHUTDOWN-RAW ( n n -- n )               \ fd how
   SHUTDOWN-CALL ;


: CLOSE-RAW ( n -- n )
   CLOSE-CALL ;


: RC>STATUS ( n -- status )
   0 < if LAST-ERROR TCP4-STATUS:failed else TCP4-STATUS:ok then ;


\ The count the call that answered rc wrote into the option int, or its errno.
: QUEUED ( n -- queue-result )
   0 < if LAST-ERROR TCP4-QUEUE--RESULT:failed exit then
   OPTION LE:U32@ LENGTH TCP4-QUEUE--RESULT:queued ;


\ POLLERR, POLLHUP and POLLNVAL all mean a read returns at once, with the end of
\ stream or the error as its answer, so they are readiness and not a failure.
: REVENTS>READY ( n -- ready-result )
   POLL-DONE and 0 <> if TCP4-READY--RESULT:ready exit then
   TCP4-READY--RESULT:idle ;


\ The one readiness path: every question a listener or a connection asks reaches
\ the AIO loop through here, with the timeout as their only difference. The
\ deadline is the poll's own linked timeout, so a signal no longer cuts the wait
\ short and there is nothing to restart; a zero timeout asks the question with a
\ zero-length link, which still answers `ready` for a descriptor already ready.
\ The loop must be running: a wait without AIO:START is E-AIO-STATE.
\ `cancelled` cannot arrive here - nothing in this module cancels, the ticket
\ never leaves this word, and the cleanup AIO registers on a submitting task runs
\ only after that task has ended - so it is a broken foreign result, exactly like
\ a sockaddr this module did not write.
: WAIT-READY ( n ms -- ready-result ) {: fd:n timeout:ms :}
   timeout MS>N 0 MAX-TIMEOUT WITHIN-RANGE
   fd >FD AIO:READABLE timeout AIO:POLL AIO:AWAIT
   MATCH AIO:outcome
      ready OF REVENTS>READY ENDOF
      timed-out OF TCP4-READY--RESULT:idle ENDOF
      cancelled OF E-RESULT throw ENDOF
      refused OF >ERRNO TCP4-READY--RESULT:failed ENDOF
   ;MATCH ;


\ A transfer longer than the span handed to the OS is not a short answer this
\ module can report, so it is refused as a broken foreign result rather than
\ trusted as a length.
: RECEIVED ( n n -- read-result ) {: actual:n size:n :}
   actual size > if E-RESULT throw then
   actual LENGTH TCP4-READ--RESULT:data ;


\ One poll(2) of the descriptor for the events, on this task's own thread with
\ no AIO loop: how many descriptors are ready, 0 once the timeout in
\ milliseconds has passed, or the errno negated. NO-TIMEOUT waits until ready.
: POLL-FD ( n n n -- n ) {: fd:n events:n timeout:n :}
   fd POLLFD LE:U32!
   events POLLFD 4 + LE:U16!
   POLLFD 1 timeout poll ;


\ Blocks until a receive on the descriptor will not wait - there is data, the
\ end of stream, or an error the receive reports - which is `ok`. A signal
\ restarts the wait; any other refusal is `failed` with its errno.
: DATA-READY ( n -- status ) {: fd:n :}
   begin
      fd POLLIN NO-TIMEOUT POLL-FD {: rc:n :}
      rc 0 >= if TCP4-STATUS:ok exit then
      rc negate EINTR <> if rc negate >ERRNO TCP4-STATUS:failed exit then
   again ;


\ What a receive that failed with the errno leaves: `ok` to receive again,
\ after an interrupt or once a wait finds the stream answering, or `failed`.
: RECEIVE-AGAIN ( n errno -- status ) {: fd:n cause:errno :}
   cause INTERRUPTED? if TCP4-STATUS:ok exit then
   cause WOULD-BLOCK? if fd DATA-READY exit then
   cause TCP4-STATUS:failed ;


\ One receive, tried again after an interrupt and, while there is nothing to
\ receive yet, after a wait for data, so a read on a non-blocking connection
\ still blocks until the stream answers. A stream read of zero bytes is the
\ peer's orderly end of stream, never an empty message.
: READ-CHUNK ( n ptr u8 n -- read-result ) {: fd:n bytes size:n :}
   begin
      fd bytes size RECEIVE-RAW dup 0 > if size RECEIVED exit then
      0= if 0 LENGTH TCP4-READ--RESULT:closed exit then
      fd LAST-ERROR RECEIVE-AGAIN
      MATCH status
         ok OF ENDOF
         failed OF TCP4-READ--RESULT:failed exit ENDOF
      ;MATCH
   again ;


: EXACT-STEP ( n connection ptr u8 NUM:byte-len -- read-result )
   {: got:n conn:connection bytes want:NUM:byte-len :}
   conn CONNECTION-FD bytes got + want BLEN>N got - READ-CHUNK ;


\ One send of the span that never waits for room. A count past the span is a
\ broken foreign result.
: SEND-STEP ( n ptr u8 n -- n ) {: fd:n bytes size:n :}
   fd bytes size SEND-RAW
   dup size > if E-RESULT throw then ;


\ One send that never waits: `moved`, `empty` when there was no room, or
\ `failed`. An interrupt retries it until the slice's mono-ns deadline has
\ passed and is `empty` after that, so no retry carries the slice past it.
: TRY-SEND ( n ptr u8 n n -- slice-result ) {: fd:n bytes size:n deadline:n :}
   begin
      fd bytes size SEND-STEP
      dup 0 > if LENGTH TCP4-SLICE--RESULT:moved exit then
      0= if E-RESULT throw then
      LAST-ERROR dup WOULD-BLOCK? if drop TCP4-SLICE--RESULT:empty exit then
      dup INTERRUPTED? 0= if TCP4-SLICE--RESULT:failed exit then drop
      mono-ns deadline > if TCP4-SLICE--RESULT:empty exit then
   again ;


\ Milliseconds left before a mono-ns deadline, never below zero.
: LEFT-MS ( n -- n ) {: deadline:n :}
   deadline mono-ns - NS-PER-MS / 0 max ;


\ Blocks until a send on the descriptor will not wait - there is room, or an
\ error the send reports - which is `ready`, or the mono-ns deadline passes,
\ `idle`. A signal restarts the wait with what is left of it, so none stretches
\ it; any other refusal is `failed` with its errno.
: ROOM-BY ( n n -- ready-result ) {: fd:n deadline:n :}
   begin
      fd POLLOUT deadline LEFT-MS POLL-FD {: rc:n :}
      rc 0 > if TCP4-READY--RESULT:ready exit then
      rc 0= if TCP4-READY--RESULT:idle exit then
      rc negate EINTR <> if rc negate >ERRNO TCP4-READY--RESULT:failed exit then
   again ;


\ The rest of a slice whose first send found no room: a wait for some until the
\ slice's mono-ns deadline, then one more send that never waits.
: AFTER-ROOM ( n ptr u8 n n -- slice-result ) {: fd:n bytes size:n deadline:n :}
   fd deadline ROOM-BY
   MATCH ready-result
      ready OF fd bytes size deadline TRY-SEND ENDOF
      idle OF TCP4-SLICE--RESULT:empty ENDOF
      failed OF TCP4-SLICE--RESULT:failed ENDOF
   ;MATCH ;


: ACCEPT-LOOP ( n -- accept-result ) {: fd:n :}
   begin
      ENDPOINT-OUTPUT fd ACCEPT-RAW dup 0 >= if
         >CONNECTION ENDPOINT@ TCP4-ACCEPT--RESULT:accepted exit
      then drop
      LAST-ERROR dup INTERRUPTED? 0= if TCP4-ACCEPT--RESULT:failed exit then drop
   again ;


public


\ A numeric IPv4 address, e.g. $7F000001 for 127.0.0.1. Address zero binds every
\ local IPv4 interface. Host names and dotted decimal are not this module's job.
: ADDRESS ( n -- address )
   dup 0 MAX-ADDRESS WITHIN-RANGE >ADDRESS ;


: PORT ( n -- port )
   dup 0 MAX-PORT WITHIN-RANGE >PORT ;


\ A span's length for the transfers below, each of which checks it against its
\ own range: READ, READ-EXACT and WRITE refuse more than one call moves, and
\ SEND-SLICE hands the OS part of a longer span.
: TRANSFER-BYTES ( n -- NUM:byte-len )
   LENGTH ;


: RECEIVING ( -- direction )
   construct direction receiving ;


: SENDING ( -- direction )
   construct direction sending ;


: BOTH ( -- direction )
   construct direction both ;


\ BIND returns an owned listener; the caller closes it exactly once. Port zero
\ asks the OS for an ephemeral port, which LOCAL then reports.
: BIND ( address port -- bind-result )
   ENDPOINT! SOCKET-RAW dup 0 < if drop LAST-ERROR TCP4-BIND--RESULT:failed exit then
   {: fd:n :}
   fd BIND-RAW 0 < if
      LAST-ERROR fd CLOSE-RAW drop TCP4-BIND--RESULT:failed
   else fd >LISTENER TCP4-BIND--RESULT:bound then ;


\ The backlog is the queue of connections completed but not yet accepted; Linux
\ caps it at SOMAXCONN.
: LISTEN ( listener n -- status ) {: lis:listener backlog:n :}
   backlog 1 MAX-BACKLOG WITHIN-RANGE
   lis LISTENER-FD backlog LISTEN-RAW RC>STATUS ;


: LOCAL ( listener -- endpoint-result )
   LISTENER-FD ENDPOINT-OUTPUT
   LOCAL-RAW 0 < if LAST-ERROR TCP4-ENDPOINT--RESULT:failed
   else ENDPOINT@ TCP4-ENDPOINT--RESULT:endpoint then ;


\ Blocks until a connection arrives. The accepted connection is owned by the
\ caller and closed separately from the listener. Use PENDING? to avoid waiting.
: ACCEPT ( listener -- accept-result )
   LISTENER-FD ACCEPT-LOOP ;


\ Blocks until the handshake completes or the OS refuses it. A refused peer
\ answers failed with ECONNREFUSED; the failed socket is closed here. The
\ connection turns non-blocking only once connected, so the connect itself
\ still blocks and answers the refusal.
: CONNECT ( address port -- connect-result )
   ENDPOINT! SOCKET-RAW dup 0 < if drop LAST-ERROR TCP4-CONNECT--RESULT:failed exit then
   {: fd:n :}
   fd CONNECT-RAW 0 < if LAST-ERROR fd CLOSE-RAW drop TCP4-CONNECT--RESULT:failed exit then
   fd NONBLOCK >CONNECTION TCP4-CONNECT--RESULT:connected ;


\ One partial read of 1..capacity bytes, blocking until the stream answers.
\ The caller owns the writable span. `closed` carries zero bytes.
: READ ( connection ptr u8 NUM:byte-len -- read-result )
   {: conn:connection bytes capacity:NUM:byte-len :}
   capacity BLEN>N 1 MAX-TRANSFER WITHIN-RANGE
   conn CONNECTION-FD bytes capacity BLEN>N READ-CHUNK ;


\ Blocks until the whole span is filled. An end of stream before that is
\ `closed` carrying the bytes already written into the span.
: READ-EXACT ( connection ptr u8 NUM:byte-len -- read-result )
   {: conn:connection bytes want:NUM:byte-len :}
   want BLEN>N 1 MAX-TRANSFER WITHIN-RANGE
   0
   begin
      dup want BLEN>N < 0= if drop want TCP4-READ--RESULT:data exit then
      dup conn bytes want EXACT-STEP
      MATCH read-result
         data OF BLEN>N + ENDOF
         closed OF drop LENGTH TCP4-READ--RESULT:closed exit ENDOF
         failed OF swap drop TCP4-READ--RESULT:failed exit ENDOF
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
\ a caller writing a span in slices does both between them. An empty span is
\ E-OPERAND.
: SEND-SLICE ( connection ptr u8 NUM:byte-len -- slice-result )
   {: conn:connection bytes size:NUM:byte-len :}
   size BLEN>N 1 < if E-OPERAND throw then
   conn CONNECTION-FD {: fd:n :}
   size BLEN>N MAX-TRANSFER min {: span:n :}
   mono-ns SEND-SLICE-MS NS-PER-MS * + {: deadline:n :}
   fd bytes span deadline TRY-SEND
   MATCH slice-result
      moved OF TCP4-SLICE--RESULT:moved ENDOF
      empty OF fd bytes span deadline AFTER-ROOM ENDOF
      failed OF TCP4-SLICE--RESULT:failed ENDOF
   ;MATCH ;


\ Blocks until every byte is accepted by the OS, which is not delivery. A failed
\ write may have transmitted part of the span; the stream is then unusable.
\ Every send slice that leaves bytes to send ends in a TASK:PAUSE, where a halt
\ ends the task, whether it moved some or none: a peer that takes a few bytes in
\ every slice holds the write for as long as it reads, but not past a halt.
: WRITE ( connection ptr u8 NUM:byte-len -- status )
   {: conn:connection bytes size:NUM:byte-len :}
   size BLEN>N 0 MAX-TRANSFER WITHIN-RANGE
   size BLEN>N 0= if TCP4-STATUS:ok exit then
   0
   begin {: sent:n :}
      conn bytes sent + size BLEN>N sent - LENGTH SEND-SLICE
      MATCH slice-result
         moved OF BLEN>N sent + ENDOF
         empty OF sent ENDOF
         failed OF TCP4-STATUS:failed exit ENDOF
      ;MATCH
      dup size BLEN>N >= if drop TCP4-STATUS:ok exit then
      TASK:PAUSE
   again ;


\ Turns Nagle's algorithm off (true) or back on (false), so a small send leaves
\ at once instead of waiting for the previous segment's acknowledgement.
: NODELAY! ( connection bool -- status ) {: conn:connection on:bool :}
   on if 1 else 0 then OPTION LE:U32!
   conn CONNECTION-FD IPPROTO-TCP TCP-NODELAY OPTION 4 OPTION-CALL RC>STATUS ;


\ The bytes the peer has sent that the connection holds and this end has not
\ read yet: FIONREAD.
: UNREAD ( connection -- queue-result )
   CONNECTION-FD {: fd:n :}
   fd FIONREAD OPTION QUEUE-CALL QUEUED ;


\ The bytes this end has written that the connection still holds, unsent or
\ sent and not yet acknowledged: Darwin's SO_NWRITE, Linux's SIOCOUTQ.
: UNSENT ( connection -- queue-result )
   CONNECTION-FD {: fd:n :}
   HB-TARGET-MACOS? if
      4 ADDRLEN LE:U32!
      fd DARWIN-SOL-SOCKET SO-NWRITE OPTION ADDRLEN OPTION-GET-CALL QUEUED exit
   then
   fd SIOCOUTQ OPTION QUEUE-CALL QUEUED ;


\ Answers whether a READ would return at once. `ready` includes the end of
\ stream and a failed connection, whose answers the following READ reports.
: READABLE? ( connection -- ready-result )
   CONNECTION-FD 0 >MS WAIT-READY ;


\ Parks the task on the AIO loop until the stream would answer a READ or the
\ deadline passes, which is `idle`. A timeout below zero is refused.
: READABLE-WITHIN? ( connection ms -- ready-result )
   {: conn:connection timeout:ms :}
   conn CONNECTION-FD timeout WAIT-READY ;


: PENDING? ( listener -- ready-result )
   LISTENER-FD 0 >MS WAIT-READY ;


\ The same wait for an arriving connection: `ready` means ACCEPT answers at
\ once, `idle` that the deadline passed with the queue still empty.
: PENDING-WITHIN? ( listener ms -- ready-result )
   {: lis:listener timeout:ms :}
   lis LISTENER-FD timeout WAIT-READY ;


\ Half-closes the stream in one or both directions; the descriptor stays open
\ until CLOSE. Shutting down sending sends the peer the end of stream.
: SHUTDOWN ( connection direction -- status ) {: conn:connection how:direction :}
   conn CONNECTION-FD how SHUT-CODE SHUTDOWN-RAW RC>STATUS ;


\ On Linux, close consumes the descriptor even if interrupted. Never retry it.
: CLOSE ( connection -- status )
   CONNECTION-FD CLOSE-RAW RC>STATUS ;


: CLOSE-LISTENER ( listener -- status )
   LISTENER-FD CLOSE-RAW RC>STATUS ;

\ ---- the descriptor seam -----------------------------------------------------
\ The three words an asynchronous caller needs and this module's own blocking
\ surface does not: the descriptor under a listener or a connection, so it can
\ be handed to a readiness or completion loop (docs/aio.md), and a socket that
\ is not connected yet, so such a loop can do the connecting. Nothing here
\ changes ownership: a descriptor read out still belongs to the handle it came
\ from, and CLOSE / CLOSE-LISTENER is still what ends it.
: LISTENER-FD ( listener -- fd )
   LISTENER-FD >FD ;

: CONNECTION-FD ( connection -- fd )
   CONNECTION-FD >FD ;

\ A fresh AF_INET stream socket, bound to nothing and connected to nothing, with
\ the flags this module's own BIND and CONNECT ask for, and non-blocking like
\ every connection this module makes. The caller owns it and closes it with
\ CLOSE.
: SOCKET ( -- socket-result )
   SOCKET-RAW dup 0 < if drop LAST-ERROR TCP4-SOCKET--RESULT:failed exit then
   NONBLOCK >CONNECTION TCP4-SOCKET--RESULT:opened ;

;package
