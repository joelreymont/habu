\ Linux AArch64 IPv4 stream sockets through exact, bounded libc bindings.
\
\ STORAGE CLASS. TASK-LOCAL. The endpoint and poll storage is one $20
\ TASK:+USER row, so each task holds its own sockaddr, socklen and pollfd and
\ any number of tasks may bind, accept, read and write at once. The payload
\ spans READ, READ-EXACT and WRITE take are caller-owned. See docs/threads.md.
require lib/errors.f
require lib/ffi-abi.f
require lib/type/deftype.f
require lib/num-types.f
require lib/task.f

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

E-TCP4-OPERAND constant E-OPERAND
E-TCP4-PLATFORM constant E-PLATFORM
E-TCP4-RESULT constant E-RESULT

private

$10 constant SOCKADDR-BYTES
$FFFF constant MAX-PORT
$FFFFFFFF constant MAX-ADDRESS
$FFFFFFFF constant U32-MASK
$7FFFF000 constant MAX-TRANSFER    \ Linux transfers at most this many bytes per call.
$1000 constant MAX-BACKLOG         \ Linux SOMAXCONN.
$80001 constant SOCKET-FLAGS       \ SOCK_STREAM | SOCK_CLOEXEC.
$80000 constant ACCEPT-FLAGS       \ SOCK_CLOEXEC on the accepted connection.
$4000 constant MSG-NOSIGNAL        \ A write to a closed peer fails; it never signals.
1 constant POLL-READ               \ POLLIN.
$39 constant POLL-DONE             \ POLLIN|POLLERR|POLLHUP|POLLNVAL: a read will not block.
$7FFFFFFF constant MAX-TIMEOUT     \ poll's timeout is a C int, in milliseconds.
1000000 constant NS-PER-MS
4 constant EINTR
0 constant SHUT-RD
1 constant SHUT-WR
2 constant SHUT-RDWR

\ Endpoint/poll storage is per task, matching FFI's argument/extent tables.
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and $20 TASK:+USER IO-STORAGE drop
: SOCKADDR ( -- ptr u8 ) IO-STORAGE BYTE-VIEW ;
: ADDRLEN ( -- ptr u8 ) IO-STORAGE BYTE-VIEW $10 + ;
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


: C-INT ( n -- n )
   U32-MASK and dup $80000000 and 0 <> if $100000000 - then ;


: LE16@ ( ptr u8 -- n ) {: source :}
   source c@ source $01 + c@ 8 lshift or ;


: LE32! ( n ptr u8 -- ) {: value:n target :}
   4 0 do value i 8 * rshift $FF and target i + c! loop ;


: LE32@ ( ptr u8 -- n ) {: source :}
   source c@ source $01 + c@ 8 lshift or
   source $02 + c@ 16 lshift or source $03 + c@ 24 lshift or ;


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
   2 SOCKADDR c! port PORT>N SOCKADDR $02 + BE16!
   address ADDRESS>N SOCKADDR $04 + BE32! ;


: ENDPOINT-OUTPUT ( -- )
   CLEAR-ENDPOINT SOCKADDR-BYTES ADDRLEN LE32! ;


: ENDPOINT@ ( -- address port )
   ADDRLEN LE32@ SOCKADDR-BYTES <> if E-RESULT throw then
   SOCKADDR c@ 2 <> SOCKADDR $01 + c@ 0 <> or if E-RESULT throw then
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

FUNCTION: SOCKET-CALL socket ( n n n -- n ) ;FUNCTION
FUNCTION: BIND-CALL bind ( n ptr u8 n -- n ) ;FUNCTION
FUNCTION: LISTEN-CALL listen ( n n -- n ) ;FUNCTION
FUNCTION: CONNECT-CALL connect ( n ptr u8 n -- n ) ;FUNCTION
FUNCTION: SEND-CALL send ( n ptr u8 n n -- n ) ;FUNCTION
FUNCTION: SHUTDOWN-CALL shutdown ( n n -- n ) ;FUNCTION
FUNCTION: CLOSE-CALL close ( n -- n ) ;FUNCTION

FUNCTION: ACCEPT-CALL accept4 ( n ptr u8 ptr u8 n -- n )
   1 $10 WRITES-BYTES                     \ sockaddr_in
   2 $04 WRITES-BYTES                     \ socklen_t
;FUNCTION

FUNCTION: LOCAL-CALL getsockname ( n ptr u8 ptr u8 -- n )
   1 $10 WRITES-BYTES                     \ sockaddr_in
   2 $04 WRITES-BYTES                     \ socklen_t
;FUNCTION

FUNCTION: RECEIVE-CALL recv ( n ptr u8 n n -- n )
   1 2 WRITES-ARG                         \ the caller's payload span
;FUNCTION

FUNCTION: POLL-CALL poll ( ptr u8 n n -- n )
   0 $08 WRITES-BYTES                     \ one pollfd
;FUNCTION


\ The platform gate. Symbol resolution is package FFI's now, so there is no
\ publication to synchronize and no cached address to invalidate here.
: INIT ( -- )
   HB-TARGET-LINUX? 0= if E-PLATFORM throw then ;


\ errno's pointer is libc-owned, thread-local, and its value a four-byte C int;
\ package FFI owns that binding for every consumer.
: LAST-ERROR ( -- errno )
   FFI:ERRNO >ERRNO ;


: INTERRUPTED? ( errno -- bool )
   ERRNO>N EINTR = ;


: SOCKET-RAW ( -- n )
   2 SOCKET-FLAGS 0 SOCKET-CALL C-INT ;


: BIND-RAW ( n -- n ) {: fd:n :}
   fd SOCKADDR SOCKADDR-BYTES BIND-CALL C-INT ;


: LISTEN-RAW ( n n -- n )                 \ fd backlog
   LISTEN-CALL C-INT ;


: ACCEPT-RAW ( n -- n ) {: fd:n :}
   fd SOCKADDR ADDRLEN ACCEPT-FLAGS ACCEPT-CALL C-INT ;


: CONNECT-RAW ( n -- n ) {: fd:n :}
   fd SOCKADDR SOCKADDR-BYTES CONNECT-CALL C-INT ;


: LOCAL-RAW ( n -- n ) {: fd:n :}
   fd SOCKADDR ADDRLEN LOCAL-CALL C-INT ;


: RECEIVE-RAW ( n ptr u8 n -- n ) {: fd:n bytes size:n :}
   fd bytes size 0 RECEIVE-CALL ;


: SEND-RAW ( n ptr u8 n -- n ) {: fd:n bytes size:n :}
   fd bytes size MSG-NOSIGNAL SEND-CALL ;


: SHUTDOWN-RAW ( n n -- n )               \ fd how
   SHUTDOWN-CALL C-INT ;


: POLL-RAW ( ms -- n ) {: timeout:ms :}
   POLLFD 1 timeout MS>N POLL-CALL C-INT ;


: CLOSE-RAW ( n -- n )
   CLOSE-CALL C-INT ;


: RC>STATUS ( n -- status )
   0 < if LAST-ERROR TCP4-STATUS:failed else TCP4-STATUS:ok then ;


: POLL! ( n -- )
   POLLFD LE32! POLL-READ POLLFD $04 + LE32! ;


\ POLLERR, POLLHUP and POLLNVAL all mean a read returns at once, with the end of
\ stream or the error as its answer, so they are readiness and not a poll failure.
: REVENTS>READY ( n -- ready-result )
   0= if TCP4-READY--RESULT:idle exit then
   POLLFD $06 + LE16@ POLL-DONE and 0 <> if TCP4-READY--RESULT:ready exit then
   TCP4-READY--RESULT:idle ;


\ Milliseconds left before an absolute monotonic deadline, floored at zero and
\ rounded up so a remaining fraction still waits.
: REMAINING ( ns -- ms )
   NS>N mono-ns - dup 0 <= if drop 0 >MS exit then
   NS-PER-MS 1 - + NS-PER-MS / >MS ;


\ The deadline is absolute, so an interrupt resumes the wait the signal cut
\ short instead of restarting it. A deadline already reached still polls once,
\ which is how the zero timeout asks its question without waiting.
: POLL-UNTIL ( ms -- ready-result ) {: timeout:ms :}
   mono-ns timeout MS>N NS-PER-MS * + >NS {: deadline:ns :}
   timeout
   begin
      POLL-RAW dup 0 >= if REVENTS>READY exit then
      drop LAST-ERROR dup INTERRUPTED? 0= if TCP4-READY--RESULT:failed exit then drop
      deadline REMAINING
   again ;


\ The one poll path: every readiness question a listener or a connection asks
\ reaches poll(2) through here, with the timeout as their only difference.
: WAIT-READY ( n ms -- ready-result ) {: fd:n timeout:ms :}
   timeout MS>N 0 MAX-TIMEOUT WITHIN-RANGE INIT
   fd POLL! timeout POLL-UNTIL ;


\ A transfer longer than the span handed to the OS is not a short answer this
\ module can report, so it is refused as a broken foreign result rather than
\ trusted as a length.
: RECEIVED ( n n -- read-result ) {: actual:n size:n :}
   actual size > if E-RESULT throw then
   actual LENGTH TCP4-READ--RESULT:data ;


\ One receive, retried only through an interrupt. A stream read of zero bytes is
\ the peer's orderly end of stream, never an empty message.
: READ-CHUNK ( n ptr u8 n -- read-result ) {: fd:n bytes size:n :}
   begin
      fd bytes size RECEIVE-RAW dup 0 > if size RECEIVED exit then
      dup 0 = if drop 0 LENGTH TCP4-READ--RESULT:closed exit then
      drop LAST-ERROR dup INTERRUPTED? 0= if TCP4-READ--RESULT:failed exit then drop
   again ;


: EXACT-STEP ( n connection ptr u8 NUM:byte-len -- read-result )
   {: got:n conn:connection bytes want:NUM:byte-len :}
   conn CONNECTION-FD bytes got + want BLEN>N got - READ-CHUNK ;


: SEND-STEP ( n connection ptr u8 NUM:byte-len -- n )
   {: sent:n conn:connection bytes size:NUM:byte-len :}
   conn CONNECTION-FD bytes sent + size BLEN>N sent - SEND-RAW
   dup size BLEN>N sent - > if E-RESULT throw then ;


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


: TRANSFER-BYTES ( n -- NUM:byte-len )
   dup 0 MAX-TRANSFER WITHIN-RANGE LENGTH ;


: RECEIVING ( -- direction )
   construct direction receiving ;


: SENDING ( -- direction )
   construct direction sending ;


: BOTH ( -- direction )
   construct direction both ;


\ BIND returns an owned listener; the caller closes it exactly once. Port zero
\ asks the OS for an ephemeral port, which LOCAL then reports.
: BIND ( address port -- bind-result )
   INIT ENDPOINT! SOCKET-RAW dup 0 < if drop LAST-ERROR TCP4-BIND--RESULT:failed exit then
   {: fd:n :}
   fd BIND-RAW 0 < if
      LAST-ERROR fd CLOSE-RAW drop TCP4-BIND--RESULT:failed
   else fd >LISTENER TCP4-BIND--RESULT:bound then ;


\ The backlog is the queue of connections completed but not yet accepted; Linux
\ caps it at SOMAXCONN.
: LISTEN ( listener n -- status ) {: lis:listener backlog:n :}
   backlog 1 MAX-BACKLOG WITHIN-RANGE INIT
   lis LISTENER-FD backlog LISTEN-RAW RC>STATUS ;


: LOCAL ( listener -- endpoint-result )
   LISTENER-FD INIT ENDPOINT-OUTPUT
   LOCAL-RAW 0 < if LAST-ERROR TCP4-ENDPOINT--RESULT:failed
   else ENDPOINT@ TCP4-ENDPOINT--RESULT:endpoint then ;


\ Blocks until a connection arrives. The accepted connection is owned by the
\ caller and closed separately from the listener. Use PENDING? to avoid waiting.
: ACCEPT ( listener -- accept-result )
   LISTENER-FD INIT ACCEPT-LOOP ;


\ Blocks until the handshake completes or the OS refuses it. A refused peer
\ answers failed with ECONNREFUSED; the failed socket is closed here.
: CONNECT ( address port -- connect-result )
   INIT ENDPOINT! SOCKET-RAW dup 0 < if drop LAST-ERROR TCP4-CONNECT--RESULT:failed exit then
   {: fd:n :}
   fd CONNECT-RAW 0 < if
      LAST-ERROR fd CLOSE-RAW drop TCP4-CONNECT--RESULT:failed
   else fd >CONNECTION TCP4-CONNECT--RESULT:connected then ;


\ One partial read of 1..capacity bytes, blocking until the stream answers.
\ The caller owns the writable span. `closed` carries zero bytes.
: READ ( connection ptr u8 NUM:byte-len -- read-result )
   {: conn:connection bytes capacity:NUM:byte-len :}
   capacity BLEN>N 1 MAX-TRANSFER WITHIN-RANGE INIT
   conn CONNECTION-FD bytes capacity BLEN>N READ-CHUNK ;


\ Blocks until the whole span is filled. An end of stream before that is
\ `closed` carrying the bytes already written into the span.
: READ-EXACT ( connection ptr u8 NUM:byte-len -- read-result )
   {: conn:connection bytes want:NUM:byte-len :}
   want BLEN>N 1 MAX-TRANSFER WITHIN-RANGE INIT
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


\ Blocks until every byte is accepted by the OS, which is not delivery. A failed
\ write may have transmitted part of the span; the stream is then unusable.
: WRITE ( connection ptr u8 NUM:byte-len -- status )
   {: conn:connection bytes size:NUM:byte-len :}
   size BLEN>N 0 MAX-TRANSFER WITHIN-RANGE INIT
   0
   begin
      dup size BLEN>N < 0= if drop TCP4-STATUS:ok exit then
      dup conn bytes size SEND-STEP
      dup 0 >= if +
      else
         drop LAST-ERROR dup INTERRUPTED? 0= if swap drop TCP4-STATUS:failed exit then drop
      then
   again ;


\ Answers whether a READ would return at once. `ready` includes the end of
\ stream and a failed connection, whose answers the following READ reports.
: READABLE? ( connection -- ready-result )
   CONNECTION-FD 0 >MS WAIT-READY ;


\ Parks the task in poll(2) until the stream would answer a READ or the
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
   conn CONNECTION-FD INIT how SHUT-CODE SHUTDOWN-RAW RC>STATUS ;


\ On Linux, close consumes the descriptor even if interrupted. Never retry it.
: CLOSE ( connection -- status )
   CONNECTION-FD INIT CLOSE-RAW RC>STATUS ;


: CLOSE-LISTENER ( listener -- status )
   LISTENER-FD INIT CLOSE-RAW RC>STATUS ;

;package
