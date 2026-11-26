\ datagram.f - datagram sockets of any address family, on Linux and macOS,
\ through exact libc bindings. A socket is blocking and close-on-exec; a
\ receive try never blocks, and RECEIVE waits between tries in AIO.
\ RECEIVE's timeout: zero makes one try and never reaches AIO; a
\ negative one waits with no deadline; any other is the milliseconds the call
\ may wait in all.
\ STORAGE CLASS. TASK-LOCAL on macOS: two 4-byte TASK:+USERS rows hold
\ getsockopt's answer and its length; none on Linux. The vector and the
\ address buffers are caller-owned.
require src/lib/error.f
require src/lib/memory.f
require src/lib/ffi-abi.f
require src/lib/type/deftype.f
require src/lib/num-types.f
require src/lib/task.f
require src/lib/aio.f
require src/lib/le.f
require src/lib/fd.f
require src/lib/vector.f

-9240 constant E-DATAGRAM-FIRST
-9249 constant E-DATAGRAM-LAST

package DATAGRAM
public

DEFTYPE ERRNO

SUMTYPE open-result
   VARIANT opened n ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

SUMTYPE status
   VARIANT ok ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

SUMTYPE receive-result
   VARIANT packet NUM:byte-len ;VARIANT
   VARIANT truncated NUM:byte-len ;VARIANT
   VARIANT timeout ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

-9240 s" a socket call or a wait answered a result it cannot give"
   ERROR:CODE E-RESULT

private

1000000 constant NS-PER-MS

CAST: BLEN>N ( NUM:byte-len -- n )
\ Each caller has shown the cell nonnegative.
CAST: N>BLEN ( n -- NUM:byte-len )

\ RTLD_DEFAULT borrows process symbols: the executable already needs libc, so
\ no library reference is retained.
PROCESS-SYMBOLS

FUNCTION: SOCKET-CALL socket ( n n n -- i32 ) ;FUNCTION
FUNCTION: BIND-CALL bind ( n str -- i32 ) ;FUNCTION
FUNCTION: CLOSE-CALL close ( n -- i32 ) ;FUNCTION
FUNCTION: SEND-CALL sendto ( n str n str -- n ) ;FUNCTION
FUNCTION: RECEIVE-CALL recvfrom ( n str n ^u8 ^u8 -- n ) ;FUNCTION

: LAST-ERROR ( -- errno )
   FFI:ERRNO >ERRNO ;


HB-TARGET-LINUX? [if]
$80002 constant SOCKET-TYPE        \ SOCK_DGRAM | SOCK_CLOEXEC

\ SOCK_CLOEXEC set the flag.
: CLOSE-ON-EXEC ( n -- n )
   drop 0 ;

: PREPARE ( n -- n )
   drop 0 ;

\ MSG_TRUNC | MSG_DONTWAIT: a truncated datagram answers its own length.
\ Receives into n new elements, then keeps only the bytes written.
: TRY ( n ^VEC:vector<u8> n ^u8 ^u8 -- n errno )
   {: fd:n v:^ most:n from from-len :}
   v VEC:COUNT {: old:n :}
   fd v most VEC:EXTEND SPAN:$ $60 from from-len RECEIVE-CALL
   LAST-ERROR {: got:n error:errno :}
   v old got 0 max most min + VEC:RESIZE
   got error ;
[then]


HB-TARGET-MACOS? [if]
2 constant SOCKET-TYPE             \ SOCK_DGRAM

FUNCTION: OPTION-CALL getsockopt ( n n n ^u8 ^u8 -- i32 ) ;FUNCTION

\ getsockopt's int answer and its socklen_t.
4 TASK:+USERS VALUE-ROW u8
4 TASK:+USERS LEN-ROW u8
: OPTION-VALUE ( -- ^u8 ) 0 VALUE-ROW ;
: OPTION-LEN ( -- ^u8 ) 0 LEN-ROW ;

\ F_SETFD FD_CLOEXEC; answers 0 or -errno.
: CLOSE-ON-EXEC ( n -- n )
   2 1 fcntl ;

\ SO_RCVBUF at SOL_SOCKET: the largest datagram the socket can queue.
: PREPARE ( n -- n ) {: fd:n :}
   4 OPTION-LEN LE:U32!
   fd $FFFF $1002 OPTION-VALUE OPTION-LEN OPTION-CALL ;

\ BSD recvfrom answers only the bytes copied: read the whole datagram, then
\ append at most n of it. $80 is MSG_DONTWAIT. The room is reserved before
\ the scratch block, so the append neither allocates nor throws.
: TRY ( n ^VEC:vector<u8> n ^u8 ^u8 -- n errno )
   {: fd:n v:^ most:n from from-len :}
   v  v VEC:COUNT most +  VEC:RESERVE
   OPTION-VALUE LE:U32@ {: size:n :}
   size MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: buf:^ extent:NUM:alloc-byte-len :}
   fd buf size >str $80 from from-len RECEIVE-CALL {: got:n :}
   LAST-ERROR {: error:errno :}
   got 0 >= if buf got most min >str v VEC:APPEND then
   buf extent MEM:RELEASE-BYTES
   got error ;
[then]


: RETRY? ( errno -- bool )
   ERRNO>N dup FD:EINTR# = swap FD:EAGAIN# = or ;


\ What a RECEIVE given timeout ms and started at start ns may still wait: a
\ negative timeout has no deadline and waits as itself.
: LEFT ( ms n -- ms ) {: timeout:ms start:n :}
   timeout MS>N 0 < if timeout exit then
   timeout MS>N mono-ns start - NS-PER-MS / -
   dup 0 < if drop 0 then >MS ;


: WAIT-READABLE ( n ms -- AIO:readiness ) {: fd:n left:ms :}
   fd >FD AIO:READABLE left AIO:WAIT-READY ;


: RECEIVED ( n NUM:byte-len -- receive-result ) {: got:n most:NUM:byte-len :}
   got most BLEN>N > if
      got N>BLEN DATAGRAM-RECEIVE--RESULT:TRUNCATED
   else
      got N>BLEN DATAGRAM-RECEIVE--RESULT:PACKET
   then ;


: BOUND ( str n -- open-result ) {: sockaddr:str fd:n :}
   fd sockaddr BIND-CALL 0 < if
      LAST-ERROR fd CLOSE-CALL drop DATAGRAM-OPEN--RESULT:FAILED
   else fd DATAGRAM-OPEN--RESULT:OPENED then ;


public


\ An unbound socket of that address family; the caller closes it exactly once.
: OPEN ( n -- open-result ) {: family:n :}
   family SOCKET-TYPE 0 SOCKET-CALL {: fd:n :}
   fd 0 < if LAST-ERROR DATAGRAM-OPEN--RESULT:FAILED exit then
   fd CLOSE-ON-EXEC {: rc:n :}
   rc 0 < if fd CLOSE-CALL drop rc negate >ERRNO DATAGRAM-OPEN--RESULT:FAILED exit then
   fd DATAGRAM-OPEN--RESULT:OPENED ;


\ A socket of that family bound to the sockaddr of that length.
: BIND ( n str -- open-result ) {: family:n sockaddr:str :}
   family OPEN MATCH DATAGRAM:open-result
      opened OF sockaddr swap BOUND ENDOF
      failed OF DATAGRAM-OPEN--RESULT:FAILED ENDOF
   ;MATCH ;


\ Sends the payload to the sockaddr of that length. Success means the OS
\ accepted the whole datagram, not that it arrived.
: SEND ( n str str -- status )
   {: fd:n sockaddr:str bytes:str :}
   fd bytes 0 sockaddr SEND-CALL 0 < if
      LAST-ERROR DATAGRAM-STATUS:FAILED
   else DATAGRAM-STATUS:OK then ;


\ Appends at most n bytes of one datagram; `truncated` carries the
\ datagram's own length. On timeout, failure or a throw COUNT is as it was.
\ The cell from-len enters as from's size; NULL-PTR for from and from-len
\ wants no sender.
: RECEIVE ( n ^VEC:vector<u8> NUM:byte-len ms ^u8 ^u8 -- receive-result )
   {: fd:n v:^ most:NUM:byte-len timeout:ms from from-len :}
   fd PREPARE 0 < if LAST-ERROR DATAGRAM-RECEIVE--RESULT:FAILED exit then
   mono-ns {: start:n :}
   begin
      fd v most BLEN>N from from-len TRY {: got:n error:errno :}
      got 0 >= if got most RECEIVED exit then
      error dup RETRY? 0= if DATAGRAM-RECEIVE--RESULT:FAILED exit then drop
      timeout start LEFT dup MS>N 0= if drop DATAGRAM-RECEIVE--RESULT:TIMEOUT exit then
      fd swap WAIT-READABLE
      MATCH AIO:readiness
         ready OF drop ENDOF
         timed-out OF DATAGRAM-RECEIVE--RESULT:TIMEOUT exit ENDOF
         rejected OF >ERRNO DATAGRAM-RECEIVE--RESULT:FAILED exit ENDOF
      ;MATCH
   again ;


\ On Linux, close consumes the descriptor even if interrupted: never retry it.
: CLOSE ( n -- status )
   CLOSE-CALL 0 < if LAST-ERROR DATAGRAM-STATUS:FAILED else DATAGRAM-STATUS:OK then ;


;package
