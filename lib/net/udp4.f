\ Linux AArch64 IPv4 datagrams through exact, bounded libc bindings.
require lib/errors.f
require lib/ffi-abi.f
require lib/type/deftype.f
require lib/cad-num-types.f
require lib/task.f

package UDP4
public

DEFTYPE ADDRESS
DEFTYPE PORT
DEFTYPE SOCKET
DEFTYPE ERRNO

SUMTYPE open-result 0
   VARIANT opened socket ;VARIANT
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

SUMTYPE receive-result 0
   VARIANT packet CAD-NUM:byte-len address port ;VARIANT
   VARIANT truncated CAD-NUM:byte-len address port ;VARIANT
   VARIANT timeout ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

E-UDP4-OPERAND constant E-OPERAND
E-UDP4-PLATFORM constant E-PLATFORM
E-UDP4-RESULT constant E-RESULT

private

$10 constant SOCKADDR-BYTES
$FFFF constant MAX-PORT
$FFFFFFFF constant MAX-ADDRESS
$FFE3 constant MAX-PAYLOAD
$80802 constant SOCKET-FLAGS       \ SOCK_DGRAM | SOCK_NONBLOCK | SOCK_CLOEXEC.
$20 constant MSG-TRUNC
1000000 constant NS-PER-MS

\ Endpoint/poll storage is per task, matching FFI's argument/extent tables.
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and $20 TASK:+USER IO-STORAGE drop
: SOCKADDR ( -- ptr u8 ) IO-STORAGE BYTE-VIEW ;
: ADDRLEN ( -- ptr u8 ) IO-STORAGE BYTE-VIEW $10 + ;
: POLLFD ( -- ptr u8 ) IO-STORAGE BYTE-VIEW $18 + ;

CAST: BLEN>N ( CAD-NUM:byte-len -- n )


: WITHIN-RANGE ( n n n -- ) {: value:n minimum:n maximum:n :}
   value minimum < value maximum > or if E-OPERAND throw then ;


: LENGTH ( n -- CAD-NUM:byte-len )
   CAD-NUM:BYTE-LEN MATCH CAD-NUM:numeric-result
      ok OF ENDOF
      negative OF E-OPERAND throw ENDOF
      zero OF E-OPERAND throw ENDOF
      overflow OF E-OPERAND throw ENDOF
      underflow OF E-OPERAND throw ENDOF
      bad-alignment OF E-OPERAND throw ENDOF
      misaligned OF E-OPERAND throw ENDOF
   ;MATCH ;


: CHECK-SOCKET ( socket -- )
   SOCKET>N 0 $7FFFFFFF WITHIN-RANGE ;


: C-INT ( n -- n )
   MAX-ADDRESS and dup $80000000 and 0 <> if $100000000 - then ;


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


\ The exact Linux AArch64 libc schemas. RTLD_DEFAULT borrows process symbols;
\ the native executable already needs libc.so.6, so no library reference is
\ acquired or retained here. Each declaration states the C function's own effect
\ and the extent of every buffer the callee writes, which is what the bounded
\ call guards; package FFI resolves each symbol on its first call and clears the
\ cache for image capture. Coverage: test/net/udp4.py exercises every binding
\ with an independent Python UDP peer.
PROCESS-SYMBOLS

FUNCTION: SOCKET-CALL socket ( n n n -- n ) ;FUNCTION
FUNCTION: BIND-CALL bind ( n ptr u8 n -- n ) ;FUNCTION
FUNCTION: CLOSE-CALL close ( n -- n ) ;FUNCTION
FUNCTION: SEND-CALL sendto ( n ptr u8 n n ptr u8 n -- n ) ;FUNCTION

FUNCTION: LOCAL-CALL getsockname ( n ptr u8 ptr u8 -- n )
   1 $10 WRITES-BYTES                     \ sockaddr_in
   2 $04 WRITES-BYTES                     \ socklen_t
;FUNCTION

FUNCTION: RECEIVE-CALL recvfrom ( n ptr u8 n n ptr u8 ptr u8 -- n )
   1 2 WRITES-ARG                         \ the caller's payload span
   4 $10 WRITES-BYTES                     \ sockaddr_in
   5 $04 WRITES-BYTES                     \ socklen_t
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


: SOCKET-RAW ( -- n )
   2 SOCKET-FLAGS 0 SOCKET-CALL C-INT ;


: BIND-RAW ( socket -- n ) {: socket:socket :}
   socket SOCKET>N SOCKADDR SOCKADDR-BYTES BIND-CALL C-INT ;


: CLOSE-RAW ( socket -- n ) {: socket:socket :}
   socket SOCKET>N CLOSE-CALL C-INT ;


: LOCAL-RAW ( socket -- n ) {: socket:socket :}
   socket SOCKET>N SOCKADDR ADDRLEN LOCAL-CALL C-INT ;


: SEND-RAW ( socket ptr u8 CAD-NUM:byte-len -- n )
   {: socket:socket bytes size:CAD-NUM:byte-len :}
   socket SOCKET>N bytes size BLEN>N 0
   SOCKADDR SOCKADDR-BYTES SEND-CALL ;


: RECEIVE-RAW ( socket ptr u8 CAD-NUM:byte-len -- n )
   {: socket:socket bytes capacity:CAD-NUM:byte-len :}
   socket SOCKET>N bytes capacity BLEN>N MSG-TRUNC
   SOCKADDR ADDRLEN RECEIVE-CALL ;


: POLL-RAW ( ms -- n ) {: timeout:ms :}
   POLLFD 1 timeout MS>N POLL-CALL C-INT ;


: POLL! ( socket -- )
   SOCKET>N POLLFD LE32! 1 POLLFD $04 + LE32! ;


: REMAINING ( ns -- ms )
   NS>N mono-ns - dup 0 <= if drop 0 >MS exit then
   NS-PER-MS 1 - + NS-PER-MS / >MS ;


: RETRY? ( errno -- bool )
   ERRNO>N dup 4 = swap 11 = or ;


: RECEIVED ( n CAD-NUM:byte-len -- receive-result ) {: actual:n capacity:CAD-NUM:byte-len :}
   actual 0 MAX-PAYLOAD WITHIN-RANGE
   actual capacity BLEN>N > if
      actual LENGTH ENDPOINT@ UDP4-RECEIVE--RESULT:truncated
   else
      actual LENGTH ENDPOINT@ UDP4-RECEIVE--RESULT:packet
   then ;


: OCTET-LENGTH ( ptr u8 n -- n ) {: text size:n :}
   0 begin dup size < while
      dup text + c@ $2E = if exit then 1 +
   repeat ;


: OCTET-VALUE ( ptr u8 n -- n ) {: text size:n :}
   size 1 3 WITHIN-RANGE
   size 1 > text c@ $30 = and if E-OPERAND throw then
   0 size 0 do
      text i + c@ $30 - dup 0 9 WITHIN-RANGE swap 10 * +
   loop dup 0 $FF WITHIN-RANGE ;


: OCTET ( ptr u8 n -- ptr u8 n n ) {: text size:n :}
   text size OCTET-LENGTH {: length:n :}
   text length + size length - text length OCTET-VALUE ;


: FOLLOWING-OCTET ( n ptr u8 n -- n ptr u8 n ) {: address:n text size:n :}
   size 1 < if E-OPERAND throw then
   text c@ $2E <> if E-OPERAND throw then
   text $01 + size 1 - OCTET {: next remaining:n octet:n :}
   address 8 lshift octet or next remaining ;


public


: ADDRESS ( n -- address )
   dup 0 MAX-ADDRESS WITHIN-RANGE >ADDRESS ;


\ Strict dotted decimal: four octets, no signs, padding, or leading zeros.
: ADDRESS$ ( ptr u8 n -- address ) {: text size:n :}
   size 7 15 WITHIN-RANGE
   text size OCTET {: next remaining:n first:n :}
   first next remaining FOLLOWING-OCTET FOLLOWING-OCTET FOLLOWING-OCTET
   {: value:n tail rest:n :}
   rest 0 <> if E-OPERAND throw then value >ADDRESS ;


: PORT ( n -- port )
   dup 0 MAX-PORT WITHIN-RANGE >PORT ;


: PAYLOAD-BYTES ( n -- CAD-NUM:byte-len )
   dup 0 MAX-PAYLOAD WITHIN-RANGE LENGTH ;


\ BIND returns an owned nonblocking socket; the caller closes it exactly once.
\ Address is a numeric IPv4 address (e.g. $7F000001); port zero asks the OS.
: BIND ( address port -- open-result )
   INIT ENDPOINT! SOCKET-RAW dup 0 < if drop LAST-ERROR UDP4-OPEN--RESULT:failed exit then
   >SOCKET {: socket:socket :}
   socket BIND-RAW 0 < if
      LAST-ERROR socket CLOSE-RAW drop UDP4-OPEN--RESULT:failed
   else socket UDP4-OPEN--RESULT:opened then ;


: LOCAL ( socket -- endpoint-result ) {: socket:socket :}
   socket CHECK-SOCKET INIT ENDPOINT-OUTPUT
   socket LOCAL-RAW 0 < if LAST-ERROR UDP4-ENDPOINT--RESULT:failed
   else ENDPOINT@ UDP4-ENDPOINT--RESULT:endpoint then ;


\ The input is borrowed for this call. Success means the OS accepted the whole
\ datagram, not that it arrived. Interrupted/would-block errors reach the caller.
: SEND ( socket address port ptr u8 CAD-NUM:byte-len -- status )
   {: socket:socket address:address port:port bytes size:CAD-NUM:byte-len :}
   socket CHECK-SOCKET size BLEN>N 0 MAX-PAYLOAD WITHIN-RANGE
   INIT address port ENDPOINT!
   socket bytes size SEND-RAW dup 0 < if drop LAST-ERROR UDP4-STATUS:failed exit then
   size BLEN>N <> if E-RESULT throw then UDP4-STATUS:ok ;


\ A packet may contain zero bytes. Capacity is 1..65507; the caller owns that
\ writable span. A truncated result carries the ORIGINAL datagram byte length,
\ while only capacity bytes were copied. Timeout zero makes one immediate try.
: RECEIVE ( socket ptr u8 CAD-NUM:byte-len ms -- receive-result )
   {: socket:socket bytes capacity:CAD-NUM:byte-len timeout:ms :}
   socket CHECK-SOCKET capacity BLEN>N 1 MAX-PAYLOAD WITHIN-RANGE
   timeout MS>N 0 $7FFFFFFF WITHIN-RANGE INIT
   mono-ns timeout MS>N NS-PER-MS * + >NS {: deadline:ns :}
   socket POLL!
   begin
      ENDPOINT-OUTPUT socket bytes capacity RECEIVE-RAW dup 0 >= if
         capacity RECEIVED exit
      then drop
      LAST-ERROR dup RETRY? 0= if UDP4-RECEIVE--RESULT:failed exit then drop
      deadline REMAINING dup MS>N 0= if drop UDP4-RECEIVE--RESULT:timeout exit then
      POLL-RAW 0 < if
         LAST-ERROR dup ERRNO>N 4 <> if UDP4-RECEIVE--RESULT:failed exit then drop
      then
   again ;


\ On Linux, close consumes the descriptor even if interrupted. Never retry it.
: CLOSE ( socket -- status )
   dup CHECK-SOCKET INIT CLOSE-RAW 0 < if LAST-ERROR UDP4-STATUS:failed else UDP4-STATUS:ok then ;


;package
