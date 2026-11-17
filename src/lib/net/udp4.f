\ udp4.f - Linux and Darwin IPv4 datagrams through exact, bounded libc
\ bindings; no protocol, sequencing or retry policy. Another OS is E-PLATFORM
\ before a socket opens. Calls must not nest within one task; close sockets
\ before image capture: they are process resources.
\ RECEIVE's timeout: zero makes one try and never reaches the AIO loop; a
\ negative one waits with no deadline; any other is the milliseconds the call
\ may wait in all. AIO:START must have run before the first wait.
\ STORAGE CLASS. TASK-LOCAL: the endpoint storage is one TASK:+USERS row
\ (IO-STORAGE); the datagram spans are caller-owned.
require src/lib/error.f
require src/lib/ffi-abi.f
require src/lib/type/deftype.f
require src/lib/num-types.f
require src/lib/task.f
require src/lib/le.f                  \ the socklen cell the endpoint carries
require src/lib/net/datagram.f

package UDP4
public

DEFTYPE ADDRESS
DEFTYPE PORT
DEFTYPE SOCKET
DEFTYPE ERRNO

SUMTYPE open-result
   VARIANT opened socket ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

SUMTYPE endpoint-result
   VARIANT endpoint address port ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

SUMTYPE status
   VARIANT ok ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

SUMTYPE receive-result
   VARIANT packet NUM:byte-len address port ;VARIANT
   VARIANT truncated NUM:byte-len address port ;VARIANT
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

\ Endpoint storage is per task: a $10 sockaddr_in and a $04 socklen_t.
$14 TASK:+USERS IO-STORAGE u8
: SOCKADDR ( -- ^u8 ) 0 IO-STORAGE ;
: ADDRLEN ( -- ^u8 ) $10 IO-STORAGE ;

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


: CHECK-SOCKET ( socket -- )
   SOCKET>N 0 $7FFFFFFF WITHIN-RANGE ;


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
   else HB-TARGET-MACOS? if
      SOCKADDR-BYTES SOCKADDR c! 2 SOCKADDR 1+ c!
   else E-PLATFORM throw then then
   port PORT>N SOCKADDR $02 + BE16!
   address ADDRESS>N SOCKADDR $04 + BE32! ;


: ENDPOINT-OUTPUT ( -- )
   CLEAR-ENDPOINT SOCKADDR-BYTES ADDRLEN LE:U32! ;


: ENDPOINT@ ( -- address port )
   ADDRLEN LE:U32@ SOCKADDR-BYTES <> if E-RESULT throw then
   HB-TARGET-LINUX-KERNEL? if
      SOCKADDR c@ 2 <> SOCKADDR 1+ c@ 0 <> or
   else HB-TARGET-MACOS? if
      SOCKADDR c@ SOCKADDR-BYTES <> SOCKADDR 1+ c@ 2 <> or
   else E-PLATFORM throw then then
   if E-RESULT throw then
   SOCKADDR $04 + BE32@ >ADDRESS SOCKADDR $02 + BE16@ >PORT ;


\ RTLD_DEFAULT borrows process symbols: the executable already needs libc, so
\ no library reference is retained.
PROCESS-SYMBOLS

FUNCTION: LOCAL-CALL getsockname ( n ^u8 ^u8 -- i32 ) ;FUNCTION

: LAST-ERROR ( -- errno )
   FFI:ERRNO >ERRNO ;


: LOCAL-RAW ( socket -- n ) {: socket:socket :}
   socket SOCKET>N SOCKADDR ADDRLEN LOCAL-CALL ;


: >OPEN-RESULT ( DATAGRAM:open-result -- open-result )
   MATCH DATAGRAM:open-result
      opened OF >SOCKET UDP4-OPEN--RESULT:OPENED ENDOF
      failed OF DATAGRAM:ERRNO>N >ERRNO UDP4-OPEN--RESULT:FAILED ENDOF
   ;MATCH ;


: >STATUS ( DATAGRAM:status -- status )
   MATCH DATAGRAM:status
      ok OF UDP4-STATUS:OK ENDOF
      failed OF DATAGRAM:ERRNO>N >ERRNO UDP4-STATUS:FAILED ENDOF
   ;MATCH ;


: >RECEIVE-RESULT ( DATAGRAM:receive-result -- receive-result )
   MATCH DATAGRAM:receive-result
      packet OF ENDPOINT@ UDP4-RECEIVE--RESULT:PACKET ENDOF
      truncated OF ENDPOINT@ UDP4-RECEIVE--RESULT:TRUNCATED ENDOF
      timeout OF UDP4-RECEIVE--RESULT:TIMEOUT ENDOF
      failed OF DATAGRAM:ERRNO>N >ERRNO UDP4-RECEIVE--RESULT:FAILED ENDOF
   ;MATCH ;


: OCTET-LENGTH ( ^u8 n -- n ) {: text size:n :}
   0 begin dup size < while
      dup text + c@ $2E = if exit then 1 +
   repeat ;


: OCTET-VALUE ( ^u8 n -- n ) {: text size:n :}
   size 1 3 WITHIN-RANGE
   size 1 > text c@ $30 = and if E-OPERAND throw then
   0 size 0 do
      text i + c@ $30 - dup 0 9 WITHIN-RANGE swap 10 * +
   loop dup 0 $FF WITHIN-RANGE ;


: OCTET ( ^u8 n -- ^u8 n n ) {: text size:n :}
   text size OCTET-LENGTH {: length:n :}
   text length + size length - text length OCTET-VALUE ;


: FOLLOWING-OCTET ( n ^u8 n -- n ^u8 n ) {: address:n text size:n :}
   size 1 < if E-OPERAND throw then
   text c@ $2E <> if E-OPERAND throw then
   text $01 + size 1 - OCTET {: next remaining:n octet:n :}
   address 8 lshift octet or next remaining ;


public


: ADDRESS ( n -- address )
   dup 0 MAX-ADDRESS WITHIN-RANGE >ADDRESS ;


\ Strict dotted decimal: four octets, no signs, padding, or leading zeros.
: ADDRESS$ ( ^u8 n -- address ) {: text size:n :}
   size 7 15 WITHIN-RANGE
   text size OCTET {: next remaining:n first:n :}
   first next remaining FOLLOWING-OCTET FOLLOWING-OCTET FOLLOWING-OCTET
   {: value:n tail rest:n :}
   rest 0 <> if E-OPERAND throw then value >ADDRESS ;


: PORT ( n -- port )
   dup 0 MAX-PORT WITHIN-RANGE >PORT ;


: PAYLOAD-BYTES ( n -- NUM:byte-len )
   dup 0 MAX-PAYLOAD WITHIN-RANGE LENGTH ;


\ BIND returns an owned socket; the caller closes it exactly once.
\ Address is a numeric IPv4 address (e.g. $7F000001); port zero asks the OS.
: BIND ( address port -- open-result )
   ENDPOINT! 2 SOCKADDR SOCKADDR-BYTES DATAGRAM:BIND >OPEN-RESULT ;


: LOCAL ( socket -- endpoint-result ) {: socket:socket :}
   socket CHECK-SOCKET ENDPOINT-OUTPUT
   socket LOCAL-RAW 0 < if LAST-ERROR UDP4-ENDPOINT--RESULT:FAILED
   else ENDPOINT@ UDP4-ENDPOINT--RESULT:ENDPOINT then ;


\ The input is borrowed for this call. SEND waits while the socket's send
\ buffer is full; an interrupted send answers `failed`.
: SEND ( socket address port ^u8 NUM:byte-len -- status )
   {: socket:socket address:address port:port bytes size:NUM:byte-len :}
   socket CHECK-SOCKET size BLEN>N 0 MAX-PAYLOAD WITHIN-RANGE
   address port ENDPOINT!
   socket SOCKET>N SOCKADDR SOCKADDR-BYTES bytes size DATAGRAM:SEND >STATUS ;


\ A packet may be empty; capacity is 1..65507. `truncated` carries the
\ datagram's own length, with only capacity bytes copied.
: RECEIVE ( socket ^u8 NUM:byte-len ms -- receive-result )
   {: socket:socket bytes capacity:NUM:byte-len timeout:ms :}
   socket CHECK-SOCKET capacity BLEN>N 1 MAX-PAYLOAD WITHIN-RANGE
   ENDPOINT-OUTPUT
   socket SOCKET>N bytes capacity timeout SOCKADDR ADDRLEN DATAGRAM:RECEIVE
   >RECEIVE-RESULT ;


\ The caller closes each socket once and never reuses it: on Linux, close
\ consumes the descriptor even if interrupted. Never retry it.
: CLOSE ( socket -- status )
   dup CHECK-SOCKET SOCKET>N DATAGRAM:CLOSE >STATUS ;


;package
