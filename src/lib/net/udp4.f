\ udp4.f - Linux and Darwin IPv4 datagrams through exact, bounded libc
\ bindings; no protocol, sequencing or retry policy. Calls must not nest within
\ one task; close sockets before image capture: they are process resources.
\ STORAGE CLASS. TASK-LOCAL: the endpoint storage is two TASK:+USERS rows
\ (SOCKADDR-ROW, ADDRLEN-ROW); the payload is caller-owned.
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

E-UDP4-OPERAND constant E-OPERAND

private

$10 constant SOCKADDR-BYTES
$FFFF constant MAX-PORT
$FFFFFFFF constant MAX-ADDRESS

\ Endpoint storage is per task: a sockaddr_in, the raw bytes the OS reads and
\ fills, and its socklen_t.
SOCKADDR-BYTES TASK:+USERS SOCKADDR-ROW u8
4 TASK:+USERS ADDRLEN-ROW u8
: SOCKADDR ( -- ^u8 ) 0 SOCKADDR-ROW ;
: ADDRLEN ( -- ^u8 ) 0 ADDRLEN-ROW ;

: WITHIN-RANGE ( n n n -- ) {: value:n minimum:n maximum:n :}
   value minimum < value maximum > or if E-OPERAND throw then ;



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
   HB-TARGET-MACOS? if
      SOCKADDR-BYTES SOCKADDR c! 2 SOCKADDR 1+ c!
   else 2 SOCKADDR c! then
   port PORT>N SOCKADDR $02 + BE16!
   address ADDRESS>N SOCKADDR $04 + BE32! ;


: ENDPOINT-OUTPUT ( -- )
   CLEAR-ENDPOINT SOCKADDR-BYTES ADDRLEN LE:U32! ;


: ENDPOINT@ ( -- address port )
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


: OCTET-LENGTH ( str -- n ) {: text:str :}
   0 begin dup text STR:LEN < while
      dup text STR:DATA + c@ $2E = if exit then 1 +
   repeat ;


: OCTET-VALUE ( str -- n ) {: text:str :}
   text STR:LEN 1 3 WITHIN-RANGE
   text STR:LEN 1 > text STR:DATA c@ $30 = and if E-OPERAND throw then
   0 text STR:LEN 0 do
      text STR:DATA i + c@ $30 - dup 0 9 WITHIN-RANGE swap 10 * +
   loop dup 0 $FF WITHIN-RANGE ;


: OCTET ( str -- str n ) {: text:str :}
   text OCTET-LENGTH {: length:n :}
   text STR:DATA length + text STR:LEN length - >str
   text STR:DATA length >str OCTET-VALUE ;


: FOLLOWING-OCTET ( n str -- n str ) {: address:n text:str :}
   text STR:LEN 1 < if E-OPERAND throw then
   text STR:DATA c@ $2E <> if E-OPERAND throw then
   text STR:DATA $01 + text STR:LEN 1 - >str OCTET {: next:str octet:n :}
   address 8 lshift octet or next ;


public


: ADDRESS ( n -- address )
   dup 0 MAX-ADDRESS WITHIN-RANGE >ADDRESS ;


\ Strict dotted decimal: four octets, no signs, padding, or leading zeros.
: ADDRESS$ ( str -- address ) {: text:str :}
   text STR:LEN 7 15 WITHIN-RANGE
   text OCTET {: next:str first:n :}
   first next FOLLOWING-OCTET FOLLOWING-OCTET FOLLOWING-OCTET
   {: value:n tail:str :}
   tail STR:LEN 0 <> if E-OPERAND throw then value >ADDRESS ;


: PORT ( n -- port )
   dup 0 MAX-PORT WITHIN-RANGE >PORT ;


\ BIND returns an owned socket; the caller closes it exactly once.
\ Address is a numeric IPv4 address (e.g. $7F000001); port zero asks the OS.
: BIND ( address port -- open-result )
   ENDPOINT! 2 SOCKADDR SOCKADDR-BYTES >str DATAGRAM:BIND >OPEN-RESULT ;


: LOCAL ( socket -- endpoint-result ) {: socket:socket :}
   ENDPOINT-OUTPUT
   socket LOCAL-RAW 0 < if LAST-ERROR UDP4-ENDPOINT--RESULT:FAILED
   else ENDPOINT@ UDP4-ENDPOINT--RESULT:ENDPOINT then ;


\ The input is borrowed for this call. SEND waits while the socket's send
\ buffer is full; an interrupted send answers `failed`.
: SEND ( socket address port str -- status )
   {: socket:socket address:address port:port bytes:str :}
   address port ENDPOINT!
   socket SOCKET>N SOCKADDR SOCKADDR-BYTES >str bytes DATAGRAM:SEND >STATUS ;


\ The caller closes each socket once and never reuses it: on Linux, close
\ consumes the descriptor even if interrupted. Never retry it.
: CLOSE ( socket -- status )
   SOCKET>N DATAGRAM:CLOSE >STATUS ;


;package
