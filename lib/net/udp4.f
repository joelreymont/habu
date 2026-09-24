\ Linux and Darwin AArch64 IPv4 datagrams through exact, bounded libc bindings.
\
\ STORAGE CLASS. TASK-LOCAL. The endpoint storage is one $18 TASK:+USER row, so
\ each task holds its own, and the datagram spans the transfer words take are
\ caller-owned. See docs/threads.md.
\
\ The socket is nonblocking and RECEIVE waits on the AIO loop (docs/aio.md): one
\ POLL for the time left on its deadline, awaited without parking a thread. A
\ program calls AIO:START before its first RECEIVE that waits; a wait with
\ no loop is E-AIO-STATE.
require lib/errors.f
require lib/memory.f
require lib/ffi-abi.f
require lib/type/deftype.f
require lib/num-types.f
require lib/task.f
require lib/aio.f
require lib/le.f                  \ the socklen cell the endpoint carries

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
: SOCKET-FLAGS ( -- n ) HB-TARGET-MACOS? if 2 else $80802 then ;       \ SOCK_DGRAM | SOCK_NONBLOCK | SOCK_CLOEXEC.
$20 constant MSG-TRUNC
1000000 constant NS-PER-MS

\ Endpoint storage is per task, matching FFI's argument/extent tables: a $10
\ sockaddr_in and a $04 socklen_t, rounded up to the cell so the row after this
\ one still starts aligned.
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and $18 TASK:+USER IO-STORAGE drop
: SOCKADDR ( -- ptr u8 ) IO-STORAGE BYTE-VIEW ;
: ADDRLEN ( -- ptr u8 ) IO-STORAGE BYTE-VIEW $10 + ;

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


: C-INT ( n -- n )
   MAX-ADDRESS and dup $80000000 and 0 <> if $100000000 - then ;


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

: LAST-ERROR ( -- errno )
   FFI:ERRNO >ERRNO ;


: SOCKET-RAW ( -- n )
   2 SOCKET-FLAGS 0 SOCKET-CALL C-INT {: fd:n :}
   fd 0 < HB-TARGET-MACOS? 0= or if fd exit then
   \ Fresh sockets have no status flags to preserve. Darwin uses O_NONBLOCK=4.
   fd 2 1 fcntl 0 <> if fd CLOSE-CALL drop E-RESULT throw then
   fd 4 4 fcntl 0 <> if fd CLOSE-CALL drop E-RESULT throw then
   fd ;


: BIND-RAW ( socket -- n ) {: socket:socket :}
   socket SOCKET>N SOCKADDR SOCKADDR-BYTES BIND-CALL C-INT ;


: CLOSE-RAW ( socket -- n ) {: socket:socket :}
   socket SOCKET>N CLOSE-CALL C-INT ;


: LOCAL-RAW ( socket -- n ) {: socket:socket :}
   socket SOCKET>N SOCKADDR ADDRLEN LOCAL-CALL C-INT ;


: SEND-RAW ( socket ptr u8 NUM:byte-len -- n )
   {: socket:socket bytes size:NUM:byte-len :}
   socket SOCKET>N bytes size BLEN>N 0
   SOCKADDR SOCKADDR-BYTES SEND-CALL ;


: DARWIN-RECEIVE ( socket ptr u8 NUM:byte-len -- n errno )
   {: socket:socket bytes capacity:NUM:byte-len :}
   \ BSD recvfrom does not report the original size after truncation. Read one
   \ complete IPv4 datagram, then copy the prefix promised to the caller.
   MAX-PAYLOAD MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: buf:ptr extent:NUM:alloc-byte-len :}
   socket SOCKET>N buf MAX-PAYLOAD 0 SOCKADDR ADDRLEN RECEIVE-CALL {: got:n :}
   LAST-ERROR {: error:errno :}
   got 0 >= if buf bytes got capacity BLEN>N min BYTE-COPY then
   buf extent MEM:RELEASE-BYTES
   got error ;

: RECEIVE-RAW ( socket ptr u8 NUM:byte-len -- n errno )
   {: socket:socket bytes capacity:NUM:byte-len :}
   HB-TARGET-MACOS? if socket bytes capacity DARWIN-RECEIVE exit then
   socket SOCKET>N bytes capacity BLEN>N MSG-TRUNC
   SOCKADDR ADDRLEN RECEIVE-CALL LAST-ERROR ;


\ The wait between two tries: one poll on the AIO loop for the time this
\ RECEIVE has left. `cancelled` cannot arrive - nothing here cancels, the ticket
\ never leaves this word, and the cleanup AIO registers on a submitting task runs
\ only after that task has ended - so it is a broken foreign result.
: WAIT-READABLE ( socket ms -- AIO:outcome ) {: socket:socket left:ms :}
   socket SOCKET>N >FD AIO:READABLE left AIO:POLL AIO:AWAIT ;


: REMAINING ( ns -- ms )
   NS>N mono-ns - dup 0 <= if drop 0 >MS exit then
   NS-PER-MS 1 - + NS-PER-MS / >MS ;


: RETRY? ( errno -- bool )
   ERRNO>N dup 4 = swap HB-TARGET-MACOS? if 35 else 11 then = or ;


: RECEIVED ( n NUM:byte-len -- receive-result ) {: actual:n capacity:NUM:byte-len :}
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


: PAYLOAD-BYTES ( n -- NUM:byte-len )
   dup 0 MAX-PAYLOAD WITHIN-RANGE LENGTH ;


\ BIND returns an owned nonblocking socket; the caller closes it exactly once.
\ Address is a numeric IPv4 address (e.g. $7F000001); port zero asks the OS.
: BIND ( address port -- open-result )
   ENDPOINT! SOCKET-RAW dup 0 < if drop LAST-ERROR UDP4-OPEN--RESULT:failed exit then
   >SOCKET {: socket:socket :}
   socket BIND-RAW 0 < if
      LAST-ERROR socket CLOSE-RAW drop UDP4-OPEN--RESULT:failed
   else socket UDP4-OPEN--RESULT:opened then ;


: LOCAL ( socket -- endpoint-result ) {: socket:socket :}
   socket CHECK-SOCKET ENDPOINT-OUTPUT
   socket LOCAL-RAW 0 < if LAST-ERROR UDP4-ENDPOINT--RESULT:failed
   else ENDPOINT@ UDP4-ENDPOINT--RESULT:endpoint then ;


\ The input is borrowed for this call. Success means the OS accepted the whole
\ datagram, not that it arrived. Interrupted/would-block errors reach the caller.
: SEND ( socket address port ptr u8 NUM:byte-len -- status )
   {: socket:socket address:address port:port bytes size:NUM:byte-len :}
   socket CHECK-SOCKET size BLEN>N 0 MAX-PAYLOAD WITHIN-RANGE
   address port ENDPOINT!
   socket bytes size SEND-RAW dup 0 < if drop LAST-ERROR UDP4-STATUS:failed exit then
   size BLEN>N <> if E-RESULT throw then UDP4-STATUS:ok ;


\ A packet may contain zero bytes. Capacity is 1..65507; the caller owns that
\ writable span. A truncated result carries the ORIGINAL datagram byte length,
\ while only capacity bytes were copied. Timeout zero makes one immediate try and
\ never reaches the loop; any other timeout is an absolute deadline the waits on
\ the loop share, so AIO:START must have run before the first of them.
: RECEIVE ( socket ptr u8 NUM:byte-len ms -- receive-result )
   {: socket:socket bytes capacity:NUM:byte-len timeout:ms :}
   socket CHECK-SOCKET capacity BLEN>N 1 MAX-PAYLOAD WITHIN-RANGE
   timeout MS>N 0 $7FFFFFFF WITHIN-RANGE
   mono-ns timeout MS>N NS-PER-MS * + >NS {: deadline:ns :}
   begin
      ENDPOINT-OUTPUT socket bytes capacity RECEIVE-RAW {: got:n error:errno :}
      got 0 >= if got capacity RECEIVED exit then
      error dup RETRY? 0= if UDP4-RECEIVE--RESULT:failed exit then drop
      deadline REMAINING dup MS>N 0= if drop UDP4-RECEIVE--RESULT:timeout exit then
      socket swap WAIT-READABLE
      MATCH AIO:outcome
         ready OF drop ENDOF
         timed-out OF UDP4-RECEIVE--RESULT:timeout exit ENDOF
         cancelled OF E-RESULT throw ENDOF
         refused OF >ERRNO UDP4-RECEIVE--RESULT:failed exit ENDOF
      ;MATCH
   again ;


\ On Linux, close consumes the descriptor even if interrupted. Never retry it.
: CLOSE ( socket -- status )
   dup CHECK-SOCKET CLOSE-RAW 0 < if LAST-ERROR UDP4-STATUS:failed else UDP4-STATUS:ok then ;


;package
