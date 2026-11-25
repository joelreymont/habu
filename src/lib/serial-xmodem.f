\ serial-xmodem.f - bounded XMODEM transfers over an already-open raw serial
\ stream. Each SEND or RECEIVE takes one overall deadline that negotiation,
\ retries and draining share. Only `completed` means done: a failure may leave
\ an accepted prefix and padding is never stripped, so the application frames
\ its own length; DISPOSE leaves the serial handle open. test/serial-xmodem.py.
\ STORAGE CLASS. CALLER-OWNED; a session or a stream serves one task at a time.
require src/lib/heap.f
require src/lib/span.f
require src/lib/vector.f
require src/lib/xmodem.f
require src/lib/serial.f

package SERIAL-XMODEM
public

SUMTYPE transfer-result
   VARIANT completed NUM:byte-len ;VARIANT
   VARIANT timeout ;VARIANT
   VARIANT closed ;VARIANT
   VARIANT failed SERIAL:errno ;VARIANT
   VARIANT cancelled ;VARIANT
   VARIANT retry-limit ;VARIANT
   VARIANT capacity ;VARIANT
;SUMTYPE

\ The packet vector is the only owned allocation; the serial handle, source and
\ output vector remain borrowed.
STRUCTURE session OPAQUE DERIVE addr
   FIELD handle SERIAL:handle
   FIELD control-wait ms
   FIELD receive-wait ms
   FIELD deadline ns
   FIELD io-error SERIAL:errno
   FIELD kind XMODEM:check-kind
   FIELD block XMODEM:payload-size
   FIELD next n
   FIELD tries n
   FIELD progress n
   FIELD maximum n
   FIELD negotiated bool
   FIELD source ^u8
   FIELD source-len n
   FIELD packet ^VEC:vector<u8>
   FIELD output ^VEC:vector<u8>
   BUFFER control 3
;STRUCTURE

E-SXMODEM-OPERAND constant E-OPERAND

private
using XMODEM

1000000 constant NS-PER-MS
10 constant MAX-TRIES

: PACKET ( ^session -- ^VEC:vector<u8> ) SESSION-PACKET @ ;

CAST: BLEN>N ( NUM:byte-len -- n )


: CHECK-MS ( ms -- )
   MS>N dup 1 < swap $7FFFFFFF > or if E-OPERAND throw then ;


: CHECK-LENGTH ( NUM:byte-len -- )
   BLEN>N 0 < if E-OPERAND throw then ;


: REMAINING ( ns -- ms )
   NS>N mono-ns - dup 0 <= if drop 0 >MS exit then
   NS-PER-MS 1 - + NS-PER-MS / >MS ;


: CHECK-TIME ( ^session -- )
   SESSION-DEADLINE @ NS>N mono-ns <= if E-SXMODEM-TIMEOUT throw then ;


: WINDOW ( ms ^session -- ns ) {: wait:ms session:^ :}
   session CHECK-TIME
   wait MS>N NS-PER-MS * mono-ns + session SESSION-DEADLINE @ NS>N min >NS ;


: CONTROL-WINDOW ( ^session -- ns )
   dup SESSION-CONTROL-WAIT @ swap WINDOW ;


: RECEIVE-WINDOW ( ^session -- ns )
   dup SESSION-RECEIVE-WAIT @ swap WINDOW ;


: FAIL-IO ( SERIAL:errno ^session -- )
   SESSION-IO-ERROR ! E-SXMODEM-IO throw ;


\ The byte is read past the packet's bytes and cut off, so a packet waiting
\ for its ACK stays whole.
: READ-BYTE ( ns ^session -- n ) {: deadline:ns session:^ :}
   deadline REMAINING {: timeout:ms :}
   timeout MS>N 0= if -1 exit then
   session PACKET {: v:^ :}
   v VEC:COUNT {: old:n :}
   session SESSION-HANDLE @ v 1 SERIAL:BYTES timeout SERIAL:READ
   MATCH SERIAL:io-result
      transferred OF drop v old VEC:AT c@ v old VEC:RESIZE ENDOF
      timeout OF -1 ENDOF
      closed OF E-SXMODEM-CLOSED throw ENDOF
      failed OF session FAIL-IO ENDOF
   ;MATCH ;


: WRITE-CHUNK ( n str ^session -- n ) {: offset:n data:str session:^ :}
   session CHECK-TIME
   session SESSION-HANDLE @ data STR:DATA offset + data STR:LEN offset - >str
   session SESSION-DEADLINE @ REMAINING SERIAL:WRITE
   MATCH SERIAL:io-result
      transferred OF BLEN>N offset + ENDOF
      timeout OF E-SXMODEM-TIMEOUT throw ENDOF
      closed OF E-SXMODEM-CLOSED throw ENDOF
      failed OF session FAIL-IO ENDOF
   ;MATCH ;


: WRITE-ALL ( str ^session -- ) {: data:str session:^ :}
   0 begin dup data STR:LEN < while data session WRITE-CHUNK repeat drop ;


: SEND-BYTE ( n ^session -- ) {: value:n session:^ :}
   session SESSION-CONTROL SPAN:$ STR:DATA {: control :}
   value control c! control 1 >str session WRITE-ALL ;


: READ-CONTROL ( ns ^session -- n ) {: deadline:ns session:^ :}
   deadline session READ-BYTE dup CAN <> if exit then drop
   deadline session READ-BYTE dup CAN = if
      drop ACK session SEND-BYTE E-SXMODEM-CANCELLED throw
   then ;


: WAIT-REPLY ( ns ^session -- n ) {: deadline:ns session:^ :}
   begin
      deadline session READ-CONTROL
      dup ACK = over NAK = or over -1 = or if exit then drop
   again ;


: WAIT-HEADER ( ns ^session -- n ) {: deadline:ns session:^ :}
   begin
      deadline session READ-CONTROL
      dup SOH = over STX = or over EOT = or over -1 = or if exit then drop
   again ;


\ Appends to the packet what arrives of its first n bytes; false when the
\ wait runs out.
: READ-CHUNK ( n ^session -- bool ) {: size:n session:^ :}
   session PACKET {: v:^ :}
   session SESSION-HANDLE @ v size v VEC:COUNT - SERIAL:BYTES
   session RECEIVE-WINDOW REMAINING SERIAL:READ
   MATCH SERIAL:io-result
      transferred OF drop TRUE ENDOF
      timeout OF FALSE ENDOF
      closed OF E-SXMODEM-CLOSED throw ENDOF
      failed OF session FAIL-IO ENDOF
   ;MATCH ;


\ A partial frame may continue while each chunk arrives within the configured
\ wait. The overall transfer deadline still bounds every chunk and drain.
: READ-REST ( n ^session -- bool ) {: size:n session:^ :}
   begin session PACKET VEC:COUNT size < while
      size session READ-CHUNK 0= if FALSE exit then
   repeat TRUE ;


: DRAIN ( ^session -- ) {: session:^ :}
   begin session RECEIVE-WINDOW session READ-BYTE -1 = until
   session CHECK-TIME ;


: TRY+ ( ^session -- )
   SESSION-TRIES dup @ 1 + dup MAX-TRIES >= if drop drop E-SXMODEM-RETRIES throw then swap ! ;


: IGNORE-IO ( SERIAL:io-result -- )
   MATCH SERIAL:io-result
      transferred OF drop ENDOF timeout OF ENDOF
      closed OF ENDOF failed OF drop ENDOF
   ;MATCH ;


\ Best effort only, without waiting beyond the transfer deadline.
: CANCEL-PEER ( ^session -- ) {: session:^ :}
   session SESSION-CONTROL SPAN:$ {: buf:str :}
   buf STR:DATA {: control :}
   CAN control c! CAN control 1 + c! CAN control 2 + c!
   session SESSION-HANDLE @ buf 0 >MS SERIAL:WRITE IGNORE-IO ;


: RESULT ( ^session n -- transfer-result ) {: session:^ error:n :}
   error 0= if session SESSION-PROGRESS @ BYTES SERIAL--XMODEM-TRANSFER--RESULT:COMPLETED exit then
   error E-SXMODEM-CANCELLED <> if session CANCEL-PEER then
   error E-SXMODEM-TIMEOUT = if SERIAL--XMODEM-TRANSFER--RESULT:TIMEOUT exit then
   error E-SXMODEM-CLOSED = if SERIAL--XMODEM-TRANSFER--RESULT:CLOSED exit then
   error E-SXMODEM-IO = if session SESSION-IO-ERROR @ SERIAL--XMODEM-TRANSFER--RESULT:FAILED exit then
   error E-SXMODEM-CANCELLED = if SERIAL--XMODEM-TRANSFER--RESULT:CANCELLED exit then
   error E-SXMODEM-RETRIES = if SERIAL--XMODEM-TRANSFER--RESULT:RETRY-LIMIT exit then
   error E-SXMODEM-CAPACITY = if SERIAL--XMODEM-TRANSFER--RESULT:CAPACITY exit then
   error throw ;


: START ( ms ^session -- ) {: timeout:ms session:^ :}
   timeout CHECK-MS session PACKET 0 VEC:RESIZE
   timeout MS>N NS-PER-MS * mono-ns + >NS session SESSION-DEADLINE !
   0 SERIAL:>ERRNO session SESSION-IO-ERROR ! 0 session SESSION-PROGRESS ! 0 session SESSION-TRIES !
   1 session SESSION-NEXT ! CRC session SESSION-KIND ! FALSE session SESSION-NEGOTIATED ! ;


: NEGOTIATE-SEND ( ^session -- ) {: session:^ :}
   begin
      session SESSION-DEADLINE @ session READ-CONTROL
      dup REQUEST-CRC = if drop CRC session SESSION-KIND ! exit then
      NAK = if SUM session SESSION-KIND ! 128 >PAYLOAD-SIZE session SESSION-BLOCK ! exit then
      session CHECK-TIME
   again ;


: CHUNK-SIZE ( ^session -- n ) {: session:^ :}
   session SESSION-SOURCE-LEN @ session SESSION-PROGRESS @ - session SESSION-BLOCK @ PAYLOAD-SIZE>N min ;


: BUILD-PACKET ( ^session -- ) {: session:^ :}
   session SESSION-SOURCE @ session SESSION-PROGRESS @ + session CHUNK-SIZE >str
   session SESSION-NEXT @ >SEQUENCE session SESSION-BLOCK @
   session SESSION-KIND @ session PACKET ENCODE ;


: SEND-PACKET ( ^session -- ) {: session:^ :}
   MAX-TRIES 0 do
      session PACKET VEC:BYTES session WRITE-ALL
      session CONTROL-WINDOW session WAIT-REPLY ACK = if unloop exit then
      session CHECK-TIME
   loop E-SXMODEM-RETRIES throw ;


: SEND-EOT ( ^session -- ) {: session:^ :}
   MAX-TRIES 0 do
      EOT session SEND-BYTE
      session CONTROL-WINDOW session WAIT-REPLY ACK = if unloop exit then
      session CHECK-TIME
   loop E-SXMODEM-RETRIES throw ;


: ADVANCE ( n ^session -- ) {: size:n session:^ :}
   size session SESSION-PROGRESS @ + session SESSION-PROGRESS !
   session SESSION-NEXT @ 1 + $FF and session SESSION-NEXT !
   0 session SESSION-TRIES ! ;


: SEND-RUN ( ^session -- ) {: session:^ :}
   session NEGOTIATE-SEND
   begin session SESSION-PROGRESS @ session SESSION-SOURCE-LEN @ < while
      session BUILD-PACKET session SEND-PACKET session CHUNK-SIZE session ADVANCE
   repeat session SEND-EOT ;


: FRAME-SIZE ( n ^session -- NUM:byte-len ) {: header:n session:^ :}
   header SOH = if 128 else 1024 then >PAYLOAD-SIZE
   session SESSION-KIND @ PACKET-BYTES ;


: READ-FRAME ( n ^session -- str ) {: header:n session:^ :}
   header session FRAME-SIZE BLEN>N {: size:n :}
   session PACKET {: v:^ :}
   v 0 VEC:RESIZE  header v VEC:PUSH c!
   size session READ-REST 0= if E-FRAME throw then
   v VEC:BYTES ;


: PREVIOUS? ( sequence ^session -- bool ) {: sequence:sequence session:^ :}
   session SESSION-PROGRESS @ 0 >
   sequence SEQUENCE>N session SESSION-NEXT @ 1 - $FF and = and ;


: ACCEPT-PAYLOAD ( str sequence ^session -- )
   {: data:str sequence:sequence session:^ :}
   sequence SEQUENCE>N session SESSION-NEXT @ = if
      data STR:LEN session SESSION-MAXIMUM @ session SESSION-PROGRESS @ - >
      if E-SXMODEM-CAPACITY throw then
      data session SESSION-OUTPUT @ VEC:APPEND
      data STR:LEN session ADVANCE ACK session SEND-BYTE exit
   then
   sequence session PREVIOUS? if session TRY+ ACK session SEND-BYTE exit then
   E-FRAME throw ;


: RECEIVE-PACKET ( n ^session -- ) {: header:n session:^ :}
   TRUE session SESSION-NEGOTIATED !
   header session READ-FRAME session SESSION-KIND @ DECODE
   session ACCEPT-PAYLOAD ;


: RECEIVE-ONE ( n ^session -- bool ) {: header:n session:^ :}
   [: header session RECEIVE-PACKET ;] catch
   dup 0= if drop TRUE exit then
   dup E-FRAME = if drop FALSE exit then throw ;


: RECEIVE-RETRY ( ^session -- ) {: session:^ :}
   session CHECK-TIME session TRY+
   session SESSION-NEGOTIATED @ if NAK else
      session SESSION-TRIES @ 3 = if SUM session SESSION-KIND ! then
      session SESSION-KIND @ CHECK-KIND>N 1 = if REQUEST-CRC else NAK then
   then session SEND-BYTE ;


: RECEIVE-RUN ( ^session -- ) {: session:^ :}
   session SESSION-OUTPUT @ 0 VEC:RESIZE REQUEST-CRC session SEND-BYTE
   begin
      session CONTROL-WINDOW session WAIT-HEADER {: header:n :}
      header EOT = if ACK session SEND-BYTE exit then
      header -1 = if session RECEIVE-RETRY else
         header session RECEIVE-ONE 0= if
            session DRAIN session RECEIVE-RETRY
         then
      then
   again ;

public

\ A fresh session is zero-initialized storage. The control wait covers an ACK or
\ the next header and must allow a whole packet at the line's baud rate; the
\ inactivity wait covers gaps inside a frame and must be shorter than the peer's
\ retransmission interval.
: INIT ( SERIAL:handle ms ms ^session -- )
   {: handle:SERIAL:handle control-wait:ms receive-wait:ms session:^ :}
   control-wait CHECK-MS receive-wait CHECK-MS
   handle SERIAL:HANDLE>N dup 0 < swap $7FFFFFFF > or if E-OPERAND throw then
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW session SESSION-PACKET !
   session PACKET 1029 VEC:RESERVE
   handle session SESSION-HANDLE !
   control-wait session SESSION-CONTROL-WAIT ! receive-wait session SESSION-RECEIVE-WAIT ! ;


\ A second DISPOSE finds the packet null and frees nothing.
: DISPOSE ( ^session -- )
   SESSION-PACKET dup @ VEC:DELETE NULL-PTR swap ! ;


\ The source must be disjoint from the session and its private packet buffer.
\ CRC mode uses the requested payload size; checksum negotiation selects 128.
\ Success requires the peer's EOT ACK and reports the original source length.
: SEND ( str XMODEM:payload-size ms ^session -- transfer-result )
   {: data:str block:XMODEM:payload-size timeout:ms session:^ :}
   block PAYLOAD-SIZE>N XMODEM:BLOCK drop
   timeout session START
   data STR:DATA session SESSION-SOURCE ! data STR:LEN session SESSION-SOURCE-LEN !
   block session SESSION-BLOCK !
   session [: dup SEND-RUN ;] catch RESULT ;


\ The output is a caller-owned vector, disjoint from the session. The limit
\ includes whole padded packets.
: RECEIVE ( NUM:byte-len ^VEC:vector<u8> ms ^session -- transfer-result )
   {: maximum:NUM:byte-len output:^ timeout:ms session:^ :}
   maximum CHECK-LENGTH timeout session START
   output session SESSION-OUTPUT ! maximum BLEN>N session SESSION-MAXIMUM !
   session [: dup RECEIVE-RUN ;] catch RESULT ;

;package
