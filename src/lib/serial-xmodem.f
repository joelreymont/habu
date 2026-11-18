split-str
\ serial-xmodem.f - bounded XMODEM transfers over an already-open raw serial
\ stream. Each SEND or RECEIVE takes one overall deadline that negotiation,
\ retries and draining share. Only `completed` means done: a failure may leave
\ an accepted prefix and padding is never stripped, so the application frames
\ its own length; DISPOSE leaves the serial handle open. test/serial-xmodem.py.
\ STORAGE CLASS. CALLER-OWNED; a session or a stream serves one task at a time.
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

E-SXMODEM-OPERAND constant E-OPERAND

private
using XMODEM

1000000 constant NS-PER-MS
10 constant MAX-TRIES
19 cells constant SESSION-SIZE

\ Caller-owned session header. The packet BUF is the only owned allocation;
\ the serial handle, source span, and destination BUF remain borrowed.
: HANDLE-FIELD ( ^a -- ^n ) BYTE-VIEW CELL-VIEW ;
: CONTROL-WAIT-FIELD ( ^a -- ^n ) BYTE-VIEW 1 cells + CELL-VIEW ;
: DEADLINE-FIELD ( ^a -- ^n ) BYTE-VIEW 2 cells + CELL-VIEW ;
: IO-ERROR-FIELD ( ^a -- ^n ) BYTE-VIEW 3 cells + CELL-VIEW ;
: KIND-FIELD ( ^a -- ^n ) BYTE-VIEW 4 cells + CELL-VIEW ;
: NEXT-FIELD ( ^a -- ^n ) BYTE-VIEW 5 cells + CELL-VIEW ;
: TRIES-FIELD ( ^a -- ^n ) BYTE-VIEW 6 cells + CELL-VIEW ;
: PROGRESS-FIELD ( ^a -- ^n ) BYTE-VIEW 7 cells + CELL-VIEW ;
: PACKET-BUF ( ^a -- ^a ) 8 cells + ;
: CONTROL-DATA ( ^a -- ^u8 ) BYTE-VIEW 11 cells + ;
: SOURCE-FIELD ( ^a -- ^^u8 ) 12 cells ptr-field ;
: SOURCE-LEN-FIELD ( ^a -- ^n ) BYTE-VIEW 13 cells + CELL-VIEW ;
: BLOCK-FIELD ( ^a -- ^n ) BYTE-VIEW 14 cells + CELL-VIEW ;
: OUTPUT-FIELD ( ^a -- ^^a ) 15 cells ptr-field ;
: MAXIMUM-FIELD ( ^a -- ^n ) BYTE-VIEW 16 cells + CELL-VIEW ;
: NEGOTIATED-FIELD ( ^a -- ^n ) BYTE-VIEW 17 cells + CELL-VIEW ;
: RECEIVE-WAIT-FIELD ( ^a -- ^n ) BYTE-VIEW 18 cells + CELL-VIEW ;

CAST: BLEN>N ( NUM:byte-len -- n )


: CHECK-MS ( ms -- )
   MS>N dup 1 < swap $7FFFFFFF > or if E-OPERAND throw then ;


: CHECK-LENGTH ( NUM:byte-len -- )
   BLEN>N 0 < if E-OPERAND throw then ;


: REMAINING ( ns -- ms )
   NS>N mono-ns - dup 0 <= if drop 0 >MS exit then
   NS-PER-MS 1 - + NS-PER-MS / >MS ;


: CHECK-TIME ( ^a -- )
   DEADLINE-FIELD @ mono-ns <= if E-SXMODEM-TIMEOUT throw then ;


: WINDOW ( n ^a -- ns ) {: wait:n session:^ :}
   session CHECK-TIME
   wait NS-PER-MS * mono-ns + session DEADLINE-FIELD @ min >NS ;


: CONTROL-WINDOW ( ^a -- ns )
   dup CONTROL-WAIT-FIELD @ swap WINDOW ;


: RECEIVE-WINDOW ( ^a -- ns )
   dup RECEIVE-WAIT-FIELD @ swap WINDOW ;


: FAIL-IO ( SERIAL:errno ^a -- )
   IO-ERROR-FIELD swap SERIAL:ERRNO>N swap ! E-SXMODEM-IO throw ;


: READ-BYTE ( ns ^a -- n ) {: deadline:ns session:^ :}
   deadline REMAINING {: timeout:ms :}
   timeout MS>N 0= if -1 exit then
   session HANDLE-FIELD @ SERIAL:>HANDLE session CONTROL-DATA 1 SERIAL:BYTES timeout SERIAL:READ
   MATCH SERIAL:io-result
      transferred OF drop session CONTROL-DATA c@ ENDOF
      timeout OF -1 ENDOF
      closed OF E-SXMODEM-CLOSED throw ENDOF
      failed OF session FAIL-IO ENDOF
   ;MATCH ;


: WRITE-CHUNK ( n str ^a -- n ) {: offset:n data size:n session:^ :}
   session CHECK-TIME
   session HANDLE-FIELD @ SERIAL:>HANDLE data offset + size offset - SERIAL:BYTES
   session DEADLINE-FIELD @ >NS REMAINING SERIAL:WRITE
   MATCH SERIAL:io-result
      transferred OF BLEN>N offset + ENDOF
      timeout OF E-SXMODEM-TIMEOUT throw ENDOF
      closed OF E-SXMODEM-CLOSED throw ENDOF
      failed OF session FAIL-IO ENDOF
   ;MATCH ;


: WRITE-ALL ( ^u8 n ^a -- ) {: data size:n session:^ :}
   0 begin dup size < while data size session WRITE-CHUNK repeat drop ;


: SEND-BYTE ( n ^a -- ) {: value:n session:^ :}
   value session CONTROL-DATA c! session CONTROL-DATA 1 session WRITE-ALL ;


: READ-CONTROL ( ns ^a -- n ) {: deadline:ns session:^ :}
   deadline session READ-BYTE dup CAN <> if exit then drop
   deadline session READ-BYTE dup CAN = if
      drop ACK session SEND-BYTE E-SXMODEM-CANCELLED throw
   then ;


: WAIT-REPLY ( ns ^a -- n ) {: deadline:ns session:^ :}
   begin
      deadline session READ-CONTROL
      dup ACK = over NAK = or over -1 = or if exit then drop
   again ;


: WAIT-HEADER ( ns ^a -- n ) {: deadline:ns session:^ :}
   begin
      deadline session READ-CONTROL
      dup SOH = over STX = or over EOT = or over -1 = or if exit then drop
   again ;


: READ-CHUNK ( n str ^a -- n ) {: offset:n data size:n session:^ :}
   session HANDLE-FIELD @ SERIAL:>HANDLE data offset + size offset - SERIAL:BYTES
   session RECEIVE-WINDOW REMAINING SERIAL:READ
   MATCH SERIAL:io-result
      transferred OF BLEN>N offset + ENDOF
      timeout OF -1 ENDOF
      closed OF E-SXMODEM-CLOSED throw ENDOF
      failed OF session FAIL-IO ENDOF
   ;MATCH ;


\ A partial frame may continue while each chunk arrives within the configured
\ wait. The overall transfer deadline still bounds every chunk and drain.
: READ-REST ( str ^a -- bool ) {: data size:n session:^ :}
   0 begin dup size < while
      data size session READ-CHUNK dup 0 < if drop FALSE exit then
   repeat drop TRUE ;


: DRAIN ( ^a -- ) {: session:^ :}
   begin session RECEIVE-WINDOW session READ-BYTE -1 = until
   session CHECK-TIME ;


: TRY+ ( ^a -- )
   TRIES-FIELD dup @ 1 + dup MAX-TRIES >= if drop drop E-SXMODEM-RETRIES throw then swap ! ;


: IGNORE-IO ( SERIAL:io-result -- )
   MATCH SERIAL:io-result
      transferred OF drop ENDOF timeout OF ENDOF
      closed OF ENDOF failed OF drop ENDOF
   ;MATCH ;


\ Best effort only, without waiting beyond the transfer deadline.
: CANCEL-PEER ( ^a -- ) {: session:^ :}
   CAN session CONTROL-DATA c! CAN session CONTROL-DATA 1 + c! CAN session CONTROL-DATA 2 + c!
   session HANDLE-FIELD @ SERIAL:>HANDLE session CONTROL-DATA 3 SERIAL:BYTES 0 >MS SERIAL:WRITE IGNORE-IO ;


: RESULT ( ^a n -- transfer-result ) {: session:^ error:n :}
   error 0= if session PROGRESS-FIELD @ BYTES SERIAL--XMODEM-TRANSFER--RESULT:COMPLETED exit then
   error E-SXMODEM-CANCELLED <> if session CANCEL-PEER then
   error E-SXMODEM-TIMEOUT = if SERIAL--XMODEM-TRANSFER--RESULT:TIMEOUT exit then
   error E-SXMODEM-CLOSED = if SERIAL--XMODEM-TRANSFER--RESULT:CLOSED exit then
   error E-SXMODEM-IO = if session IO-ERROR-FIELD @ SERIAL:>ERRNO SERIAL--XMODEM-TRANSFER--RESULT:FAILED exit then
   error E-SXMODEM-CANCELLED = if SERIAL--XMODEM-TRANSFER--RESULT:CANCELLED exit then
   error E-SXMODEM-RETRIES = if SERIAL--XMODEM-TRANSFER--RESULT:RETRY-LIMIT exit then
   error E-SXMODEM-CAPACITY = if SERIAL--XMODEM-TRANSFER--RESULT:CAPACITY exit then
   error throw ;


: START ( ms ^a -- ) {: timeout:ms session:^ :}
   timeout CHECK-MS session PACKET-BUF BUF:CLEAR
   timeout MS>N NS-PER-MS * mono-ns + session DEADLINE-FIELD !
   0 session IO-ERROR-FIELD ! 0 session PROGRESS-FIELD ! 0 session TRIES-FIELD !
   1 session NEXT-FIELD ! 1 session KIND-FIELD ! 0 session NEGOTIATED-FIELD ! ;


: NEGOTIATE-SEND ( ^a -- ) {: session:^ :}
   begin
      session DEADLINE-FIELD @ >NS session READ-CONTROL
      dup REQUEST-CRC = if drop 1 session KIND-FIELD ! exit then
      NAK = if 0 session KIND-FIELD ! 128 session BLOCK-FIELD ! exit then
      session CHECK-TIME
   again ;


: CHUNK-SIZE ( ^a -- n ) {: session:^ :}
   session SOURCE-LEN-FIELD @ session PROGRESS-FIELD @ - session BLOCK-FIELD @ min ;


: BUILD-PACKET ( ^a -- ) {: session:^ :}
   session SOURCE-FIELD @ session PROGRESS-FIELD @ + session CHUNK-SIZE BYTES
   session NEXT-FIELD @ >SEQUENCE session BLOCK-FIELD @ >PAYLOAD-SIZE
   session KIND-FIELD @ >CHECK-KIND session PACKET-BUF ENCODE ;


: SEND-PACKET ( ^a -- ) {: session:^ :}
   MAX-TRIES 0 do
      session PACKET-BUF BUF:SPAN$ BLEN>N session WRITE-ALL
      session CONTROL-WINDOW session WAIT-REPLY ACK = if unloop exit then
      session CHECK-TIME
   loop E-SXMODEM-RETRIES throw ;


: SEND-EOT ( ^a -- ) {: session:^ :}
   MAX-TRIES 0 do
      EOT session SEND-BYTE
      session CONTROL-WINDOW session WAIT-REPLY ACK = if unloop exit then
      session CHECK-TIME
   loop E-SXMODEM-RETRIES throw ;


: ADVANCE ( n ^a -- ) {: size:n session:^ :}
   size session PROGRESS-FIELD @ + session PROGRESS-FIELD !
   session NEXT-FIELD @ 1 + $FF and session NEXT-FIELD !
   0 session TRIES-FIELD ! ;


: SEND-RUN ( ^a -- ) {: session:^ :}
   session NEGOTIATE-SEND
   begin session PROGRESS-FIELD @ session SOURCE-LEN-FIELD @ < while
      session BUILD-PACKET session SEND-PACKET session CHUNK-SIZE session ADVANCE
   repeat session SEND-EOT ;


: FRAME-SIZE ( n ^a -- NUM:byte-len ) {: header:n session:^ :}
   header SOH = if 128 else 1024 then >PAYLOAD-SIZE
   session KIND-FIELD @ >CHECK-KIND PACKET-BYTES ;


: READ-FRAME ( ^a -- NUM:byte-len ) {: session:^ :}
   session CONTROL-DATA c@ {: header:n :}
   header session FRAME-SIZE {: size:NUM:byte-len :}
   session PACKET-BUF BUF:SPAN$ drop {: data :}
   header data c!
   data 1 + size BLEN>N 1 - session READ-REST 0= if E-FRAME throw then size ;


: PREVIOUS? ( sequence ^a -- bool ) {: sequence:sequence session:^ :}
   session PROGRESS-FIELD @ 0 >
   sequence SEQUENCE>N session NEXT-FIELD @ 1 - $FF and = and ;


: ACCEPT-PAYLOAD ( ^u8 NUM:byte-len sequence ^a -- )
   {: data size:NUM:byte-len sequence:sequence session:^ :}
   sequence SEQUENCE>N session NEXT-FIELD @ = if
      size BLEN>N session MAXIMUM-FIELD @ session PROGRESS-FIELD @ - >
      if E-SXMODEM-CAPACITY throw then
      data size session OUTPUT-FIELD @ BUF:APPEND-SPAN
      size BLEN>N session ADVANCE ACK session SEND-BYTE exit
   then
   sequence session PREVIOUS? if session TRY+ ACK session SEND-BYTE exit then
   E-FRAME throw ;


: RECEIVE-PACKET ( ^a -- ) {: session:^ :}
   1 session NEGOTIATED-FIELD !
   session READ-FRAME {: size:NUM:byte-len :}
   session PACKET-BUF BUF:SPAN$ drop size session KIND-FIELD @ >CHECK-KIND DECODE
   session ACCEPT-PAYLOAD ;


: RECEIVE-ONE ( ^a -- bool )
   [: dup RECEIVE-PACKET ;] catch swap drop
   dup 0= if drop TRUE exit then
   dup E-FRAME = if drop FALSE exit then throw ;


: RECEIVE-RETRY ( ^a -- ) {: session:^ :}
   session CHECK-TIME session TRY+
   session NEGOTIATED-FIELD @ 0= if
      session TRIES-FIELD @ 3 = if 0 session KIND-FIELD ! then
      session KIND-FIELD @ 1 = if REQUEST-CRC else NAK then
   else NAK then session SEND-BYTE ;


: RECEIVE-RUN ( ^a -- ) {: session:^ :}
   session OUTPUT-FIELD @ BUF:CLEAR REQUEST-CRC session SEND-BYTE
   begin
      session CONTROL-WINDOW session WAIT-HEADER {: header:n :}
      header EOT = if ACK session SEND-BYTE exit then
      header -1 = if session RECEIVE-RETRY else
         session RECEIVE-ONE 0= if session DRAIN session RECEIVE-RETRY then
      then
   again ;

public

: SESSION-BYTES ( -- n ) SESSION-SIZE ;


\ A fresh session is zero-initialized storage of SESSION-BYTES bytes. The control
\ wait covers an ACK or the next header and must allow a whole packet at the
\ line's baud rate; the inactivity wait covers gaps inside a frame and must be
\ shorter than the peer's retransmission interval.
: INIT ( SERIAL:handle ms ms ^a -- )
   {: handle:SERIAL:handle control-wait:ms receive-wait:ms session:^ :}
   control-wait CHECK-MS receive-wait CHECK-MS
   handle SERIAL:HANDLE>N dup 0 < swap $7FFFFFFF > or if E-OPERAND throw then
   session PACKET-BUF 1029 BYTES BUF:INIT
   handle SERIAL:HANDLE>N session HANDLE-FIELD !
   control-wait MS>N session CONTROL-WAIT-FIELD ! receive-wait MS>N session RECEIVE-WAIT-FIELD ! ;


: DISPOSE ( ^a -- ) PACKET-BUF BUF:DISPOSE ;


\ The source must be disjoint from the session and its private packet buffer.
\ CRC mode uses the requested payload size; checksum negotiation selects 128.
\ Success requires the peer's EOT ACK and reports the original source length.
: SEND ( ^u8 NUM:byte-len XMODEM:payload-size ms ^a -- transfer-result )
   {: data size:NUM:byte-len block:XMODEM:payload-size timeout:ms session:^ :}
   size CHECK-LENGTH block PAYLOAD-SIZE>N XMODEM:BLOCK drop
   timeout session START
   data session SOURCE-FIELD ! size BLEN>N session SOURCE-LEN-FIELD !
   block PAYLOAD-SIZE>N session BLOCK-FIELD !
   session [: dup SEND-RUN ;] catch RESULT ;


\ The output is an initialized caller-owned BUF, disjoint from the session. The
\ limit includes whole padded packets.
: RECEIVE ( NUM:byte-len ^a ms ^a -- transfer-result )
   {: maximum:NUM:byte-len output:^ timeout:ms session:^ :}
   maximum CHECK-LENGTH output BUF:SPAN$ 2drop timeout session START
   output session OUTPUT-FIELD ! maximum BLEN>N session MAXIMUM-FIELD !
   session [: dup RECEIVE-RUN ;] catch RESULT ;

;package
