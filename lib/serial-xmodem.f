\ Bounded XMODEM transfers over an already-open raw serial stream.
require lib/xmodem.f
require lib/serial.f

package SERIAL-XMODEM
public

SUMTYPE transfer-result 0
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
: HANDLE-FIELD ( ptr a -- ptr n ) BYTE-VIEW CELL-VIEW ;
: CONTROL-WAIT-FIELD ( ptr a -- ptr n ) BYTE-VIEW 1 cells + CELL-VIEW ;
: DEADLINE-FIELD ( ptr a -- ptr n ) BYTE-VIEW 2 cells + CELL-VIEW ;
: IO-ERROR-FIELD ( ptr a -- ptr n ) BYTE-VIEW 3 cells + CELL-VIEW ;
: KIND-FIELD ( ptr a -- ptr n ) BYTE-VIEW 4 cells + CELL-VIEW ;
: NEXT-FIELD ( ptr a -- ptr n ) BYTE-VIEW 5 cells + CELL-VIEW ;
: TRIES-FIELD ( ptr a -- ptr n ) BYTE-VIEW 6 cells + CELL-VIEW ;
: PROGRESS-FIELD ( ptr a -- ptr n ) BYTE-VIEW 7 cells + CELL-VIEW ;
: PACKET-BUF ( ptr a -- ptr a ) 8 cells + ;
: CONTROL-DATA ( ptr a -- ptr u8 ) BYTE-VIEW 11 cells + ;
: SOURCE-FIELD ( ptr a -- ptr ptr u8 ) 12 cells ptr-field ;
: SOURCE-LEN-FIELD ( ptr a -- ptr n ) BYTE-VIEW 13 cells + CELL-VIEW ;
: BLOCK-FIELD ( ptr a -- ptr n ) BYTE-VIEW 14 cells + CELL-VIEW ;
: OUTPUT-FIELD ( ptr a -- ptr ptr a ) 15 cells ptr-field ;
: MAXIMUM-FIELD ( ptr a -- ptr n ) BYTE-VIEW 16 cells + CELL-VIEW ;
: NEGOTIATED-FIELD ( ptr a -- ptr n ) BYTE-VIEW 17 cells + CELL-VIEW ;
: RECEIVE-WAIT-FIELD ( ptr a -- ptr n ) BYTE-VIEW 18 cells + CELL-VIEW ;

CAST: BLEN>N ( NUM:byte-len -- n )


: CHECK-MS ( ms -- )
   MS>N dup 1 < swap $7FFFFFFF > or if E-OPERAND throw then ;


: CHECK-LENGTH ( NUM:byte-len -- )
   BLEN>N 0 < if E-OPERAND throw then ;


: REMAINING ( ns -- ms )
   NS>N mono-ns - dup 0 <= if drop 0 >MS exit then
   NS-PER-MS 1 - + NS-PER-MS / >MS ;


: CHECK-TIME ( ptr a -- )
   DEADLINE-FIELD @ mono-ns <= if E-SXMODEM-TIMEOUT throw then ;


: WINDOW ( n ptr a -- ns ) {: wait:n session:ptr :}
   session CHECK-TIME
   wait NS-PER-MS * mono-ns + session DEADLINE-FIELD @ min >NS ;


: CONTROL-WINDOW ( ptr a -- ns )
   dup CONTROL-WAIT-FIELD @ swap WINDOW ;


: RECEIVE-WINDOW ( ptr a -- ns )
   dup RECEIVE-WAIT-FIELD @ swap WINDOW ;


: FAIL-IO ( SERIAL:errno ptr a -- )
   IO-ERROR-FIELD swap SERIAL:ERRNO>N swap ! E-SXMODEM-IO throw ;


: READ-BYTE ( ns ptr a -- n ) {: deadline:ns session:ptr :}
   deadline REMAINING {: timeout:ms :}
   timeout MS>N 0= if -1 exit then
   session HANDLE-FIELD @ SERIAL:>HANDLE session CONTROL-DATA 1 SERIAL:BYTES timeout SERIAL:READ
   MATCH SERIAL:io-result
      transferred OF drop session CONTROL-DATA c@ ENDOF
      timeout OF -1 ENDOF
      closed OF E-SXMODEM-CLOSED throw ENDOF
      failed OF session FAIL-IO ENDOF
   ;MATCH ;


: WRITE-CHUNK ( n ptr u8 n ptr a -- n ) {: offset:n data size:n session:ptr :}
   session CHECK-TIME
   session HANDLE-FIELD @ SERIAL:>HANDLE data offset + size offset - SERIAL:BYTES
   session DEADLINE-FIELD @ >NS REMAINING SERIAL:WRITE
   MATCH SERIAL:io-result
      transferred OF BLEN>N offset + ENDOF
      timeout OF E-SXMODEM-TIMEOUT throw ENDOF
      closed OF E-SXMODEM-CLOSED throw ENDOF
      failed OF session FAIL-IO ENDOF
   ;MATCH ;


: WRITE-ALL ( ptr u8 n ptr a -- ) {: data size:n session:ptr :}
   0 begin dup size < while data size session WRITE-CHUNK repeat drop ;


: SEND-BYTE ( n ptr a -- ) {: value:n session:ptr :}
   value session CONTROL-DATA c! session CONTROL-DATA 1 session WRITE-ALL ;


: READ-CONTROL ( ns ptr a -- n ) {: deadline:ns session:ptr :}
   deadline session READ-BYTE dup CAN <> if exit then drop
   deadline session READ-BYTE dup CAN = if
      drop ACK session SEND-BYTE E-SXMODEM-CANCELLED throw
   then ;


: WAIT-REPLY ( ns ptr a -- n ) {: deadline:ns session:ptr :}
   begin
      deadline session READ-CONTROL
      dup ACK = over NAK = or over -1 = or if exit then drop
   again ;


: WAIT-HEADER ( ns ptr a -- n ) {: deadline:ns session:ptr :}
   begin
      deadline session READ-CONTROL
      dup SOH = over STX = or over EOT = or over -1 = or if exit then drop
   again ;


: READ-CHUNK ( n ptr u8 n ptr a -- n ) {: offset:n data size:n session:ptr :}
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
: READ-REST ( ptr u8 n ptr a -- bool ) {: data size:n session:ptr :}
   0 begin dup size < while
      data size session READ-CHUNK dup 0 < if drop FALSE exit then
   repeat drop TRUE ;


: DRAIN ( ptr a -- ) {: session:ptr :}
   begin session RECEIVE-WINDOW session READ-BYTE -1 = until
   session CHECK-TIME ;


: TRY+ ( ptr a -- )
   TRIES-FIELD dup @ 1 + dup MAX-TRIES >= if drop drop E-SXMODEM-RETRIES throw then swap ! ;


: IGNORE-IO ( SERIAL:io-result -- )
   MATCH SERIAL:io-result
      transferred OF drop ENDOF timeout OF ENDOF
      closed OF ENDOF failed OF drop ENDOF
   ;MATCH ;


\ Best effort only, without waiting beyond the transfer deadline.
: CANCEL-PEER ( ptr a -- ) {: session:ptr :}
   CAN session CONTROL-DATA c! CAN session CONTROL-DATA 1 + c! CAN session CONTROL-DATA 2 + c!
   session HANDLE-FIELD @ SERIAL:>HANDLE session CONTROL-DATA 3 SERIAL:BYTES 0 >MS SERIAL:WRITE IGNORE-IO ;


: RESULT ( ptr a n -- transfer-result ) {: session:ptr error:n :}
   error 0= if session PROGRESS-FIELD @ BYTES SERIAL--XMODEM-TRANSFER--RESULT:completed exit then
   error E-SXMODEM-CANCELLED <> if session CANCEL-PEER then
   error E-SXMODEM-TIMEOUT = if SERIAL--XMODEM-TRANSFER--RESULT:timeout exit then
   error E-SXMODEM-CLOSED = if SERIAL--XMODEM-TRANSFER--RESULT:closed exit then
   error E-SXMODEM-IO = if session IO-ERROR-FIELD @ SERIAL:>ERRNO SERIAL--XMODEM-TRANSFER--RESULT:failed exit then
   error E-SXMODEM-CANCELLED = if SERIAL--XMODEM-TRANSFER--RESULT:cancelled exit then
   error E-SXMODEM-RETRIES = if SERIAL--XMODEM-TRANSFER--RESULT:retry-limit exit then
   error E-SXMODEM-CAPACITY = if SERIAL--XMODEM-TRANSFER--RESULT:capacity exit then
   error throw ;


: START ( ms ptr a -- ) {: timeout:ms session:ptr :}
   timeout CHECK-MS session PACKET-BUF BUF:CLEAR
   timeout MS>N NS-PER-MS * mono-ns + session DEADLINE-FIELD !
   0 session IO-ERROR-FIELD ! 0 session PROGRESS-FIELD ! 0 session TRIES-FIELD !
   1 session NEXT-FIELD ! 1 session KIND-FIELD ! 0 session NEGOTIATED-FIELD ! ;


: NEGOTIATE-SEND ( ptr a -- ) {: session:ptr :}
   begin
      session DEADLINE-FIELD @ >NS session READ-CONTROL
      dup REQUEST-CRC = if drop 1 session KIND-FIELD ! exit then
      NAK = if 0 session KIND-FIELD ! 128 session BLOCK-FIELD ! exit then
      session CHECK-TIME
   again ;


: CHUNK-SIZE ( ptr a -- n ) {: session:ptr :}
   session SOURCE-LEN-FIELD @ session PROGRESS-FIELD @ - session BLOCK-FIELD @ min ;


: BUILD-PACKET ( ptr a -- ) {: session:ptr :}
   session SOURCE-FIELD @ session PROGRESS-FIELD @ + session CHUNK-SIZE BYTES
   session NEXT-FIELD @ >SEQUENCE session BLOCK-FIELD @ >PAYLOAD-SIZE
   session KIND-FIELD @ >CHECK-KIND session PACKET-BUF ENCODE ;


: SEND-PACKET ( ptr a -- ) {: session:ptr :}
   MAX-TRIES 0 do
      session PACKET-BUF BUF:SPAN$ BLEN>N session WRITE-ALL
      session CONTROL-WINDOW session WAIT-REPLY ACK = if unloop exit then
      session CHECK-TIME
   loop E-SXMODEM-RETRIES throw ;


: SEND-EOT ( ptr a -- ) {: session:ptr :}
   MAX-TRIES 0 do
      EOT session SEND-BYTE
      session CONTROL-WINDOW session WAIT-REPLY ACK = if unloop exit then
      session CHECK-TIME
   loop E-SXMODEM-RETRIES throw ;


: ADVANCE ( n ptr a -- ) {: size:n session:ptr :}
   size session PROGRESS-FIELD @ + session PROGRESS-FIELD !
   session NEXT-FIELD @ 1 + $FF and session NEXT-FIELD !
   0 session TRIES-FIELD ! ;


: SEND-RUN ( ptr a -- ) {: session:ptr :}
   session NEGOTIATE-SEND
   begin session PROGRESS-FIELD @ session SOURCE-LEN-FIELD @ < while
      session BUILD-PACKET session SEND-PACKET session CHUNK-SIZE session ADVANCE
   repeat session SEND-EOT ;


: FRAME-SIZE ( n ptr a -- NUM:byte-len ) {: header:n session:ptr :}
   header SOH = if 128 else 1024 then >PAYLOAD-SIZE
   session KIND-FIELD @ >CHECK-KIND PACKET-BYTES ;


: READ-FRAME ( ptr a -- NUM:byte-len ) {: session:ptr :}
   session CONTROL-DATA c@ {: header:n :}
   header session FRAME-SIZE {: size:NUM:byte-len :}
   session PACKET-BUF BUF:SPAN$ drop {: data :}
   header data c!
   data 1 + size BLEN>N 1 - session READ-REST 0= if E-FRAME throw then size ;


: PREVIOUS? ( sequence ptr a -- bool ) {: sequence:sequence session:ptr :}
   session PROGRESS-FIELD @ 0 >
   sequence SEQUENCE>N session NEXT-FIELD @ 1 - $FF and = and ;


: ACCEPT-PAYLOAD ( ptr u8 NUM:byte-len sequence ptr a -- )
   {: data size:NUM:byte-len sequence:sequence session:ptr :}
   sequence SEQUENCE>N session NEXT-FIELD @ = if
      size BLEN>N session MAXIMUM-FIELD @ session PROGRESS-FIELD @ - >
      if E-SXMODEM-CAPACITY throw then
      data size session OUTPUT-FIELD @ BUF:APPEND-SPAN
      size BLEN>N session ADVANCE ACK session SEND-BYTE exit
   then
   sequence session PREVIOUS? if session TRY+ ACK session SEND-BYTE exit then
   E-FRAME throw ;


: RECEIVE-PACKET ( ptr a -- ) {: session:ptr :}
   1 session NEGOTIATED-FIELD !
   session READ-FRAME {: size:NUM:byte-len :}
   session PACKET-BUF BUF:SPAN$ drop size session KIND-FIELD @ >CHECK-KIND DECODE
   session ACCEPT-PAYLOAD ;


: RECEIVE-ONE ( ptr a -- bool )
   [: dup RECEIVE-PACKET ;] catch swap drop
   dup 0= if drop TRUE exit then
   dup E-FRAME = if drop FALSE exit then throw ;


: RECEIVE-RETRY ( ptr a -- ) {: session:ptr :}
   session CHECK-TIME session TRY+
   session NEGOTIATED-FIELD @ 0= if
      session TRIES-FIELD @ 3 = if 0 session KIND-FIELD ! then
      session KIND-FIELD @ 1 = if REQUEST-CRC else NAK then
   else NAK then session SEND-BYTE ;


: RECEIVE-RUN ( ptr a -- ) {: session:ptr :}
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


\ A fresh session is zero-initialized storage of SESSION-BYTES bytes. Control wait
\ and receive inactivity are separate, chosen for the peer and stream speed.
: INIT ( SERIAL:handle ms ms ptr a -- )
   {: handle:SERIAL:handle control-wait:ms receive-wait:ms session:ptr :}
   control-wait CHECK-MS receive-wait CHECK-MS
   handle SERIAL:HANDLE>N dup 0 < swap $7FFFFFFF > or if E-OPERAND throw then
   session PACKET-BUF 1029 BYTES BUF:INIT
   handle SERIAL:HANDLE>N session HANDLE-FIELD !
   control-wait MS>N session CONTROL-WAIT-FIELD ! receive-wait MS>N session RECEIVE-WAIT-FIELD ! ;


: DISPOSE ( ptr a -- ) PACKET-BUF BUF:DISPOSE ;


\ The source must be disjoint from the session and its private packet buffer.
\ CRC mode uses the requested payload size; checksum negotiation selects 128.
\ Success requires the peer's EOT ACK and reports the original source length.
: SEND ( ptr u8 NUM:byte-len XMODEM:payload-size ms ptr a -- transfer-result )
   {: data size:NUM:byte-len block:XMODEM:payload-size timeout:ms session:ptr :}
   size CHECK-LENGTH block PAYLOAD-SIZE>N XMODEM:BLOCK drop
   timeout session START
   data session SOURCE-FIELD ! size BLEN>N session SOURCE-LEN-FIELD !
   block PAYLOAD-SIZE>N session BLOCK-FIELD !
   session [: dup SEND-RUN ;] catch RESULT ;


\ The output is an initialized caller-owned BUF, disjoint from the session.
\ The limit includes whole padded packets. Failure may leave a partial prefix;
\ only completed establishes receipt through EOT. No padding is removed.
: RECEIVE ( NUM:byte-len ptr a ms ptr a -- transfer-result )
   {: maximum:NUM:byte-len output:ptr timeout:ms session:ptr :}
   maximum CHECK-LENGTH output BUF:SPAN$ 2drop timeout session START
   output session OUTPUT-FIELD ! maximum BLEN>N session MAXIMUM-FIELD !
   session [: dup RECEIVE-RUN ;] catch RESULT ;

;package
