\ XMODEM packet framing. No transport, file, or device policy.
require lib/errors.f
require lib/type/deftype.f
require lib/byte-buffer.f

package XMODEM
public

DEFTYPE SEQUENCE
DEFTYPE PAYLOAD-SIZE
DEFTYPE CHECK-KIND

E-XMODEM-OPERAND constant E-OPERAND
E-XMODEM-FRAME constant E-FRAME

$01 constant SOH
$02 constant STX
$04 constant EOT
$06 constant ACK
$15 constant NAK
$18 constant CAN
$43 constant REQUEST-CRC

private
using BUF

CAST: BLEN>N ( NUM:byte-len -- n )


: CHECK-SEQUENCE ( n -- )
   dup 0 < swap $FF > or if E-OPERAND throw then ;


: CHECK-BLOCK ( n -- )
   dup 128 = swap 1024 = or 0= if E-OPERAND throw then ;


: CHECK-KIND ( n -- )
   dup 0 < swap 1 > or if E-OPERAND throw then ;


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


: CRC-BYTE ( n n -- n )
   8 lshift xor
   8 0 do
      dup $8000 and 0 <> if 1 lshift $1021 xor else 1 lshift then
      $FFFF and
   loop ;


: CHECK-VALUE ( ptr u8 n n -- n ) {: data size:n kind:n :}
   0 size 0 ?do
      data i + c@ kind 1 = if CRC-BYTE else + $FF and then
   loop ;


: START-BYTE ( n -- n )
   128 = if SOH else STX then ;


: WIRE-BLOCK ( n -- n )
   dup SOH = if drop 128 exit then
   STX = if 1024 exit then E-FRAME throw ;


: CHECK-ENCODE ( n n n n -- ) {: size:n sequence:n block:n kind:n :}
   sequence CHECK-SEQUENCE block CHECK-BLOCK kind CHECK-KIND
   size 0 < size block > or if E-OPERAND throw then ;


: HEADER+ ( n n ptr a -- ) {: sequence:n block:n output:ptr :}
   block START-BYTE output APPEND-BYTE
   sequence output APPEND-BYTE sequence $FF xor output APPEND-BYTE ;


: PAYLOAD+ ( ptr u8 n n ptr a -- ) {: data size:n block:n output:ptr :}
   data size LENGTH output APPEND-SPAN
   block size - 0 ?do $1A output APPEND-BYTE loop ;


: TRAILER+ ( n n ptr a -- ) {: block:n kind:n output:ptr :}
   output SPAN$ drop 3 + block kind CHECK-VALUE {: check:n :}
   kind 1 = if check 8 rshift output APPEND-BYTE then
   check $FF and output APPEND-BYTE ;


: TRAILER@ ( ptr u8 n -- n ) {: data kind:n :}
   data c@ kind 1 = if 8 lshift data 1 + c@ or then ;


: CHECK-DECODE ( ptr u8 n n -- n ) {: data size:n kind:n :}
   kind CHECK-KIND
   size 4 < if E-FRAME throw then
   data c@ WIRE-BLOCK {: block:n :}
   size block 4 + kind + <> if E-FRAME throw then
   data 1 + c@ data 2 + c@ xor $FF <> if E-FRAME throw then
   data 3 + block kind CHECK-VALUE
   data 3 + block + kind TRAILER@ <> if E-FRAME throw then block ;

public

: BYTES ( n -- NUM:byte-len ) LENGTH ;
: SEQUENCE ( n -- sequence ) dup CHECK-SEQUENCE >SEQUENCE ;
: BLOCK ( n -- payload-size ) dup CHECK-BLOCK >PAYLOAD-SIZE ;
: SUM ( -- check-kind ) 0 >CHECK-KIND ;
: CRC ( -- check-kind ) 1 >CHECK-KIND ;


: PACKET-BYTES ( payload-size check-kind -- NUM:byte-len )
   CHECK-KIND>N swap PAYLOAD-SIZE>N {: kind:n block:n :}
   kind CHECK-KIND block CHECK-BLOCK block 4 + kind + LENGTH ;


\ Output is an initialized BUF. The borrowed payload must not alias it.
\ Input length may be zero; unused payload bytes are all 0x1A.
: ENCODE ( ptr u8 NUM:byte-len sequence payload-size check-kind ptr a -- )
   {: data size:NUM:byte-len sequence:sequence block:payload-size kind:check-kind output:ptr :}
   size BLEN>N sequence SEQUENCE>N block PAYLOAD-SIZE>N kind CHECK-KIND>N CHECK-ENCODE
   output block kind PACKET-BYTES RESERVE output CLEAR
   sequence SEQUENCE>N block PAYLOAD-SIZE>N output HEADER+
   data size BLEN>N block PAYLOAD-SIZE>N output PAYLOAD+
   block PAYLOAD-SIZE>N kind CHECK-KIND>N output TRAILER+ ;


\ Validates one entire packet, then borrows its full 128/1024-byte payload.
\ XMODEM carries no exact binary length; never strip 0x1A from this span.
: DECODE ( ptr u8 NUM:byte-len check-kind -- ptr u8 NUM:byte-len sequence )
   {: data size:NUM:byte-len kind:check-kind :}
   data size BLEN>N kind CHECK-KIND>N CHECK-DECODE {: block:n :}
   data 3 + block LENGTH data 1 + c@ >SEQUENCE ;

;package
