\ ws-frame.f - the RFC 6455 frame header: encode it, decode it, mask a payload.
\
\ A frame is a header of 2 to 14 bytes and then its payload (RFC 6455 section
\ 5.2). The header carries FIN, three reserved bits, a four-bit opcode, the
\ mask bit, the payload length in 7, 16 or 64 bits, and a 32-bit masking key
\ when the mask bit is set. A client masks every frame it sends and a server
\ masks none (section 5.1), so the sender, not the key, decides the mask bit,
\ and the key of an unmasked frame's header is 0.
\
\ DECODE-HEADER reads the bytes held so far and answers either the header and
\ the count of bytes it took, or the count the header needs before it can be
\ read. It refuses as soon as the bytes it holds break the protocol: a reserved
\ bit is E-WS-RSV, since no extension is negotiated; a reserved opcode is
\ E-WS-OPCODE; a fragmented control frame, or one past 125 bytes, is
\ E-WS-CONTROL; a mask bit the sender must not set or must set is E-WS-MASK;
\ an extended length with its high bit set, or not in its shortest form, is
\ E-WS-LENGTH (section 5.2's MUST); a payload past the caller's bound is
\ E-WS-TOO-BIG.
\
\ ENCODE-HEADER writes each length in its shortest form, so every header it
\ writes decodes to the header it was given and every header DECODE-HEADER
\ accepts encodes back to the bytes it came from. It refuses what a sender must
\ not write, with the same codes, and a span too small for the header with
\ E-SPAN-CAPACITY, before it writes a byte.
\
\ MASK applies a masking key to a payload in place, so masking twice restores
\ it. CLOSE-CODE! writes the status code that opens a close frame's payload.
\
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require lib/errors.f
require lib/span.f

package WS
public

\ The opcodes RFC 6455 defines; OPCODE>WIRE holds their values on the wire, and
\ every other value is reserved.
ENUM opcode continuation text binary close ping pong ;ENUM

\ Who sent a frame: a client masks it and a server does not (section 5.1).
ENUM sender client server ;ENUM

\ A close frame's status code (section 7.4.1).
NEWTYPE close-code 0

\ A frame header: fin is set on a message's last frame, len is the payload's
\ length in bytes, and key is the masking key, 0 on an unmasked frame.
STRUCTURE header 0
   FIELD fin bool
   FIELD op opcode
   FIELD len n
   FIELD key n
;STRUCTURE

\ What DECODE-HEADER makes of the bytes held: either `need`, the count of bytes
\ the header takes, to hold before decoding again, or the `frame` header and
\ the count of bytes it took, where the payload starts.
ENUM decoded 0
   VARIANT need FIELD bytes n ;VARIANT
   VARIANT frame FIELD head header FIELD size n ;VARIANT
;ENUM

14 constant HEADER-MAX            \ two bytes, a 64-bit length and a key

private

CAST: >CLOSE-CODE ( n -- close-code )
CAST: CLOSE-CODE>N ( close-code -- n )

$80 constant FIN-BIT
$70 constant RSV-BITS
$0F constant OPCODE-BITS
$08 constant CONTROL-BIT          \ set in every control opcode (section 5.5)
$80 constant MASK-BIT
$7F constant LEN7-BITS
125 constant LEN7-MAX             \ the longest 7-bit length, and a control frame's limit
126 constant LEN16-MARK           \ the 7-bit length that announces a 16-bit one
127 constant LEN64-MARK           \ the 7-bit length that announces a 64-bit one
$FFFF constant LEN16-MAX
$FFFFFFFF constant KEY-MAX
4 constant KEY-BYTES
2 constant CODE-BYTES             \ a close frame's status code


: OPCODE>WIRE ( opcode -- n )
   MATCH opcode
      continuation OF $0 ENDOF
      text OF $1 ENDOF
      binary OF $2 ENDOF
      close OF $8 ENDOF
      ping OF $9 ENDOF
      pong OF $A ENDOF
   ;MATCH ;


: WIRE>OPCODE ( n -- opcode ) {: w:n :}
   w $0 = if WS-OPCODE:continuation exit then
   w $1 = if WS-OPCODE:text exit then
   w $2 = if WS-OPCODE:binary exit then
   w $8 = if WS-OPCODE:close exit then
   w $9 = if WS-OPCODE:ping exit then
   w $A = if WS-OPCODE:pong exit then
   E-WS-OPCODE throw ;


: CONTROL? ( opcode -- bool )
   OPCODE>WIRE CONTROL-BIT and 0<> ;


: MASKS? ( sender -- bool )
   MATCH sender
      client OF true ENDOF
      server OF false ENDOF
   ;MATCH ;


\ The k bytes at p as one number, high byte first, and a number into k bytes.
: BE@ ( ptr u8 n -- n ) {: p k:n :}
   0 k 0 ?do 8 lshift p i + c@ or loop ;

: BE! ( n ptr u8 n -- ) {: v p k:n :}
   k 0 ?do v k 1- i - 8 * rshift $FF and p i + c! loop ;


\ The bytes of the extended length field a length takes in its shortest form,
\ and the ones a 7-bit length announces.
: EXT-BYTES ( n -- n ) {: len:n :}
   len LEN7-MAX <= if 0 exit then
   len LEN16-MAX <= if 2 exit then
   8 ;

: EXT-OF ( n -- n ) {: len7:n :}
   len7 LEN16-MARK = if 2 exit then
   len7 LEN64-MARK = if 8 exit then
   0 ;


\ The 7-bit length field for a length: the length itself, or the mark of its
\ extended form.
: LEN7 ( n -- n ) {: len:n :}
   len EXT-BYTES {: ext:n :}
   ext 0= if len exit then
   ext 2 = if LEN16-MARK exit then
   LEN64-MARK ;

: KEY-SIZE ( bool -- n )
   if KEY-BYTES else 0 then ;


\ The first byte: FIN, the reserved bits and the opcode. A control frame is
\ never fragmented (section 5.5).
: FIRST-BYTE ( n -- bool opcode ) {: b:n :}
   b RSV-BITS and 0<> if E-WS-RSV throw then
   b OPCODE-BITS and WIRE>OPCODE {: op :}
   b FIN-BIT and 0<> {: fin:bool :}
   op CONTROL? fin 0= and if E-WS-CONTROL throw then
   fin op ;


\ The second byte: the mask bit, which this sender must set or clear, and the
\ 7-bit length, at most 125 on a control frame.
: SECOND-BYTE ( n sender opcode -- bool n ) {: b:n who op :}
   b MASK-BIT and 0<> {: masked:bool :}
   masked who MASKS? xor if E-WS-MASK throw then
   b LEN7-BITS and {: len7:n :}
   op CONTROL? len7 LEN7-MAX > and if E-WS-CONTROL throw then
   masked len7 ;

public


\ Write a header as this sender sends it and answer its size, 2 to HEADER-MAX.
: ENCODE-HEADER ( header sender SPAN:span<u8> -- n ) {: h who s :}
   h WS-HEADER:UNMAKE {: fin:bool op len:n key:n :}
   op CONTROL? if
      fin 0= len LEN7-MAX > or if E-WS-CONTROL throw then
   then
   len 0 < if E-WS-LENGTH throw then
   who MASKS? {: masked:bool :}
   masked if key 0 < key KEY-MAX > or else key 0<> then
   if E-WS-MASK throw then
   len EXT-BYTES {: ext:n :}
   2 ext + masked KEY-SIZE + {: size:n :}
   s SPAN:$ size < if E-SPAN-CAPACITY throw then {: out :}
   fin if FIN-BIT else 0 then op OPCODE>WIRE or out c!
   masked if MASK-BIT else 0 then len LEN7 or out 1+ c!
   len out 2 + ext BE!
   key out 2 + ext + masked KEY-SIZE BE!
   size ;


\ Read the header at the start of the n bytes held, sent by this sender, whose
\ payload may be no longer than the bound.
: DECODE-HEADER ( ptr u8 n sender n -- decoded ) {: p held:n who max:n :}
   held 0 < if E-SPAN-LENGTH throw then
   held 1 < if 2 WS-DECODED:need exit then
   p c@ FIRST-BYTE {: fin:bool op :}
   held 2 < if 2 WS-DECODED:need exit then
   p 1+ c@ who op SECOND-BYTE {: masked:bool len7:n :}
   len7 EXT-OF {: ext:n :}
   2 ext + masked KEY-SIZE + {: size:n :}
   held 2 ext + < if size WS-DECODED:need exit then
   ext 0= if len7 else p 2 + ext BE@ then {: len:n :}
   len 0 < len EXT-BYTES ext <> or if E-WS-LENGTH throw then
   len max > if E-WS-TOO-BIG throw then
   held size < if size WS-DECODED:need exit then
   fin op len p 2 + ext + masked KEY-SIZE BE@ WS-HEADER:MAKE
   size WS-DECODED:frame ;


\ XOR each payload byte with the key's byte at its offset mod 4, the key's high
\ byte first (section 5.3).
: MASK ( n SPAN:span<u8> -- ) {: key:n s :}
   key 0 < key KEY-MAX > or if E-WS-MASK throw then
   s SPAN:$ {: p u:n :}
   u 0 ?do
      p i + c@ key 3 i 3 and - 8 * rshift xor $FF and p i + c!
   loop ;


\ The status code, high byte first, as a close frame's payload begins.
: CLOSE-CODE! ( close-code SPAN:span<u8> -- ) {: code s :}
   s SPAN:$ CODE-BYTES < if E-SPAN-CAPACITY throw then {: out :}
   code CLOSE-CODE>N out CODE-BYTES BE! ;

: CLOSE-NORMAL ( -- close-code ) 1000 >CLOSE-CODE ;
: CLOSE-PROTOCOL ( -- close-code ) 1002 >CLOSE-CODE ;
: CLOSE-INVALID-DATA ( -- close-code ) 1007 >CLOSE-CODE ;
: CLOSE-TOO-BIG ( -- close-code ) 1009 >CLOSE-CODE ;

;package
