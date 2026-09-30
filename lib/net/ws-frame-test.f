\ ws-frame-test.f - the RFC 6455 frame codec: the examples of section 5.7 byte
\ for byte, every length form at its edges, headers from both senders round
\ trip, a header still short of its bytes, and every refusal.
\ Run: bin/hb --load lib/net/ws-frame-test.f
\
\ Frames are written in hex, two lowercase digits a byte, as section 5.7 prints
\ them. A decoded header is checked by encoding it again: the encoder is pinned
\ byte for byte by the RFC's examples, so a header that encodes back to the
\ bytes it came from was read field for field.

require lib/test.f
require lib/span.f
require lib/net/ws-frame.f

package WS-FRAME-TEST

$A5 constant CANARY
$37FA213D constant RFC-KEY        \ the masking key of section 5.7's examples
$7FFFFFFFFFFFFFFF constant LEN-MAX \ the longest 64-bit length, its high bit clear

WS:HEADER-MAX SPAN-BUFFER: OUT
32 SPAN-BUFFER: HEX
16 SPAN-BUFFER: STAGED
variable STAGED-LEN
variable BOUND
TYPED-VARIABLE WHO WS:sender

: OUT$ ( n -- ptr u8 n )
   OUT SPAN:$ drop swap ;

: HEX$ ( ptr u8 n -- ptr u8 n ) {: a u:n :}
   HEX u 2 * SPAN:TAKE SPAN:$ {: h hu:n :}
   u 0 ?do a i + c@ h i 2 * + BYTE>HEX loop
   h hu ;

: NIBBLE ( n -- n ) {: c:n :}
   c [char] a >= if c [char] a - $0A + exit then
   c [char] 0 - ;

\ A case's bytes are staged, with the sender and the bound, because a refusal
\ runs in a quotation and a quotation sees no locals.
: STAGE ( ptr u8 n WS:sender n -- ) {: a u:n who max:n :}
   u 2 / {: k:n :}
   k 0 ?do
      a i 2 * + c@ NIBBLE 4 lshift a i 2 * + 1+ c@ NIBBLE or $FF and
      STAGED i SPAN:U8!
   loop
   k STAGED-LEN !
   who WHO !
   max BOUND ! ;

: STAGED$ ( -- ptr u8 n )
   STAGED SPAN:$ drop STAGED-LEN @ ;

: DECODE-STAGED ( -- WS:decoded )
   STAGED$ WHO @ BOUND @ WS:DECODE-HEADER ;

\ Write a header into OUT and forget the count: the body of a refusal.
: PUT ( WS:header WS:sender -- )
   OUT WS:ENCODE-HEADER drop ;

: ENCODES ( WS:header WS:sender ptr u8 n -- ) {: h who want wu:n :}
   want wu T-LABEL
   h who OUT WS:ENCODE-HEADER OUT$ HEX$ want wu T$= ;

\ The bytes decode, under the bound, to a frame whose header takes `size` bytes
\ and encodes back to them.
: DECODES ( ptr u8 n WS:sender n n -- ) {: a u:n who max:n size:n :}
   a u who max STAGE
   a u T-LABEL
   DECODE-STAGED MATCH WS:decoded
      need OF drop s" a need, not a frame" T-FAIL-AS ENDOF
      frame OF {: h got:n :}
         got size T=
         a u T-LABEL
         h who OUT WS:ENCODE-HEADER OUT$ HEX$ a size 2 * T$=
      ENDOF
   ;MATCH ;

\ The bytes are a header's first part, and the whole header takes `want`.
: NEEDS ( ptr u8 n WS:sender n -- ) {: a u:n who want:n :}
   a u who LEN-MAX STAGE
   a u T-LABEL
   DECODE-STAGED MATCH WS:decoded
      need OF want T= ENDOF
      frame OF drop drop s" a frame, not a need" T-FAIL-AS ENDOF
   ;MATCH ;

: REFUSES ( ptr u8 n WS:sender n n -- ) {: a u:n who max:n code:n :}
   a u who max STAGE
   a u T-LABEL
   [: DECODE-STAGED drop ;] code TTHROWSQ ;

\ Section 5.7's frames, written.
: RFC-ENCODE ( -- )
   true WS-OPCODE:text 5 0 WS-HEADER:MAKE WS-SENDER:server s" 8105" ENCODES
   true WS-OPCODE:text 5 RFC-KEY WS-HEADER:MAKE WS-SENDER:client s" 818537fa213d" ENCODES
   false WS-OPCODE:text 3 0 WS-HEADER:MAKE WS-SENDER:server s" 0103" ENCODES
   true WS-OPCODE:continuation 2 0 WS-HEADER:MAKE WS-SENDER:server s" 8002" ENCODES
   true WS-OPCODE:ping 5 0 WS-HEADER:MAKE WS-SENDER:server s" 8905" ENCODES
   true WS-OPCODE:pong 5 RFC-KEY WS-HEADER:MAKE WS-SENDER:client s" 8a8537fa213d" ENCODES
   true WS-OPCODE:binary 256 0 WS-HEADER:MAKE WS-SENDER:server s" 827e0100" ENCODES
   true WS-OPCODE:binary 65536 0 WS-HEADER:MAKE WS-SENDER:server
   s" 827f0000000000010000" ENCODES ;

\ Section 5.7's frames, read with their payloads behind them.
: RFC-DECODE ( -- )
   s" 810548656c6c6f" WS-SENDER:server LEN-MAX 2 DECODES
   s" 818537fa213d7f9f4d5158" WS-SENDER:client LEN-MAX 6 DECODES
   s" 010348656c" WS-SENDER:server LEN-MAX 2 DECODES
   s" 80026c6f" WS-SENDER:server LEN-MAX 2 DECODES
   s" 890548656c6c6f" WS-SENDER:server LEN-MAX 2 DECODES
   s" 8a8537fa213d7f9f4d5158" WS-SENDER:client LEN-MAX 6 DECODES
   s" 827e0100" WS-SENDER:server LEN-MAX 4 DECODES
   s" 827f0000000000010000" WS-SENDER:server LEN-MAX 10 DECODES ;

\ Masking "Hello" with section 5.7's key gives the payload it prints, and
\ masking again gives "Hello" back.
: MASKS ( -- )
   s" Hello" STAGED SPAN:COPY
   RFC-KEY STAGED 5 SPAN:TAKE WS:MASK
   s" Hello masked" T-LABEL
   STAGED SPAN:$ drop 5 HEX$ s" 7f9f4d5158" T$=
   RFC-KEY STAGED 5 SPAN:TAKE WS:MASK
   s" and masked again" T-LABEL
   STAGED SPAN:$ drop 5 s" Hello" T$=
   s" a negative key" T-LABEL
   [: -1 STAGED WS:MASK ;] E-WS-MASK TTHROWSQ
   s" a key past 32 bits" T-LABEL
   [: $100000000 STAGED WS:MASK ;] E-WS-MASK TTHROWSQ ;

\ The masked "Hello" frame carries the key that unmasks its own payload.
: UNMASKS ( -- )
   s" 818537fa213d7f9f4d5158" WS-SENDER:client LEN-MAX STAGE
   s" the masked Hello frame" T-LABEL
   DECODE-STAGED MATCH WS:decoded
      need OF drop s" a need, not a frame" T-FAIL-AS ENDOF
      frame OF {: h size:n :}
         h WS-HEADER:UNMAKE {: fin:bool op len:n key:n :}
         key STAGED size len SPAN:SUB WS:MASK
         STAGED size len SPAN:SUB SPAN:$ s" Hello" T$=
      ENDOF
   ;MATCH ;

\ Each length at the edge of its form, both ways; the 64-bit form up to the
\ largest length its clear high bit allows.
: LENGTHS ( -- )
   true WS-OPCODE:binary 125 0 WS-HEADER:MAKE WS-SENDER:server s" 827d" ENCODES
   true WS-OPCODE:binary 126 0 WS-HEADER:MAKE WS-SENDER:server s" 827e007e" ENCODES
   true WS-OPCODE:binary 65535 0 WS-HEADER:MAKE WS-SENDER:server s" 827effff" ENCODES
   true WS-OPCODE:binary $123456789A 0 WS-HEADER:MAKE WS-SENDER:server
   s" 827f000000123456789a" ENCODES
   true WS-OPCODE:binary LEN-MAX 0 WS-HEADER:MAKE WS-SENDER:server
   s" 827f7fffffffffffffff" ENCODES
   s" 827d" WS-SENDER:server LEN-MAX 2 DECODES
   s" 827e007e" WS-SENDER:server LEN-MAX 4 DECODES
   s" 827effff" WS-SENDER:server LEN-MAX 4 DECODES
   s" 827f000000123456789a" WS-SENDER:server LEN-MAX 10 DECODES
   s" 827f7fffffffffffffff" WS-SENDER:server LEN-MAX 10 DECODES ;

\ A header written for a sender reads back from exactly the bytes written, and
\ encodes to them again.
: TRIP ( WS:header WS:sender -- ) {: h who :}
   h who OUT WS:ENCODE-HEADER {: n:n :}
   n OUT$ STAGED SPAN:COPY
   n STAGED-LEN !
   who WHO !
   LEN-MAX BOUND !
   STAGED$ HEX$ T-LABEL
   DECODE-STAGED MATCH WS:decoded
      need OF drop s" a need, not a frame" T-FAIL-AS ENDOF
      frame OF {: back got:n :}
         got n T=
         STAGED$ HEX$ T-LABEL
         back who OUT WS:ENCODE-HEADER OUT$ STAGED$ T$=
      ENDOF
   ;MATCH ;

\ The same header from a server, unmasked, and from a client under the key.
: TRIPS ( bool WS:opcode n n -- ) {: fin:bool op len:n key:n :}
   fin op len 0 WS-HEADER:MAKE WS-SENDER:server TRIP
   fin op len key WS-HEADER:MAKE WS-SENDER:client TRIP ;

: ROUND-TRIP ( -- )
   true WS-OPCODE:text 0 0 TRIPS
   false WS-OPCODE:text 125 $FFFFFFFF TRIPS
   true WS-OPCODE:continuation 126 $01020304 TRIPS
   false WS-OPCODE:continuation 65535 RFC-KEY TRIPS
   true WS-OPCODE:binary 65536 $80000001 TRIPS
   false WS-OPCODE:binary LEN-MAX $00FF00FF TRIPS
   true WS-OPCODE:close 125 $DEADBEEF TRIPS
   true WS-OPCODE:ping 0 1 TRIPS
   true WS-OPCODE:pong 1 $7FFFFFFF TRIPS ;

\ Too few bytes answer the count the whole header takes: 2 until the second
\ byte says how long the rest is.
: SHORT ( -- )
   s" " WS-SENDER:server 2 NEEDS
   s" 81" WS-SENDER:server 2 NEEDS
   s" 827e" WS-SENDER:server 4 NEEDS
   s" 827e01" WS-SENDER:server 4 NEEDS
   s" 827f" WS-SENDER:server 10 NEEDS
   s" 827f00000000000001" WS-SENDER:server 10 NEEDS
   s" 8185" WS-SENDER:client 6 NEEDS
   s" 818537fa21" WS-SENDER:client 6 NEEDS
   s" 82fe0100" WS-SENDER:client 8 NEEDS
   s" 82ff" WS-SENDER:client 14 NEEDS
   s" 82ff0000000000010000" WS-SENDER:client 14 NEEDS
   s" a negative count" T-LABEL
   [: 0 OUT$ drop -1 WS-SENDER:server LEN-MAX WS:DECODE-HEADER drop ;]
   E-SPAN-LENGTH TTHROWSQ ;

: RESERVED-OPCODE ( n -- ) {: b:n :}
   b $FF and STAGED 0 SPAN:U8!
   1 STAGED-LEN !
   WS-SENDER:server WHO !
   LEN-MAX BOUND !
   s" a reserved opcode" T-LABEL
   [: DECODE-STAGED drop ;] E-WS-OPCODE TTHROWSQ ;

\ Each refusal is made from the fewest bytes that show it.
: FIRST-BYTE ( -- )
   s" c1" WS-SENDER:server LEN-MAX E-WS-RSV REFUSES
   s" a1" WS-SENDER:server LEN-MAX E-WS-RSV REFUSES
   s" 91" WS-SENDER:server LEN-MAX E-WS-RSV REFUSES
   $88 $83 do i RESERVED-OPCODE loop
   $90 $8B do i RESERVED-OPCODE loop
   s" 08" WS-SENDER:server LEN-MAX E-WS-CONTROL REFUSES
   s" 09" WS-SENDER:server LEN-MAX E-WS-CONTROL REFUSES
   s" 0a" WS-SENDER:server LEN-MAX E-WS-CONTROL REFUSES ;

: SECOND-BYTE ( -- )
   s" 8105" WS-SENDER:client LEN-MAX E-WS-MASK REFUSES
   s" 8185" WS-SENDER:server LEN-MAX E-WS-MASK REFUSES
   s" 82fe" WS-SENDER:server LEN-MAX E-WS-MASK REFUSES
   s" 897e" WS-SENDER:server LEN-MAX E-WS-CONTROL REFUSES
   s" 887f" WS-SENDER:server LEN-MAX E-WS-CONTROL REFUSES
   s" 89fe" WS-SENDER:client LEN-MAX E-WS-CONTROL REFUSES
   s" 897d" WS-SENDER:server LEN-MAX 2 DECODES ;

\ A length in a longer form than it needs, or with the 64-bit high bit set, is
\ refused as soon as its bytes are held, before the key.
: NOT-MINIMAL ( -- )
   s" 827e0000" WS-SENDER:server LEN-MAX E-WS-LENGTH REFUSES
   s" 827e007d" WS-SENDER:server LEN-MAX E-WS-LENGTH REFUSES
   s" 827f000000000000007e" WS-SENDER:server LEN-MAX E-WS-LENGTH REFUSES
   s" 827f000000000000ffff" WS-SENDER:server LEN-MAX E-WS-LENGTH REFUSES
   s" 827f8000000000000000" WS-SENDER:server LEN-MAX E-WS-LENGTH REFUSES
   s" 827fffffffffffffffff" WS-SENDER:server LEN-MAX E-WS-LENGTH REFUSES
   s" 82fe007d" WS-SENDER:client LEN-MAX E-WS-LENGTH REFUSES ;

\ A payload of exactly the bound is taken and one byte more is refused, in each
\ length form, and before the key.
: AT-BOUND ( -- )
   s" 8205" WS-SENDER:server 5 2 DECODES
   s" 8205" WS-SENDER:server 4 E-WS-TOO-BIG REFUSES
   s" 827e0100" WS-SENDER:server 256 4 DECODES
   s" 827e0100" WS-SENDER:server 255 E-WS-TOO-BIG REFUSES
   s" 827f0000000000010000" WS-SENDER:server 65536 10 DECODES
   s" 827f0000000000010000" WS-SENDER:server 65535 E-WS-TOO-BIG REFUSES
   s" 82fe0100" WS-SENDER:client 255 E-WS-TOO-BIG REFUSES ;

\ What a sender may not write: a fragmented or long control frame, a negative
\ length, a key on a server's frame or a key past 32 bits.
: ENCODE-REFUSALS ( -- )
   s" a fragmented ping" T-LABEL
   [: false WS-OPCODE:ping 0 0 WS-HEADER:MAKE WS-SENDER:server PUT ;]
   E-WS-CONTROL TTHROWSQ
   s" a close of 126 bytes" T-LABEL
   [: true WS-OPCODE:close 126 0 WS-HEADER:MAKE WS-SENDER:server PUT ;]
   E-WS-CONTROL TTHROWSQ
   true WS-OPCODE:close 125 0 WS-HEADER:MAKE WS-SENDER:server s" 887d" ENCODES
   s" a negative length" T-LABEL
   [: true WS-OPCODE:binary -1 0 WS-HEADER:MAKE WS-SENDER:server PUT ;]
   E-WS-LENGTH TTHROWSQ
   s" a key on a server frame" T-LABEL
   [: true WS-OPCODE:text 0 1 WS-HEADER:MAKE WS-SENDER:server PUT ;]
   E-WS-MASK TTHROWSQ
   s" a negative key" T-LABEL
   [: true WS-OPCODE:text 0 -1 WS-HEADER:MAKE WS-SENDER:client PUT ;]
   E-WS-MASK TTHROWSQ
   s" a key past 32 bits" T-LABEL
   [: true WS-OPCODE:text 0 $100000000 WS-HEADER:MAKE WS-SENDER:client PUT ;]
   E-WS-MASK TTHROWSQ ;

\ A span short of the header is refused before a byte is written, and an exact
\ fit is written whole.
: CAPACITY ( -- )
   CANARY OUT SPAN:FILL
   s" a 14-byte header in 13 bytes" T-LABEL
   [: true WS-OPCODE:binary 65536 RFC-KEY WS-HEADER:MAKE WS-SENDER:client
      OUT 13 SPAN:TAKE WS:ENCODE-HEADER drop ;]
   E-SPAN-CAPACITY TTHROWSQ
   s" writes none of it" T-LABEL
   OUT 0 SPAN:U8@ CANARY T=
   s" and fills 14 exactly" T-LABEL
   true WS-OPCODE:binary 65536 RFC-KEY WS-HEADER:MAKE WS-SENDER:client
   OUT WS:ENCODE-HEADER WS:HEADER-MAX T=
   s" a close code in one byte" T-LABEL
   [: WS:CLOSE-NORMAL OUT 1 SPAN:TAKE WS:CLOSE-CODE! ;] E-SPAN-CAPACITY TTHROWSQ ;

\ A server's normal close is 88 02 03 e8, and each status is its RFC number.
: CLOSES ( -- )
   true WS-OPCODE:close 2 0 WS-HEADER:MAKE WS-SENDER:server OUT WS:ENCODE-HEADER
   {: n:n :}
   WS:CLOSE-NORMAL OUT n SPAN:SKIP WS:CLOSE-CODE!
   s" a normal close" T-LABEL
   n 2 + OUT$ HEX$ s" 880203e8" T$=
   WS:CLOSE-PROTOCOL OUT WS:CLOSE-CODE!
   s" 1002" T-LABEL
   2 OUT$ HEX$ s" 03ea" T$=
   WS:CLOSE-INVALID-DATA OUT WS:CLOSE-CODE!
   s" 1007" T-LABEL
   2 OUT$ HEX$ s" 03ef" T$=
   WS:CLOSE-TOO-BIG OUT WS:CLOSE-CODE!
   s" 1009" T-LABEL
   2 OUT$ HEX$ s" 03f1" T$= ;

public

: RUN ( -- )
   T-RESET
   RFC-ENCODE RFC-DECODE MASKS UNMASKS LENGTHS ROUND-TRIP SHORT
   FIRST-BYTE SECOND-BYTE NOT-MINIMAL AT-BOUND ENCODE-REFUSALS CAPACITY CLOSES ;

;package

WS-FRAME-TEST:RUN
T-REPORT
