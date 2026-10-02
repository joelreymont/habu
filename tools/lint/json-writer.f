\ json-writer.f - small emit-only JSON writer for native lints. This is
\ intentionally smaller than tools/json.f.

require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f

32 constant LJW-NUM-CAP

8 constant LJW-BS
9 constant LJW-TAB
10 constant LJW-LF
12 constant LJW-FF
13 constant LJW-CR
32 constant LJW-SP
34 constant LJW-DQ
44 constant LJW-COMMA-C
48 constant LJW-ZERO
58 constant LJW-COLON-C
92 constant LJW-BACKSLASH
123 constant LJW-LBRACE
125 constant LJW-RBRACE

\ A packet is as long as the strings it carries, escaped: a token, a definition
\ or a path from a source of whatever size its caller reads. The buffer grows
\ to the packet, so no packet is refused for its length.
DYNAMIC-BUFFER LJW-BUF u8
create LJW-NUM-BUF LJW-NUM-CAP allot

variable LJW-LEN
variable LJW-NUM-I

: LJW-RESET ( -- )
   0 LJW-LEN ! ;

\ The packet's first byte, with room for n more after the packet. Growth may
\ move the buffer, so every address is taken after the reserve. Byte 0 needs a
\ capacity of one even for an empty packet.
: LJW-ROOM ( n -- ptr u8 )
   LJW-LEN @ + 1 max LJW-BUF-RESERVE
   0 LJW-BUF ;

: LJW-C ( n -- ) {: c:n :}
   c 1 LJW-ROOM LJW-LEN @ + c!
   LJW-LEN @ 1+ LJW-LEN ! ;

: LJW-RAW ( ptr u8 n -- ) {: a:ptr u:n :}
   a u LJW-ROOM LJW-LEN @ + u LINT-BMOVE
   LJW-LEN @ u + LJW-LEN ! ;

: LJW-HEX ( n -- u8 )
   dup 10 < IF LJW-ZERO + ELSE 55 + THEN ;

: LJW-U00 ( n -- )
   LJW-BACKSLASH LJW-C
   117 LJW-C
   LJW-ZERO LJW-C
   LJW-ZERO LJW-C
   dup 4 rshift LJW-HEX LJW-C
   $F and LJW-HEX LJW-C ;

: LJW-ESC-C ( n -- ) {: c:n :}
   c LJW-DQ = IF LJW-BACKSLASH LJW-C LJW-DQ LJW-C exit THEN
   c LJW-BACKSLASH = IF LJW-BACKSLASH LJW-C LJW-BACKSLASH LJW-C exit THEN
   c LJW-BS = IF LJW-BACKSLASH LJW-C 98 LJW-C exit THEN
   c LJW-FF = IF LJW-BACKSLASH LJW-C 102 LJW-C exit THEN
   c LJW-LF = IF LJW-BACKSLASH LJW-C 110 LJW-C exit THEN
   c LJW-CR = IF LJW-BACKSLASH LJW-C 114 LJW-C exit THEN
   c LJW-TAB = IF LJW-BACKSLASH LJW-C 116 LJW-C exit THEN
   c LJW-SP < IF c LJW-U00 exit THEN
   c LJW-C ;

: LJW-STRING ( ptr u8 n -- ) {: a:ptr u:n :}
   LJW-DQ LJW-C
   0 begin dup u < while
      dup a + c@ LJW-ESC-C
      1+
   repeat drop
   LJW-DQ LJW-C ;

: LJW-KEY ( ptr u8 n -- )
   LJW-STRING
   LJW-COLON-C LJW-C ;

: LJW-COMMA ( -- )
   LJW-COMMA-C LJW-C ;

: LJW-OBJECT-START ( -- )
   LJW-LBRACE LJW-C ;

: LJW-OBJECT-END ( -- )
   LJW-RBRACE LJW-C ;

: LJW-U ( n -- ) {: u:n :}
   LJW-NUM-CAP LJW-NUM-I !
   u 0= IF
      LJW-ZERO LJW-C
      exit
   THEN
   u begin dup 0 > while
      dup 10 mod LJW-ZERO +
      LJW-NUM-I @ 1- LJW-NUM-I !
      LJW-NUM-BUF LJW-NUM-I @ + c!
      10 /
   repeat drop
   LJW-NUM-BUF LJW-NUM-I @ + LJW-NUM-CAP LJW-NUM-I @ - LJW-RAW ;

\ Division truncates toward zero, so a negative value's last digit is the
\ negated remainder and the rest is the negated quotient; MIN-N, which has no
\ positive form, needs no negate of its own.
: LJW-INT ( n -- ) {: v:n :}            \ a signed integer
   v 0 < 0= IF v LJW-U exit THEN
   $2d LJW-C
   v 10 / negate {: high:n :}
   high 0 > IF high LJW-U THEN
   LJW-ZERO v 10 mod - LJW-C ;

: LJW$ ( -- ptr u8 n )
   0 LJW-ROOM LJW-LEN @ ;
