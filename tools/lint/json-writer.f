\ json-writer.f - small emit-only JSON writer for native lints.
\ Load after tools/lint/text.f, tools/lint/token.f, and tools/lint/lib.f. This is intentionally smaller than tools/json.f.

\ The character constants all carry a -C tail so no member shadows a word its
\ siblings call: a package member is visible to every member defined after it,
\ so a constant named CR would silently replace the core newline word.
package JSON-WRITE

$4000 constant CAP
32 constant NUM-CAP

8 constant BS-C
9 constant TAB-C
10 constant LF-C
12 constant FF-C
13 constant CR-C
32 constant SP-C
34 constant DQ-C
44 constant COMMA-C
48 constant ZERO-C
58 constant COLON-C
92 constant BACKSLASH-C
123 constant LBRACE-C
125 constant RBRACE-C

create BUF CAP allot
create NUM-BUF NUM-CAP allot

variable LEN
variable NUM-I

: PUT ( n -- ) {: c:n :}
   LEN @ 1+ CAP > IF s" lint-json: buffer overflow" 76 die THEN
   c BUF LEN @ + c!
   LEN @ 1+ LEN ! ;

: RAW ( ptr u8 n -- ) {: a:ptr u:n :}
   LEN @ u + CAP > IF s" lint-json: buffer overflow" 76 die THEN
   a BUF LEN @ + u LINT-BMOVE
   LEN @ u + LEN ! ;

\ One nibble to its ASCII hex digit. Both are plain byte values: the input is
\ masked to 0..15 by the caller, the result is what PUT stores.
: HEX ( n -- n )
   dup 10 < IF ZERO-C + ELSE 55 + THEN ;

: U00 ( n -- )
   BACKSLASH-C PUT
   117 PUT
   ZERO-C PUT
   ZERO-C PUT
   dup 4 rshift HEX PUT
   $F and HEX PUT ;

: ESC-C ( n -- ) {: c:n :}
   c DQ-C = IF BACKSLASH-C PUT DQ-C PUT exit THEN
   c BACKSLASH-C = IF BACKSLASH-C PUT BACKSLASH-C PUT exit THEN
   c BS-C = IF BACKSLASH-C PUT 98 PUT exit THEN
   c FF-C = IF BACKSLASH-C PUT 102 PUT exit THEN
   c LF-C = IF BACKSLASH-C PUT 110 PUT exit THEN
   c CR-C = IF BACKSLASH-C PUT 114 PUT exit THEN
   c TAB-C = IF BACKSLASH-C PUT 116 PUT exit THEN
   c SP-C < IF c U00 exit THEN
   c PUT ;

public

: RESET ( -- )
   0 LEN ! ;

: STRING ( ptr u8 n -- ) {: a:ptr u:n :}
   DQ-C PUT
   0 begin dup u < while
      dup a + c@ ESC-C
      1+
   repeat drop
   DQ-C PUT ;

: KEY ( ptr u8 n -- )
   STRING
   COLON-C PUT ;

: COMMA ( -- )
   COMMA-C PUT ;

: OBJECT-START ( -- )
   LBRACE-C PUT ;

: OBJECT-END ( -- )
   RBRACE-C PUT ;

: U ( n -- ) {: u:n :}
   NUM-CAP NUM-I !
   u 0= IF
      ZERO-C PUT
      exit
   THEN
   u begin dup 0 > while
      dup 10 mod ZERO-C +
      NUM-I @ 1- NUM-I !
      NUM-BUF NUM-I @ + c!
      10 /
   repeat drop
   NUM-BUF NUM-I @ + NUM-CAP NUM-I @ - RAW ;

: $ ( -- ptr u8 n )
   BUF LEN @ ;

;package
