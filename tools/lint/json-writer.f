\ json-writer.f - small emit-only JSON writer for native lints.
\ Load after tools/lint/text.f, tools/lint/token.f, and tools/lint/lib.f. This is intentionally smaller than tools/json.f.

0 set-check

$4000 constant LJW-CAP
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

create LJW-BUF LJW-CAP allot
create LJW-NUM-BUF LJW-NUM-CAP allot

variable LJW-LEN
variable LJW-NUM-I

: LJW-RESET ( -- )
   0 LJW-LEN ! ;

: LJW-C {: c :} ( c -- )
   LJW-LEN @ 1+ LJW-CAP > IF s" lint-json: buffer overflow" 76 die THEN
   c LJW-BUF LJW-LEN @ + c!
   LJW-LEN @ 1+ LJW-LEN ! ;

: LJW-RAW {: a u :} ( a u -- )
   LJW-LEN @ u + LJW-CAP > IF s" lint-json: buffer overflow" 76 die THEN
   a LJW-BUF LJW-LEN @ + u BMOVE
   LJW-LEN @ u + LJW-LEN ! ;

: LJW-HEX ( n -- c )
   dup 10 < IF LJW-ZERO + ELSE 55 + THEN ;

: LJW-U00 ( c -- )
   LJW-BACKSLASH LJW-C
   117 LJW-C
   LJW-ZERO LJW-C
   LJW-ZERO LJW-C
   dup 4 rshift LJW-HEX LJW-C
   $F and LJW-HEX LJW-C ;

: LJW-ESC-C {: c :} ( c -- )
   c LJW-DQ = IF LJW-BACKSLASH LJW-C LJW-DQ LJW-C exit THEN
   c LJW-BACKSLASH = IF LJW-BACKSLASH LJW-C LJW-BACKSLASH LJW-C exit THEN
   c LJW-BS = IF LJW-BACKSLASH LJW-C 98 LJW-C exit THEN
   c LJW-FF = IF LJW-BACKSLASH LJW-C 102 LJW-C exit THEN
   c LJW-LF = IF LJW-BACKSLASH LJW-C 110 LJW-C exit THEN
   c LJW-CR = IF LJW-BACKSLASH LJW-C 114 LJW-C exit THEN
   c LJW-TAB = IF LJW-BACKSLASH LJW-C 116 LJW-C exit THEN
   c LJW-SP < IF c LJW-U00 exit THEN
   c LJW-C ;

: LJW-STRING {: a u :} ( a u -- )
   LJW-DQ LJW-C
   0 begin dup u < while
      dup a + c@ LJW-ESC-C
      1+
   repeat drop
   LJW-DQ LJW-C ;

: LJW-KEY ( a u -- )
   LJW-STRING
   LJW-COLON-C LJW-C ;

: LJW-COMMA ( -- )
   LJW-COMMA-C LJW-C ;

: LJW-OBJECT-START ( -- )
   LJW-LBRACE LJW-C ;

: LJW-OBJECT-END ( -- )
   LJW-RBRACE LJW-C ;

: LJW-U {: u :} ( u -- )
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

: LJW$ ( -- a u )
   LJW-BUF LJW-LEN @ ;
