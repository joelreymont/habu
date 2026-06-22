\ json-write.f - checked emit-only JSON writer.
\
\ Load after lib/errors.f, lib/string.f, and lib/memory.f.

1 constant JW-MIN-CAP
32 constant JW-NUM-CAP

8 constant JW-BS
9 constant JW-TAB
10 constant JW-LF
12 constant JW-FF
13 constant JW-CR
32 constant JW-SP
34 constant JW-DQ
44 constant JW-COMMA-C
48 constant JW-ZERO
58 constant JW-COLON-C
91 constant JW-LBRACK
92 constant JW-BACKSLASH
93 constant JW-RBRACK
123 constant JW-LBRACE
125 constant JW-RBRACE
255 constant JW-BYTE-MAX

create JW-NUM-BUF JW-NUM-CAP allot

variable JW-BUF-A
variable JW-BUF-CAP
variable JW-LEN
variable JW-NUM-I

TRUSTED: JW-BUF ( -- ptr u8 )
   JW-BUF-A @ ;

: JW-CAP ( -- n )
   JW-BUF-CAP @ ;

: JW-STORE-SPAN ( ptr u8 n -- )
   JW-BUF-CAP ! JW-BUF-A ! ;

: JW-MIN-ONE ( n -- n )
   dup JW-MIN-CAP < if drop JW-MIN-CAP then ;

: JW-NEED-CAP ( n -- n ) {: add :}
   add 0 < if E-JW-CAPACITY throw then
   JW-LEN @ 0 < if E-JW-CAPACITY throw then
   add MEM-MAX-N JW-LEN @ - >= if E-JW-CAPACITY throw then
   JW-LEN @ add + ;

: JW-COPY-OLD ( ptr u8 -- ) {: dst:ptr :}
   JW-LEN @ 0 > if JW-BUF dst JW-LEN @ BYTE-COPY then ;

: JW-GROW ( n -- ) {: need :}
   need JW-MIN-ONE MEM-ALLOC-64K-SPAN
   over JW-COPY-OLD
   JW-STORE-SPAN ;

: JW-CHECK-ROOM ( n -- )
   JW-NEED-CAP dup JW-CAP > if JW-GROW else drop then ;

: JW-ENSURE-INITIAL ( -- )
   JW-CAP 0= if JW-MIN-CAP JW-GROW then ;

: JW-RESET ( -- )
   JW-ENSURE-INITIAL
   0 JW-LEN ! ;

: JW-C ( n -- ) {: c :}
   c 0 < if E-JW-BYTE throw then
   c JW-BYTE-MAX > if E-JW-BYTE throw then
   1 JW-CHECK-ROOM
   c JW-BUF JW-LEN @ + c!
   JW-LEN @ 1+ JW-LEN ! ;

: JW-RAW ( ptr u8 n -- ) {: a:ptr u :}
   u JW-CHECK-ROOM
   a JW-BUF JW-LEN @ + u BYTE-COPY
   JW-LEN @ u + JW-LEN ! ;

: JW-HEX ( n -- n )
   dup 10 < if JW-ZERO + else 55 + then ;

: JW-U00 ( n -- ) {: c :}
   JW-BACKSLASH JW-C
   117 JW-C
   JW-ZERO JW-C
   JW-ZERO JW-C
   c 4 rshift JW-HEX JW-C
   c $F and JW-HEX JW-C ;

: JW-ESC-C ( n -- ) {: c :}
   c JW-DQ = if JW-BACKSLASH JW-C JW-DQ JW-C exit then
   c JW-BACKSLASH = if JW-BACKSLASH JW-C JW-BACKSLASH JW-C exit then
   c JW-BS = if JW-BACKSLASH JW-C 98 JW-C exit then
   c JW-FF = if JW-BACKSLASH JW-C 102 JW-C exit then
   c JW-LF = if JW-BACKSLASH JW-C 110 JW-C exit then
   c JW-CR = if JW-BACKSLASH JW-C 114 JW-C exit then
   c JW-TAB = if JW-BACKSLASH JW-C 116 JW-C exit then
   c JW-SP < if c JW-U00 exit then
   c JW-C ;

: JW-STRING ( ptr u8 n -- ) {: a:ptr u :}
   JW-DQ JW-C
   0 begin dup u < while
      dup a + c@ JW-ESC-C
      1+
   repeat drop
   JW-DQ JW-C ;

: JW-KEY ( ptr u8 n -- )
   JW-STRING
   JW-COLON-C JW-C ;

: JW-OBJECT-START ( -- )
   JW-LBRACE JW-C ;

: JW-OBJECT-END ( -- )
   JW-RBRACE JW-C ;

: JW-ARRAY-START ( -- )
   JW-LBRACK JW-C ;

: JW-ARRAY-END ( -- )
   JW-RBRACK JW-C ;

: JW-COMMA ( -- )
   JW-COMMA-C JW-C ;

: JW-NULL ( -- )
   s" null" JW-RAW ;

: JW-BOOL ( bool -- )
   if s" true" else s" false" then JW-RAW ;

: JW-U ( n -- ) {: u :}
   u 0 < if E-JW-BYTE throw then
   JW-NUM-CAP JW-NUM-I !
   u 0= if JW-ZERO JW-C exit then
   u begin dup 0 > while
      dup 10 mod JW-ZERO +
      JW-NUM-I @ 1- JW-NUM-I !
      JW-NUM-BUF JW-NUM-I @ + c!
      10 /
   repeat drop
   JW-NUM-BUF JW-NUM-I @ + JW-NUM-CAP JW-NUM-I @ - JW-RAW ;

: JW-FIELD-RAW ( ptr u8 n ptr u8 n -- ) {: key:ptr keyu val:ptr valu :}
   key keyu JW-KEY
   val valu JW-RAW ;

: JW-FIELD-S ( ptr u8 n ptr u8 n -- ) {: key:ptr keyu val:ptr valu :}
   key keyu JW-KEY
   val valu JW-STRING ;

: JW-FIELD-U ( ptr u8 n n -- ) {: key:ptr keyu val :}
   key keyu JW-KEY
   val JW-U ;

: JW-FIELD-BOOL ( ptr u8 n bool -- ) {: key:ptr keyu val :}
   key keyu JW-KEY
   val JW-BOOL ;

: JW-FIELD-NULL ( ptr u8 n -- )
   JW-KEY
   JW-NULL ;

: JW$ ( -- ptr u8 n )
   JW-ENSURE-INITIAL
   JW-BUF JW-LEN @ ;
