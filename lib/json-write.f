\ json-write.f - checked emit-only JSON writer.
\
\ The module lives in `package JSON-WRITE`. Callers build compact JSON through
\ the qualified public API: JSON-WRITE:RESET starts a fresh output buffer, the
\ value emitters JSON-WRITE:STRING / RAW / U / BOOL / NULL append one JSON value,
\ JSON-WRITE:KEY writes one escaped object key plus its colon, JSON-WRITE:COMMA /
\ OBJECT-START / OBJECT-END / ARRAY-START / ARRAY-END write structural
\ delimiters, the JSON-WRITE:FIELD-S / FIELD-U / FIELD-BOOL / FIELD-NULL /
\ FIELD-RAW helpers write one key-and-value pair, and JSON-WRITE:$ returns the
\ accumulated output bytes. The growable output buffer, the capacity/length
\ refinement helpers, the single-byte and escape emitters, and every constant,
\ buffer, and state variable are package-private.
\
require lib/errors.f
require lib/string.f
require lib/memory.f

package JSON-WRITE

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
variable JW-OUT-LEN
variable JW-NUM-I

: JW-BUF-FIELD ( -- ptr ptr u8 )
   JW-BUF-A 0 ptr-field ;

: JW-BUF@ ( -- ptr u8 )
   JW-BUF-FIELD @ ;

: JW-BUF! ( ptr u8 -- )
   JW-BUF-FIELD ! ;

: JW-BUF ( -- ptr u8 )
   JW-BUF@ ;

: JW-CAP ( -- n )
   JW-BUF-CAP @ ;

: JW-LEN ( n -- len )
   dup 0 < if E-JW-CAPACITY throw then
   dup MEM-MAX-N > if E-JW-CAPACITY throw then
   >LEN ;

: JW-STORE-SPAN ( ptr u8 n -- )
   JW-BUF-CAP ! JW-BUF! ;

: JW-MIN-ONE ( n -- n )
   dup JW-MIN-CAP < if drop JW-MIN-CAP then ;

: JW-NEED-CAP-LEN ( len -- n ) {: add :}
   JW-OUT-LEN @ 0 < if E-JW-CAPACITY throw then
   add LEN>N MEM-MAX-N JW-OUT-LEN @ - >= if E-JW-CAPACITY throw then
   JW-OUT-LEN @ add LEN>N + ;

: JW-NEED-CAP ( n -- n )
   JW-LEN JW-NEED-CAP-LEN ;

: JW-COPY-OLD ( ptr u8 -- ) {: dst:ptr :}
   JW-OUT-LEN @ 0 > if JW-BUF dst JW-OUT-LEN @ BYTE-COPY then ;

\ Storage flows through MEM-ALLOC-64K-SPAN / MEM:RELEASE-BYTES.
: JW-RELEASE-SPAN ( ptr u8 n -- ) {: a:ptr cap:n :}
   a cap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

\ Copy into the new mapping, install it, then release the prior one. The release
\ is LAST and the allocation runs before anything is overwritten, so a grow whose
\ alloc throws leaves the old span owned and intact (BUF:INSTALL-RESIZE's order).
\ The first grow has no prior span: capacity zero is the proved no-op.
: JW-GROW ( n -- ) {: need :}
   JW-BUF@ {: old:ptr :}
   JW-CAP {: oldcap:n :}
   need JW-MIN-ONE MEM-ALLOC-64K-SPAN
   over JW-COPY-OLD
   JW-STORE-SPAN
   oldcap 0 > if old oldcap JW-RELEASE-SPAN then ;

: JW-CHECK-LEN-ROOM ( len -- )
   JW-NEED-CAP-LEN dup JW-CAP > if JW-GROW else drop then ;

: JW-CHECK-ROOM ( n -- )
   JW-LEN JW-CHECK-LEN-ROOM ;

: JW-ENSURE-INITIAL ( -- )
   JW-CAP 0= if JW-MIN-CAP JW-GROW then ;

public

: RESET ( -- )
   JW-ENSURE-INITIAL
   0 JW-OUT-LEN ! ;

private

: JW-C ( n -- ) {: c :}
   c 0 < if E-JW-BYTE throw then
   c JW-BYTE-MAX > if E-JW-BYTE throw then
   1 JW-CHECK-ROOM
   c JW-BUF JW-OUT-LEN @ + c!
   JW-OUT-LEN @ 1+ JW-OUT-LEN ! ;

: JW-RAW-LEN ( ptr u8 len -- ) {: a:ptr u :}
   u JW-CHECK-LEN-ROOM
   a JW-BUF JW-OUT-LEN @ + u BYTE-COPY-LEN
   JW-OUT-LEN @ u LEN>N + JW-OUT-LEN ! ;

public

: RAW ( ptr u8 n -- )
   JW-LEN JW-RAW-LEN ;

private

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

public

: STRING ( ptr u8 n -- ) {: a:ptr u:n :}
   JW-DQ JW-C
   0 begin dup u < while
      dup a + c@ JW-ESC-C
      1+
   repeat drop
   JW-DQ JW-C ;

: KEY ( ptr u8 n -- )
   STRING
   JW-COLON-C JW-C ;

: OBJECT-START ( -- )
   JW-LBRACE JW-C ;

: OBJECT-END ( -- )
   JW-RBRACE JW-C ;

: ARRAY-START ( -- )
   JW-LBRACK JW-C ;

: ARRAY-END ( -- )
   JW-RBRACK JW-C ;

: COMMA ( -- )
   JW-COMMA-C JW-C ;

: NULL ( -- )
   s" null" RAW ;

: BOOL ( bool -- )
   if s" true" else s" false" then RAW ;

: U ( n -- ) {: u:n :}
   u 0 < if E-JW-BYTE throw then
   JW-NUM-CAP JW-NUM-I !
   u 0= if JW-ZERO JW-C exit then
   u begin dup 0 > while
      dup 10 mod JW-ZERO +
      JW-NUM-I @ 1- JW-NUM-I !
      JW-NUM-BUF JW-NUM-I @ + c!
      10 /
   repeat drop
   JW-NUM-BUF JW-NUM-I @ + JW-NUM-CAP JW-NUM-I @ - RAW ;

: FIELD-RAW ( ptr u8 n ptr u8 n -- ) {: kp:ptr keyu:n vp:ptr valu:n :}
   kp keyu KEY
   vp valu RAW ;

: FIELD-S ( ptr u8 n ptr u8 n -- ) {: kp:ptr keyu:n vp:ptr valu:n :}
   kp keyu KEY
   vp valu STRING ;

: FIELD-U ( ptr u8 n n -- ) {: kp:ptr keyu:n val:n :}
   kp keyu KEY
   val U ;

: FIELD-BOOL ( ptr u8 n bool -- ) {: kp:ptr keyu:n val:bool :}
   kp keyu KEY
   val BOOL ;

: FIELD-NULL ( ptr u8 n -- )
   KEY
   NULL ;

: $ ( -- ptr u8 n )
   JW-ENSURE-INITIAL
   JW-BUF JW-OUT-LEN @ ;

;package
