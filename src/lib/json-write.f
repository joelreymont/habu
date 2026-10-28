\ json-write.f - checked emit-only JSON writer over caller-owned storage.
\
\ The module keeps no process state. A caller declares one writer record
\ (`TYPED-VARIABLE W JSON-WRITE:writer`, or a `TYPED-BUFFER` of them) plus the
\ output bytes it owns. JSON-WRITE:OPEN binds fixed output and OPEN-BUF binds
\ an initialized caller-owned BUF header. Writers over different buffers share
\ nothing, so two tasks can each write JSON without a lock; the caller keeps its
\ own buffer live and exclusive until CLOSE.
\
\ The handle is the nominal `ptr JSON-WRITE:writer`, which only the typed
\ storage definers mint: a raw cell cannot stand in for one. A writer that was
\ never opened - the zero image a definer leaves - or one already closed refuses
\ every operation with E-JW-STATE.
\
\ Every emitter answers its writer, so a document reads as one chain and
\ JSON-WRITE:$ ends the chain with the bytes written so far. An emitter appends
\ all of its bytes or none. Fixed output refuses a value that will not fit with
\ E-JW-CAPACITY; growable output propagates BUF/allocation failures. A document
\ that hit a refusal is incomplete, and the caller RESETs or CLOSEs the writer.
\
\ Callers build compact JSON through the qualified public API: JSON-WRITE:OPEN /
\ OPEN-BUF / RESET / CLOSE bind and control the writer, the value emitters
\ JSON-WRITE:STRING / RAW / U / INT / BOOL / NULL append one JSON value,
\ JSON-WRITE:KEY writes one escaped object key plus its colon,
\ JSON-WRITE:COMMA / OBJECT-START / OBJECT-END / ARRAY-START / ARRAY-END write
\ structural delimiters, the JSON-WRITE:FIELD-S / FIELD-U / FIELD-INT /
\ FIELD-BOOL / FIELD-NULL / FIELD-RAW helpers write one key-and-value pair, and
\ JSON-WRITE:$ returns the accumulated output bytes. The record accessors, the
\ capacity/length refinement helpers, the single-byte and escape emitters, and
\ every constant are package-private.
\
\ OPEN-BUF clears the BUF and grows it as needed, including when an emitter
\ reads bytes already in it; CLOSE leaves the bytes to the caller, who disposes
\ the BUF after their last use. The span JSON-WRITE:$ answers stays valid until
\ the next write. A byte outside 0..255 is E-JW-BYTE, a null or negative output
\ E-JW-OUTPUT, and a negative or null-with-bytes span E-JW-SOURCE, which also
\ covers a span inside a growable writer's capacity but outside its written
\ bytes. U refuses a negative number with E-JW-BYTE; INT writes every cell, the
\ minimum from STR-MIN-I64$; both reserve the number's whole width first.
\ FIELD-RAW is only for a known-valid JSON fragment. Size fixed output for the
\ worst case: one byte escapes to six (\u00XX).
\
\ STORAGE CLASS. CALLER-OWNED: the writer record and its output; no module state.

require src/lib/errors.f
require src/lib/string.f
require src/lib/byte-buffer.f

package JSON-WRITE

public

\ Fixed output uses out/cap/len. Growable output uses the caller's BUF header
\ through buf. A closed writer keeps CLOSED-CAP.
STRUCTURE writer
  FIELD out ptr u8
  FIELD cap n
  FIELD len n
  FIELD buf ptr n
;STRUCTURE

private

-1 constant JW-CLOSED-CAP

8 constant JW-BS
9 constant JW-TAB
10 constant JW-LF
12 constant JW-FF
13 constant JW-CR
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

10 constant JW-RADIX
1 constant JW-PLAIN-N            \ escaped width of an ordinary byte
2 constant JW-QUOTE-N            \ the two string delimiters

\ The escaped width of each byte value: 1 plain, 2 for \" \\ \b \t \n \f \r,
\ 6 for the other control bytes (\u00XX). Scans index it inline.
create JW-ESC-W
   6 c, 6 c, 6 c, 6 c, 6 c, 6 c, 6 c, 6 c, 2 c, 2 c, 2 c, 6 c, 2 c, 2 c, 6 c, 6 c,   \ $00
   6 c, 6 c, 6 c, 6 c, 6 c, 6 c, 6 c, 6 c, 6 c, 6 c, 6 c, 6 c, 6 c, 6 c, 6 c, 6 c,   \ $10
   1 c, 1 c, 2 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c,   \ $20
   1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c,   \ $30
   1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c,   \ $40
   1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 2 c, 1 c, 1 c, 1 c,   \ $50
   1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c,   \ $60
   1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c,   \ $70
   1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c,   \ $80
   1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c,   \ $90
   1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c,   \ $A0
   1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c,   \ $B0
   1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c,   \ $C0
   1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c,   \ $D0
   1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c,   \ $E0
   1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c, 1 c,   \ $F0

: JW-BUF@ ( ptr writer -- ptr n )
   @ JSON--WRITE-WRITER:UNMAKE {: vp:ptr cap:n used:n buf:ptr :}
   buf ;

: JW-LIVE ( ptr writer -- ptr u8 n n )   \ output buffer, capacity, length
   @ JSON--WRITE-WRITER:UNMAKE {: vp:ptr cap:n used:n buf:ptr :}
   cap 0 < if E-JW-STATE throw then
   buf 0= if
      vp 0= if E-JW-STATE throw then
      vp cap used exit
   then
   buf BUF:SPAN$ BUF:BLEN>N {: data:ptr length:n :}
   data buf BUF:CAP@ BUF:BLEN>N length ;

: JW-USED! ( ptr writer ptr u8 n n -- ) {: w vp:ptr cap:n used:n :}
   vp cap used NULL-PTR JSON--WRITE-WRITER:MAKE w ! ;

\ Refuse a source span nobody can read.
: JW-SPAN ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-JW-SOURCE throw then
   u 0 > a 0= and if E-JW-SOURCE throw then ;

: JW-ALIAS-AT ( ptr u8 n n ptr u8 n -- n )
   {: base:ptr cap:n used:n a:ptr u:n :}
   a base < if -1 exit then
   a base - {: off:n :}
   off cap >= if -1 exit then
   off used > if E-JW-SOURCE throw then
   u used off - > if E-JW-SOURCE throw then
   off ;

: JW-ALIAS ( ptr writer ptr u8 n -- n ) {: w a:ptr u:n :}
   w JW-BUF@ 0= if -1 exit then
   w JW-LIVE {: base:ptr cap:n used:n :}
   base cap used a u JW-ALIAS-AT ;

: JW-REBASE ( ptr writer ptr u8 n -- ptr u8 ) {: w a:ptr off:n :}
   off 0 < if a exit then
   w JW-LIVE {: base:ptr cap:n used:n :}
   base off + ;

: JW-SIZE+ ( n n -- n ) {: left:n right:n :}
   right MEM-MAX-N left - > if E-JW-CAPACITY throw then
   left right + ;

: JW-ROOM ( ptr writer n -- ptr writer ) {: w need:n :}
   w JW-LIVE {: vp:ptr cap:n used:n :}
   need 0 < need MEM-MAX-N used - > or if E-JW-CAPACITY throw then
   w JW-BUF@ {: buf:ptr :}
   buf 0= if
      need cap used - > if E-JW-CAPACITY throw then
      w exit
   then
   buf used need + BUF:N>BLEN BUF:ENSURE
   w ;

\ Extend the output by n bytes JW-ROOM has made room for and answer the span
\ over them; it is valid until the next write.
: JW-TAKE ( ptr writer n -- SPAN:span<u8> ) {: w n:n :}
   w JW-BUF@ {: buf:ptr :}
   buf 0= if
      w JW-LIVE {: vp:ptr cap:n used:n :}
      w vp cap used n + JW-USED!
      vp used + n SPAN:MAKE exit
   then
   buf n BUF:N>BLEN BUF:GROW ;

: JW-C ( ptr writer n -- ptr writer ) {: w c:n :}
   c 0 < c JW-BYTE-MAX > or if E-JW-BYTE throw then
   w JW-BUF@ {: buf:ptr :}
   buf 0= if
      w JW-LIVE {: vp:ptr cap:n used:n :}
      used cap >= if E-JW-CAPACITY throw then
      c vp used + c!
      w vp cap used 1+ JW-USED!
      w exit
   then
   c buf BUF:APPEND-BYTE w ;

\ The source may be a span of this writer's own output. Remember its offset
\ before reserving, then reacquire the base after a BUF relocation.
: JW-APPEND-LEN ( ptr writer ptr u8 len -- ptr writer ) {: w a:ptr u:len :}
   w @ JSON--WRITE-WRITER:UNMAKE {: vp:ptr cap:n used:n buf:ptr :}
   cap 0 < if E-JW-STATE throw then
   u LEN>N {: count:n :}
   buf 0= if
      vp 0= if E-JW-STATE throw then
      count MEM-MAX-N used - > if E-JW-CAPACITY throw then
      count cap used - > if E-JW-CAPACITY throw then
      a vp used + u BYTE-COPY-LEN
      w vp cap used count + JW-USED!
      w exit
   then
   buf BUF:SPAN$ BUF:BLEN>N {: base:ptr length:n :}
   buf BUF:CAP@ BUF:BLEN>N {: capacity:n :}
   base capacity length a count JW-ALIAS-AT {: off:n :}
   count MEM-MAX-N length - > if E-JW-CAPACITY throw then
   buf length count + BUF:N>BLEN BUF:ENSURE
   off 0 < if a else buf BUF:SPAN$ drop off + then
   count BUF:N>BLEN buf BUF:APPEND-SPAN
   w ;

public

: OPEN ( ptr writer ptr u8 n -- ptr writer ) {: w vp:ptr cap:n :}
   vp 0= if E-JW-OUTPUT throw then
   cap 0 < if E-JW-OUTPUT throw then
   w vp cap 0 JW-USED!
   w ;

: OPEN-BUF ( ptr writer ptr n -- ptr writer ) {: w buf:ptr :}
   buf 0= if E-JW-OUTPUT throw then
   buf BUF:CLEAR
   NULL-PTR 0 0 buf JSON--WRITE-WRITER:MAKE w !
   w ;

: RESET ( ptr writer -- ptr writer ) {: w :}
   w JW-BUF@ {: buf:ptr :}
   buf 0= if
      w JW-LIVE {: vp:ptr cap:n used:n :}
      w vp cap 0 JW-USED!
      w exit
   then
   buf BUF:CLEAR
   w ;

: CLOSE ( ptr writer -- ) {: w :}
   w JW-BUF@ 0= if w JW-LIVE 2drop drop then
   w NULL-PTR JW-CLOSED-CAP 0 JW-USED! ;

: $ ( ptr writer -- ptr u8 n )
   JW-LIVE {: vp:ptr cap:n used:n :}
   vp used ;

: RAW ( ptr writer ptr u8 n -- ptr writer ) {: a:ptr u:n :}
   a u JW-SPAN
   a u >LEN JW-APPEND-LEN ;

private

: JW-HEX ( n -- n )
   dup 10 < if JW-ZERO + else 55 + then ;

\ Bytes a quoted string will take: the delimiters plus each byte's width. A
\ width is at most six, so the sum fits a cell for any span in memory; the one
\ checked add refuses it exactly when a running checked sum would.
: JW-STR-N ( ptr u8 n -- n ) {: a:ptr u:n :}
   JW-ESC-W BYTE-VIEW {: w:ptr :}
   0 u 0 ?do a i + c@ w + c@ + loop
   JW-QUOTE-N swap JW-SIZE+ ;

\ The letter after the backslash in a two-byte escape of c, or 0 for \u00XX.
: JW-ESC-LETTER ( n -- n ) {: c:n :}
   c JW-DQ = c JW-BACKSLASH = or if c exit then
   c JW-BS = if 98 exit then
   c JW-FF = if 102 exit then
   c JW-LF = if 110 exit then
   c JW-CR = if 114 exit then
   c JW-TAB = if 116 exit then
   0 ;

\ Write the escape of c at offset at of s; answer the offset past it.
: JW-ESC ( SPAN:span<u8> n n -- n ) {: s at:n c:n :}
   JW-BACKSLASH s at SPAN:U8!
   c JW-ESC-LETTER {: l:n :}
   l 0 > if l s at 1+ SPAN:U8! at 2 + exit then
   117 s at 1+ SPAN:U8!
   JW-ZERO s at 2 + SPAN:U8!
   JW-ZERO s at 3 + SPAN:U8!
   c 4 rshift JW-HEX s at 4 + SPAN:U8!
   c $F and JW-HEX s at 5 + SPAN:U8!
   at 6 + ;

\ The offset of the first byte at or after start that escapes, or u.
: JW-RUN-END ( ptr u8 n n -- n ) {: a:ptr u:n start:n :}
   JW-ESC-W BYTE-VIEW {: w:ptr :}
   u start ?do a i + c@ w + c@ JW-PLAIN-N <> if i unloop exit then loop
   u ;

\ Copy the plain run from source offset i, then escape the byte that ends it;
\ answer the next output and source offsets.
: JW-ESC-STEP ( n n ptr u8 n SPAN:span<u8> -- n n )
   {: at:n i:n a:ptr u:n s :}
   a u i JW-RUN-END {: e:n :}
   e i - {: run:n :}
   run 0 > if a i + run s at SPAN:SKIP SPAN:COPY then
   e u = if at run + e exit then
   s at run + a e + c@ JW-ESC  e 1+ ;

\ Write the quoted, escaped source into s, which is exactly its width.
: JW-QUOTE-INTO ( ptr u8 n SPAN:span<u8> -- )
   {: a:ptr u:n s :}
   JW-DQ s 0 SPAN:U8!
   s SPAN:LEN JW-QUOTE-N - u = if
      a u s 1 SPAN:SKIP SPAN:COPY
   else
      1 0 begin dup u < while a u s JW-ESC-STEP repeat 2drop
   then
   JW-DQ s s SPAN:LEN 1- SPAN:U8! ;

: JW-DIGIT-COUNT ( n -- n ) {: u:n :}
   u JW-RADIX < if 1 exit then
   u JW-RADIX / RECURSE 1+ ;

: JW-DIGITS ( ptr writer n -- ptr writer ) {: u:n :}
   u JW-RADIX >= if u JW-RADIX / RECURSE then
   u JW-RADIX mod JW-ZERO + JW-C ;

\ Each source byte needs at least one output byte, so refuse an overlong fixed
\ span before reading it. Both reservations may relocate caller-owned output;
\ rebase the source after each before scanning or emitting it. The escapes go
\ straight into the span JW-TAKE answers over the whole quoted width.
: JW-QUOTED ( ptr writer ptr u8 n n -- ptr writer )
   {: w a:ptr u:n suffix:n :}
   a u JW-SPAN
   w a u JW-ALIAS {: off:n :}
   w u JW-ROOM drop
   w a off JW-REBASE u JW-STR-N {: width:n :}
   w width suffix JW-SIZE+ JW-ROOM drop
   w width JW-TAKE {: s :}
   w a off JW-REBASE u s JW-QUOTE-INTO
   w
   suffix 0 > if JW-COLON-C JW-C then ;

public

: STRING ( ptr writer ptr u8 n -- ptr writer )
   0 JW-QUOTED ;

: KEY ( ptr writer ptr u8 n -- ptr writer )
   1 JW-QUOTED ;

: OBJECT-START ( ptr writer -- ptr writer )
   JW-LBRACE JW-C ;

: OBJECT-END ( ptr writer -- ptr writer )
   JW-RBRACE JW-C ;

: ARRAY-START ( ptr writer -- ptr writer )
   JW-LBRACK JW-C ;

: ARRAY-END ( ptr writer -- ptr writer )
   JW-RBRACK JW-C ;

: COMMA ( ptr writer -- ptr writer )
   JW-COMMA-C JW-C ;

: NULL ( ptr writer -- ptr writer )
   s" null" RAW ;

: BOOL ( ptr writer bool -- ptr writer )
   if s" true" else s" false" then RAW ;

: U ( ptr writer n -- ptr writer ) {: u:n :}
   u 0 < if E-JW-BYTE throw then
   u JW-DIGIT-COUNT JW-ROOM
   u JW-DIGITS ;

\ The minimum cell negates to itself. Its quotient by ten has a positive
\ magnitude that fits in a cell, and its final digit comes from the remainder.
\ Reserve the whole width first, so a number is written whole or not at all.
: INT ( ptr writer n -- ptr writer )
   {: v:n :}
   v 0 >= if v U exit then
   v STR-MIN-I64 = if
      STR-I64-DIGITS 1+ JW-ROOM
      STR-MINUS JW-C
      v JW-RADIX / negate JW-DIGITS
      v JW-RADIX mod negate JW-ZERO + JW-C exit
   then
   v negate {: m:n :}
   m JW-DIGIT-COUNT 1+ JW-ROOM
   STR-MINUS JW-C
   m JW-DIGITS ;

: FIELD-RAW ( ptr writer ptr u8 n ptr u8 n -- ptr writer )
   {: kp:ptr keyu:n vp:ptr valu:n :}
   dup vp valu JW-ALIAS {: off:n :}
   kp keyu KEY
   dup vp off JW-REBASE valu RAW ;

: FIELD-S ( ptr writer ptr u8 n ptr u8 n -- ptr writer )
   {: kp:ptr keyu:n vp:ptr valu:n :}
   dup vp valu JW-ALIAS {: off:n :}
   kp keyu KEY
   dup vp off JW-REBASE valu STRING ;

: FIELD-U ( ptr writer ptr u8 n n -- ptr writer ) {: kp:ptr keyu:n val:n :}
   kp keyu KEY
   val U ;

: FIELD-INT ( ptr writer ptr u8 n n -- ptr writer )
   {: kp:ptr keyu:n val:n :}
   kp keyu KEY
   val INT ;

: FIELD-BOOL ( ptr writer ptr u8 n bool -- ptr writer )
   {: kp:ptr keyu:n val:bool :}
   kp keyu KEY
   val BOOL ;

: FIELD-NULL ( ptr writer ptr u8 n -- ptr writer )
   KEY
   NULL ;

;package
