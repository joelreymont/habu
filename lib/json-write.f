\ json-write.f - checked emit-only JSON writer over caller-owned storage.
\
\ The module keeps no process state. A caller declares one writer record
\ (`TYPED-VARIABLE W JSON-WRITE:writer`, or a `TYPED-BUFFER` of them) plus the
\ output bytes it owns, and JSON-WRITE:OPEN binds the two. Writers over
\ different buffers share nothing, so two tasks can each write JSON without a
\ lock; the caller keeps its own buffer live and exclusive until CLOSE.
\
\ The handle is the nominal `ptr JSON-WRITE:writer`, which only the typed
\ storage definers mint: a raw cell cannot stand in for one. A writer that was
\ never opened - the zero image a definer leaves - or one already closed refuses
\ every operation with E-JW-STATE.
\
\ Every emitter answers its writer, so a document reads as one chain and
\ JSON-WRITE:$ ends the chain with the bytes written so far. An emitter appends
\ all of its bytes or none: a value that does not fit the caller's buffer is
\ E-JW-CAPACITY, never a truncated value. A document that hit a refusal is
\ incomplete, and the caller RESETs or CLOSEs the writer.
\
\ Callers build compact JSON through the qualified public API: JSON-WRITE:OPEN /
\ RESET / CLOSE own the writer, the value emitters JSON-WRITE:STRING / RAW / U /
\ BOOL / NULL append one JSON value, JSON-WRITE:KEY writes one escaped object key
\ plus its colon, JSON-WRITE:COMMA / OBJECT-START / OBJECT-END / ARRAY-START /
\ ARRAY-END write structural delimiters, the JSON-WRITE:FIELD-S / FIELD-U /
\ FIELD-BOOL / FIELD-NULL / FIELD-RAW helpers write one key-and-value pair, and
\ JSON-WRITE:$ returns the accumulated output bytes. The record accessors, the
\ capacity/length refinement helpers, the single-byte and escape emitters, and
\ every constant are package-private.

require lib/errors.f
require lib/string.f

package JSON-WRITE

public

\ out = the caller's output buffer, cap = its byte capacity, len = bytes written.
\ A closed writer keeps CLOSED-CAP so a stale handle is refused, not written to.
STRUCTURE writer 0
  FIELD out ptr u8
  FIELD cap n
  FIELD len n
;STRUCTURE

private

-1 constant JW-CLOSED-CAP

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

10 constant JW-RADIX
1 constant JW-PLAIN-N            \ escaped width of an ordinary byte
2 constant JW-SHORT-ESC-N        \ escaped width of \" \\ \b \f \n \r \t
6 constant JW-U00-N              \ escaped width of \u00XX
2 constant JW-QUOTE-N            \ the two string delimiters

: JW-LIVE ( ptr writer -- ptr u8 n n )   \ output buffer, capacity, length
   @ JSON--WRITE-WRITER:UNMAKE {: vp:ptr cap:n used:n :}
   vp 0= if E-JW-STATE throw then
   cap 0 < if E-JW-STATE throw then
   vp cap used ;

: JW-USED! ( ptr writer ptr u8 n n -- ) {: w vp:ptr cap:n used:n :}
   vp cap used JSON--WRITE-WRITER:MAKE w ! ;

\ Refuse a source span nobody can read.
: JW-SPAN ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-JW-SOURCE throw then
   u 0 > a 0= and if E-JW-SOURCE throw then ;

: JW-LEN ( n -- len )   \ refine a byte count JW-SPAN has already accepted
   dup 0 < if E-JW-SOURCE throw then
   >LEN ;

: JW-ROOM ( ptr writer n -- ptr writer ) {: w need:n :}
   w JW-LIVE {: vp:ptr cap:n used:n :}
   need cap used - > if E-JW-CAPACITY throw then
   w ;

: JW-C ( ptr writer n -- ptr writer ) {: w c:n :}
   c 0 < c JW-BYTE-MAX > or if E-JW-BYTE throw then
   w JW-LIVE {: vp:ptr cap:n used:n :}
   used cap >= if E-JW-CAPACITY throw then
   c vp used + c!
   w vp cap used 1+ JW-USED!
   w ;

\ The source may be a span of this writer's own output: a copy always runs from
\ [out, out+len) into [out+len, ...), so the two never overlap and the buffer
\ never moves under the caller.
: JW-APPEND-LEN ( ptr writer ptr u8 len -- ptr writer ) {: w a:ptr u:len :}
   w u LEN>N JW-ROOM
   JW-LIVE {: vp:ptr cap:n used:n :}
   a vp used + u BYTE-COPY-LEN
   w vp cap used u LEN>N + JW-USED!
   w ;

public

: OPEN ( ptr writer ptr u8 n -- ptr writer ) {: w vp:ptr cap:n :}
   vp 0= if E-JW-OUTPUT throw then
   cap 0 < if E-JW-OUTPUT throw then
   w vp cap 0 JW-USED!
   w ;

: RESET ( ptr writer -- ptr writer ) {: w :}
   w JW-LIVE {: vp:ptr cap:n used:n :}
   w vp cap 0 JW-USED!
   w ;

: CLOSE ( ptr writer -- ) {: w :}
   w JW-LIVE {: vp:ptr cap:n used:n :}
   w vp JW-CLOSED-CAP 0 JW-USED! ;

: $ ( ptr writer -- ptr u8 n )
   JW-LIVE {: vp:ptr cap:n used:n :}
   vp used ;

: RAW ( ptr writer ptr u8 n -- ptr writer ) {: a:ptr u:n :}
   a u JW-SPAN
   a u JW-LEN JW-APPEND-LEN ;

private

: JW-HEX ( n -- n )
   dup 10 < if JW-ZERO + else 55 + then ;

: JW-U00 ( ptr writer n -- ptr writer ) {: c:n :}
   JW-BACKSLASH JW-C
   117 JW-C
   JW-ZERO JW-C
   JW-ZERO JW-C
   c 4 rshift JW-HEX JW-C
   c $F and JW-HEX JW-C ;

: JW-SHORT-ESC? ( n -- bool ) {: c:n :}
   c JW-DQ = c JW-BACKSLASH = or c JW-BS = or c JW-FF = or
   c JW-LF = or c JW-CR = or c JW-TAB = or ;

: JW-ESC-N ( n -- n ) {: c:n :}
   c JW-SHORT-ESC? if JW-SHORT-ESC-N exit then
   c JW-SP < if JW-U00-N else JW-PLAIN-N then ;

: JW-STR-N ( ptr u8 n -- n ) {: a:ptr u:n :}   \ bytes a quoted string will take
   JW-QUOTE-N 0 begin dup u < while            \ ( total idx )
      dup a + c@ JW-ESC-N rot + swap 1+
   repeat drop ;

: JW-ESC-C ( ptr writer n -- ptr writer ) {: c:n :}
   c JW-DQ = if JW-BACKSLASH JW-C JW-DQ JW-C exit then
   c JW-BACKSLASH = if JW-BACKSLASH JW-C JW-BACKSLASH JW-C exit then
   c JW-BS = if JW-BACKSLASH JW-C 98 JW-C exit then
   c JW-FF = if JW-BACKSLASH JW-C 102 JW-C exit then
   c JW-LF = if JW-BACKSLASH JW-C 110 JW-C exit then
   c JW-CR = if JW-BACKSLASH JW-C 114 JW-C exit then
   c JW-TAB = if JW-BACKSLASH JW-C 116 JW-C exit then
   c JW-SP < if c JW-U00 exit then
   c JW-C ;

: JW-DIGIT-COUNT ( n -- n ) {: u:n :}
   u JW-RADIX < if 1 exit then
   u JW-RADIX / RECURSE 1+ ;

: JW-DIGITS ( ptr writer n -- ptr writer ) {: u:n :}
   u JW-RADIX >= if u JW-RADIX / RECURSE then
   u JW-RADIX mod JW-ZERO + JW-C ;

public

: STRING ( ptr writer ptr u8 n -- ptr writer ) {: a:ptr u:n :}
   a u JW-SPAN
   a u JW-STR-N JW-ROOM
   JW-DQ JW-C
   0 begin dup u < while                      \ ( w idx )
      dup a + c@ rot swap JW-ESC-C            \ ( idx w ): escape the byte at idx
      swap 1+
   repeat drop
   JW-DQ JW-C ;

: KEY ( ptr writer ptr u8 n -- ptr writer ) {: a:ptr u:n :}
   a u JW-SPAN
   a u JW-STR-N 1+ JW-ROOM
   a u STRING
   JW-COLON-C JW-C ;

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

: FIELD-RAW ( ptr writer ptr u8 n ptr u8 n -- ptr writer )
   {: kp:ptr keyu:n vp:ptr valu:n :}
   kp keyu KEY
   vp valu RAW ;

: FIELD-S ( ptr writer ptr u8 n ptr u8 n -- ptr writer )
   {: kp:ptr keyu:n vp:ptr valu:n :}
   kp keyu KEY
   vp valu STRING ;

: FIELD-U ( ptr writer ptr u8 n n -- ptr writer ) {: kp:ptr keyu:n val:n :}
   kp keyu KEY
   val U ;

: FIELD-BOOL ( ptr writer ptr u8 n bool -- ptr writer )
   {: kp:ptr keyu:n val:bool :}
   kp keyu KEY
   val BOOL ;

: FIELD-NULL ( ptr writer ptr u8 n -- ptr writer )
   KEY
   NULL ;

;package
