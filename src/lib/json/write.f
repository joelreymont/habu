\ write.f - the emit-only JSON writer, WRITE-OPEN through WRITE-FIELD-NULL.
\ A file of package JSON (src/lib/json.f).
\
\ A source span may lie in a growable writer's own output, and any step that
\ grows the output may relocate it. A word that takes a source records the
\ span's offset with JW-ALIAS first and recomputes its address with JW-REBASE
\ after each such step, before reading it.

require src/lib/error.f
require src/lib/string.f
require src/lib/byte-buffer.f
require src/lib/utf8-scalar.f
require src/lib/utf16.f

package JSON

public

\ fresh: nothing written since WRITE-OPEN, WRITE-OPEN-BUF or WRITE-RESET;
\ valued: a value last; opened: a START last; separated: a comma last; keyed:
\ a key last, its value due; quoting: inside WRITE-STRING-START's open string.
ENUM writer-layout fresh valued opened separated keyed quoting ;ENUM

\ Fixed output uses out/cap/len. Growable output uses the caller's BUF header
\ through buf. A closed writer keeps CLOSED-CAP. indent is the spaces per
\ nesting level; 0 is compact. ascii is the mode WRITE-ASCII! sets. depth
\ counts the open containers.
STRUCTURE writer
  FIELD out ^u8
  FIELD cap n
  FIELD len n
  FIELD buf ^n
  FIELD indent n
  FIELD ascii bool
  FIELD depth n
  FIELD layout writer-layout
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

10 constant JW-RADIX
1 constant JW-PLAIN-N            \ escaped width of an ordinary byte
2 constant JW-QUOTE-N            \ the two string delimiters
1 constant JW-COLON-N            \ a compact key's separator, `:`
2 constant JW-COLON-SP-N         \ an indented key's separator, `: `
6 constant JW-U-N                \ one \uXXXX escape
12 constant JW-PAIR-N            \ a surrogate pair's two escapes

117 constant JW-U-C              \ the u of \uXXXX
$7F constant JW-DEL
$FFFF constant JW-BMP-LAST       \ the last scalar one UTF-16 unit holds

create JW-KEY-SEP JW-COLON-C c, JW-SP c,   \ each separator is a prefix of it

\ The escaped width of each byte value: 1 plain, 2 for \" \\ \b \t \n \f \r,
\ 6 for the other control bytes (\u00XX).
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

: JW-BUF@ ( ^writer -- ^n )
   @ JSON-WRITER:UNMAKE {: vp:^ cap:n used:n buf:^ indent:n ascii:bool depth:n layout:writer-layout :}
   buf ;

\ indent, depth, layout. Unchecked: also reads a closed writer.
: JW-LAYOUT@ ( ^writer -- n n writer-layout )
   @ JSON-WRITER:UNMAKE {: vp:^ cap:n used:n buf:^ indent:n ascii:bool depth:n layout:writer-layout :}
   indent depth layout ;

: JW-ASCII@ ( ^writer -- bool )
   @ JSON-WRITER:UNMAKE {: vp:^ cap:n used:n buf:^ indent:n ascii:bool depth:n layout:writer-layout :}
   ascii ;

: JW-LAYOUT! ( ^writer n writer-layout -- )
   {: w depth:n layout:writer-layout :}
   w @ JSON-WRITER:UNMAKE 2drop {: vp:^ cap:n used:n buf:^ indent:n ascii:bool :}
   vp cap used buf indent ascii depth layout JSON-WRITER:MAKE w ! ;

: JW-LIVE ( ^writer -- ^u8 n n )   \ output buffer, capacity, length
   @ JSON-WRITER:UNMAKE {: vp:^ cap:n used:n buf:^ indent:n ascii:bool depth:n layout:writer-layout :}
   cap 0 < if E-JSON-WRITE-STATE throw then
   buf 0= if
      vp 0= if E-JSON-WRITE-STATE throw then
      vp cap used exit
   then
   buf BUF:SPAN$ BUF:BLEN>N {: data:^ length:n :}
   data buf BUF:CAP@ BUF:BLEN>N length ;

: JW-USED! ( ^writer ^u8 n n -- )
   {: w vp:^ cap:n used:n :}
   w JW-LAYOUT@ {: indent:n depth:n layout:writer-layout :}
   w JW-ASCII@ {: ascii:bool :}
   vp cap used NULL-PTR indent ascii depth layout JSON-WRITER:MAKE w ! ;

: JW-SPAN ( ^u8 n -- ) {: a:^ u:n :}
   u 0 < if E-JSON-WRITE-SOURCE throw then
   u 0 > a 0= and if E-JSON-WRITE-SOURCE throw then ;

\ A span's offset in a growable writer's output, or -1 for fixed output or
\ outside its capacity; one inside the capacity but past the written bytes is
\ E-JSON-WRITE-SOURCE.
: JW-ALIAS ( ^writer ^u8 n -- n ) {: w a:^ u:n :}
   w JW-BUF@ 0= if -1 exit then
   w JW-LIVE {: base:^ cap:n used:n :}
   a base < if -1 exit then
   a base - {: off:n :}
   off cap >= if -1 exit then
   off used > if E-JSON-WRITE-SOURCE throw then
   u used off - > if E-JSON-WRITE-SOURCE throw then
   off ;

: JW-REBASE ( ^writer ^u8 n -- ^u8 ) {: w a:^ off:n :}
   off 0 < if a exit then
   w JW-LIVE {: base:^ cap:n used:n :}
   base off + ;

: JW-SIZE+ ( n n -- n ) {: left:n right:n :}
   right MEM-MAX-N left - > if E-JSON-WRITE-CAPACITY throw then
   left right + ;

: JW-ROOM ( ^writer n -- ^writer ) {: w need:n :}
   w JW-LIVE {: vp:^ cap:n used:n :}
   need 0 < need MEM-MAX-N used - > or if E-JSON-WRITE-CAPACITY throw then
   w JW-BUF@ {: buf:^ :}
   buf 0= if
      need cap used - > if E-JSON-WRITE-CAPACITY throw then
      w exit
   then
   buf used need + BUF:N>BLEN BUF:ENSURE
   w ;

\ Extend the output by n bytes JW-ROOM has made room for and answer the span
\ over them; it is valid until the next write.
: JW-TAKE ( ^writer n -- SPAN:span<u8> ) {: w n:n :}
   w JW-BUF@ {: buf:^ :}
   buf 0= if
      w JW-LIVE {: vp:^ cap:n used:n :}
      w vp cap used n + JW-USED!
      vp used + n SPAN:MAKE exit
   then
   buf n BUF:N>BLEN BUF:GROW ;

: JW-C ( ^writer n -- ^writer ) {: w c:n :}
   w JW-BUF@ {: buf:^ :}
   buf 0= if
      w JW-LIVE {: vp:^ cap:n used:n :}
      used cap >= if E-JSON-WRITE-CAPACITY throw then
      c vp used + c!
      w vp cap used 1+ JW-USED!
      w exit
   then
   c buf BUF:APPEND-BYTE w ;

: JW-BREAK-N ( n n -- n ) {: indent:n depth:n :}
   indent 0= if 0 exit then
   depth MEM-MAX-N indent / > if E-JSON-WRITE-CAPACITY throw then
   1 depth indent * JW-SIZE+ ;

\ Reserve a token of count bytes behind a break of brk bytes as one room, then
\ write the break; the token writes into the rest.
: JW-BREAK-AT ( ^writer n n -- ^writer ) {: w count:n brk:n :}
   w count brk JW-SIZE+ JW-ROOM drop
   brk 0= if w exit then
   w brk JW-TAKE {: s :}
   JW-SP s SPAN:FILL
   JW-LF s 0 SPAN:U8!
   w ;

\ A refusal leaves the length, depth and layout unchanged.
: JW-TOKEN ( ^writer n -- ^writer )
   {: w count:n :}
   w JW-LIVE 2drop drop
   w JW-LAYOUT@ {: indent:n depth:n layout:writer-layout :}
   layout MATCH writer-layout
      fresh OF 0 ENDOF
      valued OF 0 ENDOF
      opened OF indent depth JW-BREAK-N ENDOF
      separated OF indent depth JW-BREAK-N ENDOF
      keyed OF 0 ENDOF
      quoting OF E-JSON-WRITE-STATE throw ENDOF
   ;MATCH {: brk:n :}
   w count brk JW-BREAK-AT
   depth construct writer-layout valued JW-LAYOUT!
   w ;

: JW-MARK ( ^writer writer-layout -- ^writer )
   {: w layout:writer-layout :}
   w JW-LAYOUT@ drop nip {: depth:n :}
   w depth layout JW-LAYOUT!
   w ;

: JW-QUOTING ( ^writer -- ^writer )
   {: w :}
   w JW-LIVE 2drop drop
   w JW-LAYOUT@ {: indent:n depth:n layout:writer-layout :}
   layout MATCH writer-layout
      fresh OF E-JSON-WRITE-STATE throw ENDOF
      valued OF E-JSON-WRITE-STATE throw ENDOF
      opened OF E-JSON-WRITE-STATE throw ENDOF
      separated OF E-JSON-WRITE-STATE throw ENDOF
      keyed OF E-JSON-WRITE-STATE throw ENDOF
      quoting OF ENDOF
   ;MATCH
   w ;

: JW-START ( ^writer n -- ^writer )
   {: w c:n :}
   w 1 JW-TOKEN c JW-C drop
   w JW-LAYOUT@ {: indent:n depth:n layout:writer-layout :}
   w depth 1+ construct writer-layout opened JW-LAYOUT!
   w ;

: JW-END ( ^writer n -- ^writer )
   {: w c:n :}
   w JW-LIVE 2drop drop
   w JW-LAYOUT@ {: indent:n depth:n layout:writer-layout :}
   depth 0= if E-JSON-WRITE-STATE throw then
   layout MATCH writer-layout
      fresh OF E-JSON-WRITE-STATE throw ENDOF
      valued OF indent depth 1- JW-BREAK-N ENDOF
      opened OF 0 ENDOF
      separated OF E-JSON-WRITE-STATE throw ENDOF
      keyed OF E-JSON-WRITE-STATE throw ENDOF
      quoting OF E-JSON-WRITE-STATE throw ENDOF
   ;MATCH {: brk:n :}
   w 1 brk JW-BREAK-AT c JW-C drop
   w depth 1- construct writer-layout valued JW-LAYOUT!
   w ;

public

\ Fixed output refuses a value that will not fit with E-JSON-WRITE-CAPACITY:
\ size it for the worst case, one byte escaping to six (\u00XX). The caller
\ keeps its buffer live and exclusive until WRITE-CLOSE.
: WRITE-OPEN ( ^writer ^u8 n -- ^writer )
   {: w vp:^ cap:n :}
   vp 0= if E-JSON-WRITE-OUTPUT throw then
   cap 0 < if E-JSON-WRITE-OUTPUT throw then
   vp cap 0 NULL-PTR 0 false 0 construct writer-layout fresh
   JSON-WRITER:MAKE w !
   w ;

\ Clears the BUF and grows it as needed, including when an emitter reads bytes
\ already in it; allocation failures propagate. WRITE-CLOSE leaves the bytes to the
\ caller, who disposes the BUF after their last use.
: WRITE-OPEN-BUF ( ^writer ^n -- ^writer )
   {: w buf:^ :}
   buf 0= if E-JSON-WRITE-OUTPUT throw then
   buf BUF:CLEAR
   NULL-PTR 0 0 buf 0 false 0 construct writer-layout fresh
   JSON-WRITER:MAKE w !
   w ;

: WRITE-RESET ( ^writer -- ^writer )
   {: w :}
   w JW-BUF@ {: buf:^ :}
   buf 0= if
      w JW-LIVE {: vp:^ cap:n used:n :}
      w vp cap 0 JW-USED!
   else
      buf BUF:CLEAR
   then
   w 0 construct writer-layout fresh JW-LAYOUT!
   w ;

: WRITE-CLOSE ( ^writer -- ) {: w :}
   w JW-BUF@ 0= if w JW-LIVE 2drop drop then
   w NULL-PTR JW-CLOSED-CAP 0 JW-USED! ;

\ The span stays valid until the next write.
: WRITTEN$ ( ^writer -- ^u8 n )
   JW-LIVE {: vp:^ cap:n used:n :}
   vp used ;

: WRITE-RAW ( ^writer ^u8 n -- ^writer ) {: w a:^ u:n :}
   a u JW-SPAN
   w a u JW-ALIAS {: off:n :}
   w u JW-TOKEN u JW-TAKE {: s :}
   w a off JW-REBASE u s SPAN:COPY
   w ;

: WRITE-INDENT! ( ^writer n -- ^writer )
   {: w indent:n :}
   indent 0 < if E-JSON-WRITE-BYTE throw then
   w JW-LIVE 2drop drop
   w @ JSON-WRITER:UNMAKE {: vp:^ cap:n used:n buf:^ old:n ascii:bool depth:n layout:writer-layout :}
   vp cap used buf indent ascii depth layout JSON-WRITER:MAKE w !
   w ;

: WRITE-ASCII! ( ^writer bool -- ^writer )
   {: w ascii:bool :}
   w JW-LIVE 2drop drop
   w @ JSON-WRITER:UNMAKE {: vp:^ cap:n used:n buf:^ indent:n old:bool depth:n layout:writer-layout :}
   vp cap used buf indent ascii depth layout JSON-WRITER:MAKE w !
   w ;

private

\ A nibble's hex digit, lowercase as JSON.stringify and Python's json write it.
: JW-HEX ( n -- n )
   dup 10 < if JW-ZERO + else 87 + then ;

\ A width is at most six, so the sum fits a cell for any span in memory; the
\ checked adds of the delimiters and separator refuse it exactly when a running
\ checked sum would.
: JW-ESC-N ( ^u8 n -- n ) {: a:^ u:n :}
   JW-ESC-W BYTE-VIEW {: w:^ :}
   0 u 0 ?do a i + c@ w + c@ + loop ;

\ The escaped width of byte c.
: JW-ESC-W@ ( n -- n )
   JW-ESC-W BYTE-VIEW + c@ ;

\ The letter after the backslash in a two-byte escape of c, or 0 for \u00XX.
: JW-ESC-LETTER ( n -- n ) {: c:n :}
   c JW-DQ = c JW-BACKSLASH = or if c exit then
   c JW-BS = if 98 exit then
   c JW-FF = if 102 exit then
   c JW-LF = if 110 exit then
   c JW-CR = if 114 exit then
   c JW-TAB = if 116 exit then
   0 ;

: JW-NIBBLE ( n n -- n ) {: u:n k:n :}   \ k counts from the high nibble
   u 3 k - 4 * rshift $F and JW-HEX ;

\ Write \uXXXX of the 16-bit unit u at at; answer the offset after it.
: JW-U4 ( SPAN:span<u8> n n -- n ) {: s at:n u:n :}
   JW-BACKSLASH s at SPAN:U8!
   JW-U-C s at 1+ SPAN:U8!
   4 0 do u i JW-NIBBLE s at 2 + i + SPAN:U8! loop
   at JW-U-N + ;

: JW-ESC ( SPAN:span<u8> n n -- n ) {: s at:n c:n :}
   c JW-ESC-LETTER {: l:n :}
   l 0= if s at c JW-U4 exit then
   JW-BACKSLASH s at SPAN:U8!
   l s at 1+ SPAN:U8!
   at 2 + ;

\ The offset of the first byte at or after start that escapes, or u.
: JW-RUN-END ( ^u8 n n -- n ) {: a:^ u:n start:n :}
   JW-ESC-W BYTE-VIEW {: w:^ :}
   u start ?do a i + c@ w + c@ JW-PLAIN-N <> if i unloop exit then loop
   u ;

: JW-ESC-STEP ( n n ^u8 n SPAN:span<u8> -- n n )
   {: at:n i:n a:^ u:n s :}
   a u i JW-RUN-END {: e:n :}
   e i - {: run:n :}
   run 0 > if a i + run s at SPAN:SKIP SPAN:COPY then
   e u = if at run + e exit then
   s at run + a e + c@ JW-ESC  e 1+ ;

\ Write the escaped source into s, which is exactly its escaped width.
: JW-ESC-INTO ( ^u8 n SPAN:span<u8> -- )
   {: a:^ u:n s :}
   s SPAN:LEN u = if a u s SPAN:COPY exit then
   0 0 begin dup u < while a u s JW-ESC-STEP repeat 2drop ;

: JW-DIGIT-COUNT ( n -- n ) {: u:n :}
   u JW-RADIX < if 1 exit then
   u JW-RADIX / RECURSE 1+ ;

: JW-DIGITS ( ^writer n -- ^writer ) {: u:n :}
   u JW-RADIX >= if u JW-RADIX / RECURSE then
   u JW-RADIX mod JW-ZERO + JW-C ;

\ The scalar at cursor i of valid UTF-8, and the cursor after it.
: JW-SCALAR@ ( ^u8 n n -- n n )
   UTF8:NEXT MATCH UTF8:scalar-step
      scalar OF ENDOF
      raw-byte OF 2drop E-JSON-WRITE-BYTE throw ENDOF
   ;MATCH ;

\ A scalar's escaped width in ASCII mode.
: JW-ASCII-W ( n -- n ) {: c:n :}
   c JW-BMP-LAST > if JW-PAIR-N exit then
   c JW-DEL >= if JW-U-N exit then
   c JW-ESC-W@ ;

: JW-ASCII-N-STEP ( n n ^u8 n -- n n )
   {: sum:n i:n a:^ u:n :}
   a u i JW-SCALAR@ {: c:n next:n :}
   sum c JW-ASCII-W + next ;

\ At most six per source byte, as the byte table's widths.
: JW-ASCII-N ( ^u8 n -- n ) {: a:^ u:n :}
   0 0 begin dup u < while a u JW-ASCII-N-STEP repeat drop ;

: JW-PAIR ( SPAN:span<u8> n n -- n ) {: s at:n c:n :}
   c UTF16:SCALAR>PAIR {: high:n low:n :}
   s at high JW-U4 {: mid:n :}
   s mid low JW-U4 ;

\ Write scalar c at at in ASCII mode; answer the offset after it.
: JW-ASCII-ESC ( SPAN:span<u8> n n -- n ) {: s at:n c:n :}
   c JW-BMP-LAST > if s at c JW-PAIR exit then
   c JW-DEL >= if s at c JW-U4 exit then
   c JW-ESC-W@ JW-PLAIN-N > if s at c JW-ESC exit then
   c s at SPAN:U8! at 1+ ;

: JW-ASCII-STEP ( n n ^u8 n SPAN:span<u8> -- n n )
   {: at:n i:n a:^ u:n s :}
   a u i JW-SCALAR@ {: c:n next:n :}
   s at c JW-ASCII-ESC next ;

: JW-ASCII-INTO ( ^u8 n SPAN:span<u8> -- )
   {: a:^ u:n s :}
   0 0 begin dup u < while a u s JW-ASCII-STEP repeat 2drop ;

: JW-BODY-N ( ^writer ^u8 n -- n ) {: w a:^ u:n :}
   w JW-ASCII@ if a u JW-ASCII-N else a u JW-ESC-N then ;

: JW-BODY-INTO ( ^writer ^u8 n SPAN:span<u8> -- )
   {: w a:^ u:n s :}
   w JW-ASCII@ if a u s JW-ASCII-INTO exit then
   a u s JW-ESC-INTO ;

\ Each source byte needs at least one output byte, so refuse an overlong fixed
\ span before reading it, then text that is not UTF-8. A quoted string or key is
\ a token, so its width reserves any due break with it; a string part never
\ breaks.
: JW-QUOTED ( ^writer ^u8 n bool n -- ^writer )
   {: w a:^ u:n quoted:bool sep:n :}
   a u JW-SPAN
   w a u JW-ALIAS {: off:n :}
   w u JW-ROOM drop
   w a off JW-REBASE {: src:^ :}
   src u UTF8:VALID? 0= if E-JSON-WRITE-BYTE throw then
   quoted if 1 JW-QUOTE-N else 0 0 then {: lead:n delims:n :}
   w src u JW-BODY-N {: body:n :}
   body delims JW-SIZE+ {: width:n :}
   w width sep JW-SIZE+ quoted if JW-TOKEN else JW-ROOM then drop
   w width sep + JW-TAKE {: s :}
   quoted if
      JW-DQ s 0 SPAN:U8!
      JW-DQ s width 1- SPAN:U8!
   then
   w a off JW-REBASE {: from:^ :}
   w from u s lead body SPAN:SUB JW-BODY-INTO
   JW-KEY-SEP sep s width SPAN:SKIP SPAN:COPY
   w ;

public

: WRITE-STRING ( ^writer ^u8 n -- ^writer )
   true 0 JW-QUOTED ;

: WRITE-KEY ( ^writer ^u8 n -- ^writer ) {: w a:^ u:n :}
   w JW-LAYOUT@ 2drop 0 > if JW-COLON-SP-N else JW-COLON-N then {: sep:n :}
   w a u true sep JW-QUOTED construct writer-layout keyed JW-MARK ;

: WRITE-STRING-START ( ^writer -- ^writer )
   1 JW-TOKEN JW-DQ JW-C construct writer-layout quoting JW-MARK ;

: WRITE-STRING-PART ( ^writer ^u8 n -- ^writer )
   {: w a:^ u:n :}
   w JW-QUOTING a u false 0 JW-QUOTED ;

: WRITE-STRING-END ( ^writer -- ^writer )
   JW-QUOTING JW-DQ JW-C construct writer-layout valued JW-MARK ;

: WRITE-OBJECT-START ( ^writer -- ^writer )
   JW-LBRACE JW-START ;

: WRITE-OBJECT-END ( ^writer -- ^writer )
   JW-RBRACE JW-END ;

: WRITE-ARRAY-START ( ^writer -- ^writer )
   JW-LBRACK JW-START ;

: WRITE-ARRAY-END ( ^writer -- ^writer )
   JW-RBRACK JW-END ;

: WRITE-COMMA ( ^writer -- ^writer )
   {: w :}
   w JW-LIVE 2drop drop
   w JW-LAYOUT@ {: indent:n depth:n layout:writer-layout :}
   layout MATCH writer-layout
      fresh OF E-JSON-WRITE-STATE throw ENDOF
      valued OF
         w JW-COMMA-C JW-C drop
         w depth construct writer-layout separated JW-LAYOUT!
      ENDOF
      opened OF ENDOF
      separated OF E-JSON-WRITE-STATE throw ENDOF
      keyed OF E-JSON-WRITE-STATE throw ENDOF
      quoting OF E-JSON-WRITE-STATE throw ENDOF
   ;MATCH
   w ;

: WRITE-NULL ( ^writer -- ^writer )
   s" null" WRITE-RAW ;

: WRITE-BOOL ( ^writer bool -- ^writer )
   if s" true" else s" false" then WRITE-RAW ;

: WRITE-U ( ^writer n -- ^writer ) {: u:n :}
   u 0 < if E-JSON-WRITE-BYTE throw then
   u JW-DIGIT-COUNT JW-TOKEN
   u JW-DIGITS ;

\ The minimum cell negates to itself. Its quotient by ten has a positive
\ magnitude that fits in a cell, and its final digit comes from the remainder.
\ Reserve the whole width first, so a number is written whole or not at all.
: WRITE-INT ( ^writer n -- ^writer )
   {: v:n :}
   v 0 >= if v WRITE-U exit then
   v STR-MIN-I64 = if
      STR-I64-DIGITS 1+ JW-TOKEN
      STR-MINUS JW-C
      v JW-RADIX / negate JW-DIGITS
      v JW-RADIX mod negate JW-ZERO + JW-C exit
   then
   v negate {: m:n :}
   m JW-DIGIT-COUNT 1+ JW-TOKEN
   STR-MINUS JW-C
   m JW-DIGITS ;

\ Only for a known-valid JSON fragment.
: WRITE-FIELD-RAW ( ^writer ^u8 n ^u8 n -- ^writer )
   {: kp:^ keyu:n vp:^ valu:n :}
   dup vp valu JW-ALIAS {: off:n :}
   kp keyu WRITE-KEY
   dup vp off JW-REBASE valu WRITE-RAW ;

: WRITE-FIELD-S ( ^writer ^u8 n ^u8 n -- ^writer )
   {: kp:^ keyu:n vp:^ valu:n :}
   dup vp valu JW-ALIAS {: off:n :}
   kp keyu WRITE-KEY
   dup vp off JW-REBASE valu WRITE-STRING ;

: WRITE-FIELD-U ( ^writer ^u8 n n -- ^writer ) {: kp:^ keyu:n val:n :}
   kp keyu WRITE-KEY
   val WRITE-U ;

: WRITE-FIELD-INT ( ^writer ^u8 n n -- ^writer )
   {: kp:^ keyu:n val:n :}
   kp keyu WRITE-KEY
   val WRITE-INT ;

: WRITE-FIELD-BOOL ( ^writer ^u8 n bool -- ^writer )
   {: kp:^ keyu:n val:bool :}
   kp keyu WRITE-KEY
   val WRITE-BOOL ;

: WRITE-FIELD-NULL ( ^writer ^u8 n -- ^writer )
   WRITE-KEY
   WRITE-NULL ;

;package
