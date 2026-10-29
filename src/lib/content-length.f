\ content-length.f - LSP/DAP Content-Length message framing over a blocking
\ descriptor; the module knows nothing of what a body holds. NEXT-LENGTH reads a
\ header block and BODY the body into the caller's span, so the module allocates
\ nothing. Any refusal spends the reader: the stream has no boundary left.
\ STORAGE CLASS. CALLER-OWNED reader record and buffer; SEND's header row is a
\ TASK:+USER row, TASK-LOCAL.

require src/lib/errors.f
require src/lib/string.f
require src/lib/span.f
require src/lib/task.f
require src/lib/fd.f
require src/lib/adt/option.f

package CONTENT-LENGTH
using SPAN
using FD
public

\ fd is the descriptor read; buf and cap the caller's buffer; pos is the first
\ byte not yet taken and end the first not yet read; want is the body length
\ NEXT-LENGTH answered and BODY has not read, -1 when none; max is the longest
\ body taken. A cap of zero is a reader never bound, or one a refusal spent.
STRUCTURE reader DERIVE addr
   FIELD fd fd
   FIELD buf ptr u8
   FIELD cap n
   FIELD pos n
   FIELD end n
   FIELD want n
   FIELD max n
;STRUCTURE

1024 constant LINE-CAP          \ the longest header line, its CR LF not counted

private

10 constant LF
13 constant CR
34 constant DQ
58 constant COLON
59 constant SEMICOLON
61 constant EQUALS

: FD@ ( ptr reader -- fd )  CONTENT--LENGTH-READER:FD @ ;
: BUF@ ( ptr reader -- ptr u8 )  CONTENT--LENGTH-READER:BUF @ ;
: CAP@ ( ptr reader -- n )  CONTENT--LENGTH-READER:CAP @ ;
: POS@ ( ptr reader -- n )  CONTENT--LENGTH-READER:POS @ ;
: POS! ( n ptr reader -- )  CONTENT--LENGTH-READER:POS ! ;
: END@ ( ptr reader -- n )  CONTENT--LENGTH-READER:END @ ;
: END! ( n ptr reader -- )  CONTENT--LENGTH-READER:END ! ;
: WANT@ ( ptr reader -- n )  CONTENT--LENGTH-READER:WANT @ ;
: WANT! ( n ptr reader -- )  CONTENT--LENGTH-READER:WANT ! ;
: MAX@ ( ptr reader -- n )  CONTENT--LENGTH-READER:MAX @ ;

: CAP! ( n ptr reader -- )  CONTENT--LENGTH-READER:CAP ! ;

\ A spent reader has no capacity left.
: SPEND ( ptr reader -- )
   {: r :}
   0 r CAP! ;

\ Spends the reader and throws the code.
: REFUSE ( ptr reader n -- )
   {: r code:n :}
   r SPEND
   code throw ;

: MALFORMED ( ptr reader -- )  E-CONTENT-LENGTH-MALFORMED REFUSE ;
: TRUNCATED ( ptr reader -- )  E-CONTENT-LENGTH-TRUNCATED REFUSE ;

: LIVE ( ptr reader -- )
   CAP@ 0= if E-CONTENT-LENGTH-STATE throw then ;

\ Bytes the buffer holds that nothing has taken.
: HELD ( ptr reader -- n )
   dup END@ swap POS@ - ;

\ One read into the buffer after the bytes it holds, which first move to its
\ start: false at end of file. Callers read only with room left - NEXT-LENGTH
\ on an empty buffer, MORE with at most LINE-CAP + 1 held of a buffer of
\ LINE-CAP + 2 or more - so a zero from the kernel is end of file, never a full
\ buffer.
: TOP-UP ( ptr reader -- bool )
   {: r :}
   r HELD {: held:n :}
   r BUF@ r POS@ +  r BUF@  held BYTE-COPY
   0 r POS!
   held r END!
   r CAP@ held - {: room:n :}
   r FD@ FD>N  r BUF@ held +  room read {: k:n :}
   k 0 < k room > or if r E-FS-IO REFUSE then
   held k + r END!
   k 0 > ;

\ Where the first LF the buffer holds is, counted from pos; -1 while none is.
: LF-INDEX ( ptr reader -- n )
   {: r :}
   r BUF@ r POS@ +  r HELD  LF INDEX-OF MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

\ Reads on toward a line's LF: a line already longer than LINE-CAP and its CR
\ is malformed, and end of file inside it is truncated.
: MORE ( ptr reader -- )
   {: r :}
   r HELD LINE-CAP 1+ > if r MALFORMED then
   r TOP-UP 0= if r TRUNCATED then ;

\ The line whose LF is i bytes past pos, without its CR LF, which it must end
\ with; pos moves past the LF. The line is valid until the next read.
: CUT ( n ptr reader -- ptr u8 n )
   {: i:n r :}
   r BUF@ r POS@ + {: a:ptr :}
   i 0= if r MALFORMED then
   a i 1- + c@ CR <> if r MALFORMED then
   i 1- {: u:n :}
   a u CR INDEX-OF MATCH option
      none OF ENDOF
      some OF drop r MALFORMED ENDOF
   ;MATCH
   u LINE-CAP > if r MALFORMED then
   r POS@ i + 1+ r POS!
   a u ;

\ The next header line, read on until its LF arrives.
: LINE ( ptr reader -- ptr u8 n )
   {: r :}
   begin r LF-INDEX dup 0 < while
      drop r MORE
   repeat
   r CUT ;

\ Whether a byte is RFC 7230's tchar, the bytes a header name is made of.
: TCHAR? ( n -- bool )
   {: c:n :}
   c STR-DIGIT? if true exit then
   c ASCII-LOWER {: l:n :}
   l [char] a >= l [char] z <= and if true exit then
   s" !#$%&'*+-.^_`|~" c INDEX-OF MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;

\ Whether a header name is a token: one tchar or more.
: TOKEN? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   u 0= if false exit then
   u 0 ?do a i + c@ TCHAR? 0= if false unloop exit then loop
   true ;

\ The digits from the first that is not a zero, the last kept, so that only a
\ count's value decides whether it fits a cell.
: SIGNIFICANT ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   0 begin dup u 1- < if dup a + c@ [char] 0 = else false then while 1+ repeat
   {: z:n :}
   a z +  u z - ;

\ Content-Length's value: a decimal count within the reader's maximum, blanks
\ around it aside.
: LENGTH ( ptr u8 n ptr reader -- n )
   {: a:ptr u:n r :}
   a u TRIM SIGNIFICANT STR-PARSE-POS MATCH option
      none OF r MALFORMED ENDOF
      some OF ENDOF
   ;MATCH
   dup r MAX@ > if r MALFORMED then ;

\ A parameter value without the quotes around a quoted string.
: UNQUOTE ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   u 2 < if a u exit then
   a c@ DQ <> if a u exit then
   a u 1- + c@ DQ <> if a u exit then
   a 1+  u 2 - ;

\ Whether one `name=value` parameter of a Content-Type is a charset other than
\ UTF-8: utf-8, or utf8 as older LSP clients send it, in any case, bare or
\ quoted.
: FOREIGN? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u EQUALS INDEX-OF MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: e:n :}
   e 0 < if false exit then
   a e TRIM s" charset" STR=CI 0= if false exit then
   a e 1+ +  u e 1+ -  TRIM UNQUOTE {: v:ptr vu:n :}
   v vu s" utf-8" STR=CI  v vu s" utf8" STR=CI  or 0= ;

\ A Content-Type value, its media type and parameters split at semicolons:
\ malformed when a charset parameter names another encoding than UTF-8.
: CONTENT-TYPE ( ptr u8 n ptr reader -- )
   {: a:ptr u:n r :}
   a u SEMICOLON 0 begin SPLIT-NEXT while {: p:ptr pu:n next:n :}
      p pu FOREIGN? if r MALFORMED then
      a u SEMICOLON next
   repeat drop 2drop ;

\ One header line: the body length it names if it is Content-Length, else the
\ length the lines before it named, -1 while none has.
: HEADER ( n ptr u8 n ptr reader -- n )
   {: seen:n a:ptr u:n r :}
   a u COLON INDEX-OF MATCH option
      none OF r MALFORMED ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: c:n :}
   a c TOKEN? 0= if r MALFORMED then
   a c 1+ +  u c 1+ - {: v:ptr vu:n :}
   a c s" Content-Type" STR=CI if v vu r CONTENT-TYPE then
   a c s" Content-Length" STR=CI 0= if seen exit then
   seen 0 >= if r MALFORMED then
   v vu r LENGTH ;

\ The header lines up to the empty one, and the body length they name.
: HEADERS ( ptr reader -- n )
   {: r :}
   -1 begin r LINE dup 0 > while
      r HEADER
   repeat 2drop
   dup 0 < if r MALFORMED then ;

\ The part of a body the buffer did not hold, read straight into the span. The
\ reader is spent while FD reads, so a read the kernel refuses (E-FS-IO)
\ leaves it spent; the whole body gives its capacity back.
: REST ( ptr reader SPAN:span<u8> n n -- )
   {: r s want:n k:n :}
   r CAP@ {: cap:n :}
   r SPEND
   r FD@  s k want k - SUB  READ-EXACT MATCH FD:fill
      full OF ENDOF
      eof OF drop r TRUNCATED ENDOF
   ;MATCH
   cap r CAP! ;

\ SEND's header is `Content-Length: ` (16 bytes), at most the STR-I64-DIGITS
\ digits of a cell, and CR LF CR LF.
16 STR-I64-DIGITS + 4 + constant HEADER-CAP
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and HEADER-CAP TASK:+USER HEADER-ROW drop

\ u's decimal digits into h from index i on, most significant first; the index
\ after them.
: DIGITS! ( n ptr u8 n -- n )
   {: u:n h:ptr i:n :}
   u 9 > if u 10 / h i RECURSE else i then {: j:n :}
   u 10 mod [char] 0 +  h j + c!
   j 1+ ;

\ The header for a body of u bytes, built in this task's header row.
: HEADER$ ( n -- ptr u8 n )
   {: u:n :}
   HEADER-ROW BYTE-VIEW {: h:ptr :}
   s" Content-Length: " {: p:ptr pu:n :}
   p h pu BYTE-COPY
   u h pu DIGITS! {: i:n :}
   s\" \r\n\r\n" {: t:ptr tu:n :}
   t h i + tu BYTE-COPY
   h  i tu + ;

public

\ Binds a reader to a descriptor, a buffer of at least LINE-CAP + 2 bytes and
\ the longest body it takes; a smaller buffer or a negative maximum is
\ E-CONTENT-LENGTH-STATE.
: BIND ( ptr reader fd SPAN:span<u8> n -- )
   {: r f:fd s max:n :}
   s $ {: p:ptr u:n :}
   u LINE-CAP 2 + < if E-CONTENT-LENGTH-STATE throw then
   max 0 < if E-CONTENT-LENGTH-STATE throw then
   f p u 0 0 -1 max CONTENT--LENGTH-READER:MAKE r ! ;

\ The next message's body length, or NONE when the stream ended on a message
\ boundary, the one end of input that is not an error. End of file inside a
\ header block or body is E-CONTENT-LENGTH-TRUNCATED; a block with no
\ Content-Length, or two, is E-CONTENT-LENGTH-MALFORMED. A kernel read refusal
\ (E-FS-IO) spends the reader too: inside a body it may have taken bytes the
\ caller never saw. A spent or unbound reader, or NEXT-LENGTH with a body
\ pending, is E-CONTENT-LENGTH-STATE.
: NEXT-LENGTH ( ptr reader -- option<n> )
   {: r :}
   r LIVE
   r WANT@ 0 >= if E-CONTENT-LENGTH-STATE throw then
   r HELD 0= if r TOP-UP 0= if OPTION:NONE exit then then
   r HEADERS dup r WANT!
   OPTION:SOME ;

\ The body NEXT-LENGTH announced, into the head of a span at least that long;
\ with none pending, E-CONTENT-LENGTH-STATE.
: BODY ( ptr reader SPAN:span<u8> -- )
   {: r s :}
   r LIVE
   r WANT@ {: want:n :}
   want 0 < if E-CONTENT-LENGTH-STATE throw then
   s LEN want < if E-SPAN-CAPACITY throw then
   r HELD want min {: k:n :}
   r BUF@ r POS@ +  k  s COPY
   r POS@ k + r POS!
   -1 r WANT!
   want k > if r s want k REST then ;

\ True while the buffer holds bytes nothing has taken: a message read from the
\ descriptor already, which a poll of the descriptor would not show.
: PENDING? ( ptr reader -- bool )  HELD 0 > ;

\ One message, header then body. The header is built in a row of its own, so a
\ body built in the task's string builder (src/lib/string.f SB) goes out intact.
: SEND ( fd ptr u8 n -- )
   {: f:fd a:ptr u:n :}
   u 0 < if E-SPAN-LENGTH throw then
   f u HEADER$ WRITE-FULL
   f a u WRITE-FULL ;

;using
;using
;package
