\ content-length.f - LSP/DAP Content-Length message framing over a blocking
\ descriptor; the module knows nothing of what a body holds. NEXT-LENGTH reads a
\ header block and BODY the body into the caller's span, so the module allocates
\ nothing. Any rejection spends the reader: the stream has no boundary left.
\ STORAGE CLASS. CALLER-OWNED reader record and buffer; SEND's header row is a
\ TASK:+USERS row, TASK-LOCAL.

require src/lib/error.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/span.f
require src/lib/task.f
require src/lib/fd.f
require src/lib/adt/option.f

package CONTENT-LENGTH
public

\ fd is the descriptor read; buf and cap the caller's buffer; pos is the first
\ byte not yet taken and end the first not yet read; want is the body length
\ NEXT-LENGTH answered and BODY has not read, -1 when none; max is the longest
\ body taken. A cap of zero is a reader never bound, or one a rejection spent.
STRUCTURE reader DERIVE addr
   FIELD fd fd
   FIELD buf ^u8
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

: FD@ ( ^reader -- fd )  CONTENT--LENGTH-READER:FD @ ;
: BUF@ ( ^reader -- ^u8 )  CONTENT--LENGTH-READER:BUF @ ;
: CAP@ ( ^reader -- n )  CONTENT--LENGTH-READER:CAP @ ;
: POS@ ( ^reader -- n )  CONTENT--LENGTH-READER:POS @ ;
: POS! ( n ^reader -- )  CONTENT--LENGTH-READER:POS ! ;
: END@ ( ^reader -- n )  CONTENT--LENGTH-READER:END @ ;
: END! ( n ^reader -- )  CONTENT--LENGTH-READER:END ! ;
: WANT@ ( ^reader -- n )  CONTENT--LENGTH-READER:WANT @ ;
: WANT! ( n ^reader -- )  CONTENT--LENGTH-READER:WANT ! ;
: MAX@ ( ^reader -- n )  CONTENT--LENGTH-READER:MAX @ ;

: CAP! ( n ^reader -- )  CONTENT--LENGTH-READER:CAP ! ;

\ A spent reader has no capacity left.
: SPEND ( ^reader -- )
   {: r :}
   0 r CAP! ;

\ Spends the reader and throws the code.
: REJECT ( ^reader n -- )
   {: r code:n :}
   r SPEND
   code throw ;

: MALFORMED ( ^reader -- )  E-CONTENT-LENGTH-MALFORMED REJECT ;
: TRUNCATED ( ^reader -- )  E-CONTENT-LENGTH-TRUNCATED REJECT ;

: LIVE ( ^reader -- )
   CAP@ 0= if E-CONTENT-LENGTH-STATE throw then ;

\ Bytes the buffer holds that nothing has taken.
: HELD ( ^reader -- n )
   dup END@ swap POS@ - ;

\ One read into the buffer after the bytes it holds, which first move to its
\ start: false at end of file. Callers read only with room left - NEXT-LENGTH
\ on an empty buffer, MORE with at most LINE-CAP + 1 held of a buffer of
\ LINE-CAP + 2 or more - so a zero from the kernel is end of file, never a full
\ buffer.
: TOP-UP ( ^reader -- bool )
   {: r :}
   r HELD {: held:n :}
   r BUF@ r POS@ +  r BUF@  held BYTE-COPY
   0 r POS!
   held r END!
   r CAP@ held - {: room:n :}
   r FD@ FD>N  r BUF@ held +  room read {: k:n :}
   k 0 < k room > or if r E-FS-IO REJECT then
   held k + r END!
   k 0 > ;

\ Where the first LF the buffer holds is, counted from pos; -1 while none is.
: LF-INDEX ( ^reader -- n )
   {: r :}
   r BUF@ r POS@ +  r HELD >str  LF STR:INDEX-OF MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

\ Reads on toward a line's LF: a line already longer than LINE-CAP and its CR
\ is malformed, and end of file inside it is truncated.
: MORE ( ^reader -- )
   {: r :}
   r HELD LINE-CAP 1+ > if r MALFORMED then
   r TOP-UP 0= if r TRUNCATED then ;

\ The line whose LF is i bytes past pos, without its CR LF, which it must end
\ with; pos moves past the LF. The line is valid until the next read.
: CUT ( n ^reader -- str )
   {: i:n r :}
   r BUF@ r POS@ + {: a:^ :}
   i 0= if r MALFORMED then
   a i 1- + c@ CR <> if r MALFORMED then
   i 1- {: u:n :}
   a u >str CR STR:INDEX-OF MATCH option
      none OF ENDOF
      some OF drop r MALFORMED ENDOF
   ;MATCH
   u LINE-CAP > if r MALFORMED then
   r POS@ i + 1+ r POS!
   a u >str ;

\ The next header line, read on until its LF arrives.
: LINE ( ^reader -- str )
   {: r :}
   begin r LF-INDEX dup 0 < while
      drop r MORE
   repeat
   r CUT ;

\ Whether a byte is RFC 7230's tchar, the bytes a header name is made of.
: TCHAR? ( n -- bool )
   {: c:n :}
   c STR:DIGIT? if true exit then
   c STR:ASCII-LOWER {: l:n :}
   l [char] a >= l [char] z <= and if true exit then
   s" !#$%&'*+-.^_`|~" c STR:INDEX-OF MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;

\ Whether a header name is a token: one tchar or more.
: TOKEN? ( str -- bool )
   {: s:str :}
   s STR:LEN 0= if false exit then
   s STR:LEN 0 ?do s STR:DATA i + c@ TCHAR? 0= if false unloop exit then loop
   true ;

\ The digits from the first that is not a zero, the last kept, so that only a
\ count's value decides whether it fits a cell.
: SIGNIFICANT ( str -- str )
   {: s:str :}
   0 begin dup s STR:LEN 1- < if dup s STR:DATA + c@ [char] 0 = else false then while 1+ repeat
   {: z:n :}
   s STR:DATA z +  s STR:LEN z - >str ;

\ Content-Length's value: a decimal count within the reader's maximum, blanks
\ around it aside.
: LENGTH ( str ^reader -- n )
   {: s:str r :}
   s STR:TRIM SIGNIFICANT STR:PARSE-POS MATCH option
      none OF r MALFORMED ENDOF
      some OF ENDOF
   ;MATCH
   dup r MAX@ > if r MALFORMED then ;

\ A parameter value without the quotes around a quoted string.
: UNQUOTE ( str -- str )
   {: s:str :}
   s STR:LEN 2 < if s exit then
   s STR:DATA c@ DQ <> if s exit then
   s STR:DATA s STR:LEN 1- + c@ DQ <> if s exit then
   s STR:DATA 1+  s STR:LEN 2 - >str ;

\ Whether one `name=value` parameter of a Content-Type is a charset other than
\ UTF-8: utf-8, or utf8 as older LSP clients send it, in any case, bare or
\ quoted.
: FOREIGN? ( str -- bool )
   {: s:str :}
   s EQUALS STR:INDEX-OF MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: e:n :}
   e 0 < if false exit then
   s STR:DATA e >str STR:TRIM s" charset" STR:=CI 0= if false exit then
   s STR:DATA e 1+ +  s STR:LEN e 1+ - >str
   STR:TRIM UNQUOTE {: v:str :}
   v s" utf-8" STR:=CI  v s" utf8" STR:=CI  or 0= ;

\ A Content-Type value, its media type and parameters split at semicolons:
\ malformed when a charset parameter names another encoding than UTF-8.
: CONTENT-TYPE ( str ^reader -- )
   {: s:str r :}
   s SEMICOLON 0 begin STR:SPLIT-NEXT while {: p:str next:n :}
      p FOREIGN? if r MALFORMED then
      s SEMICOLON next
   repeat drop drop ;

\ One header line: the body length it names if it is Content-Length, else the
\ length the lines before it named, -1 while none has.
: HEADER ( n str ^reader -- n )
   {: seen:n s:str r :}
   s COLON STR:INDEX-OF MATCH option
      none OF r MALFORMED ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: c:n :}
   s STR:DATA c >str TOKEN? 0= if r MALFORMED then
   s STR:DATA c 1+ +  s STR:LEN c 1+ - >str {: v:str :}
   s STR:DATA c >str s" Content-Type" STR:=CI
   if v r CONTENT-TYPE then
   s STR:DATA c >str s" Content-Length" STR:=CI 0= if seen exit then
   seen 0 >= if r MALFORMED then
   v r LENGTH ;

\ The header lines up to the empty one, and the body length they name.
: HEADERS ( ^reader -- n )
   {: r :}
   -1 begin r LINE dup STR:LEN 0 > while
      r HEADER
   repeat drop
   dup 0 < if r MALFORMED then ;

\ The part of a body the buffer did not hold, read straight into the span. The
\ reader is spent while FD reads, so a read the kernel rejects (E-FS-IO)
\ leaves it spent; the whole body gives its capacity back.
: REST ( ^reader SPAN:span<u8> n n -- )
   {: r s want:n k:n :}
   r CAP@ {: cap:n :}
   r SPEND
   r FD@  s k want k - SPAN:SUB  FD:READ-EXACT MATCH FD:fill
      full OF ENDOF
      eof OF drop r TRUNCATED ENDOF
   ;MATCH
   cap r CAP! ;

\ SEND's header is `Content-Length: ` (16 bytes), at most the STR:I64-DIGITS
\ digits of a cell, and CR LF CR LF.
16 STR:I64-DIGITS + 4 + constant HEADER-CAP
HEADER-CAP TASK:+USERS HEADER-ROW u8

\ u's decimal digits into h from index i on, most significant first; the index
\ after them.
: DIGITS! ( n ^u8 n -- n )
   {: u:n h:^ i:n :}
   u 9 > if u 10 / h i RECURSE else i then {: j:n :}
   u 10 mod [char] 0 +  h j + c!
   j 1+ ;

\ The header for a body of u bytes, built in this task's header row.
: HEADER$ ( n -- str )
   {: u:n :}
   0 HEADER-ROW {: h:^ :}
   s" Content-Length: " {: p:str :}
   p STR:DATA h p STR:LEN BYTE-COPY
   u h p STR:LEN DIGITS! {: i:n :}
   s\" \r\n\r\n" {: t:str :}
   t STR:DATA h i + t STR:LEN BYTE-COPY
   h  i t STR:LEN + >str ;

public

\ Binds a reader to a descriptor, a buffer of at least LINE-CAP + 2 bytes and
\ the longest body it takes; a smaller buffer or a negative maximum is
\ E-CONTENT-LENGTH-STATE.
: BIND ( ^reader fd SPAN:span<u8> n -- )
   {: r f:fd s max:n :}
   s SPAN:$ {: bytes:str :}
   bytes STR:LEN LINE-CAP 2 + < if E-CONTENT-LENGTH-STATE throw then
   max 0 < if E-CONTENT-LENGTH-STATE throw then
   f bytes STR:DATA bytes STR:LEN 0 0 -1 max
   CONTENT--LENGTH-READER:MAKE r ! ;

\ The next message's body length, or NONE when the stream ended on a message
\ boundary, the one end of input that is not an error. End of file inside a
\ header block or body is E-CONTENT-LENGTH-TRUNCATED; a block with no
\ Content-Length, or two, is E-CONTENT-LENGTH-MALFORMED. A kernel read rejection
\ (E-FS-IO) spends the reader too: inside a body it may have taken bytes the
\ caller never saw. A spent or unbound reader, or NEXT-LENGTH with a body
\ pending, is E-CONTENT-LENGTH-STATE.
: NEXT-LENGTH ( ^reader -- option<n> )
   {: r :}
   r LIVE
   r WANT@ 0 >= if E-CONTENT-LENGTH-STATE throw then
   r HELD 0= if r TOP-UP 0= if OPTION:NONE exit then then
   r HEADERS dup r WANT!
   OPTION:SOME ;

\ The body NEXT-LENGTH announced, into the head of a span at least that long;
\ with none pending, E-CONTENT-LENGTH-STATE.
: BODY ( ^reader SPAN:span<u8> -- )
   {: r s :}
   r LIVE
   r WANT@ {: want:n :}
   want 0 < if E-CONTENT-LENGTH-STATE throw then
   s SPAN:LEN want < if E-SPAN-CAPACITY throw then
   r HELD want min {: k:n :}
   r BUF@ r POS@ +  k >str  s SPAN:COPY
   r POS@ k + r POS!
   -1 r WANT!
   want k > if r s want k REST then ;

\ True while the buffer holds bytes nothing has taken: a message read from the
\ descriptor already, which a poll of the descriptor would not show.
: PENDING? ( ^reader -- bool )  HELD 0 > ;

\ One message, header then body. The header is built in a row of its own, so a
\ body built in the task's string builder (src/lib/string.f SB) goes out intact.
: SEND ( fd str -- )
   {: f:fd s:str :}
   s STR:LEN 0 < if E-SPAN-LENGTH throw then
   f s STR:LEN HEADER$ FD:WRITE-FULL
   f s FD:WRITE-FULL ;

;package
