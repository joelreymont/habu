\ reject.f - the text of an XML rejection, thrown with its code; part of package
\ XML (src/lib/xml.f). The text is `L:C: expected <expected>, found <found>;
\ <fix>`, L and C counting bytes from the first byte of the span given, which
\ sits at a line and column the caller gives. <found> is the token at the
\ place: the end of the input; `byte $HH` for a control byte or a byte no UTF-8
\ character starts; else in backticks a markup byte, a space, the run of whole
\ characters, each an ASCII graphic byte but markup or a UTF-8 character above
\ ASCII, or at `&` the `&`, the run after it and a `;` closing it; at most
\ FOUND-CAP bytes of it cut on a character's first byte, and then `...` when
\ cut or when the run reaches the span's end and input follows the span. A
\ character the span's end cuts, with input after it, counts as reaching the
\ span's end, and alone shows as `...`. Found counts over the span and never
\ refills. The text's size is counted before it is written, so it needs no
\ fixed buffer and never touches SB.
\ REJECT-UNITS places in UTF-16 bytes: a line ends at an LF unit, a column
\ counts units, and <found> is the end of the input, `byte $HH` for a lone
\ last byte, or `unit $HHHH`.

require src/lib/prelude.f
require src/lib/utf8-scalar.f
require src/lib/fmt.f
require src/lib/reason.f
require src/lib/memory.f
require src/core/rejection.f

package XML

32 constant FOUND-CAP

\ Where a span's first byte sits in the whole input, and whether input follows
\ the span.
STRUCTURE place
   FIELD line n
   FIELD column n
   FIELD more bool
;STRUCTURE

\ The place of a span counted from its own first byte, with nothing after it.
: WHOLE ( -- place ) 1 1 false PLACE-MAKE ;

0 constant FOUND-END
1 constant FOUND-BYTE
2 constant FOUND-TOKEN
3 constant FOUND-CUT
4 constant FOUND-UNIT-LE
5 constant FOUND-UNIT-BE

: END$ ( -- ^u8 n ) s" the end of the input" ;
: BYTE$ ( -- ^u8 n ) s" byte $" ;
: UNIT$ ( -- ^u8 n ) s" unit $" ;
: CUT$ ( -- ^u8 n ) s" ..." ;
: TICK$ ( -- ^u8 n ) s" `" ;
: LINE-SEP$ ( -- ^u8 n ) s" :" ;
: EXPECTED-SEP$ ( -- ^u8 n ) s" : expected " ;
: FOUND-SEP$ ( -- ^u8 n ) s" , found " ;
: FIX-SEP$ ( -- ^u8 n ) s" ; " ;

: MARKUP? ( n -- bool )
   dup $3C = over $3E = or over $26 = or over $3B = or over $3D = or
   over $2F = or over $22 = or over $27 = or over $3F = or swap $21 = or ;

\ An ASCII byte of a run: graphic and no markup byte.
: RUN? ( n -- bool )
   dup $20 > over $7F < and swap MARKUP? 0= and ;

\ The bytes of a span shown within FOUND-CAP, cut back to a character's first
\ byte, and FOUND-CUT when any are dropped, else FOUND-TOKEN.
: CAPPED ( ^u8 n -- n n ) {: a:^ u:n :}
   u FOUND-CAP <= if u FOUND-TOKEN exit then
   a u FOUND-CAP UTF8:FIT FOUND-CUT ;

\ The end of the run's character at off, or off when no run character is there.
: RUN-STEP ( ^u8 n n -- n ) {: a:^ u:n off:n :}
   a off + c@ {: b:n :}
   b $80 < if b RUN? if off 1+ else off then exit then
   a u off UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF nip ENDOF
      raw-byte OF 2drop off ENDOF
   ;MATCH ;

\ The end of the run from start; the scan stops FOUND-CAP + 1 bytes past off,
\ and the run's last character may end past that.
: RUN-END ( ^u8 n n n -- n ) {: a:^ u:n off:n start:n :}
   off FOUND-CAP + 1+ u min {: lim:n :}
   start begin
      dup lim < if dup a u rot RUN-STEP tuck < else false then
   while repeat ;

\ The token from off to end, whose run ends at run.
: FOUND-SPAN ( ^u8 n n n n bool -- n n )
   {: a:^ u:n off:n run:n end:n more:bool :}
   a off + end off - CAPPED {: len:n form:n :}
   more run u = a u run UTF8:CUT? or and if len FOUND-CUT exit then
   len form ;

: FOUND-RUN ( ^u8 n n bool -- n n ) {: a:^ u:n off:n more:bool :}
   a u off off RUN-END {: run:n :}
   a u off run run more FOUND-SPAN ;

: FOUND-REF ( ^u8 n n bool -- n n ) {: a:^ u:n off:n more:bool :}
   a u off off 1+ RUN-END {: run:n :}
   run u < if a run + c@ $3B = else false then
   if run 1+ else run then {: end:n :}
   a u off run end more FOUND-SPAN ;

\ The length and form of the token at off.
: FOUND-FORM ( ^u8 n n bool -- n n ) {: a:^ u:n off:n more:bool :}
   off u >= if 0 FOUND-END exit then
   more a u off UTF8:CUT? and if 0 FOUND-CUT exit then
   a off + c@ {: b:n :}
   b $26 = if a u off more FOUND-REF exit then
   a u off RUN-STEP off > if a u off more FOUND-RUN exit then
   b $80 >= b $20 < or b $7F = or if 1 FOUND-BYTE exit then
   1 FOUND-TOKEN ;

: FOUND-SIZE ( n n -- n ) {: len:n form:n :}
   form FOUND-END = if END$ nip exit then
   form FOUND-BYTE = if BYTE$ nip 2 + exit then
   form FOUND-CUT = if len CUT$ nip + 2 + exit then
   form FOUND-UNIT-LE = form FOUND-UNIT-BE = or if UNIT$ nip 4 + exit then
   len 2 + ;

: PUT ( ^u8 n ^u8 n -- ^u8 n ) {: buf:^ at:n a:^ u:n :}
   a buf at + u BYTE-COPY
   buf at u + ;

: PUT-HEX ( ^u8 n n -- ^u8 n )   2 FMT:HEX-WIDTH$ PUT ;

: PUT-FOUND ( ^u8 n ^u8 n n n -- ^u8 n )
   {: buf:^ at:n a:^ off:n len:n form:n :}
   form FOUND-END = if buf at END$ PUT exit then
   form FOUND-BYTE = if buf at BYTE$ PUT a off + c@ PUT-HEX exit then
   form FOUND-UNIT-LE = if
      buf at UNIT$ PUT a off + 1+ c@ PUT-HEX a off + c@ PUT-HEX exit
   then
   form FOUND-UNIT-BE = if
      buf at UNIT$ PUT a off + c@ PUT-HEX a off + 1+ c@ PUT-HEX exit
   then
   buf at TICK$ PUT a off + len PUT
   form FOUND-CUT = if CUT$ PUT then
   TICK$ PUT ;

: TEXT-SIZE ( n n n n n n -- n ) {: line:n col:n len:n form:n eu:n fu:n :}
   line FMT:INT$ nip col FMT:INT$ nip + LINE-SEP$ nip +
   EXPECTED-SEP$ nip + FOUND-SEP$ nip + FIX-SEP$ nip + eu + fu +
   len form FOUND-SIZE + ;

: REJECT-SCOPE
   ( n n ^u8 n n n ^u8 n ^u8 n n ^u8 NUM:alloc-byte-len -- )
   drop {: line:n col:n a:^ off:n len:n form:n
      ea:^ eu:n fa:^ fu:n code:n buf:^ :}
   buf 0 line FMT:INT$ PUT LINE-SEP$ PUT col FMT:INT$ PUT
   EXPECTED-SEP$ PUT ea eu PUT FOUND-SEP$ PUT
   a off len form PUT-FOUND
   FIX-SEP$ PUT fa fu PUT
   code REASON:THROW ;

\ The last throw is never reached: it shows the checker that the word never
\ returns, so a branch that calls it needs no result.
: REJECT-WRITE ( n n ^u8 n n n ^u8 n ^u8 n n -- )
   {: line:n col:n a:^ off:n len:n form:n ea:^ eu:n fa:^ fu:n code:n :}
   line col a off len form ea eu fa fu code
   line col len form eu fu TEXT-SIZE MEM:BYTES-ALLOC-LEN
   [: REJECT-SCOPE ;] MEM:WITH-BYTES
   code throw ;

\ Rejects at off in the span a u, whose first byte sits at the place at.
: REJECT-FROM ( ^u8 n place n ^u8 n ^u8 n n -- )
   {: a:^ u:n at:place off:n ea:^ eu:n fa:^ fu:n code:n :}
   at PLACE-UNMAKE {: bl:n bc:n more:bool :}
   a a off + bl bc 0 REJECTION:POSITION drop {: line:n col:n :}
   a u off more FOUND-FORM {: len:n form:n :}
   line col a off len form ea eu fa fu code REJECT-WRITE ;

\ The line and column of byte off, even, in UTF-16 bytes from a, big-endian
\ when be is set.
: UNIT-POSITION ( ^u8 n bool -- n n ) {: a:^ off:n be:bool :}
   1 1 off 0 ?do
      a i + c@ a i + 1+ c@ be if swap then
      0= swap $0A = and if drop 1+ 1 else 1+ then
   2 +loop ;

\ Rejects at byte off of the UTF-16 bytes a u, big-endian when be is set.
: REJECT-UNITS ( ^u8 n n bool ^u8 n ^u8 n n -- )
   {: a:^ u:n off:n be:bool ea:^ eu:n fa:^ fu:n code:n :}
   a off be UNIT-POSITION {: line:n col:n :}
   off u >= if 0 FOUND-END else
      u off - 1 = if 1 FOUND-BYTE else
         2 be if FOUND-UNIT-BE else FOUND-UNIT-LE then
      then
   then {: len:n form:n :}
   line col a off len form ea eu fa fu code REJECT-WRITE ;

;package
