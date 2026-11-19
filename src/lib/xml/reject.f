split-str
\ reject.f - the text of an XML rejection, thrown with its code; part of package
\ XML (src/lib/xml.f). The text is `L:C: expected <expected>, found <found>;
\ <fix>`, L and C counted from the first byte of the span given, which sits at
\ the place the caller gives. <found> is the token at the place: the end of
\ the input; `byte $HH` for a control byte or a byte no UTF-8 character starts;
\ else in backticks a markup byte, a space, the run of whole characters, each an
\ ASCII graphic byte but markup or a UTF-8 character above ASCII, or at `&` the
\ `&`, the run after it and a `;` closing it; at most REASON:QUOTE-MOST bytes of
\ it cut on a character's first byte and made visible, and then `...` when cut
\ or when the run reaches the span's end and input follows the span. A
\ character the span's end cuts, with input after it, counts as reaching the
\ span's end, and alone shows as `...`. Found counts over the span and never
\ refills. REASON writes the text, so it never touches SB.
\ REJECT-UNITS places in UTF-16 bytes: a line ends at an LF unit, a column
\ counts units, and <found> is the end of the input, `byte $HH` for a lone
\ last byte, or `unit $HHHH`.

require src/lib/prelude.f
require src/lib/utf8-scalar.f
require src/lib/utf16.f
require src/lib/fmt.f
require src/lib/reason.f
require src/core/rejection.f

package XML

\ A place's kind: its columns count UTF-16 units for COUNT-UNITS, else bytes;
\ input follows the span only for COUNT-MORE.
0 constant COUNT-BYTES
1 constant COUNT-MORE
2 constant COUNT-UNITS

\ Where a span's first byte sits in the whole input.
STRUCTURE place
   FIELD line n
   FIELD column n
   FIELD kind n
;STRUCTURE

\ The place of a span counted from its own first byte.
: WHOLE ( -- place ) 1 1 COUNT-BYTES PLACE-MAKE ;

\ The line and column of byte off of the UTF-8 bytes from a, counting UTF-16
\ units from line l, column c.
: UTF16-POSITION ( ^u8 n n n -- n n ) {: a:^ off:n l:n c:n :}
   a a off + l c 0 REJECTION:POSITION drop {: line:n col:n :}
   line l = if 0 c else off col - 1+ 1 then {: from:n base:n :}
   line base a from + off from - UTF16:UNITS + ;

\ The line and column of byte off of the span a, whose first byte sits at the
\ place at.
: PLACE-POSITION ( ^u8 place n -- n n ) {: a:^ at:place off:n :}
   at PLACE-UNMAKE {: l:n c:n kind:n :}
   kind COUNT-UNITS = if a off l c UTF16-POSITION exit then
   a a off + l c 0 REJECTION:POSITION drop ;

0 constant FOUND-END
1 constant FOUND-BYTE
2 constant FOUND-TOKEN
3 constant FOUND-CUT
4 constant FOUND-UNIT-LE
5 constant FOUND-UNIT-BE

: END$ ( -- str ) s" the end of the input" ;
: UNIT$ ( -- str ) s" unit $" ;

: MARKUP? ( n -- bool )
   dup $3C = over $3E = or over $26 = or over $3B = or over $3D = or
   over $2F = or over $22 = or over $27 = or over $3F = or swap $21 = or ;

\ An ASCII byte of a run: graphic and no markup byte.
: RUN? ( n -- bool )
   dup $20 > over $7F < and swap MARKUP? 0= and ;

\ The bytes of a span shown within REASON:QUOTE-MOST, cut back to a character's
\ first byte, and FOUND-CUT when any are dropped, else FOUND-TOKEN.
: CAPPED ( ^u8 n -- n n ) {: a:^ u:n :}
   u REASON:QUOTE-MOST <= if u FOUND-TOKEN exit then
   a u REASON:QUOTE-MOST UTF8:FIT FOUND-CUT ;

\ The end of the run's character at off, or off when no run character is there.
: RUN-STEP ( str n -- n ) {: a:^ u:n off:n :}
   a off + c@ {: b:n :}
   b $80 < if b RUN? if off 1+ else off then exit then
   a u off UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF nip ENDOF
      raw-byte OF 2drop off ENDOF
   ;MATCH ;

\ The end of the run from start; the scan stops REASON:QUOTE-MOST + 1 bytes
\ past off, and the run's last character may end past that.
: RUN-END ( str n n -- n ) {: a:^ u:n off:n start:n :}
   off REASON:QUOTE-MOST + 1+ u min {: lim:n :}
   start begin
      dup lim < if dup a u rot RUN-STEP tuck < else false then
   while repeat ;

\ The token from off to end, whose run ends at run.
: FOUND-SPAN ( str n n n bool -- n n )
   {: a:^ u:n off:n run:n end:n more:bool :}
   a off + end off - CAPPED {: len:n form:n :}
   more run u = a u run UTF8:CUT? or and if len FOUND-CUT exit then
   len form ;

: FOUND-RUN ( str n bool -- n n ) {: a:^ u:n off:n more:bool :}
   a u off off RUN-END {: run:n :}
   a u off run run more FOUND-SPAN ;

: FOUND-REF ( str n bool -- n n ) {: a:^ u:n off:n more:bool :}
   a u off off 1+ RUN-END {: run:n :}
   run u < if a run + c@ $3B = else false then
   if run 1+ else run then {: end:n :}
   a u off run end more FOUND-SPAN ;

\ The length and form of the token at off.
: FOUND-FORM ( str n bool -- n n ) {: a:^ u:n off:n more:bool :}
   off u >= if 0 FOUND-END exit then
   more a u off UTF8:CUT? and if 0 FOUND-CUT exit then
   a off + c@ {: b:n :}
   b $26 = if a u off more FOUND-REF exit then
   a u off RUN-STEP off > if a u off more FOUND-RUN exit then
   b $80 >= b $20 < or b $7F = or if 1 FOUND-BYTE exit then
   1 FOUND-TOKEN ;

: PUT-FOUND ( ^u8 n n n -- ) {: a:^ off:n len:n form:n :}
   form FOUND-END = if END$ REASON:APPEND exit then
   form FOUND-BYTE = if a off + c@ REASON:BYTE exit then
   form FOUND-UNIT-LE = if
      UNIT$ REASON:APPEND
      a off + 1+ c@ 2 FMT:HEX-WIDTH$ REASON:APPEND
      a off + c@ 2 FMT:HEX-WIDTH$ REASON:APPEND exit
   then
   form FOUND-UNIT-BE = if
      UNIT$ REASON:APPEND
      a off + c@ 2 FMT:HEX-WIDTH$ REASON:APPEND
      a off + 1+ c@ 2 FMT:HEX-WIDTH$ REASON:APPEND exit
   then
   a off + len  form FOUND-CUT =  REASON:QUOTE ;

\ Writes the found part and the fix, and throws code with the text.
: WRITE-TAIL ( ^u8 n n n str n -- )
   {: a:^ off:n len:n form:n fa:^ fu:n code:n :}
   REASON:FOUND
   a off len form PUT-FOUND
   REASON:FIX fa fu REASON:APPEND
   code REASON:STOP ;

\ Opens the text of a rejection at off in the span a, whose first byte sits at
\ the place at, up to its expected part.
: HEAD-FROM ( ^u8 place n -- ) {: a:^ at:place off:n :}
   REASON:OPEN
   a at off PLACE-POSITION REASON:AT REASON:EXPECTED ;

\ Writes the found part at off of the span a u and the fix, and throws code
\ with the text.
: TAIL-FROM ( str place n str n -- )
   {: a:^ u:n at:place off:n fa:^ fu:n code:n :}
   at PLACE-UNMAKE nip nip COUNT-MORE = {: more:bool :}
   a u off more FOUND-FORM {: len:n form:n :}
   a off len form fa fu code WRITE-TAIL ;

\ Rejects at off in the span a u, whose first byte sits at the place at.
: REJECT-FROM ( str place n str str n -- )
   {: a:^ u:n at:place off:n ea:^ eu:n fa:^ fu:n code:n :}
   a at off HEAD-FROM
   ea eu REASON:APPEND
   a u at off fa fu code TAIL-FROM ;

\ The line and column of byte off, even, in UTF-16 bytes from a, big-endian
\ when be is set.
: UNIT-POSITION ( ^u8 n bool -- n n ) {: a:^ off:n be:bool :}
   1 1 off 0 ?do
      a i + c@ a i + 1+ c@ be if swap then
      0= swap $0A = and if drop 1+ 1 else 1+ then
   2 +loop ;

\ Rejects at byte off of the UTF-16 bytes a u, big-endian when be is set.
: REJECT-UNITS ( str n bool str str n -- )
   {: a:^ u:n off:n be:bool ea:^ eu:n fa:^ fu:n code:n :}
   a off be UNIT-POSITION {: line:n col:n :}
   off u >= if 0 FOUND-END else
      u off - 1 = if 1 FOUND-BYTE else
         2 be if FOUND-UNIT-BE else FOUND-UNIT-LE then
      then
   then {: len:n form:n :}
   REASON:OPEN
   line col REASON:AT REASON:EXPECTED
   ea eu REASON:APPEND
   a off len form fa fu code WRITE-TAIL ;

;package
