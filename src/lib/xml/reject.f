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

\ A place's kind: its columns count UTF-16 units for COUNTING-UNITS, else bytes;
\ input follows the span only for COUNTING-MORE.
ENUM counting DERIVE eq bytes more units ;ENUM

\ Where a span's first byte sits in the whole input.
STRUCTURE place
   FIELD line n
   FIELD column n
   FIELD kind counting
;STRUCTURE

\ The place of a span counted from its own first byte.
: WHOLE ( -- place ) 1 1 COUNTING-BYTES PLACE-MAKE ;

\ The line and column of byte off of the UTF-8 bytes s, counting UTF-16
\ units from line l, column c.
: UTF16-POSITION ( str n n n -- n n ) {: s:str off:n l:n c:n :}
   s STR:DATA s STR:DATA off + l c 0 REJECTION:POSITION drop {: line:n col:n :}
   line l = if 0 c else off col - 1+ 1 then {: from:n base:n :}
   line base s STR:DATA from + off from - >str UTF16:UNITS + ;

\ The line and column of byte off of the span s, whose first byte sits at
\ the place at.
: PLACE-POSITION ( str place n -- n n ) {: s:str at:place off:n :}
   at PLACE-UNMAKE {: l:n c:n kind:counting :}
   kind COUNTING-UNITS COUNTING-EQ if s off l c UTF16-POSITION exit then
   s STR:DATA s STR:DATA off + l c 0 REJECTION:POSITION drop ;

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
: CAPPED ( str -- n n ) {: s:str :}
   s STR:LEN REASON:QUOTE-MOST <= if s STR:LEN FOUND-TOKEN exit then
   s REASON:QUOTE-MOST UTF8:FIT FOUND-CUT ;

\ The end of the run's character at off, or off when no run character is there.
: RUN-STEP ( str n -- n ) {: s:str off:n :}
   s STR:DATA off + c@ {: b:n :}
   b $80 < if b RUN? if off 1+ else off then exit then
   s off UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF nip ENDOF
      raw-byte OF 2drop off ENDOF
   ;MATCH ;

\ The end of the run from start; the scan stops REASON:QUOTE-MOST + 1 bytes
\ past off, and the run's last character may end past that.
: RUN-END ( str n n -- n ) {: s:str off:n start:n :}
   off REASON:QUOTE-MOST + 1+ s STR:LEN min {: lim:n :}
   start begin
      dup lim < if dup s swap RUN-STEP tuck < else false then
   while repeat ;

\ The token from off to end, whose run ends at run.
: FOUND-SPAN ( str n n n bool -- n n )
   {: s:str off:n run:n end:n more:bool :}
   s STR:DATA off + end off - >str CAPPED {: len:n form:n :}
   more run s STR:LEN = s run UTF8:CUT? or and if len FOUND-CUT exit then
   len form ;

: FOUND-RUN ( str n bool -- n n ) {: s:str off:n more:bool :}
   s off off RUN-END {: run:n :}
   s off run run more FOUND-SPAN ;

: FOUND-REF ( str n bool -- n n ) {: s:str off:n more:bool :}
   s off off 1+ RUN-END {: run:n :}
   run s STR:LEN < if s STR:DATA run + c@ $3B = else false then
   if run 1+ else run then {: end:n :}
   s off run end more FOUND-SPAN ;

\ The length and form of the token at off.
: FOUND-FORM ( str n bool -- n n ) {: s:str off:n more:bool :}
   off s STR:LEN >= if 0 FOUND-END exit then
   more s off UTF8:CUT? and if 0 FOUND-CUT exit then
   s STR:DATA off + c@ {: b:n :}
   b $26 = if s off more FOUND-REF exit then
   s off RUN-STEP off > if s off more FOUND-RUN exit then
   b $80 >= b $20 < or b $7F = or if 1 FOUND-BYTE exit then
   1 FOUND-TOKEN ;

: PUT-FOUND ( str n n n -- ) {: s:str off:n len:n form:n :}
   form FOUND-END = if END$ REASON:APPEND exit then
   form FOUND-BYTE = if s STR:DATA off + c@ REASON:BYTE exit then
   form FOUND-UNIT-LE = if
      UNIT$ REASON:APPEND
      s STR:DATA off + 1+ c@ 2 FMT:HEX-WIDTH$ REASON:APPEND
      s STR:DATA off + c@ 2 FMT:HEX-WIDTH$ REASON:APPEND exit
   then
   form FOUND-UNIT-BE = if
      UNIT$ REASON:APPEND
      s STR:DATA off + c@ 2 FMT:HEX-WIDTH$ REASON:APPEND
      s STR:DATA off + 1+ c@ 2 FMT:HEX-WIDTH$ REASON:APPEND exit
   then
   s STR:DATA off + len >str  form FOUND-CUT =  REASON:QUOTE ;

\ Writes the found part and the fix, and throws code with the text.
: WRITE-TAIL ( str n n n str n -- )
   {: s:str off:n len:n form:n fs:str code:n :}
   REASON:FOUND
   s off len form PUT-FOUND
   REASON:FIX fs REASON:APPEND
   code REASON:STOP ;

\ Opens the text of a rejection at off in the span s, whose first byte sits
\ at the place at, up to its expected part.
: HEAD-FROM ( str place n -- ) {: s:str at:place off:n :}
   REASON:OPEN
   s at off PLACE-POSITION REASON:AT REASON:EXPECTED ;

\ Writes the found part at off of the span s and the fix, and throws code
\ with the text.
: TAIL-FROM ( str place n str n -- )
   {: s:str at:place off:n fs:str code:n :}
   at PLACE-UNMAKE nip nip COUNTING-MORE COUNTING-EQ {: more:bool :}
   s off more FOUND-FORM {: len:n form:n :}
   s off len form fs code WRITE-TAIL ;

\ Rejects at off in the span s, whose first byte sits at the place at.
: REJECT-FROM ( str place n str str n -- )
   {: s:str at:place off:n es:str fs:str code:n :}
   s at off HEAD-FROM
   es REASON:APPEND
   s at off fs code TAIL-FROM ;

\ The line and column of byte off, even, in the UTF-16 bytes s, big-endian
\ when be is set.
: UNIT-POSITION ( str n bool -- n n ) {: s:str off:n be:bool :}
   1 1 off 0 ?do
      s STR:DATA i + c@ s STR:DATA i + 1+ c@ be if swap then
      0= swap $0A = and if drop 1+ 1 else 1+ then
   2 +loop ;

\ Rejects at byte off of the UTF-16 bytes s, big-endian when be is set.
: REJECT-UNITS ( str n bool str str n -- )
   {: s:str off:n be:bool es:str fs:str code:n :}
   s off be UNIT-POSITION {: line:n col:n :}
   off s STR:LEN >= if 0 FOUND-END else
      s STR:LEN off - 1 = if 1 FOUND-BYTE else
         2 be if FOUND-UNIT-BE else FOUND-UNIT-LE then
      then
   then {: len:n form:n :}
   REASON:OPEN
   line col REASON:AT REASON:EXPECTED
   es REASON:APPEND
   s off len form fs code WRITE-TAIL ;

;package
