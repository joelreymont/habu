\ read.f - the JSON pull/cursor reader. A file of package JSON
\ (src/lib/json.f).
\
\ FIRST-REPEAT groups an object's decoded keys a byte at a time: a group
\ agreeing on its first d bytes splits by the first byte after d its
\ keys do not all share, a key's end a class of its own, each part kept
\ in document order, until a part is one key, two keys compared whole or
\ keys that all end, which are equal. The second of equal keys is their
\ earliest repeat; each byte is read a fixed number of times.

require src/lib/error.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/string.f
require src/lib/float.f
require src/lib/adt/option.f
require src/lib/utf16.f
require src/lib/utf8-scalar.f
require src/lib/vector.f
require src/lib/heap.f
require src/lib/reason.f
require src/core/rejection.f

\ The E-JSON-READ-* codes stay global names, as callers spell them.
-3900 constant E-JSON-READ-FIRST
-3999 constant E-JSON-READ-LAST
-3900 s" a bare word, a bad byte or a trailing comma for a value"
ERROR:CODE E-JSON-READ-MALFORMED
-3901 s" an unterminated string or a raw control byte in a string"
ERROR:CODE E-JSON-READ-STRING
-3902 s" an invalid backslash or \uXXXX escape sequence"
ERROR:CODE E-JSON-READ-ESCAPE
-3903 s" a lone or mismatched UTF-16 surrogate in a \u escape"
ERROR:CODE E-JSON-READ-SURROGATE
-3905 s" a byte other than white space after the top-level value"
ERROR:CODE E-JSON-READ-TRAILING
-3906 s" a malformed number token or an integer overflow"
ERROR:CODE E-JSON-READ-NUMBER
-3907 s" the input ended where a value, container or key belongs"
ERROR:CODE E-JSON-READ-EOF
-3908 s" no ':' after an object key"
ERROR:CODE E-JSON-READ-COLON
-3909 s" no ',' or matching container close where one belongs"
ERROR:CODE E-JSON-READ-COMMA
-3910 s" an accessor on the wrong token kind, or a call the reader's phase does not allow"
ERROR:CODE E-JSON-READ-STATE

package JSON

public

-3914 s" a negative length, or a null source of positive length"
ERROR:CODE E-READ-SOURCE
-3915 s" no object, an unknown, repeated or missing member, a member name or value its reader or a program rejected, or a value of another kind than the word reads"
ERROR:CODE E-MEMBER

DEFLINEAR JSON:reader

\ kinds is the container stack's first 64 levels, bit i set when level i is an
\ object, so a document nested at most 64 deep parses with no allocation; more
\ is the mapping holding the rest, more-cells cells of it, grown by doubling,
\ and more-cells 0 means the record owns no mapping. No valid document is
\ rejected for its depth; a failed growth is recorded as the reader's failure.
STRUCTURE cursor OPAQUE DERIVE addr
   FIELD src ^u8
   FIELD src-len n
   FIELD pos n
   FIELD depth n
   FIELD state n
   FIELD kind n
   FIELD tok-at n
   FIELD tok-len n
   FIELD dst ^u8
   FIELD dst-cap n
   FIELD out n
   FIELD in n
   FIELD mode n
   FIELD matched n
   FIELD fail n
   FIELD kinds n
   FIELD more ^u8
   FIELD more-cells n
;STRUCTURE

private

9 constant TAB
10 constant LF
13 constant CH-CR
32 constant SP
34 constant DQ
43 constant PLUS
44 constant COMMA
45 constant MINUS
46 constant DOT
48 constant ZERO
58 constant COLON
69 constant EXP-UPPER
91 constant LBRACK
92 constant BACKSLASH
93 constant RBRACK
101 constant EXP-LOWER
102 constant CH-F
110 constant CH-N
116 constant CH-T
117 constant CH-U
123 constant LBRACE
125 constant RBRACE

16 constant HEX-BASE

$80 constant UTF1-MAX
$800 constant UTF2-MAX
$10000 constant UTF3-MAX
$C2 constant UTF2-MIN
$E0 constant UTF2-END
$F0 constant UTF3-END
$F5 constant UTF4-END
$A0 constant UTF-E0-MIN
$90 constant UTF-F0-MIN
$ED constant UTF-SUR-LEAD
$9F constant UTF-SUR-MAX
$F4 constant UTF-MAX-LEAD
$8F constant UTF-MAX-SECOND
$3F constant UTF-MASK
$80 constant UTF-CONT
$C0 constant UTF2-LEAD
$E0 constant UTF3-LEAD
$F0 constant UTF4-LEAD
$D800 constant SUR-HI
$DC00 constant SUR-LO
$E000 constant SUR-END

public

0 constant T-OBJ
1 constant T-OBJ-END
2 constant T-ARR
3 constant T-ARR-END
4 constant T-KEY
5 constant T-STR
6 constant T-INT
7 constant T-FLOAT
8 constant T-TRUE
9 constant T-FALSE
10 constant T-NULL
11 constant T-END

: KIND$ ( n -- str )
   case
      T-OBJ of s" an object" endof
      T-OBJ-END of s" an object's end" endof
      T-ARR of s" an array" endof
      T-ARR-END of s" an array's end" endof
      T-KEY of s" a key" endof
      T-STR of s" a string" endof
      T-INT of s" an integer" endof
      T-FLOAT of s" a number with a fraction or exponent" endof
      T-TRUE of s" true" endof
      T-FALSE of s" false" endof
      T-NULL of s" null" endof
      T-END of s" the text's end" endof
      s" no token kind" swap
   endcase ;

private

99 constant RETRY

\ Cursor state machine (STATE): a value token drives one transition each.
\  ST-VALUE   value required (doc start / after ':' / after ',' in an array)
\  ST-ELEM    array just opened: a value or ']'
\  ST-KEY     object just opened: a key string or '}'
\  ST-MEMBER  after ',' in an object: a key string (no '}': trailing comma)
\  ST-SEP     value complete inside a container: ',' or the matching close
\  ST-DONE    top-level value complete: only whitespace then JSON:T-END
\ A ',' in ST-SEP is consumed silently (STEP returns RETRY and READ-NEXT loops);
\ container closes are emitted as T-OBJ-END / T-ARR-END.
0 constant ST-VALUE
1 constant ST-ELEM
2 constant ST-KEY
3 constant ST-MEMBER
4 constant ST-SEP
5 constant ST-DONE

0 constant UMODE-WRITE
1 constant UMODE-MATCH

64 constant WORD-BITS        \ container levels per stack word
512 constant FIRST-MORE      \ cells in the first mapping: 32768 more levels

\ Outside JSON a reader comes only from READ-INIT and ends only at
\ READ-CLOSE.
LINEAR: MINT-READER ( ^cursor -- JSON:reader )
LINEAR: ERASE-READER ( JSON:reader -- ^cursor )

\ Keep the token live while exposing its record.
: READER>STATE ( JSON:reader -- JSON:reader ^cursor )
   ERASE-READER dup MINT-READER swap ;

: INIT-STATE ( ^cursor str -- ) {: state:^ source:str :}
   source STR:DATA state CURSOR-SRC !
   source STR:LEN state CURSOR-SRC-LEN !
   0 state CURSOR-POS !
   0 state CURSOR-DEPTH !
   ST-VALUE state CURSOR-STATE !
   T-END state CURSOR-KIND !
   0 state CURSOR-TOK-AT !
   0 state CURSOR-TOK-LEN !
   UMODE-WRITE state CURSOR-MODE !
   1 state CURSOR-MATCHED !
   0 state CURSOR-FAIL ! ;

\ Reads a source byte its caller has bounds-checked.
: BYTE-AT ( ^cursor n -- n ) {: state:^ idx:n :}
   state CURSOR-SRC @ idx + c@ ;

: EOF? ( ^cursor -- bool ) {: state:^ :}
   state CURSOR-POS @ state CURSOR-SRC-LEN @ >= ;

: PEEK ( ^cursor -- n ) {: state:^ :}
   state state CURSOR-POS @ BYTE-AT ;

: ADVANCE ( ^cursor -- ) {: state:^ :}
   state CURSOR-POS dup @ 1+ swap ! ;

: MORE-SPAN ( ^cursor -- SPAN:span<u8> ) {: state:^ :}
   state CURSOR-MORE @ state CURSOR-MORE-CELLS @ cells SPAN:MAKE ;

\ Frees the stack mapping a cursor owns; freeing the cursor's storage first leaks it.
: MORE-RELEASE ( ^cursor -- ) {: state:^ :}
   state MORE-SPAN {: old :}
   NULL-PTR state CURSOR-MORE !
   0 state CURSOR-MORE-CELLS !
   old MEM:FREE-SPAN ;

\ Make stack word w (w >= 1 lives in cell w-1 of the mapping) exist. Levels
\ are pushed one at a time, so one doubling always covers w.
: MORE-ENSURE ( ^cursor n -- ) {: state:^ w:n :}
   w state CURSOR-MORE-CELLS @ <= if exit then
   state CURSOR-MORE-CELLS @ {: have:n :}
   have 0= if FIRST-MORE else have 2 * then {: want:n :}
   want cells MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: grown :}
   state MORE-SPAN {: old :}
   old SPAN:$ grown SPAN:COPY
   grown SPAN:$ STR:DATA state CURSOR-MORE !
   want state CURSOR-MORE-CELLS !
   old MEM:FREE-SPAN ;

\ The level's word exists: it is below the depth, or PUSH ensured it.
: KIND-WORD ( ^cursor n -- ^n ) {: state:^ level:n :}
   level WORD-BITS / {: w:n :}
   w 0= if state CURSOR-KINDS exit then
   state CURSOR-MORE @ w 1- cells + CELL-VIEW ;

\ <found>: the input's end, `byte $HH` for a byte no line shows, else up to
\ REASON:QUOTE-MOST token bytes, made visible, in backticks, `...` if cut.
$7F constant DEL

0 constant FOUND-END
1 constant FOUND-BYTE
2 constant FOUND-TOKEN
3 constant FOUND-CUT

: END$ ( -- str ) s" the end of the input" ;

: SHOWN? ( n -- bool )
   dup SP >= swap DEL < and ;

: WS? ( n -- bool )
   dup SP = over TAB = or over LF = or swap CH-CR = or ;

: STRUCTURAL? ( n -- bool )
   dup LBRACE = over RBRACE = or over LBRACK = or over RBRACK = or
   over COLON = or swap COMMA = or ;

\ An ASCII byte of a bare run.
: BARE? ( n -- bool )
   dup SHOWN? over SP <> and over STRUCTURAL? 0= and swap DQ <> and ;

\ Where a token's scan stops: one byte past REASON:QUOTE-MOST tells a cut.
: FOUND-LIM ( ^cursor n -- n ) {: state:^ off:n :}
   off REASON:QUOTE-MOST + 1+ state CURSOR-SRC-LEN @ min ;

\ A backslash before a quote or a backslash: the pair is shown together, so
\ an escaped quote never reads as the string's close.
: PAIRED? ( ^cursor n n -- bool ) {: state:^ lim:n i:n :}
   state i BYTE-AT BACKSLASH <> if false exit then
   i 1+ lim >= if false exit then
   state i 1+ BYTE-AT dup DQ = swap BACKSLASH = or ;

\ The index after the character at i, or i when its byte is a control byte or
\ starts no valid UTF-8 sequence.
: CHAR-END ( ^cursor n -- n ) {: state:^ i:n :}
   state i BYTE-AT {: b:n :}
   b UTF1-MAX < if b SHOWN? if i 1+ else i then exit then
   state CURSOR-SRC @ state CURSOR-SRC-LEN @ >str i UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF nip ENDOF
      raw-byte OF 2drop i ENDOF
   ;MATCH ;

\ The index after the bare run's character at i, or i when it ends the run.
: BARE-STEP ( ^cursor n -- n ) {: state:^ i:n :}
   state i BYTE-AT {: b:n :}
   b UTF1-MAX >= if state i CHAR-END exit then
   b BARE? if i 1+ else i then ;

\ A run of whole characters, so the last may end past lim.
: BARE-END ( ^cursor n n -- n ) {: state:^ lim:n off:n :}
   off begin
      dup lim < if state over BARE-STEP 2dup <> else dup false then
   while nip repeat
   drop ;

: FOUND-RUN ( ^cursor n -- n n ) {: state:^ off:n :}
   state state off FOUND-LIM off BARE-END off - {: len:n :}
   len REASON:QUOTE-MOST <= if len FOUND-TOKEN exit then
   state CURSOR-SRC @ off + len >str REASON:QUOTE-MOST UTF8:FIT FOUND-CUT ;

\ The next index after the shown character at i, and whether it closed the
\ string.
: STRING-STEP ( ^cursor n n -- n bool ) {: state:^ lim:n i:n :}
   state i BYTE-AT DQ = if i 1+ true exit then
   state lim i PAIRED? if i 2 + false exit then
   state i CHAR-END false ;

: STRING-MORE? ( ^cursor n n bool -- bool )
   {: state:^ lim:n i:n closed:bool :}
   closed if false exit then
   i lim >= if false exit then
   state i CHAR-END i > ;

\ The end of the string token opened at off, and whether it closed.
: STRING-END ( ^cursor n n -- n bool ) {: state:^ lim:n off:n :}
   off 1+ false
   begin 2dup state lim 2swap STRING-MORE? while
      drop state lim rot STRING-STEP
   repeat ;

\ Whole when it closes, or the input ends, within REASON:QUOTE-MOST bytes; else
\ cut at a byte CHAR-END does not step or at REASON:QUOTE-MOST, on a
\ character's first byte.
: FOUND-STRING ( ^cursor n -- n n ) {: state:^ off:n :}
   state state off FOUND-LIM off STRING-END {: end:n closed:bool :}
   end off - {: len:n :}
   closed end state CURSOR-SRC-LEN @ = or
   len REASON:QUOTE-MOST <= and if len FOUND-TOKEN exit then
   state CURSOR-SRC @ off + len >str REASON:QUOTE-MOST UTF8:FIT FOUND-CUT ;

\ The length and form of the token at off. Inside a string a quote closes it;
\ elsewhere it opens a string token.
: FOUND-FORM ( ^cursor n bool -- n n ) {: state:^ off:n inside:bool :}
   off state CURSOR-SRC-LEN @ >= if 0 FOUND-END exit then
   state off BYTE-AT {: b:n :}
   b UTF1-MAX >= if
      state off CHAR-END off > if state off FOUND-RUN else 1 FOUND-BYTE then exit
   then
   b SHOWN? 0= if 1 FOUND-BYTE exit then
   b BARE? if state off FOUND-RUN exit then
   b DQ = inside 0= and if state off FOUND-STRING exit then
   1 FOUND-TOKEN ;

: PUT-FOUND ( ^cursor n n n -- ) {: state:^ off:n len:n form:n :}
   form FOUND-END = if END$ REASON:APPEND exit then
   form FOUND-BYTE = if state off BYTE-AT REASON:BYTE exit then
   state CURSOR-SRC @ off + len >str  form FOUND-CUT =  REASON:QUOTE ;

\ The line and column of off.
: PLACE ( ^cursor n -- n n ) {: state:^ off:n :}
   state CURSOR-SRC @ {: src:^ :}
   src src off + 1 1 0 REJECTION:POSITION drop ;

\ The codes whose place is inside a string.
: IN-STRING? ( n -- bool )
   dup E-JSON-READ-STRING = over E-JSON-READ-ESCAPE = or
   swap E-JSON-READ-SURROGATE = or ;

\ Opens the text of the place at off, up to its expected part.
: REJECT-HEAD ( ^cursor n -- ) {: state:^ off:n :}
   REASON:OPEN
   state off PLACE REASON:AT REASON:EXPECTED ;

\ Writes the found part and the fix, and throws code with the text.
: REJECT-TAIL ( ^cursor n str n -- ) {: state:^ off:n fix:str code:n :}
   REASON:FOUND
   state off state off code IN-STRING? FOUND-FORM PUT-FOUND
   REASON:FIX fix REASON:APPEND
   code REASON:STOP ;

\ Throws code with the text of the place at off, SB untouched.
: REJECT-AT ( ^cursor n str str n -- )
   {: state:^ off:n expect:str fix:str code:n :}
   state off REJECT-HEAD
   expect REASON:APPEND
   state off fix code REJECT-TAIL ;

\ The current token's first byte: a string's or a key's opening quote.
: TOKEN-START ( ^cursor -- n ) {: state:^ :}
   state CURSOR-TOK-AT @
   state CURSOR-KIND @ dup T-STR = swap T-KEY = or if 1- then ;

: REJECT-TOKEN ( ^cursor str str n -- )
   {: state:^ expect:str fix:str code:n :}
   state state TOKEN-START expect fix code REJECT-AT ;

\ Rejects the current token for its kind, given the kind expected.
: KIND-REJECT ( ^cursor str -- ) {: state:^ expect:str :}
   state expect
   s" write that kind of value here, or read this token with the word for its kind"
   E-JSON-READ-STATE REJECT-TOKEN ;

: READER-CODE? ( n -- bool )
   dup E-JSON-READ-FIRST <= swap E-JSON-READ-LAST >= and ;

public

\ Whether a reader code is the input's fault: every one but E-READ-SOURCE,
\ a caller's wrong source.
: INPUT-FAULT? ( n -- bool )
   dup READER-CODE? swap E-READ-SOURCE <> and ;

private

\ The byte at the place, -1 at the end of the input.
: PLACE-BYTE ( ^cursor -- n ) {: state:^ :}
   state EOF? if -1 exit then
   state PEEK ;

: STATE-EXPECTED ( n -- str ) {: st:n :}
   st ST-ELEM = if s" a value or `]`" exit then
   st ST-KEY = if s" a member name or `}`" exit then
   st ST-MEMBER = if s" a member name" exit then
   st ST-SEP = if
      s" `,` or the close of the open array or object" exit
   then
   s" a value" ;

: LITERAL? ( n -- bool )
   dup CH-T = over CH-F = or swap CH-N = or ;

: LITERAL$ ( n -- str ) {: b:n :}
   b CH-T = if s" `true`" exit then
   b CH-F = if s" `false`" exit then
   s" `null`" ;

\ The last byte before the place that is not white space, -1 at the document's
\ start.
: PRIOR-BYTE ( ^cursor -- n ) {: state:^ :}
   state CURSOR-POS @ begin
      dup 0 > if state over 1- BYTE-AT WS? else false then
   while 1- repeat {: at:n :}
   at 0= if -1 exit then
   state at 1- BYTE-AT ;

: VALUE-FIX ( n -- str ) {: prior:n :}
   prior COMMA = if s" write a value, or remove a trailing comma" exit then
   prior COLON = if s" write the member's value" exit then
   s" write a JSON value" ;

: MALFORMED-FIX ( n n n -- str ) {: st:n b:n prior:n :}
   st ST-ELEM = if s" write a value, or close the array with `]`" exit then
   st ST-KEY = if
      s" write the member's name in double quotes, or close the object with `}`"
      exit
   then
   st ST-MEMBER = if
      s" write the next member's name in double quotes, or remove a trailing comma"
      exit
   then
   b RBRACK = b RBRACE = or if prior VALUE-FIX exit then
   s" write a value: an object, an array, a string in double quotes, a number, `true`, `false` or `null`" ;

\ READ-VALUE sends only t, f and n to READ-LIT, so a value phase rejected at
\ one of them rejected a misspelt literal.
: MALFORMED-ADVICE ( n n n -- str str ) {: st:n b:n prior:n :}
   REASON:HIDDEN? if
      st STATE-EXPECTED
      st -1 -1 MALFORMED-FIX exit
   then
   st ST-VALUE = st ST-ELEM = or b LITERAL? and if
      b LITERAL$
      s" write the word in full and in lower case, or quote the text as a string"
      exit
   then
   st STATE-EXPECTED st b prior MALFORMED-FIX ;

: STRING-ADVICE ( n -- str str ) {: b:n :}
   b 0 < if
      s\" the closing `\"` of the string" s\" close the string with `\"`" exit
   then
   REASON:HIDDEN? if
      s" a UTF-8 character or an escape"
      s" write the text in UTF-8, with control bytes as escapes" exit
   then
   b SP < if
      s" a character or an escape"
      s" write the control byte as an escape such as `\t`, `\n` or `\u001F`" exit
   then
   s" a UTF-8 character" s" write the text in UTF-8" ;

: ESCAPE-ADVICE ( n -- str str ) {: code:n :}
   code E-JSON-READ-ESCAPE = if
      s\" an escape letter from `\"\\/bfnrt`, or `u` and four hex digits"
      s\" write a valid escape, or `\\\\` for a backslash itself" exit
   then
   code E-JSON-READ-SURROGATE = if
      s" a `\uD800`-`\uDBFF` escape and then a `\uDC00`-`\uDFFF` escape"
      s" write both halves of the surrogate pair, or the character itself in UTF-8"
      exit
   then
   s" JSON text" s" correct the text here" ;

: SYNTAX-ADVICE ( n -- str str ) {: code:n :}
   code E-JSON-READ-COLON = if
      s" `:` after the member name" s" write `:` between the name and its value"
      exit
   then
   code E-JSON-READ-TRAILING = if
      s" the end of the input after the top-level value"
      s" remove the text after the value, or put all the values in one array"
      exit
   then
   code E-JSON-READ-NUMBER = if
      s" a number"
      s" write a JSON number such as `-12`, `0.5` or `2e-3`: digits after `-`, `.` and `e`, and no leading `0` before more digits"
      exit
   then
   code ESCAPE-ADVICE ;

\ The expected text and fix of a reader code, from its phase and the source at
\ the place; the container stack is released by then, so a close names either.
: ADVICE ( ^cursor n -- str str ) {: state:^ code:n :}
   state CURSOR-STATE @ {: st:n :}
   state PLACE-BYTE {: b:n :}
   code E-JSON-READ-MALFORMED = if
      st b state PRIOR-BYTE MALFORMED-ADVICE exit
   then
   code E-JSON-READ-STRING = if b STRING-ADVICE exit then
   code E-JSON-READ-EOF = if
      st STATE-EXPECTED
      s" write the rest of the document, closing each open array with `]` and object with `}`"
      exit
   then
   code E-JSON-READ-COMMA = if
      ST-SEP STATE-EXPECTED
      s" write `,` between the values, or close the open array with `]` or object with `}`"
      exit
   then
   code SYNTAX-ADVICE ;

\ Throws the reader's failure: a reader code, which STEP threw plain, with its
\ place's text; any other code plain.
: THROW-FAILURE ( ^cursor -- ) {: state:^ :}
   state CURSOR-FAIL @ {: code:n :}
   code READER-CODE? if
      state state CURSOR-POS @ state code ADVICE code REJECT-AT
   then
   code throw ;

\ After the first advance failure released the mapping, nothing may read the
\ stack: every path that would throws the failure again first, with the same
\ text, so a later advance never pairs the code with another reader's text.
: REQUIRE-SOUND ( ^cursor -- ) {: state:^ :}
   state CURSOR-FAIL @ 0<> if state THROW-FAILURE then ;

: REQUIRE-SOURCE ( str -- ) {: source:str :}
   source STR:LEN 0 < if E-READ-SOURCE throw then
   source STR:LEN 0 > source STR:DATA 0= and if E-READ-SOURCE throw then ;

: CLEAR-CURSOR ( ^cursor -- ) {: state:^ :}
   NULL-PTR 0 0 0 0 0 0 0  NULL-PTR 0 0 0 0 0 0 0  NULL-PTR 0  CURSOR-MAKE state ! ;

public

: READ-INIT ( ^cursor str -- JSON:reader )
   {: state:^ source:str :}
   source REQUIRE-SOURCE
   state source INIT-STATE
   state MINT-READER ;

: READ-CLOSE ( JSON:reader -- )
   ERASE-READER MORE-RELEASE ;

\ Release what a record owns after a caller's throw abandoned its live reader.
\ Idempotent. A static record that is only ever re-initialised needs no READ-DISPOSE: the
\ next reader on the record reuses the mapping and its READ-CLOSE releases it.
: READ-DISPOSE ( ^cursor -- ) {: state:^ :}
   state MORE-RELEASE
   state CLEAR-CURSOR ;

: READ-TOKEN ( JSON:reader -- JSON:reader n )
   READER>STATE CURSOR-KIND @ ;

\ The raw lexeme, borrowed from the source, escapes still encoded.
: READ-SPAN$ ( JSON:reader -- JSON:reader str )
   READER>STATE {: state:^ :}
   state CURSOR-SRC @ state CURSOR-TOK-AT @ +
   state CURSOR-TOK-LEN @ >str ;

private

: SKIP-WS ( ^cursor -- ) {: state:^ :}
   begin
      state CURSOR-POS @ state CURSOR-SRC-LEN @ < if
         state CURSOR-SRC @ state CURSOR-POS @ + c@ WS?
      else
         false
      then
   while
      state CURSOR-POS dup @ 1+ swap !
   repeat ;

: LEVEL-MASK ( n -- n )
   WORD-BITS mod 1 swap lshift ;

: PUSH ( ^cursor bool -- ) {: state:^ obj:bool :}
   state CURSOR-DEPTH @ {: level:n :}
   state level WORD-BITS / MORE-ENSURE
   state level KIND-WORD {: word:^ :}
   level LEVEL-MASK {: mask:n :}
   obj if word @ mask or else word @ mask invert and then word !
   level 1+ state CURSOR-DEPTH ! ;

: POP ( ^cursor -- ) {: state:^ :}
   state CURSOR-DEPTH dup @ 1- swap ! ;

: IN-OBJECT? ( ^cursor -- bool ) {: state:^ :}
   state CURSOR-DEPTH @ 1- {: level:n :}
   state level KIND-WORD @ level LEVEL-MASK and 0<> ;

: AFTER-VALUE ( ^cursor -- ) {: state:^ :}
   state CURSOR-DEPTH @ 0= if ST-DONE else ST-SEP then
   state CURSOR-STATE ! ;

: HEX-DIGIT ( n -- n )
   dup STR:HEX-DIGIT? 0= if E-JSON-READ-ESCAPE throw then
   STR:HEX-DIGIT-VALUE ;

: SCAN-HEX ( ^cursor n -- n ) {: state:^ value:n :}
   state EOF? if E-JSON-READ-ESCAPE throw then
   value HEX-BASE *
   state PEEK HEX-DIGIT
   +
   state ADVANCE ;

: SCAN-HEX4 ( ^cursor -- n ) {: state:^ :}
   state 0 SCAN-HEX
   state swap SCAN-HEX
   state swap SCAN-HEX
   state swap SCAN-HEX ;

\ The byte each one-letter escape stands for, indexed by its letter: \" \\ \/
\ \b \f \n \r \t. Zero marks every other byte; no escape stands for NUL.
create UNESC-MAP
   0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c,   \ $00
   0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c,   \ $10
   0 c, 0 c, 34 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 47 c,   \ $20
   0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c,   \ $30
   0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c,   \ $40
   0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 92 c, 0 c, 0 c, 0 c,   \ $50
   0 c, 0 c, 8 c, 0 c, 0 c, 0 c, 12 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 10 c, 0 c,   \ $60
   0 c, 0 c, 13 c, 0 c, 9 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c,   \ $70
   0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c,   \ $80
   0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c,   \ $90
   0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c,   \ $A0
   0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c,   \ $B0
   0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c,   \ $C0
   0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c,   \ $D0
   0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c,   \ $E0
   0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c,   \ $F0

: SIMPLE-ESC? ( n -- bool )
   UNESC-MAP BYTE-VIEW + c@ 0 <> ;

: SCAN-SURROGATE ( ^cursor -- ) {: state:^ :}
   state EOF? if E-JSON-READ-SURROGATE throw then
   state PEEK BACKSLASH <> if E-JSON-READ-SURROGATE throw then
   state CURSOR-POS @ {: at:n :}
   state ADVANCE
   state EOF? if E-JSON-READ-SURROGATE throw then
   state PEEK CH-U <> if at state CURSOR-POS ! E-JSON-READ-SURROGATE throw then
   state ADVANCE
   state SCAN-HEX4 {: lo:n :}
   lo SUR-LO < lo SUR-END >= or if at state CURSOR-POS ! E-JSON-READ-SURROGATE throw then ;

\ PLAIN-END admits every one-letter escape, so the backslash here starts a
\ \u escape or an invalid or unfinished one.
: SCAN-ESC ( ^cursor -- ) {: state:^ :}
   state CURSOR-POS @ {: at:n :}
   state ADVANCE
   state EOF? if E-JSON-READ-ESCAPE throw then
   state PEEK CH-U <> if E-JSON-READ-ESCAPE throw then
   state ADVANCE
   state SCAN-HEX4 {: cp:n :}
   cp SUR-HI < if exit then
   cp SUR-END >= if exit then
   cp SUR-LO >= if at state CURSOR-POS ! E-JSON-READ-SURROGATE throw then
   state SCAN-SURROGATE ;

: REQUIRE-UTF-BYTES ( ^cursor n -- ) {: state:^ count:n :}
   state CURSOR-POS @ count + state CURSOR-SRC-LEN @ > if E-JSON-READ-STRING throw then ;

: CONT? ( n -- bool )
   $C0 and UTF-CONT = ;

: UTF-BYTE ( ^cursor n -- n ) {: state:^ off:n :}
   state state CURSOR-POS @ off + BYTE-AT ;

: ADVANCE-N ( ^cursor n -- ) {: state:^ count:n :}
   state CURSOR-POS dup @ count + swap ! ;

: SCAN-UTF2 ( ^cursor -- ) {: state:^ :}
   state 2 REQUIRE-UTF-BYTES
   state 1 UTF-BYTE CONT? 0= if E-JSON-READ-STRING throw then
   state 2 ADVANCE-N ;

: SCAN-UTF3 ( ^cursor n -- ) {: state:^ lead:n :}
   state 3 REQUIRE-UTF-BYTES
   state 1 UTF-BYTE {: second:n :}
   second CONT? 0= state 2 UTF-BYTE CONT? 0= or if E-JSON-READ-STRING throw then
   lead UTF2-END = second UTF-E0-MIN < and if E-JSON-READ-STRING throw then
   lead UTF-SUR-LEAD = second UTF-SUR-MAX > and if E-JSON-READ-STRING throw then
   state 3 ADVANCE-N ;

: SCAN-UTF4 ( ^cursor n -- ) {: state:^ lead:n :}
   state 4 REQUIRE-UTF-BYTES
   state 1 UTF-BYTE {: second:n :}
   second CONT? 0= state 2 UTF-BYTE CONT? 0= or
   state 3 UTF-BYTE CONT? 0= or if E-JSON-READ-STRING throw then
   lead UTF3-END = second UTF-F0-MIN < and if E-JSON-READ-STRING throw then
   lead UTF-MAX-LEAD = second UTF-MAX-SECOND > and if E-JSON-READ-STRING throw then
   state 4 ADVANCE-N ;

: SCAN-UTF8 ( ^cursor n -- ) {: state:^ lead:n :}
   lead UTF2-MIN >= lead UTF2-END < and if state SCAN-UTF2 exit then
   lead UTF2-END >= lead UTF3-END < and if state lead SCAN-UTF3 exit then
   lead UTF3-END >= lead UTF4-END < and if state lead SCAN-UTF4 exit then
   E-JSON-READ-STRING throw ;

\ The offset of the first byte at or after pos that the cursor words must
\ check - a quote, a control byte, a UTF-8 lead, or the backslash of a \u,
\ invalid or unfinished escape - or the source length. Printable ASCII and
\ one-letter escapes need no other check.
: PLAIN-END ( str n -- n ) {: s:str pos:n :}
   s STR:LEN pos ?do
      s STR:DATA i + c@ {: c:n :}
      c BACKSLASH = if
         i 1+ s STR:LEN >= if i unloop exit then
         s STR:DATA i 1+ + c@ SIMPLE-ESC? 0= if i unloop exit then
         2
      else
         c SP < c UTF1-MAX >= or c DQ = or if i unloop exit then
         1
      then
   +loop
   s STR:LEN ;

: SKIP-PLAIN ( ^cursor -- ) {: state:^ :}
   state CURSOR-SRC @ state CURSOR-SRC-LEN @ >str state CURSOR-POS @ PLAIN-END
   state CURSOR-POS ! ;

: SCAN-STRING ( ^cursor -- n n ) {: state:^ :}
   state ADVANCE
   state CURSOR-POS @ {: at:n :}
   begin
      state SKIP-PLAIN
      state EOF? if E-JSON-READ-STRING throw then
      state PEEK
      dup DQ = if
         drop
         at state CURSOR-POS @ at -
         state ADVANCE exit
      then
      dup BACKSLASH = if
         drop state SCAN-ESC
      else
         \ A control byte is no UTF-8 lead, so SCAN-UTF8 rejects it.
         state swap SCAN-UTF8
      then
   again ;

: NUM-CHAR? ( n -- bool )
   dup STR:DIGIT? if drop true exit then
   dup MINUS = if drop true exit then
   dup PLUS = if drop true exit then
   dup DOT = if drop true exit then
   dup EXP-LOWER = if drop true exit then
   EXP-UPPER = ;


: DIGITS-END ( str n -- n )
   {: s:str i:n :}
   i begin
      dup s STR:LEN < if dup s STR:DATA + c@ STR:DIGIT? else false then
   while 1+ repeat ;


\ The number's span and kind, T-FLOAT once a fraction or exponent is read.
\ READ-VALUE starts a number only at a minus or a digit, so at is in the source.
: SCAN-NUMBER ( ^cursor -- n n n )
   {: state:^ :}
   state CURSOR-SRC @ state CURSOR-SRC-LEN @ state CURSOR-POS @
   {: src:^ len:n at:n :}
   src at + c@ MINUS = if at 1+ else at then {: int-at:n :}
   src len >str int-at DIGITS-END {: int-end:n :}
   int-end int-at = if E-JSON-READ-NUMBER throw then
   src int-at + c@ ZERO = int-end int-at 1+ > and if
      E-JSON-READ-NUMBER throw
   then
   int-end len < if src int-end + c@ DOT = else false then
   if
      src len >str int-end 1+ DIGITS-END
      dup int-end 1+ = if E-JSON-READ-NUMBER throw then
   else int-end then {: frac-end:n :}
   frac-end len < if
      src frac-end + c@ dup EXP-LOWER = swap EXP-UPPER = or
   else false then
   if
      frac-end 1+
      dup len < if dup src + c@ dup PLUS = swap MINUS = or if 1+ then then
      {: exp-at:n :}
      src len >str exp-at DIGITS-END
      dup exp-at = if E-JSON-READ-NUMBER throw then
   else frac-end then {: end:n :}
   end len < if src end + c@ NUM-CHAR? if E-JSON-READ-NUMBER throw then then
   end state CURSOR-POS !
   at end at - end int-end > if T-FLOAT else T-INT then ;


: RAW-SPAN$ ( ^cursor -- str ) {: state:^ :}
   state CURSOR-SRC @ state CURSOR-TOK-AT @ +
   state CURSOR-TOK-LEN @ >str ;

: MATCH-LIT? ( ^cursor str -- bool ) {: state:^ lit:str :}
   state CURSOR-POS @ lit STR:LEN + state CURSOR-SRC-LEN @ > if false exit then
   0 begin dup lit STR:LEN < while
      dup lit STR:DATA + c@
      over state CURSOR-SRC @ state CURSOR-POS @ + + c@ <> if
         drop false exit
      then
      1+
   repeat drop true ;

\ A string, key, number or opened container becomes the token only once its
\ whole advance has passed, so a failed advance leaves the previous token
\ readable.
: PUBLISH ( ^cursor n n n -- ) {: state:^ at:n len:n kind:n :}
   at state CURSOR-TOK-AT !
   len state CURSOR-TOK-LEN !
   kind state CURSOR-KIND ! ;

: READ-LIT ( ^cursor str n -- n ) {: state:^ lit:str kind:n :}
   state lit MATCH-LIT? 0= if E-JSON-READ-MALFORMED throw then
   state state CURSOR-POS @ lit STR:LEN kind PUBLISH
   state CURSOR-POS dup @ lit STR:LEN + swap !
   state AFTER-VALUE
   kind ;

: READ-TRUE ( ^cursor -- n )
   s" true" T-TRUE READ-LIT ;

: READ-FALSE ( ^cursor -- n )
   s" false" T-FALSE READ-LIT ;

: READ-NULL ( ^cursor -- n )
   s" null" T-NULL READ-LIT ;

: OPEN-OBJ ( ^cursor -- n ) {: state:^ :}
   state CURSOR-POS @ {: at:n :}
   state CURSOR-POS dup @ 1+ swap !
   state true PUSH
   state at 1 T-OBJ PUBLISH
   ST-KEY state CURSOR-STATE !
   T-OBJ ;

: OPEN-ARR ( ^cursor -- n ) {: state:^ :}
   state CURSOR-POS @ {: at:n :}
   state CURSOR-POS dup @ 1+ swap !
   state false PUSH
   state at 1 T-ARR PUBLISH
   ST-ELEM state CURSOR-STATE !
   T-ARR ;

: CLOSE-OBJ ( ^cursor -- n ) {: state:^ :}
   state state CURSOR-POS @ 1 T-OBJ-END PUBLISH
   state CURSOR-POS dup @ 1+ swap !
   state POP
   state AFTER-VALUE
   T-OBJ-END ;

: CLOSE-ARR ( ^cursor -- n ) {: state:^ :}
   state state CURSOR-POS @ 1 T-ARR-END PUBLISH
   state CURSOR-POS dup @ 1+ swap !
   state POP
   state AFTER-VALUE
   T-ARR-END ;

: READ-STRING ( ^cursor -- n ) {: state:^ :}
   state state SCAN-STRING T-STR PUBLISH
   state AFTER-VALUE
   T-STR ;

: READ-KEY ( ^cursor -- n ) {: state:^ :}
   state SCAN-STRING {: at:n len:n :}
   state SKIP-WS
   state CURSOR-POS @ state CURSOR-SRC-LEN @ >= if E-JSON-READ-COLON throw then
   state CURSOR-SRC @ state CURSOR-POS @ + c@ COLON <> if
      E-JSON-READ-COLON throw
   then
   state CURSOR-POS dup @ 1+ swap !
   state at len T-KEY PUBLISH
   ST-VALUE state CURSOR-STATE !
   T-KEY ;

: READ-NUMBER ( ^cursor -- n )
   {: state:^ :}
   state SCAN-NUMBER {: at:n len:n kind:n :}
   state at len kind PUBLISH
   state AFTER-VALUE
   kind ;

: READ-VALUE ( ^cursor n -- n ) {: state:^ c:n :}
   c LBRACE = if state OPEN-OBJ exit then
   c LBRACK = if state OPEN-ARR exit then
   c DQ = if state READ-STRING exit then
   c CH-T = if state READ-TRUE exit then
   c CH-F = if state READ-FALSE exit then
   c CH-N = if state READ-NULL exit then
   c MINUS = if state READ-NUMBER exit then
   c STR:DIGIT? if state READ-NUMBER exit then
   E-JSON-READ-MALFORMED throw ;

: DO-ELEM ( ^cursor n -- n ) {: state:^ c:n :}
   c RBRACK = if state CLOSE-ARR exit then
   state c READ-VALUE ;

: DO-KEY ( ^cursor n -- n ) {: state:^ c:n :}
   c RBRACE = if state CLOSE-OBJ exit then
   c DQ = if state READ-KEY exit then
   E-JSON-READ-MALFORMED throw ;

: DO-MEMBER ( ^cursor n -- n ) {: state:^ c:n :}
   c DQ = if state READ-KEY exit then
   E-JSON-READ-MALFORMED throw ;

: DO-SEP ( ^cursor n -- n ) {: state:^ c:n :}
   c COMMA = if
      state CURSOR-POS dup @ 1+ swap !
      state IN-OBJECT? if ST-MEMBER else ST-VALUE then
      state CURSOR-STATE !
      RETRY exit
   then
   c RBRACE = state IN-OBJECT? and if
      state CLOSE-OBJ exit
   then
   c RBRACK = state IN-OBJECT? 0= and if
      state CLOSE-ARR exit
   then
   E-JSON-READ-COMMA throw ;

: PUBLISH-END ( ^cursor -- n ) {: state:^ :}
   state state CURSOR-POS @ 0 T-END PUBLISH
   T-END ;

: STEP ( ^cursor -- n ) {: state:^ :}
   state SKIP-WS
   state CURSOR-POS @ state CURSOR-SRC-LEN @ >= if
      state CURSOR-STATE @ ST-DONE = if state PUBLISH-END exit then
     E-JSON-READ-EOF throw
   then
   state CURSOR-SRC @ state CURSOR-POS @ + c@ {: c:n :}
   state CURSOR-STATE @ ST-DONE = if E-JSON-READ-TRAILING throw then
   state CURSOR-STATE @ ST-VALUE = if state c READ-VALUE exit then
   state CURSOR-STATE @ ST-ELEM = if state c DO-ELEM exit then
   state CURSOR-STATE @ ST-KEY = if state c DO-KEY exit then
   state CURSOR-STATE @ ST-MEMBER = if state c DO-MEMBER exit then
   state c DO-SEP ;

\ The token lands in the slot under the state, so catch sees a preserving row.
: STEP-SLOT ( n ^cursor -- n ^cursor ) {: state:^ :}
   drop
   begin state STEP dup RETRY = while drop repeat
   state ;

\ A failed STEP's code is kept and every later advance throws it: the cursor
\ may lie past the rejected text. Accessor and phase rejections leave it usable.
: NEXT-INNER ( ^cursor -- n ) {: state:^ :}
   state REQUIRE-SOUND
   0 state [: STEP-SLOT ;] catch nip {: token code:n :}
   code 0= if token exit then
   state MORE-RELEASE
   code state CURSOR-FAIL !
   state THROW-FAILURE ;

public

: READ-NEXT ( JSON:reader -- JSON:reader n )
   READER>STATE NEXT-INNER ;

\ READ-FINISH reads to the document's end from any position (before the first READ-NEXT,
\ inside containers, after the last value, or after T-END, which a done reader
\ answers again) and consumes the reader. A malformed or truncated rest throws
\ the reader's own E-JSON-READ-* code. A caller that took only the members it needs
\ still learns the document is well formed.
: READ-FINISH ( JSON:reader -- )
   begin READ-NEXT T-END = until
   READ-CLOSE ;

private

: INT-INNER ( ^cursor -- n ) {: state:^ :}
   state CURSOR-KIND @ T-INT <> if state s" an integer" KIND-REJECT then
   state RAW-SPAN$ STR:>NUMBER? MATCH option
     none OF
        state s" an integer from -9223372036854775808 to 9223372036854775807"
        s" write an integer in that range, or read it with READ-U64 or READ-FLOAT"
        E-JSON-READ-NUMBER REJECT-TOKEN
     ENDOF
     some OF ENDOF
   ;MATCH ;

\ 2^64-1 is U64-TENTH * 10 + U64-LAST: a digit after a larger accumulator,
\ a negative one included, or after U64-TENTH and above U64-LAST overflows.
\ Halving -1 unsigned, then dividing by five, divides 2^64-1 by ten.
-1 1 rshift 5 / constant U64-TENTH
-1 U64-TENTH STR:BASE * - constant U64-LAST

: U64-REJECT ( ^cursor -- ) {: state:^ :}
   state s" an integer from 0 to 18446744073709551615"
   s" write digits alone, with no sign, fraction or exponent, or read the number with READ-FLOAT"
   E-JSON-READ-NUMBER REJECT-TOKEN ;

: U64-DIGIT ( ^cursor n n -- n ) {: state:^ acc:n d:n :}
   acc 0<  acc U64-TENTH > or if state U64-REJECT then
   acc U64-TENTH = d U64-LAST > and if state U64-REJECT then
   acc STR:BASE * d + ;

\ SCAN-NUMBER leaves a T-INT token an optional minus and digits.
: U64-INNER ( ^cursor -- n ) {: state:^ :}
   state CURSOR-KIND @ {: kind:n :}
   kind T-FLOAT = if state U64-REJECT then
   kind T-INT <> if state s" an integer" KIND-REJECT then
   state RAW-SPAN$ {: digits:str :}
   digits STR:DATA c@ MINUS = if state U64-REJECT then
   0 digits STR:LEN 0 ?do state swap digits STR:DATA i + c@ STR:DIGIT-VALUE U64-DIGIT loop ;

: FLOAT-INNER ( ^cursor -- r ) {: state:^ :}
   state CURSOR-KIND @ dup T-INT = swap T-FLOAT = or 0= if
      state s" a number" KIND-REJECT
   then
   state RAW-SPAN$ STR>FLOAT MATCH option
     none OF
        state s" a number a binary64 float can be read from"
        s" write the number with fewer digits or an exponent nearer zero"
        E-JSON-READ-NUMBER REJECT-TOKEN
     ENDOF
     some OF ENDOF
   ;MATCH ;

public

: READ-INT ( JSON:reader -- JSON:reader n )
   READER>STATE INT-INNER ;

\ A value at or above 2^63 is answered as a negative cell, its 64 bits.
: READ-U64 ( JSON:reader -- JSON:reader n )
   READER>STATE U64-INNER ;

: READ-FLOAT ( JSON:reader -- JSON:reader r )
   READER>STATE FLOAT-INNER ;

private

: WRITE-BYTE ( ^cursor n -- ) {: state:^ byte:n :}
   state CURSOR-OUT @ state CURSOR-DST-CAP @ >= if E-SPAN-CAPACITY throw then
   byte state CURSOR-DST @ state CURSOR-OUT @ + c! ;

: MATCH-BYTE ( ^cursor n -- ) {: state:^ byte:n :}
   state CURSOR-MATCHED @ 0= if exit then
   state CURSOR-OUT @ state CURSOR-DST-CAP @ >= if
      0 state CURSOR-MATCHED ! exit
   then
   state CURSOR-DST @ state CURSOR-OUT @ + c@ byte <> if
      0 state CURSOR-MATCHED !
   then ;

: EMIT-BYTE ( ^cursor n -- ) {: state:^ byte:n :}
   state CURSOR-MODE @ UMODE-MATCH = if
      state byte MATCH-BYTE
   else
      state byte WRITE-BYTE
   then
   state CURSOR-OUT dup @ 1+ swap ! ;

: HEX-NEXT ( ^cursor ^u8 n -- n ) {: state:^ a:^ value:n :}
   value HEX-BASE *
   a state CURSOR-IN @ + c@ STR:HEX-DIGIT-VALUE
   +
   state CURSOR-IN dup @ 1+ swap ! ;

: READ-HEX4 ( ^cursor ^u8 -- n ) {: state:^ a:^ :}
   state a 0 HEX-NEXT
   state a rot HEX-NEXT
   state a rot HEX-NEXT
   state a rot HEX-NEXT ;

: EMIT-UTF8 ( ^cursor n -- ) {: state:^ cp:n :}
   cp UTF1-MAX < if state cp EMIT-BYTE exit then
   cp UTF2-MAX < if
      state cp 6 rshift UTF2-LEAD or EMIT-BYTE
      state cp UTF-MASK and UTF-CONT or EMIT-BYTE exit
   then
   cp UTF3-MAX < if
      state cp 12 rshift UTF3-LEAD or EMIT-BYTE
      state cp 6 rshift UTF-MASK and UTF-CONT or EMIT-BYTE
      state cp UTF-MASK and UTF-CONT or EMIT-BYTE exit
   then
   state cp 18 rshift UTF4-LEAD or EMIT-BYTE
   state cp 12 rshift UTF-MASK and UTF-CONT or EMIT-BYTE
   state cp 6 rshift UTF-MASK and UTF-CONT or EMIT-BYTE
   state cp UTF-MASK and UTF-CONT or EMIT-BYTE ;

\ SCAN-STRING passed this \u escape and any surrogate pair, so UNI only decodes.
: UNI ( ^cursor ^u8 -- ) {: state:^ a:^ :}
   state a READ-HEX4 {: hi:n :}
   hi SUR-HI < hi SUR-END >= or if state hi EMIT-UTF8 exit then
   state CURSOR-IN dup @ 2 + swap !
   state a READ-HEX4 {: lo:n :}
   state hi lo UTF16:PAIR>SCALAR EMIT-UTF8 ;

\ Decode from in until the token ends or a \u escape starts. Each byte and
\ one-letter escape writes or matches one byte as EMIT-BYTE would, with out on
\ the stack. A \u escape returns with in at its backslash; the registers hold
\ in and out on return.
: UNESC-RUN ( ^cursor str -- ) {: state:^ s:str :}
   state CURSOR-DST @ {: d:^ :}
   state CURSOR-DST-CAP @ {: cap:n :}
   state CURSOR-MODE @ UMODE-MATCH = {: match:bool :}
   UNESC-MAP BYTE-VIEW {: m:^ :}
   state CURSOR-OUT @ s STR:LEN state CURSOR-IN @ ?do
      s STR:DATA i + c@ {: c:n :}
      c BACKSLASH = if
         s STR:DATA i 1+ + c@ {: e:n :}
         e CH-U = if i state CURSOR-IN ! state CURSOR-OUT ! unloop exit then
         m e + c@
         2
      else
         c 1
      then
      {: b:n step:n :}
      match if
         dup cap >= if
            0 state CURSOR-MATCHED !
         else
            d over + c@ b <> if 0 state CURSOR-MATCHED ! then
         then
      else
         dup cap >= if E-SPAN-CAPACITY throw then
         d over + b swap c!
      then
      1+ step
   +loop
   state CURSOR-OUT ! s STR:LEN state CURSOR-IN ! ;

: UNESC-RESET ( ^cursor ^u8 n n -- ) {: state:^ dst:^ cap:n mode:n :}
   dst state CURSOR-DST !
   cap state CURSOR-DST-CAP !
   0 state CURSOR-IN !
   0 state CURSOR-OUT !
   mode state CURSOR-MODE !
   1 state CURSOR-MATCHED ! ;

\ Only a token SCAN-STRING passed is T-STR or T-KEY, so it lies inside the
\ source and every backslash in it starts a valid escape. UNI reads a \u
\ escape's digits from a unchecked.
: UNESC-ALL ( ^cursor -- ) {: state:^ :}
   state CURSOR-TOK-LEN @ {: len:n :}
   state CURSOR-SRC @ state CURSOR-TOK-AT @ + {: a:^ :}
   begin state CURSOR-IN @ len < while
      state a len >str UNESC-RUN
      state CURSOR-IN @ len < if
         state CURSOR-IN dup @ 2 + swap !
         state a UNI
      then
   repeat ;

: REQUIRE-STRING ( ^cursor -- ) {: state:^ :}
   state CURSOR-KIND @ dup T-STR = swap T-KEY = or if exit then
   state s" a string or a member name" KIND-REJECT ;

: MATCH-INNER ( ^cursor str -- bool )
   {: state:^ key:str :}
   state REQUIRE-STRING
   state key str> UMODE-MATCH UNESC-RESET
   state UNESC-ALL
   state CURSOR-MATCHED @ 0= 0=
   state CURSOR-OUT @ key STR:LEN = and ;

\ Decodes the current string or key into the vector from byte at on, answering
\ its length, which its raw length bounds; the vector may hold the text itself.
\ Another token is E-JSON-READ-STATE before the vector changes.
: DECODE-INTO ( ^cursor ^VEC:vector<u8> n -- n )
   {: state:^ v:^ at:n :}
   state REQUIRE-STRING
   state CURSOR-TOK-LEN @ {: raw:n :}
   raw 0= if 0 exit then
   v at raw +  state CURSOR-SRC @  VEC:MAKE-ROOM  state CURSOR-SRC !
   v at raw + VEC:ENSURE
   state  v at VEC:AT  raw  UMODE-WRITE UNESC-RESET
   state UNESC-ALL
   state CURSOR-OUT @ ;

: DECODE-APPEND ( ^cursor ^VEC:vector<u8> -- )
   {: state:^ v:^ :}
   v VEC:COUNT {: at:n :}
   state v at DECODE-INTO {: u:n :}
   v at u + VEC:RESIZE ;

\ Replaces the vector's bytes with the current string or key, decoded, and
\ answers them until the vector next changes.
: DECODED$ ( ^cursor ^VEC:vector<u8> -- str )
   {: state:^ v:^ :}
   v 0 VEC:RESIZE  state v DECODE-APPEND  v VEC:BYTES ;

public

\ Appends the current string or key, decoded, to a vector, whose earlier
\ bytes stay. Another token is E-JSON-READ-STATE before the vector changes.
: READ-STR-APPEND ( JSON:reader ^VEC:vector<u8> -- JSON:reader ) {: v:^ :}
   READER>STATE v DECODE-APPEND ;

\ Replaces V's bytes with the current string or key, decoded, and answers them
\ until V next changes; V must not hold the text read; another token keeps V.
: READ-STR ( JSON:reader ^VEC:vector<u8> -- JSON:reader str ) {: v:^ :}
   READER>STATE {: state:^ :}
   state REQUIRE-STRING
   state v DECODED$ ;

private

: SCALAR? ( n -- bool )
   dup T-STR = over T-INT = or over T-FLOAT = or
   over T-TRUE = or over T-FALSE = or swap T-NULL = or ;

: OPENER? ( n -- bool )
   dup T-OBJ = swap T-ARR = or ;

: CLOSER? ( n -- bool )
   dup T-OBJ-END = swap T-ARR-END = or ;

: VALUE-TOKEN? ( n -- bool )
   dup SCALAR? swap OPENER? or ;

\ NEXT-INNER answers T-END only once every container is closed, so not while
\ the count is positive.
: SKIP-INNER ( ^cursor -- ) {: state:^ :}
   state CURSOR-KIND @ SCALAR? if exit then
   state CURSOR-KIND @ OPENER? 0= if state s" a value" KIND-REJECT then
   1 begin dup 0 > while
      state NEXT-INNER
      dup OPENER? if drop 1+ else
      dup CLOSER? if drop 1- else drop then then
   repeat drop ;

: FIND-PHASE? ( n -- bool )
   dup ST-KEY = swap ST-SEP = or ;

: REQUIRE-KEY-SPAN ( ^cursor str -- ) {: state:^ key:str :}
   key STR:LEN 0 <  key STR:LEN 0 > key STR:DATA 0= and  or if
      state s" a key of 0 or more bytes at a non-null address"
      s" pass the key's address and its length"
      E-JSON-READ-STATE REJECT-TOKEN
   then ;

: ANCHOR-REJECT ( ^cursor -- ) {: state:^ :}
   state s" an object's `{` or a member's value"
   s" write an object here, or read members only at an object's `{` or after a member's value"
   E-JSON-READ-STATE REJECT-TOKEN ;

: FIND-ANCHOR ( ^cursor -- n ) {: state:^ :}
   state CURSOR-DEPTH @ dup 0 <= if state ANCHOR-REJECT then
   state IN-OBJECT? 0= if state ANCHOR-REJECT then
   state CURSOR-STATE @ FIND-PHASE? 0= if state ANCHOR-REJECT then ;

: REQUIRE-KEY-DEPTH ( ^cursor n -- ) {: state:^ depth:n :}
   state CURSOR-DEPTH @ depth = if exit then
   state s" a member of the object READ-FIND-KEY searches"
   s" keep the reader in that object while READ-FIND-KEY reads it"
   E-JSON-READ-STATE REJECT-TOKEN ;

: MEMBER-NAME-REJECT ( ^cursor -- ) {: state:^ :}
   state s" a member name or the object's `}`"
   s" write the next member's name in double quotes, or close the object with `}`"
   E-JSON-READ-STATE REJECT-TOKEN ;

: FIND-INNER ( ^cursor str -- bool ) {: state:^ key:str :}
   state key REQUIRE-KEY-SPAN
   state REQUIRE-SOUND
   state FIND-ANCHOR {: depth:n :}
   begin
      state NEXT-INNER {: token:n :}
      token T-OBJ-END = if
         state depth 1- REQUIRE-KEY-DEPTH
         false exit
      then
      token T-KEY <> if state MEMBER-NAME-REJECT then
      state depth REQUIRE-KEY-DEPTH
      state key MATCH-INNER {: found:bool :}
      state NEXT-INNER {: value:n :}
      value VALUE-TOKEN? 0= if
         state s" the member's value" s" write a value after the member's `:`"
         E-JSON-READ-STATE REJECT-TOKEN
      then
      found if true exit then
      state SKIP-INNER
      state depth REQUIRE-KEY-DEPTH
      state CURSOR-STATE @ ST-SEP <> if
         state s" `,` or the object's `}`"
         s" write `,` between the members, or close the object with `}`"
         E-JSON-READ-STATE REJECT-TOKEN
      then
   again ;

public

: READ-SKIP-VALUE ( JSON:reader -- JSON:reader )
   READER>STATE SKIP-INNER ;

private

: VALUE-SPAN-INNER ( ^cursor -- str ) {: state:^ :}
   state CURSOR-KIND @ OPENER? 0= if state s" an object or an array" KIND-REJECT then
   state CURSOR-TOK-AT @ {: start:n :}
   state SKIP-INNER
   state CURSOR-SRC @ start +
   state CURSOR-POS @ start - >str ;

: ANY-SPAN-INNER ( ^cursor -- str )
   {: state:^ :}
   state CURSOR-KIND @ OPENER? if state VALUE-SPAN-INNER exit then
   state CURSOR-KIND @ SCALAR? 0= if state s" a value" KIND-REJECT then
   state CURSOR-KIND @ T-STR <> if state RAW-SPAN$ exit then
   state RAW-SPAN$ {: raw:str :}
   raw STR:DATA 1- raw STR:LEN 2 + >str ;

public

\ On an opener, the source bytes through the matching close; the cursor moves
\ past the value.
: READ-VALUE-SPAN$ ( JSON:reader -- JSON:reader str )
   READER>STATE VALUE-SPAN-INNER ;

\ On an opener, as READ-VALUE-SPAN$; on a scalar, its bytes as written, a string's
\ quotes and escapes included, the cursor staying; else E-JSON-READ-STATE.
: READ-ANY-SPAN$ ( JSON:reader -- JSON:reader str )
   READER>STATE ANY-SPAN-INNER ;

\ Searches only the current object, skipping each unmatched value, and stops at
\ its close. It and READ-STR-EQ? compare decoded bytes without a buffer, so key
\ length is not bounded by reader storage.
: READ-FIND-KEY ( JSON:reader str -- JSON:reader bool )
   {: key:str :}
   READER>STATE key FIND-INNER ;

\ The value text of the one top-level member the key names, as
\ serde reads a struct's field: other keys, repeated or not, are
\ skipped. E-MEMBER stands only once the whole text reads well
\ formed, placed at the first token of a text that is no object, at the
\ object's `}` for a missing member, or at a repeated member's second value.
\ A throw leaves the cursor for the next READ-INIT.
: READ-MEMBER$ ( str str ^cursor -- str )
   {: text:str key:str c:^ :}
   c text READ-INIT READ-NEXT
   T-OBJ <> if
      c TOKEN-START swap READ-FINISH
      c swap s" an object"
      s" write the text as one JSON object holding the member"
      E-MEMBER REJECT-AT
   then
   key READ-FIND-KEY 0= if
      c TOKEN-START swap READ-FINISH
      c swap s" the member the key names" s" add the member to the object"
      E-MEMBER REJECT-AT
   then
   READ-ANY-SPAN$ {: v:str :}
   key READ-FIND-KEY {: again:bool :}
   c TOKEN-START {: at:n :}
   READ-FINISH again if
      c at s" the member once" s" remove the repeated member, keeping one"
      E-MEMBER REJECT-AT
   then
   v ;

private

\ The names READ-LISTED takes are separated by single blanks; an empty
\ list holds none.
: READ-NAME-COUNT ( str -- n ) {: names:str :}
   names STR:LEN 0= if 0 exit then
   1 names STR:LEN 0 ?do names STR:DATA i + c@ SP = if 1+ then loop ;

\ The names a list holds before a last name `*`, and whether it has one.
: LISTED ( str -- str bool ) {: list:str :}
   list STR:LEN 0= if list false exit then
   list STR:DATA list STR:LEN + 1- c@ [char] * <> if list false exit then
   list STR:LEN 1 = if list STR:DATA 0 >str true exit then
   list STR:DATA list STR:LEN + 2 - c@ SP <> if list false exit then
   list STR:DATA list STR:LEN 2 - >str true ;

\ The name that starts at start, as listed, and the next name's start.
: NAME-AT ( str n -- str n ) {: list:str start:n :}
   list SP start STR:SPLIT-NEXT drop ;

\ A listed name that ends in `?` names an optional member.
: OPTIONAL? ( str -- bool ) {: name:str :}
   name STR:LEN 0= if false exit then
   name STR:DATA name STR:LEN + 1- c@ [char] ? = ;

\ The member's name a listed name gives.
: BARE ( str -- str ) {: name:str :}
   name OPTIONAL? if name STR:DATA name STR:LEN 1- >str exit then
   name ;

\ Whether the current key matches the name at start, and the next start.
: NAME-MATCH? ( n ^cursor str -- n bool )
   {: start:n state:^ list:str :}
   list start NAME-AT {: name:str next:n :}
   next state name BARE MATCH-INNER ;

\ The index of the name the current key decodes to, or -1.
: KEY-INDEX ( ^cursor str n -- n ) {: state:^ list:str count:n :}
   0 0 begin dup count < while
      swap state list NAME-MATCH? if drop exit then
      swap 1+
   repeat 2drop -1 ;

: ONE-OF$ ( -- str ) s" one of " ;
: LIST-SEP$ ( -- str ) s" , " ;
: MEMBER-OF$ ( -- str ) s" the member `" ;

\ Writes the name at start in backticks, after LIST-SEP$ unless it is the
\ first; answers the next name's start.
: PUT-NAME ( str n n -- n ) {: list:str start:n idx:n :}
   list start NAME-AT {: name:str next:n :}
   idx 0<> if LIST-SEP$ REASON:APPEND then
   s" `" REASON:APPEND  name BARE REASON:APPEND  s" `" REASON:APPEND
   next ;

: UNKNOWN-FIX$ ( -- str )
   s" rename the member to one of those, or remove it" ;

\ Rejects the current key, which no listed name matches.
: UNKNOWN-REJECT ( ^cursor str n -- ) {: state:^ list:str count:n :}
   count 0= if
      state s" the object's `}`" s" remove the member"
      E-MEMBER REJECT-TOKEN
   then
   state state TOKEN-START REJECT-HEAD
   ONE-OF$ REASON:APPEND
   0 count 0 ?do list swap i PUT-NAME loop drop
   state state TOKEN-START UNKNOWN-FIX$ E-MEMBER REJECT-TAIL ;

\ Rejects the name at start unless its member was seen or the name is
\ optional, and answers the next name's start.
: REQUIRE-NAME ( n ^cursor str bool -- n )
   {: start:n state:^ list:str seen:bool :}
   list start NAME-AT {: name:str next:n :}
   seen  name OPTIONAL?  or if next exit then
   state state TOKEN-START REJECT-HEAD
   MEMBER-OF$ REASON:APPEND  name REASON:APPEND  s" `" REASON:APPEND
   state state TOKEN-START s" add the member to the object"
   E-MEMBER REJECT-TAIL ;

\ At the object's `}`: rejects the first required name not seen.
: REQUIRE-ALL ( ^cursor str n ^u8 -- )
   {: state:^ list:str count:n seen:^ :}
   0 count 0 ?do
      state list  seen i + c@ 0<>  REQUIRE-NAME
   loop drop ;

\ Reads from the current key to its member's value's last token.
: SKIP-MEMBER ( ^cursor -- ) {: state:^ :}
   state NEXT-INNER drop state SKIP-INNER ;

\ The current key's name's index, marked seen, the reader on its value; -1
\ for a key skipped under `*`. After a key the reader yields a value or throws.
: MEMBER-INDEX ( ^cursor str n bool ^u8 -- n )
   {: state:^ list:str count:n wild:bool seen:^ :}
   state list count KEY-INDEX {: idx:n :}
   idx 0< wild and if state SKIP-MEMBER -1 exit then
   idx 0< if state list count UNKNOWN-REJECT then
   seen idx + c@ 0<> if
      state s" each member once" s" remove the repeated member, keeping one"
      E-MEMBER REJECT-TOKEN
   then
   1 seen idx + c!
   state NEXT-INNER drop idx ;

: REQUIRE-OBJECT ( ^cursor -- ) {: state:^ :}
   state CURSOR-KIND @ T-OBJ = if exit then
   state s" an object" s" write an object here" E-MEMBER REJECT-TOKEN ;

\ A value's span holds at least one byte and ends at the reader's position, and
\ the reader is on its last token: at depth 0 that is ST-DONE before T-END.
: REQUIRE-VALUE-SPAN ( ^cursor str -- ) {: state:^ span:str :}
   state CURSOR-POS @ {: pos:n :}
   span STR:DATA state CURSOR-SRC @ - {: off:n :}
   state CURSOR-STATE @ {: st:n :}
   st ST-SEP =  st ST-DONE =  state CURSOR-KIND @ T-END <> and  or 0=
   off 0 < or  span STR:LEN 1 < or  pos off - span STR:LEN <> or if
      state s" the span of the value the reader just read"
      s" pass READ-ANY-SPAN$'s answer for that value before the reader moves past it"
      E-JSON-READ-STATE REJECT-TOKEN
   then ;

\ A seen vector holds the listed object's depth, then a byte per listed name.
: SEEN-BYTES ( str -- n ) LISTED drop READ-NAME-COUNT 1 cells + ;

public

\ Rejects the current token with E-MEMBER at its place, given the expected
\ text and the fix. The reader stays as it is; its owner closes or disposes it.
: READ-REJECT ( JSON:reader str str -- )
   {: expect:str fix:str :}
   READER>STATE expect fix E-MEMBER REJECT-TOKEN ;

\ The caller empties seen before the object's first call; a call that finds
\ it empty sizes it to SEEN-BYTES for the names, all zero.
: READ-LISTED ( JSON:reader str ^VEC:vector<u8> -- JSON:reader n )
   {: list:str seen:^ :}
   READER>STATE {: state:^ :}
   state REQUIRE-SOUND
   list LISTED {: names:str wild:bool :}
   names READ-NAME-COUNT {: count:n :}
   seen VEC:COUNT 0= if seen list SEEN-BYTES VEC:RESIZE then
   seen 0 VEC:AT CELL-VIEW {: held:^ :}
   seen 0 VEC:AT 1 cells + {: marks:^ :}
   held @ 0= if
      state REQUIRE-OBJECT
      state CURSOR-DEPTH @ held !
   else
      state CURSOR-STATE @ ST-SEP <>  state CURSOR-DEPTH @ held @ <>  or if
         state s" the last token of the member's value"
         s" leave the reader on the value's last token, with READ-SKIP-VALUE for a value not read"
         E-JSON-READ-STATE REJECT-TOKEN
      then
   then
   begin
      state NEXT-INNER dup T-OBJ-END = if
         drop state names count marks REQUIRE-ALL -1 exit
      then
      T-KEY <> if state MEMBER-NAME-REJECT then
      state names count wild marks MEMBER-INDEX
   dup 0< while drop repeat ;

\ A container's opener is read again, so the depth counts it as before.
: READ-REWIND ( JSON:reader str -- JSON:reader ) {: span:str :}
   READER>STATE {: state:^ :}
   state REQUIRE-SOUND
   state span REQUIRE-VALUE-SPAN
   span STR:DATA state CURSOR-SRC @ - state CURSOR-POS !
   ST-VALUE state CURSOR-STATE !
   state NEXT-INNER drop ;

\ Whether the current string or key decodes to exactly these bytes, matched as
\ they decode, so no buffer bounds the string; the cursor stays where it was.
: READ-STR-EQ? ( JSON:reader str -- JSON:reader bool )
   {: want:str :}
   READER>STATE {: state:^ :}
   state want REQUIRE-KEY-SPAN
   state want MATCH-INNER ;

private

\ The classes a key has at a byte: its end and the 256 byte values.
257 constant CLASS-COUNT

\ The vectors keep the most keys held at once, the room at most their
\ raw bytes; classes is a fixed CLASS-COUNT 2 * cells.
STRUCTURE keys DERIVE addr
   FIELD table ^VEC:vector<n>
   FIELD indices ^VEC:vector<n>
   FIELD spare ^VEC:vector<n>
   FIELD classes ^VEC:vector<n>
   FIELD places ^VEC:vector<n>
   FIELD room ^VEC:vector<u8>
;STRUCTURE

\ A vector not yet made is NULL-PTR, which DELETE ignores.
: KEYS-CLOSE ( ^keys -- )
   {: keys:^ :}
   keys KEYS-TABLE @ VEC:DELETE
   keys KEYS-INDICES @ VEC:DELETE
   keys KEYS-SPARE @ VEC:DELETE
   keys KEYS-CLASSES @ VEC:DELETE
   keys KEYS-PLACES @ VEC:DELETE
   keys KEYS-ROOM @ VEC:DELETE ;

\ Each vector starts with element 0, so AT 0 is its base; the table's
\ is its count, 0 keys held.
: KEYS-OPEN ( ^keys -- )
   {: keys:^ :}
   HEAP:ALLOCATOR {: al:^ :}
   al 1 [type] n VEC:NEW keys KEYS-TABLE !
   al 1 [type] n VEC:NEW keys KEYS-INDICES !
   al 1 [type] n VEC:NEW keys KEYS-SPARE !
   al CLASS-COUNT 2 * [type] n VEC:NEW keys KEYS-CLASSES !
   al 1 [type] n VEC:NEW keys KEYS-PLACES !
   al 1 [type] u8 VEC:NEW keys KEYS-ROOM ! ;

: RUN-KEYS ( R ^keys [ R ^keys -- S ] -- S )
   over KEYS-OPEN execute ;

: KEYS-SCOPE ( R ^keys [ R ^keys -- S ] -- S )
   {: keys:^ body :}
   keys body
   [: RUN-KEYS ;] [: keys KEYS-CLOSE ;] finally ;

\ Runs body on empty key storage, released on return or throw.
: WITH-KEYS ( R [ R ^keys -- S ] -- S )
   {: body :}
   [type] keys [: body KEYS-SCOPE ;] MEM:WITH-RECORD ;

\ Cell 0 counts the keys held, k; cells 1..k+1 hold room offsets,
\ the first 0, and key i decodes to room bytes o(i)..o(i+1).
: TABLE ( ^keys -- ^n )
   KEYS-TABLE @ 0 VEC:AT ;

\ Key indices, a range grouped by decoded bytes once all are read.
: ORDER ( ^keys -- ^n )
   KEYS-INDICES @ 0 VEC:AT ;

\ The cells a group's keys move through, cell i beside order's.
: SPARE ( ^keys -- ^n )
   KEYS-SPARE @ 0 VEC:AT ;

\ The first CLASS-COUNT cells count, then place, each class of a pass,
\ zero between passes; the next CLASS-COUNT list the classes the pass saw.
: CLASSES ( ^keys -- ^n )
   KEYS-CLASSES @ 0 VEC:AT ;

\ Cell i holds key i's offset in the text, at its opening quote.
: PLACES ( ^keys -- ^n )
   KEYS-PLACES @ 0 VEC:AT ;

: ROOM ( ^keys -- ^u8 )
   KEYS-ROOM @ 0 VEC:AT ;

: OFFSET-AT ( ^n n -- ^n )
   1+ cells + ;

: OFFSET ( ^n n -- n )
   OFFSET-AT @ ;


: KEY$ ( ^n ^u8 n -- str )
   {: table:^ room:^ idx:n :}
   table idx OFFSET {: from:n :}
   room from +  table idx 1+ OFFSET from - >str ;


\ Grows the table to its count and offsets 0..k+1 and the places to hold
\ key k; the blocks may move.
: GROW-KEYS ( ^keys n -- )
   {: keys:^ k:n :}
   keys KEYS-TABLE @ k 3 + VEC:ENSURE
   keys KEYS-PLACES @ k 1+ VEC:ENSURE ;

\ On a key: records it as key k, after the k before it.
: RECORD-KEY ( ^cursor ^keys -- )
   {: state:^ keys:^ :}
   keys TABLE @ {: k:n :}
   keys k GROW-KEYS
   state TOKEN-START  keys PLACES k cells + !
   keys TABLE k OFFSET {: from:n :}
   state keys KEYS-ROOM @ from DECODE-INTO {: ku:n :}
   from ku + keys TABLE k 1+ OFFSET-AT !
   k 1+ keys TABLE ! ;


: RECORD-KEYS ( ^cursor ^keys -- )
   {: a:^ keys:^ :}
   begin a NEXT-INNER T-KEY = while
      a keys RECORD-KEY
      a NEXT-INNER drop
      a SKIP-INNER
   repeat ;

: FILL-ORDER ( ^n n n -- )
   {: order:^ lo:n k:n :}
   k lo ?do i order i cells + ! loop ;

: NTH ( ^n n -- n )
   cells + @ ;

\ Key idx's class at byte d: 0 where it ends, else 1 + its byte.
: CLASS ( ^n ^u8 n n -- n )
   {: table:^ room:^ idx:n d:n :}
   table room idx KEY$ {: key:str :}
   d key STR:LEN = if 0 exit then
   key STR:DATA d + c@ 1+ ;

: KEY-FROM$ ( ^n ^u8 n n -- str )
   {: table:^ room:^ idx:n d:n :}
   table room idx KEY$ {: key:str :}
   key STR:DATA d +  key STR:LEN d - >str ;

\ Whether every key of order[a..b) has the first's byte at p.
: AGREE? ( n ^keys n n -- bool )
   {: p:n keys:^ a:n b:n :}
   keys TABLE keys ROOM keys ORDER {: table:^ room:^ order:^ :}
   table room order a NTH p CLASS {: c:n :}
   c 0= if false exit then
   b a 1+ ?do
      table room order i NTH p CLASS
      c <> if false unloop exit then
   loop true ;

\ The first byte from d on where a key of order[a..b) ends or differs
\ from the first, so each byte before it is read once.
: PREFIX ( ^keys n n n -- n )
   {: keys:^ a:n b:n d:n :}
   d begin dup keys a b AGREE? while 1+ repeat ;

: SEEN-AT ( ^n n -- ^n )
   CLASS-COUNT + cells + ;

\ Counts a key of class c, listing c as seen class m when it is new;
\ answers how many classes are seen.
: COUNT-KEY ( n ^n n -- n )
   {: m:n cl:^ c:n :}
   cl c cells + {: at:^ :}
   at @ 1+ at !
   at @ 1 > if m exit then
   c cl m SEEN-AT !  m 1+ ;

\ Counts the keys of order[a..b) by their class at byte d; answers how
\ many classes it saw.
: COUNT-CLASSES ( ^keys n n n -- n )
   {: keys:^ a:n b:n d:n :}
   keys TABLE keys ROOM keys ORDER keys CLASSES
   {: table:^ room:^ order:^ cl:^ :}
   0 b a ?do
      cl table room order i NTH d CLASS COUNT-KEY
   loop ;

\ Turns a class's count into its first place, s; answers the place
\ after the class.
: START-CLASS ( n ^n -- n )
   {: s:n at:^ :}
   at @ s +  s at ! ;

\ Places the m seen classes from a in the order first seen.
: PLACE-CLASSES ( ^n n n -- )
   {: cl:^ a:n m:n :}
   a m 0 ?do cl  cl i SEEN-AT @ cells +  START-CLASS loop
   drop ;

\ Puts key idx at its class's next place in spare.
: PUT ( ^n ^u8 ^n ^n n n -- )
   {: table:^ room:^ spare:^ cl:^ idx:n d:n :}
   cl  table room idx d CLASS  cells + {: at:^ :}
   idx  spare at @ cells + !
   at @ 1+ at ! ;

\ Moves the keys of order[a..b) to their classes' places through spare,
\ each class in document order.
: SCATTER ( ^keys n n n -- )
   {: keys:^ a:n b:n d:n :}
   keys TABLE keys ROOM keys SPARE keys CLASSES keys ORDER
   {: table:^ room:^ spare:^ cl:^ order:^ :}
   b a ?do table room spare cl order i NTH d PUT loop
   b a ?do spare i NTH  order i cells + ! loop ;

\ Lists group order[s..e), three or more keys agreeing on d bytes, in
\ its idle spare cells s..s+2 ahead of head; answers the new head.
: WAIT ( ^keys n n n n -- n )
   {: keys:^ s:n e:n d:n head:n :}
   keys SPARE s cells + {: w:^ :}
   e w !  d w 1 cells + !  head w 2 cells + !
   s ;

\ best, or the second of order[s..s+2) when the two keys, agreeing on
\ d bytes, are equal.
: PAIR-REPEAT ( n ^keys n n -- n )
   {: best:n keys:^ s:n d:n :}
   keys TABLE keys ROOM keys ORDER {: table:^ room:^ order:^ :}
   table room order s NTH d KEY-FROM$
   table room order s 1+ NTH d KEY-FROM$ STR:=
   if order s 1+ NTH best min exit then best ;

\ Settles group order[s..e), agreeing on d bytes: fewer than two keys
\ are done, two compared and more wait; answers best and the head.
: SETTLE ( n n ^keys n n n -- n n )
   {: best:n head:n keys:^ s:n e:n d:n :}
   e s - 2 < if best head exit then
   e s - 2 = if best keys s d PAIR-REPEAT head exit then
   best  keys s e d head WAIT ;

\ best, or the second of order[s..e), keys that all end at the same
\ byte, when it holds two or more.
: ENDED-REPEAT ( n ^keys n n -- n )
   {: best:n keys:^ s:n e:n :}
   e s - 2 < if best exit then
   keys ORDER s 1+ NTH best min ;

\ Seen class j and the place after its keys, its cell zeroed for the
\ next pass.
: TAKE-CLASS ( ^n n -- n n )
   {: cl:^ j:n :}
   cl j SEEN-AT @ {: c:n :}
   c  cl c cells + @
   0 cl c cells + ! ;

\ Settles seen class j of a pass at byte d, its keys from s; answers
\ where the next class starts.
: SETTLE-CLASS ( n n n ^keys n n -- n n n )
   {: best:n head:n s:n keys:^ j:n d:n :}
   keys CLASSES j TAKE-CLASS {: c:n e:n :}
   c 0= if best keys s e ENDED-REPEAT head e exit then
   best head keys s e d 1+ SETTLE e ;

\ Settles the m classes a pass at byte d placed from a.
: SETTLE-CLASSES ( n n ^keys n n n -- n n )
   {: best:n head:n keys:^ a:n m:n d:n :}
   best head a
   m 0 ?do keys i d SETTLE-CLASS loop drop ;

\ Splits group order[a..b), agreeing on d bytes, by the first byte
\ after those all its keys share, and settles each part.
: GROUP-PASS ( n n ^keys n n n -- n n )
   {: best:n head:n keys:^ a:n b:n d:n :}
   keys a b d PREFIX {: p:n :}
   keys a b p COUNT-CLASSES {: m:n :}
   keys CLASSES a m PLACE-CLASSES
   keys a b p SCATTER
   best head keys a m p SETTLE-CLASSES ;

\ Takes the group at head off the list and runs its pass.
: NEXT-GROUP ( n n ^keys -- n n )
   {: best:n head:n keys:^ :}
   keys SPARE head cells + {: w:^ :}
   best  w 2 cells + @
   keys head  w @  w 1 cells + @  GROUP-PASS ;

\ The least repeat among keys lo..k-1, k when there is none; the list
\ of waiting groups ends at k.
: GROUP-REPEAT ( ^keys n n -- n )
   {: keys:^ lo:n k:n :}
   k k keys lo k 0 SETTLE
   begin dup k <> while keys NEXT-GROUP repeat drop ;


\ The earliest of keys lo..k-1 that decodes as one before it in the
\ range.
: FIRST-REPEAT ( ^keys n -- option<n> )
   {: keys:^ lo:n :}
   keys TABLE @ {: k:n :}
   keys KEYS-INDICES @ k VEC:ENSURE
   keys KEYS-SPARE @ k VEC:ENSURE
   keys ORDER lo k FILL-ORDER
   keys lo k GROUP-REPEAT {: at:n :}
   at k = if OPTION:NONE exit then
   at OPTION:SOME ;


\ The answer stands only once the whole text has read as well formed.
: REPEAT-IN ( ^cursor ^keys str -- option<n> )
   {: a:^ keys:^ text:str :}
   a text INIT-STATE
   a NEXT-INNER T-OBJ <> if
      a s" an object" s" write the text as one JSON object"
      E-JSON-READ-STATE REJECT-TOKEN
   then
   a keys RECORD-KEYS
   begin a NEXT-INNER T-END = until
   keys 0 FIRST-REPEAT ;

: REPEAT-WITH ( str ^cursor -- option<n> )
   {: text:str a:^ :}
   [: a [: text REPEAT-IN ;] WITH-KEYS ;]
   [: a MORE-RELEASE ;] finally ;

public

: READ-REPEAT ( str -- option<n> )
   {: text:str :}
   text REQUIRE-SOURCE
   text [type] cursor [: REPEAT-WITH ;] MEM:WITH-RECORD ;

;package
