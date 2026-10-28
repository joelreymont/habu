\ json-read.f - checked JSON pull/cursor parser.
\
\ STORAGE CLASS. CALLER-OWNED, so this module is safe from any number of
\ tasks. The reader holds no module state at all: the caller declares a
\ JR:cursor record (`TYPED-VARIABLE X JR:cursor`, `n TYPED-BUFFER XS
\ JR:cursor` or `FIELD rd JR:cursor`) and owns both that record and the source
\ span, so two tasks parsing two documents share nothing. Typed storage is
\ zero-filled, and a zero-filled record owns nothing.
\
\ The parser is a cursor over the caller's source buffer and record. INIT
\ returns a linear reader; every operation threads that reader and CLOSE consumes
\ it. No valid document is refused for its depth, and no caller sizes storage:
\ the record keeps the container stack one bit per open container, 64 levels
\ inline, so a document nested at most 64 deep parses with no allocation and no
\ process-global state. Past 64 levels the record owns one mapping the reader
\ grows by doubling; a failed growth is MEM's E-MEM-MAP or E-MEM-SIZE, recorded
\ as the reader's failure. CLOSE, FINISH and the first advance failure release
\ that mapping. A caller whose own throw abandons a live reader leaves the
\ mapping in the record: the next reader on that record reuses it and its CLOSE
\ releases it, and DISPOSE, which is idempotent, releases it at the record's
\ end of life. A static record that is only ever re-INITed needs no DISPOSE.
\
\ Structural well-formedness (RFC 8259: strict, no comments, no trailing commas,
\ no single quotes, string escapes, and Unicode scalar encoding) is validated as
\ the cursor advances; number and value decode happen on demand. Every failure
\ is a named throw in the -3900..-3999 JSON-reader block, apart from a failed
\ growth. A throw from an advance leaves the reader failed: every later NEXT,
\ SKIP-VALUE, VALUE-SPAN$, FIND-KEY or FINISH that would move the cursor throws
\ that first code again. Accessor refusals (INT, FLOAT, STR, STR-EQ?) and
\ wrong-phase E-JR-STATE refusals leave the reader usable.
\
\ The parser lives in `package JR`. External callers use the record type
\ JR:cursor, the qualified public API (JR:INIT, JR:CLOSE, JR:FINISH,
\ JR:DISPOSE, JR:NEXT, JR:TOKEN, JR:SPAN$, JR:VALUE-SPAN$, JR:INT, JR:FLOAT,
\ JR:STR, JR:STR-EQ?, JR:SKIP-VALUE, JR:FIND-KEY) and the qualified token kinds
\ (JR:T-OBJ .. JR:T-END). Callers keep both record and source live and
\ exclusive until CLOSE or FINISH. The record's fields, the byte constants, the
\ reader's LINEAR: mint and erase, cursor state, and scan/decode helpers -
\ including AT, which reads a source byte its caller has bounds-checked - are
\ package-private and sealed after the module is assembled.
\
\ INIT refuses a negative source length or a null source with bytes (E-SOURCE)
\ before it mints the reader. The checker cannot prove the lifetime or
\ exclusivity of the record and the source; those stay the caller's promise.
\ FINISH reads the rest of the document from wherever the cursor stands, so a
\ caller that took only the members it needs still learns the document is well
\ formed. SPAN$ borrows the raw lexeme, escapes encoded; STR decodes into
\ non-null caller storage (a negative or short capacity is E-JR-STATE).
\ VALUE-SPAN$ on an opener answers the source bytes through the matching close
\ and moves past the value. FIND-KEY searches only the current object, skipping
\ each unmatched value, and stops at its close; it and STR-EQ? compare decoded
\ bytes without a buffer, so key length is not bounded by reader storage.
\
\ Cursor state machine (STATE): a value token drives one transition each.
\  ST-VALUE   value required (doc start / after ':' / after ',' in an array)
\  ST-ELEM    array just opened: a value or ']'
\  ST-KEY     object just opened: a key string or '}'
\  ST-MEMBER  after ',' in an object: a key string (no '}': trailing comma)
\  ST-SEP     value complete inside a container: ',' or the matching close
\  ST-DONE    top-level value complete: only whitespace then JR:T-END
\ A ',' in ST-SEP is consumed silently (STEP returns RETRY and NEXT loops);
\ container closes are emitted as T-OBJ-END / T-ARR-END.

require src/lib/errors.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/string.f
require src/lib/string-roles.f               \ package STR: the typed string surface
require src/lib/float.f
require src/lib/adt/option.f                 \ option<NUM:index> for STR:INDEX-OF (switchover wave A)

\ JSON reader: -3900..-3999, minted here beside the package its last code
\ (JR:E-SOURCE) belongs to (docs/forth-card.md § 6). The E-JR-* codes stay
\ global names, as callers spell them.
-3900 constant E-JR-FIRST
-3999 constant E-JR-LAST
-3900 constant E-JR-MALFORMED   \ value expected but got a bare word / bad char / trailing comma
-3901 constant E-JR-STRING      \ unterminated string or unescaped control byte in a string
-3902 constant E-JR-ESCAPE      \ invalid backslash or \uXXXX escape sequence
-3903 constant E-JR-SURROGATE   \ lone or mismatched UTF-16 surrogate in a \u escape
-3905 constant E-JR-TRAILING    \ non-whitespace after the top-level value
-3906 constant E-JR-NUMBER      \ malformed number token or integer overflow
-3907 constant E-JR-EOF         \ input ended while a value/container/key was expected
-3908 constant E-JR-COLON       \ missing ':' after an object key
-3909 constant E-JR-COMMA       \ expected ',' or the matching container close
-3910 constant E-JR-STATE       \ accessor called on the wrong token kind or dst buffer too small

package JR

public

-3914 constant E-SOURCE      \ source length is negative or a positive length has a null source

DEFLINEAR JR:reader

\ The reader's whole state. src..fail are the cursor and decode registers;
\ kinds is the container stack's first 64 levels, bit i set when level i is an
\ object; more is the mapping holding the rest, more-cells cells of it, and
\ more-cells 0 means the record owns no mapping.
STRUCTURE cursor OPAQUE DERIVE addr
   FIELD src ptr u8
   FIELD src-len n
   FIELD pos n
   FIELD depth n
   FIELD state n
   FIELD kind n
   FIELD tok-at n
   FIELD tok-len n
   FIELD dst ptr u8
   FIELD dst-cap n
   FIELD out n
   FIELD in n
   FIELD num-at n
   FIELD mode n
   FIELD matched n
   FIELD fail n
   FIELD kinds n
   FIELD more ptr u8
   FIELD more-cells n
;STRUCTURE

private

\ ---- byte constants -------------------------------------------------------
9 constant TAB
10 constant LF
13 constant CR
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

\ ---- unicode / utf-8 ------------------------------------------------------
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
$10000 constant SUR-BASE
10 constant SUR-SHIFT

\ ---- token kinds (public: JR:T-*) -----------------------------------------
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

private

99 constant RETRY

\ ---- parser state ---------------------------------------------------------
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

\ ---- reader token ---------------------------------------------------------
\ A JR:reader is the caller's record under a linear token. A linear token's
\ payload is a scalar con, so the record crosses as raw cells only through this
\ private cast pair, and these private rows are its only mint and erase: outside
\ JR a reader comes only from INIT and ends only at CLOSE.
CAST: >CELLS ( ptr cursor -- ptr n )
CAST: >CURSOR ( ptr n -- ptr cursor )
LINEAR: MINT-READER ( ptr n -- JR:reader )
LINEAR: ERASE-READER ( JR:reader -- ptr n )

\ Keep the token live while exposing its record.
: READER>STATE ( JR:reader -- JR:reader ptr cursor )
   ERASE-READER dup MINT-READER swap >CURSOR ;

: INIT-STATE ( ptr cursor ptr u8 n -- ) {: state:ptr source:ptr len:n :}
   source state CURSOR-SRC !
   len state CURSOR-SRC-LEN !
   0 state CURSOR-POS !
   0 state CURSOR-DEPTH !
   ST-VALUE state CURSOR-STATE !
   T-END state CURSOR-KIND !
   0 state CURSOR-TOK-AT !
   0 state CURSOR-TOK-LEN !
   UMODE-WRITE state CURSOR-MODE !
   1 state CURSOR-MATCHED !
   0 state CURSOR-FAIL ! ;

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

\ ---- source cursor --------------------------------------------------------
: AT ( ptr cursor n -- n ) {: state:ptr idx:n :}
   state CURSOR-SRC @ idx + c@ ;

: EOF? ( ptr cursor -- bool ) {: state:ptr :}
   state CURSOR-POS @ state CURSOR-SRC-LEN @ >= ;

: PEEK ( ptr cursor -- n ) {: state:ptr :}
   state state CURSOR-POS @ AT ;

: ADVANCE ( ptr cursor -- ) {: state:ptr :}
   state CURSOR-POS dup @ 1+ swap ! ;

\ ---- container stack storage ----------------------------------------------
\ The mapping as the span it is: more-cells is the ownership token, nonzero
\ exactly while the record owns a mapping of that many cells at more.
: MORE-SPAN ( ptr cursor -- SPAN:span<u8> ) {: state:ptr :}
   state CURSOR-MORE @ state CURSOR-MORE-CELLS @ cells SPAN:MAKE ;

: MORE-RELEASE ( ptr cursor -- ) {: state:ptr :}
   state CURSOR-MORE-CELLS @ 0= if exit then
   state MORE-SPAN {: old :}
   NULL-PTR state CURSOR-MORE !
   0 state CURSOR-MORE-CELLS !
   old MEM:FREE-SPAN ;

\ Make stack word w (w >= 1 lives in cell w-1 of the mapping) exist. Levels
\ are pushed one at a time, so one doubling always covers w. The old words are
\ copied before the new mapping is installed, and the old one is released last.
: MORE-ENSURE ( ptr cursor n -- ) {: state:ptr w:n :}
   w state CURSOR-MORE-CELLS @ <= if exit then
   state CURSOR-MORE-CELLS @ {: have:n :}
   have 0= if FIRST-MORE else have 2 * then {: want:n :}
   want cells MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: grown :}
   state MORE-SPAN {: old :}
   old SPAN:$ grown SPAN:COPY
   grown SPAN:$ drop state CURSOR-MORE !
   want state CURSOR-MORE-CELLS !
   have 0<> if old MEM:FREE-SPAN then ;

\ The cell holding container level's stack word. The level's word exists: it
\ is below the depth, or PUSH ensured it.
: KIND-WORD ( ptr cursor n -- ptr n ) {: state:ptr level:n :}
   level WORD-BITS / {: w:n :}
   w 0= if state CURSOR-KINDS exit then
   state CURSOR-MORE @ w 1- cells + CELL-VIEW ;

\ After the first advance failure released the mapping, nothing may read the
\ stack: every path that would throws the failure again first.
: REQUIRE-SOUND ( ptr cursor -- ) {: state:ptr :}
   state CURSOR-FAIL @ {: failed:n :}
   failed 0<> if failed throw then ;

public

: INIT ( ptr cursor ptr u8 n -- JR:reader )
   {: state:ptr source:ptr len:n :}
   len 0 < if E-SOURCE throw then
   len 0 > source 0= and if E-SOURCE throw then
   state source len INIT-STATE
   state >CELLS MINT-READER ;

\ CLOSE consumes the token and releases the record's mapping.
: CLOSE ( JR:reader -- )
   ERASE-READER >CURSOR MORE-RELEASE ;

\ Release what a record owns after a caller's throw abandoned its live reader.
: DISPOSE ( ptr cursor -- )
   MORE-RELEASE ;

: TOKEN ( JR:reader -- JR:reader n )
   READER>STATE CURSOR-KIND @ ;

: SPAN$ ( JR:reader -- JR:reader ptr u8 n )
   READER>STATE {: state:ptr :}
   state CURSOR-SRC @ state CURSOR-TOK-AT @ +
   state CURSOR-TOK-LEN @ ;

private

\ ---- whitespace -----------------------------------------------------------
: WS? ( n -- bool )
   dup SP = over TAB = or over LF = or swap CR = or ;

: SKIP-WS ( ptr cursor -- ) {: state:ptr :}
   begin
      state CURSOR-POS @ state CURSOR-SRC-LEN @ < if
         state CURSOR-SRC @ state CURSOR-POS @ + c@ WS?
      else
         FALSE
      then
   while
      state CURSOR-POS dup @ 1+ swap !
   repeat ;

\ ---- container stack ------------------------------------------------------
\ One bit per level: set for an object, clear for an array.
: LEVEL-MASK ( n -- n )
   WORD-BITS mod 1 swap lshift ;

: PUSH ( ptr cursor bool -- ) {: state:ptr obj:bool :}
   state CURSOR-DEPTH @ {: level:n :}
   state level WORD-BITS / MORE-ENSURE
   state level KIND-WORD {: word:ptr :}
   level LEVEL-MASK {: mask:n :}
   obj if word @ mask or else word @ mask invert and then word !
   level 1+ state CURSOR-DEPTH ! ;

: POP ( ptr cursor -- ) {: state:ptr :}
   state CURSOR-DEPTH dup @ 1- swap ! ;

\ Whether the innermost open container is an object.
: IN-OBJECT? ( ptr cursor -- bool ) {: state:ptr :}
   state CURSOR-DEPTH @ 1- {: level:n :}
   state level KIND-WORD @ level LEVEL-MASK and 0<> ;

: AFTER-VALUE ( ptr cursor -- ) {: state:ptr :}
   state CURSOR-DEPTH @ 0= if ST-DONE else ST-SEP then
   state CURSOR-STATE ! ;

\ ---- string token scanning ------------------------------------------------
\ The value of this hex digit byte; any other byte is a malformed escape.
: HEX-DIGIT ( n -- n )
   dup STR-HEX-DIGIT? 0= if E-JR-ESCAPE throw then
   STR-HEX-DIGIT-VALUE ;

: SCAN-HEX ( ptr cursor n -- n ) {: state:ptr value:n :}
   state EOF? if E-JR-ESCAPE throw then
   value HEX-BASE *
   state PEEK HEX-DIGIT
   +
   state ADVANCE ;

: SCAN-HEX4 ( ptr cursor -- n ) {: state:ptr :}
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

: SCAN-SURROGATE ( ptr cursor n -- ) {: state:ptr hi:n :}
   hi SUR-LO >= if E-JR-SURROGATE throw then
   state EOF? if E-JR-SURROGATE throw then
   state PEEK BACKSLASH <> if E-JR-SURROGATE throw then
   state ADVANCE
   state EOF? if E-JR-SURROGATE throw then
   state PEEK CH-U <> if E-JR-SURROGATE throw then
   state ADVANCE
   state SCAN-HEX4 {: lo:n :}
   lo SUR-LO < lo SUR-END >= or if E-JR-SURROGATE throw then ;

\ PLAIN-END admits every one-letter escape, so the backslash here starts a
\ \u escape or an invalid or unfinished one.
: SCAN-ESC ( ptr cursor -- ) {: state:ptr :}
   state ADVANCE
   state EOF? if E-JR-ESCAPE throw then
   state PEEK CH-U <> if E-JR-ESCAPE throw then
   state ADVANCE
   state SCAN-HEX4 {: cp:n :}
   cp SUR-HI < if exit then
   cp SUR-END >= if exit then
   state cp SCAN-SURROGATE ;

: REQUIRE-UTF-BYTES ( ptr cursor n -- ) {: state:ptr count:n :}
   state CURSOR-POS @ count + state CURSOR-SRC-LEN @ > if E-JR-STRING throw then ;

: CONT? ( n -- bool )
   $C0 and UTF-CONT = ;

: UTF-BYTE ( ptr cursor n -- n ) {: state:ptr off:n :}
   state state CURSOR-POS @ off + AT ;

: ADVANCE-N ( ptr cursor n -- ) {: state:ptr count:n :}
   state CURSOR-POS dup @ count + swap ! ;

: SCAN-UTF2 ( ptr cursor -- ) {: state:ptr :}
   state 2 REQUIRE-UTF-BYTES
   state 1 UTF-BYTE CONT? 0= if E-JR-STRING throw then
   state 2 ADVANCE-N ;

: SCAN-UTF3 ( ptr cursor n -- ) {: state:ptr lead:n :}
   state 3 REQUIRE-UTF-BYTES
   state 1 UTF-BYTE {: second:n :}
   second CONT? 0= state 2 UTF-BYTE CONT? 0= or if E-JR-STRING throw then
   lead UTF2-END = second UTF-E0-MIN < and if E-JR-STRING throw then
   lead UTF-SUR-LEAD = second UTF-SUR-MAX > and if E-JR-STRING throw then
   state 3 ADVANCE-N ;

: SCAN-UTF4 ( ptr cursor n -- ) {: state:ptr lead:n :}
   state 4 REQUIRE-UTF-BYTES
   state 1 UTF-BYTE {: second:n :}
   second CONT? 0= state 2 UTF-BYTE CONT? 0= or
   state 3 UTF-BYTE CONT? 0= or if E-JR-STRING throw then
   lead UTF3-END = second UTF-F0-MIN < and if E-JR-STRING throw then
   lead UTF-MAX-LEAD = second UTF-MAX-SECOND > and if E-JR-STRING throw then
   state 4 ADVANCE-N ;

: SCAN-UTF8 ( ptr cursor n -- ) {: state:ptr lead:n :}
   lead UTF2-MIN >= lead UTF2-END < and if state SCAN-UTF2 exit then
   lead UTF2-END >= lead UTF3-END < and if state lead SCAN-UTF3 exit then
   lead UTF3-END >= lead UTF4-END < and if state lead SCAN-UTF4 exit then
   E-JR-STRING throw ;

\ The offset of the first byte at or after pos that the cursor words must
\ check - a quote, a control byte, a UTF-8 lead, or the backslash of a \u,
\ invalid or unfinished escape - or the source length. Printable ASCII and
\ one-letter escapes need no other check.
: PLAIN-END ( ptr u8 n n -- n ) {: a:ptr len:n pos:n :}
   len pos ?do
      a i + c@ {: c:n :}
      c BACKSLASH = if
         i 1+ len >= if i unloop exit then
         a i 1+ + c@ SIMPLE-ESC? 0= if i unloop exit then
         2
      else
         c SP < c UTF1-MAX >= or c DQ = or if i unloop exit then
         1
      then
   +loop
   len ;

\ Move past the bytes PLAIN-END admits.
: SKIP-PLAIN ( ptr cursor -- ) {: state:ptr :}
   state CURSOR-SRC @ state CURSOR-SRC-LEN @ state CURSOR-POS @ PLAIN-END
   state CURSOR-POS ! ;

: SCAN-STRING ( ptr cursor -- n n ) {: state:ptr :}
   state ADVANCE
   state CURSOR-POS @ {: at:n :}
   begin
      state SKIP-PLAIN
      state EOF? if E-JR-STRING throw then
      state PEEK
      dup DQ = if
         drop
         at state CURSOR-POS @ at -
         state ADVANCE exit
      then
      dup BACKSLASH = if
         drop state SCAN-ESC
      else
         \ A control byte is no UTF-8 lead, so SCAN-UTF8 refuses it.
         state swap SCAN-UTF8
      then
   again ;

\ ---- number token scanning + validation -----------------------------------
: NUM-CHAR? ( n -- bool )
   dup STR-DIGIT? if drop TRUE exit then
   dup MINUS = if drop TRUE exit then
   dup PLUS = if drop TRUE exit then
   dup DOT = if drop TRUE exit then
   dup EXP-LOWER = if drop TRUE exit then
   EXP-UPPER = ;

: SCAN-NUMBER ( ptr cursor -- n n ) {: state:ptr :}
   state CURSOR-POS @ {: at:n :}
   begin
      state CURSOR-POS @ state CURSOR-SRC-LEN @ < if
         state CURSOR-SRC @ state CURSOR-POS @ + c@ NUM-CHAR?
      else
         FALSE
      then
   while
      state CURSOR-POS dup @ 1+ swap !
   repeat
   at state CURSOR-POS @ at - ;

: NV-DIGIT? ( ptr cursor ptr u8 n -- bool ) {: state:ptr a:ptr u:n :}
   state CURSOR-NUM-AT @ u >= if FALSE exit then
   a state CURSOR-NUM-AT @ + c@ STR-DIGIT? ;

: NV-SKIP-DIGITS ( ptr cursor ptr u8 n -- n ) {: state:ptr a:ptr u:n :}
   0 begin state a u NV-DIGIT? while
      state CURSOR-NUM-AT dup @ 1+ swap !
      1+
   repeat ;

: NV-INT? ( ptr cursor ptr u8 n -- bool ) {: state:ptr a:ptr u:n :}
   state a u NV-DIGIT? 0= if FALSE exit then
   a state CURSOR-NUM-AT @ + c@ ZERO = if
      state CURSOR-NUM-AT dup @ 1+ swap !
      state a u NV-DIGIT? if FALSE exit then
      TRUE exit
   then
   state a u NV-SKIP-DIGITS drop TRUE ;

: NV-FRAC? ( ptr cursor ptr u8 n -- bool ) {: state:ptr a:ptr u:n :}
   state CURSOR-NUM-AT @ u >= if TRUE exit then
   a state CURSOR-NUM-AT @ + c@ DOT <> if TRUE exit then
   state CURSOR-NUM-AT dup @ 1+ swap !
   state a u NV-SKIP-DIGITS 0= if FALSE exit then
   TRUE ;

: NV-EXP? ( ptr cursor ptr u8 n -- bool ) {: state:ptr a:ptr u:n :}
   state CURSOR-NUM-AT @ u >= if TRUE exit then
   a state CURSOR-NUM-AT @ + c@ dup EXP-LOWER <> swap EXP-UPPER <> and if TRUE exit then
   state CURSOR-NUM-AT dup @ 1+ swap !
   state CURSOR-NUM-AT @ u < if
      a state CURSOR-NUM-AT @ + c@ dup PLUS = swap MINUS = or if
         state CURSOR-NUM-AT dup @ 1+ swap !
      then
   then
   state a u NV-SKIP-DIGITS 0= if FALSE exit then
   TRUE ;

\ The kind of this number lexeme: T-INT for an integer part alone, T-FLOAT
\ once a fraction or exponent follows it. A malformed lexeme is E-JR-NUMBER.
\ READ-VALUE starts a number only at a minus or a digit, so u is positive.
: NUMBER-KIND ( ptr cursor ptr u8 n -- n ) {: state:ptr a:ptr u:n :}
   0 state CURSOR-NUM-AT !
   a state CURSOR-NUM-AT @ + c@ MINUS = if
      state CURSOR-NUM-AT dup @ 1+ swap !
   then
   state a u NV-INT? 0= if E-JR-NUMBER throw then
   state CURSOR-NUM-AT @ {: int-end:n :}
   state a u NV-FRAC? 0= if E-JR-NUMBER throw then
   state a u NV-EXP? 0= if E-JR-NUMBER throw then
   state CURSOR-NUM-AT @ u <> if E-JR-NUMBER throw then
   state CURSOR-NUM-AT @ int-end > if T-FLOAT else T-INT then ;

: RAW-SPAN$ ( ptr cursor -- ptr u8 n ) {: state:ptr :}
   state CURSOR-SRC @ state CURSOR-TOK-AT @ +
   state CURSOR-TOK-LEN @ ;

\ ---- literal tokens -------------------------------------------------------
: MATCH-LIT? ( ptr cursor ptr u8 n -- bool ) {: state:ptr la:ptr lu:n :}
   state CURSOR-POS @ lu + state CURSOR-SRC-LEN @ > if FALSE exit then
   0 begin dup lu < while
      dup la + c@
      over state CURSOR-SRC @ state CURSOR-POS @ + + c@ <> if
         drop FALSE exit
      then
      1+
   repeat drop TRUE ;

\ PUBLISH sets the token. A string, key, number or opened container becomes the
\ token only once its whole advance has passed, so a failed advance leaves the
\ previous token readable.
: PUBLISH ( ptr cursor n n n -- ) {: state:ptr at:n len:n kind:n :}
   at state CURSOR-TOK-AT !
   len state CURSOR-TOK-LEN !
   kind state CURSOR-KIND ! ;

: READ-LIT ( ptr cursor ptr u8 n n -- n ) {: state:ptr la:ptr lu:n kind:n :}
   state la lu MATCH-LIT? 0= if E-JR-MALFORMED throw then
   state state CURSOR-POS @ lu kind PUBLISH
   state CURSOR-POS dup @ lu + swap !
   state AFTER-VALUE
   kind ;

: READ-TRUE ( ptr cursor -- n )
   s" true" T-TRUE READ-LIT ;

: READ-FALSE ( ptr cursor -- n )
   s" false" T-FALSE READ-LIT ;

: READ-NULL ( ptr cursor -- n )
   s" null" T-NULL READ-LIT ;

\ ---- value readers --------------------------------------------------------
: OPEN-OBJ ( ptr cursor -- n ) {: state:ptr :}
   state CURSOR-POS @ {: at:n :}
   state CURSOR-POS dup @ 1+ swap !
   state TRUE PUSH
   state at 1 T-OBJ PUBLISH
   ST-KEY state CURSOR-STATE !
   T-OBJ ;

: OPEN-ARR ( ptr cursor -- n ) {: state:ptr :}
   state CURSOR-POS @ {: at:n :}
   state CURSOR-POS dup @ 1+ swap !
   state FALSE PUSH
   state at 1 T-ARR PUBLISH
   ST-ELEM state CURSOR-STATE !
   T-ARR ;

: CLOSE-OBJ ( ptr cursor -- n ) {: state:ptr :}
   state state CURSOR-POS @ 1 T-OBJ-END PUBLISH
   state CURSOR-POS dup @ 1+ swap !
   state POP
   state AFTER-VALUE
   T-OBJ-END ;

: CLOSE-ARR ( ptr cursor -- n ) {: state:ptr :}
   state state CURSOR-POS @ 1 T-ARR-END PUBLISH
   state CURSOR-POS dup @ 1+ swap !
   state POP
   state AFTER-VALUE
   T-ARR-END ;

: READ-STRING ( ptr cursor -- n ) {: state:ptr :}
   state state SCAN-STRING T-STR PUBLISH
   state AFTER-VALUE
   T-STR ;

: READ-KEY ( ptr cursor -- n ) {: state:ptr :}
   state SCAN-STRING {: at:n len:n :}
   state SKIP-WS
   state CURSOR-POS @ state CURSOR-SRC-LEN @ >= if E-JR-COLON throw then
   state CURSOR-SRC @ state CURSOR-POS @ + c@ COLON <> if
      E-JR-COLON throw
   then
   state CURSOR-POS dup @ 1+ swap !
   state at len T-KEY PUBLISH
   ST-VALUE state CURSOR-STATE !
   T-KEY ;

: READ-NUMBER ( ptr cursor -- n ) {: state:ptr :}
   state SCAN-NUMBER {: at:n len:n :}
   state state CURSOR-SRC @ at + len NUMBER-KIND {: kind:n :}
   state at len kind PUBLISH
   state AFTER-VALUE
   kind ;

: READ-VALUE ( ptr cursor n -- n ) {: state:ptr c:n :}
   c LBRACE = if state OPEN-OBJ exit then
   c LBRACK = if state OPEN-ARR exit then
   c DQ = if state READ-STRING exit then
   c CH-T = if state READ-TRUE exit then
   c CH-F = if state READ-FALSE exit then
   c CH-N = if state READ-NULL exit then
   c MINUS = if state READ-NUMBER exit then
   c STR-DIGIT? if state READ-NUMBER exit then
   E-JR-MALFORMED throw ;

\ ---- state dispatch -------------------------------------------------------
: DO-ELEM ( ptr cursor n -- n ) {: state:ptr c:n :}
   c RBRACK = if state CLOSE-ARR exit then
   state c READ-VALUE ;

: DO-KEY ( ptr cursor n -- n ) {: state:ptr c:n :}
   c RBRACE = if state CLOSE-OBJ exit then
   c DQ = if state READ-KEY exit then
   E-JR-MALFORMED throw ;

: DO-MEMBER ( ptr cursor n -- n ) {: state:ptr c:n :}
   c DQ = if state READ-KEY exit then
   E-JR-MALFORMED throw ;

: DO-SEP ( ptr cursor n -- n ) {: state:ptr c:n :}
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
   E-JR-COMMA throw ;

: PUBLISH-END ( ptr cursor -- n ) {: state:ptr :}
   state state CURSOR-POS @ 0 T-END PUBLISH
   T-END ;

: STEP ( ptr cursor -- n ) {: state:ptr :}
   state SKIP-WS
   state CURSOR-POS @ state CURSOR-SRC-LEN @ >= if
      state CURSOR-STATE @ ST-DONE = if state PUBLISH-END exit then
     E-JR-EOF throw
   then
   state CURSOR-SRC @ state CURSOR-POS @ + c@ {: c:n :}
   state CURSOR-STATE @ ST-DONE = if E-JR-TRAILING throw then
   state CURSOR-STATE @ ST-VALUE = if state c READ-VALUE exit then
   state CURSOR-STATE @ ST-ELEM = if state c DO-ELEM exit then
   state CURSOR-STATE @ ST-KEY = if state c DO-KEY exit then
   state CURSOR-STATE @ ST-MEMBER = if state c DO-MEMBER exit then
   state c DO-SEP ;

\ The token lands in the slot under the state, so catch sees a preserving row.
: STEP-SLOT ( n ptr cursor -- n ptr cursor ) {: state:ptr :}
   drop
   begin state STEP dup RETRY = while drop repeat
   state ;

\ Every advance comes through here. A failed STEP may leave the cursor past the
\ text it rejected while STATE still describes the text before it, so the code
\ is stored and every later advance throws it again: a reader never parses on
\ from a failure as if the rejected text had been valid. The failure also
\ releases the stack mapping, which REQUIRE-SOUND keeps anything from reading.
: NEXT-INNER ( ptr cursor -- n ) {: state:ptr :}
   state REQUIRE-SOUND
   0 state [: STEP-SLOT ;] catch nip {: token code:n :}
   code 0= if token exit then
   state MORE-RELEASE
   code state CURSOR-FAIL !
   code throw ;

public

: NEXT ( JR:reader -- JR:reader n )
   READER>STATE NEXT-INNER ;

\ FINISH reads to the document's end from any position (before the first NEXT,
\ inside containers, after the last value, or after T-END, which a done reader
\ answers again) and consumes the reader. A malformed or truncated rest throws
\ the reader's own E-JR-* code.
: FINISH ( JR:reader -- )
   begin NEXT T-END = until
   CLOSE ;

private

\ ---- number value decode --------------------------------------------------
: INT-INNER ( ptr cursor -- n ) {: state:ptr :}
   state CURSOR-KIND @ T-INT <> if E-JR-STATE throw then
   state RAW-SPAN$ STR>NUMBER? MATCH option
     none OF E-JR-NUMBER throw ENDOF
     some OF ENDOF
   ;MATCH ;

: FLOAT-INNER ( ptr cursor -- r ) {: state:ptr :}
   state CURSOR-KIND @ dup T-INT = swap T-FLOAT = or 0= if E-JR-STATE throw then
   state RAW-SPAN$ STR>FLOAT MATCH option
     none OF E-JR-NUMBER throw ENDOF
     some OF ENDOF
   ;MATCH ;

public

: INT ( JR:reader -- JR:reader n )
   READER>STATE INT-INNER ;

: FLOAT ( JR:reader -- JR:reader r )
   READER>STATE FLOAT-INNER ;

private

\ ---- string value decode (unescape into caller buffer) --------------------
: WRITE-BYTE ( ptr cursor n -- ) {: state:ptr byte:n :}
   state CURSOR-OUT @ state CURSOR-DST-CAP @ >= if E-JR-STATE throw then
   byte state CURSOR-DST @ state CURSOR-OUT @ + c! ;

: MATCH-BYTE ( ptr cursor n -- ) {: state:ptr byte:n :}
   state CURSOR-MATCHED @ 0= if exit then
   state CURSOR-OUT @ state CURSOR-DST-CAP @ >= if
      0 state CURSOR-MATCHED ! exit
   then
   state CURSOR-DST @ state CURSOR-OUT @ + c@ byte <> if
      0 state CURSOR-MATCHED !
   then ;

: EMIT-BYTE ( ptr cursor n -- ) {: state:ptr byte:n :}
   state CURSOR-MODE @ UMODE-MATCH = if
      state byte MATCH-BYTE
   else
      state byte WRITE-BYTE
   then
   state CURSOR-OUT dup @ 1+ swap ! ;

: HEX-NEXT ( ptr cursor ptr u8 n -- n ) {: state:ptr a:ptr value:n :}
   value HEX-BASE *
   a state CURSOR-IN @ + c@ STR-HEX-DIGIT-VALUE
   +
   state CURSOR-IN dup @ 1+ swap ! ;

: READ-HEX4 ( ptr cursor ptr u8 -- n ) {: state:ptr a:ptr :}
   state a 0 HEX-NEXT
   state a rot HEX-NEXT
   state a rot HEX-NEXT
   state a rot HEX-NEXT ;

: EMIT-UTF8 ( ptr cursor n -- ) {: state:ptr cp:n :}
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
: UNI ( ptr cursor ptr u8 -- ) {: state:ptr a:ptr :}
   state a READ-HEX4 {: hi:n :}
   hi SUR-HI < hi SUR-END >= or if state hi EMIT-UTF8 exit then
   state CURSOR-IN dup @ 2 + swap !
   state a READ-HEX4 {: lo:n :}
   state SUR-BASE hi SUR-HI - SUR-SHIFT lshift + lo SUR-LO - + EMIT-UTF8 ;

\ Decode from in until the token ends or a \u escape starts. Each byte and
\ one-letter escape writes or matches one byte as EMIT-BYTE would, with out on
\ the stack. A \u escape returns with in at its backslash; the registers hold
\ in and out on return.
: UNESC-RUN ( ptr cursor ptr u8 n -- ) {: state:ptr a:ptr len:n :}
   state CURSOR-DST @ {: d:ptr :}
   state CURSOR-DST-CAP @ {: cap:n :}
   state CURSOR-MODE @ UMODE-MATCH = {: match:bool :}
   UNESC-MAP BYTE-VIEW {: m:ptr :}
   state CURSOR-OUT @ len state CURSOR-IN @ ?do
      a i + c@ {: c:n :}
      c BACKSLASH = if
         a i 1+ + c@ {: e:n :}
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
         dup cap >= if E-JR-STATE throw then
         d over + b swap c!
      then
      1+ step
   +loop
   state CURSOR-OUT ! len state CURSOR-IN ! ;

: UNESC-RESET ( ptr cursor ptr u8 n n -- ) {: state:ptr dst:ptr cap:n mode:n :}
   dst state CURSOR-DST !
   cap state CURSOR-DST-CAP !
   0 state CURSOR-IN !
   0 state CURSOR-OUT !
   mode state CURSOR-MODE !
   1 state CURSOR-MATCHED ! ;

\ Only a token SCAN-STRING passed is T-STR or T-KEY, so it lies inside the
\ source and every backslash in it starts a valid escape. UNI reads a \u
\ escape's digits from a unchecked.
: UNESC-ALL ( ptr cursor -- ) {: state:ptr :}
   state CURSOR-TOK-LEN @ {: len:n :}
   state CURSOR-SRC @ state CURSOR-TOK-AT @ + {: a:ptr :}
   begin state CURSOR-IN @ len < while
      state a len UNESC-RUN
      state CURSOR-IN @ len < if
         state CURSOR-IN dup @ 2 + swap !
         state a UNI
      then
   repeat ;

: STR-INNER ( ptr cursor ptr u8 n -- n ) {: state:ptr dst:ptr cap:n :}
   state CURSOR-KIND @ dup T-STR = swap T-KEY = or 0= if E-JR-STATE throw then
   dst 0= cap 0 < or if E-JR-STATE throw then
   state dst cap UMODE-WRITE UNESC-RESET
   state UNESC-ALL
   state CURSOR-OUT @ ;

: MATCH-INNER ( ptr cursor ptr u8 n -- bool )
   {: state:ptr key:ptr len:n :}
   state CURSOR-KIND @ dup T-STR = swap T-KEY = or 0= if E-JR-STATE throw then
   state key len UMODE-MATCH UNESC-RESET
   state UNESC-ALL
   state CURSOR-MATCHED @ 0= 0=
   state CURSOR-OUT @ len = and ;

public

: STR ( JR:reader ptr u8 n -- JR:reader n )
   {: dst:ptr cap:n :}
   READER>STATE dst cap STR-INNER ;

private

\ ---- skip / find ----------------------------------------------------------
: SCALAR? ( n -- bool )
   dup T-STR = over T-INT = or over T-FLOAT = or
   over T-TRUE = or over T-FALSE = or swap T-NULL = or ;

: OPENER? ( n -- bool )
   dup T-OBJ = swap T-ARR = or ;

: CLOSER? ( n -- bool )
   dup T-OBJ-END = swap T-ARR-END = or ;

: VALUE-TOKEN? ( n -- bool )
   dup SCALAR? swap OPENER? or ;

: SKIP-INNER ( ptr cursor -- ) {: state:ptr :}
   state CURSOR-KIND @ SCALAR? if exit then
   state CURSOR-KIND @ OPENER? 0= if E-JR-STATE throw then
   1 begin dup 0 > while
      state NEXT-INNER
      dup OPENER? if drop 1+ else
      dup CLOSER? if drop 1- else
      dup T-END = if E-JR-EOF throw else drop then then then
   repeat drop ;

: FIND-PHASE? ( n -- bool )
   dup ST-KEY = swap ST-SEP = or ;

: REQUIRE-KEY-SPAN ( ptr u8 n -- ) {: key:ptr len:n :}
   len 0 < if E-JR-STATE throw then
   len 0 > key 0= and if E-JR-STATE throw then ;

: FIND-ANCHOR ( ptr cursor -- n ) {: state:ptr :}
   state CURSOR-DEPTH @ dup 0 <= if E-JR-STATE throw then
   state IN-OBJECT? 0= if E-JR-STATE throw then
   state CURSOR-STATE @ FIND-PHASE? 0= if E-JR-STATE throw then ;

: REQUIRE-KEY-DEPTH ( ptr cursor n -- ) {: state:ptr depth:n :}
   state CURSOR-DEPTH @ depth <> if E-JR-STATE throw then ;

: FIND-INNER ( ptr cursor ptr u8 n -- bool ) {: state:ptr key:ptr len:n :}
   key len REQUIRE-KEY-SPAN
   state REQUIRE-SOUND
   state FIND-ANCHOR {: depth:n :}
   begin
      state NEXT-INNER {: token:n :}
      token T-OBJ-END = if
         state depth 1- REQUIRE-KEY-DEPTH
         FALSE exit
      then
      token T-KEY <> if E-JR-STATE throw then
      state depth REQUIRE-KEY-DEPTH
      state key len MATCH-INNER {: found:bool :}
      state NEXT-INNER {: value:n :}
      value VALUE-TOKEN? 0= if E-JR-STATE throw then
      found if TRUE exit then
      state SKIP-INNER
      state depth REQUIRE-KEY-DEPTH
      state CURSOR-STATE @ ST-SEP <> if E-JR-STATE throw then
   again ;

public

: SKIP-VALUE ( JR:reader -- JR:reader )
   READER>STATE SKIP-INNER ;

private

: VALUE-SPAN-INNER ( ptr cursor -- ptr u8 n ) {: state:ptr :}
   state CURSOR-KIND @ OPENER? 0= if E-JR-STATE throw then
   state CURSOR-TOK-AT @ {: start:n :}
   state SKIP-INNER
   state CURSOR-SRC @ start +
   state CURSOR-POS @ start - ;

public

: VALUE-SPAN$ ( JR:reader -- JR:reader ptr u8 n )
   READER>STATE VALUE-SPAN-INNER ;

: FIND-KEY ( JR:reader ptr u8 n -- JR:reader bool )
   {: key:ptr len:n :}
   READER>STATE key len FIND-INNER ;

\ Whether the current string or key decodes to exactly these bytes. The bytes
\ stream through STR's decoder, so no buffer bounds the string, and the cursor
\ stays where it was.
: STR-EQ? ( JR:reader ptr u8 n -- JR:reader bool )
   {: want:ptr len:n :}
   want len REQUIRE-KEY-SPAN
   READER>STATE want len MATCH-INNER ;

private

get-current prot-wid-add

public

get-current prot-wid-add

;package
