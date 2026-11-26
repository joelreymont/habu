\ utf8-scalar.f - strict reentrant UTF-8 scalar decoding with byte fallback. NEXT
\ answers SCALAR and the next cursor for a canonical sequence, else RAW-BYTE with
\ the lead byte and cursor+1: malformed, truncated, non-shortest, surrogate and
\ out-of-range input is data and never throws. VALID? is true when every step is a
\ SCALAR (empty included). FIT cuts a span at a cap without splitting a character.
\ SUBPART-END answers the end of the longest prefix of a well-formed sequence at
\ a cursor (Unicode 3.9, Table 3-7), at least the cursor's byte. CUT? is true
\ when the bytes from a cursor to the end, at least one, are a proper prefix of
\ a well-formed sequence. APPEND-SCALAR appends a scalar's shortest UTF-8 to a
\ vector.
\ Negative lengths, caps and cursors are E-STR-BOUNDS. STORAGE CLASS. CALLER-OWNED.

require src/lib/error.f
require src/lib/vector.f

package UTF8

public

SUMTYPE scalar-step
   VARIANT scalar n n ;VARIANT
   VARIANT raw-byte n n ;VARIANT
;SUMTYPE

private

$80 constant ASCII-LIMIT
$C0 constant TWO-LEAD-FIRST
$E0 constant THREE-LEAD-FIRST
$F0 constant FOUR-LEAD-FIRST
$F8 constant INVALID-LEAD-FIRST

$1F constant TWO-LEAD-MASK
$0F constant THREE-LEAD-MASK
$07 constant FOUR-LEAD-MASK
$C0 constant CONTINUATION-MASK
$80 constant CONTINUATION-TAG
$3F constant CONTINUATION-PAYLOAD-MASK

1 constant RAW-WIDTH
2 constant TWO-WIDTH
3 constant THREE-WIDTH
4 constant FOUR-WIDTH
6 constant CONTINUATION-SHIFT

$80 constant TWO-MINIMUM
$800 constant THREE-MINIMUM
$10000 constant FOUR-MINIMUM
$D800 constant SURROGATE-FIRST
$DFFF constant SURROGATE-LAST
$10FFFF constant SCALAR-LAST

\ Table 3-7's leads, and the bytes each allows second; later bytes are
\ CONTINUATION-LEAST..CONTINUATION-MOST.
$C2 constant TWO-LEAD-LEAST
$ED constant SURROGATE-LEAD
$F4 constant FOUR-LEAD-MOST
$80 constant CONTINUATION-LEAST
$BF constant CONTINUATION-MOST
$A0 constant E0-SECOND-LEAST
$9F constant ED-SECOND-MOST
$90 constant F0-SECOND-LEAST
$8F constant F4-SECOND-MOST

: YES ( -- bool )
   0 0= ;

: NO ( -- bool )
   0 0= 0= ;

: NONNEGATIVE ( n -- )
   0 < if
      E-STR-BOUNDS throw
   then ;

: CURSOR-CHECK ( n n -- ) {: length:n cursor:n :}
   length NONNEGATIVE
   cursor NONNEGATIVE
   cursor length >= if
      E-STR-BOUNDS throw
   then ;

: LEAD@ ( str n -- n ) {: source:str cursor:n :}
   source STR:LEN cursor CURSOR-CHECK
   source STR:DATA cursor + c@ ;

: CONTINUATION? ( n -- bool )
   CONTINUATION-MASK and CONTINUATION-TAG = ;

: SURROGATE? ( n -- bool ) {: scalar:n :}
   scalar SURROGATE-FIRST >=
   scalar SURROGATE-LAST <= and ;

: CANONICAL-SCALAR? ( n n -- bool ) {: scalar:n minimum:n :}
   scalar minimum >=
   scalar SCALAR-LAST <= and
   scalar SURROGATE? 0= and ;

: LEAD-SEED ( n n -- n ) {: lead:n width:n :}
   width TWO-WIDTH = if
      lead TWO-LEAD-MASK and exit
   then
   width THREE-WIDTH = if
      lead THREE-LEAD-MASK and exit
   then
   width FOUR-WIDTH = if
      lead FOUR-LEAD-MASK and exit
   then
   E-STR-BOUNDS throw ;

: WIDTH-MINIMUM ( n -- n ) {: width:n :}
   width TWO-WIDTH = if
      TWO-MINIMUM exit
   then
   width THREE-WIDTH = if
      THREE-MINIMUM exit
   then
   width FOUR-WIDTH = if
      FOUR-MINIMUM exit
   then
   E-STR-BOUNDS throw ;

: FOLD-CONTINUATIONS ( str n n n -- n bool )
   {: source:str width:n index:n scalar:n :}
   index width = if
      scalar YES exit
   then
   index source STR:LEN >= if
      scalar NO exit
   then
   source STR:DATA index + c@ {: byte:n :}
   byte CONTINUATION? 0= if
      scalar NO exit
   then
   source width index 1+
   scalar CONTINUATION-SHIFT lshift byte CONTINUATION-PAYLOAD-MASK and or
   recurse ;

: >RAW-BYTE ( n n -- scalar-step ) {: byte:n cursor:n :}
   byte cursor RAW-WIDTH + UTF8-SCALAR--STEP:RAW-BYTE ;

: >SCALAR ( n n n -- scalar-step ) {: scalar:n cursor:n width:n :}
   scalar cursor width + UTF8-SCALAR--STEP:SCALAR ;

: DECODED-STEP ( n n n n -- scalar-step )
   {: scalar:n cursor:n lead:n width:n :}
   scalar width WIDTH-MINIMUM CANONICAL-SCALAR? if
      scalar cursor width >SCALAR
   else
      lead cursor >RAW-BYTE
   then ;

: DECODE-MULTIBYTE ( str n n n -- scalar-step )
   {: source:str cursor:n lead:n width:n :}
   source width RAW-WIDTH lead width LEAD-SEED
   FOLD-CONTINUATIONS if
      cursor lead width DECODED-STEP
   else
      drop lead cursor >RAW-BYTE
   then ;

: DECODE-LEAD ( str n n -- scalar-step )
   {: source:str cursor:n lead:n :}
   lead ASCII-LIMIT < if
      lead cursor RAW-WIDTH >SCALAR exit
   then
   lead TWO-LEAD-FIRST < if
      lead cursor >RAW-BYTE exit
   then
   lead THREE-LEAD-FIRST < if
      source cursor lead TWO-WIDTH DECODE-MULTIBYTE exit
   then
   lead FOUR-LEAD-FIRST < if
      source cursor lead THREE-WIDTH DECODE-MULTIBYTE exit
   then
   lead INVALID-LEAD-FIRST < if
      source cursor lead FOUR-WIDTH DECODE-MULTIBYTE exit
   then
   lead cursor >RAW-BYTE ;

\ A well-formed sequence's width for its lead, 1 for a byte that leads none.
: LEAD-WIDTH ( n -- n ) {: lead:n :}
   lead TWO-LEAD-LEAST < if 1 exit then
   lead THREE-LEAD-FIRST < if 2 exit then
   lead FOUR-LEAD-FIRST < if 3 exit then
   lead FOUR-LEAD-MOST <= if 4 exit then
   1 ;

: SECOND-LEAST ( n -- n ) {: lead:n :}
   lead THREE-LEAD-FIRST = if E0-SECOND-LEAST exit then
   lead FOUR-LEAD-FIRST = if F0-SECOND-LEAST exit then
   CONTINUATION-LEAST ;

: SECOND-MOST ( n -- n ) {: lead:n :}
   lead SURROGATE-LEAD = if ED-SECOND-MOST exit then
   lead FOUR-LEAD-MOST = if F4-SECOND-MOST exit then
   CONTINUATION-MOST ;

: IN-RANGE? ( n n n -- bool ) {: byte:n least:n most:n :}
   byte least >= byte most <= and ;

\ Whether `byte` may follow at `index` after `lead` in a well-formed sequence.
: TRAIL-OK? ( n n n -- bool ) {: lead:n index:n byte:n :}
   index 1 = if
      byte lead SECOND-LEAST lead SECOND-MOST IN-RANGE? exit
   then
   byte CONTINUATION-LEAST CONTINUATION-MOST IN-RANGE? ;

: SUBPART ( str n n -- n ) {: at:str lead:n index:n :}
   index at STR:LEN >= if index exit then
   lead index at STR:DATA index + c@ TRAIL-OK? 0= if index exit then
   at lead index 1+ recurse ;

\ A lead byte holding the scalar's high bits, then TAIL continuation bytes of
\ six bits each, highest first.
: APPEND-ENCODED ( n n n ^VEC:vector<u8> -- )
   {: scalar:n lead:n tail:n v:^ :}
   scalar tail CONTINUATION-SHIFT * rshift lead or v VEC:PUSH c!
   tail 0 ?do
      scalar tail 1- i - CONTINUATION-SHIFT * rshift
      CONTINUATION-PAYLOAD-MASK and CONTINUATION-TAG or v VEC:PUSH c!
   loop ;

public

\ The caller passes a scalar (0 to U+10FFFF, no surrogate); nothing checks it.
: APPEND-SCALAR ( n ^VEC:vector<u8> -- )
   {: scalar:n v:^ :}
   scalar TWO-MINIMUM < if scalar v VEC:PUSH c! exit then
   scalar THREE-MINIMUM < if scalar TWO-LEAD-FIRST 1 v APPEND-ENCODED exit then
   scalar FOUR-MINIMUM < if scalar THREE-LEAD-FIRST 2 v APPEND-ENCODED exit then
   scalar FOUR-LEAD-FIRST 3 v APPEND-ENCODED ;

: NEXT ( str n -- scalar-step ) {: source:str cursor:n :}
   source cursor LEAD@ {: lead:n :}
   source STR:DATA cursor + source STR:LEN cursor - >str
   cursor lead DECODE-LEAD ;

: VALID? ( str -- bool )
   {: source:str :}
   source STR:LEN NONNEGATIVE
   0 begin dup source STR:LEN < while
      dup source STR:DATA + c@ ASCII-LIMIT < if
         1+
      else
         source swap NEXT
         MATCH scalar-step
            scalar OF nip ENDOF
            raw-byte OF 2drop NO exit ENDOF
         ;MATCH
      then
   repeat
   drop YES ;

: SUBPART-END ( str n -- n ) {: source:str cursor:n :}
   source cursor LEAD@ {: lead:n :}
   source STR:DATA cursor + {: at:^ :}
   lead LEAD-WIDTH source STR:LEN cursor - min {: limit:n :}
   cursor at limit >str lead 1 SUBPART + ;

: CUT? ( str n -- bool ) {: source:str cursor:n :}
   source STR:LEN NONNEGATIVE
   cursor source STR:LEN = if NO exit then
   source cursor LEAD@ LEAD-WIDTH source STR:LEN cursor - > if
      source cursor SUBPART-END source STR:LEN =
   else
      NO
   then ;

\ The start of the character SUBPART-END bounds that crosses the cap, else the
\ cap. Only a non-continuation byte starts one, at most three bytes before it.
: FIT ( str n -- n ) {: source:str cap:n :}
   source STR:LEN NONNEGATIVE  cap NONNEGATIVE
   source STR:LEN cap <= if source STR:LEN exit then
   cap FOUR-WIDTH 1- - 0 max {: least:n :}
   cap 1- begin
      dup least >= if
         dup source STR:DATA + c@ CONTINUATION?
      else NO then
   while 1- repeat {: start:n :}
   start least < if cap exit then
   source start SUBPART-END cap > if start else cap then ;

;package
