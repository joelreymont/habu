\ utf8-scalar.f - strict reentrant UTF-8 scalar decoding with byte fallback. NEXT
\ answers SCALAR and the next cursor for a canonical sequence, else RAW-BYTE with
\ the lead byte and cursor+1: malformed, truncated, non-shortest, surrogate and
\ out-of-range input is data and never throws. VALID? is true when every step is a
\ SCALAR (empty included). FIT cuts a span at a cap without ending in a character.
\ SUBPART-END answers the end of the longest prefix of a well-formed sequence at
\ a cursor (Unicode 3.9, Table 3-7), at least the cursor's byte. CUT? is true
\ when the bytes from a cursor to the end, at least one, are a proper prefix of
\ a well-formed sequence.
\ Negative lengths, caps and cursors are E-STR-BOUNDS. STORAGE CLASS. CALLER-OWNED.

require src/lib/error.f

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

: LEAD@ ( ^u8 n n -- n ) {: source:^ length:n cursor:n :}
   length cursor CURSOR-CHECK
   source cursor + c@ ;

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

: FOLD-CONTINUATIONS ( ^u8 n n n n -- n bool )
   {: source:^ remaining:n width:n index:n scalar:n :}
   index width = if
      scalar YES exit
   then
   index remaining >= if
      scalar NO exit
   then
   source index + c@ {: byte:n :}
   byte CONTINUATION? 0= if
      scalar NO exit
   then
   source remaining width index 1+
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

: DECODE-MULTIBYTE ( ^u8 n n n n -- scalar-step )
   {: source:^ remaining:n cursor:n lead:n width:n :}
   source remaining width RAW-WIDTH lead width LEAD-SEED
   FOLD-CONTINUATIONS if
      cursor lead width DECODED-STEP
   else
      drop lead cursor >RAW-BYTE
   then ;

: DECODE-LEAD ( ^u8 n n n -- scalar-step )
   {: source:^ remaining:n cursor:n lead:n :}
   lead ASCII-LIMIT < if
      lead cursor RAW-WIDTH >SCALAR exit
   then
   lead TWO-LEAD-FIRST < if
      lead cursor >RAW-BYTE exit
   then
   lead THREE-LEAD-FIRST < if
      source remaining cursor lead TWO-WIDTH DECODE-MULTIBYTE exit
   then
   lead FOUR-LEAD-FIRST < if
      source remaining cursor lead THREE-WIDTH DECODE-MULTIBYTE exit
   then
   lead INVALID-LEAD-FIRST < if
      source remaining cursor lead FOUR-WIDTH DECODE-MULTIBYTE exit
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

: SUBPART ( ^u8 n n n -- n ) {: at:^ limit:n lead:n index:n :}
   index limit >= if index exit then
   lead index at index + c@ TRAIL-OK? 0= if index exit then
   at limit lead index 1+ recurse ;

public

: NEXT ( ^u8 n n -- scalar-step ) {: source:^ length:n cursor:n :}
   source length cursor LEAD@ {: lead:n :}
   source cursor + length cursor - cursor lead DECODE-LEAD ;

: VALID? ( ^u8 n -- bool )
   {: source:^ length:n :}
   length NONNEGATIVE
   0 begin dup length < while
      dup source + c@ ASCII-LIMIT < if
         1+
      else
         source length rot NEXT
         MATCH scalar-step
            scalar OF nip ENDOF
            raw-byte OF 2drop NO exit ENDOF
         ;MATCH
      then
   repeat
   drop YES ;

: SUBPART-END ( ^u8 n n -- n ) {: source:^ length:n cursor:n :}
   source length cursor LEAD@ {: lead:n :}
   source cursor + {: at:^ :}
   lead LEAD-WIDTH length cursor - min {: limit:n :}
   cursor at limit lead 1 SUBPART + ;

: CUT? ( ^u8 n n -- bool ) {: source:^ length:n cursor:n :}
   length NONNEGATIVE
   cursor length = if NO exit then
   source length cursor LEAD@ LEAD-WIDTH length cursor - > if
      source length cursor SUBPART-END length =
   else
      NO
   then ;

\ A cap before a continuation byte backs up to its lead, and never more than a
\ character's three trailing bytes, however many stray continuations precede it.
: FIT ( ^u8 n n -- n ) {: source:^ length:n cap:n :}
   length NONNEGATIVE  cap NONNEGATIVE
   length cap <= if length exit then
   cap begin
      dup 0 > over cap FOUR-WIDTH - 1+ > and if
         dup source + c@ CONTINUATION?
      else NO then
   while 1- repeat ;

;package
