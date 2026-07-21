\ utf8-scalar.f - strict reentrant UTF-8 scalar decoding with byte fallback.
\
\ NEXT takes an explicit counted byte span and cursor. A valid canonical UTF-8
\ sequence returns SCALAR with its code point and absolute next cursor. Invalid
\ UTF-8 returns RAW-BYTE with the exact lead byte and cursor+1. Cursor misuse is
\ a bounds error; malformed input is data and never throws.

require lib/errors.f

package UTF8

public

SUMTYPE scalar-step 0
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

: LEAD@ ( ptr u8 n n -- n ) {: source:ptr length:n cursor:n :}
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

: FOLD-CONTINUATIONS ( ptr u8 n n n n -- n bool )
   {: source:ptr remaining:n width:n index:n scalar:n :}
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

: DECODE-MULTIBYTE ( ptr u8 n n n n -- scalar-step )
   {: source:ptr remaining:n cursor:n lead:n width:n :}
   source remaining width RAW-WIDTH lead width LEAD-SEED
   FOLD-CONTINUATIONS if
      cursor lead width DECODED-STEP
   else
      drop lead cursor >RAW-BYTE
   then ;

: DECODE-LEAD ( ptr u8 n n n -- scalar-step )
   {: source:ptr remaining:n cursor:n lead:n :}
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

public

: NEXT ( ptr u8 n n -- scalar-step ) {: source:ptr length:n cursor:n :}
   source length cursor LEAD@ {: lead:n :}
   source cursor + length cursor - cursor lead DECODE-LEAD ;

;package
