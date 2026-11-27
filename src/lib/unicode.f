\ unicode.f - Unicode whitespace, locale-independent full casefold equality
\ and full lowercasing, from the pinned Unicode 16.0.0 case data generated
\ into src/lib/unicode/case-data.f. CASEFOLD= compares whole UTF-8 spans by
\ full case folding (Straße equals STRASSE), with no normalization, keeping
\ embedded NULs. FOLD writes a span's full case folding and LOWER its full
\ lowercase (Straße folds to strasse but lowercases to straße). LOWERS-TO?
\ tells whether a span's full lowercase is another span byte for byte, and
\ LOWER= whether two spans' full lowercases are the same bytes. No language
\ applies: each scalar maps by itself except U+03A3, which lowers by
\ Final_Sigma as Rust's str::to_lowercase decides it, to ς when, past
\ Case_Ignorable scalars, a Cased scalar precedes it and none follows.
\ Every word walks a span's mapped scalars with NEXT and allocates nothing.
\ STORAGE CLASS. PROCESS-WIDE read-only tables; any task may call.

require src/lib/sort.f
require src/lib/unicode/class.f
require src/lib/unicode/case-data.f
require src/lib/utf8-scalar.f
require src/lib/string.f

package UNICODE

public

-9150 s" a negative length or malformed UTF-8" ERROR:CODE E-UTF8
-9154 s" the destination is shorter than the mapped text: pass a larger destination"
   ERROR:CODE E-CAPACITY

EXPORT UNICODE-CLASS:WHITE-SPACE?
EXPORT UNICODE-CLASS:ALPHABETIC?
EXPORT UNICODE-CLASS:UPPERCASE?
EXPORT UNICODE-CLASS:MARK?

private

$80 constant ASCII-LIMIT
$3A3 constant CAPITAL-SIGMA
$3C3 constant SMALL-SIGMA
$3C2 constant FINAL-SIGMA
$C0 constant CONTINUATION-MASK
$80 constant CONTINUATION-TAG
21 constant TARGET-BITS
$1FFFFF constant TARGET-MASK

\ A walk's mode: a span's scalars as they are, folded or lowered.
0 constant AS-IS
1 constant FOLDING
2 constant LOWERING

: VALID-LENGTH ( n -- )
   0 < if E-UTF8 throw then ;

: VALID-UTF8 ( str -- )
   {: source:str :}
   source STR:LEN VALID-LENGTH
   source UTF8:VALID? 0= if E-UTF8 throw then ;

\ The scalar at a cursor of valid UTF-8 and the cursor after it.
: SCALAR@ ( str n -- n n )
   UTF8:NEXT MATCH UTF8:scalar-step
      scalar OF ENDOF
      raw-byte OF ENDOF
   ;MATCH ;

\ The cursor of the scalar before a cursor of valid UTF-8.
: SCALAR-BEFORE ( str n -- n )
   {: text:str at:n :}
   at 1- begin
      dup text STR:DATA + c@ CONTINUATION-MASK and
      CONTINUATION-TAG =
   while 1- repeat ;

: ENTRY ( ^n n -- ^n )
   {: table:^ idx:n :}
   table idx 2 * cells + ;

\ The last of a table's sorted two-cell entries whose first cell is at
\ most cp, and whether there is one.
: AT-MOST ( n ^n n -- ^n bool )
   {: cp:n table:^ count:n :}
   0 count [: table swap ENTRY @ cp <= ;] SORT:PARTITION-POINT
   {: at:n :}
   at 0= if table false exit then
   table at 1- ENTRY true ;

: IN-RANGES? ( n ^n n -- bool )
   {: cp:n ranges:^ count:n :}
   cp ranges count AT-MOST 0= if drop false exit then
   cell+ @ cp >= ;

: CASED? ( n -- bool )
   UNICODE-CASE-DATA:CASED-RANGES IN-RANGES? ;

: IGNORABLE? ( n -- bool )
   UNICODE-CASE-DATA:CASE-IGNORABLE-RANGES IN-RANGES? ;

\ A mapping table's packed targets for cp, else cp itself.
: MAPPING ( n ^n n -- n )
   {: cp:n table:^ count:n :}
   cp table count AT-MOST 0= if drop cp exit then
   {: entry:^ :}
   entry @ cp <> if cp exit then
   entry cell+ @ ;

\ Whether the first scalar before a cursor that is not Case_Ignorable is
\ Cased; false at the start.
: CASED-BEFORE? ( str n -- bool )
   {: text:str at:n :}
   at begin dup 0 > while
      text swap SCALAR-BEFORE {: before:n :}
      text before SCALAR@ drop {: cp:n :}
      cp IGNORABLE? 0= if cp CASED? exit then
      before
   repeat
   drop false ;

\ Whether the first scalar from a cursor that is not Case_Ignorable is
\ Cased; false at the end.
: CASED-AFTER? ( str n -- bool )
   {: text:str at:n :}
   at begin dup text STR:LEN < while
      text swap SCALAR@ {: cp:n next:n :}
      cp IGNORABLE? 0= if cp CASED? exit then
      next
   repeat
   drop false ;

\ The lowercase of the U+03A3 between two cursors. A scalar both Cased
\ and Case_Ignorable is passed.
: SIGMA ( str n n -- n )
   {: text:str at:n next:n :}
   text at CASED-BEFORE? text next CASED-AFTER? 0= and
   if FINAL-SIGMA else SMALL-SIGMA then ;

\ The packed targets of the scalar cp between two cursors in a mode.
: MAPPED ( str n n n n -- n )
   {: text:str at:n next:n cp:n mode:n :}
   mode AS-IS = if cp exit then
   cp ASCII-LIMIT < if cp STR:ASCII-LOWER exit then
   mode FOLDING = if
      cp UNICODE-CASE-DATA:FOLD-MAPPINGS MAPPING exit
   then
   cp CAPITAL-SIGMA = if text at next SIGMA exit then
   cp UNICODE-CASE-DATA:LOWER-MAPPINGS MAPPING ;

\ The rest of packed targets and the first.
: TARGET ( n -- n n )
   {: packed:n :}
   packed TARGET-BITS rshift packed TARGET-MASK and ;

\ One step of a valid span's walk in a mode from a cursor and packed
\ targets left: the next of both, a mapped scalar and true, or false.
: NEXT ( n n str n -- n n n bool )
   {: at:n rest:n text:str mode:n :}
   rest 0<> if at rest TARGET true exit then
   at text STR:LEN >= if at 0 0 false exit then
   text at SCALAR@ {: cp:n next:n :}
   next text at next cp mode MAPPED TARGET true ;

: SCALAR-LENGTH ( n -- n )
   {: cp:n :}
   cp $80 < if 1 exit then
   cp $800 < if 2 exit then
   cp $10000 < if 3 exit then
   4 ;

: LEAD ( n -- n )
   {: length:n :}
   length 2 = if $C0 exit then
   length 3 = if $E0 exit then
   $F0 ;

\ Writes a scalar's UTF-8 and answers the address after it.
: SCALAR! ( n ^u8 -- ^u8 )
   {: cp:n out:^ :}
   cp SCALAR-LENGTH {: length:n :}
   length 1 = if cp out c! out 1+ exit then
   cp length 1- 6 * rshift length LEAD or out c!
   length 1 ?do
      cp length 1- i - 6 * rshift $3F and $80 or out i + c!
   loop
   out length + ;

\ The UTF-8 length of a valid span's walk.
: WALK-LENGTH ( str n -- n )
   {: text:str mode:n :}
   0 0 0 begin
      {: size:n at:n rest:n :}
      at rest text mode NEXT {: next:n left:n cp:n more:bool :}
      more 0= if size exit then
      size cp SCALAR-LENGTH + next left
   again ;

: WALK-WRITE ( str n ^u8 -- )
   {: text:str mode:n out:^ :}
   out 0 0 begin
      {: to:^ at:n rest:n :}
      at rest text mode NEXT {: next:n left:n cp:n more:bool :}
      more 0= if exit then
      cp to SCALAR! next left
   again ;

\ Writes a span's walk only when the destination holds all of it.
: MAP-INTO ( str ^u8 n n -- n )
   {: text:str out:^ capacity:n mode:n :}
   text VALID-UTF8
   text mode WALK-LENGTH {: size:n :}
   capacity size < if E-CAPACITY throw then
   text mode out WALK-WRITE
   size ;

\ Whether two valid spans' walks answer the same scalars.
: SAME-WALKS? ( str n str n -- bool )
   {: left:str lmode:n right:str rmode:n :}
   0 0 0 0 begin
      {: la:n lr:n ra:n rr:n :}
      la lr left lmode NEXT {: la2:n lr2:n lc:n lmore:bool :}
      ra rr right rmode NEXT {: ra2:n rr2:n rc:n rmore:bool :}
      lmore 0= if rmore 0= exit then
      rmore 0= if false exit then
      lc rc <> if false exit then
      la2 lr2 ra2 rr2
   again ;

public

: CASEFOLD= ( str str -- bool )
   {: left:str right:str :}
   left VALID-UTF8 right VALID-UTF8
   left FOLDING right FOLDING SAME-WALKS? ;

: FOLDED-BYTES ( str -- n )
   dup VALID-UTF8 FOLDING WALK-LENGTH ;

: FOLD ( str ^u8 n -- n )
   FOLDING MAP-INTO ;

: LOWERED-BYTES ( str -- n )
   dup VALID-UTF8 LOWERING WALK-LENGTH ;

: LOWER ( str ^u8 n -- n )
   LOWERING MAP-INTO ;

: LOWERS-TO? ( str str -- bool )
   {: source:str target:str :}
   target VALID-UTF8 source VALID-UTF8
   source LOWERING target AS-IS SAME-WALKS? ;

: LOWER= ( str str -- bool )
   {: left:str right:str :}
   left VALID-UTF8 right VALID-UTF8
   left LOWERING right LOWERING SAME-WALKS? ;

seal-package
;package
