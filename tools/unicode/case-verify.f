\ case-verify.f - independent exhaustive verifier for the Unicode case tables.
\ It reads the four inputs with a parser of its own and checks, for every
\ scalar, UNICODE:FOLD and UNICODE:LOWER of the scalar alone against
\ CaseFolding.txt, UnicodeData.txt and SpecialCasing.txt; its Cased and
\ Case_Ignorable ranges against DerivedCoreProperties.txt; and how it sets a
\ following and a preceding U+03A3 by Final_Sigma. It then compares the table
\ with a fresh byte-identical regeneration.

require src/lib/prelude.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/unicode.f
require tools/unicode/case-generate.f
require src/lib/unicode/case-data.f
require tools/unicode/class-generate.f

package UNICODE-CASE-VERIFY

private

-7809 s" the Unicode case source data is malformed or disagrees with the case tables"
   ERROR:FAULT E-CASE-MISMATCH

$10FFFF constant SCALAR-MAX
$110000 constant SCALAR-COUNT
$D800 constant SURROGATE-FIRST
$DFFF constant SURROGATE-LAST
1 constant CASED-BIT
2 constant IGNORABLE-BIT
10 constant LINE-FEED
59 constant SEMICOLON
35 constant HASH
46 constant DOT
32 constant SPACE
64 constant CAPACITY

\ Each scalar's mapping in four cells: its target count, zero when it maps
\ to itself, then up to three targets.
DYNAMIC-BUFFER LOWER-TRUTH n
DYNAMIC-BUFFER FOLD-TRUTH n
\ Each scalar's CASED-BIT and IGNORABLE-BIT, from the source and the table.
DYNAMIC-BUFFER PROP-TRUTH n
DYNAMIC-BUFFER PROP-TABLE n
create INPUT CAPACITY allot
create WANT CAPACITY allot
create GOT CAPACITY allot

: UNICODE-PATH$ ( -- str )
   s" data/unicode/16.0.0/UnicodeData.txt" ;

: SPECIAL-PATH$ ( -- str )
   s" data/unicode/16.0.0/SpecialCasing.txt" ;

: FOLDING-PATH$ ( -- str )
   s" data/unicode/16.0.0/CaseFolding.txt" ;

: DERIVED-PATH$ ( -- str )
   s" data/unicode/16.0.0/DerivedCoreProperties.txt" ;

: DATA-PATH$ ( -- str )
   s" src/lib/unicode/case-data.f" ;

: CHECK-DIGESTS ( -- )
   UNICODE-PATH$
   s" ff58e5823bd095166564a006e47d111130813dcf8bf234ef79fa51a870edb48f"
   UNICODE-CLASS-TOOL:CHECK-FILE-DIGEST
   SPECIAL-PATH$
   s" 8d5de354eef79f2395a54c9c7dcebbaf3d30fc962d0f85611ea97aa973a0c451"
   UNICODE-CLASS-TOOL:CHECK-FILE-DIGEST
   FOLDING-PATH$
   s" 6f1f9c588eb4a5c718d9e8f93b782685e5c7fec872cf05e8e6878053599e09bb"
   UNICODE-CLASS-TOOL:CHECK-FILE-DIGEST
   DERIVED-PATH$
   s" 39d35161f2954497f69e08bdb9e701493f476a3d30222de20028feda36c1dabd"
   UNICODE-CLASS-TOOL:CHECK-FILE-DIGEST ;

: MISMATCH ( -- )
   E-CASE-MISMATCH throw ;

\ The whole file, in a buffer the process keeps.
: READ-INPUT ( str -- str )
   dup FS:FILE-SIZE 1+ MEM-ALLOC-64K-SPAN {: path:str buf:str :}
   path buf STR:DATA buf STR:LEN FS:READ-ALL {: got:n :}
   buf STR:DATA got >str ;

: SCALAR? ( n -- bool ) {: cp:n :}
   cp 0 >= cp SCALAR-MAX <= and
   cp SURROGATE-FIRST < cp SURROGATE-LAST > or and ;

: HEX-DIGIT ( n -- n bool ) {: c:n :}
   c 48 >= c 57 <= and if c 48 - true exit then
   c 65 >= c 70 <= and if c 55 - true exit then
   0 false ;

: PARSE-HEX ( str -- n ) {: s:str :}
   s STR:LEN 0= s STR:LEN 6 > or if MISMATCH then
   0 s STR:LEN 0 ?do
      16 * s STR:DATA i + c@ HEX-DIGIT 0= if MISMATCH then +
   loop ;

: BYTE-INDEX ( str n -- n ) {: s:str c:n :}
   0 begin dup s STR:LEN < while
      dup s STR:DATA + c@ c = if exit then
      1+
   repeat ;

\ Each line of a file with its comment cut and trimmed, blank ones skipped.
: EACH-ROW ( str [ str -- ] -- ) {: s:str q :}
   0 begin dup s STR:LEN <= while
      {: start:n :}
      s LINE-FEED start STR:SPLIT-NEXT {: line:str next:n valid:bool :}
      valid 0= if MISMATCH then
      line STR:DATA line HASH BYTE-INDEX >str STR:TRIM {: row:str :}
      row STR:LEN 0<> if row q execute then
      next
   repeat drop ;

\ The field of a `;`-separated row that starts at start, trimmed.
: FIELD-AT ( str n -- str n ) {: s:str start:n :}
   s SEMICOLON start STR:SPLIT-NEXT {: f:str next:n valid:bool :}
   valid 0= if MISMATCH then
   f STR:TRIM next ;

\ Stores a space-separated list of hex scalars as a count and up to three
\ scalars in four cells; a list of the scalar itself stores count zero.
: TRUTH! ( n str [ n -- ^n ] -- ) {: cp:n s:str truth :}
   cp SCALAR? 0= if MISMATCH then
   0 0 begin dup s STR:LEN <= while
      {: count:n start:n :}
      count 3 >= if MISMATCH then
      s SPACE start STR:SPLIT-NEXT {: f:str next:n valid:bool :}
      f PARSE-HEX dup SCALAR? 0= if MISMATCH then
      cp 4 * count + 1+ truth execute !
      count 1+ next
   repeat drop {: count:n :}
   count 1 = cp 4 * 1+ truth execute @ cp = and if 0 else count then
   cp 4 * truth execute ! ;

: UNICODE-LINE ( str -- ) {: row:str :}
   row 0 FIELD-AT {: code:str next:n :}
   next 12 0 ?do row swap FIELD-AT {: f:str after:n :} after loop
   row swap FIELD-AT {: lower:str after:n :}
   lower STR:LEN 0= if exit then
   code PARSE-HEX lower [: LOWER-TRUTH ;] TRUTH! ;

\ Only an unconditional row, whose fifth field is empty, sets a lowercase.
: SPECIAL-LINE ( str -- ) {: row:str :}
   row 0 FIELD-AT {: code:str next:n :}
   row next FIELD-AT {: lower:str next2:n :}
   row next2 FIELD-AT {: title:str next3:n :}
   row next3 FIELD-AT {: upper:str next4:n :}
   row next4 FIELD-AT {: condition:str next5:n :}
   condition STR:LEN 0<> lower STR:LEN 0= or if exit then
   code PARSE-HEX lower [: LOWER-TRUTH ;] TRUTH! ;

: FOLDING-LINE ( str -- ) {: row:str :}
   row 0 FIELD-AT {: code:str next:n :}
   row next FIELD-AT {: status:str next2:n :}
   row next2 FIELD-AT {: mapping:str next3:n :}
   status s" C" STR:= status s" F" STR:= or 0= if exit then
   code PARSE-HEX mapping [: FOLD-TRUTH ;] TRUTH! ;

\ `XXXX` or `XXXX..YYYY`.
: RANGE-BOUNDS ( str -- n n ) {: s:str :}
   s DOT BYTE-INDEX {: pos:n :}
   pos s STR:LEN = if s PARSE-HEX dup exit then
   pos 2 + s STR:LEN >= if MISMATCH then
   s STR:DATA pos + 1+ c@ DOT <> if MISMATCH then
   s STR:DATA pos >str PARSE-HEX
   s STR:DATA pos 2 + + s STR:LEN pos 2 + - >str PARSE-HEX ;

: PROP+ ( n n n -- ) {: lo:n hi:n bit:n :}
   hi lo < if MISMATCH then
   hi 1+ lo ?do
      i SCALAR? 0= if MISMATCH then
      i PROP-TRUTH @ bit or i PROP-TRUTH !
   loop ;

: DERIVED-LINE ( str -- ) {: row:str :}
   row 0 FIELD-AT {: range:str next:n :}
   row next FIELD-AT {: prop:str next2:n :}
   prop s" Cased" STR:= if range RANGE-BOUNDS CASED-BIT PROP+ exit then
   prop s" Case_Ignorable" STR:= if
      range RANGE-BOUNDS IGNORABLE-BIT PROP+
   then ;

: CLEAR ( -- )
   SCALAR-COUNT 4 * LOWER-TRUTH-RESERVE
   SCALAR-COUNT 4 * FOLD-TRUTH-RESERVE
   SCALAR-COUNT PROP-TRUTH-RESERVE
   SCALAR-COUNT PROP-TABLE-RESERVE
   SCALAR-COUNT 0 ?do
      0 i 4 * LOWER-TRUTH ! 0 i 4 * FOLD-TRUTH !
      0 i PROP-TRUTH ! 0 i PROP-TABLE !
   loop ;

: LOAD-TRUTH ( -- )
   CLEAR
   UNICODE-PATH$ READ-INPUT [: UNICODE-LINE ;] EACH-ROW
   SPECIAL-PATH$ READ-INPUT [: SPECIAL-LINE ;] EACH-ROW
   FOLDING-PATH$ READ-INPUT [: FOLDING-LINE ;] EACH-ROW
   DERIVED-PATH$ READ-INPUT [: DERIVED-LINE ;] EACH-ROW ;

\ Writes a scalar's UTF-8 and answers the address after it.
: ENCODE ( n ^u8 -- ^u8 ) {: cp:n at:^ :}
   cp $80 < if cp at c! at 1+ exit then
   cp $800 < if
      cp 6 rshift $C0 or at c!
      cp $3F and $80 or at 1+ c! at 2 + exit
   then
   cp $10000 < if
      cp 12 rshift $E0 or at c!
      cp 6 rshift $3F and $80 or at 1+ c!
      cp $3F and $80 or at 2 + c! at 3 + exit
   then
   cp 18 rshift $F0 or at c!
   cp 12 rshift $3F and $80 or at 1+ c!
   cp 6 rshift $3F and $80 or at 2 + c!
   cp $3F and $80 or at 3 + c! at 4 + ;

\ WANT holds the UTF-8 of a scalar's truth; answers its length.
: EXPECTED ( n [ n -- ^n ] -- n ) {: cp:n truth :}
   cp 4 * truth execute @ {: count:n :}
   count 0= if cp WANT ENCODE WANT - exit then
   WANT count 0 ?do cp 4 * i + 1+ truth execute @ swap ENCODE loop
   WANT - ;

: CHECK-MAP ( n n [ str ^u8 n -- n ] [ n -- ^n ] -- )
   {: cp:n size:n map truth :}
   INPUT size >str GOT CAPACITY map execute {: got:n :}
   cp truth EXPECTED {: want:n :}
   GOT got >str WANT want >str STR:= 0= if MISMATCH then ;

\ Lowers INPUT up to an end into GOT; answers the lowercase's length.
: LOWER-INPUT ( ^u8 -- n ) {: end:^ :}
   INPUT end INPUT - >str GOT CAPACITY UNICODE:LOWER ;

\ Whether the U+03A3 lowered at an offset of GOT's got bytes became final
\ sigma, U+03C2.
: FINAL? ( n n -- bool ) {: got:n at:n :}
   at 0 < at 2 + got > or if MISMATCH then
   GOT at + c@ $CF <> if MISMATCH then
   GOT at + 1+ c@ $82 = ;

: SIGMA-AFTER? ( n -- bool )
   INPUT ENCODE $3A3 swap ENCODE LOWER-INPUT dup 2 - FINAL? ;

: SIGMA-BETWEEN? ( n -- bool )
   65 INPUT c! INPUT 1+ ENCODE $3A3 swap ENCODE LOWER-INPUT dup 2 - FINAL? ;

: SIGMA-BEFORE? ( n -- bool )
   65 INPUT c! $3A3 INPUT 1+ ENCODE ENCODE LOWER-INPUT 1 FINAL? ;

: IGNORABLE? ( n -- bool )
   PROP-TRUTH @ IGNORABLE-BIT and 0<> ;

: CASED? ( n -- bool )
   PROP-TRUTH @ CASED-BIT and 0<> ;

: AGREE? ( bool bool -- bool )
   if else 0= then ;

\ Final in cp U+03A3 exactly when cp is Cased, not Case_Ignorable; in A cp
\ U+03A3 when cp is either; in A U+03A3 cp unless cp is Cased, not ignorable.
: CHECK-SIGMA ( n -- ) {: cp:n :}
   cp SIGMA-AFTER? cp CASED? cp IGNORABLE? 0= and AGREE? 0= if MISMATCH then
   cp SIGMA-BETWEEN? cp CASED? cp IGNORABLE? or AGREE? 0= if MISMATCH then
   cp SIGMA-BEFORE? cp IGNORABLE? cp CASED? 0= or AGREE? 0= if MISMATCH then ;

: CHECK-SCALAR ( n -- ) {: cp:n :}
   cp INPUT ENCODE INPUT - {: size:n :}
   cp size [: UNICODE:FOLD ;] [: FOLD-TRUTH ;] CHECK-MAP
   cp size [: UNICODE:LOWER ;] [: LOWER-TRUTH ;] CHECK-MAP
   cp CHECK-SIGMA ;

: CHECK-ALL-SCALARS ( -- )
   SCALAR-COUNT 0 ?do i SCALAR? if i CHECK-SCALAR then loop ;

\ Marks a range table's bit, checking that each range is a run of scalars
\ that starts past the gap after the one before it.
: MARK-TABLE ( ^n n n -- ) {: table:^ count:n bit:n :}
   -2 count 0 ?do
      {: last:n :}
      table i 2 * cells + @ table i 2 * cells + cell+ @ {: lo:n hi:n :}
      lo last 1+ <= hi lo < or if MISMATCH then
      hi 1+ lo ?do
         i SCALAR? 0= if MISMATCH then
         i PROP-TABLE @ bit or i PROP-TABLE !
      loop
      hi
   loop drop ;

: CHECK-RANGES ( -- )
   UNICODE-CASE-DATA:CASED-RANGES CASED-BIT MARK-TABLE
   UNICODE-CASE-DATA:CASE-IGNORABLE-RANGES IGNORABLE-BIT MARK-TABLE
   SCALAR-COUNT 0 ?do
      i PROP-TABLE @ i PROP-TRUTH @ <> if MISMATCH then
   loop ;

: CHECK-REGENERATION ( -- )
   UNICODE-CASE-TOOL:LOAD-PINNED
   UNICODE-CASE-TOOL:RENDER {: generated:str :}
   DATA-PATH$ READ-INPUT {: buf:str :}
   generated buf STR:= 0= if MISMATCH then ;

public

: VERIFY ( -- )
   CHECK-DIGESTS
   LOAD-TRUTH
   CHECK-RANGES
   CHECK-ALL-SCALARS
   CHECK-REGENERATION ;

;package
