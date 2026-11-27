\ case-generate.f - deterministic Unicode case-mapping source generator.
\ A scalar's full lowercase is UnicodeData.txt field 13, replaced by the lower
\ field of an unconditional SpecialCasing.txt row; its full folding is its
\ CaseFolding.txt C or F row. Both are kept per scalar, packed, and a walk over
\ every scalar lists those that do not map to themselves, in order. The Cased
\ and Case_Ignorable rows of DerivedCoreProperties.txt mark each scalar, and a
\ walk joins the runs of a mark into ranges.

require src/lib/prelude.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/codegen.f
require tools/unicode/class-parse.f
require tools/unicode/class-generate.f

package UNICODE-CASE-TOOL

private

$110000 constant SCALAR-COUNT
3 constant TARGET-MAX
21 constant TARGET-BITS
$1FFFFF constant TARGET-MASK
1 constant CASED-BIT
2 constant IGNORABLE-BIT
4 constant SPECIAL-BIT
10 constant LINE-FEED
59 constant SEMICOLON
32 constant SPACE

\ Each scalar's targets, TARGET-BITS apart and first lowest; zero when the
\ scalar maps to itself.
DYNAMIC-BUFFER LOWERS n
DYNAMIC-BUFFER FOLDS n
\ Each scalar's CASED-BIT, IGNORABLE-BIT and SPECIAL-BIT, the last set by
\ its unconditional SpecialCasing.txt row.
DYNAMIC-BUFFER MARKS n
variable PREVIOUS
CODEGEN:BUFFER OUTPUT

: UNICODE-PATH$ ( -- str )
   s" data/unicode/16.0.0/UnicodeData.txt" ;

: SPECIAL-PATH$ ( -- str )
   s" data/unicode/16.0.0/SpecialCasing.txt" ;

: FOLDING-PATH$ ( -- str )
   s" data/unicode/16.0.0/CaseFolding.txt" ;

: DERIVED-PATH$ ( -- str )
   s" data/unicode/16.0.0/DerivedCoreProperties.txt" ;

: OUTPUT-PATH$ ( -- str )
   s" src/lib/unicode/case-data.f" ;

: EXPECTED-UNICODE-DIGEST$ ( -- str )
   s" ff58e5823bd095166564a006e47d111130813dcf8bf234ef79fa51a870edb48f" ;

: EXPECTED-SPECIAL-DIGEST$ ( -- str )
   s" 8d5de354eef79f2395a54c9c7dcebbaf3d30fc962d0f85611ea97aa973a0c451" ;

: EXPECTED-FOLDING-DIGEST$ ( -- str )
   s" 6f1f9c588eb4a5c718d9e8f93b782685e5c7fec872cf05e8e6878053599e09bb" ;

: EXPECTED-DERIVED-DIGEST$ ( -- str )
   s" 39d35161f2954497f69e08bdb9e701493f476a3d30222de20028feda36c1dabd" ;

: CHECK-INPUT-DIGESTS ( -- )
   UNICODE-PATH$ EXPECTED-UNICODE-DIGEST$
   UNICODE-CLASS-TOOL:CHECK-FILE-DIGEST
   SPECIAL-PATH$ EXPECTED-SPECIAL-DIGEST$
   UNICODE-CLASS-TOOL:CHECK-FILE-DIGEST
   FOLDING-PATH$ EXPECTED-FOLDING-DIGEST$
   UNICODE-CLASS-TOOL:CHECK-FILE-DIGEST
   DERIVED-PATH$ EXPECTED-DERIVED-DIGEST$
   UNICODE-CLASS-TOOL:CHECK-FILE-DIGEST ;

\ The whole file, in a buffer the process keeps.
: READ-INPUT ( str -- str )
   dup FS:FILE-SIZE 1+ MEM-ALLOC-64K-SPAN {: path:str buf:str :}
   path buf STR:DATA buf STR:LEN FS:READ-ALL {: got:n :}
   buf STR:DATA got >str ;

: SYNTAX ( -- )
   UNICODE-CLASS-TOOL:E-SYNTAX throw ;

: BAD-RANGE ( -- )
   UNICODE-CLASS-TOOL:E-RANGE throw ;

: CHECK-SCALAR ( n n -- ) {: lo:n hi:n :}
   lo hi <> if SYNTAX then
   lo lo UNICODE-CLASS-TOOL:SCALAR-RANGE ;

\ The cursor of the field after the one that starts at a cursor.
: NEXT-START ( n str -- n ) {: start:n s:str :}
   s start UNICODE-CLASS-TOOL:NEXT-FIELD {: f:str next:n :} next ;

\ The trimmed field of a value at an index.
: FIELD ( str n -- str ) {: s:str idx:n :}
   0 idx 0 ?do s NEXT-START loop {: start:n :}
   s start UNICODE-CLASS-TOOL:NEXT-FIELD {: f:str next:n :} f STR:TRIM ;

\ One to TARGET-MAX hex scalars, one space apart, packed.
: PARSE-TARGETS ( str -- n ) {: s:str :}
   s STR:LEN 0= if SYNTAX then
   0 0 0 begin dup s STR:LEN <= while
      {: packed:n count:n start:n :}
      count TARGET-MAX >= if SYNTAX then
      s SPACE start STR:SPLIT-NEXT {: f:str next:n valid:bool :}
      f UNICODE-CLASS-TOOL:PARSE-HEX dup dup UNICODE-CLASS-TOOL:SCALAR-RANGE
      count TARGET-BITS * lshift packed or
      count 1+ next
   repeat 2drop ;

\ Stores a scalar's packed targets in a table cell, zero for itself.
: TARGETS! ( n n ^n -- ) {: cp:n packed:n at:^ :}
   packed cp = if 0 else packed then at ! ;

: MARK! ( n n -- ) {: cp:n bit:n :}
   cp MARKS {: at:^ :}
   at @ bit and 0<> if BAD-RANGE then
   at @ bit or at ! ;

\ The 14 fields after a UnicodeData.txt row's code; field 13 is index 12.
\ Rows ascend; the surrogates' range rows have no lowercase.
: UNICODE-ROW ( n n str -- ) {: lo:n hi:n s:str :}
   lo hi <> if SYNTAX then
   lo PREVIOUS @ <= if BAD-RANGE then
   lo PREVIOUS !
   s SEMICOLON STR:COUNT-CHAR 13 <> if SYNTAX then
   s 12 FIELD {: lower:str :}
   lower STR:LEN 0= if exit then
   lo lo UNICODE-CLASS-TOOL:SCALAR-RANGE
   lo lower UNICODE-CLASS-TOOL:PARSE-HEX dup dup UNICODE-CLASS-TOOL:SCALAR-RANGE
   lo LOWERS TARGETS! ;

\ `lower; title; upper;` after the code, then a condition list on a
\ conditional row; only an unconditional row's lower field counts.
: SPECIAL-ROW ( n n str -- ) {: lo:n hi:n s:str :}
   lo hi CHECK-SCALAR
   s SEMICOLON STR:COUNT-CHAR {: fields:n :}
   fields 4 = if s 3 FIELD STR:LEN 0= if SYNTAX then exit then
   fields 3 <> if SYNTAX then
   s 0 FIELD {: lower:str :}
   lower STR:LEN 0= if exit then
   lo SPECIAL-BIT MARK!
   lo lower PARSE-TARGETS lo LOWERS TARGETS! ;

\ `status; mapping;` after the code; statuses C and F are full folding.
: FOLDING-ROW ( n n str -- ) {: lo:n hi:n s:str :}
   lo hi CHECK-SCALAR
   s SEMICOLON STR:COUNT-CHAR 2 <> if SYNTAX then
   s 0 FIELD {: status:str :}
   status STR:LEN 1 <> if SYNTAX then
   status STR:DATA c@ {: c:n :}
   c [char] S = c [char] T = or if exit then
   c [char] C = c [char] F = or 0= if SYNTAX then
   lo FOLDS @ 0<> if BAD-RANGE then
   lo s 1 FIELD PARSE-TARGETS lo FOLDS TARGETS! ;

: MARK-RANGE ( n n n -- ) {: lo:n hi:n bit:n :}
   lo hi UNICODE-CLASS-TOOL:SCALAR-RANGE
   hi 1+ lo ?do i bit MARK! loop ;

: DERIVED-ROW ( n n str -- ) {: lo:n hi:n s:str :}
   s s" Cased" STR:= if lo hi CASED-BIT MARK-RANGE exit then
   s s" Case_Ignorable" STR:= if lo hi IGNORABLE-BIT MARK-RANGE then ;

: CLEAR ( -- )
   SCALAR-COUNT LOWERS-RESERVE
   SCALAR-COUNT FOLDS-RESERVE
   SCALAR-COUNT MARKS-RESERVE
   SCALAR-COUNT 0 ?do 0 i LOWERS ! 0 i FOLDS ! 0 i MARKS ! loop
   -1 PREVIOUS ! ;

: OUT$ ( str -- )
   OUTPUT CODEGEN:APPEND-STRING ;

: OUT-N ( n -- )
   OUTPUT CODEGEN:APPEND-DECIMAL ;

: OUT-LINE ( str -- )
   OUT$ LINE-FEED OUTPUT CODEGEN:APPEND-BYTE ;

\ The head of a table: its count, its buffer and its first cell.
: OUT-HEAD ( str n -- ) {: name:str count:n :}
   count OUT-N s"  constant " OUT$ name OUT$ s" -TABLE-COUNT" OUT-LINE
   count 2 * 1 max OUT-N s"  TYPED-BUFFER " OUT$ name OUT$
   s" -TABLE n" OUT-LINE
   s" 0 " OUT$ name OUT$ s" -TABLE" OUT-LINE ;

: OUT-TAIL ( -- )
   s" drop" OUT-LINE
   s" " OUT-LINE ;

\ A mapping line: the scalar and its three target cells, zero past the last.
: OUT-MAPPING ( n n -- ) {: cp:n packed:n :}
   s"    " OUT$ cp OUT-N
   TARGET-MAX 0 ?do
      s"  " OUT$ packed i TARGET-BITS * rshift TARGET-MASK and OUT-N
   loop
   s"  M," OUT-LINE ;

: MAPPING-COUNT ( [ n -- ^n ] -- n ) {: table :}
   0 SCALAR-COUNT 0 ?do i table execute @ 0<> if 1+ then loop ;

: OUT-MAPPINGS ( str [ n -- ^n ] -- ) {: name:str table :}
   name table MAPPING-COUNT OUT-HEAD
   SCALAR-COUNT 0 ?do
      i table execute @ {: packed:n :}
      packed 0<> if i packed OUT-MAPPING then
   loop
   OUT-TAIL ;

: MARKED? ( n n -- bool ) {: cp:n bit:n :}
   cp MARKS @ bit and 0<> ;

\ The scalar after the run of a mark that starts at cp.
: RUN-END ( n n -- n ) {: cp:n bit:n :}
   cp begin dup SCALAR-COUNT < while
      dup bit MARKED? 0= if exit then
      1+
   repeat ;

\ The number of runs of a mark.
: RUN-COUNT ( n -- n ) {: bit:n :}
   0 0 begin dup SCALAR-COUNT < while
      {: count:n cp:n :}
      cp bit MARKED? if count 1+ cp bit RUN-END else count cp 1+ then
   repeat drop ;

: OUT-RANGE ( n n -- ) {: lo:n hi:n :}
   s"    " OUT$ lo OUT-N s"  " OUT$ hi OUT-N s"  R," OUT-LINE ;

: OUT-RANGES ( str n -- ) {: name:str bit:n :}
   name bit RUN-COUNT OUT-HEAD
   0 begin dup SCALAR-COUNT < while
      {: cp:n :}
      cp bit MARKED? if
         cp bit RUN-END {: end:n :}
         cp end 1- OUT-RANGE end
      else cp 1+ then
   repeat drop
   OUT-TAIL ;

: OUT-PREAMBLE ( -- )
   s" \ case-data.f - generated Unicode 16.0.0 full case mappings and case properties." OUT-LINE
   s" \ Generated by tools/unicode/case-generate.f; do not edit." OUT-LINE
   s" \ Each mapping table holds a scalar and a target cell per entry, sorted by scalar;" OUT-LINE
   s" \ the target cell packs one to three scalars 21 bits apart, first lowest, then zero." OUT-LINE
   s" \ Each range table holds sorted, disjoint, inclusive first and last cell pairs." OUT-LINE
   s" " OUT-LINE
   s" require src/lib/prelude.f" OUT-LINE
   s" " OUT-LINE
   s" package UNICODE-CASE-DATA" OUT-LINE
   s" " OUT-LINE
   s" private" OUT-LINE
   s" " OUT-LINE
   s" : M, ( ^n n n n n -- ^n )" OUT-LINE
   s"    {: at:^ cp:n first:n second:n third:n :}" OUT-LINE
   s"    third 21 lshift second or 21 lshift first or" OUT-LINE
   s"    cp at ! at cell+ !" OUT-LINE
   s"    at 2 cells + ;" OUT-LINE
   s" " OUT-LINE
   s" : R, ( ^n n n -- ^n )" OUT-LINE
   s"    {: at:^ lo:n hi:n :}" OUT-LINE
   s"    lo at ! hi at cell+ !" OUT-LINE
   s"    at 2 cells + ;" OUT-LINE
   s" " OUT-LINE ;

\ An accessor answers its table's first cell and entry count together.
: OUT-ACCESSOR ( str str -- ) {: name:str suffix:str :}
   s" : " OUT$ name OUT$ suffix OUT$ s"  ( -- ^n n )" OUT-LINE
   s"    0 " OUT$ name OUT$ s" -TABLE " OUT$
   name OUT$ s" -TABLE-COUNT ;" OUT-LINE
   s" " OUT-LINE ;

: OUT-ACCESSORS ( -- )
   s" public" OUT-LINE
   s" " OUT-LINE
   s" LOWER" s" -MAPPINGS" OUT-ACCESSOR
   s" FOLD" s" -MAPPINGS" OUT-ACCESSOR
   s" CASED" s" -RANGES" OUT-ACCESSOR
   s" CASE-IGNORABLE" s" -RANGES" OUT-ACCESSOR
   s" ;package" OUT-LINE ;

: LOAD-PINNED ( -- )
   CHECK-INPUT-DIGESTS
   CLEAR
   UNICODE-PATH$ READ-INPUT
   [: UNICODE-ROW ;] UNICODE-CLASS-TOOL:RANGE-ROWS
   SPECIAL-PATH$ READ-INPUT
   [: SPECIAL-ROW ;] UNICODE-CLASS-TOOL:RANGE-ROWS
   FOLDING-PATH$ READ-INPUT
   [: FOLDING-ROW ;] UNICODE-CLASS-TOOL:RANGE-ROWS
   DERIVED-PATH$ READ-INPUT
   [: DERIVED-ROW ;] UNICODE-CLASS-TOOL:RANGE-ROWS ;

public

: RENDER ( -- str )
   LOAD-PINNED
   OUTPUT CODEGEN:RESET
   OUT-PREAMBLE
   s" LOWER" [: LOWERS ;] OUT-MAPPINGS
   s" FOLD" [: FOLDS ;] OUT-MAPPINGS
   s" CASED" CASED-BIT OUT-RANGES
   s" CASE-IGNORABLE" IGNORABLE-BIT OUT-RANGES
   OUT-ACCESSORS
   OUTPUT CODEGEN:CONTENTS ;

: WRITE-PINNED ( -- )
   OUTPUT-PATH$ RENDER FS:ATOMIC-WRITE-FILE ;

;package
