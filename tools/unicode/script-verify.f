\ script-verify.f - independent exhaustive verifier for the Unicode Script table.
\ It reads both inputs with a parser of its own, checks each variant's name and
\ tag against PropertyValueAliases.txt, every scalar's answer against
\ Scripts.txt, and the table against a fresh byte-identical regeneration.

require src/lib/prelude.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/codegen.f
require src/lib/unicode/script.f
require tools/unicode/script-generate.f

package UNICODE-SCRIPT-VERIFY

private

-7808 constant E-SCRIPT-MISMATCH

$10FFFF constant SCALAR-MAX
$110000 constant SCALAR-COUNT
256 constant NAME-CAPACITY
10 constant LINE-FEED
59 constant SEMICOLON
35 constant HASH
46 constant DOT
95 constant UNDERSCORE
45 constant HYPHEN

NAME-CAPACITY TYPED-BUFFER NAME-A ptr u8
NAME-CAPACITY TYPED-BUFFER NAME-U n
variable NAME-N
\ Each scalar's script index plus one; zero for no Scripts.txt row.
DYNAMIC-BUFFER TRUTH n
variable UNKNOWN-TAG
variable EVAL-TAG
512 CODEGEN:BUFFER TEXT

: SCRIPTS-PATH$ ( -- ptr u8 n )
   s" data/unicode/16.0.0/Scripts.txt" ;

: ALIASES-PATH$ ( -- ptr u8 n )
   s" data/unicode/16.0.0/PropertyValueAliases.txt" ;

: DATA-PATH$ ( -- ptr u8 n )
   s" src/lib/unicode/script-data.f" ;

: EXPECTED-SCRIPTS-DIGEST$ ( -- ptr u8 n )
   s" 9e88f0a677df47311106340be8ede2ecdacd9c1c931831218d2be6d5508e0039" ;

: EXPECTED-ALIASES-DIGEST$ ( -- ptr u8 n )
   s" 440fd3e5460b9bfe31da67b6f923992e1989d31fe2ed91e091c4b8f8e2620bf9" ;

: CHECK-DIGESTS ( -- )
   SCRIPTS-PATH$ EXPECTED-SCRIPTS-DIGEST$
   UNICODE-CLASS-TOOL:CHECK-FILE-DIGEST
   ALIASES-PATH$ EXPECTED-ALIASES-DIGEST$
   UNICODE-CLASS-TOOL:CHECK-FILE-DIGEST ;

\ The whole file, in a buffer the process keeps: names point into it.
: READ-INPUT ( ptr u8 n -- ptr u8 n )
   2dup FS:FILE-SIZE 1+ MEM-ALLOC-64K-SPAN {: path:ptr pathu:n buf:ptr cap:n :}
   path pathu buf cap FS:READ-ALL {: got:n :}
   buf got ;

: HEX-DIGIT ( n -- n bool ) {: c:n :}
   c 48 >= c 57 <= and if c 48 - true exit then
   c 65 >= c 70 <= and if c 55 - true exit then
   c 97 >= c 102 <= and if c 87 - true exit then
   0 false ;

: PARSE-HEX ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0= u 6 > or if E-SCRIPT-MISMATCH throw then
   0 u 0 ?do
      16 * a i + c@ HEX-DIGIT 0= if E-SCRIPT-MISMATCH throw then +
   loop ;

: BYTE-INDEX ( ptr u8 n n -- n ) {: a:ptr u:n c:n :}
   0 begin dup u < while
      dup a + c@ c = if exit then
      1+
   repeat ;

\ Each line of a file, in order.
: EACH-LINE ( ptr u8 n [ ptr u8 n -- ] -- ) {: a:ptr u:n q :}
   0 begin dup u <= while
      {: start:n :}
      a u LINE-FEED start SPLIT-NEXT {: line:ptr lineu:n next:n valid:bool :}
      valid 0= if E-SCRIPT-MISMATCH throw then
      line lineu q execute
      next
   repeat drop ;

\ The field of a `;`-separated line that starts at start, trimmed.
: FIELD-AT ( ptr u8 n n -- ptr u8 n n ) {: a:ptr u:n start:n :}
   a u SEMICOLON start SPLIT-NEXT {: fa:ptr fu:n next:n valid:bool :}
   valid 0= if E-SCRIPT-MISMATCH throw then
   fa fu TRIM next ;

: NAME+ ( ptr u8 n -- ) {: a:ptr u:n :}
   NAME-N @ NAME-CAPACITY >= if E-SCRIPT-MISMATCH throw then
   a NAME-N @ NAME-A !
   u NAME-N @ NAME-U !
   NAME-N @ 1+ NAME-N ! ;

: ALIAS-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u 0 FIELD-AT {: pa:ptr pu:n next:n :}
   pa pu s" sc" STR= 0= if exit then
   a u next FIELD-AT {: code:ptr codeu:n next2:n :}
   codeu 4 <> if E-SCRIPT-MISMATCH throw then
   a u next2 FIELD-AT drop NAME+ ;

: NAME-INDEX ( ptr u8 n -- n ) {: a:ptr u:n :}
   NAME-N @ 0 ?do
      a u i NAME-A @ i NAME-U @ STR= if i unloop exit then
   loop
   E-SCRIPT-MISMATCH throw ;

: LOAD-NAMES ( -- )
   0 NAME-N !
   ALIASES-PATH$ READ-INPUT [: ALIAS-LINE ;] EACH-LINE
   s" Unknown" NAME-INDEX UNKNOWN-TAG ! ;

: TRUTH+ ( n n -- ) {: cp:n tag:n :}
   cp 0 < cp SCALAR-MAX > or if E-SCRIPT-MISMATCH throw then
   cp TRUTH {: at:ptr :}
   at @ 0<> if E-SCRIPT-MISMATCH throw then
   tag 1+ at ! ;

\ `XXXX` or `XXXX..YYYY`.
: RANGE-BOUNDS ( ptr u8 n -- n n ) {: a:ptr u:n :}
   a u DOT BYTE-INDEX {: pos:n :}
   pos u = if a u PARSE-HEX dup exit then
   pos 2 + u >= if E-SCRIPT-MISMATCH throw then
   a pos + 1+ c@ DOT <> if E-SCRIPT-MISMATCH throw then
   a pos PARSE-HEX a pos 2 + + u pos 2 + - PARSE-HEX ;

: SCRIPTS-LINE ( ptr u8 n -- ) {: line:ptr lineu:n :}
   line line lineu HASH BYTE-INDEX TRIM {: a:ptr u:n :}
   u 0= if exit then
   a u 0 FIELD-AT {: ra:ptr ru:n next:n :}
   a u next FIELD-AT drop NAME-INDEX {: tag:n :}
   ra ru RANGE-BOUNDS {: lo:n hi:n :}
   hi lo < if E-SCRIPT-MISMATCH throw then
   hi 1+ lo ?do i tag TRUTH+ loop ;

: LOAD-TRUTH ( -- )
   SCALAR-COUNT TRUTH-RESERVE
   SCALAR-COUNT 0 ?do 0 i TRUTH ! loop
   SCRIPTS-PATH$ READ-INPUT [: SCRIPTS-LINE ;] EACH-LINE ;

\ Lower case, with `_` as `-`.
: VARIANT-BYTE ( n -- n ) {: c:n :}
   c UNDERSCORE = if HYPHEN exit then
   c 65 >= c 90 <= and if c 32 + exit then
   c ;

: TEXT$ ( ptr u8 n -- )
   TEXT CODEGEN:APPEND-STRING ;

\ Evaluates the variant named for script idx, storing its tag through TAG!.
: VARIANT-TAG ( n -- n ) {: idx:n :}
   TEXT CODEGEN:RESET
   s" UNICODE--SCRIPT-SCRIPT:" TEXT$
   idx NAME-A @ idx NAME-U @ {: a:ptr u:n :}
   u 0 ?do a i + c@ VARIANT-BYTE TEXT CODEGEN:APPEND-BYTE loop
   s"  UNICODE--SCRIPT-SCRIPT:TAG UNICODE-SCRIPT-VERIFY:TAG!" TEXT$
   -1 EVAL-TAG !
   TEXT CODEGEN:CONTENTS evaluate-closed
   EVAL-TAG @ ;

: CHECK-VARIANTS ( -- )
   NAME-N @ 0 ?do
      i VARIANT-TAG i <> if E-SCRIPT-MISMATCH throw then
   loop ;

: ANSWER ( n -- n )
   UNICODE-SCRIPT:SCRIPT UNICODE--SCRIPT-SCRIPT:TAG ;

: EXPECTED ( n -- n ) {: cp:n :}
   cp TRUTH @ {: m:n :}
   m 0= if UNKNOWN-TAG @ else m 1- then ;

: CHECK-ALL-SCALARS ( -- )
   SCALAR-COUNT 0 ?do
      i ANSWER i EXPECTED <> if E-SCRIPT-MISMATCH throw then
   loop ;

: CHECK-NON-SCALAR ( n -- )
   ANSWER UNKNOWN-TAG @ <> if E-SCRIPT-MISMATCH throw then ;

: CHECK-REGENERATION ( -- )
   UNICODE-SCRIPT-TOOL:LOAD-PINNED
   UNICODE-SCRIPT-TOOL:RENDER {: generated:ptr generatedu:n :}
   DATA-PATH$ READ-INPUT {: buf:ptr got:n :}
   generated generatedu buf got STR= 0= if E-SCRIPT-MISMATCH throw then ;

public

\ The hook of VARIANT-TAG's text, which runs at top level.
: TAG! ( n -- )
   EVAL-TAG ! ;

: VERIFY ( -- )
   CHECK-DIGESTS
   LOAD-NAMES
   LOAD-TRUTH
   CHECK-VARIANTS
   CHECK-ALL-SCALARS
   -1 CHECK-NON-SCALAR
   SCALAR-MAX 1+ CHECK-NON-SCALAR
   CHECK-REGENERATION ;

;package
