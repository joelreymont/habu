\ script-generate.f - deterministic Unicode Script source generator.
\ The variants are the Script values of PropertyValueAliases.txt in its order,
\ so a value's tag is its index there. Each Scripts.txt row marks its scalars;
\ a walk over every scalar then joins runs of one script into sorted ranges.

require src/lib/prelude.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/codegen.f
require tools/unicode/class-parse.f
require tools/unicode/class-generate.f

package UNICODE-SCRIPT-TOOL

private

UNICODE-CLASS-TOOL:E-SYNTAX constant E-SYNTAX
UNICODE-CLASS-TOOL:E-RANGE constant E-RANGE
UNICODE-CLASS-TOOL:E-CAPACITY constant E-CAPACITY
UNICODE-CLASS-TOOL:E-STATE constant E-STATE

$10FFFF constant SCALAR-MAX
$110000 constant SCALAR-COUNT
256 constant NAME-CAPACITY
2048 constant RANGE-CAPACITY
10 constant LINE-FEED
59 constant SEMICOLON
95 constant UNDERSCORE
45 constant HYPHEN

NAME-CAPACITY TYPED-BUFFER NAME-A ptr u8
NAME-CAPACITY TYPED-BUFFER NAME-U n
variable NAME-N
\ Each scalar's script index plus one; zero for no Scripts.txt row.
DYNAMIC-BUFFER MARK n
RANGE-CAPACITY TYPED-BUFFER RANGE-LO n
RANGE-CAPACITY TYPED-BUFFER RANGE-HI n
RANGE-CAPACITY TYPED-BUFFER RANGE-TAG n
variable RANGE-N
variable READY

131072 CODEGEN:BUFFER OUTPUT

: SCRIPTS-PATH$ ( -- ptr u8 n )
   s" data/unicode/16.0.0/Scripts.txt" ;

: ALIASES-PATH$ ( -- ptr u8 n )
   s" data/unicode/16.0.0/PropertyValueAliases.txt" ;

: OUTPUT-PATH$ ( -- ptr u8 n )
   s" src/lib/unicode/script-data.f" ;

: EXPECTED-SCRIPTS-DIGEST$ ( -- ptr u8 n )
   s" 9e88f0a677df47311106340be8ede2ecdacd9c1c931831218d2be6d5508e0039" ;

: EXPECTED-ALIASES-DIGEST$ ( -- ptr u8 n )
   s" 440fd3e5460b9bfe31da67b6f923992e1989d31fe2ed91e091c4b8f8e2620bf9" ;

: CHECK-INPUT-DIGESTS ( -- )
   SCRIPTS-PATH$ EXPECTED-SCRIPTS-DIGEST$
   UNICODE-CLASS-TOOL:CHECK-FILE-DIGEST
   ALIASES-PATH$ EXPECTED-ALIASES-DIGEST$
   UNICODE-CLASS-TOOL:CHECK-FILE-DIGEST ;

\ The whole file, in a buffer the process keeps: names point into it.
: READ-INPUT ( ptr u8 n -- ptr u8 n )
   2dup FS:FILE-SIZE 1+ MEM-ALLOC-64K-SPAN {: path:ptr pathu:n buf:ptr cap:n :}
   path pathu buf cap FS:READ-ALL {: got:n :}
   buf got ;

: NAME+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if E-SYNTAX throw then
   NAME-N @ NAME-CAPACITY >= if E-CAPACITY throw then
   a NAME-N @ NAME-A !
   u NAME-N @ NAME-U !
   NAME-N @ 1+ NAME-N ! ;

: NEXT-FIELD ( ptr u8 n n -- ptr u8 n n bool ) {: a:ptr u:n start:n :}
   a u SEMICOLON start SPLIT-NEXT {: fa:ptr fu:n next:n valid:bool :}
   fa fu TRIM next valid ;

\ An `sc ; Code ; Long_Name [; Alias]` row adds its long name.
: ALIAS-ROW ( ptr u8 n -- ) {: line:ptr lineu:n :}
   line lineu 0 NEXT-FIELD {: pa:ptr pu:n next1:n ok1:bool :}
   pa pu s" sc" STR= 0= if exit then
   line lineu next1 NEXT-FIELD {: ca:ptr cu:n next2:n ok2:bool :}
   line lineu next2 NEXT-FIELD {: la:ptr lu:n next3:n ok3:bool :}
   ok1 ok2 and ok3 and 0= if E-SYNTAX throw then
   cu 4 <> if E-SYNTAX throw then
   next3 drop la lu NAME+ ;

: SCAN-ALIASES ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u <= while
      {: start:n :}
      a u LINE-FEED start SPLIT-NEXT {: line:ptr lineu:n next:n valid:bool :}
      valid 0= if E-SYNTAX throw then
      line lineu ALIAS-ROW
      next
   repeat drop ;

: NAME-INDEX ( ptr u8 n -- n ) {: a:ptr u:n :}
   NAME-N @ 0 ?do
      a u i NAME-A @ i NAME-U @ STR= if i unloop exit then
   loop
   E-SYNTAX throw ;

: MARK-SCALAR ( n n -- ) {: cp:n tag:n :}
   cp MARK {: at:ptr :}
   at @ 0<> if E-RANGE throw then
   tag 1+ at ! ;

: SCRIPT-ROW ( n n ptr u8 n -- ) {: lo:n hi:n a:ptr u:n :}
   lo 0 < hi lo < or hi SCALAR-MAX > or if E-RANGE throw then
   a u NAME-INDEX {: tag:n :}
   hi 1+ lo ?do i tag MARK-SCALAR loop ;

: CLEAR-MARKS ( -- )
   SCALAR-COUNT 0 ?do 0 i MARK ! loop ;

: RANGE+ ( n n n -- ) {: lo:n hi:n tag:n :}
   RANGE-N @ RANGE-CAPACITY >= if E-CAPACITY throw then
   lo RANGE-N @ RANGE-LO !
   hi RANGE-N @ RANGE-HI !
   tag RANGE-N @ RANGE-TAG !
   RANGE-N @ 1+ RANGE-N ! ;

\ The scalar after the run of mark m that starts at cp.
: RUN-END ( n n -- n ) {: cp:n m:n :}
   cp begin dup SCALAR-COUNT < while
      dup MARK @ m <> if exit then
      1+
   repeat ;

: COLLECT-RANGES ( -- )
   0 RANGE-N !
   0 begin dup SCALAR-COUNT < while
      {: cp:n :}
      cp MARK @ {: m:n :}
      cp m RUN-END {: end:n :}
      m 0<> if cp end 1- m 1- RANGE+ then
      end
   repeat drop ;

: OUT$ ( ptr u8 n -- )
   OUTPUT CODEGEN:APPEND-STRING ;

: OUT-N ( n -- )
   OUTPUT CODEGEN:APPEND-DECIMAL ;

: OUT-BYTE ( n -- )
   OUTPUT CODEGEN:APPEND-BYTE ;

: OUT-LINE ( ptr u8 n -- )
   OUT$ LINE-FEED OUT-BYTE ;

\ Lower case, with `_` as `-`.
: VARIANT-BYTE ( n -- n ) {: c:n :}
   c UNDERSCORE = if HYPHEN exit then
   c 65 >= c 90 <= and if c 32 + exit then
   c ;

: OUT-VARIANT ( n -- ) {: idx:n :}
   s"    " OUT$
   idx NAME-A @ idx NAME-U @ {: a:ptr u:n :}
   u 0 ?do a i + c@ VARIANT-BYTE OUT-BYTE loop
   s" " OUT-LINE ;

: OUT-ENUM ( -- )
   s" public" OUT-LINE
   s" " OUT-LINE
   s" ENUM script DERIVE eq hash" OUT-LINE
   NAME-N @ 0 ?do i OUT-VARIANT loop
   s" ;ENUM" OUT-LINE
   s" " OUT-LINE ;

: OUT-PREAMBLE ( -- )
   s" \ script-data.f - generated Unicode 16.0.0 Script values and ranges." OUT-LINE
   s" \ Generated by tools/unicode/script-generate.f; do not edit." OUT-LINE
   s" \ The table holds sorted, disjoint, inclusive first and last cells with the" OUT-LINE
   s" \ tag of their script; touching ranges of one script are joined." OUT-LINE
   s" " OUT-LINE
   s" require src/lib/prelude.f" OUT-LINE
   s" " OUT-LINE
   s" package UNICODE-SCRIPT" OUT-LINE
   s" " OUT-LINE ;

: OUT-WRITER ( -- )
   s" private" OUT-LINE
   s" " OUT-LINE
   s" : RANGE, ( ptr n n n n -- ptr n )" OUT-LINE
   s"    {: at:ptr lo:n hi:n tag:n :}" OUT-LINE
   s"    lo at ! hi at cell+ ! tag at 2 cells + !" OUT-LINE
   s"    at 3 cells + ;" OUT-LINE
   s" " OUT-LINE ;

: OUT-RANGE ( n -- ) {: idx:n :}
   s"    " OUT$ idx RANGE-LO @ OUT-N
   s"  " OUT$ idx RANGE-HI @ OUT-N
   s"  " OUT$ idx RANGE-TAG @ OUT-N
   s"  RANGE," OUT-LINE ;

: OUT-TABLE ( -- )
   RANGE-N @ OUT-N s"  constant RANGE-COUNT" OUT-LINE
   RANGE-N @ 3 * OUT-N s"  TYPED-BUFFER RANGE-TABLE n" OUT-LINE
   s" 0 RANGE-TABLE" OUT-LINE
   RANGE-N @ 0 ?do i OUT-RANGE loop
   s" drop" OUT-LINE
   s" " OUT-LINE
   s" ;package" OUT-LINE ;

public

: LOAD-PINNED ( -- )
   CHECK-INPUT-DIGESTS
   false READY !
   0 NAME-N !
   ALIASES-PATH$ READ-INPUT SCAN-ALIASES
   SCALAR-COUNT MARK-RESERVE
   CLEAR-MARKS
   SCRIPTS-PATH$ READ-INPUT
   [: SCRIPT-ROW ;] UNICODE-CLASS-TOOL:RANGE-ROWS
   COLLECT-RANGES
   true READY ! ;

: RENDER ( -- ptr u8 n )
   READY @ 0= if E-STATE throw then
   OUTPUT CODEGEN:RESET
   OUT-PREAMBLE
   OUT-ENUM
   OUT-WRITER
   OUT-TABLE
   OUTPUT CODEGEN:CONTENTS ;

: WRITE-PINNED ( -- )
   LOAD-PINNED
   OUTPUT-PATH$ RENDER FS:ATOMIC-WRITE-FILE ;

;package
