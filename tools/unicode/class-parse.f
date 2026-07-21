\ class-parse.f - checked parser for pinned Unicode character-class inputs.

require lib/prelude.f
require lib/string.f

package UNICODE-CLASS-TOOL

public

-7800 constant E-SYNTAX
-7801 constant E-RANGE
-7802 constant E-CAPACITY
-7803 constant E-STATE
-7805 constant E-DIGEST

private

$10FFFF constant SCALAR-MAX
$D800 constant SURROGATE-FIRST
$DFFF constant SURROGATE-LAST
1024 constant RANGE-CAPACITY
10 constant LINE-FEED
59 constant SEMICOLON
35 constant HASH
46 constant DOT
1 constant CLASS-LETTER
2 constant CLASS-NUMBER

create STAGE-LETTER-LO RANGE-CAPACITY cells allot
create STAGE-LETTER-HI RANGE-CAPACITY cells allot
create STAGE-NUMBER-LO RANGE-CAPACITY cells allot
create STAGE-NUMBER-HI RANGE-CAPACITY cells allot
create STAGE-SPACE-LO RANGE-CAPACITY cells allot
create STAGE-SPACE-HI RANGE-CAPACITY cells allot
variable STAGE-LETTER-N
variable STAGE-NUMBER-N
variable STAGE-SPACE-N

create LETTER-LO RANGE-CAPACITY cells allot
create LETTER-HI RANGE-CAPACITY cells allot
create NUMBER-LO RANGE-CAPACITY cells allot
create NUMBER-HI RANGE-CAPACITY cells allot
create SPACE-LO RANGE-CAPACITY cells allot
create SPACE-HI RANGE-CAPACITY cells allot
variable LETTER-N
variable NUMBER-N
variable SPACE-N
variable UNICODE-READY
variable SPACE-READY

variable SOURCE-PREV
variable PENDING
variable PENDING-LO
variable PENDING-CLASS
variable PENDING-NAME-A
variable PENDING-NAME-U

: FALSE-VALUE ( -- bool )   0 0= 0= ;
: TRUE-VALUE ( -- bool )    0 0= ;

: SLOT ( n ptr a -- ptr a ) {: idx:n base:ptr :}
   idx 0 < if E-RANGE throw then
   idx RANGE-CAPACITY >= if E-RANGE throw then
   base idx cells + ;

: HEX-DIGIT ( n -- n bool ) {: c:n :}
   c 48 >= c 57 <= and if c 48 - TRUE-VALUE exit then
   c 65 >= c 70 <= and if c 55 - TRUE-VALUE exit then
   c 97 >= c 102 <= and if c 87 - TRUE-VALUE exit then
   0 FALSE-VALUE ;

: HEX-FOLD ( ptr u8 n n n -- n ) {: a:ptr u:n pos:n acc:n :}
   pos u >= if acc exit then
   a pos + c@ HEX-DIGIT 0= if drop E-SYNTAX throw then {: digit:n :}
   a u pos 1+ acc 16 * digit + recurse ;

: PARSE-HEX ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0= u 6 > or if E-SYNTAX throw then
   a u 0 0 HEX-FOLD ;

: SCALAR-RANGE ( n n -- ) {: lo:n hi:n :}
   lo 0 < hi lo < or hi SCALAR-MAX > or if E-RANGE throw then
   lo SURROGATE-LAST <= hi SURROGATE-FIRST >= and if E-RANGE throw then ;

: BYTE-INDEX ( ptr u8 n n -- n ) {: a:ptr u:n c:n :}
   0 begin dup u < while
      dup a + c@ c = if exit then
      1+
   repeat ;

: NEXT-FIELD ( ptr u8 n n -- ptr u8 n n ) {: a:ptr u:n start:n :}
   a u SEMICOLON start SPLIT-NEXT {: fa:ptr fu:n next:n valid:bool :}
   valid 0= next u > or if E-SYNTAX throw then
   fa fu next ;

: CATEGORY-SECOND? ( n ptr u8 n -- bool ) {: c:n allowed:ptr allowedu:n :}
   allowed allowedu c BYTE-INDEX allowedu < ;

: CATEGORY ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 2 <> if E-SYNTAX throw then
   a c@ 76 = if
      a 1+ c@ s" ultmo" CATEGORY-SECOND? 0= if E-SYNTAX throw then
      CLASS-LETTER exit
   then
   a c@ 78 = if
      a 1+ c@ s" dlo" CATEGORY-SECOND? 0= if E-SYNTAX throw then
      CLASS-NUMBER exit
   then
   a c@ 77 = if a 1+ c@ s" nce" CATEGORY-SECOND? 0= if E-SYNTAX throw then 0 exit then
   a c@ 80 = if a 1+ c@ s" cdseifo" CATEGORY-SECOND? 0= if E-SYNTAX throw then 0 exit then
   a c@ 83 = if a 1+ c@ s" mcko" CATEGORY-SECOND? 0= if E-SYNTAX throw then 0 exit then
   a c@ 90 = if a 1+ c@ s" slp" CATEGORY-SECOND? 0= if E-SYNTAX throw then 0 exit then
   a c@ 67 = if a 1+ c@ s" cfson" CATEGORY-SECOND? 0= if E-SYNTAX throw then 0 exit then
   E-SYNTAX throw ;

: FIRST-NAME? ( ptr u8 n -- bool )
   s" , First>" ENDS-WITH? ;

: LAST-NAME? ( ptr u8 n -- bool )
   s" , Last>" ENDS-WITH? ;

: SAME-RANGE-NAME? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 7 < PENDING-NAME-U @ 8 < or if FALSE-VALUE exit then
   a u 7 - PENDING-NAME-A @ PENDING-NAME-U @ 8 - STR= ;

: STAGE-APPEND ( n n ptr a ptr a ptr n -- )
   {: lo:n hi:n los:ptr his:ptr countp:ptr :}
   lo hi SCALAR-RANGE
   countp @ 0 > if
      countp @ 1- his SLOT @ {: prev-hi:n :}
      lo prev-hi <= if E-RANGE throw then
      prev-hi SCALAR-MAX < lo prev-hi 1+ = and if
         hi countp @ 1- his SLOT !
         exit
      then
   then
   countp @ RANGE-CAPACITY >= if E-CAPACITY throw then
   lo countp @ los SLOT !
   hi countp @ his SLOT !
   countp @ 1+ countp ! ;

: STAGE-CLASS+ ( n n n -- ) {: lo:n hi:n class:n :}
   class CLASS-LETTER = if
      lo hi STAGE-LETTER-LO STAGE-LETTER-HI STAGE-LETTER-N STAGE-APPEND
      exit
   then
   class CLASS-NUMBER = if
      lo hi STAGE-NUMBER-LO STAGE-NUMBER-HI STAGE-NUMBER-N STAGE-APPEND
   then ;

: SOURCE-ORDER ( n -- ) {: cp:n :}
   cp 0 < cp SCALAR-MAX > or if E-RANGE throw then
   cp SOURCE-PREV @ <= if E-RANGE throw then ;

: OPEN-RANGE ( n ptr u8 n n -- ) {: cp:n name:ptr nameu:n class:n :}
   cp SOURCE-ORDER
   cp SOURCE-PREV !
   TRUE-VALUE PENDING !
   cp PENDING-LO !
   class PENDING-CLASS !
   name PENDING-NAME-A !
   nameu PENDING-NAME-U ! ;

: CLOSE-RANGE ( n ptr u8 n n -- ) {: cp:n name:ptr nameu:n class:n :}
   PENDING @ 0= if E-SYNTAX throw then
   cp SOURCE-ORDER
   name nameu SAME-RANGE-NAME? 0= if E-SYNTAX throw then
   class PENDING-CLASS @ <> if E-SYNTAX throw then
   PENDING-LO @ cp class STAGE-CLASS+
   cp SOURCE-PREV !
   FALSE-VALUE PENDING ! ;

: UNICODE-ROW ( ptr u8 n -- ) {: line:ptr lineu:n :}
   line lineu TRIM {: row:ptr rowu:n :}
   rowu 0= if exit then
   row rowu SEMICOLON COUNT-CHAR 14 <> if E-SYNTAX throw then
   row rowu 0 NEXT-FIELD {: cpa:ptr cpu:n next1:n :}
   row rowu next1 NEXT-FIELD {: name:ptr nameu:n next2:n :}
   row rowu next2 NEXT-FIELD {: cat:ptr catu:n next3:n :}
   next3 drop
   cpa cpu TRIM PARSE-HEX {: cp:n :}
   cat catu TRIM CATEGORY {: class:n :}
   PENDING @ if
      name nameu LAST-NAME? 0= if E-SYNTAX throw then
      cp name nameu class CLOSE-RANGE
      exit
   then
   name nameu LAST-NAME? if E-SYNTAX throw then
   name nameu FIRST-NAME? if
      cp name nameu class OPEN-RANGE
      exit
   then
   cp SOURCE-ORDER
   cp SOURCE-PREV !
   cp cp class STAGE-CLASS+ ;

: STAGE-UNICODE-RESET ( -- )
   0 STAGE-LETTER-N !
   0 STAGE-NUMBER-N !
   -1 SOURCE-PREV !
   FALSE-VALUE PENDING ! ;

: SCAN-UNICODE ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u <= while
      {: start:n :}
      a u LINE-FEED start SPLIT-NEXT {: line:ptr lineu:n next:n valid:bool :}
      valid 0= if E-SYNTAX throw then
      line lineu UNICODE-ROW
      next
   repeat drop
   PENDING @ if E-SYNTAX throw then ;

: COPY-RANGES ( ptr a ptr a n ptr a ptr a -- )
   {: src-lo:ptr src-hi:ptr count:n dst-lo:ptr dst-hi:ptr :}
   0 begin dup count < while
      dup src-lo SLOT @ over dst-lo SLOT !
      dup src-hi SLOT @ over dst-hi SLOT !
      1+
   repeat drop ;

: COMMIT-UNICODE ( -- )
   STAGE-LETTER-LO STAGE-LETTER-HI STAGE-LETTER-N @ LETTER-LO LETTER-HI COPY-RANGES
   STAGE-NUMBER-LO STAGE-NUMBER-HI STAGE-NUMBER-N @ NUMBER-LO NUMBER-HI COPY-RANGES
   STAGE-LETTER-N @ LETTER-N !
   STAGE-NUMBER-N @ NUMBER-N !
   TRUE-VALUE UNICODE-READY ! ;

: RANGE-PART ( ptr u8 n -- n n ) {: a:ptr u:n :}
   a u DOT BYTE-INDEX {: pos:n :}
   pos u = if a u TRIM PARSE-HEX dup exit then
   pos 0= pos 1+ u >= or if E-SYNTAX throw then
   a pos + 1+ c@ DOT <> if E-SYNTAX throw then
   a pos TRIM PARSE-HEX
   a pos 2 + + u pos 2 + - TRIM PARSE-HEX ;

: SPACE-PROPERTY? ( ptr u8 n -- bool )
   TRIM s" White_Space" STR= ;

: PROP-ROW ( ptr u8 n -- ) {: line:ptr lineu:n :}
   line lineu HASH BYTE-INDEX {: comment:n :}
   line comment TRIM {: row:ptr rowu:n :}
   rowu 0= if exit then
   row rowu 0 NEXT-FIELD {: range:ptr rangeu:n next:n :}
   row next + rowu next - TRIM {: prop:ptr propu:n :}
   range rangeu TRIM RANGE-PART {: lo:n hi:n :}
   prop propu SPACE-PROPERTY? 0= if lo hi SCALAR-RANGE exit then
   lo hi STAGE-SPACE-LO STAGE-SPACE-HI STAGE-SPACE-N STAGE-APPEND ;

: STAGE-SPACE-RESET ( -- )
   0 STAGE-SPACE-N ! ;

: SCAN-PROPS ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u <= while
      {: start:n :}
      a u LINE-FEED start SPLIT-NEXT {: line:ptr lineu:n next:n valid:bool :}
      valid 0= if E-SYNTAX throw then
      line lineu PROP-ROW
      next
   repeat drop ;

: COMMIT-SPACE ( -- )
   STAGE-SPACE-LO STAGE-SPACE-HI STAGE-SPACE-N @ SPACE-LO SPACE-HI COPY-RANGES
   STAGE-SPACE-N @ SPACE-N !
   TRUE-VALUE SPACE-READY ! ;

: VALID-INDEX ( n n -- n ) {: idx:n count:n :}
   idx 0 < idx count >= or if E-RANGE throw then
   idx ;

: RANGE@ ( n ptr a ptr a n -- n n ) {: idx:n los:ptr his:ptr count:n :}
   idx count VALID-INDEX {: safe:n :}
   safe los SLOT @ safe his SLOT @ ;

public

: RESET ( -- )
   FALSE-VALUE UNICODE-READY !
   FALSE-VALUE SPACE-READY !
   0 LETTER-N !
   0 NUMBER-N !
   0 SPACE-N ! ;

: PARSE-UNICODE ( ptr u8 n -- )
   STAGE-UNICODE-RESET
   SCAN-UNICODE
   COMMIT-UNICODE ;

: PARSE-PROPERTIES ( ptr u8 n -- )
   STAGE-SPACE-RESET
   SCAN-PROPS
   COMMIT-SPACE ;

: READY? ( -- bool )
   UNICODE-READY @ 0= 0= SPACE-READY @ 0= 0= and ;

: LETTER-COUNT ( -- n )   LETTER-N @ ;
: NUMBER-COUNT ( -- n )   NUMBER-N @ ;
: SPACE-COUNT ( -- n )    SPACE-N @ ;

: LETTER-RANGE@ ( n -- n n )
   LETTER-LO LETTER-HI LETTER-N @ RANGE@ ;

: NUMBER-RANGE@ ( n -- n n )
   NUMBER-LO NUMBER-HI NUMBER-N @ RANGE@ ;

: SPACE-RANGE@ ( n -- n n )
   SPACE-LO SPACE-HI SPACE-N @ RANGE@ ;

;package
