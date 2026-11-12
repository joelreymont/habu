\ class-parse.f - checked parser for pinned Unicode character-class inputs.

require src/lib/prelude.f
require src/lib/string.f

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
3 constant CLASS-LETTER-NUMBER
4 constant CLASS-UPPERCASE
5 constant CLASS-MARK
6 constant CLASS-MATH-SYMBOL
7 constant CLASS-OTHER-SYMBOL
111 constant OPEN-TYPE
99 constant CLOSE-TYPE

RANGE-CAPACITY TYPED-BUFFER LETTER-LO n
RANGE-CAPACITY TYPED-BUFFER LETTER-HI n
RANGE-CAPACITY TYPED-BUFFER NUMBER-LO n
RANGE-CAPACITY TYPED-BUFFER NUMBER-HI n
RANGE-CAPACITY TYPED-BUFFER SPACE-LO n
RANGE-CAPACITY TYPED-BUFFER SPACE-HI n
variable LETTER-N
variable NUMBER-N
variable SPACE-N
RANGE-CAPACITY TYPED-BUFFER NL-LO n
RANGE-CAPACITY TYPED-BUFFER NL-HI n
RANGE-CAPACITY TYPED-BUFFER OTHER-LO n
RANGE-CAPACITY TYPED-BUFFER OTHER-HI n
variable NL-N
RANGE-CAPACITY TYPED-BUFFER UPPER-LO n
RANGE-CAPACITY TYPED-BUFFER UPPER-HI n
variable UPPER-N
variable OTHER-N
RANGE-CAPACITY TYPED-BUFFER OTHER-UPPER-LO n
RANGE-CAPACITY TYPED-BUFFER OTHER-UPPER-HI n
variable OTHER-UPPER-N
RANGE-CAPACITY TYPED-BUFFER MARK-LO n
RANGE-CAPACITY TYPED-BUFFER MARK-HI n
variable MARK-N
RANGE-CAPACITY TYPED-BUFFER MATH-LO n
RANGE-CAPACITY TYPED-BUFFER MATH-HI n
variable MATH-N
RANGE-CAPACITY TYPED-BUFFER SO-LO n
RANGE-CAPACITY TYPED-BUFFER SO-HI n
variable SO-N
RANGE-CAPACITY TYPED-BUFFER OPEN-CP n
RANGE-CAPACITY TYPED-BUFFER OPEN-PAIR n
variable OPEN-N
RANGE-CAPACITY TYPED-BUFFER CLOSE-CP n
RANGE-CAPACITY TYPED-BUFFER CLOSE-PAIR n
variable CLOSE-N
variable UNICODE-READY
variable SPACE-READY
variable BRACKETS-READY

variable SOURCE-PREV
variable PENDING
variable PENDING-LO
variable PENDING-CLASS
TYPED-VARIABLE PENDING-NAME-A ptr u8
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

: SYMBOL-CLASS ( n -- n ) {: c:n :}
   c s" mcko" CATEGORY-SECOND? 0= if E-SYNTAX throw then
   c 109 = if CLASS-MATH-SYMBOL exit then
   c 111 = if CLASS-OTHER-SYMBOL exit then
   0 ;

: CATEGORY ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 2 <> if E-SYNTAX throw then
   a c@ 76 = if
      a 1+ c@ s" ultmo" CATEGORY-SECOND? 0= if E-SYNTAX throw then
      a 1+ c@ 117 = if CLASS-UPPERCASE else CLASS-LETTER then exit
   then
   a c@ 78 = if
      a 1+ c@ s" dlo" CATEGORY-SECOND? 0= if E-SYNTAX throw then
      a 1+ c@ 108 = if CLASS-LETTER-NUMBER else CLASS-NUMBER then exit
   then
   a c@ 77 = if a 1+ c@ s" nce" CATEGORY-SECOND? 0= if E-SYNTAX throw then CLASS-MARK exit then
   a c@ 80 = if a 1+ c@ s" cdseifo" CATEGORY-SECOND? 0= if E-SYNTAX throw then 0 exit then
   a c@ 83 = if a 1+ c@ SYMBOL-CLASS exit then
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

: RANGE-APPEND ( n n ptr n ptr n ptr n -- )
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

: CLASS+ ( n n n -- ) {: lo:n hi:n class:n :}
   class CLASS-UPPERCASE = if
      lo hi 0 UPPER-LO 0 UPPER-HI UPPER-N RANGE-APPEND
   then
   class CLASS-LETTER = class CLASS-UPPERCASE = or if
      lo hi 0 LETTER-LO 0 LETTER-HI LETTER-N RANGE-APPEND
      exit
   then
   class CLASS-MARK = if
      lo hi 0 MARK-LO 0 MARK-HI MARK-N RANGE-APPEND
      exit
   then
   class CLASS-MATH-SYMBOL = if
      lo hi 0 MATH-LO 0 MATH-HI MATH-N RANGE-APPEND
      exit
   then
   class CLASS-OTHER-SYMBOL = if
      lo hi 0 SO-LO 0 SO-HI SO-N RANGE-APPEND
      exit
   then
   class CLASS-LETTER-NUMBER = if
      lo hi 0 NL-LO 0 NL-HI NL-N RANGE-APPEND
   then
   class CLASS-NUMBER = class CLASS-LETTER-NUMBER = or if
      lo hi 0 NUMBER-LO 0 NUMBER-HI NUMBER-N RANGE-APPEND
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
   PENDING-LO @ cp class CLASS+
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
   cp cp class CLASS+ ;

: SCAN-UNICODE ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u <= while
      {: start:n :}
      a u LINE-FEED start SPLIT-NEXT {: line:ptr lineu:n next:n valid:bool :}
      valid 0= if E-SYNTAX throw then
      line lineu UNICODE-ROW
      next
   repeat drop
   PENDING @ if E-SYNTAX throw then ;

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
   prop propu s" Other_Uppercase" STR= if
      lo hi 0 OTHER-UPPER-LO 0 OTHER-UPPER-HI OTHER-UPPER-N RANGE-APPEND exit
   then
   prop propu s" Other_Alphabetic" STR= if
      lo hi 0 OTHER-LO 0 OTHER-HI OTHER-N RANGE-APPEND exit
   then
   prop propu SPACE-PROPERTY? 0= if lo hi SCALAR-RANGE exit then
   lo hi 0 SPACE-LO 0 SPACE-HI SPACE-N RANGE-APPEND ;

: SCAN-PROPS ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u <= while
      {: start:n :}
      a u LINE-FEED start SPLIT-NEXT {: line:ptr lineu:n next:n valid:bool :}
      valid 0= if E-SYNTAX throw then
      line lineu PROP-ROW
      next
   repeat drop ;

\ A table of scalar and paired-bracket cells, kept strictly ascending by scalar.
: PAIR-APPEND ( n n ptr n ptr n ptr n -- )
   {: cp:n pair:n cps:ptr pairs:ptr countp:ptr :}
   cp cp SCALAR-RANGE
   pair pair SCALAR-RANGE
   countp @ 0 > if
      countp @ 1- cps SLOT @ cp >= if E-RANGE throw then
   then
   countp @ RANGE-CAPACITY >= if E-CAPACITY throw then
   cp countp @ cps SLOT !
   pair countp @ pairs SLOT !
   countp @ 1+ countp ! ;

\ The pinned file lists only Open and Close rows; a None row is refused.
: BRACKET+ ( n n n -- ) {: cp:n pair:n type:n :}
   type OPEN-TYPE = if
      cp pair 0 OPEN-CP 0 OPEN-PAIR OPEN-N PAIR-APPEND exit
   then
   type CLOSE-TYPE = if
      cp pair 0 CLOSE-CP 0 CLOSE-PAIR CLOSE-N PAIR-APPEND exit
   then
   E-SYNTAX throw ;

: BRACKET-ROW ( ptr u8 n -- ) {: line:ptr lineu:n :}
   line lineu HASH BYTE-INDEX {: comment:n :}
   line comment TRIM {: row:ptr rowu:n :}
   rowu 0= if exit then
   row rowu 0 NEXT-FIELD {: cpa:ptr cpu:n next1:n :}
   row rowu next1 NEXT-FIELD {: pa:ptr pu:n next2:n :}
   row next2 + rowu next2 - TRIM {: ta:ptr tu:n :}
   tu 1 <> if E-SYNTAX throw then
   cpa cpu TRIM PARSE-HEX pa pu TRIM PARSE-HEX ta c@ BRACKET+ ;

: SCAN-BRACKETS ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u <= while
      {: start:n :}
      a u LINE-FEED start SPLIT-NEXT {: line:ptr lineu:n next:n valid:bool :}
      valid 0= if E-SYNTAX throw then
      line lineu BRACKET-ROW
      next
   repeat drop ;

: VALID-INDEX ( n n -- n ) {: idx:n count:n :}
   idx 0 < idx count >= or if E-RANGE throw then
   idx ;

: RANGE@ ( n ptr n ptr n n -- n n ) {: idx:n los:ptr his:ptr count:n :}
   idx count VALID-INDEX {: safe:n :}
   safe los SLOT @ safe his SLOT @ ;

public

: RESET ( -- )
   FALSE-VALUE UNICODE-READY !
   FALSE-VALUE SPACE-READY !
   FALSE-VALUE BRACKETS-READY !
   0 LETTER-N !
   0 NUMBER-N !
   0 SPACE-N ! 0 NL-N ! 0 OTHER-N ! 0 UPPER-N ! 0 OTHER-UPPER-N ! 0 MARK-N !
   0 MATH-N ! 0 SO-N !
   0 OPEN-N ! 0 CLOSE-N ! ;

: PARSE-UNICODE ( ptr u8 n -- )
   FALSE-VALUE UNICODE-READY !
   0 LETTER-N ! 0 NUMBER-N ! 0 NL-N ! 0 UPPER-N ! 0 MARK-N !
   0 MATH-N ! 0 SO-N !
   -1 SOURCE-PREV ! FALSE-VALUE PENDING !
   SCAN-UNICODE
   TRUE-VALUE UNICODE-READY ! ;

: PARSE-PROPERTIES ( ptr u8 n -- )
   FALSE-VALUE SPACE-READY !
   0 SPACE-N ! 0 OTHER-N ! 0 OTHER-UPPER-N !
   SCAN-PROPS
   TRUE-VALUE SPACE-READY ! ;

: PARSE-BRACKETS ( ptr u8 n -- )
   FALSE-VALUE BRACKETS-READY !
   0 OPEN-N ! 0 CLOSE-N !
   SCAN-BRACKETS
   TRUE-VALUE BRACKETS-READY ! ;

: READY? ( -- bool )
   UNICODE-READY @ 0= 0= SPACE-READY @ 0= 0= and
   BRACKETS-READY @ 0= 0= and ;

: LETTER-COUNT ( -- n )   LETTER-N @ ;
: NUMBER-COUNT ( -- n )   NUMBER-N @ ;
: SPACE-COUNT ( -- n )    SPACE-N @ ;
: LETTER-NUMBER-COUNT ( -- n ) NL-N @ ;
: OTHER-ALPHABETIC-COUNT ( -- n ) OTHER-N @ ;

: LETTER-RANGE@ ( n -- n n )
   0 LETTER-LO 0 LETTER-HI LETTER-N @ RANGE@ ;

: NUMBER-RANGE@ ( n -- n n )
   0 NUMBER-LO 0 NUMBER-HI NUMBER-N @ RANGE@ ;

: SPACE-RANGE@ ( n -- n n )
   0 SPACE-LO 0 SPACE-HI SPACE-N @ RANGE@ ;

: LETTER-NUMBER-RANGE@ ( n -- n n )
   0 NL-LO 0 NL-HI NL-N @ RANGE@ ;

: OTHER-ALPHABETIC-RANGE@ ( n -- n n )
   0 OTHER-LO 0 OTHER-HI OTHER-N @ RANGE@ ;


: UPPERCASE-LETTER-COUNT ( -- n ) UPPER-N @ ;
: OTHER-UPPERCASE-COUNT ( -- n ) OTHER-UPPER-N @ ;


: UPPERCASE-LETTER-RANGE@ ( n -- n n )
   0 UPPER-LO 0 UPPER-HI UPPER-N @ RANGE@ ;


: OTHER-UPPERCASE-RANGE@ ( n -- n n )
   0 OTHER-UPPER-LO 0 OTHER-UPPER-HI OTHER-UPPER-N @ RANGE@ ;

: MARK-COUNT ( -- n ) MARK-N @ ;

: MARK-RANGE@ ( n -- n n )
   0 MARK-LO 0 MARK-HI MARK-N @ RANGE@ ;

: MATH-SYMBOL-COUNT ( -- n ) MATH-N @ ;

: MATH-SYMBOL-RANGE@ ( n -- n n )
   0 MATH-LO 0 MATH-HI MATH-N @ RANGE@ ;

: OTHER-SYMBOL-COUNT ( -- n ) SO-N @ ;

: OTHER-SYMBOL-RANGE@ ( n -- n n )
   0 SO-LO 0 SO-HI SO-N @ RANGE@ ;

: OPEN-BRACKET-COUNT ( -- n ) OPEN-N @ ;
: CLOSE-BRACKET-COUNT ( -- n ) CLOSE-N @ ;

\ An opening bracket and its closing pair.
: OPEN-BRACKET-PAIR@ ( n -- n n )
   0 OPEN-CP 0 OPEN-PAIR OPEN-N @ RANGE@ ;

\ A closing bracket and its opening pair.
: CLOSE-BRACKET-PAIR@ ( n -- n n )
   0 CLOSE-CP 0 CLOSE-PAIR CLOSE-N @ RANGE@ ;

;package
