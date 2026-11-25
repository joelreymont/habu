\ class-parse.f - checked parser for pinned Unicode character-class inputs.

require src/lib/prelude.f
require src/lib/string.f
require src/lib/error.f

package UNICODE-CLASS-TOOL

public

-7800 s" a pinned Unicode data file has a row this parser does not read" ERROR:FAULT E-SYNTAX
-7801 s" a Unicode data range is reversed, outside the scalars, in the surrogates, out of order or overlapping another, or a table index is out of range"
   ERROR:FAULT E-RANGE
-7802 s" the pinned Unicode data has more ranges or names than a table holds" ERROR:FAULT E-CAPACITY
-7803 s" a table was rendered before the pinned Unicode data was loaded" ERROR:FAULT E-STATE
-7805 s" a Unicode data file cannot be read or its SHA-256 is not the pinned one: restore the pinned file" ERROR:FAULT E-DIGEST

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
8 constant CLASS-HIDDEN
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
RANGE-CAPACITY TYPED-BUFFER HIDDEN-LO n
RANGE-CAPACITY TYPED-BUFFER HIDDEN-HI n
variable HIDDEN-N
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
TYPED-VARIABLE PENDING-NAME-A ^u8
variable PENDING-NAME-U

: FALSE-VALUE ( -- bool )   0 0= 0= ;
: TRUE-VALUE ( -- bool )    0 0= ;

: SLOT ( n ^a -- ^a ) {: idx:n base:^ :}
   idx 0 < if E-RANGE throw then
   idx RANGE-CAPACITY >= if E-RANGE throw then
   base idx cells + ;

: HEX-DIGIT ( n -- n bool ) {: c:n :}
   c 48 >= c 57 <= and if c 48 - TRUE-VALUE exit then
   c 65 >= c 70 <= and if c 55 - TRUE-VALUE exit then
   c 97 >= c 102 <= and if c 87 - TRUE-VALUE exit then
   0 FALSE-VALUE ;

: HEX-FOLD ( str n n -- n ) {: s:str pos:n acc:n :}
   pos s STR:LEN >= if acc exit then
   s STR:DATA pos + c@ HEX-DIGIT 0= if drop E-SYNTAX throw then {: digit:n :}
   s pos 1+ acc 16 * digit + recurse ;

: PARSE-HEX ( str -- n ) {: s:str :}
   s STR:LEN 0= s STR:LEN 6 > or if E-SYNTAX throw then
   s 0 0 HEX-FOLD ;

: SCALAR-RANGE ( n n -- ) {: lo:n hi:n :}
   lo 0 < hi lo < or hi SCALAR-MAX > or if E-RANGE throw then
   lo SURROGATE-LAST <= hi SURROGATE-FIRST >= and if E-RANGE throw then ;

: BYTE-INDEX ( str n -- n ) {: s:str c:n :}
   0 begin dup s STR:LEN < while
      dup s STR:DATA + c@ c = if exit then
      1+
   repeat ;

: NEXT-FIELD ( str n -- str n ) {: s:str start:n :}
   s SEMICOLON start STR:SPLIT-NEXT {: f:str next:n valid:bool :}
   valid 0= next s STR:LEN > or if E-SYNTAX throw then
   f next ;

: CATEGORY-SECOND? ( n str -- bool ) {: c:n allowed:str :}
   allowed c BYTE-INDEX allowed STR:LEN < ;

: SYMBOL-CLASS ( n -- n ) {: c:n :}
   c s" mcko" CATEGORY-SECOND? 0= if E-SYNTAX throw then
   c 109 = if CLASS-MATH-SYMBOL exit then
   c 111 = if CLASS-OTHER-SYMBOL exit then
   0 ;

: OTHER-CLASS ( n -- n ) {: c:n :}
   c s" cfson" CATEGORY-SECOND? 0= if E-SYNTAX throw then
   c 99 = c 102 = or if CLASS-HIDDEN exit then
   0 ;

: SEPARATOR-CLASS ( n -- n ) {: c:n :}
   c s" slp" CATEGORY-SECOND? 0= if E-SYNTAX throw then
   c 108 = c 112 = or if CLASS-HIDDEN exit then
   0 ;

: CATEGORY ( str -- n ) {: s:str :}
   s STR:LEN 2 <> if E-SYNTAX throw then
   s STR:DATA c@ 76 = if
      s STR:DATA 1+ c@ s" ultmo" CATEGORY-SECOND? 0= if E-SYNTAX throw then
      s STR:DATA 1+ c@ 117 = if CLASS-UPPERCASE else CLASS-LETTER then exit
   then
   s STR:DATA c@ 78 = if
      s STR:DATA 1+ c@ s" dlo" CATEGORY-SECOND? 0= if E-SYNTAX throw then
      s STR:DATA 1+ c@ 108 = if CLASS-LETTER-NUMBER else CLASS-NUMBER then exit
   then
   s STR:DATA c@ 77 = if s STR:DATA 1+ c@ s" nce" CATEGORY-SECOND? 0= if E-SYNTAX throw then CLASS-MARK exit then
   s STR:DATA c@ 80 = if s STR:DATA 1+ c@ s" cdseifo" CATEGORY-SECOND? 0= if E-SYNTAX throw then 0 exit then
   s STR:DATA c@ 83 = if s STR:DATA 1+ c@ SYMBOL-CLASS exit then
   s STR:DATA c@ 90 = if s STR:DATA 1+ c@ SEPARATOR-CLASS exit then
   s STR:DATA c@ 67 = if s STR:DATA 1+ c@ OTHER-CLASS exit then
   E-SYNTAX throw ;

: FIRST-NAME? ( str -- bool )
   s" , First>" STR:ENDS-WITH? ;

: LAST-NAME? ( str -- bool )
   s" , Last>" STR:ENDS-WITH? ;

: SAME-RANGE-NAME? ( str -- bool ) {: s:str :}
   s STR:LEN 7 < PENDING-NAME-U @ 8 < or if FALSE-VALUE exit then
   s STR:DATA s STR:LEN 7 - >str
   PENDING-NAME-A @ PENDING-NAME-U @ 8 - >str STR:= ;

: RANGE-APPEND ( n n ^n ^n ^n -- )
   {: lo:n hi:n los:^ his:^ countp:^ :}
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
   class CLASS-HIDDEN = if
      lo hi 0 HIDDEN-LO 0 HIDDEN-HI HIDDEN-N RANGE-APPEND
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

: OPEN-RANGE ( n str n -- ) {: cp:n name:str class:n :}
   cp SOURCE-ORDER
   cp SOURCE-PREV !
   TRUE-VALUE PENDING !
   cp PENDING-LO !
   class PENDING-CLASS !
   name STR:DATA PENDING-NAME-A !
   name STR:LEN PENDING-NAME-U ! ;

: CLOSE-RANGE ( n str n -- ) {: cp:n name:str class:n :}
   PENDING @ 0= if E-SYNTAX throw then
   cp SOURCE-ORDER
   name SAME-RANGE-NAME? 0= if E-SYNTAX throw then
   class PENDING-CLASS @ <> if E-SYNTAX throw then
   PENDING-LO @ cp class CLASS+
   cp SOURCE-PREV !
   FALSE-VALUE PENDING ! ;

: UNICODE-ROW ( str -- ) {: line:str :}
   line STR:TRIM {: row:str :}
   row STR:LEN 0= if exit then
   row SEMICOLON STR:COUNT-CHAR 14 <> if E-SYNTAX throw then
   row 0 NEXT-FIELD {: code:str next1:n :}
   row next1 NEXT-FIELD {: name:str next2:n :}
   row next2 NEXT-FIELD {: cat:str next3:n :}
   next3 drop
   code STR:TRIM PARSE-HEX {: cp:n :}
   cat STR:TRIM CATEGORY {: class:n :}
   PENDING @ if
      name LAST-NAME? 0= if E-SYNTAX throw then
      cp name class CLOSE-RANGE
      exit
   then
   name LAST-NAME? if E-SYNTAX throw then
   name FIRST-NAME? if
      cp name class OPEN-RANGE
      exit
   then
   cp SOURCE-ORDER
   cp SOURCE-PREV !
   cp cp class CLASS+ ;

: SCAN-UNICODE ( str -- ) {: s:str :}
   0 begin dup s STR:LEN <= while
      {: start:n :}
      s LINE-FEED start STR:SPLIT-NEXT {: line:str next:n valid:bool :}
      valid 0= if E-SYNTAX throw then
      line UNICODE-ROW
      next
   repeat drop
   PENDING @ if E-SYNTAX throw then ;

: RANGE-PART ( str -- n n ) {: s:str :}
   s DOT BYTE-INDEX {: pos:n :}
   pos s STR:LEN = if s STR:TRIM PARSE-HEX dup exit then
   pos 0= pos 1+ s STR:LEN >= or if E-SYNTAX throw then
   s STR:DATA pos + 1+ c@ DOT <> if E-SYNTAX throw then
   s STR:DATA pos >str STR:TRIM PARSE-HEX
   s STR:DATA pos 2 + + s STR:LEN pos 2 + - >str STR:TRIM PARSE-HEX ;

: RANGE-ROW ( str [ n n str -- ] -- )
   {: line:str q :}
   line HASH BYTE-INDEX {: comment:n :}
   line STR:DATA comment >str STR:TRIM {: row:str :}
   row STR:LEN 0= if exit then
   row 0 NEXT-FIELD {: range:str next:n :}
   range STR:TRIM RANGE-PART
   row STR:DATA next + row STR:LEN next - >str STR:TRIM q execute ;

public

\ Calls q with the first and last scalar and the trimmed value of each
\ `XXXX ; Value` or `XXXX..YYYY ; Value` row of a UCD file, skipping blank
\ and comment text.
: RANGE-ROWS ( str [ n n str -- ] -- ) {: s:str q :}
   0 begin dup s STR:LEN <= while
      {: start:n :}
      s LINE-FEED start STR:SPLIT-NEXT {: line:str next:n valid:bool :}
      valid 0= if E-SYNTAX throw then
      line q RANGE-ROW
      next
   repeat drop ;

private

: SPACE-PROPERTY? ( str -- bool )
   STR:TRIM s" White_Space" STR:= ;

: PROP+ ( n n str -- ) {: lo:n hi:n prop:str :}
   prop s" Other_Uppercase" STR:= if
      lo hi 0 OTHER-UPPER-LO 0 OTHER-UPPER-HI OTHER-UPPER-N RANGE-APPEND exit
   then
   prop s" Other_Alphabetic" STR:= if
      lo hi 0 OTHER-LO 0 OTHER-HI OTHER-N RANGE-APPEND exit
   then
   prop SPACE-PROPERTY? 0= if lo hi SCALAR-RANGE exit then
   lo hi 0 SPACE-LO 0 SPACE-HI SPACE-N RANGE-APPEND ;

: SCAN-PROPS ( str -- )
   [: PROP+ ;] RANGE-ROWS ;

\ A table of scalar and paired-bracket cells, kept strictly ascending by scalar.
: PAIR-APPEND ( n n ^n ^n ^n -- )
   {: cp:n pair:n cps:^ pairs:^ countp:^ :}
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

: BRACKET-ROW ( str -- ) {: line:str :}
   line HASH BYTE-INDEX {: comment:n :}
   line STR:DATA comment >str STR:TRIM {: row:str :}
   row STR:LEN 0= if exit then
   row 0 NEXT-FIELD {: code:str next1:n :}
   row next1 NEXT-FIELD {: pair:str next2:n :}
   row STR:DATA next2 + row STR:LEN next2 - >str STR:TRIM {: t:str :}
   t STR:LEN 1 <> if E-SYNTAX throw then
   code STR:TRIM PARSE-HEX pair STR:TRIM PARSE-HEX
   t STR:DATA c@ BRACKET+ ;

: SCAN-BRACKETS ( str -- ) {: s:str :}
   0 begin dup s STR:LEN <= while
      {: start:n :}
      s LINE-FEED start STR:SPLIT-NEXT {: line:str next:n valid:bool :}
      valid 0= if E-SYNTAX throw then
      line BRACKET-ROW
      next
   repeat drop ;

: VALID-INDEX ( n n -- n ) {: idx:n count:n :}
   idx 0 < idx count >= or if E-RANGE throw then
   idx ;

: RANGE@ ( n ^n ^n n -- n n ) {: idx:n los:^ his:^ count:n :}
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
   0 MATH-N ! 0 SO-N ! 0 HIDDEN-N !
   0 OPEN-N ! 0 CLOSE-N ! ;

: PARSE-UNICODE ( str -- )
   FALSE-VALUE UNICODE-READY !
   0 LETTER-N ! 0 NUMBER-N ! 0 NL-N ! 0 UPPER-N ! 0 MARK-N !
   0 MATH-N ! 0 SO-N ! 0 HIDDEN-N !
   -1 SOURCE-PREV ! FALSE-VALUE PENDING !
   SCAN-UNICODE
   TRUE-VALUE UNICODE-READY ! ;

: PARSE-PROPERTIES ( str -- )
   FALSE-VALUE SPACE-READY !
   0 SPACE-N ! 0 OTHER-N ! 0 OTHER-UPPER-N !
   SCAN-PROPS
   TRUE-VALUE SPACE-READY ! ;

: PARSE-BRACKETS ( str -- )
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

: HIDDEN-COUNT ( -- n ) HIDDEN-N @ ;

: HIDDEN-RANGE@ ( n -- n n )
   0 HIDDEN-LO 0 HIDDEN-HI HIDDEN-N @ RANGE@ ;

: OPEN-BRACKET-COUNT ( -- n ) OPEN-N @ ;
: CLOSE-BRACKET-COUNT ( -- n ) CLOSE-N @ ;

\ An opening bracket and its closing pair.
: OPEN-BRACKET-PAIR@ ( n -- n n )
   0 OPEN-CP 0 OPEN-PAIR OPEN-N @ RANGE@ ;

\ A closing bracket and its opening pair.
: CLOSE-BRACKET-PAIR@ ( n -- n n )
   0 CLOSE-CP 0 CLOSE-PAIR CLOSE-N @ RANGE@ ;

;package
