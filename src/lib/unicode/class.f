\ class.f - complete pinned Unicode character-class queries.
\
\ STORAGE CLASS. PROCESS-WIDE read-only tables (src/lib/unicode/class-data.f,
\ generated); any task may query.

require src/lib/unicode/class-data.f

package UNICODE-CLASS

private

$10FFFF constant SCALAR-MAX
$D800 constant SURROGATE-FIRST
$DFFF constant SURROGATE-LAST

: RANGE-SEARCH ( n ptr n n -- bool )
   {: cp:n ranges:ptr count:n :}
   0 count
   begin 2dup < while
      {: lo:n hi:n :}
      lo hi + 2 / {: mid:n :}
      ranges mid 2 * cells + {: pair:ptr :}
      cp pair @ < if lo mid else
         cp pair cell+ @ > if mid 1+ hi else true exit then
      then
   repeat
   2drop false ;

public

: SCALAR? ( n -- bool ) {: cp:n :}
   cp 0 >= cp SCALAR-MAX <= and
   cp SURROGATE-FIRST >= cp SURROGATE-LAST <= and 0= and ;

: LETTER? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if false exit then
   cp UNICODE-CLASS-DATA:LETTER-RANGES RANGE-SEARCH ;

: NUMBER? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if false exit then
   cp UNICODE-CLASS-DATA:NUMBER-RANGES RANGE-SEARCH ;

\ Unicode General_Category M: Mn, Mc and Me.
: MARK? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if false exit then
   cp UNICODE-CLASS-DATA:MARK-RANGES RANGE-SEARCH ;

: WHITE-SPACE? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if false exit then
   cp UNICODE-CLASS-DATA:WHITE-SPACE-RANGES RANGE-SEARCH ;

\ Unicode Alphabetic = L + Nl + Other_Alphabetic, from the pinned inputs.
: ALPHABETIC? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if false exit then
   cp LETTER? if true exit then
   cp UNICODE-CLASS-DATA:LETTER-NUMBER-RANGES RANGE-SEARCH
   if true exit then
   cp UNICODE-CLASS-DATA:OTHER-ALPHABETIC-RANGES RANGE-SEARCH ;

\ Unicode Uppercase = Lu + Other_Uppercase, including Roman/circled capitals.
: UPPERCASE? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if false exit then
   cp UNICODE-CLASS-DATA:UPPERCASE-LETTER-RANGES RANGE-SEARCH
   if true exit then
   cp UNICODE-CLASS-DATA:OTHER-UPPERCASE-RANGES RANGE-SEARCH ;

;package
