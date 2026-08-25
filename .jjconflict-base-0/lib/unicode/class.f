\ class.f - complete pinned Unicode character-class queries.

require lib/unicode/class-data.f

package UNICODE-CLASS

private

$10FFFF constant SCALAR-MAX
$D800 constant SURROGATE-FIRST
$DFFF constant SURROGATE-LAST

: FALSE-VALUE ( -- bool )   0 0= 0= ;

: LETTER-SEARCH ( n n n -- bool ) {: cp:n lo:n hi:n :}
   lo hi >= if FALSE-VALUE exit then
   lo hi + 2 / {: mid:n :}
   mid UNICODE-CLASS-DATA:LETTER-RANGE@ {: first:n last:n :}
   cp first < if cp lo mid recurse exit then
   cp last > if cp mid 1+ hi recurse exit then
   0 0= ;

: NUMBER-SEARCH ( n n n -- bool ) {: cp:n lo:n hi:n :}
   lo hi >= if FALSE-VALUE exit then
   lo hi + 2 / {: mid:n :}
   mid UNICODE-CLASS-DATA:NUMBER-RANGE@ {: first:n last:n :}
   cp first < if cp lo mid recurse exit then
   cp last > if cp mid 1+ hi recurse exit then
   0 0= ;

: SPACE-SEARCH ( n n n -- bool ) {: cp:n lo:n hi:n :}
   lo hi >= if FALSE-VALUE exit then
   lo hi + 2 / {: mid:n :}
   mid UNICODE-CLASS-DATA:WHITE-SPACE-RANGE@ {: first:n last:n :}
   cp first < if cp lo mid recurse exit then
   cp last > if cp mid 1+ hi recurse exit then
   0 0= ;

public

: SCALAR? ( n -- bool ) {: cp:n :}
   cp 0 >= cp SCALAR-MAX <= and
   cp SURROGATE-FIRST >= cp SURROGATE-LAST <= and 0= and ;

: LETTER? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if FALSE-VALUE exit then
   cp 0 UNICODE-CLASS-DATA:LETTER-RANGE-COUNT LETTER-SEARCH ;

: NUMBER? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if FALSE-VALUE exit then
   cp 0 UNICODE-CLASS-DATA:NUMBER-RANGE-COUNT NUMBER-SEARCH ;

: WHITE-SPACE? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if FALSE-VALUE exit then
   cp 0 UNICODE-CLASS-DATA:WHITE-SPACE-RANGE-COUNT SPACE-SEARCH ;

: VERSION$ ( -- ptr u8 n )
   UNICODE-CLASS-DATA:VERSION$ ;

: UNICODE-DATA-SHA256$ ( -- ptr u8 n )
   UNICODE-CLASS-DATA:UNICODE-DATA-SHA256$ ;

: PROP-LIST-SHA256$ ( -- ptr u8 n )
   UNICODE-CLASS-DATA:PROP-LIST-SHA256$ ;

;package
