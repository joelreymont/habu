\ class.f - complete pinned Unicode character-class and bracket-pair queries.
\
\ STORAGE CLASS. PROCESS-WIDE read-only tables (src/lib/unicode/class-data.f,
\ generated); any task may query.

require src/lib/unicode/table.f
require src/lib/unicode/class-data.f

package UNICODE-CLASS

private

$10FFFF constant SCALAR-MAX
$D800 constant SURROGATE-FIRST
$DFFF constant SURROGATE-LAST

public

: SCALAR? ( n -- bool ) {: cp:n :}
   cp 0 >= cp SCALAR-MAX <= and
   cp SURROGATE-FIRST >= cp SURROGATE-LAST <= and 0= and ;

: LETTER? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if false exit then
   cp UNICODE-CLASS-DATA:LETTER-RANGES
   UNICODE-TABLE:IN-RANGES? ;

: NUMBER? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if false exit then
   cp UNICODE-CLASS-DATA:NUMBER-RANGES
   UNICODE-TABLE:IN-RANGES? ;

\ Unicode General_Category M: Mn, Mc and Me.
: MARK? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if false exit then
   cp UNICODE-CLASS-DATA:MARK-RANGES
   UNICODE-TABLE:IN-RANGES? ;

\ Unicode General_Category Sm.
: MATH-SYMBOL? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if false exit then
   cp UNICODE-CLASS-DATA:MATH-SYMBOL-RANGES
   UNICODE-TABLE:IN-RANGES? ;

\ Unicode General_Category So.
: OTHER-SYMBOL? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if false exit then
   cp UNICODE-CLASS-DATA:OTHER-SYMBOL-RANGES
   UNICODE-TABLE:IN-RANGES? ;

: WHITE-SPACE? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if false exit then
   cp UNICODE-CLASS-DATA:WHITE-SPACE-RANGES
   UNICODE-TABLE:IN-RANGES? ;

\ Unicode Alphabetic = L + Nl + Other_Alphabetic, from the pinned inputs.
: ALPHABETIC? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if false exit then
   cp LETTER? if true exit then
   cp UNICODE-CLASS-DATA:LETTER-NUMBER-RANGES
   UNICODE-TABLE:IN-RANGES?
   if true exit then
   cp UNICODE-CLASS-DATA:OTHER-ALPHABETIC-RANGES
   UNICODE-TABLE:IN-RANGES? ;

\ Unicode Uppercase = Lu + Other_Uppercase, including Roman/circled capitals.
: UPPERCASE? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if false exit then
   cp UNICODE-CLASS-DATA:UPPERCASE-LETTER-RANGES
   UNICODE-TABLE:IN-RANGES?
   if true exit then
   cp UNICODE-CLASS-DATA:OTHER-UPPERCASE-RANGES
   UNICODE-TABLE:IN-RANGES? ;

\ Unicode Bidi_Paired_Bracket_Type Open.
: OPEN-BRACKET? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if false exit then
   cp UNICODE-CLASS-DATA:OPEN-BRACKET-PAIRS
   UNICODE-TABLE:PAIR nip ;

\ Unicode Bidi_Paired_Bracket_Type Close.
: CLOSE-BRACKET? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if false exit then
   cp UNICODE-CLASS-DATA:CLOSE-BRACKET-PAIRS
   UNICODE-TABLE:PAIR nip ;

\ Unicode Bidi_Paired_Bracket; one with none, or a non-scalar, is itself.
: PAIRED-BRACKET ( n -- n ) {: cp:n :}
   cp SCALAR? 0= if cp exit then
   cp UNICODE-CLASS-DATA:OPEN-BRACKET-PAIRS
   UNICODE-TABLE:PAIR if exit then drop
   cp UNICODE-CLASS-DATA:CLOSE-BRACKET-PAIRS
   UNICODE-TABLE:PAIR if exit then drop
   cp ;

;package
