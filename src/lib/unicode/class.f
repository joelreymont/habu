\ class.f - complete pinned Unicode character-class and bracket-pair queries.
\
\ STORAGE CLASS. PROCESS-WIDE read-only tables (src/lib/unicode/class-data.f,
\ generated); any task may query.

require src/lib/unicode/class-data.f

package UNICODE-CLASS

private

$10FFFF constant SCALAR-MAX
$D800 constant SURROGATE-FIRST
$DFFF constant SURROGATE-LAST

: ENTRY ( ptr n n -- ptr n )
   {: table:ptr idx:n :}
   table idx 2 * cells + ;

\ How many entries of a table of cell pairs sorted by first cell have a
\ first cell at most cp.
: UPPER-BOUND ( n ptr n n -- n )
   {: cp:n table:ptr count:n :}
   0 count
   begin 2dup < while
      {: lo:n hi:n :}
      lo hi + 2 / {: mid:n :}
      cp table mid ENTRY @ < if lo mid else mid 1+ hi then
   repeat
   drop ;

: RANGE-SEARCH ( n ptr n n -- bool )
   {: cp:n ranges:ptr count:n :}
   cp ranges count UPPER-BOUND {: at:n :}
   at 0= if false exit then
   cp ranges at 1- ENTRY cell+ @ <= ;

\ The second cell of the entry whose first cell is cp, and whether
\ there is one.
: PAIR-SEARCH ( n ptr n n -- n bool )
   {: cp:n pairs:ptr count:n :}
   cp pairs count UPPER-BOUND {: at:n :}
   at 0= if 0 false exit then
   pairs at 1- ENTRY {: pair:ptr :}
   pair @ cp <> if 0 false exit then
   pair cell+ @ true ;

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

\ Unicode General_Category Sm.
: MATH-SYMBOL? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if false exit then
   cp UNICODE-CLASS-DATA:MATH-SYMBOL-RANGES RANGE-SEARCH ;

\ Unicode General_Category So.
: OTHER-SYMBOL? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if false exit then
   cp UNICODE-CLASS-DATA:OTHER-SYMBOL-RANGES RANGE-SEARCH ;

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

\ Unicode Bidi_Paired_Bracket_Type Open.
: OPEN-BRACKET? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if false exit then
   cp UNICODE-CLASS-DATA:OPEN-BRACKET-PAIRS PAIR-SEARCH nip ;

\ Unicode Bidi_Paired_Bracket_Type Close.
: CLOSE-BRACKET? ( n -- bool ) {: cp:n :}
   cp SCALAR? 0= if false exit then
   cp UNICODE-CLASS-DATA:CLOSE-BRACKET-PAIRS PAIR-SEARCH nip ;

\ Unicode Bidi_Paired_Bracket; one with none, or a non-scalar, is itself.
: PAIRED-BRACKET ( n -- n ) {: cp:n :}
   cp SCALAR? 0= if cp exit then
   cp UNICODE-CLASS-DATA:OPEN-BRACKET-PAIRS PAIR-SEARCH if exit then drop
   cp UNICODE-CLASS-DATA:CLOSE-BRACKET-PAIRS PAIR-SEARCH if exit then drop
   cp ;

;package
