\ utf8-lossy.f - bytes as UTF-8, each ill-formed part replaced. REPAIR answers
\ valid input itself, uncopied; otherwise a copy in the caller's arena with each
\ maximal subpart of an ill-formed sequence (Unicode 3.9, Table 3-8) replaced by
\ U+FFFD: the longest prefix of a well-formed sequence (Table 3-7), else one
\ byte. A negative length is E-STR-BOUNDS. STORAGE CLASS. CALLER-OWNED: the
\ answer is the input or lives in the arena given.

require src/lib/error.f
require src/lib/arena.f
require src/lib/utf8-scalar.f

package UTF8-LOSSY

\ Table 3-7's leads, and the bytes each allows second; later bytes are
\ CONTINUATION-LEAST..CONTINUATION-MOST.
$C2 constant TWO-LEAD-LEAST
$E0 constant THREE-LEAD-LEAST
$ED constant SURROGATE-LEAD
$F0 constant FOUR-LEAD-LEAST
$F4 constant FOUR-LEAD-MOST
$80 constant CONTINUATION-LEAST
$BF constant CONTINUATION-MOST
$A0 constant E0-SECOND-LEAST
$9F constant ED-SECOND-MOST
$90 constant F0-SECOND-LEAST
$8F constant F4-SECOND-MOST

: REPLACEMENT ( -- ^u8 n )
   S\" \xEF\xBF\xBD" ;

\ A well-formed sequence's width for its lead, 1 for a byte that leads none.
: LEAD-WIDTH ( n -- n ) {: lead:n :}
   lead TWO-LEAD-LEAST < if 1 exit then
   lead THREE-LEAD-LEAST < if 2 exit then
   lead FOUR-LEAD-LEAST < if 3 exit then
   lead FOUR-LEAD-MOST <= if 4 exit then
   1 ;

: SECOND-LEAST ( n -- n ) {: lead:n :}
   lead THREE-LEAD-LEAST = if E0-SECOND-LEAST exit then
   lead FOUR-LEAD-LEAST = if F0-SECOND-LEAST exit then
   CONTINUATION-LEAST ;

: SECOND-MOST ( n -- n ) {: lead:n :}
   lead SURROGATE-LEAD = if ED-SECOND-MOST exit then
   lead FOUR-LEAD-MOST = if F4-SECOND-MOST exit then
   CONTINUATION-MOST ;

: IN-RANGE? ( n n n -- bool ) {: byte:n least:n most:n :}
   byte least >= byte most <= and ;

\ Whether `byte` may follow at `index` after `lead` in a well-formed sequence.
: TRAIL-OK? ( n n n -- bool ) {: lead:n index:n byte:n :}
   index 1 = if
      byte lead SECOND-LEAST lead SECOND-MOST IN-RANGE? exit
   then
   byte CONTINUATION-LEAST CONTINUATION-MOST IN-RANGE? ;

: SUBPART ( ^u8 n n n -- n ) {: at:^ limit:n lead:n index:n :}
   index limit >= if index exit then
   lead index at index + c@ TRAIL-OK? 0= if index exit then
   at limit lead index 1+ recurse ;

\ The end of the maximal subpart at `cursor`, which NEXT answers RAW-BYTE.
: SUBPART-END ( ^u8 n n -- n ) {: source:^ length:n cursor:n :}
   source cursor + {: at:^ :}
   at c@ {: lead:n :}
   lead LEAD-WIDTH length cursor - min {: limit:n :}
   cursor at limit lead 1 SUBPART + ;

\ The next piece's end, and whether it is copied or replaced.
: STEP ( ^u8 n n -- n bool ) {: source:^ length:n cursor:n :}
   source length cursor UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF nip true ENDOF
      raw-byte OF
         2drop source length cursor SUBPART-END false
      ENDOF
   ;MATCH ;

: PIECE ( n ^u8 n n -- n n ) {: total:n source:^ length:n cursor:n :}
   source length cursor STEP if
      dup cursor - total + swap
   else
      total REPLACEMENT nip + swap
   then ;

: COPIED ( ^u8 n n ^u8 -- ^u8 n ) {: source:^ cursor:n next:n dst:^ :}
   source cursor + dst next cursor - BYTE-COPY
   dst next cursor - + next ;

: REPLACED ( n ^u8 -- ^u8 n ) {: next:n dst:^ :}
   REPLACEMENT {: r:^ width:n :}
   r dst width BYTE-COPY
   dst width + next ;

: PUT ( ^u8 ^u8 n n -- ^u8 n ) {: dst:^ source:^ length:n cursor:n :}
   source length cursor STEP if
      source cursor rot dst COPIED
   else
      dst REPLACED
   then ;

: COPY-SIZE ( ^u8 n -- n ) {: source:^ length:n :}
   0 0 begin dup length < while
      source length rot PIECE
   repeat drop ;

: COPY-FILL ( ^u8 ^u8 n -- ) {: out:^ source:^ length:n :}
   out 0 begin dup length < while
      source length rot PUT
   repeat 2drop ;

public

: REPAIR ( ^u8 n ^ARENA:arena -- ^u8 n )
   {: source:^ length:n a:^ :}
   source length UTF8:VALID? if source length exit then
   a source length COPY-SIZE ARENA:ALLOC {: out:^ size:n :}
   out source length COPY-FILL
   out size ;

;package
