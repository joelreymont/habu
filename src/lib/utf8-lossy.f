split-str
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

: REPLACEMENT ( -- str )
   S\" \xEF\xBF\xBD" ;

\ The next piece's end, and whether it is copied or replaced.
: STEP ( str n -- n bool ) {: source:^ length:n cursor:n :}
   source length cursor UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF nip true ENDOF
      raw-byte OF
         2drop source length cursor UTF8:SUBPART-END false
      ENDOF
   ;MATCH ;

: PIECE ( n str n -- n n ) {: total:n source:^ length:n cursor:n :}
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

: PUT ( ^u8 str n -- ^u8 n ) {: dst:^ source:^ length:n cursor:n :}
   source length cursor STEP if
      source cursor rot dst COPIED
   else
      dst REPLACED
   then ;

: COPY-SIZE ( str -- n ) {: source:^ length:n :}
   0 0 begin dup length < while
      source length rot PIECE
   repeat drop ;

: COPY-FILL ( ^u8 str -- ) {: out:^ source:^ length:n :}
   out 0 begin dup length < while
      source length rot PUT
   repeat 2drop ;

public

: REPAIR ( str ^ARENA:arena -- str )
   {: source:^ length:n a:^ :}
   source length UTF8:VALID? if source length exit then
   a source length COPY-SIZE ARENA:ALLOC {: out:^ size:n :}
   out source length COPY-FILL
   out size ;

;package
