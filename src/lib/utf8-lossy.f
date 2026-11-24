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
: STEP ( str n -- n bool ) {: source:str cursor:n :}
   source cursor UTF8:NEXT
   MATCH UTF8:scalar-step
      scalar OF nip true ENDOF
      raw-byte OF
         2drop source cursor UTF8:SUBPART-END false
      ENDOF
   ;MATCH ;

: PIECE ( n str n -- n n ) {: total:n source:str cursor:n :}
   source cursor STEP if
      dup cursor - total + swap
   else
      total REPLACEMENT STR:LEN + swap
   then ;

: COPIED ( ^u8 n n ^u8 -- ^u8 n ) {: source:^ cursor:n next:n dst:^ :}
   source cursor + dst next cursor - BYTE-COPY
   dst next cursor - + next ;

: REPLACED ( n ^u8 -- ^u8 n ) {: next:n dst:^ :}
   REPLACEMENT {: r:str :}
   r STR:DATA dst r STR:LEN BYTE-COPY
   dst r STR:LEN + next ;

: PUT ( ^u8 str n -- ^u8 n ) {: dst:^ source:str cursor:n :}
   source cursor STEP if
      source STR:DATA cursor rot dst COPIED
   else
      dst REPLACED
   then ;

: COPY-SIZE ( str -- n ) {: source:str :}
   0 0 begin dup source STR:LEN < while
      source swap PIECE
   repeat drop ;

: COPY-FILL ( ^u8 str -- ) {: out:^ source:str :}
   out 0 begin dup source STR:LEN < while
      source swap PUT
   repeat 2drop ;

public

: REPAIR ( str ^ARENA:arena -- str )
   {: source:str a:^ :}
   source UTF8:VALID? if source exit then
   a source COPY-SIZE ARENA:ALLOC {: out:^ size:n :}
   out source COPY-FILL
   out size >str ;

;package
