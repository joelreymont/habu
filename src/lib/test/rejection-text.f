\ rejection-text.f - rejection records as text a test compares, package
\ REJECTION-TEXT. FLAT drops each record's gutter, so expected text holds no
\ width that moves when lines are added above the record. A record runs from a
\ line starting `error[` or `warning[` to the next empty line; its gutter is the
\ blanks before its `-->`, 1 with none, and each later line that starts with that
\ many blanks or digits and a blank loses them and the blank.
\ STORAGE CLASS. PROCESS-WIDE: one vector, which each FLAT reuses.

require src/lib/heap.f
require src/lib/vector.f
require src/lib/string.f

package REJECTION-TEXT
private

\ null until the first FLAT
TYPED-VARIABLE OUT ^VEC:vector<u8>
variable GUTTER
\ true from a record's header to the empty line after it
variable IN-RECORD

: EMIT+ ( str -- ) OUT @ VEC:APPEND ;

: HEADER? ( str -- bool ) {: s:str :}
   s s" error[" STR:STARTS-WITH?  s s" warning[" STR:STARTS-WITH? or ;

\ Whether s begins with the gutter: GUTTER blanks or digits, then a blank.
: GUTTERED? ( str -- bool ) {: s:str :}
   GUTTER @ {: g:n :}
   s STR:LEN g <= IF false EXIT THEN
   s STR:DATA g + c@ 32 <> IF false EXIT THEN
   g 0 ?DO
      s STR:DATA i + c@ {: c:n :}
      c 32 =  c STR:DIGIT? or 0= IF false unloop EXIT THEN
   LOOP true ;

\ The line s, its newline excluded, onto OUT, its record's gutter dropped.
: LINE+ ( str -- ) {: s:str :}
   s HEADER? IF 1 GUTTER !  true IN-RECORD !  s EMIT+ EXIT THEN
   IN-RECORD @ 0= IF s EMIT+ EXIT THEN
   s STR:LEN 0= IF false IN-RECORD ! EXIT THEN
   s STR:LTRIM {: t:str :}
   t s" -->" STR:STARTS-WITH? IF s STR:LEN t STR:LEN - GUTTER !  t EMIT+ EXIT THEN
   s GUTTERED? IF GUTTER @ 1 + {: g:n :}  s STR:DATA g +  s STR:LEN g - >str  EMIT+ EXIT THEN
   s EMIT+ ;

public

\ The text s with each record's gutter dropped, valid until the next FLAT,
\ so s is never an earlier FLAT's answer.
: FLAT ( str -- str ) {: s:str :}
   OUT @ NULL-PTR = IF HEAP:ALLOCATOR 0 [type] u8 VEC:NEW OUT ! THEN
   OUT @ 0 VEC:RESIZE  false IN-RECORD !  1 GUTTER !
   s 10 0 BEGIN STR:SPLIT-NEXT WHILE {: p:str next:n :}
      p LINE+
      next s STR:LEN <= IF 10 OUT @ VEC:PUSH c! THEN
      s 10 next
   REPEAT drop drop
   OUT @ VEC:BYTES ;

\ Whether text s, its records' gutters dropped, holds e.
: HAS? ( str str -- bool ) {: s:str e:str :}
   s FLAT e STR:CONTAINS? ;

;package
