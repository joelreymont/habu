\ long-type.f - a definer whose stray built local has a type text longer than
\ the token buffers' first capacity, so its rejection grows them.

package BITEM-LONG
private

5000 BUFFER: TEXT  variable TEXT-U
: TEXT$ ( -- ptr u8 n ) TEXT TEXT-U @ ;
: ADD ( ptr u8 n -- )
   {: a:ptr u:n :}
   a TEXT TEXT-U @ + u BYTE-COPY  u TEXT-U +! ;
: ELEM ( -- ) 17 0 ?do s" ptr " ADD loop  s" u8 " ADD ;

\ [ ptr … ptr u8 … -- ]: 64 inputs of 17 ptrs, 4550 bytes.
: LONG$ ( -- ptr u8 n )
   0 TEXT-U !  s" [ " ADD  64 0 ?do ELEM loop  s" -- ]" ADD  TEXT$ ;

: MAKE-LONG ( ptr u8 n -- )
   EFFECT:EMPTY define-word  LONG$ EFFECT:READ-TYPE EFFECT:LOCAL drop
   postpone ; ;

s" BI-LONG" MAKE-LONG

;package
