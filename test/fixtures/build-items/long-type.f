\ long-type.f - a definer whose stray built local has a type text longer than
\ the token buffers' first capacity, so its rejection grows them.

package BITEM-LONG
private

5000 BUFFER: TEXT  variable TEXT-U
: TEXT$ ( -- ^u8 n ) TEXT TEXT-U @ ;
: ADD ( ^u8 n -- )
   {: a:^ u:n :}
   a TEXT TEXT-U @ + u BYTE-COPY  u TEXT-U +! ;
: ELEM ( -- ) 17 0 ?do s" ^" ADD loop  s" u8 " ADD ;

\ [ ^^…^u8 … -- ]: 228 inputs of 17 carets, 4566 bytes, above checker.f
\ TOKBUF-INIT-CAP (4096).
: LONG$ ( -- ^u8 n )
   0 TEXT-U !  s" [ " ADD  228 0 ?do ELEM loop  s" -- ]" ADD  TEXT$ ;

: MAKE-LONG ( ^u8 n -- )
   EFFECT:EMPTY define-word  LONG$ EFFECT:READ-TYPE EFFECT:LOCAL drop
   postpone ; ;

s" BI-LONG" MAKE-LONG

;package
