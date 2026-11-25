\ long-type.f - a definer whose stray built local has a type text longer than
\ the token buffers' first capacity, so its rejection grows them.

package BITEM-LONG
private

5000 BUFFER: TEXT  variable TEXT-U
: TEXT$ ( -- str ) TEXT TEXT-U @ >str ;
: ADD ( str -- )
   {: s:str :}
   s STR:DATA TEXT TEXT-U @ + s STR:LEN BYTE-COPY
   s STR:LEN TEXT-U +! ;
: ELEM ( -- ) 17 0 ?do s" ^" ADD loop  s" u8 " ADD ;

\ [ ^^…^u8 … -- ]: 228 inputs of 17 carets, 4566 bytes, above checker.f
\ TOKBUF-INIT-CAP (4096).
: LONG$ ( -- str )
   0 TEXT-U !  s" [ " ADD  228 0 ?do ELEM loop  s" -- ]" ADD  TEXT$ ;

: MAKE-LONG ( str -- )
   EFFECT:EMPTY define-word  LONG$ EFFECT:READ-TYPE EFFECT:LOCAL drop
   postpone ; ;

s" BI-LONG" MAKE-LONG

;package
