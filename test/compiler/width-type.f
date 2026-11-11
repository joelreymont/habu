\ width-type.f - the built-in type `width<t>`: one cell holding the bytes t
\ takes in storage, read with `width>n`. Its slot takes a type of any width, a
\ width of one type is not a width of another, and no storage holds one.
require src/lib/test.f
require src/lib/string.f

\ File-global: the programs below compile at run time, in the scope a real
\ program writes them in.
STRUCTURE turn DERIVE addr FIELD alive bool FIELD angle r ;STRUCTURE

package WIDTH-TYPE-TEST

private

70 constant CHECK-RC    \ the engine refusing a definition it cannot certify

create DIAG-BUF 8192 allot
8192 constant DIAG-CAP

: ARM ( -- )   \ capture the next diagnostic as JSON instead of printing it
   DIAG-BUF DIAG-CAP DIAG-BUFFER!  true DIAG-JSON! ;

: DISARM ( -- )
   false DIAG-JSON!  DIAG-BUFFER-OFF ;

: HAS? ( ptr u8 n -- )
   DIAG-BUFFER$ 2swap CONTAINS? TTRUE ;

: STORED ( -- )
   S\" \"code\":\"E-BAD-STORAGE\"" HAS?
   s" a width is not stored: keep its bytes (width>n)" HAS? ;

: CASE-GENERIC ( -- )
   s" a width of any type is read and passed on" T-LABEL
   [: s" : W1 ( width<t> -- n ) width>n ;" evaluate-closed ;] 0 TTHROWSQ
   [: s" : W2 ( width<t> -- width<t> ) ;" evaluate-closed ;] 0 TTHROWSQ ;

: CASE-RECORD ( -- )
   s" a record of two cells binds the slot" T-LABEL
   [: s" : W3 ( width<turn> -- n ) width>n ;" evaluate-closed ;] 0 TTHROWSQ
   [: s" : W4 ( width<turn> -- n ) W1 ;" evaluate-closed ;] 0 TTHROWSQ ;

: CASE-OTHER-TYPE ( -- )
   s" a width of one type is not a width of another" T-LABEL
   ARM
   [: s" : W5 ( width<n> -- n ) W3 ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   S\" \"expected\":\"width<turn> \",\"actual\":\"width<n> \"" HAS?
   DISARM ;

: CASE-NOT-N ( -- )
   s" an n is not a width, and a width takes no arithmetic" T-LABEL
   ARM
   [: s" : W6 ( n -- width<n> ) ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   S\" \"expected\":\"width<n> \",\"actual\":\"n \"" HAS?
   DISARM
   ARM
   [: s" : W7 ( width<n> -- n ) 1 + ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   S\" \"expected\":\"n n \",\"actual\":\"width<n> n \"" HAS?
   DISARM ;

: CASE-STORAGE ( -- )
   s" no storage holds a width" T-LABEL
   ARM
   [: s" TYPED-VARIABLE WV width<n>" evaluate-closed ;] CHECK-RC TTHROWSQ
   STORED
   DISARM
   ARM
   [: s" 4 TYPED-BUFFER WB width<n>" evaluate-closed ;] CHECK-RC TTHROWSQ
   STORED
   DISARM ;

: DECL-REFUSED ( ptr u8 n -- )   \ the declaration text is refused for its width
   {: a:ptr u:n :}
   ARM
   [: a u evaluate-closed ;] 7109 TTHROWSQ
   s" a width is not stored: keep its bytes (width>n)" HAS?
   DISARM ;

: CASE-MEMBER ( -- )
   s" no declaration holds a width member" T-LABEL
   s" STRUCTURE sw1 FIELD w width<n> ;STRUCTURE" DECL-REFUSED
   s" STRUCTURE sw2<a> FIELD w width<a> ;STRUCTURE" DECL-REFUSED
   s" STRUCTURE sw3 FIELD w ptr width<n> ;STRUCTURE" DECL-REFUSED
   s" STRUCTURE sw4 FIELD w option<width<n>> ;STRUCTURE" DECL-REFUSED
   s" SUMTYPE stw VARIANT one width<n> ;VARIANT ;SUMTYPE" DECL-REFUSED ;

public

: RUN ( -- )
   T-RESET
   CASE-GENERIC
   CASE-RECORD
   CASE-OTHER-TYPE
   CASE-NOT-N
   CASE-STORAGE
   CASE-MEMBER
   T-REPORT ;

;package

WIDTH-TYPE-TEST:RUN
