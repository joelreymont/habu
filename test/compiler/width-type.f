\ width-type.f - the built-in type `width<t>`: one cell holding the bytes t
\ takes in storage, read with `width>n`.
require src/lib/test.f
require src/lib/string.f

\ File-global: the programs below compile at run time, in the scope a real
\ program writes them in.
STRUCTURE turn DERIVE addr FIELD alive bool FIELD angle r ;STRUCTURE
DEFLINEAR wtlin
STRUCTURE wtbox FIELD t wtlin FIELD k n ;STRUCTURE

package WIDTH-TYPE-TEST

private

70 constant CHECK-RC    \ the engine refusing a definition it cannot certify

create DIAG-BUF 8192 allot
8192 constant DIAG-CAP

: ARM ( -- )
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
   DISARM
   ARM
   [: s" : W8 ( width<u8> -- width<u32> ) ;" evaluate-closed ;]
   CHECK-RC TTHROWSQ
   S\" \"expected\":\"width<u32> \",\"actual\":\"width<u8> \"" HAS?
   DISARM
   ARM
   [: s" : W9 ( width<n> -- width<u8> ) ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   S\" \"expected\":\"width<u8> \",\"actual\":\"width<n> \"" HAS?
   DISARM
   [: s" : W10 ( width<u32> -- n ) width>n ;" evaluate-closed ;] 0 TTHROWSQ
   ARM
   [: s" : W11 ( width<u8> -- n ) W10 ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   S\" \"expected\":\"width<u32> \",\"actual\":\"width<u8> \"" HAS?
   DISARM
   ARM
   [: s" : W12 ( [ -- width<u8> ] -- [ -- width<u32> ] ) ;" evaluate-closed ;]
   CHECK-RC TTHROWSQ
   S\" \"expected\":\"[ -- width<u32> ] \",\"actual\":\"[ -- width<u8> ] \""
   HAS?
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

: DECL-REFUSED ( ptr u8 n -- )
   {: a:ptr u:n :}
   ARM
   [: a u evaluate-closed ;] 7109 TTHROWSQ
   s" a width is not stored: keep its bytes (width>n)" HAS?
   DISARM ;

: CASE-TYPE-FORM ( -- )
   s" [type] t in a body is the bytes t takes in storage" T-LABEL
   s" : TW1 ( -- width<turn> ) [type] turn ; TW1 width>n" TEST-EVAL:N 16 T=
   s" : TW2 ( -- width<n> ) [type] n ; TW2 width>n" TEST-EVAL:N 8 T=
   s" : TW3 ( -- width<u8> ) [type] u8 ; TW3 width>n" TEST-EVAL:N 1 T=
   s" : TW4 ( -- n ) [type] ptr turn width>n ; TW4" TEST-EVAL:N 8 T=
   s" : TW5 ( -- n ) [: [type] turn ;] execute width>n ; TW5" TEST-EVAL:N 16 T=
   s" : TW6 ( -- n ) [type] turn W1 ; TW6" TEST-EVAL:N 16 T= ;

: FORM-REFUSED ( ptr u8 n -- )
   {: a:ptr u:n :}
   ARM
   [: a u evaluate-closed ;] CHECK-RC TTHROWSQ
   S\" \"code\":\"E-BAD-STORAGE\"" HAS?
   s" [type] names no type storage holds" HAS?
   DISARM ;

: FORM-PINNED ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n f:ptr fu:n :}
   ARM
   [: a u evaluate-closed ;] CHECK-RC TTHROWSQ
   f fu HAS?
   DISARM ;

: CASE-TYPE-FORM-REFUSED ( -- )
   s" [type] refuses a type storage cannot hold" T-LABEL
   s" : TX1 ( ptr t -- n ) drop [type] t width>n ;" FORM-REFUSED
   s" : TX2 ( ptr t -- n ) drop [type] ptr t width>n ;" FORM-REFUSED
   s" : TX3 ( -- n ) [type] wtlin width>n ;" FORM-REFUSED
   s" : TX4 ( -- n ) [type] wtbox width>n ;" FORM-REFUSED
   s" : TX5 ( -- n ) [type] xt width>n ;" FORM-REFUSED
   s" : TX6 ( -- n ) [type] width<n> width>n ;" FORM-REFUSED
   s" : TX7 ( -- n ) [type] nosuch width>n ;" FORM-REFUSED
   s" : TX8 ( -- n ) [type] ;" FORM-REFUSED
   s" : TX11 ( -- n ) drop [type] u32 width>n ;"
   S\" \"token\":\"u32\"" FORM-PINNED
   s" : TX12 ( -- n ) drop [type] ;"
   S\" \"token\":\"[type]\"" FORM-PINNED
   s" : TX13 ( -- n ) [type] u32 then ;"
   S\" \"token\":\"u32\"" FORM-PINNED ;

: CASE-TYPE-FORM-END ( -- )
   s" a [type] span ends where its definition or quotation ends" T-LABEL
   s" : TX9 [type] ptr ;" FORM-REFUSED
   s" : TX10 ( -- n ) [: [type] ptr ;] drop 1 ;"
   S\" \"token\":\"ptr\"" FORM-PINNED ;

: CASE-TYPE-FORM-NAME ( -- )
   s" [type] is a keyword no definition is named" T-LABEL
   s" : [type] ;" TEST-EVAL:RC CHECK-RC T= ;

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
   CASE-TYPE-FORM
   CASE-TYPE-FORM-REFUSED
   CASE-TYPE-FORM-END
   CASE-TYPE-FORM-NAME
   T-REPORT ;

;package

WIDTH-TYPE-TEST:RUN
