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


: ARM ( -- )
   REJECTION:CAPTURE  true REJECTION:JSON ! ;

: DISARM ( -- )
   false REJECTION:JSON !  REJECTION:BUFFER-OFF ;

: HAS? ( ^u8 n -- )
   REJECTION:BUFFER$ 2swap CONTAINS? TTRUE ;

: STORED ( -- )
   S\" \"code\":\"E-BAD-STORAGE\"" HAS?
   S\" \"label\":\"width in a stored type\"" HAS? ;

: MISMATCH ( ^u8 n -- )   \ the notes' expected and found
   S\" \"code\":\"E-MISMATCH\"" HAS?  HAS? ;

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
   [: s" : W5 ( width<n> -- n ) W3 ;" evaluate-closed ;] 70 TTHROWSQ
   S\" \"expected\":\"width<turn>\",\"found\":\"width<n>\"" MISMATCH
   DISARM
   ARM
   [: s" : W8 ( width<u8> -- width<u32> ) ;" evaluate-closed ;]
   70 TTHROWSQ
   S\" \"expected\":\"width<u32>\",\"found\":\"width<u8>\"" MISMATCH
   DISARM
   ARM
   [: s" : W9 ( width<n> -- width<u8> ) ;" evaluate-closed ;] 70 TTHROWSQ
   S\" \"expected\":\"width<u8>\",\"found\":\"width<n>\"" MISMATCH
   DISARM
   [: s" : W10 ( width<u32> -- n ) width>n ;" evaluate-closed ;] 0 TTHROWSQ
   ARM
   [: s" : W11 ( width<u8> -- n ) W10 ;" evaluate-closed ;] 70 TTHROWSQ
   S\" \"expected\":\"width<u32>\",\"found\":\"width<u8>\"" MISMATCH
   DISARM
   ARM
   [: s" : W12 ( [ -- width<u8> ] -- [ -- width<u32> ] ) ;" evaluate-closed ;]
   70 TTHROWSQ
   S\" \"expected\":\"[ -- width<u32> ]\",\"found\":\"[ -- width<u8> ]\""
   MISMATCH
   DISARM ;

: CASE-NOT-N ( -- )
   s" an n is not a width, and a width takes no arithmetic" T-LABEL
   ARM
   [: s" : W6 ( n -- width<n> ) ;" evaluate-closed ;] 70 TTHROWSQ
   S\" \"expected\":\"width<n>\",\"found\":\"n\"" MISMATCH
   DISARM
   ARM
   [: s" : W7 ( width<n> -- n ) 1 + ;" evaluate-closed ;] 70 TTHROWSQ
   S\" \"expected\":\"n n\",\"found\":\"width<n> n\"" MISMATCH
   DISARM ;

: CASE-STORAGE ( -- )
   s" no storage holds a width" T-LABEL
   ARM
   [: s" TYPED-VARIABLE WV width<n>" evaluate-closed ;] 70 TTHROWSQ
   STORED
   DISARM
   ARM
   [: s" 4 TYPED-BUFFER WB width<n>" evaluate-closed ;] 70 TTHROWSQ
   STORED
   DISARM ;

: DECL-REFUSED ( ^u8 n -- )
   {: a:^ u:n :}
   ARM
   [: a u evaluate-closed ;] 70 TTHROWSQ
   S\" \"code\":\"E-BAD-DECLARATION\"" HAS?
   s" a width is not stored: keep its bytes (width>n)" HAS?
   DISARM ;

: CASE-TYPE-FORM ( -- )
   s" [type] t in a body is the bytes t takes in storage" T-LABEL
   s" : TW1 ( -- width<turn> ) [type] turn ; TW1 width>n" TEST-EVAL:N 16 T=
   s" : TW2 ( -- width<n> ) [type] n ; TW2 width>n" TEST-EVAL:N 8 T=
   s" : TW3 ( -- width<u8> ) [type] u8 ; TW3 width>n" TEST-EVAL:N 1 T=
   s" : TW4 ( -- n ) [type] ^turn width>n ; TW4" TEST-EVAL:N 8 T=
   s" : TW5 ( -- n ) [: [type] turn ;] execute width>n ; TW5" TEST-EVAL:N 16 T=
   s" : TW6 ( -- n ) [type] turn W1 ; TW6" TEST-EVAL:N 16 T= ;

: FORM-REFUSED ( ^u8 n -- )
   {: a:^ u:n :}
   ARM
   [: a u evaluate-closed ;] 70 TTHROWSQ
   S\" \"code\":\"E-BAD-STORAGE\"" HAS?
   s" [type] names no type storage holds" HAS?
   DISARM ;

: FORM-PINNED ( ^u8 n ^u8 n -- )
   {: a:^ u:n f:^ fu:n :}
   ARM
   [: a u evaluate-closed ;] 70 TTHROWSQ
   f fu HAS?
   DISARM ;

: CASE-TYPE-FORM-REFUSED ( -- )
   s" [type] refuses a type storage cannot hold" T-LABEL
   s" : TX1 ( ^t -- n ) drop [type] t width>n ;" FORM-REFUSED
   s" : TX2 ( ^t -- n ) drop [type] ^t width>n ;" FORM-REFUSED
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
   s" : TX9 [type] ^ ;" FORM-REFUSED
   s" : TX10 ( -- n ) [: [type] ^ ;] drop 1 ;"
   S\" \"token\":\"^\"" FORM-PINNED ;

: CASE-TYPE-FORM-NAME ( -- )
   s" [type] is a keyword no definition is named" T-LABEL
   s" : [type] ;" TEST-EVAL:RC 70 T= ;

: CASE-MEMBER ( -- )
   s" no declaration holds a width member" T-LABEL
   s" STRUCTURE sw1 FIELD w width<n> ;STRUCTURE" DECL-REFUSED
   s" STRUCTURE sw2<a> FIELD w width<a> ;STRUCTURE" DECL-REFUSED
   s" STRUCTURE sw3 FIELD w ^width<n> ;STRUCTURE" DECL-REFUSED
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
