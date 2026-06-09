\ quots.fs — quotation literals [: … ;] and ' / ['] (xt typed as quot). Fills
\ CHECK-QUOT. Combinators (EXECUTE DIP KEEP …) are ordinary DB prims — checked by
\ normal application, not here.

\ Check a quotation body: tokens until ;]  (sub-checks via CHECK-WORD; nests).
: CHECK-QSEG ( -- )
   begin
     B-NEXT 2dup s" ;]" CI= if 2drop exit then
     dup 0= if E-QUOT throw then            \ unterminated
     CHECK-WORD
   again ;

\ [: … ;]  — check the body on a fresh stack, push quot<that effect>.
: DO-QUOT-LIT ( -- )
   DCUR @ RCUR @ {: sd sr :}                 \ save outer current
   1 RV-ALLOC MK-ROW {: qin :}               \ fresh quotation input data row
   1 RV-ALLOC MK-ROW {: qrin :}              \ fresh quotation input return row
   qin DCUR !  qrin RCUR !
   CHECK-QSEG                                \ check body to ;]
   qin  DCUR @  qrin  RCUR @  MK-EFFECT MK-QUOT {: q :}
   sd DCUR !  sr RCUR !                      \ restore outer
   q PUSH-DTYPE ;

\ ' NAME  /  ['] NAME  — push quot< NAME's charted effect >.
: DO-TICK ( -- )
   B-NEXT dup 0= if E-QUOT throw then
   EFFECT-OF dup 0= if E-UNCHECKED throw then
   PARSE-SIG MK-QUOT PUSH-DTYPE ;

: (CHECK-QUOT) ( a u -- f )
   2dup s" [:"  CI= if 2drop DO-QUOT-LIT true exit then
   2dup s" [']" CI= if 2drop DO-TICK     true exit then
   2dup s" '"   CI= if 2drop DO-TICK     true exit then
   2drop false ;
' (CHECK-QUOT) is CHECK-QUOT
