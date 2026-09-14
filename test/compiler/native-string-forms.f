\ Preserve string behavior through the checker reader and the native source tape.
require test/gate-common.f

package NSTRING-FORMS-TEST

255 constant COUNTED-MAX
create BOUNDARY COUNTED-MAX 1+ allot

: BOUNDARY! ( -- )
   COUNTED-MAX 1+ 0 ?do [char] A BOUNDARY i + c! loop ;


: SOURCE-START ( -- )
   GE-SRC-RESET
   s" 1 set-tier" GE-SRC-LINE
   s" package STRING-FORM-SUBJECT" GE-SRC-LINE
   s" : RUN ( -- ) " GE-SRC+ ;


: QUOTED ( ptr u8 n ptr u8 n -- )
   {: opener:ptr openeru:n body:ptr bodyu:n :}
   opener openeru GE-SRC+ GE-SRC-SP
   body bodyu GE-SRC+ GE-DQ GE-SRC-C ;


: SOURCE-END ( -- )
   s"  ;" GE-SRC-LINE
   s" RUN" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;


: CHECK-OUTPUT ( ptr u8 n -- ) {: expected:ptr expectedu:n :}
   GE-EVAL-FORK-CAPTURE
   s" native string form compiles and runs" GE-EXPECT-OK
   expected expectedu s" native string form exact bytes" GE-EXPECT-OUT ;


: PRINTED ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: opener:ptr openeru:n body:ptr bodyu:n expected:ptr expectedu:n :}
   SOURCE-START
   opener openeru body bodyu QUOTED
   SOURCE-END
   expected expectedu CHECK-OUTPUT ;


: COUNTED-SOURCE ( ptr u8 n ptr u8 n -- )
   SOURCE-START QUOTED
   s"  count type" GE-SRC+
   SOURCE-END ;


: COUNTED ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: opener:ptr openeru:n body:ptr bodyu:n expected:ptr expectedu:n :}
   opener openeru body bodyu COUNTED-SOURCE
   expected expectedu CHECK-OUTPUT ;


: PRINTED-CASES ( -- )
   S\" .\q" s" hi" s" hi" PRINTED
   S\" .\q" s" " s" " PRINTED
   S\" .\\\q" S\" a\\tb\\q" S\" a\tb\q" PRINTED
   S\" .\\\q" s" " s" " PRINTED ;


: COUNTED-CASES ( -- )
   S\" c\q" s" ok" s" ok" COUNTED
   S\" c\q" s" " s" " COUNTED
   S\" c\\\q" S\" a\\tb\\q" S\" a\tb\q" COUNTED
   S\" c\\\q" s" " s" " COUNTED ;


: COUNTED-BOUNDARY ( -- )
   BOUNDARY!
   S\" c\q" BOUNDARY COUNTED-MAX BOUNDARY COUNTED-MAX COUNTED
   S\" c\\\q" BOUNDARY COUNTED-MAX BOUNDARY COUNTED-MAX COUNTED ;


: CARRIED-SOURCE ( -- )
   GE-SRC-RESET
   s" 1 set-tier" GE-SRC-LINE
   s" package STRING-FORM-SUBJECT" GE-SRC-LINE
   S\" : STACK ( n -- n ) .\q stack\q ;" GE-SRC-LINE
   S\" : LOCAL ( n -- n ) {: v:n :} .\q local\q v ;" GE-SRC-LINE
   s" : RUN ( -- ) 7 STACK LOCAL . ; RUN" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;


: CARRIED-CASE ( -- )
   CARRIED-SOURCE
   S\" stacklocal7\n" CHECK-OUTPUT ;


: TOO-LONG ( ptr u8 n -- )
   BOUNDARY COUNTED-MAX 1+ COUNTED-SOURCE
   76 s" counted string too long" s" native counted string bound" GE-EVAL-FORK-BAD ;


: BODY ( -- )
   s" native-string-forms" GT-START
   PRINTED-CASES COUNTED-CASES COUNTED-BOUNDARY
   CARRIED-CASE
   S\" c\q" TOO-LONG
   S\" c\\\q" TOO-LONG
   s" PASS: native counted and printed strings" type cr ;


public

: RUN ( -- )
   [: BODY ;] [: GT-CLEANUP ;] finally ;

;package

NSTRING-FORMS-TEST:RUN
