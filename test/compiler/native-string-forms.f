\ Preserve string behavior through the checker reader and the native source tape.
\ Each subject program starts with `0 set-tier`, so tier 0 (src/habu/t0.f)
\ compiles it whatever tier this test runs at.
require test/gate-common.f

package NSTRING-FORMS-TEST

255 constant COUNTED-MAX
create BOUNDARY COUNTED-MAX 1+ allot

: TIER-LINE ( -- )
   s" 0 set-tier" GE-SRC-LINE ;

: BOUNDARY! ( -- )
   COUNTED-MAX 1+ 0 ?do [char] A BOUNDARY i + c! loop ;


: SOURCE-START ( -- )
   GE-SRC-RESET
   TIER-LINE
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
   TIER-LINE
   s" package STRING-FORM-SUBJECT" GE-SRC-LINE
   S\" : STACK ( n -- n ) .\q stack\q ;" GE-SRC-LINE
   S\" : LOCAL ( n -- n ) {: v:n :} .\q local\q v ;" GE-SRC-LINE
   s" : RUN ( -- ) 7 STACK LOCAL . ; RUN" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;


: CARRIED-CASE ( -- )
   CARRIED-SOURCE
   S\" stacklocal7\n" CHECK-OUTPUT ;


\ `[package]` compiles the open package's name, folded: in the package, in its
\ part, in a definition published into the part (`: PART:QUALIFIED`, which names
\ the open package's record, not the wordlist it lands in), after a reopening,
\ and empty outside any package.
: PACKAGE-SOURCE ( -- )
   GE-SRC-RESET
   TIER-LINE
   s" package STRING-FORM-SUBJECT" GE-SRC-LINE
   s" : OPEN-NAME ( -- ) [package] type ;" GE-SRC-LINE
   s" package PART" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : NAME ( -- ) [package] type ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" : PART:QUALIFIED ( -- ) [package] type [char] | emit ;" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : SHOW ( -- ) OPEN-NAME [char] | emit PART:NAME [char] | emit PART:QUALIFIED ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" package STRING-FORM-SUBJECT" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : REOPENED ( -- ) [package] type [char] | emit ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" : OUTSIDE ( -- ) [package] type [char] | emit ;" GE-SRC-LINE
   s" STRING-FORM-SUBJECT:SHOW STRING-FORM-SUBJECT:REOPENED OUTSIDE" GE-SRC-LINE ;


: PACKAGE-CASE ( -- )
   PACKAGE-SOURCE
   s" string-form-subject|string-form-subject:part|string-form-subject|string-form-subject||"
   CHECK-OUTPUT ;


: TOO-LONG ( ptr u8 n -- )
   BOUNDARY COUNTED-MAX 1+ COUNTED-SOURCE
   76 s" counted string too long" s" native counted string bound" GE-EVAL-FORK-BAD ;


: EVAL-FLOOR ( -- ptr u8 )
   data-base S0-CELL + 0 ptr-field @ ;


\ Source evaluation owns an isolated stack and is closed: a source that leaves
\ results is refused there with E-EVAL-RESIDUE, and neither those results nor
\ the stack allocation may leak into this caller.
: CHECK-EVAL-CALLER ( ptr u8 n n -- ) {: source:ptr size:n want:n :}
   source size GE-EVAL-SRC!
   depth {: before:n :}
   EVAL-FLOOR {: old:ptr :}
   [: GE-EVAL-SOURCE ;] catch {: rc:n :}
   rc want <> if s" isolated evaluation result" GE-FAIL then
   depth before <> if s" isolated evaluation caller stack" GE-FAIL then
   EVAL-FLOOR old <> if s" isolated evaluation caller floor" GE-FAIL then ;


: EVAL-CALLER-CASES ( -- )
   s" " 0 CHECK-EVAL-CALLER
   s" 11 22" E-EVAL-RESIDUE CHECK-EVAL-CALLER
   s" 7 throw" 7 CHECK-EVAL-CALLER
   s" 33 44" E-EVAL-RESIDUE CHECK-EVAL-CALLER ;


: FORM-CASES ( -- )
   PRINTED-CASES COUNTED-CASES COUNTED-BOUNDARY
   CARRIED-CASE PACKAGE-CASE ;


: BODY ( -- )
   s" native-string-forms" GT-START
   EVAL-CALLER-CASES
   FORM-CASES
   S\" c\q" TOO-LONG
   S\" c\\\q" TOO-LONG
   s" PASS: native counted and printed strings" type cr ;


public

: RUN ( -- )
   [: BODY ;] [: GT-CLEANUP ;] finally ;

;package

NSTRING-FORMS-TEST:RUN
