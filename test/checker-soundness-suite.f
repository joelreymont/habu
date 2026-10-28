\ checker-soundness-suite.f - a certified body must not exceed its declared effect.
\ Each case pins one way a body used to launder a value past its contract:
\ a quotation argument consuming a narrower input than the parameter supplies,
\ a return-row read or pop below the frame this definition may see, a
\ declared row unified with another or given fixed cells, and a signature with
\ tokens after its output side.

require src/lib/test.f

package CHECKER-SOUNDNESS
public

PRODUCT point FIELD x n FIELD y n ;PRODUCT

: TAKE-U8 ( u8 -- ) drop ;
: TAKE-I64 ( i64 -- ) drop ;
: MK-I64 ( -- i64 ) 300 ;
: MK-U8 ( -- u8 ) 3 ;
: CALL-I64 ( [ i64 -- ] -- ) MK-I64 swap execute ;
: CALL-U8 ( [ u8 -- ] -- ) MK-U8 swap execute ;
: CALL-I64-Q ( [ [ i64 -- ] -- ] -- ) ['] TAKE-I64 swap execute ;
: CALL-U8-Q ( [ [ u8 -- ] -- ] -- ) ['] TAKE-U8 swap execute ;

\ CHECK-QUIET-CANDIDATE!: -1 certified, 0 rejected, 1 uncheckable.
: QUOTATION-INPUTS ( -- )
   s" a quotation consuming a narrower input does not satisfy a wider parameter" T-LABEL
   s" CS-Q1 ( -- ) [: CHECKER-SOUNDNESS:TAKE-U8 ;] CHECKER-SOUNDNESS:CALL-I64"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" a quotation consuming a wider input satisfies a narrower parameter" T-LABEL
   s" CS-Q2 ( -- ) [: CHECKER-SOUNDNESS:TAKE-I64 ;] CHECKER-SOUNDNESS:CALL-U8"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" an exact quotation input still passes" T-LABEL
   s" CS-Q3 ( -- ) [: CHECKER-SOUNDNESS:TAKE-I64 ;] CHECKER-SOUNDNESS:CALL-I64"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" a nested quotation input reverses the direction twice" T-LABEL
   s" CS-Q6 ( -- ) [: CHECKER-SOUNDNESS:CALL-U8 ;] CHECKER-SOUNDNESS:CALL-I64-Q"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-Q7 ( -- ) [: CHECKER-SOUNDNESS:CALL-I64 ;] CHECKER-SOUNDNESS:CALL-U8-Q"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" the accepted shapes execute" T-LABEL
   [: TAKE-I64 ;] CALL-U8
   [: CALL-U8 ;] CALL-I64-Q
   depth 0 T= ;

: RETURN-ROW ( -- )
   s" a read below the definition's own return frame is refused" T-LABEL
   s" CS-R1 ( -- n ) r@" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-R2 ( -- n n ) 2r@" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-R3 ( n -- n n n ) >r 2r@ r>" CHECK-QUIET-CANDIDATE! 0 T=
   s" a pop below the frame is a borrow even when the balance is restored" T-LABEL
   s" CS-R4 ( -- n ) r> dup >r" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-R5 ( -- n n ) 2r> 2dup 2>r" CHECK-QUIET-CANDIDATE! 0 T=
   s" a quotation cannot borrow on the definition's behalf" T-LABEL
   s" CS-R6 ( -- n ) [: r> dup >r ;] execute" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-R7 ( n -- n ) [: drop r@ ;] catch drop" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-R7B ( n -- n ) [: 1 + ;] catch drop" CHECK-QUIET-CANDIDATE! -1 T=
   s" a product below the frame cannot be borrowed by a pair read" T-LABEL
   s" CS-R8 ( CHECKER-SOUNDNESS:point -- n CHECKER-SOUNDNESS:point ) >r 2r@ r> drop"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" cells this definition pushed may be read" T-LABEL
   s" CS-R9 ( n -- n n ) >r r@ r>" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-R10 ( n n -- n n n n ) 2>r 2r@ 2r>" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-R11 ( n -- n n ) >r begin r@ 0= until r@ r>" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-R12 ( CHECKER-SOUNDNESS:point -- CHECKER-SOUNDNESS:point CHECKER-SOUNDNESS:point ) >r r@ r>"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" a signature may not declare a return cell" T-LABEL
   s" CS-R13 ( R | S n -- R n | S n ) r@" CHECK-QUIET-CANDIDATE! 0 T= ;

\ An R-to-S quotation is the only way to S: a body that never runs it on some
\ path unifies the declared rows, and the caller could pick any S.
: ROW-QUANTIFIERS ( -- )
   s" a body that drops its R-to-S quotation is refused" T-LABEL
   s" CS-W1 ( R [ R -- S ] -- S ) drop" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-W2 ( R CHECKER-SOUNDNESS:point [ R CHECKER-SOUNDNESS:point -- S CHECKER-SOUNDNESS:point ] -- S CHECKER-SOUNDNESS:point ) drop"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-W3 ( R f [ R -- S ] -- S ) swap IF execute ELSE drop THEN"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-W4 ( R [ R -- S ] [ S -- T ] -- T ) nip execute" CHECK-QUIET-CANDIDATE! 0 T=
   s" a declared row stays distinct from the unnamed row" T-LABEL
   s" CS-W5 ( R n -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
   s" a declared row is not given fixed cells" T-LABEL
   s" CS-W6 ( R -- S ) 1" CHECK-QUIET-CANDIDATE! 0 T=
   s" the unnamed row is not given fixed cells either" T-LABEL
   s" CS-W10 ( R n [ R n -- ] -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-W11 ( R f [ R -- ] -- ) swap IF execute ELSE drop 1 THEN"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-W12 ( R [ R -- ] -- ) drop 1" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-W13 ( R -- ) 1" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-W14 ( R n -- )" CHECK-QUIET-CANDIDATE! 0 T=
   s" running the quotation on every path keeps the rows distinct" T-LABEL
   s" CS-W7 ( R [ R -- S ] -- S ) execute" CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-W8 ( R f [ R -- S ] -- S ) swap IF execute ELSE execute THEN"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-W9 ( R [ R -- S ] [ S -- R ] -- R ) drop drop" CHECK-QUIET-CANDIDATE! -1 T= ;

: SIGNATURE-END ( -- )
   s" tokens after the output side are a syntax error" T-LABEL
   s" CS-S1 ( n -- n -- n n ) dup" CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-S2 ( n -- n n ) dup" CHECK-QUIET-CANDIDATE! -1 T=
   s" a quotation effect inside the signature still closes cleanly" T-LABEL
   s" CS-S3 ( n [ n -- n ] -- n ) execute" CHECK-QUIET-CANDIDATE! -1 T= ;

: RUN ( -- )
   T-RESET
   QUOTATION-INPUTS
   RETURN-ROW
   ROW-QUANTIFIERS
   SIGNATURE-END
   T-REPORT ;

RUN
;package
