\ build-items.f - a definer builds words of keyword, type, operand and call
\ items: a match over a family of its own answers as the same text does, an
\ if and a begin loop, a construct, a call of a word found where the definer
\ runs, and an exact call of PKG:TAIL that a part named PKG of the building
\ package does not capture. An operand that names no variant and a call of an
\ unknown name are refused, and the word is abandoned.

require src/lib/test.f

package BITEM-PKG
public
: ANSWER ( -- n ) 1 ;
;package

package BITEM
private

package BITEM-PKG
public
: ANSWER ( -- n ) 2 ;
;package

ENUM bfam ba bb ;ENUM

: BI-SEVEN ( -- n ) 7 ;

\ The engine's cell holds INTERPRETER:ITEM-RUN as a raw execution token.
CAST: AS-ITEM ( n -- [ ptr u8 n n n -- ] )
CAST: TYPE>N ( EFFECT:type -- n )

: ITEM ( ptr u8 n n n -- )
   data-base PROVIDED-XT:ITEM-CELL + @ AS-ITEM execute ;
: KW ( ptr u8 n -- ) 0 ITEM-ABI:KEYWORD ITEM ;
: OPND ( ptr u8 n -- ) 0 ITEM-ABI:OPERAND ITEM ;
: BFAM ( -- EFFECT:type ) s" bitem:bfam" EFFECT:FAMILY ;
: BFAM-ITEM ( -- ) s" bfam" BFAM TYPE>N ITEM-ABI:TYPE ITEM ;

\ : NAME ( bfam -- n ) match bfam ba of 1 endof X of 2 endof ;match ;
: MAKE-MATCH ( ptr u8 n ptr u8 n -- ) {: xa:ptr xu:n :}
   EFFECT:NEW BFAM EFFECT:IN EFFECT:N EFFECT:OUT define-word
   s" match" KW  BFAM-ITEM
   s" ba" OPND  s" of" KW  1 literal  s" endof" KW
   xa xu OPND  s" of" KW  2 literal  s" endof" KW
   s" ;match" KW
   postpone ; ;

\ : NAME ( n -- n )
\    dup 0 > if begin 1 - dup 3 < until 100 + else drop 20 then ;
: MAKE-FLOW ( ptr u8 n -- )
   EFFECT:NEW EFFECT:N EFFECT:IN EFFECT:N EFFECT:OUT define-word
   postpone dup  0 literal  postpone >  s" if" KW
   s" begin" KW  1 literal  postpone -  postpone dup  3 literal  postpone <
   s" until" KW  100 literal  postpone +
   s" else" KW  postpone drop  20 literal  s" then" KW
   postpone ; ;

\ : NAME ( -- bfam ) construct bfam bb ;
: MAKE-CONSTRUCT ( ptr u8 n -- )
   EFFECT:NEW BFAM EFFECT:OUT define-word
   s" construct" KW  BFAM-ITEM  s" bb" OPND
   postpone ; ;

\ : NAME ( -- n ) X ; with X found by OP.
: MAKE-CALL ( ptr u8 n ptr u8 n n -- ) {: xa:ptr xu:n op:n :}
   EFFECT:NEW EFFECT:N EFFECT:OUT define-word
   xa xu 0 op ITEM
   postpone ; ;

: BI-TEXT ( bfam -- n ) match bfam ba of 1 endof bb of 2 endof ;match ;
: BI-A ( -- bfam ) construct bfam ba ;
: BI-B ( -- bfam ) construct bfam bb ;

s" BI-MATCH" s" bb" MAKE-MATCH
BI-A BI-MATCH 1 T=
BI-B BI-MATCH 2 T=
BI-A BI-MATCH BI-A BI-TEXT T=
BI-B BI-MATCH BI-B BI-TEXT T=

s" BI-FLOW" MAKE-FLOW
10 BI-FLOW 102 T=
-5 BI-FLOW 20 T=

s" BI-MADE" MAKE-CONSTRUCT
BI-MADE BI-TEXT 2 T=

s" BI-CALL" s" BI-SEVEN" ITEM-ABI:CALL MAKE-CALL
BI-CALL 7 T=

\ The text binds the part's ANSWER, the exact call the top-level package's.
: BI-PART ( -- n ) BITEM-PKG:ANSWER ;
s" BI-EXACT" s" BITEM-PKG:ANSWER" ITEM-ABI:CALL-EXACT MAKE-CALL
BI-PART 2 T=
BI-EXACT 1 T=

\ Each refused name is free after its refusal: a text definition takes it.
: BAD-OPERAND ( -- n ) [: s" BI-BAD-OPERAND" s" bz" MAKE-MATCH ;] catch ;
BAD-OPERAND 70 T=
s" : BI-BAD-OPERAND ( -- n ) 3 ;" evaluate-closed
BI-BAD-OPERAND 3 T=

: BAD-CALL ( -- n )
   [: s" BI-BAD-CALL" s" BI-NO-SUCH" ITEM-ABI:CALL MAKE-CALL ;] catch ;
BAD-CALL 70 T=
s" : BI-BAD-CALL ( -- n ) 4 ;" evaluate-closed
BI-BAD-CALL 4 T=
;package

T-REPORT
