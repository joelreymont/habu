\ build-items.f - a definer builds words of keyword, type, operand and call
\ items: a match over a family of its own answers as the same text does, an
\ if and a begin loop, a construct, a call of a word found where the definer
\ runs, an exact call of PKG:TAIL that a part named PKG of the building
\ package does not capture, and typed locals: two of `n`, one of the family
\ matched, two groups, and one of a parametric family the effect numbers
\ apart from the file. An operand that names no variant, a call of an unknown
\ name, a local outside a group, a read of an index never declared, a local
\ of a type its reader does not take, a read of a local whose block ended, a
\ local whose type is a 24th parameter, refused naming the word, and a local
\ in a text definition are refused, and the word is abandoned. The name a
\ field loan awaits takes the steps its text twin's token takes: the same
\ code and diagnostic.

require src/lib/test.f
require src/lib/c2-memory.f
require src/lib/adt/nullable.f
require src/lib/test/subject.f
require test/decl-diag-capture.f

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

: ITEM ( ptr u8 n n n -- )
   data-base PROVIDED-XT:ITEM-CELL + @ AS-ITEM execute ;
: KW ( ptr u8 n -- ) EFFECT:KEYWORD ;
: OPND ( ptr u8 n -- ) EFFECT:OPERAND ;
: BFAM ( -- EFFECT:type ) s" bitem:bfam" EFFECT:NAMED ;
: BFAM-ITEM ( -- ) s" bfam" BFAM EFFECT:TYPE-ITEM ;

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
   op ITEM-ABI:CALL = if xa xu EFFECT:CALL else xa xu 0 op ITEM then
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

\ : NAME ( n n -- n ) {: a:n b:n :} b a - ;
: MAKE-SUB ( ptr u8 n -- )
   EFFECT:NEW EFFECT:N EFFECT:IN EFFECT:N EFFECT:IN EFFECT:N EFFECT:OUT
   define-word
   s" {:" KW  EFFECT:N EFFECT:LOCAL  EFFECT:N EFFECT:LOCAL  s" :}" KW
   {: a:n b:n :}
   b EFFECT:LOCAL@  a EFFECT:LOCAL@  postpone -
   postpone ; ;

\ : NAME ( bfam -- n ) {: x:bfam :}
\    x match bfam ba of 1 endof bb of 2 endof ;match ;
: MAKE-LOCAL-MATCH ( ptr u8 n -- )
   EFFECT:NEW BFAM EFFECT:IN EFFECT:N EFFECT:OUT define-word
   s" {:" KW  BFAM EFFECT:LOCAL  s" :}" KW  EFFECT:LOCAL@
   s" match" KW  BFAM-ITEM
   s" ba" OPND  s" of" KW  1 literal  s" endof" KW
   s" bb" OPND  s" of" KW  2 literal  s" endof" KW
   s" ;match" KW
   postpone ; ;

: GROUP ( -- n ) s" {:" KW  EFFECT:N EFFECT:LOCAL  s" :}" KW ;

\ : NAME ( n n -- n ) {: a:n :} {: b:n :} a b - ;
: MAKE-GROUPS ( ptr u8 n -- )
   EFFECT:NEW EFFECT:N EFFECT:IN EFFECT:N EFFECT:IN EFFECT:N EFFECT:OUT
   define-word
   GROUP GROUP {: a:n b:n :}
   a EFFECT:LOCAL@  b EFFECT:LOCAL@  postpone -
   postpone ; ;

: BI-SUB-TEXT ( n n -- n ) {: a:n b:n :} b a - ;
: BI-LOCAL-TEXT ( bfam -- n )
   {: x:bfam :}
   x match bfam ba of 1 endof bb of 2 endof ;match ;
: BI-GROUPS-TEXT ( n n -- n ) {: a:n :} {: b:n :} a b - ;

s" BI-SUB" MAKE-SUB
10 3 BI-SUB -7 T=
10 3 BI-SUB 10 3 BI-SUB-TEXT T=

s" BI-LOCAL-MATCH" MAKE-LOCAL-MATCH
BI-A BI-LOCAL-MATCH 1 T=
BI-B BI-LOCAL-MATCH 2 T=
BI-B BI-LOCAL-MATCH BI-B BI-LOCAL-TEXT T=

s" BI-GROUPS" MAKE-GROUPS
10 3 BI-GROUPS -7 T=
10 3 BI-GROUPS 10 3 BI-GROUPS-TEXT T=

\ : NAME ( -- ) l0 ; with the local outside a group.
: MAKE-STRAY ( ptr u8 n -- )
   EFFECT:NEW define-word  EFFECT:N EFFECT:LOCAL drop  postpone ; ;
: BAD-STRAY ( -- n ) [: s" BI-BAD-STRAY" MAKE-STRAY ;] catch ;
BAD-STRAY 70 T=
s" : BI-BAD-STRAY ( -- n ) 5 ;" evaluate-closed
BI-BAD-STRAY 5 T=

\ : NAME ( -- n ) l9 ; with no local declared.
: MAKE-UNREAD ( ptr u8 n -- )
   EFFECT:NEW EFFECT:N EFFECT:OUT define-word  9 EFFECT:LOCAL@  postpone ; ;
: BAD-UNREAD ( -- n ) [: s" BI-BAD-UNREAD" MAKE-UNREAD ;] catch ;
BAD-UNREAD 70 T=
s" : BI-BAD-UNREAD ( -- n ) 6 ;" evaluate-closed
BI-BAD-UNREAD 6 T=

\ ( nullable<a> nullable<b> nullable<c> nullable<d> -- nullable<a> )
\    drop drop drop {: x:nullable<a> :} x ;
\ The local's type is the file's fourth parameter, which the effect numbers
\ first: the local is the first input's type value.
: NUL ( -- EFFECT:type ) s" nullable" EFFECT:NAMED ;
: MAKE-KEEP ( ptr u8 n -- )
   NUL NUL NUL NUL {: b c d t :}
   EFFECT:NEW t EFFECT:IN b EFFECT:IN c EFFECT:IN d EFFECT:IN t EFFECT:OUT
   define-word
   postpone drop  postpone drop  postpone drop
   s" {:" KW  t EFFECT:LOCAL  s" :}" KW  EFFECT:LOCAL@
   postpone ; ;

: NUL-BYTE ( nullable<u8> -- n )
   MATCH nullable
      none OF 0 ENDOF
      some OF c@ ENDOF
   ;MATCH ;
: NONES ( -- nullable<a> nullable<b> nullable<c> )
   NULLABLE:NONE NULLABLE:NONE NULLABLE:NONE ;
: BI-KEEP-TEXT
   ( nullable<a> nullable<b> nullable<c> nullable<d> -- nullable<a> )
   drop drop drop {: x:nullable<a> :} x ;

: KEEP-BUILT ( -- n ) [: s" BI-KEEP" MAKE-KEEP ;] catch ;
KEEP-BUILT 0 T=
s" Q" drop NULLABLE:SOME NONES BI-KEEP NUL-BYTE 81 T=
s" Q" drop NULLABLE:SOME NONES BI-KEEP-TEXT NUL-BYTE 81 T=

\ : NAME ( bfam -- n ) {: x:bfam :} x 1 + ;
: MAKE-WRONG ( ptr u8 n -- )
   EFFECT:NEW BFAM EFFECT:IN EFFECT:N EFFECT:OUT define-word
   s" {:" KW  BFAM EFFECT:LOCAL  s" :}" KW  EFFECT:LOCAL@
   1 literal  postpone +
   postpone ; ;
: BAD-WRONG ( -- n ) [: s" BI-BAD-WRONG" MAKE-WRONG ;] catch ;
BAD-WRONG 70 T=
s" : BI-BAD-WRONG ( -- n ) 7 ;" evaluate-closed
BI-BAD-WRONG 7 T=

\ : NAME ( n bool -- n ) if {: a:n :} a then {: b:n :} a ; with b in the
\ slot a's block left.
: MAKE-ENDED ( ptr u8 n -- )
   EFFECT:NEW EFFECT:N EFFECT:IN s" bool" EFFECT:NAMED EFFECT:IN
   EFFECT:N EFFECT:OUT define-word
   s" if" KW  GROUP {: a:n :}  a EFFECT:LOCAL@  s" then" KW
   GROUP drop  a EFFECT:LOCAL@
   postpone ; ;
: BAD-ENDED ( -- n ) [: s" BI-BAD-ENDED" MAKE-ENDED ;] catch ;
BAD-ENDED 70 T=
s" : BI-BAD-ENDED ( -- n ) 8 ;" evaluate-closed
BI-BAD-ENDED 8 T=

\ : NAME ( nullable<a> ... -- ) {: x:nullable<z> :} drop ... ; with 23
\ inputs, so the local's z is a 24th parameter.
: MAKE-WIDE ( ptr u8 n -- )
   EFFECT:NEW  23 0 ?do NUL EFFECT:IN loop  define-word
   s" {:" KW  NUL EFFECT:LOCAL drop  s" :}" KW
   22 0 ?do postpone drop loop
   postpone ; ;

\ : NAME ( -- ) [ EFFECT:N EFFECT:LOCAL drop ] ; as text, which no
\ define-word effect numbers.
public
: LOCAL-NOW ( -- ) EFFECT:N EFFECT:LOCAL drop ; immediate
private
s" LOCAL-NOW" 0 parse-imm
: BAD-TEXT ( -- n )
   [: s" : BI-BAD-TEXT ( -- ) LOCAL-NOW ;" evaluate-closed ;] catch ;
BAD-TEXT 70 T=
s" : BI-BAD-TEXT ( -- n ) 9 ;" evaluate-closed
BI-BAD-TEXT 9 T=

8192 BUFFER: BUILT-DIAG
variable BUILT-DIAG-U
: KEEP-DIAG ( -- )
   DECL-DIAG:TEXT$ {: a:ptr u:n :}
   a BUILT-DIAG u BYTE-COPY  u BUILT-DIAG-U ! ;
: BUILT-DIAG$ ( -- ptr u8 n ) BUILT-DIAG BUILT-DIAG-U @ ;

\ : NAME ( -- ) C2-MEM:WITH-FIELD inner ; with no view to lend a field of.
: MAKE-LOAN ( ptr u8 n -- )
   EFFECT:NEW define-word
   s" C2-MEM:WITH-FIELD" EFFECT:CALL  s" inner" OPND
   postpone ; ;

: LOAN-BUILT ( -- n ) [: s" BI-LOAN" MAKE-LOAN ;] catch ;
: LOAN-TEXT ( -- n )
   [: s" : BI-LOAN ( -- ) C2-MEM:WITH-FIELD inner ;" evaluate-closed ;] catch ;
DECL-DIAG:PROSE  LOAN-BUILT 70 T=  KEEP-DIAG
DECL-DIAG:PROSE  LOAN-TEXT 70 T=
BUILT-DIAG$ DECL-DIAG:TEXT$ T$=  DECL-DIAG:OFF

public
: WIDE-GO ( -- ) s" BI-WIDE" MAKE-WIDE ;
: TEXT-GO ( -- ) s" : BI-TEXT ( -- ) BITEM:LOCAL-NOW ;" evaluate-closed ;
;package

\ A rejection's text goes to fd 2, which a child's capture reads.
s" BITEM:WIDE-GO" 76
s" define-word has more than 23 type parameters: 'BI-WIDE'"
SUBJECT:REFUSED? TTRUE
s" BITEM:TEXT-GO" 70
s" EFFECT:LOCAL needs a definition define-word opened" SUBJECT:REFUSED? TTRUE

T-REPORT
