\ build-items.f - a definer builds words of keyword, type, operand and call
\ items: a match over a family of its own answers as the same text does, an if
\ and a begin loop, a construct, a call of a word found where the definer runs,
\ an exact call of PKG:TAIL that a part named PKG of the building package does
\ not capture, and typed locals: two of `n`, one of the family matched, two
\ groups, and one of a parametric family the effect numbers apart from the
\ file. A variable a definer declares of a quotation type holds a quotation of
\ that type, and a store of another is rejected; one of a type that may hold a
\ linear value is rejected, the type named. An operand that names no variant, a
\ call of an unknown name, a local outside a group, a read of an index never
\ declared, a local of a type its reader does not take, a read of a local whose
\ block ended, rejected naming the local's index, a local whose type is a 24th
\ parameter, rejected naming the word, and a local or a local's read in a text
\ definition are rejected, and the word is abandoned. The name a field loan
\ awaits takes the steps its text twin's token takes: the same code and
\ diagnostic. Inside an open match an item's kind decides its role: a call in
\ an abandoned one is skipped, a call where the family belongs and a type where
\ a variant belongs get the match's rejection. An item spelled by no token or
\ by two is rejected. EFFECT:READ-TYPE reads a linear and a width type as a
\ signature does, and rejects a letter, a type variable's or a view's region's,
\ at the line that ran the definer, with the fix, as EFFECT:NAMED and
\ EFFECT:READ-TYPE reject an unknown family, which a definer names as source
\ in its package would, and EFFECT:IN a type that is no effect. A build of a
\ program whose source stores a definer's type is rejected with the record its
\ load prints.

require src/lib/test.f
require src/lib/c2-memory.f
require src/lib/adt/nullable.f
require src/lib/test/subject.f
require src/lib/test/ending.f
require test/decl-diag-capture.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-command.f
require src/lib/engine-candidate.f

package BITEM-PKG
public
: ANSWER ( -- n ) 1 ;
;package

package BITEM-OWN
private
SUMTYPE secret VARIANT hid n ;VARIANT ;SUMTYPE
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
   EFFECT:EMPTY BFAM EFFECT:IN EFFECT:N EFFECT:OUT define-word
   s" match" KW  BFAM-ITEM
   s" ba" OPND  s" of" KW  1 literal  s" endof" KW
   xa xu OPND  s" of" KW  2 literal  s" endof" KW
   s" ;match" KW
   postpone ; ;

\ : NAME ( n -- n )
\    dup 0 > if begin 1 - dup 3 < until 100 + else drop 20 then ;
: MAKE-FLOW ( ptr u8 n -- )
   EFFECT:EMPTY EFFECT:N EFFECT:IN EFFECT:N EFFECT:OUT define-word
   postpone dup  0 literal  postpone >  s" if" KW
   s" begin" KW  1 literal  postpone -  postpone dup  3 literal  postpone <
   s" until" KW  100 literal  postpone +
   s" else" KW  postpone drop  20 literal  s" then" KW
   postpone ; ;

\ : NAME ( -- bfam ) construct bfam bb ;
: MAKE-CONSTRUCT ( ptr u8 n -- )
   EFFECT:EMPTY BFAM EFFECT:OUT define-word
   s" construct" KW  BFAM-ITEM  s" bb" OPND
   postpone ; ;

\ : NAME ( -- n ) X ; with X found by OP.
: MAKE-CALL ( ptr u8 n ptr u8 n n -- ) {: xa:ptr xu:n op:n :}
   EFFECT:EMPTY EFFECT:N EFFECT:OUT define-word
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
   EFFECT:EMPTY EFFECT:N EFFECT:IN EFFECT:N EFFECT:IN EFFECT:N EFFECT:OUT
   define-word
   s" {:" KW  EFFECT:N EFFECT:LOCAL  EFFECT:N EFFECT:LOCAL  s" :}" KW
   {: a:n b:n :}
   b EFFECT:LOCAL@  a EFFECT:LOCAL@  postpone -
   postpone ; ;

\ : NAME ( bfam -- n ) {: x:bfam :}
\    x match bfam ba of 1 endof bb of 2 endof ;match ;
: MAKE-LOCAL-MATCH ( ptr u8 n -- )
   EFFECT:EMPTY BFAM EFFECT:IN EFFECT:N EFFECT:OUT define-word
   s" {:" KW  BFAM EFFECT:LOCAL  s" :}" KW  EFFECT:LOCAL@
   s" match" KW  BFAM-ITEM
   s" ba" OPND  s" of" KW  1 literal  s" endof" KW
   s" bb" OPND  s" of" KW  2 literal  s" endof" KW
   s" ;match" KW
   postpone ; ;

: GROUP ( -- n ) s" {:" KW  EFFECT:N EFFECT:LOCAL  s" :}" KW ;

\ : NAME ( n n -- n ) {: a:n :} {: b:n :} a b - ;
: MAKE-GROUPS ( ptr u8 n -- )
   EFFECT:EMPTY EFFECT:N EFFECT:IN EFFECT:N EFFECT:IN EFFECT:N EFFECT:OUT
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
   EFFECT:EMPTY define-word  EFFECT:N EFFECT:LOCAL drop  postpone ; ;
: BAD-STRAY ( -- n ) [: s" BI-BAD-STRAY" MAKE-STRAY ;] catch ;
BAD-STRAY 70 T=
s" : BI-BAD-STRAY ( -- n ) 5 ;" evaluate-closed
BI-BAD-STRAY 5 T=

\ : NAME ( -- n ) l9 ; with no local declared.
: MAKE-UNREAD ( ptr u8 n -- )
   EFFECT:EMPTY EFFECT:N EFFECT:OUT define-word  9 EFFECT:LOCAL@  postpone ; ;
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
   EFFECT:EMPTY t EFFECT:IN b EFFECT:IN c EFFECT:IN d EFFECT:IN t EFFECT:OUT
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
   EFFECT:EMPTY BFAM EFFECT:IN EFFECT:N EFFECT:OUT define-word
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
   EFFECT:EMPTY EFFECT:N EFFECT:IN s" bool" EFFECT:NAMED EFFECT:IN
   EFFECT:N EFFECT:OUT define-word
   s" if" KW  GROUP {: a:n :}  a EFFECT:LOCAL@  s" then" KW
   GROUP drop  a EFFECT:LOCAL@
   postpone ; ;
: BAD-ENDED ( -- n ) [: s" BI-BAD-ENDED" MAKE-ENDED ;] catch ;
BAD-ENDED 70 T=
s" : BI-BAD-ENDED ( -- n ) 8 ;" evaluate-closed
BI-BAD-ENDED 8 T=

\ : NAME ( n -- n ) {: a:n :} l-1 ; inside a's block.
: MAKE-LNEG ( ptr u8 n -- )
   EFFECT:EMPTY EFFECT:N EFFECT:IN EFFECT:N EFFECT:OUT define-word
   GROUP drop  -1 EFFECT:LOCAL@
   postpone ; ;

\ : NAME ( nullable<a> ... -- ) {: x:nullable<z> :} drop ... ; with 23
\ inputs, so the local's z is a 24th parameter.
: MAKE-WIDE ( ptr u8 n -- )
   EFFECT:EMPTY  23 0 ?do NUL EFFECT:IN loop  define-word
   s" {:" KW  NUL EFFECT:LOCAL drop  s" :}" KW
   22 0 ?do postpone drop loop
   postpone ; ;

\ : NAME ( -- ) [ EFFECT:N EFFECT:LOCAL drop ] ; as text, which no
\ define-word effect numbers; LOCAL-READ-NOW reads local 0 the same way.
public
: LOCAL-NOW ( -- ) EFFECT:N EFFECT:LOCAL drop ; immediate
: LOCAL-READ-NOW ( -- ) 0 EFFECT:LOCAL@ ; immediate
private
s" LOCAL-NOW" 0 parse-imm
s" LOCAL-READ-NOW" 0 parse-imm
: BAD-TEXT ( -- n )
   [: s" : BI-BAD-TEXT ( -- ) LOCAL-NOW ;" evaluate-closed ;] catch ;
BAD-TEXT 70 T=
s" : BI-BAD-TEXT ( -- n ) 9 ;" evaluate-closed
BI-BAD-TEXT 9 T=

\ TYPED-VARIABLE NAME [ n -- n ]
: MAKE-QVAR ( ptr u8 n -- )
   EFFECT:EMPTY EFFECT:N EFFECT:IN EFFECT:N EFFECT:OUT EFFECT:VARIABLE-OF ;

s" BI-QVAR" MAKE-QVAR
: BI-QSTORE ( -- ) [: 1 + ;] BI-QVAR ! ;
BI-QSTORE
5 BI-QVAR @ execute 6 T=
: BAD-QSTORE ( -- n )
   [: s" : BI-BAD-QSTORE ( -- ) [: dup ;] BI-QVAR ! ;" evaluate-closed ;] catch ;
BAD-QSTORE 70 T=
5 BI-QVAR @ execute 6 T=

\ TYPED-VARIABLE NAME [ n -- option<a> ]
: MAKE-OVAR ( ptr u8 n -- )
   EFFECT:EMPTY EFFECT:N EFFECT:IN s" option" EFFECT:NAMED EFFECT:OUT
   EFFECT:VARIABLE-OF ;
: BAD-OVAR ( -- n ) [: s" BI-BAD-OVAR" MAKE-OVAR ;] catch ;
DECL-DIAG:PROSE  BAD-OVAR 70 T=
s" '[ n -- option<a> ]'" DECL-DIAG:HAS? TTRUE  DECL-DIAG:OFF

8192 BUFFER: BUILT-DIAG
variable BUILT-DIAG-U
: KEEP-DIAG ( -- )
   DECL-DIAG:TEXT$ {: a:ptr u:n :}
   a BUILT-DIAG u BYTE-COPY  u BUILT-DIAG-U ! ;
: BUILT-DIAG$ ( -- ptr u8 n ) BUILT-DIAG BUILT-DIAG-U @ ;

\ : NAME ( -- ) C2-MEM:WITH-FIELD inner ; with no view to lend a field of.
: MAKE-LOAN ( ptr u8 n -- )
   EFFECT:EMPTY define-word
   s" C2-MEM:WITH-FIELD" EFFECT:CALL  s" inner" OPND
   postpone ; ;

: LOAN-BUILT ( -- n ) [: s" BI-LOAN" MAKE-LOAN ;] catch ;
: LOAN-TEXT ( -- n )
   [: s" : BI-LOAN ( -- ) C2-MEM:WITH-FIELD inner ;" evaluate-closed ;] catch ;
DECL-DIAG:PROSE  LOAN-BUILT 70 T=  KEEP-DIAG
DECL-DIAG:PROSE  LOAN-TEXT 70 T=
BUILT-DIAG$ DECL-DIAG:TEXT$ T$=  DECL-DIAG:OFF

\ : NAME ( bfam -- n ) match zz ba of evaluate endof ;match ; whose family
\ resolves nothing: the call is skipped through `;match` as its text is.
: MAKE-ABANDONED ( ptr u8 n -- )
   EFFECT:EMPTY BFAM EFFECT:IN EFFECT:N EFFECT:OUT define-word
   s" match" KW  s" zz" EFFECT:N EFFECT:TYPE-ITEM
   s" ba" OPND  s" of" KW  s" evaluate" EFFECT:CALL  s" endof" KW
   s" ;match" KW
   postpone ; ;
: ABANDONED ( -- n ) [: s" BI-ABANDONED" MAKE-ABANDONED ;] catch ;
DECL-DIAG:PROSE  ABANDONED 70 T=
s" bad match: unknown type family" DECL-DIAG:HAS? TTRUE
s" unsafe" DECL-DIAG:HAS? TFALSE  DECL-DIAG:OFF

\ : NAME ( bfam -- n ) match BFAM ba of 1 endof bb of 2 endof ;match ;
\ with BFAM a call: a call names no family, though a family spells it.
: MAKE-CALL-FAMILY ( ptr u8 n -- )
   EFFECT:EMPTY BFAM EFFECT:IN EFFECT:N EFFECT:OUT define-word
   s" match" KW  s" BFAM" EFFECT:CALL
   s" ba" OPND  s" of" KW  1 literal  s" endof" KW
   s" bb" OPND  s" of" KW  2 literal  s" endof" KW
   s" ;match" KW
   postpone ; ;
: CALL-FAMILY ( -- n ) [: s" BI-CALL-FAMILY" MAKE-CALL-FAMILY ;] catch ;
DECL-DIAG:PROSE  CALL-FAMILY 70 T=
s" bad match: unknown type family" DECL-DIAG:HAS? TTRUE  DECL-DIAG:OFF

\ : NAME ( bfam -- n ) match bfam zz of 1 endof bb of 2 endof ;match ; with
\ zz a type where the variant belongs: the match's own rejection.
: MAKE-TYPE-VARIANT ( ptr u8 n -- )
   EFFECT:EMPTY BFAM EFFECT:IN EFFECT:N EFFECT:OUT define-word
   s" match" KW  BFAM-ITEM
   s" zz" BFAM EFFECT:TYPE-ITEM  s" of" KW  1 literal  s" endof" KW
   s" bb" OPND  s" of" KW  2 literal  s" endof" KW
   s" ;match" KW
   postpone ; ;
: TYPE-VARIANT ( -- n ) [: s" BI-TYPE-VARIANT" MAKE-TYPE-VARIANT ;] catch ;
DECL-DIAG:PROSE  TYPE-VARIANT 70 T=
s" bad match: unknown variant" DECL-DIAG:HAS? TTRUE  DECL-DIAG:OFF

\ : NAME ( -- n ) 5 S ; with S an item spelled by the string: an item's
\ spelling is one token, else it is rejected as define-word's name is, 74.
: MAKE-SPELLED ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n :}
   EFFECT:EMPTY EFFECT:N EFFECT:OUT define-word
   5 literal  sa su KW
   postpone ; ;
: EMPTY-ITEM ( -- n ) [: s" BI-EMPTY-ITEM" s" " MAKE-SPELLED ;] catch ;
: BLANK-ITEM ( -- n )
   [: s" BI-BLANK-ITEM" s" 1 drop" MAKE-SPELLED ;] catch ;
EMPTY-ITEM 74 T=
BLANK-ITEM 74 T=
s" : BI-BLANK-ITEM ( -- n ) 8 ;" evaluate-closed
BI-BLANK-ITEM 8 T=

public
DEFLINEAR BITEM:ltok
private
LINEAR: LTOK ( n -- BITEM:ltok )
LINEAR: LTOK>N ( BITEM:ltok -- n )
: BI-U8-WIDTH ( -- width<u8> ) [type] u8 ;

\ : NAME ( t -- t ) ;
: MAKE-ID ( ptr u8 n EFFECT:type -- ) {: t :}
   EFFECT:EMPTY t EFFECT:IN  t EFFECT:OUT  define-word  postpone ; ;

SUMTYPE blin VARIANT owned BITEM:ltok ;VARIANT ;SUMTYPE
: BI-LIN-MAKE ( n -- blin ) LTOK construct blin owned ;
: BI-LIN-N ( blin -- n ) match blin owned of LTOK>N endof ;match ;

s" BI-LIN" s" blin" EFFECT:READ-TYPE MAKE-ID
: BI-LIN-RUN ( -- n ) 5 BI-LIN-MAKE BI-LIN BI-LIN-N ;
BI-LIN-RUN 5 T=
s" BI-WIDTH" s" width<u8>" EFFECT:READ-TYPE MAKE-ID
BI-U8-WIDTH BI-WIDTH width>n 1 T=

: PTR-A ( -- n ) [: s" BI-PTR-A" s" ptr a" EFFECT:READ-TYPE MAKE-ID ;] catch ;
: VIEW ( -- n )
   [: s" BI-VIEW" s" read-view<p,q,u8>" EFFECT:READ-TYPE MAKE-ID ;] catch ;
: LETTER-FIX ( -- )
   s" = help: a built definition's types are concrete: write one such as n or ptr u8"
   DECL-DIAG:HAS? TTRUE ;
: NAMED-BAD ( -- n ) [: s" no-such-family" EFFECT:NAMED drop ;] catch ;
: IN-BAD ( -- n ) [: EFFECT:N EFFECT:N EFFECT:IN drop ;] catch ;
: READ-BAD ( -- n )
   [: s" BI-UNK" s" no-such-family" EFFECT:READ-TYPE MAKE-ID ;] catch ;
: SECRET ( -- EFFECT:type ) s" bitem-own:secret" EFFECT:NAMED ;

\ : NAME ( bitem-own:secret -- n ) match secret hid of endof ;match ;
: MAKE-PEEK ( ptr u8 n -- )
   EFFECT:EMPTY SECRET EFFECT:IN EFFECT:N EFFECT:OUT define-word
   s" match" KW  s" secret" SECRET EFFECT:TYPE-ITEM
   s" hid" OPND  s" of" KW  s" endof" KW  s" ;match" KW
   postpone ; ;
: PEEK-BAD ( -- n ) [: s" BI-PEEK" MAKE-PEEK ;] catch ;
: READING-HERE ( -- ) s" the definer was reading here" DECL-DIAG:HAS? TTRUE ;
: FAMILY-FIX ( -- )
   s" = help: name n, bool, r or a family the package running the definer can name"
   DECL-DIAG:HAS? TTRUE ;

DECL-DIAG:PROSE  PTR-A 70 T=
s" error[E-BAD-SIGNATURE]: the type 'ptr a' has the letter 'a', which names no type"
DECL-DIAG:HAS? TTRUE
s" --> " DECL-DIAG:HAS? TTRUE  s" build-items.f:" DECL-DIAG:HAS? TTRUE
s" | DECL-DIAG:PROSE  PTR-A 70 T=" DECL-DIAG:HAS? TTRUE
s" |                  ^^^^^ the definer was reading here" DECL-DIAG:HAS? TTRUE
LETTER-FIX  s" ptr u8 in place of 'a'" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE  VIEW 70 T=
s" error[E-BAD-SIGNATURE]: the type 'read-view<p,q,u8>' has the letter 'p', which names no type"
DECL-DIAG:HAS? TTRUE
s" --> " DECL-DIAG:HAS? TTRUE  s" build-items.f:" DECL-DIAG:HAS? TTRUE
s" | DECL-DIAG:PROSE  VIEW 70 T=" DECL-DIAG:HAS? TTRUE
s" |                  ^^^^ the definer was reading here" DECL-DIAG:HAS? TTRUE
LETTER-FIX  s" ptr u8 in place of 'p'" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE  NAMED-BAD 70 T=
s" error[E-UNKNOWN-SIGNATURE-TYPE]: EFFECT:NAMED found no type named 'no-such-family'"
DECL-DIAG:HAS? TTRUE  READING-HERE  FAMILY-FIX
DECL-DIAG:PROSE  PEEK-BAD 70 T=
s" error[E-UNKNOWN-SIGNATURE-TYPE]: EFFECT:NAMED found no type named 'bitem-own:secret'"
DECL-DIAG:HAS? TTRUE  READING-HERE  FAMILY-FIX
DECL-DIAG:PROSE  IN-BAD 70 T=
s" error[E-NOT-EFFECT]: EFFECT:IN or EFFECT:OUT was given a type that is no effect"
DECL-DIAG:HAS? TTRUE  READING-HERE
s" = help: pass the effect EFFECT:EMPTY made, or one EFFECT:IN or EFFECT:OUT answered"
DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE  READ-BAD 70 T=
s" error[E-UNKNOWN-SIGNATURE-TYPE]: " DECL-DIAG:HAS? TTRUE
S\" 'no-such-family'\n" DECL-DIAG:HAS? TTRUE  READING-HERE
s" = help: declare a type of that name, or correct its spelling"
DECL-DIAG:HAS? TTRUE  DECL-DIAG:OFF

$400 constant LNEG-CAP
LNEG-CAP BUFFER: LNEG-OUT
LNEG-CAP BUFFER: LNEG-ERR
: LNEG-HAS? ( len ptr u8 n -- bool )
   {: eu:len a:ptr u:n :}
   LNEG-ERR eu LEN>N a u CONTAINS? ;

public
: WIDE-GO ( -- ) s" BI-WIDE" MAKE-WIDE ;
: TEXT-GO ( -- ) s" : BI-TEXT ( -- ) BITEM:LOCAL-NOW ;" evaluate-closed ;
: TEXT-REF-GO ( -- )
   s" : BI-TEXT-REF ( -- ) BITEM:LOCAL-READ-NOW ;" evaluate-closed ;
: ENDED-GO ( -- ) s" BI-ENDED" MAKE-ENDED ;
: LNEG-GO ( -- ) s" BI-LNEG" MAKE-LNEG ;
\ A read of local -1 names no bind: undefined, never an ended block.
: LNEG-CHILD ( -- )
   s" BITEM:LNEG-GO" {: src:ptr srcu:n :}
   src srcu LNEG-OUT LNEG-CAP >LEN LNEG-ERR LNEG-CAP >LEN
   10000 >MS SUBJECT:RUN {: ou:len eu:len oc :}
   src srcu LNEG-OUT ou LEN>N LNEG-ERR eu LEN>N oc 70 T-ENDING-EXITED=
   eu s" E-UNDEFINED habu: in bi-lneg: undefined word 'l-1'" LNEG-HAS? TTRUE
   eu s" block has ended" LNEG-HAS? TFALSE ;
;package

\ A rejection's text goes to fd 2, which a child's capture reads.
s" BITEM:WIDE-GO" 76
s" define-word has more than 23 type parameters: 'BI-WIDE'"
SUBJECT:REFUSED? TTRUE
s" BITEM:TEXT-GO" 70
s" EFFECT:LOCAL needs a definition define-word opened" SUBJECT:REFUSED? TTRUE
s" BITEM:TEXT-REF-GO" 70
s" EFFECT:LOCAL@ needs a definition define-word opened" SUBJECT:REFUSED? TTRUE
s" BITEM:ENDED-GO" 70
s" reads built local 0, whose `{:` block has ended" SUBJECT:REFUSED? TTRUE
\ A built body's rejection names the line of the definer's call.
s" include test/fixtures/build-items/placed.f" 70
s" placed.f:19" SUBJECT:REFUSED? TTRUE
\ A built local is named by its declared type, not a listing index.
s" include test/fixtures/build-items/placed.f" 70
s" at 'n' reads built local 0" SUBJECT:REFUSED? TTRUE
BITEM:LNEG-CHILD

\ A fresh hb runs the build, since the build is under test.
package BITEM-STORED
private
PATH-CAP BUFFER: DIR  variable DIR-U
PATH-CAP BUFFER: APP  variable APP-U
: DIR$ ( -- ptr u8 n ) DIR DIR-U @ ;
: APP$ ( -- ptr u8 n ) APP APP-U @ ;

: BUILD ( CMD:command -- CMD:command n )
   CMD:RESET
   s" --load" CMD:ARG+  s" tools/hb-build.f" CMD:ARG+  s" --" CMD:ARG+
   s" test/fixtures/postpone-stored-use.f" CMD:ARG+
   s" -o" CMD:ARG+  APP$ CMD:ARG+  s" HABU_OUT" DIR$ CMD:ENV+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N ;

: STORED ( CMD:command -- CMD:command )
   BUILD {: rc:n :}
   CMD:ERR$ s" a definer's type ends with the load that built it: build it where it is used"
   CONTAINS? TTRUE
   CMD:ERR$ s" HELD" CONTAINS? TTRUE
   rc 74 T= ;

: CHECKS ( -- ) [: STORED ;] CMD:WITH-COMMAND ;
: REMOVE ( -- ) DIR$ FS:REMOVE-TREE ;

public
: RUN ( -- )
   s" habu-bitem-stored" FS:MKTEMP-DIR {: a:ptr u:n :}
   a DIR u BYTE-COPY  u DIR-U !
   DIR$ s" app" APP FS:JOIN-PATH APP-U !
   ['] CHECKS ['] REMOVE finally ;
;package

BITEM-STORED:RUN

T-REPORT
