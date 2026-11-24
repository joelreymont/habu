split-str
\ build-items.f - a definer builds words of keyword, type, operand and call
\ items: a match over a family of its own answers as the same text does, an if
\ and a begin loop, a construct, a call of a word found where the definer runs,
\ and typed locals: two of `n`, one of the family matched, two
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
\ signature does, and a quotation type of 300 cells, which a built word takes
\ from a quotation of 300 cells and not of 299; it rejects a letter, a type variable's or a view's region's,
\ at the line that ran the definer, with the fix, as EFFECT:NAMED and
\ EFFECT:READ-TYPE reject an unknown family, which a definer names as source
\ in its package would, and EFFECT:IN a type that is no effect.
\ EFFECT:READ-TYPE reads `xt` and `[ xt -- n ]` as a signature does, and a
\ built word takes an `xt` and runs a quotation of one; EFFECT:VARIABLE-OF of
\ `xt` is rejected with the storage definers' reason, and VARIABLE-OF and
\ TYPE-ITEM of a type forged from a number as no type. A build of a
\ program whose source stores a definer's type is rejected with the record its
\ load prints. A rejected built local whose type text outgrows the checker's
\ first token buffers is named by that whole text. A program that requires
\ src/lib/c2-memory.f builds cold and warm to the same image and copies a byte
\ into owned bytes, and one whose definition ticks a C2-MEM scoped entry is
\ rejected. A program keeping more address rows than the band holds builds
\ cold and warm to the same image and runs a quotation the build stored.

require src/lib/test.f
require src/lib/c2-memory.f
require src/lib/adt/nullable.f
require src/lib/test/subject.f
require test/decl-diag-capture.f
require src/lib/memory.f
require src/lib/string-option.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-command.f
require src/lib/engine-candidate.f
require src/lib/codesign.f
require tools/gate-json-assert-core.f

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

: KW ( str -- ) EFFECT:KEYWORD ;
: OPND ( str -- ) EFFECT:OPERAND ;
: BFAM ( -- EFFECT:type ) s" bitem:bfam" EFFECT:NAMED ;
: BFAM-ITEM ( -- ) s" bfam" BFAM EFFECT:TYPE-ITEM ;

\ : NAME ( bfam -- n ) match bfam ba of 1 endof X of 2 endof ;match ;
: MAKE-MATCH ( str str -- ) {: xa:^ xu:n :}
   EFFECT:EMPTY BFAM EFFECT:IN EFFECT:N EFFECT:OUT define-word
   s" match" KW  BFAM-ITEM
   s" ba" OPND  s" of" KW  1 literal  s" endof" KW
   xa xu OPND  s" of" KW  2 literal  s" endof" KW
   s" ;match" KW
   postpone ; ;

\ : NAME ( n -- n )
\    dup 0 > if begin 1 - dup 3 < until 100 + else drop 20 then ;
: MAKE-FLOW ( str -- )
   EFFECT:EMPTY EFFECT:N EFFECT:IN EFFECT:N EFFECT:OUT define-word
   postpone dup  0 literal  postpone >  s" if" KW
   s" begin" KW  1 literal  postpone -  postpone dup  3 literal  postpone <
   s" until" KW  100 literal  postpone +
   s" else" KW  postpone drop  20 literal  s" then" KW
   postpone ; ;

\ : NAME ( -- bfam ) construct bfam bb ;
: MAKE-CONSTRUCT ( str -- )
   EFFECT:EMPTY BFAM EFFECT:OUT define-word
   s" construct" KW  BFAM-ITEM  s" bb" OPND
   postpone ; ;

\ : NAME ( -- n ) X ;
: MAKE-CALL ( str str -- ) {: xa:^ xu:n :}
   EFFECT:EMPTY EFFECT:N EFFECT:OUT define-word
   xa xu EFFECT:CALL
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

s" BI-CALL" s" BI-SEVEN" MAKE-CALL
BI-CALL 7 T=

\ The text binds the part's ANSWER, not the top-level package's.
: BI-PART ( -- n ) BITEM-PKG:ANSWER ;
BI-PART 2 T=

\ Each refused name is free after its refusal: a text definition takes it.
: BAD-OPERAND ( -- n ) [: s" BI-BAD-OPERAND" s" bz" MAKE-MATCH ;] catch ;
BAD-OPERAND 70 T=
s" : BI-BAD-OPERAND ( -- n ) 3 ;" evaluate-closed
BI-BAD-OPERAND 3 T=

: BAD-CALL ( -- n )
   [: s" BI-BAD-CALL" s" BI-NO-SUCH" MAKE-CALL ;] catch ;
BAD-CALL 70 T=
s" : BI-BAD-CALL ( -- n ) 4 ;" evaluate-closed
BI-BAD-CALL 4 T=

\ : NAME ( n n -- n ) {: a:n b:n :} b a - ;
: MAKE-SUB ( str -- )
   EFFECT:EMPTY EFFECT:N EFFECT:IN EFFECT:N EFFECT:IN EFFECT:N EFFECT:OUT
   define-word
   s" {:" KW  EFFECT:N EFFECT:LOCAL  EFFECT:N EFFECT:LOCAL  s" :}" KW
   {: a:n b:n :}
   b EFFECT:LOCAL@  a EFFECT:LOCAL@  postpone -
   postpone ; ;

\ : NAME ( bfam -- n ) {: x:bfam :}
\    x match bfam ba of 1 endof bb of 2 endof ;match ;
: MAKE-LOCAL-MATCH ( str -- )
   EFFECT:EMPTY BFAM EFFECT:IN EFFECT:N EFFECT:OUT define-word
   s" {:" KW  BFAM EFFECT:LOCAL  s" :}" KW  EFFECT:LOCAL@
   s" match" KW  BFAM-ITEM
   s" ba" OPND  s" of" KW  1 literal  s" endof" KW
   s" bb" OPND  s" of" KW  2 literal  s" endof" KW
   s" ;match" KW
   postpone ; ;

: GROUP ( -- n ) s" {:" KW  EFFECT:N EFFECT:LOCAL  s" :}" KW ;

\ : NAME ( n n -- n ) {: a:n :} {: b:n :} a b - ;
: MAKE-GROUPS ( str -- )
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
: MAKE-STRAY ( str -- )
   EFFECT:EMPTY define-word  EFFECT:N EFFECT:LOCAL drop  postpone ; ;
: BAD-STRAY ( -- n ) [: s" BI-BAD-STRAY" MAKE-STRAY ;] catch ;
BAD-STRAY 70 T=
s" : BI-BAD-STRAY ( -- n ) 5 ;" evaluate-closed
BI-BAD-STRAY 5 T=

\ : NAME ( -- n ) l9 ; with no local declared; UNREAD-OPEN leaves the word
\ open before its `;`.
: UNREAD-OPEN ( str -- )
   EFFECT:EMPTY EFFECT:N EFFECT:OUT define-word  9 EFFECT:LOCAL@ ;
: MAKE-UNREAD ( str -- ) UNREAD-OPEN  postpone ; ;
: BAD-UNREAD ( -- n ) [: s" BI-BAD-UNREAD" MAKE-UNREAD ;] catch ;
BAD-UNREAD 70 T=
s" : BI-BAD-UNREAD ( -- n ) 6 ;" evaluate-closed
BI-BAD-UNREAD 6 T=

\ ( nullable<a> nullable<b> nullable<c> nullable<d> -- nullable<a> )
\    drop drop drop {: x:nullable<a> :} x ;
\ The local's type is the file's fourth parameter, which the effect numbers
\ first: the local is the first input's type value.
: NUL ( -- EFFECT:type ) s" nullable" EFFECT:NAMED ;
: MAKE-KEEP ( str -- )
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
: MAKE-WRONG ( str -- )
   EFFECT:EMPTY BFAM EFFECT:IN EFFECT:N EFFECT:OUT define-word
   s" {:" KW  BFAM EFFECT:LOCAL  s" :}" KW  EFFECT:LOCAL@
   1 literal  postpone +
   postpone ; ;
: BAD-WRONG ( -- n ) [: s" BI-BAD-WRONG" MAKE-WRONG ;] catch ;
BAD-WRONG 70 T=
s" : BI-BAD-WRONG ( -- n ) 7 ;" evaluate-closed
BI-BAD-WRONG 7 T=

\ : NAME ( n bool -- n ) if {: a:n :} a then {: b:n :} a ; with b in the
\ slot a's block left; ENDED-OPEN leaves the word open before its `;`.
: ENDED-OPEN ( str -- )
   EFFECT:EMPTY EFFECT:N EFFECT:IN s" bool" EFFECT:NAMED EFFECT:IN
   EFFECT:N EFFECT:OUT define-word
   s" if" KW  GROUP {: a:n :}  a EFFECT:LOCAL@  s" then" KW
   GROUP drop  a EFFECT:LOCAL@ ;
: MAKE-ENDED ( str -- ) ENDED-OPEN  postpone ; ;
: BAD-ENDED ( -- n ) [: s" BI-BAD-ENDED" MAKE-ENDED ;] catch ;
BAD-ENDED 70 T=
s" : BI-BAD-ENDED ( -- n ) 8 ;" evaluate-closed
BI-BAD-ENDED 8 T=

\ : NAME ( n -- n ) {: a:n :} l-1 ; inside a's block.
: MAKE-LNEG ( str -- )
   EFFECT:EMPTY EFFECT:N EFFECT:IN EFFECT:N EFFECT:OUT define-word
   GROUP drop  -1 EFFECT:LOCAL@
   postpone ; ;

\ : NAME ( nullable<a> ... -- ) {: x:nullable<z> :} drop ... ; with 23
\ inputs, so the local's z is a 24th parameter.
: MAKE-WIDE ( str -- )
   EFFECT:EMPTY  23 0 ?do NUL EFFECT:IN loop  define-word
   s" {:" KW  NUL EFFECT:LOCAL drop  s" :}" KW
   22 0 ?do postpone drop loop
   postpone ; ;

\ TYPED-VARIABLE NAME [ nullable<a> ... -- ] with 24 parameters.
: MAKE-VWIDE ( str -- )
   EFFECT:EMPTY  24 0 ?do NUL EFFECT:IN loop  EFFECT:VARIABLE-OF ;

\ : NAME ( -- ) [ EFFECT:N EFFECT:LOCAL drop ] ; as text, which no
\ define-word effect numbers; LOCAL-READ-NOW reads local 0 the same way.
public
: LOCAL-NOW ( -- ) EFFECT:N EFFECT:LOCAL drop ; immediate
: LOCAL-READ-NOW ( -- ) 0 EFFECT:LOCAL@ ; immediate
private
: BAD-TEXT ( -- n )
   [: s" : BI-BAD-TEXT ( -- ) LOCAL-NOW ;" evaluate-closed ;] catch ;
BAD-TEXT 70 T=
s" : BI-BAD-TEXT ( -- n ) 9 ;" evaluate-closed
BI-BAD-TEXT 9 T=

\ TYPED-VARIABLE NAME [ n -- n ]
: MAKE-QVAR ( str -- )
   EFFECT:EMPTY EFFECT:N EFFECT:IN EFFECT:N EFFECT:OUT EFFECT:VARIABLE-OF ;

s" BI-QVAR" MAKE-QVAR
: BI-QSTORE ( -- ) [: 1 + ;] BI-QVAR ! ;
BI-QSTORE
5 BI-QVAR @ execute 6 T=
: BAD-QSTORE ( -- n )
   [: s" : BI-BAD-QSTORE ( -- ) [: dup ;] BI-QVAR ! ;" evaluate-closed ;] catch ;
BAD-QSTORE 70 T=
5 BI-QVAR @ execute 6 T=

\ TYPED-VARIABLE NAME [ n -- option<a> ]: its parameter follows the file's
\ earlier ones, and the variable numbers it first.
: MAKE-OVAR ( str -- )
   EFFECT:EMPTY EFFECT:N EFFECT:IN s" option" EFFECT:NAMED EFFECT:OUT
   EFFECT:VARIABLE-OF ;
: BAD-OVAR ( -- n ) [: s" BI-BAD-OVAR" MAKE-OVAR ;] catch ;
DECL-DIAG:PROSE  BAD-OVAR 70 T=
s" = word: [ n -- option<a> ]" DECL-DIAG:HAS? TTRUE  DECL-DIAG:OFF

8192 BUFFER: BUILT-DIAG
variable BUILT-DIAG-U
: KEEP-DIAG ( -- )
   DECL-DIAG:TEXT$ {: a:^ u:n :}
   a BUILT-DIAG u BYTE-COPY  u BUILT-DIAG-U ! ;
: BUILT-DIAG$ ( -- str ) BUILT-DIAG BUILT-DIAG-U @ ;

\ : NAME ( -- ) C2-MEM:WITH-FIELD inner ; with no view to lend a field of.
: MAKE-LOAN ( str -- )
   EFFECT:EMPTY define-word
   s" C2-MEM:WITH-FIELD" EFFECT:CALL  s" inner" OPND
   postpone ; ;

: LOAN-BUILT ( -- n ) [: s" BI-LOAN" MAKE-LOAN ;] catch ;
: LOAN-TEXT ( -- n )
   [: s" : BI-LOAN ( -- ) C2-MEM:WITH-FIELD inner ;" evaluate-closed ;] catch ;
\ A record's first line, and its notes from `= declared:` on: the twins' places
\ differ, the built one's at the definer's call.
: CUT ( ^u8 n ^u8 n -- n ) {: a:^ u:n ka:^ ku:n :}
   a u ka ku STR:FIND-SUB MATCH option
      none OF u ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;
: HEAD$ ( str -- str ) {: a:^ u:n :}
   a  a u S\" \n" CUT ;
: NOTES$ ( str -- str ) {: a:^ u:n :}
   a u s" = declared:" CUT {: k:n :}
   a k +  u k - ;
DECL-DIAG:PROSE  LOAN-BUILT 70 T=  KEEP-DIAG
DECL-DIAG:PROSE  LOAN-TEXT 70 T=
BUILT-DIAG$ HEAD$  DECL-DIAG:TEXT$ HEAD$ T$=
BUILT-DIAG$ NOTES$  DECL-DIAG:TEXT$ NOTES$ T$=  DECL-DIAG:OFF

\ : NAME ( bfam -- n ) match zz ba of evaluate endof ;match ; whose family
\ resolves nothing: the call is skipped through `;match` as its text is.
: MAKE-ABANDONED ( str -- )
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
: MAKE-CALL-FAMILY ( str -- )
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
: MAKE-TYPE-VARIANT ( str -- )
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
\ spelling is one token, else it is rejected as define-word's name is, 70.
: MAKE-SPELLED ( str str -- ) {: sa:^ su:n :}
   EFFECT:EMPTY EFFECT:N EFFECT:OUT define-word
   5 literal  sa su KW
   postpone ; ;
: EMPTY-ITEM ( -- n ) [: s" BI-EMPTY-ITEM" s" " MAKE-SPELLED ;] catch ;
: BLANK-ITEM ( -- n )
   [: s" BI-BLANK-ITEM" s" 1 drop" MAKE-SPELLED ;] catch ;
EMPTY-ITEM 70 T=
BLANK-ITEM 70 T=
s" : BI-BLANK-ITEM ( -- n ) 8 ;" evaluate-closed
BI-BLANK-ITEM 8 T=

public
DEFLINEAR BITEM:ltok
private
LINEAR: LTOK ( n -- BITEM:ltok )
LINEAR: LTOK>N ( BITEM:ltok -- n )
: BI-U8-WIDTH ( -- width<u8> ) [type] u8 ;

\ : NAME ( t -- t ) ;
: MAKE-ID ( str EFFECT:type -- ) {: t :}
   EFFECT:EMPTY t EFFECT:IN  t EFFECT:OUT  define-word  postpone ; ;

SUMTYPE blin VARIANT owned BITEM:ltok ;VARIANT ;SUMTYPE
: BI-LIN-MAKE ( n -- blin ) LTOK construct blin owned ;
: BI-LIN-N ( blin -- n ) match blin owned of LTOK>N endof ;match ;

s" BI-LIN" s" blin" EFFECT:READ-TYPE MAKE-ID
: BI-LIN-RUN ( -- n ) 5 BI-LIN-MAKE BI-LIN BI-LIN-N ;
BI-LIN-RUN 5 T=
s" BI-WIDTH" s" width<u8>" EFFECT:READ-TYPE MAKE-ID
BI-U8-WIDTH BI-WIDTH width>n 1 T=
s" BI-XT" s" xt" EFFECT:READ-TYPE MAKE-ID
: BI-XT-RUN ( -- ) ['] type BI-XT drop ;
s" BI-XQ" s" [ xt -- n ]" EFFECT:READ-TYPE MAKE-ID
: BI-XQ-RUN ( -- n ) ['] type [: drop 7 ;] BI-XQ execute ;
BI-XQ-RUN 7 T=

: PTR-A ( -- n ) [: s" BI-PTR-A" s" ^a" EFFECT:READ-TYPE MAKE-ID ;] catch ;
: VIEW ( -- n )
   [: s" BI-VIEW" s" read-view<p,q,u8>" EFFECT:READ-TYPE MAKE-ID ;] catch ;
: LETTER-FIX ( -- )
   s" = help: a built definition's types are concrete: write one such as n or ^u8"
   DECL-DIAG:HAS? TTRUE ;
: NAMED-BAD ( -- n ) [: s" no-such-family" EFFECT:NAMED drop ;] catch ;
: IN-BAD ( -- n ) [: EFFECT:N EFFECT:N EFFECT:IN drop ;] catch ;
: READ-BAD ( -- n )
   [: s" BI-UNK" s" no-such-family" EFFECT:READ-TYPE MAKE-ID ;] catch ;
: XT-VAR ( -- n )
   [: s" BI-XVAR" s" xt" EFFECT:READ-TYPE EFFECT:VARIABLE-OF ;] catch ;
: SECRET ( -- EFFECT:type ) s" bitem-own:secret" EFFECT:NAMED ;

\ : NAME ( bitem-own:secret -- n ) match secret hid of endof ;match ;
: MAKE-PEEK ( str -- )
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
s" error[E-BAD-SIGNATURE]: the type '^a' has the letter 'a', which names no type"
DECL-DIAG:HAS? TTRUE
s" --> " DECL-DIAG:HAS? TTRUE  s" build-items.f:" DECL-DIAG:HAS? TTRUE
s" | DECL-DIAG:PROSE  PTR-A 70 T=" DECL-DIAG:HAS? TTRUE
s" |                  ^^^^^ the definer was reading here" DECL-DIAG:HAS? TTRUE
LETTER-FIX  s" ^u8 in place of 'a'" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE  VIEW 70 T=
s" error[E-BAD-SIGNATURE]: the type 'read-view<p,q,u8>' has the letter 'p', which names no type"
DECL-DIAG:HAS? TTRUE
s" --> " DECL-DIAG:HAS? TTRUE  s" build-items.f:" DECL-DIAG:HAS? TTRUE
s" | DECL-DIAG:PROSE  VIEW 70 T=" DECL-DIAG:HAS? TTRUE
s" |                  ^^^^ the definer was reading here" DECL-DIAG:HAS? TTRUE
LETTER-FIX  s" ^u8 in place of 'p'" DECL-DIAG:HAS? TTRUE
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
DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE  XT-VAR 70 T=
s" error[E-BAD-STORAGE]: the type of `BI-XVAR` holds an `xt`, which no storage holds"
DECL-DIAG:HAS? TTRUE
s" |                  ^^^^^^ xt in a stored type" DECL-DIAG:HAS? TTRUE
s" = help: declare `BI-XVAR` with a quotation type such as `[ n -- n ]` in place of `xt`"
DECL-DIAG:HAS? TTRUE  DECL-DIAG:OFF

\ A type a program forged from a number, given to a definer.
CAST: >BI-TYPE ( n -- EFFECT:type )
: BI-FORGED ( -- EFFECT:type ) $10000000000 1000 + >BI-TYPE ;
: FORGED-VAR ( -- n )
   [: s" BI-RPV" BI-FORGED EFFECT:VARIABLE-OF ;] catch ;
\ : NAME ( -- n ) x ; with x the forged type
: MAKE-FORGED-ITEM ( str -- )
   EFFECT:EMPTY EFFECT:N EFFECT:OUT define-word
   s" x" BI-FORGED EFFECT:TYPE-ITEM  postpone ; ;
: FORGED-ITEM ( -- n ) [: s" BI-FORGED-ITEM" MAKE-FORGED-ITEM ;] catch ;
: NOT-TYPE ( -- )
   s" error[E-NOT-TYPE]: a definer was given a value that is no type"
   DECL-DIAG:HAS? TTRUE  READING-HERE ;
DECL-DIAG:PROSE  FORGED-VAR 70 T=  NOT-TYPE
DECL-DIAG:PROSE  FORGED-ITEM 70 T=  NOT-TYPE  DECL-DIAG:OFF

300 constant LONG-N
s" [ -- ]" nip LONG-N 2 * + constant LONG-TYPE-CAP
s" : BI-LONG-OK ( -- ) [: ;] BI-LONG drop ;" nip LONG-N 5 * +
constant LONG-CALL-CAP
LONG-TYPE-CAP BUFFER: LONG-TYPE
LONG-CALL-CAP BUFFER: LONG-CALL
variable LONG-U
: LONG+ ( ^u8 str -- ) {: dst:^ a:^ u:n :}
   a dst LONG-U @ + u BYTE-COPY  LONG-U @ u + LONG-U ! ;
\ [ n n … -- ] of LONG-N cells
: LONG-TYPE$ ( -- str )
   0 LONG-U !  LONG-TYPE s" [ " LONG+
   LONG-N 0 ?do LONG-TYPE s" n " LONG+ loop
   LONG-TYPE s" -- ]" LONG+  LONG-TYPE LONG-U @ ;
\ : NAME ( -- ) [: drop drop … ;] BI-LONG drop ; of k drops
: LONG-CALL$ ( str n -- str ) {: na:^ nu:n k:n :}
   0 LONG-U !  LONG-CALL s" : " LONG+  LONG-CALL na nu LONG+
   LONG-CALL s"  ( -- ) [: " LONG+
   k 0 ?do LONG-CALL s" drop " LONG+ loop
   LONG-CALL s" ;] BI-LONG drop ;" LONG+  LONG-CALL LONG-U @ ;
LONG-TYPE-CAP 64 + BUFFER: LONG-HEAD
\ The rejection's head: BI-LONG takes the LONG-N cells and gets a quotation.
: LONG-HEAD$ ( -- str )
   LONG-TYPE$ {: ta:^ tu:n :}
   0 LONG-U !  LONG-HEAD s" error[E-MISMATCH]: `BI-LONG` takes " LONG+
   LONG-HEAD ta tu LONG+  LONG-HEAD s" ; the stack holds [ a b " LONG+
   LONG-HEAD LONG-U @ ;

s" BI-LONG" LONG-TYPE$ EFFECT:READ-TYPE MAKE-ID
s" BI-LONG-OK" LONG-N LONG-CALL$ TEST-EVAL:RC 0 T=
DECL-DIAG:PROSE
s" BI-LONG-NO" LONG-N 1 - LONG-CALL$ TEST-EVAL:RC 70 T=
LONG-HEAD$ DECL-DIAG:HAS? TTRUE
s"  ^^^^^^^ takes [ n n " DECL-DIAG:HAS? TTRUE
s" = help: give `BI-LONG` [ n n " DECL-DIAG:HAS? TTRUE
s"  297 298 -- ]" DECL-DIAG:HAS? TTRUE  DECL-DIAG:OFF

\ : NAME ( n n -- n ) {: a:n b:n :} a b, finished by the text that follows.
: SUB2-OPEN ( str -- )
   EFFECT:EMPTY EFFECT:N EFFECT:IN EFFECT:N EFFECT:IN EFFECT:N EFFECT:OUT
   define-word
   s" {:" KW  EFFECT:N EFFECT:LOCAL  EFFECT:N EFFECT:LOCAL  s" :}" KW
   {: a:n b:n :}
   a EFFECT:LOCAL@  b EFFECT:LOCAL@ ;

public
: SUB2-GO ( -- ) s" BI-SUB2" SUB2-OPEN ;
: ENDED-TEXT-GO ( -- ) s" BI-ENDED-TEXT" ENDED-OPEN ;
: UNREAD-GO ( -- ) s" BI-UNREAD-TEXT" UNREAD-OPEN ;
: WIDE-GO ( -- ) s" BI-WIDE" MAKE-WIDE ;
: VWIDE-GO ( -- ) s" BI-VWIDE" MAKE-VWIDE ;
: TEXT-GO ( -- ) s" : BI-TEXT ( -- ) BITEM:LOCAL-NOW ;" evaluate-closed ;
: TEXT-REF-GO ( -- )
   s" : BI-TEXT-REF ( -- ) BITEM:LOCAL-READ-NOW ;" evaluate-closed ;
: ENDED-GO ( -- ) s" BI-ENDED" MAKE-ENDED ;
: STRAY-GO ( -- ) s" BI-STRAY" MAKE-STRAY ;
: LNEG-GO ( -- ) s" BI-LNEG" MAKE-LNEG ;
\ A read of local -1 names no bind: unbound, never an ended block.
: LNEG-CHILD ( -- )
   s" BITEM:LNEG-GO" 70 SUBJECT:REJECTED {: e:^ eu:n exited:bool :}
   exited TTRUE
   e eu s" reads built local -1, which no `{:` group bound before it"
   STR:CONTAINS? TTRUE
   e eu s" block has ended" STR:CONTAINS? TFALSE ;

\ A built body's rejection names the line of the definer's call, and a built
\ local by its declared type, not a listing index.
: PLACED-CHILD ( -- )
   s" include test/fixtures/build-items/placed.f" 70 SUBJECT:REJECTED
   {: e:^ eu:n exited:bool :}
   exited TTRUE
   e eu s" placed.f:19" STR:CONTAINS? TTRUE
   e eu s" `n` reads built local 0" STR:CONTAINS? TTRUE ;

\ A read pinned after an earlier failure is placed at the definer's call.
: UNBOUND-CHILD ( -- )
   s" include test/fixtures/build-items/unbound.f" 70 SUBJECT:REJECTED
   {: e:^ eu:n exited:bool :}
   exited TTRUE
   e eu s" error[E-UNDEFINED]: reads built local 9, which no `{:` group bound before it"
   STR:CONTAINS? TTRUE
   e eu s\" 14 | s\" BI-UNBOUND\" MAKE-UNBOUND" STR:CONTAINS? TTRUE
   e eu s" unbound.f:14:16" STR:CONTAINS? TTRUE ;

\ A call an evaluated string appends is placed in no line of the file whose
\ text opened the definition: the string's offsets are not the file's.
: EVALUATED-CHILD ( -- )
   s" include test/fixtures/build-items/evaluated.f" 70 SUBJECT:REJECTED
   {: e:^ eu:n exited:bool :}
   exited TTRUE
   e eu s" input underflow" STR:CONTAINS? TTRUE
   e eu S\" evaluated.f:1\n" STR:CONTAINS? TFALSE ;

\ A read no group bound, in a body text finishes, names the local by its
\ index, never by its listing token.
: UNREAD-CHILD ( -- )
   s" BITEM:UNREAD-GO 1 + ;" 70 SUBJECT:REJECTED {: e:^ eu:n exited:bool :}
   exited TTRUE
   e eu s" reads built local 9, which no `{:` group bound before it"
   STR:CONTAINS? TTRUE
   e eu s" l9" STR:CONTAINS? TFALSE ;

\ Two built locals of one type read in a body text finishes are two locals.
s" SUB2-GO - ;" evaluate-closed  10 3 BI-SUB2 7 T=

\ A built definition's rejection, captured as JSON in place of its prose, is
\ a record of the one form whose message leads with the read: no listing token.
private
: JSON-ON ( -- )  REJECTION:CAPTURE  true REJECTION:JSON ! ;
: JSON-OFF ( -- )  false REJECTION:JSON !  REJECTION:BUFFER-OFF ;
: UNREAD-JSON ( -- n ) [: s" UNREAD-GO 1 + ;" evaluate-closed ;] catch ;
: JSON-HAS ( str -- ) REJECTION:BUFFER$ 2swap STR:CONTAINS? TTRUE ;
JSON-ON  UNREAD-JSON 70 T=  JSON-OFF
S\" \"code\":\"E-UNDEFINED\"" JSON-HAS
S\" \"message\":\"reads built local 9, which no `{:` group bound before it" JSON-HAS
REJECTION:BUFFER$ s" l9" STR:CONTAINS? TFALSE
: JSON-FORM ( -- ) REJECTION:BUFFER$ GJA-PARSE GJA-RECORD-FORM ;
JSON-FORM
;package

\ A rejection's text goes to fd 2, which a child's capture reads.
s" BITEM:WIDE-GO" 70
s" the effect `define-word` got for `BI-WIDE` has too many type parameters"
SUBJECT:REFUSED? TTRUE
s" BITEM:VWIDE-GO" 70
s" the type `EFFECT:VARIABLE-OF` got for `BI-VWIDE` has too many type parameters"
SUBJECT:REFUSED? TTRUE
s" BITEM:TEXT-GO" 70
s" EFFECT:LOCAL needs a definition define-word opened" SUBJECT:REFUSED? TTRUE
s" BITEM:TEXT-REF-GO" 70
s" EFFECT:LOCAL@ needs a definition define-word opened" SUBJECT:REFUSED? TTRUE
s" BITEM:ENDED-GO" 70
s" reads built local 0, whose `{:` block has ended" SUBJECT:REFUSED? TTRUE
\ A local outside a group is rejected as a stray local.
s" BITEM:STRAY-GO" 70
s" is a built local outside a `{:` group" SUBJECT:REFUSED? TTRUE
\ A read of a local whose block ended, in a body text finishes, is rejected.
s" BITEM:ENDED-TEXT-GO 1 + ;" 70
s" `n` reads built local 0, whose `{:` block has ended" SUBJECT:REFUSED? TTRUE
BITEM:PLACED-CHILD
BITEM:UNBOUND-CHILD
BITEM:EVALUATED-CHILD
BITEM:LNEG-CHILD
BITEM:UNREAD-CHILD

\ A fresh hb runs each build, since the build is under test; each case has its
\ own out under DIR, and the warm build reuses the cold one's.
package BITEM-BUILD
private
PATH-CAP BUFFER: DIR  variable DIR-U
PATH-CAP BUFFER: PATH
: DIR$ ( -- str ) DIR DIR-U @ ;
\ NAME's path in DIR, in bytes the next call reuses.
: IN$ ( str -- str ) DIR$ 2swap PATH FS:JOIN-PATH PATH swap ;

\ Builds SRC into DIR's PROG under DIR's OUT.
: BUILD ( CMD:command ^u8 n ^u8 n ^u8 n -- CMD:command n )
   {: src:^ srcu:n out:^ outu:n prog:^ progu:n :}
   CMD:RESET
   s" --load" CMD:ARG+  s" tools/hb-build.f" CMD:ARG+  s" --" CMD:ARG+
   src srcu CMD:ARG+  s" -o" CMD:ARG+  prog progu IN$ CMD:ARG+
   s" HABU_OUT" out outu IN$ CMD:ENV+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N ;

\ A status other than the one expected shows the status and both streams.
: SHOW ( CMD:command n n -- CMD:command n ) {: rc:n want:n :}
   rc want <> if
      s" build-items: exited " type rc .
      s" stdout: " type CMD:OUT$ type cr  s" stderr: " type CMD:ERR$ type cr
   then
   rc ;

: STORED ( CMD:command -- CMD:command )
   s" test/fixtures/postpone-stored-use.f" s" stored" s" stored-app" BUILD
   70 SHOW {: rc:n :}
   CMD:ERR$ s" a definer's type ends with the load that built it: build it where it is used"
   STR:CONTAINS? TTRUE
   CMD:ERR$ s" HELD" STR:CONTAINS? TTRUE
   rc 70 T= ;

\ Built program NAME's bytes, in storage its size makes.
: PROG-READ ( str -- str )
   IN$ {: p:^ pu:n :}
   p pu FS:FILE-SIZE {: u:n :}
   u MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop {: b:^ :}
   p pu b u FS:READ-ALL u T=  b u ;
: PROG-FREE ( str -- ) MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

\ Built programs A and B are the same image.
: SAME-PROGS ( str str -- )
   {: x:^ xu:n y:^ yu:n :}
   x xu PROG-READ {: a:^ au:n :}
   y yu PROG-READ {: b:^ bu:n :}
   a au b bu CODESIGN:SAME-IMAGE? TTRUE
   a au PROG-FREE  b bu PROG-FREE ;

: C2-BYTES ( CMD:command -- CMD:command )
   s" test/fixtures/build-items/c2-bytes.f" s" c2-bytes" s" c2-bytes-cold" BUILD 0 SHOW {: cold:n :}
   cold 0 T=  cold 0<> if exit then
   s" test/fixtures/build-items/c2-bytes.f" s" c2-bytes" s" c2-bytes-warm" BUILD 0 SHOW {: warm:n :}
   warm 0 T=  warm 0<> if exit then
   s" c2-bytes-cold" s" c2-bytes-warm" SAME-PROGS
   CMD:RESET  s" c2-bytes-cold" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N 0 SHOW {: rc:n :}
   CMD:OUT$ S\" bitem-c2-bytes: a\n" STR:= TTRUE
   rc 0 T= ;

: CAP-BUILD ( CMD:command ^u8 n -- CMD:command bool ) {: prog:^ progu:n :}
   s" test/fixtures/build-items/cap-grown.f" s" cap-grown" prog progu BUILD
   0 SHOW {: rc:n :}
   rc 0 T=  rc 0= ;

\ A program keeping more rows than the band holds runs with them relocated, and
\ its next row grows the list by one.
: CAP-GROWN ( CMD:command -- CMD:command )
   s" cap-grown-cold" CAP-BUILD 0= if exit then
   s" cap-grown-warm" CAP-BUILD 0= if exit then
   s" cap-grown-cold" s" cap-grown-warm" SAME-PROGS
   CMD:RESET  s" cap-grown-cold" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N 0 SHOW {: rc:n :}
   CMD:OUT$ S\" 42\n-1\n1\n" STR:= TTRUE
   rc 0 T= ;

\ The build's own C2-MEM entries are scoped entries too.
: TICK ( CMD:command -- CMD:command )
   s" test/fixtures/build-items/scoped-tick.f" s" tick" s" tick-app" BUILD
   70 SHOW {: rc:n :}
   CMD:ERR$ s" `C2-MEM:WITH-INIT` is a scoped entry, which only a direct call may reach"
   STR:CONTAINS? TTRUE
   CMD:ERR$ s" scoped-tick.f:8:" STR:CONTAINS? TTRUE
   rc 70 T= ;

: CHECKS ( -- ) [: STORED C2-BYTES CAP-GROWN TICK ;] CMD:WITH-COMMAND ;
: REMOVE ( -- ) DIR$ FS:REMOVE-TREE ;

public
: RUN ( -- )
   s" habu-bitem-build" FS:MKTEMP-DIR {: a:^ u:n :}
   a DIR u BYTE-COPY  u DIR-U !
   ['] CHECKS ['] REMOVE finally ;
;package

BITEM-BUILD:RUN

\ A fresh hb runs the fixture: the case needs the token buffers at their first
\ capacity, which an earlier check in this process could have grown. The record
\ names the local by its whole type text, at the definer's call.
package BITEM-LONG-TYPE
private
: LONG ( CMD:command -- CMD:command )
   CMD:RESET
   s" --load" CMD:ARG+  s" test/fixtures/build-items/long-type.f" CMD:ARG+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N 70 T=
   CMD:ERR$ s" error[E-UNDEFINED]: `[ ^^" STR:CONTAINS? TTRUE
   CMD:ERR$ s" ^u8 -- ]` is a built local outside a `{:` group" STR:CONTAINS? TTRUE
   CMD:ERR$ s" long-type.f:24:13" STR:CONTAINS? TTRUE ;

public
: RUN ( -- ) [: LONG ;] CMD:WITH-COMMAND ;
;package

BITEM-LONG-TYPE:RUN

\ A fresh hb loads the fixture, and check.f checks it: each grows the storage
\ definers' scratch from its first capacity, which an earlier case in this
\ process could have grown.
package BITEM-LONG-STORAGE
private
: LOAD ( CMD:command -- CMD:command )
   CMD:RESET
   s" --load" CMD:ARG+  s" test/fixtures/build-items/long-storage.f" CMD:ARG+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N 0 T= ;

: CHECK ( CMD:command -- CMD:command )
   CMD:RESET
   s" --load" CMD:ARG+  s" tools/check.f" CMD:ARG+  s" --" CMD:ARG+
   s" test/fixtures/build-items/long-storage.f" CMD:ARG+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N 0 T= ;

public
: RUN ( -- ) [: LOAD CHECK ;] CMD:WITH-COMMAND ;
;package

BITEM-LONG-STORAGE:RUN

T-REPORT
