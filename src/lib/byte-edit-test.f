require src/lib/test.f
require src/lib/heap.f
require src/lib/byte-edit.f

package BYTE-EDIT-TEST
using EDIT
$100 constant OUTPUT-CAP
\ Enough edits to grow the rows several times.
100 constant MANY
create OUTPUT OUTPUT-CAP allot
create BINARY $41 c, 0 c, $FF c, $42 c,
create BINARY-EXPECTED $41 c, $7F c, 0 c, $42 c,
create BINARY-INSERT $7F c, 0 c,
create MANY-SOURCE MANY allot
create MANY-EXPECTED MANY 2 * allot

: OPEN ( str -- EDIT:editor )
   {: source:str :}
   HEAP:ALLOCATOR source INIT ;

: RESULT= ( EDIT:editor str -- EDIT:editor )
   {: expected:str :}
   OUTPUT OUTPUT-CAP EDIT:WRITE
   OUTPUT swap >str expected T$= ;

: MIXED-EDITS ( -- )
   s" abcdefgh" OPEN
   1 >OFF 2 >LEN s" XYZZ" REPLACE
   5 >OFF 0 >LEN s" !" REPLACE
   7 >OFF 1 >LEN s" " REPLACE
   s" aXYZZde!fg" RESULT=
   s" aXYZZde!fg" RESULT=
   EDIT:CLOSE ;

: EXACT-PRESERVATION ( -- )
   s" <?xml?><a k='v'>old</a><!-- unchanged -->" OPEN
   16 >OFF 3 >LEN s" new&amp;more" REPLACE
   s" <?xml?><a k='v'>new&amp;more</a><!-- unchanged -->" RESULT=
   EDIT:CLOSE
   BINARY 4 >str OPEN
   1 >OFF 2 >LEN BINARY-INSERT 2 >str REPLACE
   BINARY-EXPECTED 4 >str RESULT= EDIT:CLOSE ;

: EMPTY-PLAN ( -- )
   s" whole original" OPEN
   s" whole original" RESULT= EDIT:CLOSE
   s" " OPEN
   s" " RESULT= EDIT:CLOSE ;

: ZERO-LENGTH-EDITS ( -- )
   s" ab" OPEN
   0 >OFF 0 >LEN s" <" REPLACE
   0 >OFF 0 >LEN s" !" REPLACE
   2 >OFF 0 >LEN s" >" REPLACE
   s" <!ab>" RESULT= EDIT:CLOSE
   s" ab" OPEN
   0 >OFF 2 >LEN s" " REPLACE
   s" " RESULT= EDIT:CLOSE ;

\ An insertion before every source byte: the result interleaves them.
: MANY-EDITS ( -- )
   MANY 0 ?do
      [char] a i 26 mod + MANY-SOURCE i + c!
      [char] - MANY-EXPECTED i 2 * + c!
      [char] a i 26 mod + MANY-EXPECTED i 2 * 1 + + c!
   loop
   MANY-SOURCE MANY >str OPEN
   MANY 0 ?do i >OFF 0 >LEN s" -" REPLACE loop
   MANY-EXPECTED MANY 2 * >str RESULT= EDIT:CLOSE ;

\ ---- the caught bodies of the failure cases ---------------------------------
\ Each failure body is a word reached by `[']`, not a quotation literal: the
\ editor is read after the catch, `catch` restores stack depth but not contents,
\ and a literal would leave stale<EDIT:editor>, which can be neither read nor
\ dropped. Callee evidence in the checker would admit a literal.
: REPLACE-NEG-OFF ( EDIT:editor -- EDIT:editor ) -1 >OFF 1 >LEN s" x" REPLACE ;
: REPLACE-PAST-END ( EDIT:editor -- EDIT:editor ) 4 >OFF 0 >LEN s" x" REPLACE ;
: REPLACE-PAST-LEN ( EDIT:editor -- EDIT:editor ) 1 >OFF 3 >LEN s" x" REPLACE ;
: REPLACE-NEG-LEN ( EDIT:editor -- EDIT:editor ) 1 >OFF -1 >LEN s" x" REPLACE ;
: REPLACE-NEG-SIZE ( EDIT:editor -- EDIT:editor ) 1 >OFF 1 >LEN s" x" STR:DATA -1 >str REPLACE ;
: REPLACE-INSIDE-LAST ( EDIT:editor -- EDIT:editor ) 3 >OFF 1 >LEN s" Y" REPLACE ;
: REPLACE-BEFORE-LAST ( EDIT:editor -- EDIT:editor ) 0 >OFF 1 >LEN s" Y" REPLACE ;
: WRITE-TINY ( EDIT:editor -- EDIT:editor ) OUTPUT 1 EDIT:WRITE drop ;
: WRITE-SOURCE ( EDIT:editor -- EDIT:editor ) BINARY 4 EDIT:WRITE drop ;
: WRITE-OUTPUT ( EDIT:editor -- EDIT:editor ) OUTPUT OUTPUT-CAP EDIT:WRITE drop ;

: BAD-RANGES ( -- )
   s" abc" OPEN
   ['] REPLACE-NEG-OFF catch E-RANGE T=
   ['] REPLACE-PAST-END catch E-RANGE T=
   ['] REPLACE-PAST-LEN catch E-RANGE T=
   ['] REPLACE-NEG-LEN catch E-RANGE T=
   ['] REPLACE-NEG-SIZE catch E-RANGE T=
   s" abc" RESULT= EDIT:CLOSE ;

: BAD-ORDER ( -- )
   s" abcdef" OPEN
   2 >OFF 2 >LEN s" X" REPLACE
   ['] REPLACE-INSIDE-LAST catch E-ORDER T=
   ['] REPLACE-BEFORE-LAST catch E-ORDER T=
   s" abXef" RESULT=
   4 >OFF 1 >LEN s" Z" REPLACE
   s" abXZf" RESULT= EDIT:CLOSE ;

: NO-PARTIAL-WRITE ( -- )
   $5A OUTPUT c!
   s" abc" OPEN
   1 >OFF 1 >LEN s" long replacement" REPLACE
   ['] WRITE-TINY catch E-CAPACITY T=
   OUTPUT c@ $5A T=
   s" along replacementc" RESULT= EDIT:CLOSE ;

: ALIAS-FAILURES ( -- )
   BINARY 4 >str OPEN
   ['] WRITE-SOURCE catch E-ALIAS T=
   0 >OFF 0 >LEN OUTPUT 1 >str REPLACE
   ['] WRITE-OUTPUT catch E-ALIAS T=
   EDIT:CLOSE ;

: CHECKED-OWNERSHIP ( -- )
   s" BYTE-EDIT-DUP ( EDIT:editor -- EDIT:editor EDIT:editor ) dup" CHECK! 0 T=
   s" BYTE-EDIT-DROP ( EDIT:editor -- ) drop" CHECK! 0 T=
   s" BYTE-EDIT-SWAP ( EDIT:editor len off str -- EDIT:editor ) EDIT:REPLACE"
   CHECK! 0 T= ;

T-RESET
MIXED-EDITS
EXACT-PRESERVATION
EMPTY-PLAN
ZERO-LENGTH-EDITS
MANY-EDITS
BAD-RANGES
BAD-ORDER
NO-PARTIAL-WRITE
ALIAS-FAILURES
CHECKED-OWNERSHIP
T-REPORT
;using
;package
