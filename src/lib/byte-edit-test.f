require src/lib/test/assert.f
require src/lib/heap.f
require src/lib/byte-edit.f

package BYTE-EDIT-TEST
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
   HEAP:ALLOCATOR source EDIT:INIT ;

: RESULT= ( EDIT:editor str -- EDIT:editor )
   {: expected:str :}
   OUTPUT OUTPUT-CAP EDIT:WRITE
   OUTPUT swap >str expected T$= ;

: MIXED-EDITS ( -- )
   s" abcdefgh" OPEN
   1 >OFF 2 >LEN s" XYZZ" EDIT:REPLACE
   5 >OFF 0 >LEN s" !" EDIT:REPLACE
   7 >OFF 1 >LEN s" " EDIT:REPLACE
   s" aXYZZde!fg" RESULT=
   s" aXYZZde!fg" RESULT=
   EDIT:CLOSE ;

: EXACT-PRESERVATION ( -- )
   s" <?xml?><a k='v'>old</a><!-- unchanged -->" OPEN
   16 >OFF 3 >LEN s" new&amp;more" EDIT:REPLACE
   s" <?xml?><a k='v'>new&amp;more</a><!-- unchanged -->" RESULT=
   EDIT:CLOSE
   BINARY 4 >str OPEN
   1 >OFF 2 >LEN BINARY-INSERT 2 >str EDIT:REPLACE
   BINARY-EXPECTED 4 >str RESULT= EDIT:CLOSE ;

: EMPTY-PLAN ( -- )
   s" whole original" OPEN
   s" whole original" RESULT= EDIT:CLOSE
   s" " OPEN
   s" " RESULT= EDIT:CLOSE ;

: ZERO-LENGTH-EDITS ( -- )
   s" ab" OPEN
   0 >OFF 0 >LEN s" <" EDIT:REPLACE
   0 >OFF 0 >LEN s" !" EDIT:REPLACE
   2 >OFF 0 >LEN s" >" EDIT:REPLACE
   s" <!ab>" RESULT= EDIT:CLOSE
   s" ab" OPEN
   0 >OFF 2 >LEN s" " EDIT:REPLACE
   s" " RESULT= EDIT:CLOSE ;

\ An insertion before every source byte: the result interleaves them.
: MANY-EDITS ( -- )
   MANY 0 ?do
      [char] a i 26 mod + MANY-SOURCE i + c!
      [char] - MANY-EXPECTED i 2 * + c!
      [char] a i 26 mod + MANY-EXPECTED i 2 * 1 + + c!
   loop
   MANY-SOURCE MANY >str OPEN
   MANY 0 ?do i >OFF 0 >LEN s" -" EDIT:REPLACE loop
   MANY-EXPECTED MANY 2 * >str RESULT= EDIT:CLOSE ;

\ ---- the caught bodies of the failure cases ---------------------------------
\ Each failure body is a word reached by `[']`, not a quotation literal: the
\ editor is read after the catch, `catch` restores stack depth but not contents,
\ and a literal would leave stale<EDIT:editor>, which can be neither read nor
\ dropped. Callee evidence in the checker would admit a literal.
: REPLACE-NEG-OFF ( EDIT:editor -- EDIT:editor ) -1 >OFF 1 >LEN s" x" EDIT:REPLACE ;
: REPLACE-PAST-END ( EDIT:editor -- EDIT:editor ) 4 >OFF 0 >LEN s" x" EDIT:REPLACE ;
: REPLACE-PAST-LEN ( EDIT:editor -- EDIT:editor ) 1 >OFF 3 >LEN s" x" EDIT:REPLACE ;
: REPLACE-NEG-LEN ( EDIT:editor -- EDIT:editor ) 1 >OFF -1 >LEN s" x" EDIT:REPLACE ;
: REPLACE-NEG-SIZE ( EDIT:editor -- EDIT:editor ) 1 >OFF 1 >LEN s" x" STR:DATA -1 >str EDIT:REPLACE ;
: REPLACE-INSIDE-LAST ( EDIT:editor -- EDIT:editor ) 3 >OFF 1 >LEN s" Y" EDIT:REPLACE ;
: REPLACE-BEFORE-LAST ( EDIT:editor -- EDIT:editor ) 0 >OFF 1 >LEN s" Y" EDIT:REPLACE ;
: WRITE-TINY ( EDIT:editor -- EDIT:editor ) OUTPUT 1 EDIT:WRITE drop ;
: WRITE-SOURCE ( EDIT:editor -- EDIT:editor ) BINARY 4 EDIT:WRITE drop ;
: WRITE-OUTPUT ( EDIT:editor -- EDIT:editor ) OUTPUT OUTPUT-CAP EDIT:WRITE drop ;

: BAD-RANGES ( -- )
   s" abc" OPEN
   ['] REPLACE-NEG-OFF catch EDIT:E-RANGE T=
   ['] REPLACE-PAST-END catch EDIT:E-RANGE T=
   ['] REPLACE-PAST-LEN catch EDIT:E-RANGE T=
   ['] REPLACE-NEG-LEN catch EDIT:E-RANGE T=
   ['] REPLACE-NEG-SIZE catch EDIT:E-RANGE T=
   s" abc" RESULT= EDIT:CLOSE ;

: BAD-ORDER ( -- )
   s" abcdef" OPEN
   2 >OFF 2 >LEN s" X" EDIT:REPLACE
   ['] REPLACE-INSIDE-LAST catch EDIT:E-ORDER T=
   ['] REPLACE-BEFORE-LAST catch EDIT:E-ORDER T=
   s" abXef" RESULT=
   4 >OFF 1 >LEN s" Z" EDIT:REPLACE
   s" abXZf" RESULT= EDIT:CLOSE ;

: NO-PARTIAL-WRITE ( -- )
   $5A OUTPUT c!
   s" abc" OPEN
   1 >OFF 1 >LEN s" long replacement" EDIT:REPLACE
   ['] WRITE-TINY catch EDIT:E-CAPACITY T=
   OUTPUT c@ $5A T=
   s" along replacementc" RESULT= EDIT:CLOSE ;

: ALIAS-FAILURES ( -- )
   BINARY 4 >str OPEN
   ['] WRITE-SOURCE catch EDIT:E-ALIAS T=
   0 >OFF 0 >LEN OUTPUT 1 >str EDIT:REPLACE
   ['] WRITE-OUTPUT catch EDIT:E-ALIAS T=
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
;package
